# Create a file from BCD data in a format appropriate for IML QC script in matlab
#
# Gordana lazin, BioChem Reload project March 7, 2017
# Emily Chisholm, Updated for Oxygen Recovery from BioChem Feb 2019
# E. Chisholm updated for groundfish reboot May 2019


require(reshape)
options(warn=1) # display warnings as they happen

# convert oxygen data if necessary - only runs if Winkler data is present
 source('convertOxy.R')

##### !USER INPUT REQUIRED! ####
file_dir <- 'E:/BioChem QC/2013+/' 
# directory where BCD files are located

#### SET UP ####
# load map file that matches IML fields to the BCD fields
map0=read.csv("BCD_IML_map_upd.csv", stringsAsFactors = FALSE)

# get data directly from staging  table incstead of reading csv
Sys.setenv(TZ = "GMT")
Sys.setenv(ORA_SDTZ = "GMT")

# read in individual BCD files produced from groundfish biolsums


files <- list.files(file_dir, pattern = '*BCD.csv', recursive = T)

#### START LOOP ####
#loop through each file
for (i in 1:length(files)){
  
  bcdo <- read.csv(paste0(file_dir, files[i]), stringsAsFactors = F)
  

  #build sample key value #already present for groundfish
  #bcdo$DIS_SAMPLE_KEY_VALUE=paste0(bcdo$MISSION_DESCRIPTOR,"_",sprintf("%03d",bcdo$EVENT_COLLECTOR_EVENT_ID),"_",bcdo$DIS_DETAIL_COLLECTOR_SAMP_ID)
  #rename some columns
  #names(bcdo)[14] <- 'DIS_DETAIL_DATA_VALUE'
  #names(bcdo)[12] <- 'DIS_DETAIL_DATA_TYPE_SEQ'
  
  
  #desc=unique(bcdo$MISSION_DESCRIPTOR) # all cruises in the BCD

  
  print(paste("Loop",i,",",bcdo$MISSION_DESCRIPTOR[1]))
  
  bcd <- bcdo
  
  if ('O2_Winkler_Molar' %in% bcd$DATA_TYPE_METHOD){
    #convert mmol/m**3 data to ml/l
    
    bcd <- convertOxy(bcd, missions = descriptor)
    print(paste('>>> Oxygen values converted for mission' , descriptor))
  }
  
 
  # load IML example file
  # load IML test file
  iml_file="BTL_01064.txt"
  
  # this is how to read iml txt file
  df=read.table(iml_file, stringsAsFactors=FALSE, sep=";")
  
  # subset df1 to have only metadata, and ctd data
  df=df[,c(1:18)] ###!!! CAUTION HARD CODING
  
  # === prepare mapping file that relates bcd fields to IML fields ====
  
  # remove lines for which there is no matchin fields
  no_match=which(map0$BCD_FIELDS=="" | map0$IML_CODE=="")
  
  map=map0[-no_match,] # remove the lines
  #=== Mapping file ready ===
  
  
  # subset BCD to only include required fields
  md=which(names(bcd) %in% map$BCD_FIELDS) # metadata columns
  dc=which(names(bcd) %in% c("DATA_TYPE_METHOD" ,"DIS_DETAIL_DATA_VALUE"))
  cols=union(md,dc) # column numbers for subsetting
  
  bd=bcd[,cols] # has only needed columns
  
  # subset only certain data types
  data_types=map$BCD_FIELDS[which(map$qc== "qc")] # data types that are QC by IML script
  
  # this is now dataframe that will be transformed for to IML format
  b=bd[which(bd$DATA_TYPE_METHOD %in% data_types),] # include only methods that are QC by IML script
  
  # trim sample ID from DIS_SAMPLE_KEY_VALUE, so it has only cruiseName_eventNumber
  #b$DIS_SAMPLE_KEY_VALUE=substr(b$DIS_SAMPLE_KEY_VALUE,1,14)
  
  # convert to wide format
  mcol=which(names(b) %in% c("DIS_SAMPLE_KEY_VALUE","DATA_TYPE_METHOD","DIS_DETAIL_DATA_VALUE")) # desired column indices
  wide=cast(b[,mcol],DIS_SAMPLE_KEY_VALUE ~ DATA_TYPE_METHOD, paste, fill=NA, value="DIS_DETAIL_DATA_VALUE") # this can be then merged to BCS metadata
  #issues with wide format
  
  # merge with metadata CAUTION HARD CODING!!!
  bm=merge(wide,unique(b[,1:8]),by="DIS_SAMPLE_KEY_VALUE")  #EC update 02/2019: now 8 metadata columns includes start depth
  
  # make IML file 
  mm=match(map$BCD_FIELDS, names(bm)) # match the column names to the map file
  mm=unique(mm[which(!is.na(mm))]) # this will be used to re-arrange the columns
  bmn=bm[,mm] # dataframe with columns in order
  
  
  # keep only matching fields and needed columns
  mapss=map[which(map$BCD_FIELDS %in% names(bmn)),1:4]
  
  # rename the columns to IML_CODE
  names(bmn)[match(mapss$BCD_FIELDS, names(bmn))] = mapss$IML_CODE
  
  
  # ==== change date and time format======
  # add leading zeroes to hour
  h=sprintf("%04d", as.numeric(bmn$Heure))
  h2 <- stringr::str_split(h, pattern = "", n = 4)
  h3 <- list()
  for(j in 1:length(h2)){
  h3[[j]] <- paste0( str_c(h2[[j]][1:2], collapse = ''), ':', str_c(h2[[j]][3:4], collapse = ''))
  }
  bmn$Heure <- unlist(h3)
  #already as character, attempting to convert to numeric makes NA
  
  # convert to posixct
  datetime=as.POSIXct(paste(bmn$Date, bmn$Heure), format="%d-%b-%y %H:%M", tz="GMT")
  
  # Convert date:
  bmn$Date=tolower(format(datetime,"%d-%b-%y"))
  
  # convert time
  bmn$Heure=format(datetime,"%H:%M:%S")
  
  
  #check that there are no nan dates
  
  if(anyNA(bmn$Heure)){
    stop('NA times produced!')
  }
  if(anyNA(bmn$Date)){
    stop('NA Dates produced!')
  }
  
  
  
  #==== done with date-time format changes ====
  
  # convert bmn to characters
  bc=data.frame(lapply(bmn, as.character), stringsAsFactors=FALSE)
  
  # add lines from mapss (unit and CTD/labo tags)
  names(bc)=mapss$IML_unit
  bc=rbind(names(bc),bc) # add units
  
  names(bc)=mapss$IML_CODE
  bc=rbind(names(bc),bc) # add names
  
  names(bc)=mapss$localisation
  bc=rbind(names(bc),bc) # add CTD/labo tags
  
  names(bc)=paste0("V",1:dim(bc)[2]) # have names be V1, V2, etc.
  
  
  #THIS SECTION WAS CAUSING ERROR, REMOVING SECOND DIGIT FROM
  #SET NUMBER CAUSING QC TO COMPARE DIFFERENT SETS
  #PRESERVED FOR POSTERITY, DO NOT RUN
  ###################################################
  # remove sample ID from the column ##!!!!   ERROR
  #!skl=bc$V1[4:length(bc$V1)] # sample key example ##!!!! ERROR
  #!underscore=unlist((gregexpr(pattern ='_',skl)))[2] ##!!! ERROR
  # find second underscore  ##!!!! ERRROR
  #!bc$V1=substr(bc$V1,1,underscore-1)##!!! ERROR
  ####!!!! THIS IS CUTTING OFF SECOND DIGIT OF SET NUMBER
  #########################################################
  
  
  #REMOVE SAMPLE ID FROM SAMPLE KEY VALUE BY DIRECTLY EVALUATING EACH ITEM
  #ACCOUNTS FOR ANY POTENTIAL STRANGE DIFFERENCES IN NUMBER OF DIGITS/CHAR
  #E. CHISHOLM JULY 9, 2019
  skl <- list()
  for ( ii in 1:length(bm$DIS_SAMPLE_KEY_VALUE)){
    skv <- bm$DIS_SAMPLE_KEY_VALUE[ii]
    si <- as.character(bm$DIS_DETAIL_COLLECTOR_SAMP_ID[ii])
    
    skl[ii] <- substr(skv, 1, nchar(skv)- nchar(si) - 1)
  }
  skl <- unlist(skl)
  
  bc$V1[4:length(bc$V1)] <- skl #insert new sample keys (after header lines)
  
  
  
  
  # copy temp, sal, oxy and ph columns and make them labo
  # add columns for Temp and Sal and oxy by copying CTD data columns
  if(length(grep("DOXY",bc[2,])) > 0 ){
    bc$oxy=bc[,grep("DOXY",bc[2,])[1]] #find CTD oxygen and add it to data
    oxy_exists = TRUE
  }else { oxy_exists = FALSE}
  if (length(grep("TE",bc[2,])) > 0 ){
    bc$temp=bc[,grep("TE",bc[2,])[1]] #find ctd tempand add it to data 
    temp_exists = TRUE
  } else { temp_exists = FALSE}
  if (length(grep("PSAL",bc[2,])) > 0 ){
    bc$sal=bc[,grep("PSAL",bc[2,])[1]] # find ctd salinity
    sal_exists = TRUE
  } else {sal_exists = FALSE}
  
  # # check if there is ph
  if (length(grep("PHPH",bc[2,]))>0) {
    bc$ph=bc[,grep("PHPH",bc[2,])[1]] # find ctd ph
    bc$ph[1]="labo"
    bc$ph[2]="pH_01"
  }
  # 
  # # change CTD to labo
  if (oxy_exists == TRUE){
    bc$oxy[1]="labo"
    bc$oxy[2]="OXY_XX"
  }
  # 
  if (sal_exists == TRUE){
    bc$sal[1]="labo"
    bc$sal[2]="SSAL_BS"
    bc$sal[3]="(g/kg)"
  }
  # 
  if (temp_exists == TRUE){
    bc$temp[1]="terrain"
    bc$temp[2]="TEMP_RT"
  }
  # 
  
  
  
  # make separate file for CTD data only and for bottle data only
  # pick CTD columns only
  nctd=grep("CTD",bc[1,])
  lctd=which(bc[2,] %in% c("OXY_XX","SSAL_BS","TEMP_RT","pH_01"))
  ctd_only=bc[,c(nctd,lctd)] # this is dataframe with CTD data pretended to be labo
  
  bcl=bc[,-lctd] 
  #bcl = bc # if this is just bottle data (labo)
  
  # write out the file
  descriptorIndex <- unlist((gregexpr(pattern ='_',skl[1])))[1]
  descriptor <- substr(skl[1],start = 1 , stop = descriptorIndex-1)
  # define the name and path to output file
  dir.create('IML_QC', showWarnings = FALSE,  recursive = TRUE )
  outpath=file.path(getwd(),"IML_QC") # folder to put the data
  outfile=file.path(outpath,paste0(descriptor,"_IML_format.txt")) # for bottle data only
  outfile_ctd=file.path(outpath,paste0(descriptor,"_IML_format_ctd.txt")) # for ctd data only
  

  # write bottle file
  write.table(bcl,file=outfile,sep=";",col.names=FALSE, row.names=FALSE, quote=FALSE, na='NaN')
  
  # write CTD file
  write.table(ctd_only,file=outfile_ctd,sep=";",col.names=FALSE, row.names=FALSE, quote=FALSE, na='NaN')
  

} #end loop for files

