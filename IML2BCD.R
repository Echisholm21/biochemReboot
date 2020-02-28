# loads flagged IML files and appends flags to the BCD file
# Gordana Lazin, April 2017
# modified to work offline from BIOCHEM
# E. Chisholm January 2019
# modified for groundfish reboot, June 2019

require(reshape)
require(plyr)
options(warn=1) # display warnings as they happen

Sys.setenv(TZ = "GMT")
Sys.setenv(ORA_SDTZ = "GMT")

#### !USER INPUT REQUIRED! ####
file_dir <- 'E:/BioChem QC/2013+/'  # directory where BCD files are stored
# directory path to the IML QC output files
fp <- 'E:/BioChem QC/2013_QC/IML_QC_output/'


#### SET UP ####
files <- list.files(file_dir, pattern = '*BCD.csv', recursive = T)


#format bcd properly to match file used to create IML format
#data$DIS_SAMPLE_KEY_VALUE=paste0(data$MISSION_DESCRIPTOR,"_",sprintf("%03d",data$EVENT_COLLECTOR_EVENT_ID),"_",data$DIS_DETAIL_COLLECTOR_SAMP_ID)



# define empty dataframe that will hold number of flags for each mission
sumtab_all <- data.frame(mission=character(), method=character(),f0=numeric(), f1=numeric(), f2=numeric(),
                         f3=numeric(), f4=numeric(), f7=numeric(), stringsAsFactors=FALSE)
missions <- list.files(file_dir)

#### start loop ####
for (i in 1:length(missions)) {
  
  bcdo <- read.csv(paste0(file_dir, files[i]), stringsAsFactors = F)
  
  #mission= missions[[1]] #run one mission at a time
  mission=missions[i]
  # 
  
  
  cat("\n","\n")
  cat(paste("-> Moving flags to BCD file for mission", mission,"..." ))
  
  # 1. Load BCD 
  
  bcdf=bcdo # create bcdf that will hold flags. in bcd all flags are 0
  
  # 2. Create file names for IML flagged files
  # IML files named by batch seq not mission
  batch_seq <- substr(mission, 6, nchar(mission))
  fn1=paste0("QC_",batch_seq,"_IML_format.txt") # create file name for bottle data
  fn2=paste0("QC_",batch_seq,"_IML_format_ctd.txt") # create file name for ctd data
  fn=c(fn1,fn2) # fn has 2 file names, one for bottle data and one for CTD
  
  
  fnp=file.path(fp, fn)
  
  # insert flags into BCD file for all the missions in the loop
  
  for (ii in 1:2) {
    
    # this is how to read iml txt file
    #had to add comment.char = '' to avoid issues with # in data frame
    df=read.table(fnp[ii], stringsAsFactors=FALSE, sep=";", na.strings = 'NaN', comment.char = '')
    
    names(df)=df[2,] # rename columns using 2nd row
    df=df[-c(1,2,3),] # delete first 3 rows
    
    #find which columns include each data type
    latind <- grep(names(df), pattern = 'Latitude')
    lonind <- grep(names(df), pattern = 'Longitude')
    timeind <- grep(names(df), pattern = 'Heure')
    dateind <- grep(names(df), pattern = 'Date')
    #combine
    excludeind <- c(latind, lonind, timeind, dateind)
    
    #d=df[,c(1,5,9:dim(df)[2])] # subset to exclude lat, lon, time and date
    d <- df[, -excludeind] #subset to exclude lat, lon, time and date in generalized format to avoid formatting issues
    
    d1=d
    
    # convert all columns excpt first and second to numeric
    d1[,3:dim(d)[2]]=data.frame(sapply(d[,3:dim(d)[2]], as.numeric))
    
    # add sample key value
    d1$sample_key=paste0(d1$Fichier,"_",d1$Echantillon)
    
    # find columns with quality flags (qc columns, qcc)
    qcc=grep("Q_", names(d1))
    
    # rename columns with the QC flags so they contai variable names
    names(d1)[qcc]=paste0("Q_",names(d1)[qcc-1])
    
    sk=grep("sample_key",names(d1)) # find column named sample_key
    
    # dataframe of flags, with sample_key
    qcdf=d1[,c(sk,qcc)]
    
    # data frame of data
    ddf=d1[,c(sk,qcc-1)]
    
    # melt QC flag dataframe
    md=melt(qcdf,"sample_key",na.rm=T, stringsAsFactors=F)
    md$variable=as.character(md$variable) # convert factors to characters
    names(md)=paste0("qc_",names(md)) # add qc_ in column name so there is no confusion for merging
    
    # add column with variable name that will be used for merging
    md$variable=gsub("Q_","",md$qc_variable)
    
    # replace flags 9 with NA
    if (length(which(md$qc_value==9))>0) {
      mdqc=md[-which(md$qc_value==9),]} else {mdqc=md}
    
    # melt data frame with data
    mdd=melt(ddf,"sample_key",na.rm=T, stringsAsFactors=F)
    mdd$variable=as.character(mdd$variable) # convert factors to characters
    
    # merge flags with data
    m=merge(mdqc,mdd, by.x=c("qc_sample_key","variable"), by.y=c("sample_key","variable"))
    
    # now, match IML names with BCD names
    # load map file that matches IML fields to the BCD fields
    map=read.csv("BCD_IML_map_upd.csv", stringsAsFactors = FALSE)
    #limit map to data types present in BCD to avoid confusion with non unique IML codes
    #E chisholm Feb 2019
    dtype <- unique(bcdo$DATA_TYPE_METHOD)
    tmap <- map[map$BCD_FIELDS %in% dtype , ]
    map <- tmap
    
    
    # if this is not ctd file
    if (length(grep("_ctd",fnp[ii]))==0) {
      ind=match(m$variable,map$IML_CODE)
      m$DATA_TYPE_METHOD=map$BCD_FIELDS[ind]
    } 
    
    # this is ctd file
    if (length(grep("_ctd",fnp[ii]))==1) {
      m$DATA_TYPE_METHOD=NA
      m$DATA_TYPE_METHOD[which(m$variable =="OXY_XX")]="O2_CTD_mLL"
      m$DATA_TYPE_METHOD[which(m$variable =="SSAL_BS" )]="Salinity_CTD"
      m$DATA_TYPE_METHOD[which(m$variable=="TEMP_RT")]="Temp_CTD_1968" 
      
      # if there is ph include it
      if ( length(grep("pH_01",m$variable))>0){
        m$DATA_TYPE_METHOD[which(m$variable=="pH_01")]="pH_CTD_nocal"   
      }
      
    } 
    
    
    
    
    
    # ==== merge with BCD based on the sample key, value, and DATA_TYPE_METHOD ====
    
    
    # Match bcd with melted IML file with flags
    
    # create matching string: sample-key-value_method_value. 
    # Have to include value so you can match replicates that have same sample ID's
    mid=paste0(m$qc_sample_key,"_",m$DATA_TYPE_METHOD,"_",m$value)
    bcdid=paste0(bcdf$DIS_SAMPLE_KEY_VALUE, "_",bcdf$DATA_TYPE_METHOD,"_",bcdf$DIS_DETAIL_DATA_VAL)
    
    fr=match(mid,bcdid) # indices of ids in BCD file that match ids in IML file m
    
    fr <- na.omit(fr) #to prevent NA erros in matching vector
    # 3, insert flags into BCD
    bcdf$DIS_DETAIL_DATA_QC_CODE[fr]=m$qc_value
    
  } #end of mission loop
  
  
  # 4. check if everything is matching
  
  
  # 5. write out BCD with flags
  
  #cruise names for archived data set
  cname <- mission
  
  dir.create(file.path(file_dir,cname), showWarnings = FALSE)
  outname=file.path(file_dir, cname,paste0(cname,"_BCD_flagged.csv")) # name of the file that will be written in the cruise directory
  
  #write.csv(bcdf,outname, quote = FALSE, na="", row.names = FALSE) # write out BCD with flags
  
  # summary table for the cruise, describing how many flags for each parameter
  sumtab=table(bcdf$DATA_TYPE_METHOD, as.numeric(bcdf$DIS_DETAIL_DATA_QC_CODE))
  
  tabname=file.path(file_dir,cname,paste0(cname,"_flag_summary.csv")) 
  
  #write.csv(sumtab,tabname) # write out flag summary
  
  cat("\n","\n")
  cat(paste("-> Flags sucessfully transfered to BCD file for mission", mission,"."))
  cat("\n","\n")
  cat("-> Summary of the flags for each parameter:")
  cat("\n","\n")
  print(sumtab)
  
  
  
  # NEW ADDITION: convert sumtab to dataframe and merge them all together
  
  # convert to dataframe
  b=as.data.frame.matrix(sumtab)
  
  # add f to the name
  names(b)=paste0("f",names(b))
  
  # add column with method and mission
  b$method=row.names(b)
  b$mission=mission
  
  # bind them all together
  sumtab_all=rbind.fill(sumtab_all,b)
  
  
  
}


# change NA to 0 in summary table for the flags

sumtab_all[is.na(sumtab_all)]=0

# aggregate results for all cruises, each method separately
all_flags=aggregate(.~method,sumtab_all[,2:8],sum)

write.csv(all_flags,"all_flags_summary.csv", row.names=F)


