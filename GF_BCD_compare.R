# E. Chisholm
# February 14 2020
# Modified version of AZMP BCS vs BioChem comparison script written by Gordana Lazin
# specifically used to compare groundfish reboot records with biochem
# script updated to include more testing as discussed with Shelley Bond

### COUNT COMPARISON####
# compare number of data records in BioChem and reload BCD staging table

#source("query_biochem.r")
#source("replace_hplc.r")

# read the csv file with all cruise names and descriptors
cn=read.csv("GF_mission_name_descriptor.csv",stringsAsFactors = FALSE) #TODO create equivalent GF csv
#desc=cn$MISSION_DESCRIPTOR # defines mission descriptor

# read in GF data from csv rather than connect to biochem

bc_table <- read.csv('E:/BioChem QC/BC_Groundfish/groundfish_2.csv', stringsAsFactors = FALSE)

# get counts

bc_counts <- count(x = bc_table, DESCRIPTOR, METHOD) %>%
  dplyr::mutate(., PLACE = 'BIOCHEM') %>%
  dplyr::rename(., 'COUNT.1.' = n) %>%
  dplyr::rename(., 'METHOD' = METHOD)


# load BCD counts

bcd_counts <- read.csv('E:/BioChem QC/gf_bcd_counts.csv', stringsAsFactors = FALSE)

# join count tables

df <- rbind.data.frame(bc_counts, bcd_counts)



# 
# # extract counts from BCD and DISCRETE_DATA tables in Biochem. First create query string:
# query="select descriptor, method, 'BIOCHEM' place, count(1)
# from biochem.discrete_data
# where descriptor in ('18HU04055','18HU05055','18HU03078','18HU99054','18HU00050','18HU01061','18HU02064','18HU03067','18HU06052','18HU07045','18HU08037','18HU09048','18HU11043','18HU10049','18HU11004','18HU12042','18PZ00002','18HU99005','18HU04009','18HU01009','18HU06008','18NE05004','18HU07001','18HU08004','18HU09005','18HU10006','18HU03005')
# and (upper(collector_station_name) not like 'F%LAB%' or upper(gear_type) not like 'PUMP')
# group by name, descriptor, method
# UNION ALL
# select mission_descriptor descriptor, data_type_method method, 'BCD' place, count(1)
# from lazing.azmp_missions_bcd
# group by mission_descriptor, data_type_method;"
# 
# 
# # Query eleinates sample IDs collected for PI
# # query="select descriptor, method, 'BIOCHEM' place, count(1)
# # from biochem.discrete_data
# # where descriptor in ('18HU04055','18HU05055','18HU03078','18HU99054','18HU00050','18HU01061','18HU02064','18HU03067','18HU06052','18HU07045','18HU08037','18HU09048','18HU11043','18HU10049','18HU11004','18HU12042','18PZ00002','18HU99005','18HU04009','18HU01009','18HU06008','18NE05004','18HU07001','18HU08004','18HU09005','18HU10006','18HU03005')
# # and (upper(collector_station_name) not like 'F%LAB%' or upper(gear_type) not like 'PUMP')
# # and COLLECTOR_SAMPLE_ID not in 
# # 
# # (select distinct COLLECTOR_SAMPLE_ID
# # from BIOCHEM.DISCRETE_DATA
# # where descriptor in ('18HU04055','18HU05055','18HU03078','18HU99054','18HU00050','18HU01061','18HU02064','18HU03067','18HU06052','18HU07045','18HU08037','18HU09048','18HU11043','18HU10049','18HU11004','18HU12042','18PZ00002','18HU99005','18HU04009','18HU01009','18HU06008','18NE05004','18HU07001','18HU08004','18HU09005','18HU10006','18HU03005')
# # and METHOD like 'PP_PI%')
# # 
# # group by name, descriptor, method
# # UNION ALL
# # select mission_descriptor descriptor, data_type_method method, 'BCD' place, count(1)
# # from lazing.azmp_missions_bcd
# # group by mission_descriptor, data_type_method;"
# 
# df=query_biochem(query)
# 



# read files with biochem and BCD staging table data counts
#df=read.csv("AZMP_BCSD-BC_counts.csv", stringsAsFactors = FALSE)

allmissions=NULL

# troubleshooting
desc <- c('18NE05027', '18NE10001', '18NE10002', '18NE10027', '18NE11002', '18NE11025', '18TL04529', '18TL04530', '18TL05605', '18TL05633', '181C04004')


# for each mission make a table that has BCD and BIOCHEM data counts side by side
# write the table to csv file
for (i in 1:length(desc)) {
  
  mission=desc[i]
  
  
  bct=df[which(df$DESCRIPTOR==mission & df$PLACE=="BIOCHEM"),] # biochem table with one mission
  
  bcd=df[which(df$DESCRIPTOR==mission & df$PLACE=="BCD"),] #bcd table with one mission
  
  # if there is HPLC pigments in the methods replace them just with HPLC
  #hpbc=grep("HPLC_",bct$METHOD) # finds where HPLC method is
  #hpbcd=grep("HPLC_",bcd$METHOD)
  
  # replace with HPLC
  #if (length(hpbc)>0) {
  #  bct=replace_hplc(bct)
  #}
  
  #if (length(hpbcd)>0) {
  #  bcd=replace_hplc(bcd)
  #}
  
  # rename count column
  cc=grep("COUNT", names(bcd))
  
  
  names(bcd)[cc]="BCD" 
  names(bct)[cc]="BIOCHEM"
  
  bcd=bcd[,!(names(bcd) %in% "PLACE")] # remove place column
  bct=bct[,!(names(bct) %in% "PLACE")] # remove place column
  
  
  mdf=merge(bcd,bct, by=c("DESCRIPTOR","METHOD"), all=T)
  
  write.csv(mdf, paste0(mission,"_counts_comparison.csv"), row.names=F,na="")
  
  allmissions=rbind(allmissions,mdf)
  
}


# tag the methods with data type

greps=c("Chl_a","Chl_Fluor","cond","HPLC","NO2NO3","O2_CTD","O2_El","PAR","Phaeo","PO4","POC","PON","Press",
        "Salinity_CTD", "Salinity_Sal","SiO4","Temp_CTD","NH3","NO2","Secchi","Salinity_CTDloop",
        "Temp_CTD_loop","TOC","ph_CTD","Winkler","Fluoresc","CO2",
             "CHL-SENSOR", "Chlorophyll A", "Nitrate", "Phosphate", "Silicate", "Temperature", "Ammonia")

tags=c("chl","chl_ctd","cond","hplc","nit","O2_ctd","O2_el","par","phaeo","pho","poc","pon","press",
       "sal_ctd", "sal_sal","sil","temp_ctd","amo","no2","secchi","sal_loop","temp_loop","toc",
       "ph_ctd","winkler","fluo","co2",
          "chl_ctd", "chl", "no2", "pho", "sil", "temp_ctd", "amo" )

allmissions$tag=NA

for (i in 1:length(greps)) {
  im=grep(greps[i],allmissions$METHOD)
  allmissions$tag[im]=tags[i]
}

write.csv(allmissions, "GF_all_counts_comparison_tagged.csv", row.names=F, na="")

####---------------------------------------------####
#### CONTENT COMPARISON ####


# compares BCD data with data from BioChem discrete data table

#source("query_biochem.r")
library(lubridate)

# read the csv file with all cruise names and descriptors
#cn=read.csv("AZMP_mission_name_descriptor.csv",stringsAsFactors = FALSE)
#cn=query_biochem("select distinct MISSION_DESCRIPTOR from AZMP_MISSIONS_BCD;") # list of all descriptor in BCD table

# TODO get all cruise names for loop

cn <- cn[cn$MISSION_DESCRIPTOR %in% desc,]
for (i in 1:length(cn[,1])) {
  
  desc=cn[i,2]
  
  report_file=paste0(desc,"_comparison_report.txt")
  # write input files into report
  sink(file=report_file,append=F, split=TRUE)
  
  cat("\n")
  cat("\n")
  cat(paste("Comparing BCD records with BioChem for mission", desc,", created", now()))
  cat("\n")
  cat("========================================================================================")
  cat("\n")
  
  
  
  colmap=read.csv("bcd_bc_column_names.csv", stringsAsFactors = F)
  tags=read.csv("GF_all_counts_comparison_tagged.csv", stringsAsFactors = FALSE)
  tagd=tags[which(tags$DESCRIPTOR==desc),]
  
  
  # load BCD file for all cruises
  bcd=read.csv("GF_BCD_BC.csv",stringsAsFactors=F)
  bcd_desc=bcd[which(bcd$MISSION_DESCRIPTOR==desc),] # bcd for one cruise
  
  
  # subset biochem table to mission
  
  bc <- bc_table %>%
    dplyr::filter(., DESCRIPTOR == desc)

  #cols=2:17 # columns in BCD file that I want to look at
  cols <- c(2:13, 16:17) # removed qc code and detection limit (missing from biochem pull)
  #bcd_cols=colmap$BCD_COLUMN_NAME[cols]
  bcd1=bcd_desc[,cols] # BCD file for certain columns
  

  # rename columns from BioChem to short names
  ibc=match(colmap$BC_COLUMN_NAME[cols],names(bc))
  ibc=ibc[!is.na(ibc)] # indices of the columns on BioChem file that match BCD columns
  
  bc1=bc[,ibc]
  
  # rename columns in both files
  names(bc1)=colmap$SHORT_NAME[cols]
  names(bcd1)=colmap$SHORT_NAME[cols]
  
  # now biochem and bcd data are in same format with same column names
  # so they will be easier to compare
  
  # Test 1: are all unique sample IDs the same
  cat("\n")
  cat("**** TEST 1: COMPARING UNIQUE SAMPLE IDs FOR THE WHOLE MISSION:")
  cat("\n")
  cat("\n")
  cat(paste("-> BCD:",length(unique(bcd1$id)),"IDs, BioChem:",length(unique(bc1$id)),"IDs" ))
  cat(c("\n","\n"))
  
  id_not_in_bcd=setdiff(unique(bc1$id),unique(bcd1$id)) # check for IDs that are not in BCD
  
  id_not_in_biochem=setdiff(unique(bcd1$id),unique(bc1$id)) # check for IDs not in Biochem
  
  if (length(id_not_in_bcd)>0) {
    cat(paste0("-> ", length(id_not_in_bcd)," Sample IDs found in BioChem but NOT in BCD:"))
    cat("\n")
    cat("\n")
    print(id_not_in_bcd)
    cat("\n")
    cat("\n")
    ind=which(bc1$id %in% id_not_in_bcd) # indices of the records not in BCD
    print(bc[ind,c(2,9,13,17,18,35,38,39,41,42,43,45)])
  }
  
  if (length(id_not_in_biochem)>0) {
    cat(c("\n","\n"))
    cat(paste0("-> Sample IDs found in BCD but NOT in BioChem:"))
    cat("\n")
    cat("\n")
    print(id_not_in_biochem) }
  
  
  # Test 2: check IDs for each parameter
  cat("\n")
  cat("\n")
  cat("**** TEST 2: COMPARING SAMPLE IDs FOR EACH PARAMETER SEPARATELY:")
  cat("\n")
  
  
  parameters=unique(tagd$tag)
  parameters <- parameters[-which(parameters=="")] # delete empty spaces
  cat(parameters)
  #parameters=parameters[-c(grep("press",parameters),grep("par",parameters))]
  
  # get only parameters taht appear twice
  
  
  for (j in 1:length(parameters)) {
    
    pr=parameters[j]
    cat("\n")
    
    
    
    #pr=readline("Which parameter would you like to compare?:")
    
    mbcd1=tagd$METHOD[which(!is.na(tagd$BCD) & tagd$tag==pr)]
    mbc1=tagd$METHOD[which(!is.na(tagd$BIOCHEM) & tagd$tag==pr)]
    
    bcd1p=bcd1[which(bcd1$method==mbcd1),] #dataframe from BCD containing desired parameter
    bc1p=bc1[which(bc1$method==mbc1),] # dataframe from Biochem containing desired parameter
    
    cat("\n")
    cat("\n")
    cat(paste("-> Checking", pr,"--",length(unique(bcd1p$id)), "IDs in BCD; ",length(unique(bc1p$id)),"IDs in BioChem"))
    cat("\n")
    cat("\n")
    # compare IDs
    
    ninbc=sort(setdiff(bcd1p$id,bc1p$id)) # not in biochem
    ninbcd=sort(setdiff(bc1p$id,bcd1p$id)) # not in BCD
    
    if (length(ninbcd)>0) {
      cat(paste("->", length(ninbcd), pr,"Sample IDs found in BioChem but NOT in BCD:"))
      cat("\n")
      cat("\n")
      print(ninbcd)
      cat("\n")
      cat("\n")
      ind=which(bc1p$id %in% ninbcd) # indices of the records not in BCD
      print(bc1p[ind,])
    }
    
    if (length(ninbc)>0) {
      cat("\n")
      cat("\n")
      cat(paste0("-> Sample IDs found in BCD but NOT in BioChem:"))
      cat("\n")
      cat("\n")
      print(ninbc)
      cat("\n")
      cat("\n")}
    
  }
  
  sink()
  
}

# ind1=which(bc1p$id %in% ninbcd)
# 
# ind2=which(bc1$id %in% ninbcd)
# 
# bc2=bc1[ind2,]
# 
# bc1[which(bc1$id==243528),]






