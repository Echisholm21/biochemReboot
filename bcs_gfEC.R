### create BCS file
### prep for data reload to BioChem for groundfish data
### metadata sourced from BiolSums and OSC cruise database
create_bcs <- function(mission, ff, bsum_o){
  #' @param mission the mission name of the file you are converting
  #' @param ff dataframe structure produced by BiolSum_BCD that is 'stacked' into BCD format with other small corrections from bSum
  #' @param bsum_o the original biolsum file read as xlsx
  #' 
  #' 
#E. Chisholm, May 2019
require(tidyr)
require(lubridate)
require(reshape)

require(xlsx)
#require(tidyverse)
#source('D:/BioChem QC/Working/stack_bcdata_EC.r')



#read in column names
#map <- read.xlsx('BCS_template.xlsx', sheetIndex = 1, stringsAsFactors = F, header = F)



#find Biolsum to pull metadata

# fp="R:/Science/BIODataSvc/SRC/BIOCHEMInventory/Data_by_Year_and_Cruise/Files_for_DIS_header.xlsx"
# files=read.xlsx(fp, sheetName = 'GROUNDFISH', stringsAsFactors =F )
# BiolSum=file.path(files$path,files$biolsum)



  # create a directory in the current folder with the cruise name
  dir.create(paste0('E:/BioChem QC/groundfish_data/',mission))
  outpath=file.path('E:/BioChem QC/groundfish_data/',mission )
  #use for outputting checks
  
# read BiolSum
# bsum=read.xlsx(BiolSum[i], sheetName ="BIOLSUMS_FOR_RELOAD", stringsAsFactors =F)


####hard coded####
mission_institute <- 'DFO BIO'
mission_protocol <- 'AZMP'
mission_data_manager_comment <- 'Maritimes BioChem Reload'
dis_headr_collector_comment1 <- 'End depth = start depth. Start depth is nominal'
dis_headr_data_manager_comment <- 'BioChem reload: QC performed using modified IML protocols. Comments from BiolSums.'
dis_headr_responsible_group <- 'AZMP'
created_by <- 'Emily Chisholm'
data_center_code <- 20
process_flag <- 'NR'


####nulls####

event_utc_offset <- 0
dis_headr_time_qc_code <- 0
dis_headr_position_qc_code <- 0
batch_seq <- 'NULL'
mission_collector_comment1 <- 'NULL'
mission_collector_comment2 <- 'NULL'
event_edate <- 'NULL'
event_etime <- 'NULL'
event_etime <- 'NULL'
event_collector_comment1 <- 'NULL'
event_collector_comment2 <- 'NULL'
event_data_manager_comment <- 'NULL'
dis_headr_edate <- 'NULL'
dis_headr_etime <- 'NULL'
dis_headr_collector_deplmt_id <- 'NULL'
dis_headr_collector <- 'NULL'
dis_headr_shared_data <- 'NULL'





####pull from biolsum####


#get times formatted

 datetime <- bsum_o$ctd_date_time
 dates <- format(as.POSIXct(strptime(datetime,"%Y-%m-%d %H:%M:%S",tz="UTC" )), format = '%d-%b-%Y')
 time <- format(as.POSIXct(strptime(datetime,"%Y-%m-%d %H:%M:%S",tz="UTC")) ,format = "%H:%M:%S")


  #datetime_sec <- as.numeric(datetime) *60*60*24
  #dates <- format(as.Date(as.numeric(datetime), origin = '1899-12-30'), format = '%d-%b-%Y')
  #time <- format(as.POSIXct(datetime_sec, origin = '1899-12-30 00:00:00', tz = 'UTC'), format = "%H:%M:%S")
 
#remove NULLs

time_index <- is.na(time)
t_ind <- grep(time_index , pattern = TRUE)

event_collector_event_id <- bsum_o$event
event_collector_stn_name <- paste0('MFD_SETNO', bsum_o$event)
event_sdate <- dates
event_stime <- time
event_min_lat <- as.numeric(bsum_o$lat)
event_max_lat <- as.numeric(bsum_o$lat)
event_min_lon <- as.numeric(bsum_o$lon)
event_max_lon <- as.numeric(bsum_o$lon)
dis_headr_sdate <- dates
dis_headr_stime <- time
dis_headr_slat <- as.numeric(bsum_o$lat)
dis_headr_elat <- as.numeric(bsum_o$lat)
dis_headr_slon <- as.numeric(bsum_o$lon)
dis_headr_elon <- as.numeric(bsum_o$lon)
dis_headr_start_depth <- as.numeric(bsum_o$depth)
dis_headr_end_depth <- as.numeric(bsum_o$depth)
dis_headr_sounding <- as.numeric(bsum_o$sounding)
dis_headr_collector_sample_id <- bsum_o$id



####stack bcs####

bsum_meta = c(
  "event",
  "cruise",
  "lat",
  "lon",
  "sounding",
  "depth",
  "ml_file",
  "xbt_file",
  "ctd_file",
  "serial_ctd",
  "serial_CTD",
  "serial_temp",
  "serial_cond",
  "id",
  "event.1",
  "upload_date",
  "upload_time",
  "ctd_date_time",
  "comments"
)

# keep only metadata column
bmeta=bsum_o[,(names(bsum_o) %in% bsum_meta)]


####generated####

#dis_sample_key_value
mission_name <- files$mission[i]
id=dis_headr_collector_sample_id       # constructed using mission, event and sample ID
id[which(is.na(id))]="0"
event_collector_event_id[which(is.na(event_collector_event_id))]="0"
DIS_SAMPLE_KEY_VALUE=paste0(mission_name,"_",event_collector_event_id,"_",id)
sample_key <- as.data.frame(cbind(DIS_SAMPLE_KEY_VALUE, id))

#define for gear type
ctd_dtypes <- c('Temp_CTD_1968', 'Salinity_CTD', 'O2_CTD_mLL', 'PAR', 'Chl_Fluor_Voltage')
bottle_dtypes <- c('Chl_a_Holm-Hansen_F', 'NH3_Tech_F', 'NO2_Tech_F', 'NO2NO3_Tech_F', 'NO3_Tech_F', 'O2_Electrode', 'O2_Winkler', 'Phaeo_Holm-HansenF', 'PO4_Tech_F', 'Salinity_Sal_PSS', 'SiO4_Tech_F')

dis_headr_gear_seq <- list()



for (j in 1:length(ff$DATA_TYPE_METHOD)){
  
  if(ff$DATA_TYPE_METHOD[j] %in% ctd_dtypes){
dis_headr_gear_seq[j] <- 90000065
  }
  
  if(ff$DATA_TYPE_METHOD[j] %in% bottle_dtypes){
    dis_headr_gear_seq[j] <- 90000019
  }
}

created_date <- format(as.POSIXct(strptime(Sys.Date(),"%Y-%m-%d",tz="UTC")) ,format = "%d-%b-%Y")


####pull from OSCcruise database####
##using csv of data pulled by Gordana, May 14 2019

oscFile <- read.csv('groundfish_missions_osc.csv', stringsAsFactors = F)
osc <- oscFile[ oscFile$MISSION_NAME == mission, ]
remove(oscFile)
#osc=mission_info(mission1)


##check for multiple legs
if (length(osc$LEG) > 1){
  #isolate dates where legs switch
  legDates <- list()
  for (k in 1:length(osc$LEG)){
    legDates[[k]] <- list('start' = osc$MISSION_SDATE[k], 'end'= osc$MISSION_EDATE[k])
  }
  
  #find differences between legs
  oscDiff <- list()
  for (k in 1:length(osc)){
    if (length(unique(osc[[k]])) > 1){
      cat('\n >>>>>>')
      cat(paste('Caution! Multiple legs present in cruise', mission))
      cat('\n Unique value for different legs:')
      print(unique(osc[k]))
      cat('\n >>>>>>')
      oscDiff[k] <- names(osc[k])
    }
    
  }
  oscDiff <- na.omit(unlist(oscDiff))
  
  combine <- readline(paste('Would you like to combine legs for ', mission, '? (y/n)'))
  if(combine == 'y'){
    for(k in 1:length(oscDiff)){
      print(paste(oscDiff[k]))
      cat(paste('\n >> ', osc[[oscDiff[k]]] ))
      cat('\n')
      #choose which value to use and fill 
      ch <- readline(paste('Please choose which value to use for', mission, oscDiff[k],'(numeric)'))
      osc[[oscDiff[k]]] <- osc[[oscDiff[k]]][as.numeric(ch)]
    }
    #take only first row so length for data frame works
    osc <- osc[1,]
  } else {
    #seperate by date in order for each leg 
    oscNew <- as.list(names(osc))
    names(oscNew) <- oscNew
    for (k in 1:length(osc)){
      #if values are all the same, cut down to one value
      if (length(unique(osc[[k]])) == 1){
        oscNew[[k]]<- osc[[k]][1]
      } else{
      #if values differ
      
      oscNew[[k]] <- osc[[k]]
      print('WARNING!! This option is not fully coded yet!!')
        }
    }
    
    osc <- oscNew
  }
}


osc_info=osc[, !names(osc) %in% c("MISSION_DESCRIPTOR","LEG")] # remove MISSION_DESCRIPTOR and LEG column
osc_info$MISSION_GEOGRAPHIC_REGION=gsub("," , "-" ,osc_info$MISSION_GEOGRAPHIC_REGION) # replace any commas with semi-column
tosc=t(osc_info) # transposed osc info table



#put all fields into final data frame

#bcs header

header=NULL

header$MISSION_DESCRIPTOR=osc$MISSION_DESCRIPTOR  
header$EVENT_COLLECTOR_EVENT_ID=   event_collector_event_id
header$EVENT_COLLECTOR_STN_NAME= event_collector_stn_name
  
header$MISSION_NAME= mission

header$MISSION_LEADER=osc_info$MISSION_LEADER                    # mission leader from OSCCRUISE database
header$MISSION_SDATE=format(as.POSIXct(strptime(osc_info$MISSION_SDATE,"%Y-%m-%d ",tz="UTC" )), format = '%d-%b-%Y')         # mission start date from OSCCRUISE database
header$MISSION_EDATE=format(as.POSIXct(strptime(osc_info$MISSION_EDATE,"%Y-%m-%d ",tz="UTC" )), format = '%d-%b-%Y')       # mission end date from OSCCRUISE database

header$MISSION_INSTITUTE=mission_institute                               # mission institute is BIO (hardcoded)
header$MISSION_PLATFORM=osc_info$MISSION_PLATFORM                # mission platform from OSCCRUISE database
header$MISSION_PROTOCOL= mission_protocol                                # mission protocol from Files_for_DIS_header.csv file
header$MISSION_GEOGRAPHIC_REGION=osc_info$MISSION_GEOGRAPHIC_REGION # mission geographic region from OSCCRUISE database
header$MISSION_COLLECTOR_COMMENT1=mission_collector_comment1              # comments if cruise has more than 1 leg, otherwise empty
header$MISSION_COLLECTOR_COMMENT2=""                             # empty
header$MISSION_DATA_MANAGER_COMMENT=mission_data_manager_comment   # hardcoded

#header$EVENT_SDATE=format(final$sdate_time_bl,"%d-%b-%Y")        # start date from BRIDGE LOG
header$EVENT_SDATE= event_sdate # start date from CTD metadata
header$EVENT_EDATE= event_edate    # last bottle from QAT file for CTD
header$EVENT_STIME= event_stime               # start time from CTD metadata in HHMM FORMAT
header$EVENT_ETIME= event_etime               # last bottle time from QAT file for CTD IN HHMM format
header$EVENT_MIN_LAT=round(event_min_lat, digits=6)  # min start lat from BRIDGE LOG and last bottle QAT file lat
header$EVENT_MAX_LAT=round(event_max_lat, digits=6)  # max start lat from BRIDGE LOG and last bottle QAT file lat
header$EVENT_MIN_LON=round(event_min_lon, digits=6)  # min start lon from BRIDGE LOG and last bottle QAT file lon
header$EVENT_MAX_LON=round(event_max_lon, digits=6)  # max start lon from BRIDGE LOG and last bottle QAT file lon
header$EVENT_UTC_OFFSET= event_utc_offset                                       # UTC offset is always ZERO (BioChem convention)
header$EVENT_COLLECTOR_COMMENT1= event_collector_comment1                # comments from BRIDGE LOG
header$EVENT_COLLECTOR_COMMENT2= ""                              # empty
header$EVENT_DATA_MANAGER_COMMENT= ""                            # empty

header$DIS_HEADR_SDATE=dis_headr_sdate    # date from QAT file 
header$DIS_HEADR_EDATE= dis_headr_edate                   # end date is same as start date (from QAT file)
header$DIS_HEADR_STIME=dis_headr_stime               # time from QAT file (each bottle has different time)
header$DIS_HEADR_ETIME= dis_headr_etime                    # end time is same as start time (from QAT file)
header$DIS_HEADR_TIME_QC_CODE= dis_headr_time_qc_code                                 # 0 means no quality control !!!! TO BE DECIDED- WHEN TO ASSIGN FLAGS???
header$DIS_HEADR_SLAT=round(dis_headr_slat, digits=6)                              # lat from QAT file
header$DIS_HEADR_ELAT=round(dis_headr_elat, digits=6)                      # end lat same as start lat (from QAT file)
header$DIS_HEADR_SLON=round(dis_headr_slon, digits=6)                              # lon from QAT file
header$DIS_HEADR_ELON=round(dis_headr_elon, digits=6)                      # end lon same as start lon (from QAT file)
header$DIS_HEADR_POSITION_QC_CODE=0                              # !!!! TO BE DECIDED- WHEN TO ASSIGN FLAGS???
header$DIS_HEADR_START_DEPTH=dis_headr_start_depth                      # Bottle depth from BiolSum for bottle and CTD or QAT pressure for CTD data only
header$DIS_HEADR_END_DEPTH=header$DIS_HEADR_START_DEPTH          # end bottle depth same as start bottle depth (from BiolSum)
header$DIS_HEADR_SOUNDING=dis_headr_sounding                     # Sounding from BRIDGE LOG
header$DIS_HEADR_COLLECTOR_DEPLMT_ID= ""                         # empty
header$DIS_HEADR_COLLECTOR_SAMPLE_ID=dis_headr_collector_sample_id             # Sample ID from BiolSum
header$DIS_HEADR_COLLECTOR=dis_headr_collector                     # from Files_for_DIS_header.csv file
header$DIS_HEADR_COLLECTOR_COMMENT1= dis_headr_collector_comment1
header$DIS_HEADR_DATA_MANAGER_COMMENT= dis_headr_data_manager_comment
header$DIS_HEADR_RESPONSIBLE_GROUP=dis_headr_responsible_group             # from Files_for_DIS_header.csv file
header$DIS_HEADR_SHARED_DATA=""                                  # empty
header$CREATED_BY=created_by                                    # INPUT BY USER
header$CREATED_DATE=created_date                     # system date when header is created
header$DATA_CENTER_CODE=20                                       # hard coded, 20 means BIO
header$PROCESS_FLAG="NR"                                         # hard coded
header$BATCH_SEQ=batch_seq                                              # empty, will be assign when loaded



#for any empty columns
#
#populate with NULL instead
for ( l in 1:length(header)){
  if (length(header[[l]]) == 0){
    
     header[l] <- 'NULL'
  }
}



#repeat fields to make full table
for (n in names(header)){
  if(length(header[[n]]) == 1){
    header[[n]] <- rep_len(x = header[[n]], length.out = length(header$EVENT_COLLECTOR_EVENT_ID))
  }
}

# header is a list. convert list to data frame
h=data.frame(header, stringsAsFactors=FALSE)

#add in gear seq and remove data removed during stack_bcdata
#h_undup <- h[!duplicated(h$DIS_HEADR_COLLECTOR_SAMPLE_ID), ]

gear_seq <- as.data.frame(cbind(unlist(dis_headr_gear_seq), ff$id), stringsAsFactors = F)
names(gear_seq) <- c('DIS_HEADR_GEAR_SEQ', 'id')
md=merge(h,gear_seq, by.x="DIS_HEADR_COLLECTOR_SAMPLE_ID",by.y="id", all.y = T) 




# add DIS_SAMPLE_KEY_VALUE to the first column
hf=merge(md, sample_key, by.x = 'DIS_HEADR_COLLECTOR_SAMPLE_ID', by.y = 'id', all.x = T)

hf_new <- hf[!duplicated(hf$DIS_HEADR_COLLECTOR_SAMPLE_ID),]

hf_new <- hf_new[!is.na(hf_new$EVENT_SDATE),]
#remove NA values
# if (length(t_ind) > 0 ){
# hf_qc <- hf_new[-t_ind,]
# }
#save file

ofp <- file.path(outpath,paste0(mission,"_BCS.csv"))
write.csv(hf_new,  row.names = F, file = ofp,  na="", quote=FALSE)

cat(c("\n","\n"))
cat(paste("-> BCS file created:",paste0(mission,"_BCS.csv")))
cat(c("\n","\n"))


}


