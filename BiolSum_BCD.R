####translate BiolSum files from groundfish cruises 
#### output BCD & BCS files
##modified from Gordana Lazin's AZMP BCD conversion scripts
##E. Chisholm April 15, 2019

####PROGRESS####
#==============#
#     32/32     #
#==============#
#note: need to reprocess 1:7 (null sounding column rather than remove) #done
#work around data allocation limit

##=== Error Log ===##
#### NED2003036 (done)     size
#### NED2005027 (done) size , 1250433 records per data type (double check w Shelley - duplicated sample IDs)
#### TEM2008775 (done) has multiple legs, need to code for this possibility, combined osc data
#### NED2010001 (done) error with bcs creation- rows for multi legs - comments from M.Kennedy -- MESSY. EC:  Combined metadata based on info from W. Maceachern w/ help from S. Bond
#### NED2012002 (done) error with bcs creation- rows for multi legs, combined osc data
#### NED2013002 (done) error in data types, one variable as character was preventing proper processing


#==============

#January 2020 EC
#Discovered error when attempting to load to staging tables
#TEM2008775, index = 19
#BCS file is missing 5 columns
#recreated BCS file



####package and source intro####
#library(readxl)

require(xlsx)
require(tidyr)
require(lubridate)
require(reshape)
#source Gordana's funtions
source('E:/BioChem QC/Working/stack_bcdata_EC.r')
source('E:/BioChem QC/Working/bcs_gfEC.R')


####read in biolsum files####
# ======== #
# BiolSum
# ======== #

fp="R:/Science/BIODataSvc/SRC/BIOCHEMInventory/Data_by_Year_and_Cruise/Files_for_DIS_header.xlsx"
files=read.xlsx(fp, sheetName = 'GROUNDFISH', stringsAsFactors =F )

# select files for this particular mission
# files=files[files$mission==mission,]
####START LOOP####
for ( i in 1:length(files$mission)){

BiolSum=file.path(files$path,files$biolsum)
mission <- files$mission[i]

####clean up biolsum format####
# read BiolSum
bsum_o=read.xlsx(BiolSum[i], sheetName ="BIOLSUMS_FOR_RELOAD", stringsAsFactors =F)
#WARNING# 
          #STRINGS AS FACTORS MUST BE SET TO FALSE OR SCRIPT WILL NOT RUN,
          #THIS IS KEY!
#bsum=clean_xls(bsum) # clean excel file

# define columns with metadata
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
  "id.1",
  "event.1",
  "upload_date",
  "upload_time",
  "ctd_date_time",
  "comments"
)

# keep only data column
bsum=bsum_o[,!(names(bsum_o) %in% bsum_meta)]

# fix the header. excel reader reads - as . for some reason
names(bsum)=gsub("Holm.Hansen","Holm-Hansen",names(bsum))


#remove CTD sigma T and duplicate id

gg <- grep(names(bsum), pattern = 'dens1')
bsum <- bsum[-gg]

# rename o2_mll to electrode mll datatype
mll=which(names(bsum)=="o2_ml")

if (length(mll)>0) {
  names(bsum)[mll]="O2_Electrode_mll"
}


#bug fix for NED2012022
if(mission == 'NED2012022'){
  #clean up biolsum data
  bsum$SiO4_Tech_F <- as.numeric(bsum$SiO4_Tech_F)
  bsum$PO4_Tech_F <- as.numeric(bsum$PO4_Tech_F)  
  bsum$NO2_Tech_F <- as.numeric(bsum$NO2_Tech_F)
  bsum$id <- as.numeric(bsum$id)
  bsum <- bsum[1:18]
  bsum$stratum <- as.numeric(bsum$stratum)
}

if (mission == 'NED2013022'){
  bsum$stratum <- as.numeric(bsum$stratum)
  bsum$`Chl_a_Holm-Hansen_F` <- as.numeric(bsum$`Chl_a_Holm-Hansen_F`)
  
  }
#bsum[['stratum']] <- as.numeric(bsum[['stratum']]) #weird bug in NED2013002 where tsratum values as character was preventing dataframe stacking
# make stacked dataset with data type sequences using stack_bcdata function
mbs <- stack_bcdata_EC(bsum) 
#Emily's version of function does not require query to biochem

#remove(bsum)
####create BCD file####
# =================== #
#   CREATE BCD FILE
# ------------------- #
# in other script would combine data from other files into ff,
# for groundfish, only source is bsum
ff <- mbs
#remove(mbs)

# rename the columns to proper names
names(ff)=gsub("variable","DATA_TYPE_METHOD",names(ff))
names(ff)=gsub("value","DIS_DETAIL_DATA_VALUE",names(ff))


#### create bcs file ####

create_bcs(mission, ff, bsum_o)

#read in bcs
bcsI <- list.files(path = paste0('D:/Biochem QC/data/',mission), pattern = 'bcs', ignore.case = T)
bcs <- read.csv(paste0('D:/Biochem QC/data/',mission,'/', bcsI), stringsAsFactors = F,  header = T, row.names = NULL)


#### compare BCD and BCS####

# check if there is any difference in IDs between BCS and data file
diff=setdiff(unique(bcs$DIS_HEADR_COLLECTOR_SAMPLE_ID),unique(ff$id))
cat('\n')
cat(diff)
cat('\n')

 
# merge BCS and data file by sample ID
mf=merge(bcs,ff, by.x="DIS_HEADR_COLLECTOR_SAMPLE_ID",by.y="id", all.x = T )

#check for duplicate erros

dup <- duplicated(mf$DIS_DETAIL_DATA_VALUE)

print(mf[dup == T,c("DIS_SAMPLE_KEY_VALUE", 'DATA_TYPE_METHOD', 'DIS_DETAIL_DATA_VALUE')])

remove(bcs, ff)

# check if all the samples have values
which(is.na(mf$DIS_DETAIL_DATA_VALUE))


# add columns for BCD file
mf$DIS_DATA_NUM=seq(1,dim(mf)[1],1)
mf$DIS_DETAIL_DATA_QC_CODE=0
mf$DIS_DETAIL_DETECTION_LIMIT=NA
mf$PROCESS_FLAG="NR"
mf$BATCH_SEQ=0
mf$CREATED_BY= 'E. Chisholm'
mf$CREATED_DATE=as.character(now())

####merge BCD and BCS####
# 3. make BCD data file: order the columns and rename if necessary

# name of the columns for BCD file
cols=c("DIS_DATA_NUM","MISSION_DESCRIPTOR","EVENT_COLLECTOR_EVENT_ID","EVENT_COLLECTOR_STN_NAME",
       "DIS_HEADR_START_DEPTH","DIS_HEADR_END_DEPTH","DIS_HEADR_SLAT","DIS_HEADR_SLON",
       "DIS_HEADR_SDATE","DIS_HEADR_STIME","DATA_TYPE_SEQ","DATA_TYPE_METHOD","DIS_DETAIL_DATA_VALUE",
       "DIS_DETAIL_DATA_QC_CODE","DIS_DETAIL_DETECTION_LIMIT","DIS_HEADR_COLLECTOR","DIS_HEADR_COLLECTOR_SAMPLE_ID",
       "CREATED_BY","CREATED_DATE","DATA_CENTER_CODE","PROCESS_FLAG","BATCH_SEQ","DIS_SAMPLE_KEY_VALUE")

# match orcer of BCD columns to the col vector
mm=match(cols,names(mf))

# BCD file is created with proper order of the columns
bcd=mf[,mm]

remove(mf)
# rename specific fields

# rename:
names(bcd)[which(names(bcd)=="DIS_HEADR_COLLECTOR")]="DIS_DETAIL_DETAIL_COLLECTOR"
names(bcd)[which(names(bcd)=="DIS_HEADR_COLLECTOR_SAMPLE_ID")]="DIS_DETAIL_COLLECTOR_SAMP_ID"
names(bcd)[which(names(bcd)=="DATA_TYPE_SEQ")]="DIS_DETAIL_DATA_TYPE_SEQ"


# replace HEADR with HEADER
names(bcd)=gsub("HEADR","HEADER",names(bcd))

# now bcd file is ready
#### save bcd file ####
bcd_filename=file.path('D:/BioChem QC/data/',mission,paste0(mission,"_BCD.csv"))
write.csv(bcd, bcd_filename, row.names = F)


#    DONE WITH BCD FILE    #
# ======================== #
#============================================#
cat(c("\n","\n"))
cat(paste("-> BCD file created:",bcd_filename))
cat(c("\n","\n"))
cat(paste0("-> Data in BCD for ", mission,":") )
cat(c("\n","\n"))
tt=data.frame(table(bcd$DATA_TYPE_METHOD))
names(tt)=c("Data Type","Number of Records")
print(tt)

remove(tt)
#### END LOOP####
}
