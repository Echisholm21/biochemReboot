# save dataframe as table in BioChem

# requires bcd and bcs formats files
require("RODBC")

setwd("~/GitHub/iml-qc")


source("RODBC_write_df2oracle.r")
source("df2oracle_formats.r")
Sys.setenv(TZ = "GMT")
Sys.setenv(ORA_SDTZ = "GMT")

options(scipen = '999')



#set biochem username and password

load('biochemlogin.RData')

# set directory for data files
bp <- 'E:/BioChem QC/2013+'

# path in working directory
#groundfish cruises
mission_list <- list.files(bp)


for(i in 1:length(mission_list)){
  
  mission <- mission_list[i]

  cat(paste('Mission: ', mission))
  #object mission needs to be defined
  wdp=file.path(bp,mission)
  
  # write a message on the screen
  cat("\n","\n")
  #cat(paste("-> Loading BCS file:",file.path(wdp,paste0(mission,"_BCS.csv"))))
  cat("\n","\n")
  cat(paste("-> Loading BCD file:",file.path(wdp,paste0(mission,"_BCD_flagged.csv"))))
  cat("\n","\n")
  # load BCS file and file formats
  #bcsf=file.path(wdp,paste0(mission,"_BCS.csv"))
 # bcs=read.csv(bcsf,stringsAsFactors = FALSE)
  #bcs_formats=read.csv(file.path(getwd(),"bcs_formats.csv"), stringsAsFactors = F)
  
  # Load BCD file and formats
  bcdf=file.path(wdp,paste0(mission,"_BCD_flagged.csv"))
  bcd=read.csv(bcdf,stringsAsFactors = FALSE)
  bcd_formats=read.csv(file.path(getwd(),"bcd_formats.csv"),stringsAsFactors = F)
  
  # check if the column names in BCS and BCD are the same as in BioChem
  #cat('BCS CHECK')
  #identical(names(bcs),bcs_formats$COLUMN_NAME)
  cat("\n","\n")
  cat('BCD CHECK')
  identical(names(bcd),bcd_formats$COLUMN_NAME)
  cat("\n","\n")
  #reorder bcs variables
  
  #bcs <- bcs[bcs_formats$COLUMN_NAME]
  
  bcd <- bcd[bcd_formats$COLUMN_NAME]
  
  # check if the column names in BCS and BCD are the same as in BioChem
  #cat('BCS CHECK 2')
 # identical(names(bcs),bcs_formats$COLUMN_NAME)
  cat("\n","\n")
  cat('BCD CHECK 2')
  identical(names(bcd),bcd_formats$COLUMN_NAME)
  cat("\n","\n")
  
  
  # change formats to the formats required for Oracle table
 # bcs1=df2oracle_formats(bcs,bcs_formats)
  bcd1=df2oracle_formats(bcd,bcd_formats)
  
  #browser()
  
  # Create a database connection.
  con = odbcConnect( dsn="PTRAN", uid=biochem$user, pwd=biochem$password, believeNRows=F)
  
  if(con != -1){
    cat('Connection successful! \n')
  }
  
  # write BCS file to Oracle table
  #RODBC_write_df2oracle(con = con,df = bcs1,oracle_table = "AZMP_MISSIONS_BCS", format_table = bcs_formats)
  
  # write BCD file to oracle table
  RODBC_write_df2oracle(con,df = bcd1,oracle_table = "BIO_NUTRIENTS_BCD", format_table = bcd_formats)
  
  # close connection to database
  odbcClose(con)
  
  cat("\n","\n")
  cat("-> BCS and BCD sucesfully uploaded to the ORACLE staging tables. \n")
  
}