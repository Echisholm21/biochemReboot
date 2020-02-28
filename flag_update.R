# incorporate reviewed flags
# E. Chisholm
# February 2020

# update BCD and BCS files

# update flag summary files

##change flag in BCD to match reviewed flag


library(tidyverse)
library(reshape)
library(plyr)

# directory path of cruise folders containing BCD, BCS and flag review files
bp <- 'E:/BioChem QC/2013+'

data_manager_comment <- 'CTD Oxygen uncalibrated - use with caution.'

#find all flagged BCD

bcd_all_fn <- list.files(bp , pattern = 'BCD_flagged.csv', recursive = TRUE, full.names = TRUE)

#bcs_all_fn <- list.files(bp, pattern = 'BCS', recursive = TRUE, full.names = TRUE)

#find all reviewed flag files

flags_all_fn <- list.files(bp, pattern = 'BCD_flagged_review.csv', recursive = TRUE, full.names = TRUE)

sumtab_all <- data.frame(mission=character(), method=character(),f0=numeric(), f1=numeric(), f2=numeric(),
                         f3=numeric(), f4=numeric(), f7=numeric(), stringsAsFactors=FALSE)

missions <- list.files(bp)

summary_df <- data.frame(mission=character(), method=character(),event=numeric(), key_value = character(), original_flag=numeric(), updated_flag=numeric(),
                         comment=character(), stringsAsFactors=FALSE)

#loop through files####

for (i in 1:length(bcd_all_fn)){
  mission <- missions[i]
  bcd_fn <- bcd_all_fn[i]
  flags_fn <- flags_all_fn[i]
  #bcs_fn <- bcs_all_fn[i]
  
  
  #read in old bcd
  bcd <- read.csv(bcd_fn, stringsAsFactors = FALSE)
  
  #read in reviewed flags
  flags <- read.csv(flags_fn, stringsAsFactors = FALSE)
  
  #check cruises match
  
  if(!is.na(flags$MISSION_DESCRIPTOR[1])){
  if (flags$MISSION_DESCRIPTOR[1] != bcd$MISSION_DESCRIPTOR[1]){
    print('Mismatched cruises!')
    print(paste('Find Flag review that matches', bcd$MISSION_DESCRIPTOR[1]))
    flags_fn <- file.choose()
    
    flags <- read.csv(flags_fn, stringsAsFactors = FALSE)
  }
  
  #remove NA lines where flags will remain the same
  flag_f <- flags[!is.na(flags$REVIEWED_DATA_QC_CODE) ,]
  
  #replace flags with reviewed values
  for (ii in 1:length(flag_f$DIS_SAMPLE_KEY_VALUE)){
    bcd$DIS_DETAIL_DATA_QC_CODE[bcd$DIS_SAMPLE_KEY_VALUE == flag_f$DIS_SAMPLE_KEY_VALUE[ii]
                                & bcd$DATA_TYPE_METHOD == flag_f$DATA_TYPE_METHOD[ii]] <- flag_f$REVIEWED_DATA_QC_CODE[ii]
    
  }
  
  #flag all electrode data
  bcd$DIS_DETAIL_DATA_QC_CODE[bcd$DATA_TYPE_METHOD == 'O2_Electrode'] <- 4
  
  #add in data manager comment
  
  # #read bcs
  #bcs <- read.csv(bcs_fn, stringsAsFactors = FALSE)
  
  #comm <- bcs$DIS_HEADR_DATA_MANAGER_COMMENT

  #new_comm <- paste(comm, data_manager_comment)
  
  #bcs$DIS_HEADR_DATA_MANAGER_COMMENT <- new_comm
  
  #write.csv(bcs, file = bcs_fn, quote = FALSE, row.names = FALSE)
  
  ###WRITE NEW BCD TABLE####
  
  write.csv(bcd, file = bcd_fn, quote = FALSE, row.names = FALSE)
  
  #summary table for the cruise, describing how many flags for each parameter
  sumtab=table(bcd$DATA_TYPE_METHOD, as.numeric(bcd$DIS_DETAIL_DATA_QC_CODE))
  
  tabname=file.path(bp, mission,paste0(mission,"_flag_summary_rev.csv"))
  
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
  
  ## add summary dataframe with comments
  
  tt <-
    data.frame(
      mission = flag_f$MISSION_DESCRIPTOR,
      method = flag_f$DATA_TYPE_METHOD,
      event = flag_f$EVENT_COLLECTOR_EVENT_ID,
      key_value = flag_f$DIS_SAMPLE_KEY_VALUE,
      original_flag = flag_f$DIS_DETAIL_DATA_QC_CODE,
      updated_flag = flag_f$REVIEWED_DATA_QC_CODE,
      comment = flag_f$COMMENT, 
      stringsAsFactors = FALSE
    )
  
  summary_df <- rbind(tt, summary_df)
  } # end skip loop if no flags in cruise
}


#change NA to 0 in summary table for the flags

sumtab_all[is.na(sumtab_all)]=0

# aggregate results for all cruises, each method separately
all_flags=aggregate(.~method,sumtab_all[,2:8],sum)

write.csv(all_flags,"all_flags_summary_rev.csv", row.names=F)

# write out all flag review comments
write.csv(summary_df, "flag_review_comments.csv", row.names = FALSE)
