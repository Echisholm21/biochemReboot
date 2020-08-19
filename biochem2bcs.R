# BioChem pull to BCS format
# E. Chisholm
# February 2020

library(dplyr)
# read in biochem data pull
creator <- 'E. Chisholm'
bc_table <- read.csv('E:/BioChem QC/BC_Groundfish/BCS_Groundfish_discrete.csv', stringsAsFactors = FALSE)
# read template

colmap <- read.csv('BCS_BC_map.csv', stringsAsFactors = FALSE)

#split by cruise for ease

descriptors <- unique(bc_table$DESCRIPTOR)

for (i in 1:length(descriptors)){
  
  desc <- descriptors[i]
  
  data <- bc_table[bc_table$DESCRIPTOR == desc,]
  
  cruise <- unique(data$NAME)
  
  # check cruise name is valid
  if(nchar(cruise) != 10){
    cn <- readline(paste('Validate cruise name : ', cruise, ' (y/n) '))
    if(cn == 'n'){
      print(paste(' Name: ', unique(data$NAME)))
      print(paste('Descriptor: ', unique(data$DESCRIPTOR)))
      print(paste('Platform : ', unique(data$PLATFORM)))
      print(paste('Events : ', unique(data$COLLECTOR_EVENT_ID)))
      new_name <- readline('Input new cruise name? ')
      
      cruise <- new_name
    }
  }
  
  # check IML event IDs
  
  if(nchar(data$COLLECTOR_EVENT_ID) > 3){
    print('Warning, strange sample IDs')
    cat('\n')
    print(unique(data$COLLECTOR_EVENT_ID))
    cat('\n')
    print('Attempt to rectify....')
    cat('\n')
    
    events <- data$COLLECTOR_EVENT_ID
    e2 <- stringr::str_split(events, pattern = substr(cruise, 4, nchar(cruise)))
    e3 <- list()
    for(ii in 1:length(e2)){
      e3[[ii]] <- e2[[ii]][2]
    }
    
    new_event <- unlist(e3)
    #new_event <- sprintf('%03d', as.numeric(e3))
    
    
    print(paste('New events: ', unique(new_event)))
    data$COLLECTOR_EVENT_ID <- new_event
  }
  
  # convert to BCS format
  
  
  
  bcs_names <- colmap$BCS[colmap$BC %in% names(data)]
  
  cat(paste0('Creating BCS for ', desc, ' with columns : ', unlist(bcs_names), '\n'))
  
  # rename bc c olumns to bcd columns
  bcs_cols <- list()
  for (ii in 1:length(names(data))){
    
    ind <- match(names(data)[ii], colmap$BC)
    
    if( !is.na(ind)){
      
      names(data)[ii] <- colmap$BCS[ind]
      
      bcs_cols[[ii]] <- ii
    }
  }
  
  bcs_cols <- unlist(bcs_cols)
  
  bcs_dat <- data[bcs_cols]
  
  # create missing bcs columns
  
  bcs_dat_c <- bcs_dat %>%
    dplyr::mutate(., CREATED_BY = creator) %>%
    dplyr::mutate(., CREATED_DATE = Sys.Date()) %>%
    dplyr::mutate(., PROCESS_FLAG = 'NR') %>%
    dplyr::mutate(., DIS_HEADER_COLLECTOR_SAMPLE_ID = data$DIS_HEADER_COLLECTOR_SAMPLE_ID) %>%
    dplyr::mutate(., BATCH_SEQ = substr(bcs_dat$MISSION_DESCRIPTOR, nchar(bcs_dat$MISSION_DESCRIPTOR) - 4, nchar(bcs_dat$MISSION_DESCRIPTOR))) %>%
    dplyr::mutate(., DIS_SAMPLE_KEY_VALUE = paste(BATCH_SEQ, EVENT_COLLECTOR_EVENT_ID, DIS_HEADR_COLLECTOR_SAMPLE_ID, sep = '_')) %>%
    dplyr::mutate(., DIS_HEADR_COLLECTOR_DEPLMT_ID = NA) %>%
    dplyr::mutate(., DIS_HEADR_RESPONSIBLE_GROUP = 'AZMP') %>%
    dplyr::mutate(., DATA_CENTER_CODE = 23)
  
  # order columns
  
  bcs_dat_c <- bcs_dat_c %>%
    select(., DIS_HEADR_COLLECTOR_SAMPLE_ID,	MISSION_DESCRIPTOR,	EVENT_COLLECTOR_EVENT_ID,	EVENT_COLLECTOR_STN_NAME,	MISSION_NAME,
           MISSION_LEADER,	MISSION_SDATE,	MISSION_EDATE,	MISSION_INSTITUTE,	MISSION_PLATFORM,	MISSION_PROTOCOL,	MISSION_GEOGRAPHIC_REGION,
           MISSION_COLLECTOR_COMMENT1,	MISSION_COLLECTOR_COMMENT2,	MISSION_DATA_MANAGER_COMMENT,	EVENT_SDATE,	EVENT_EDATE,	EVENT_STIME,
           EVENT_ETIME,	EVENT_MIN_LAT,	EVENT_MAX_LAT,	EVENT_MIN_LON,	EVENT_MAX_LON,	EVENT_UTC_OFFSET,	EVENT_COLLECTOR_COMMENT1,	EVENT_COLLECTOR_COMMENT2,
           EVENT_DATA_MANAGER_COMMENT,	DIS_HEADR_SDATE,	DIS_HEADR_EDATE,	DIS_HEADR_STIME,	DIS_HEADR_ETIME,	DIS_HEADR_TIME_QC_CODE,	DIS_HEADR_SLAT,
           DIS_HEADR_ELAT,	DIS_HEADR_SLON,	DIS_HEADR_ELON,	DIS_HEADR_POSITION_QC_CODE,	DIS_HEADR_START_DEPTH,	DIS_HEADR_END_DEPTH,	DIS_HEADR_SOUNDING,
           DIS_HEADR_COLLECTOR_DEPLMT_ID,	DIS_HEADR_COLLECTOR,	DIS_HEADR_COLLECTOR_COMMENT1,	DIS_HEADR_DATA_MANAGER_COMMENT,	DIS_HEADR_RESPONSIBLE_GROUP,
           DIS_HEADR_SHARED_DATA,	CREATED_BY,	CREATED_DATE,	DATA_CENTER_CODE,	PROCESS_FLAG,	BATCH_SEQ,	DIS_HEADR_GEAR_SEQ,	DIS_SAMPLE_KEY_VALUE

    )
  
  # check for commas which cause erros in csv format
  
  for ( ii in 1:length(bcs_dat_c[1,])){
    
    bcs_dat_c[,ii] <- gsub(pattern = ',', replacement = ' - ', bcs_dat_c[ , ii])
    
  }
  # export BCD
  
  save_folder <- file.path('E:/BioChem QC/2013+', cruise)
  dir.create(save_folder, recursive = TRUE, showWarnings = FALSE)
  write.csv(bcd_dat_c, paste0(save_folder, '/', cruise, '_BCD.csv'), row.names = FALSE, quote = FALSE)
  
  cat(paste('BCS file saved for ', cruise), '! \n')
}


