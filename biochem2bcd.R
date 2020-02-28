# Quality Control on biochem data 2013 - 2019
# E. Chisholm
# February 2020

library(dplyr)
# read in biochem data pull

bc_table <- read.csv('C:/Users/ChisholmE/2013_discrete.csv', stringsAsFactors = FALSE)

#split by cruise for ease

descriptors <- unique(bc_table$DESCRIPTOR)

for (i in 1:length(descriptors)){

  desc <- descriptors[i]
  
  data <- bc_table[bc_table$DESCRIPTOR == desc,]

  cruise <- unique(data$NAME)
  
  # check cruise name is valid
  if(nchar(cruise) > 10){
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
    print('Warning, IML sample IDs')
    cat('\n')
    print(unique(data$COLLECTOR_EVENT_ID))
    cat('\n')
    print('Attempt to rectify....')
    cat('\n')
    
    events <- data$COLLECTOR_EVENT_ID
   e2 <- stringr::str_split(events, pattern = cruise)
   e3 <- list()
   for(ii in 1:length(e2)){
     e3[[ii]] <- e2[[ii]][2]
   }
   
  new_event <- unlist(e3)
   #new_event <- sprintf('%03d', as.numeric(e3))
   
   
   print(paste('New events: ', unique(new_event)))
   data$COLLECTOR_EVENT_ID <- new_event
    }
  
# convert to BCD format


colmap=read.csv("bcd_bc_column_names.csv", stringsAsFactors = F)

bcd_names <- colmap$BCD_COLUMN_NAME[colmap$BC_COLUMN_NAME %in% names(data)]

cat(paste0('Creating BCD for ', desc, ' with columns : ', unlist(bcd_names), '\n'))

# rename bc c olumns to bcd columns
bcd_cols <- list()
for (ii in 1:length(names(data))){
  
  ind <- match(names(data)[ii], colmap$BC_COLUMN_NAME)
  
  if( !is.na(ind)){
    
    names(data)[ii] <- colmap$BCD_COLUMN_NAME[ind]
    
    bcd_cols[[ii]] <- ii
  }
}

bcd_cols <- unlist(bcd_cols)

bcd_dat <- data[bcd_cols]

# create missing bcd columns

bcd_dat_c <- bcd_dat %>%
  dplyr::mutate(., DIS_DATA_NUM = seq(1:length(bcd_dat[,1]))) %>%
  dplyr::mutate(., CREATED_BY = 'E. CHISHOLM') %>%
  dplyr::mutate(., CREATED_DATE = Sys.Date()) %>%
  dplyr::mutate(., PROCESS_FLAG = 'NR') %>%
  dplyr::mutate(., BATCH_SEQ = substr(bcd_dat$MISSION_DESCRIPTOR, nchar(bcd_dat$MISSION_DESCRIPTOR) - 4, nchar(bcd_dat$MISSION_DESCRIPTOR))) %>%
  dplyr::mutate(., DIS_SAMPLE_KEY_VALUE = paste(BATCH_SEQ, EVENT_COLLECTOR_EVENT_ID, DIS_DETAIL_COLLECTOR_SAMP_ID, sep = '_'))

# order columns

bcd_dat_c <- bcd_dat_c %>%
  select(., DIS_DATA_NUM,	MISSION_DESCRIPTOR,	EVENT_COLLECTOR_EVENT_ID,	EVENT_COLLECTOR_STN_NAME,	DIS_HEADER_START_DEPTH,	DIS_HEADER_END_DEPTH,
         DIS_HEADER_SLAT,	DIS_HEADER_SLON,	DIS_HEADER_SDATE,	DIS_HEADER_STIME,	DIS_DETAIL_DATA_TYPE_SEQ,	DATA_TYPE_METHOD,
         DIS_DETAIL_DATA_VALUE,	DIS_DETAIL_DATA_QC_CODE,	DIS_DETAIL_DETECTION_LIMIT,	DIS_DETAIL_DETAIL_COLLECTOR,
         DIS_DETAIL_COLLECTOR_SAMP_ID,	CREATED_BY,	CREATED_DATE,	DATA_CENTER_CODE,	PROCESS_FLAG,	BATCH_SEQ,	DIS_SAMPLE_KEY_VALUE
)

# check for commas which cause erros in csv format

for ( ii in 1:length(bcd_dat_c[1,])){
  
bcd_dat_c[,ii] <- gsub(pattern = ',', replacement = ' - ', bcd_dat_c[ , ii])

}
# export BCD

save_folder <- file.path('E:/BioChem QC/2013+', cruise)
dir.create(save_folder, recursive = TRUE, showWarnings = FALSE)
write.csv(bcd_dat_c, paste0(save_folder, '/', cruise, '_BCD.csv'), row.names = FALSE, quote = FALSE)

cat(paste('BCD file saved for ', cruise), '! \n')
}


