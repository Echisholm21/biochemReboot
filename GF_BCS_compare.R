# E. Chisholm
# February 14 2020
# Modified version of AZMP BCS vs BioChem comparison script written by Gordana Lazin
# specifically used to compare groundfish reboot records with biochem
# script updated to include more testing as discussed with Shelley Bond

# compare bcs csv files with what was loaded to biochem
# writes out report file with column comparison for all cruises






library(lubridate)



# this script is not working, I think what will have to happen is a full rewrite. Shelley has provided the BCD data in a table and some of the comments etc in a seperate table but it needs to be put into BCS format in order for comparison to be efficient. 
# step one is to create a BCS format from biochem pulls
# step two is to write comparison script
# step three is to create an efficient ouput spreadsheet that details issues

file_dir <- 'E:/BioChem QC/groundfish_data/'
cruises <- list.files(file_dir)
cruises <- cruises[-18] #read me

#trouble cruises
cruises <- c('NED2005027', 'NED2010001', 'NED2010002', 'NED2010027', 'NED2011002', 'NED2011025', 'TEL2004529', 'TEL2004530', 'TEL2005605', 'TEL2005633', 'TEM2004004')
  #c('18NE05027', '18NE10001', '18NE10002', '18NE10027', '18NE11002', '18NE11025', '18TL04529', '18TL04530', '18TL05605', '18TL05633', '181C04004')

cn=read.csv("GF_mission_name_descriptor.csv",stringsAsFactors = FALSE) #TODO create equivalent GF csv
desc_list=cn$MISSION_DESCRIPTOR[cn$MISSION_NAME %in% cruises]

#bc_table <- read.csv('E:/BioChem QC/BC_Groundfish/Groundfish.csv', stringsAsFactors = FALSE)

# bcs_bc contains data from all missions
bcs_bc=read.csv("E:/BioChem QC/BC_Groundfish/groundfish_bcs_2.csv",stringsAsFactors=FALSE) 

# loop through cruises and compare BCS files
for (j in 1:length(cruises)) {
  
  mission = cruises[j]
  desc <- desc_list[j]
  # =================== #
  # DEFINE REPORT FILE
  # =================== #
  
  n=lubridate::now() # make time stamp to mark the start of processing
  timestamp=paste0(year(n), sprintf("%02d",month(n)),sprintf("%02d",day(n)),
                   sprintf("%02d",hour(n)),sprintf("%02d",minute(n)),sprintf("%02d",floor(second(n))))
  
  
  # name of the report file                 
  report_file=paste0(mission,"_BCS_report_COMPARISON_",timestamp, ".txt")
  #report_file=file.path(outpath,report_file)
  
  # =========== DONE WITH REPORT FILE ==============
  # Start writing to the report file
  sink(file=report_file,append=TRUE, split=TRUE)
  
  
  
  
  
  cat(c("\n","\n"))
  cat(paste("===== COMPARISON FOR MISSION", mission, "======"))
  cat(c("\n","\n"))
  # individual cruise files
  
  bcdpath = file.path(file_dir,mission)
  bcd_file = list.files(bcdpath, pattern = '_BCD.csv', recursive = TRUE, full.names = TRUE)
  bcs_file = list.files(bcdpath, pattern = '_BCS.csv', recursive = TRUE, full.names = TRUE)
  
  #bcd=read.csv(file.path(bcdpath,bcd_file), stringsAsFactors=FALSE)
  bcs = read.csv(bcs_file, stringsAsFactors=FALSE)
  

  # get biochem data frame
  
  
 
  # bcsbc contains data from particular mission
  bcsbc=bcs_bc[which(bcs_bc$DESCRIPTOR==desc),]
  
  if(nrow(bcsbc) >0 ){
  
  # correct event IDs
  
  
  if(nchar(bcsbc$COLLECTOR_EVENT_ID) > 3){
    cat('Warning, strange sample IDs')
    cat('\n')
    print(unique(bcsbc$COLLECTOR_EVENT_ID))
    cat('\n')
    cat('Attempt to rectify....')
    cat('\n')
    
    events <- bcsbc$COLLECTOR_EVENT_ID
    e2 <- stringr::str_split(events, pattern = substr(mission, 4, nchar(mission)))
    e3 <- list()
    for(ii in 1:length(e2)){
      e3[[ii]] <- e2[[ii]][2]
    }
    
    new_event <- unlist(e3)
    #new_event <- sprintf('%03d', as.numeric(e3))
    
    cat('New Events: \n')
    print( unique(new_event))
    bcsbc$COLLECTOR_EVENT_ID <- new_event
  }
  # create dis_sample_key_value
  cat(' =======================================================================')
  cat('\n \n')
  bcsbc <- bcsbc %>%
    dplyr::mutate(., DIS_SAMPLE_KEY_VALUE = paste0(.data$NAME,'_', .data$COLLECTOR_EVENT_ID,'_', .data$COLLECTOR_SAMPLE_ID)) # TODO : wrong event ID?
  
  # bcd_bc contains data from all the missions
  #bcd_bc=read.csv("GF_BCD_BC.csv",stringsAsFactors=FALSE)
  
  # bcd contains biochem data from one particular mission
  #bcdbc=bcd_bc[which(bcd_bc$MISSION_DESCRIPTOR==desc),]
  
  # order dataframes by DIS_SAMPLE_KEY_VALUE
  bcs=bcs[with(bcs, order(DIS_SAMPLE_KEY_VALUE)), ]
  bcsbc=bcsbc[with(bcsbc, order(DIS_SAMPLE_KEY_VALUE)), ]
  
  # have to convert the dates to the same format
  
  #write.csv(bcs,"BCS_HUD99054.csv", row.names=F)
  #write.csv(bcsbc,"BCSBC_HUD99054.csv", row.names=F)
  
  # =========================== #
  # COMPARE BCS
  
  # change dates in both files
  # columns with dates
  
  idate=grep("DATE", names(bcs))
  iidate <- which(names(bcsbc) %in% c('MISSION_END', 'EVENT_START', 'HEADER_START', 'EVENT_END', 'HEADER_END', 'MISSION_START'))
  
  # change dates in BCS
  bcs[,idate]=lapply(bcs[,idate], function(x) {as.Date(x,format = "%d-%b-%Y")})
  
  # change the dates in BCS from BioChem
  bcsbc[,iidate]=lapply(bcsbc[,iidate], function(x) {as.Date(x,"%d-%b-%y")})
  
  
  # check if column by column if they are identical
  
  colmap <- read.csv('BCS_BC_map.csv', stringsAsFactors = FALSE)
 
  for (i in 1:length(colmap$BCS)){
    
    mapd <- colmap[i,]
    
    if(nchar(mapd$BC) > 0){
      n <- 0
      cat('----------------------\n')
      cat('\n')
      cat("Comparing", mapd$BCS, "to", mapd$BC, "\n")
      
      
      
      if(is.na(unique(bcs[[mapd$BCS]])) | as.character(unique(bcs[[mapd$BCS]])) == 'NULL'){
        cat('BCS column populated with NAs! \n')
        n <-1
      }
      if(is.na(unique(bcsbc[[mapd$BC]])) | as.character(unique(bcsbc[[mapd$BC]])) == 'NULL'){
        cat('BC column populated with NAs! \n')
        n <- n+1
      }
      if (n == 0){
      
      if(colmap$TYPE[i] == 'num'){
        bcs_c <- as.numeric(unique(bcs[[mapd$BCS]]))
        bc_c <- as.numeric(unique(bcsbc[[mapd$BC]]))
        
        # compare number of unique values
        if (length(bcs_c) != length(bc_c)){
          if (length(bcs_c[!bcs_c %in% bc_c]) > 0 ){
            cat(" --> Unique values present in BCS which do not exist in BioChem!\n")
            print(bcs_c[!bcs_c %in% bc_c])
            cat('\n')
            cat('Associated Key Values: \n')
            tt <- na.omit(bcs$DIS_SAMPLE_KEY_VALUE[bcs[[mapd$BCS]] == bcs_c[!bcs_c %in% bc_c]])
            attributes(tt) <- NULL
            print(tt)
            cat('\n')
          }
          if (length(bc_c[!bc_c %in% bcs_c]) > 0 ){
            cat(" --> Unique values present in BioChem which do not exist in BCS! \n")
            print(bc_c[!bc_c %in% bcs_c])
            cat('\n')
            cat('Associated Key Values: \n')
            tt <- na.omit(bcsbc$DIS_SAMPLE_KEY_VALUE[bcsbc[[mapd$BC]] == bc_c[!bc_c %in% bcs_c]])
            attributes(tt) <- NULL
            print(tt)
            cat('\n')
          }
          
          
        }else{
        
        if (!is.na(colmap$diff_comp[i]) ){
        # compare unique values
        bcs_order <- sort(bcs_c)
        bc_order <- sort(bc_c)
        
        if (bc_order == 0){
          cat('No valid values provided in BioChem records; unable to compare values. \n')
        }else{
        dd <- bcs_order - bc_order
        if (max(dd) > 0){
          cat(' --> Maximum difference of ', max(dd), '\n ' )
        }else{
          cat(' --> BioChem and BCS values match exactly! \n')
        }
        }
        }else{
          bcs_order <- sort(bcs_c)
          bc_order <- sort(bc_c)
          
          if (identical(bcs_order, bc_order)){
            cat(' --> BioChem and BCS values match exactly! \n')
          }else{
            cat('Difference detected in values : \n')
            
            if (length(bcs_c[!bcs_c %in% bc_c]) > 0 ){
              cat(" --> Unique values present in BCS which do not exist in BioChem!\n")
              print(bcs_c[!bcs_c %in% bc_c])
              cat('\n')
            }
            if (length(bc_c[!bc_c %in% bcs_c]) > 0 ){
              cat(" --> Unique values present in BioChem which do not exist in BCS! \n")
              print(bc_c[!bc_c %in% bcs_c])
              cat('\n')
            }
          }
        }
        }
      }
      
      if (colmap$TYPE[i] == 'char'){
      
        bcs_c <- stringr::str_trim(unique(bcs[[mapd$BCS]]))
        bc_c <- stringr::str_trim(unique(bcsbc[[mapd$BC]]))
      if(length(bcs_c) != length(bc_c)){
        if (length(bcs_c[!bcs_c %in% bc_c]) > 0 ){
        cat(" --> Unique values present in BCS which do not exist in BioChem!\n")
          
          print(bcs_c[!bcs_c %in% bc_c])
          
          cat('\n')
          
        }
        
        if (length(bc_c[!bc_c %in% bcs_c]) > 0){
          cat(" --> Unique values present in BioChem which do not exist in BCS! \n")
          
          print(bc_c[!bc_c %in% bcs_c])
          cat('\n')
        }
        
      }else{
      
      if(identical(bcs_c, bc_c) | length(bcs_c[!bcs_c %in% bc_c]) + length(bc_c[!bc_c %in% bcs_c]) == 0){
        cat(' --> BioChem and BCS values match exactly! \n')
      }else{
        cat(' --> Difference detected between unique values! \n')
        cat("         BCS values not in BioChem: \n")
        print(bcs_c[!bcs_c %in% bc_c])
        cat("\n")
        cat("         BioChem values not in BCS: \n")
        print(bc_c[!bc_c %in% bcs_c])
        cat("\n")
      }
      }
      }
        
        if (colmap$TYPE[i] == 'date'){
          bcs_c <- as.Date(unique(bcs[[mapd$BCS]]))
          bc_c <- as.Date(unique(bcsbc[[mapd$BC]]))
          
          if (length(bc_c[!bc_c  %in% bcs_c]) > 0){
            cat(" --> Unique values present in BCS which do not exist in BioChem!\n")
            
            print(bcs_c[!bcs_c  %in% bc_c])
            cat('\n')
            
          }
          if (length(bcs_c[!bcs_c  %in% bc_c]) > 0){
            cat(" --> Unique values present in BioChem which do not exist in BCS! \n")
            
            print(bc_c[!bc_c  %in% bcs_c])
            cat('\n')
          }
          
          if (!identical(bcs_c, bc_c)){
            vv <- bcs_c- bc_c
            cat(' --> Difference of', vv, attr(vv,which = 'units'), '\n ' )
          }else{
            cat(' --> BioChem and BCS values match exactly! \n')
          }
          
        }
        
        if (colmap$TYPE[i] == 'time'){
          bcs_c <- format(as.POSIXct(strptime(unique(bcs[[mapd$BCS]]),"%H:%M:%S",tz="UTC")) ,format = "%H:%M")
      
          bc_c <- format(as.POSIXct(strptime(sprintf('%04d',unique(bcsbc[[mapd$BC]])),"%H%M",tz="UTC")) ,format = "%H:%M")
          
         
        
          
          if(length(bcs_c) != length(bc_c)){
            if (length(bc_c[!bc_c  %in% bcs_c]) > 0){
              cat(" --> Unique values present in BCS which do not exist in BioChem!\n")
              
              print(bcs_c[!bcs_c  %in% bc_c])
              
              
            }
            if (length(bcs_c[!bcs_c  %in% bc_c]) > 0){
              cat(" --> Unique values present in BioChem which do not exist in BCS! \n")
              
              print(bc_c[!bc_c  %in% bcs_c])
            }
            cat("         BCS values: \n")
            print(bcs_c)
            cat("\n")
            cat("         BioChem values: \n")
            print(bc_c)
            cat("\n")
          }else{
          if (!identical(bcs_c, bc_c)){
            comp_bcs<- as.POSIXct(bcs_c, format = '%H:%M')
            comp_bc <- as.POSIXct(bc_c, format = '%H:%M')
            vv <- comp_bcs- comp_bc
            
            print(vv)
            
          }else{
            cat(' --> BioChem and BCS values match exactly! \n')
          }
          
          }
        }
      }
      
      if (n == 2){
        cat('Both BCS and BioChem columns are populated with NAs! \n')
      }
      
      if (n == 1){
        if(is.na(unique(bcs[[mapd$BCS]])) | unique(bcs[[mapd$BCS]]) == 'NULL'){
        cat(' --> Warning! BCS column', mapd$BCS,' missing data! \n')
        cat('BioChem values: \n')
        print(unique(bcsbc[[mapd$BC]]))
        cat('\n')
        } else{
          cat(' --> Warning! BioChem column', mapd$BC,' missing data! \n')
          cat('BCS values: \n')
          print(unique(bcs[[mapd$BCS]]))
          cat('\n')
        }
      }
      
    }
    
  }
  }else{
  cat('No BioChem records found for ', desc, '! \n')
}
  
  sink()
}


