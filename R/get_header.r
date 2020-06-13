# read a nicely formatted header
get_header <- function(etuff_file, metaTypes = NULL){  ## read etuff header

  if (is.null(metaTypes)){
    print('metaTypes is NULL. Trying to retrieve from github.')
    metaTypes <- utils::read.csv(url("https://raw.githubusercontent.com/camrinbraun/tagbase/master/eTagMetadataInventory.csv"))

    warning('Some restrictions on metaTypes$Necessity are being relaxed. Provide your own metaTypes input here to keep default necessity values.')
    ## this relaxes some of the previously required meta attributes
    metaTypes$Necessity[which(metaTypes$AttributeID %in% c(3,8,100,101,200,302,400:404,1000))] <- 'recommended'

  }

  hdr <- get_etuff_hdr(etuff_file, metaTypes)
  hdr <- hdr %>% spread(varName, varVal)

  ## something here to auto-format the hdr cols...
  for (i in 1:ncol(hdr)){

    ## check and convert header vals to appropriate classes
    if (length(metaTypes$class[which(metaTypes$AttributeName %in% names(hdr)[i])]) == 0) next
    if (metaTypes$class[which(metaTypes$AttributeName %in% names(hdr)[i])] == 'character') hdr[,i] <- as.character(hdr[,i])

    if (metaTypes$class[which(metaTypes$AttributeName %in% names(hdr)[i])] == 'numeric'){
      hdr[,i] <- as.numeric(as.character(hdr[,i]))
      #print(paste('numeric', i))
    }

    if (metaTypes$class[which(metaTypes$AttributeName %in% names(hdr)[i])] == 'factor') hdr[,i] <- as.factor(as.character(hdr[,i]))

    ## logicals require some flexibility with yes/no answers
    if (metaTypes$class[which(metaTypes$AttributeName %in% names(hdr)[i])] == 'logical'){
      if (is.na(as.logical(as.character(hdr[,i])))){
        if (length(grep('no', as.character(hdr[,i]))) > 0){
          hdr[,i] <- FALSE
        } else if(length(grep('yes', as.character(hdr[,i]))) > 0){
          hdr[,i] <- TRUE
        } else{
          warning('Cant automatically convert the logical class header values to as.logical. Leaving them as character class. This may have unexpected consequences.')
        }

      } else{
        hdr[,i] <- as.logical(as.character(hdr[,i]))
      }
    }

    ## POSIX requires some auto date formatting
    if (metaTypes$class[which(metaTypes$AttributeName %in% names(hdr)[i])] == 'POSIXct'){
      old_hdr <- hdr[,i]
      if (!is.na(as.POSIXct(hdr[,i], format = '%m/%d/%y', tz='UTC'))){
        hdr[,i] <- as.POSIXct(hdr[,i], format = '%m/%d/%y', tz='UTC')
      } else{
        hdr[,i] <- as.POSIXct(hdr[,i], format = '%Y-%m-%d', tz='UTC')
      }

      print(paste(old_hdr, ' has been parsed as ', hdr[,i], '. If this is incorrect, the auto-detection of the date format is wrong.', sep=''))
      #rm(old_hdr)

    }

  }

  return(hdr)

}
