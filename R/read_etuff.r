#' Read an eTUFF file
#'
#' Read tag data stored as eTUFF
#'
#' @param etuff_file is an etuff text file
#' @param header is logical indicating whether or not the target etuff_file has a header. This will nearly always be TRUE (default).
#' @param metaTypes is a dataframe that describes the appropriate inventory of metadata vocabulary. Default is NULL in which this table is read from Github.
#' @importFrom data.table fread
#' @export
#' @return an etuff object

read_etuff <- function(etuff_file, header = TRUE, metaTypes = NULL){

  if (is.null(metaTypes)){
    print('metaTypes is NULL. Trying to retrieve from github.')
    metaTypes <- utils::read.csv(url("https://raw.githubusercontent.com/camrinbraun/tagbase/master/eTagMetadataInventory.csv"))

    warning('Some restrictions on metaTypes$Necessity are being relaxed. Provide your own metaTypes input here to keep default necessity values.')
    ## this relaxes some of the previously required meta attributes
    metaTypes$Necessity[which(metaTypes$AttributeID %in% c(3,8,100,101,200,302,400:404,1000))] <- 'recommended'

  }

  ## header will nearly always be true (default) but special circumstances may necessitate reading an etuff file that has NOT been written with a header
  if (header){
    ## read etuff header
    hdr <- get_etuff_hdr(etuff_file)
    hdr <- hdr %>% tidyr::spread(varName, varVal)

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
        } else if (!is.na(as.POSIXct(hdr[,i], format = '%Y-%m-%d', tz='UTC'))){
          hdr[,i] <- as.POSIXct(hdr[,i], format = '%Y-%m-%d', tz='UTC')
        } else if (!is.na(as.POSIXct(as.numeric(hdr[,i]), origin = '1970-01-01', tz='UTC'))){
          hdr[,i] <- as.POSIXct(as.numeric(hdr[,i]), origin = '1970-01-01', tz='UTC')
        } else{
          stop('Unable to parse header dates.')
        }

        print(paste(old_hdr, ' has been parsed as ', hdr[,i], '. If this is incorrect, the auto-detection of the date format is wrong.', sep=''))
        rm(old_hdr)

      }

    }

  } else{
    hdr <- NULL
  }

  ## read etuff data
  if (!is.null(hdr)){
    # figure out how many hdr lines to skip when reading the data
    x <- scan(etuff_file, what = character(), sep=',', nmax=1000)
    skipLines <- grep('DateTime', x) - 1

    df <- data.frame(data.table::fread(etuff_file, sep=',', header = T, skip = skipLines))

  } else{
    df <- data.frame(data.table::fread(etuff_file, sep=',', header = T, skip = 0))
  }

  df <- df %>% dplyr::filter(!is.na(VariableID)) %>%
    dplyr::select(-c(VariableID, VariableUnits)) %>%
    tidyr::spread(VariableName, VariableValue)

  ## format date time
  names(df)[1] <- 'DateTime'

  ## datetime is blank for histo bins
  if (any(as.character(df$DateTime) == '' | is.na(df$DateTime))){
    bins <- df[which(as.character(df$DateTime) == '' | is.na(df$DateTime)),]
    drop_idx <- which(apply(bins, 2, FUN=function(x) all(is.na(x) | x == '')))
    bins <- bins[,-drop_idx]
    df <- df[which(as.character(df$DateTime) != ''),]
  }

  df$DateTime <- as.POSIXct(df$DateTime, tz='UTC')
  warning('Current TZ specification is UTC.')

  df$id <- hdr$instrument_name

  if (!exists('bins')) bins <- NULL

  etuff <- list(etuff = df, meta = hdr, bins = bins)
  class(etuff) <- 'etuff'
  return(etuff)
}
