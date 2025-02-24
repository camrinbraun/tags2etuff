#' Read Lotek archival tag data
#'
#' Smart way to read lotek archival tag data.
#'
#' @param dir is directory the target files are stored in
#' @export

read_lotek <- function(dir){

  ## list files in directory
  fList <- list.files(dir, full.names = TRUE, recursive = TRUE)

  check_switch <- FALSE

  #=======================
  ## read time series
  #=======================
  print('Reading time series...')
  print('Lotek time series files need _00.csv or Dive Log.csv in the file name.')

  if (length(grep('_00.csv', fList)) == 1 | length(grep('Dive Log.csv', fList)) == 1){

    if(length(grep('_00.csv', fList)) == 1) series_file <- fList[grep('_00.csv', fList)]
    if(length(grep('Dive Log.csv', fList)) == 1) series_file <- fList[grep('Dive Log.csv', fList)]

    ts <- data.table::fread(file=series_file, sep = ',', header = T)

    if(length(grep('Dive Log.csv', fList)) == 1){
      ## read header separate in case there are non-UTF chars
      x <- scan(series_file, what = character(), sep=',', nmax = 100)
      Encoding(x) <- "UTF-8"
      x <- iconv(x, "UTF-8", "UTF-8",sub='')
      hdr <- x[1:4]
      if (length(hdr) != ncol(ts)) hdr <- c(hdr, 'unknown')
      hdr <- gsub(" ", "", hdr, fixed = TRUE)

      names(ts) <- hdr

    }

    ## check for ts and dl logs being backwards from expected
    check_switch <- TRUE

  } else if (length(grep('_00.csv', fList)) > 1){
    stop('More than 1 file was found in the input directory that matched _00.csv. Only one log file matching that extension is allowed.')
  } else if (length(grep('timeseries', fList, ignore.case = T)) == 1){

    series_file <- fList[grep('timeseries', fList, ignore.case = T)]
    ts <- data.table::fread(series_file, sep = ',', header = T)

  } else if (length(grep('timeseries', fList, ignore.case = T)) > 1){
    stop('More than 1 file was found in the input directory that matched "timeseries". Only one log filename matching that text string is allowed.')
  } else{
    stop('No time series file was found.')
  }

  #=======================
  ## read daily log
  #=======================
  print('Reading daily log...')
  print('Lotek daily files need _01.csv or Day Log.csv in the file name.')

  if (length(grep('_01.csv', fList)) == 1 | length(grep('Day Log.csv', fList)) == 1){

    if(length(grep('_01.csv', fList)) == 1) daylog_file <- fList[grep('_01.csv', fList)]
    if(length(grep('Day Log.csv', fList)) == 1) daylog_file <- fList[grep('Day Log.csv', fList)]

    dl <- data.table::fread(file=daylog_file, sep = ',', skip = 1)#, header = F, nrows = 10, blank.lines.skip = F, stringsAsFactors = F)

    ## read header separate in case there are non-UTF chars
    x <- scan(daylog_file, what = character(), sep=',', nmax = 100)
    Encoding(x) <- "UTF-8"
    x <- iconv(x, "UTF-8", "UTF-8",sub='')

    ## get start char
    if (length(grep('Rec #', x, ignore.case = T)) != 0){
      start <- grep('Rec #', x, ignore.case = T)
    } else if (length(grep('Mission Date', x, ignore.case = T)) != 0){
      start <- grep('Mission Date', x, ignore.case = T)
    } else{
      start <- 1
    }

    ## get end char
    if (length(grep('C_TooDimFlag', x, ignore.case = T)) > 0){
      end <- grep('C_TooDimFlag', x, ignore.case = T)
    } else if (length(grep('T S Pointer', x, ignore.case = T)) > 0){
      end <- grep('T S Pointer', x, ignore.case = T)
    } else if (length(grep('Valid Flag', x, ignore.case = T)) > 0){
      end <- grep('Valid Flag', x, ignore.case = T)
    } else {
      end <- ncol(dl) + start - 1
    }

    hdr <- x[start:end]
    if (length(hdr) != ncol(dl)) hdr <- c(hdr, 'unknown')
    hdr <- gsub(" ", "", hdr, fixed = TRUE)

    names(dl) <- hdr

    ## check for ts and dl logs being backwards from expected
    check_switch <- TRUE

  } else if (length(grep('_01.csv', fList)) > 1){
    stop('More than 1 file was found in the input directory that matched _01.csv. Only one log file matching that extension is allowed.')
  } else if (length(grep('daylog', fList, ignore.case = T)) == 1){
    daylog_file <- fList[grep('daylog', fList, ignore.case = T)]

    ## read without header then add it
    dl <- data.table::fread(file=daylog_file, sep = ',')#, header = F, nrows = 10, blank.lines.skip = F, stringsAsFactors = F)

    ## read header separate in case there are non-UTF chars
    x <- scan(daylog_file, what = character(), sep=',', nmax = 100)
    Encoding(x) <- "UTF-8"
    x <- iconv(x, "UTF-8", "UTF-8",sub='')

    ## get start char
    if (length(grep('Rec #', x, ignore.case = T)) != 0){
      start <- grep('Rec #', x, ignore.case = T)
    } else if (length(grep('Mission Date', x, ignore.case = T)) != 0){
      start <- grep('Mission Date', x, ignore.case = T)
    }

    ## get end char
    if (length(grep('C_TooDimFlag', x, ignore.case = T)) > 0){
      end <- grep('C_TooDimFlag', x, ignore.case = T)
    } else if (length(grep('T S Pointer', x, ignore.case = T)) > 0){
      end <- grep('T S Pointer', x, ignore.case = T)
    } else {
      end <- ncol(dl) + start - 1
    }

    hdr <- x[start:end]
    if (length(hdr) != ncol(dl)) hdr <- c(hdr, 'unknown')
    hdr <- gsub(" ", "", hdr, fixed = TRUE)

    names(dl) <- hdr

  } else if (length(grep('daylog', fList, ignore.case = T)) > 1){
    stop('More than 1 file was found in the input directory that matched "daylog". Only one log filename matching that text string is allowed.')
  } else{
    stop('No time daylog file was found.')
  }


  ## are the 00 and 01 logs opposite from what we expect?
  if (check_switch &
      any(c('Longitude [degs]', 'Longitude[degs]') %in% names(ts)) &
      any(c('ExtTemp [C]', 'ExtTemp[C]', 'Ext temp deg C', 'ExttempdegC') %in% names(dl))){ ## then ts and dl are likely backwards

    warning('Time series and daily log appear to be specified opposite of what we expect. Switching the input files to (hopefully) appropriately specify these file types.')
    dl_old <- dl
    dl <- ts
    ts <- dl_old

  }

  ## remove all the weird spacing from col names
  names(ts) <- gsub(" ", "", names(ts), fixed = TRUE)
  names(dl) <- gsub(" ", "", names(dl), fixed = TRUE)

  ## are there more than logs 00 and 01?
  if (length(grep('_02.csv', fList)) > 0) warning('More than logs _00.csv and _01.csv were detected. More than 2 logs is not currently supported, thus additional logs are ignored.')

  out <- list(daylog = data.frame(dl), timeseries = data.frame(ts))
  return(out)
}
