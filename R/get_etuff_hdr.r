#' Get header as metadata from eTUFF file
#'
#' WARNING: this is an internal function. Instead see \code{get_header}.
#'
#' @param etuff_file is file name to etuff file of interest
#' @return a dataframe of metadata from the header of an eTUFF file
#'

get_etuff_hdr <- function(etuff_file){
  x <- scan(etuff_file, what = character(), sep=',', nmax = 1000)

  # figure out how many hdr lines to skip when reading the data
  skipLines <- grep('DateTime', x) - 1

  # trim x to just the header
  x <- x[1:skipLines]
  x <- x[grep('=', x)]

  for (i in 1:length(x)){

    if (has.invalid.multibyte.string(x[i])){
      warning('Multibyte string detected in one of the header elements. It is being skipped.')
      next
    }
    varName <- stringr::str_trim(
      substr(x[i],
             stringr::str_locate(x[i], ':')[2] + 1,
             stringr::str_locate(x[i], '=')[2] - 2))

    varVal <- stringr::str_trim(
      substr(x[i],
             stringr::str_locate(x[i], '= ')[2],
             nchar(x[i])))
    if (i == 1){
      varDF <- data.frame(varName = as.character(varName), varVal = as.character(varVal))
    } else{
      varDF$varName <- as.character(varDF$varName)
      varDF$varVal <- as.character(varDF$varVal)
      varDF[i,] <- c(varName, varVal)
    }
  }

  varDF$varName <- as.character(varDF$varName)
  varDF$varVal <- as.character(varDF$varVal)
  varDF[c(length(x) + 1),] <- c('skip', skipLines)
  varDF
}

## from https://stackoverflow.com/questions/35589887/how-to-detect-encoding-problems-that-will-generate-invalid-multibyte-string-er
has.invalid.multibyte.string  <- function(x, return.elements=F)
{
  # determine if "invalid multibyte string" error will be triggered
  # if return.elements=T, then output is logical along x, otherwise single logical
  if (is.null(x))
    return(F)
  if (return.elements)
  {
    n <- length(x)
    out <- rep(F,n)
    for (i in 1:n)
      out[i] <- is.error(try(toupper(x[i]),silent = T))
  }
  else
    out <- mb.is.error(try(toupper(x),silent = T))
  return(out)
}

mb.is.error <- function(x)
{
  # test output of try()
  return(class(x)[1]=="try-error")
}
