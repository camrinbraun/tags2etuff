#' Convert recovered tag to eTUFF
#'
#' Converts recovered or archival tag dataset to eTUFF in a "computationally-friendly" way compared to the normal conversion.
#'
#' @param archival is long data frame of archival data
#' @param vars is character list of variables of interest. These must match accepted variable names (see obsTypes).
#'
#' @return a dataframe subset of the archival data containing only the vars specified
#' @export
#'

archival_to_etuff <- function(archival, vars){

  idx <- list()
  nmax <- length(vars)
  for (ii in 1:nmax) idx[[ii]] <- grep(vars[ii], archival$VariableName, fixed = T)

  if (length(unlist(idx)) == 0){
    warning('No names in this eTUFF file correspond to input vars.')
    return(archival)
  }

  idx <- unique(unlist(idx))
  archival <- archival[idx,]
  archival <- archival %>% dplyr::select(-c(id)) %>% spread(VariableName, VariableValue)
  archival <- archival[order(archival$DateTime),]

  return(archival)

}

