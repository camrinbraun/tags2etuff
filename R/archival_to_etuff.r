archival_to_etuff <- function(archival, vars){

  idx <- list()
  nmax <- length(vars)
  for (ii in 1:nmax) idx[[ii]] <- grep(vars[ii], archival$VariableName, ignore.case = T)

  if (length(unlist(idx)) == 0){
    warning('No names in this eTUFF file correspond to input vars.')
    return(archival)
  }

  archival <- archival[unlist(idx),]
  archival <- archival %>% dplyr::select(-c(id)) %>% spread(VariableName, VariableValue)
  archival <- archival[order(archival$DateTime),]

  return(archival)

}

