Mode <- function(x, na.rm=FALSE) {
  # function from SO at http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
  if(na.rm){
    ux <- na.omit(unique(x))
  } else{
    ux <- unique(x)
  }
  ux[which.max(tabulate(match(x, ux)))]
}
