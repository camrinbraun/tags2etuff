#' Compute the mode of an input vector
#'
#' Compute the mode of an input vector
#'
#' @param x is input vector
#' @param na.rm is logical indicating whether or not to ignore NA values
#' @return statistical mode of input vector
#' @export

Mode <- function(x, na.rm=FALSE) {
  # function from SO at http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
  if(na.rm){
    ux <- na.omit(unique(x))
  } else{
    ux <- unique(x)
  }
  ux[which.max(tabulate(match(x, ux)))]
}
