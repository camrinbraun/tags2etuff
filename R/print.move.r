print.move <- function(object){
  with(object, cat('Move object for ID', pred$id[1], '. Species ', platform, ".\n", sep = ''))
  cat('Predicted movement data is:')
  with(object, print(head(pred, 3)))

  # add if statements to show if other data, such as vertical, exists...

  # make sure print model convergence...

  cat("...\n")

}
