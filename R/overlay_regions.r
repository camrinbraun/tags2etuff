overlay_regions <- function(df, eddy_regions){
  coordinates(df) <- ~lon+lat
  proj4string(df) <- proj4string(eddy_regions)

  # get region names
  get_regions <- names(eddy_regions)

  #create list of SpatialPolygons from original SpatialPolygons
  eddy_regions <- lapply(eddy_regions@polygons, function(x) SpatialPolygons(list(x)))

  # do the overlay
  in_eddy_regions <- lapply(eddy_regions, FUN=function(x) !is.na(over(df, as(x, "SpatialPolygons"))))

  idx <- rep(NA, nrow(df))
  for (i in 1:length(in_eddy_regions)){
    idx[which(in_eddy_regions[[i]])] <- get_regions[i]

  }

  idx

}
