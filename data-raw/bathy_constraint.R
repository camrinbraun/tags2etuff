
## create bathymetry constraint for use in simulating tracks
## resulting bdf object will be available internally to sim function(s)
## bdf is global raster of 1/4 deg resolution indicating on land (NA) or not (1)

library(maptools); library(raster)

data(wrld_simpl)
world <- wrld_simpl # easier name...

r.ref <- raster(ncol = 1440, nrow = 720)
r.ref[] <- 0
world.polys <- as(world, 'SpatialPolygons')
r.polys <- rasterize(world.polys, r.ref, mask = TRUE)
r.polys[is.na(r.polys)] <- 1
r.polys[r.polys == 0] <- NA

bdf <- as(r.polys, 'SpatialPixelsDataFrame')
crs(bdf) <- NA

devtools::use_data(bdf, internal = TRUE)
