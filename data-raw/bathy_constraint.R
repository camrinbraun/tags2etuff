
# get bathymetry raster from erddap
bathy <- HMMoce::get.bath.data(65, 115, -70, -45, folder = tempdir(), res=1)

bathy.mod <- aggregate(bathy, 5)
bathy.mod[bathy.mod > -1] <- NA

bdf <- as(bathy.mod, 'SpatialPixelsDataFrame')
crs(bdf) <- NA

#devtools::use_data(x, mtcars, internal = TRUE)


## OR

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
