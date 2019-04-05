devtools::load_all('~/work/RCode/analyzePSAT')
devtools::load_all('~/work/RCode/tags2etuff')

library(plyr); library(dplyr); library(tidyr); library(sp); library(ggplot2)
## a function that takes Argos data (from eTUFF), some simulation parameters (opt) and optional PSAT data (i.e. double-tagged fish; from eTUFF)
## output should be of class move
etuff_file <- '~/Desktop/data_org/159924_2013_128416/159924_2013_128416_eTUFF_hdr.txt'
move <- build_move(etuff_file, ssm = 'foieGras', behav = TRUE, sim = TRUE, time.step = 3, slice = 2, reps = 30)

print(move)
plot(move)

## get (if needed), read (req'd) and prepare (if needed such as spat/temp subset) the AVISO eddies atlas.
splims <- list(xmin = -85,xmax = -50, ymin = 25, ymax = 55)
tlims <- list(min=as.Date('2013-01-01'), max=NA)

eddies <- get_eddies(eddyFile = "~/work/Data/eddies/eddy_trajectory_2.0exp_19930101_20180118.nc",
                      meanderFilter = 'nzd', keepMeander = TRUE,
                      splims = splims, tlims = tlims, wwidth = 14, tmin = 10, min.life = 5)

# create example region polygons
p1 <- matrix(c(-75,39,
               -75,42,
               -60,42,
               -60,39), ncol = 2, byrow = TRUE)
p2 <- matrix(c(-80,20,
               -80,35,
               -60,35,
               -60,20), ncol = 2, byrow = TRUE)
p1s <- Polygons(list(Polygon(p1)), 'GulfStream')
p2s <- Polygons(list(Polygon(p2)), 'Sargasso')
sps <- SpatialPolygons(list(p1s, p2s))


move_eddies <- collocate_eddies(move, eddies, eddy_regions = sps)

plot(move_eddies, type='map')
plot(move_eddies, type='eddy-centric')


