## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, error = TRUE)

## ---- message=F----------------------------------------------------------
library(tags2etuff)
library(tidyverse)

## ---- cache=T------------------------------------------------------------

## tell it where your data lives
data_dir <- '../inst/extdata/159922_2020_196385/'

## load all tag metadata master sheet
meta <- readRDS('../inst/extdata/meta_example.rds')

## note there are MANY input options to this function!
etuff <- tag_to_etuff(data_dir, meta, gpe3 = TRUE)


## ----fig_height=16, fig.asp=1--------------------------------------------
g <- qc_psat_etuff(etuff, meta_row, writePNG = F, map = T)
gridExtra::grid.arrange(g)


## ------------------------------------------------------------------------

## here's the metadata magic
build_meta_head(meta_row = etuff$meta, write_hdr = F)


## ------------------------------------------------------------------------
## we add the QC vars it wants
etuff$meta$found_problem <- 'no'
etuff$meta$person_qc <- 'Camrin Braun'
etuff$meta$waypoints_source <- 'GPE3'


## and now it works
build_meta_head(meta_row = etuff$meta, filename = outName_hdr, write_hdr = F)


## ------------------------------------------------------------------------

## check the meta hdr
etuff_file <- paste(data_dir, etuff$meta$instrument_name, '_eTUFF.txt', sep='')

write_etuff(etuff, etuff_file, meta_row = etuff$meta)


## ------------------------------------------------------------------------
etuff_file <- './inst/extdata/159922_2020_196385/159922_2020_196385_eTUFF.txt'

etuff <- read_etuff(etuff_file)


## ------------------------------------------------------------------------
pdt <- get_pdt(etuff)
head(pdt)


## ---- message=F, warning=F, cache=T--------------------------------------
pdt_interp <- interp_pdt(etuff)

## ------------------------------------------------------------------------
series <- get_series(etuff)
head(series)


## ---- eval = F-----------------------------------------------------------
#  series <- get_series(etuff, what_tz = 'Asia/Riyadh')

## ---- message=F, warning=F-----------------------------------------------
## steal some temp values from the interpolated pdt
series <- add_series_temp(series, pdt, pdt_interp)

## add a day/night column to your series data based on local sunrise/sunset times
series$dn <- add_daynight(series, etuff)
head(series)

## you can also get your own df of sunrise and sunset times if you care (this is happening under the hood in add_daynight above) 
srss <- get_srss(etuff)
head(srss)


## ----message=F, warning=F, echo=F, fig_height=8, fig.asp=1---------------

srss <- srss[which(srss$DateTime < max(series$DateTime_local, na.rm=T)),]
## use the nighttime interval from srss to easily grab the next day's sunrise as the "end of night" period for plotting
srss$end_night <- srss$night_interval@start + srss$night_interval@.Data
  
ggplot(srss) + geom_rect(aes(xmin=sunset, xmax=end_night, ymin=-650, ymax=0), alpha=0.25, colour='grey') +
  geom_point(data = series, aes(x=DateTime, y=depth * -1, colour=temperature)) + ylim(-650,0) + ylab('Depth (m)')


## ---- eval=T, message=F, warning=F---------------------------------------
track <- get_track(etuff)
head(track)


## ---- eval=T, message=F, warning=F---------------------------------------
tad <- get_tad(etuff)
head(tad)

tat <- get_tad(etuff)
head(tat)

