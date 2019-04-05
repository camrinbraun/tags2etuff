

build_move <- function(etuff_file, ssm = NULL, behav = TRUE, sim = TRUE, time.step = 3, slice = NULL, reps = NULL,...){

  ## optional inputs:
  #     foieGras -> time.step, vmax, by arg to slice, ...
  #     crawl ->
  #     adehabitatLT (for simulations) -> reps
  #     mpm (behav) -> optim = c("nlminb", "optim"), verbose = FALSE
  #     slice for subsampling from foieGras prediction

  #------------------
  ## read and format the etuff (e.g. spread) to be some combination of dt, lat, lon, lc
  #------------------

  hdr <- get_etuff_hdr(etuff_file)

  df <- read.table(etuff_file, sep = ',', header = T, skip = hdr[which(hdr$varName == 'skip'), 2])
  df <- df %>% dplyr::select(-c(VariableID, VariableUnits)) %>% spread(VariableName, VariableValue)
  df$latitude <- as.numeric(as.character(df$latitude))
  df$longitude <- as.numeric(as.character(df$longitude))
  df <- df[,c('X...DateTime','argosLC','latitude','longitude')]
  df$id <- hdr[grep('instrument_name', hdr$varName), 2]
  names(df) <- c('date','lc','lat','lon','id')
  df <- df[,c('id','date','lc','lon','lat')]
  df$date <- as.POSIXct(df$date, tz='UTC')
  df$lc <- as.character(df$lc)
  print(str(df))

  #------------------
  ## deal with gaps...
  #------------------


  #------------------
  ## filter and standardize argos data
  #------------------

  if (!is.null(ssm)){
    if (ssm == 'foieGras'){

      # fit CRW SSM to filter outliers (max speed 10 m/s) and standardize (6 hr tstep)
      cat("filtering with foieGras::fit_ssm()...\n")
      t1 <- Sys.time()
      ssm_fit <- foieGras::fit_ssm(df, model='crw', time.step = time.step, vmax=10)
      t2 <- Sys.time()
      if (!ssm_fit$converged) stop('fit_ssm did not converge. Try a different time step or consider dealing with temporal gaps in the data.')
      cat(sprintf("\nfit took %d seconds...\n", round(as.numeric(difftime(t2, t1, units = 'secs')), 0)))

      ## grab predicted locations in unprojected form
      plocs <- foieGras::grab(ssm_fit, what = "p", as_sf = FALSE)

      ## subsample from predicted locations, if applicable
      if (!is.null(slice)) plocs <- plocs %>% do(slice(., seq(1, n(), by = slice)))

    } else if (ssm == 'crawl'){

      stop('fitting SSM using crawl is not yet supported.')

    } else{

    }

  } else {
    cat("no SSM filtering applied...\n")
    ssm <- NULL
  }

  #------------------
  ## estimate behavior
  #------------------

  if (behav){

    cat("fitting behavior model with mpm...\n")
    start <- Sys.time()
    mpm_fit <- mpm::mpm(plocs[,c('id','date','lon','lat')])
    end <- Sys.time()
    mpm_fit$runtime <- as.numeric(difftime(end, start, units='mins'))
    cat(sprintf('mpm fit took %d seconds.\n', round(as.numeric(difftime(end, start, units='secs')),0)))

    # add g to plocs for use later
    plocs$g <- mpm_fit$fitted$g

  } else {
    cat("no behavior model fit...\n")
    plocs <- NULL
  }


  #------------------
  ## simulate
  #------------------

  if (sim){

    cat("building simulations...\n")
    # build trajectories to base sims on
    tr1 <- adehabitatLT::as.ltraj(cbind(plocs$lon, plocs$lat), date=plocs$date, id=as.factor(plocs$id),
                    proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

    # parameterize the sims
    if (is.null(reps)) reps <- 30
    crw <- adehabitatLT::NMs.randomCRW(na.omit(tr1), rangles=TRUE, rdist=TRUE,
                         #treatment.func = myfunc,
                         #treatment.par = map,
                         constraint.func = consfun,
                         constraint.par = bdf, nrep = reps)
    # simulate
    tmp <- adehabitatLT::testNM(crw)

    for (i in 1:length(tmp)){
      id.i <- names(tmp)[i]
      for (b in 1:length(tmp[[i]])){
        tmp.b <- tmp[[i]][[b]]
        names(tmp.b) <- c('lon','lat','date')
        tmp.b$id <- id.i
        tmp.b$id.iter <- paste(id.i, '.', b, sep='')
        #tmp.b$date <- as.Date(tmp.b$date)
        #tmp.b$bathy <- raster::extract(bathy, cbind(tmp.b$lon, tmp.b$lat))
        #tmp.b <- tmp.b[which(!is.na(tmp.b$bathy)),]

        if (b == 1){
          tmp_out <- tmp.b
        } else {
          tmp_out <- rbind(tmp_out, tmp.b)
        }
      } # end b loop
    } # end i loop

    # output from tmp loop is rbind(df) w cols lon, lat, date, id, id.iter

  } else {
    cat("no simulations are being generated...\n")
    sim <- NULL
  }


  #------------------
  ## add PSAT?
  #------------------

  ## should output variable vert, otherwise NULL
  cat("\nvertical data set to NULL...\n")
  vert = NULL

  #------------------
  ## make output
  #------------------
  cat("\ngenerating output of class \"move\"...\n")

  out <- list(
    pred = data.frame(plocs),
    sim = tmp_out,
    ssm_fit = ssm_fit,
    mpm_fit = mpm_fit,
    vert = vert,
    platform = hdr[grep('platform', hdr$varName), 2],
    meta = hdr %>% spread(varName, varVal)

  )

  class(out) <- append("move", class(out))
  out

} # end build_move


