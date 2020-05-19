get_3d <- function(etuff, series = NULL,...){

  ## sort out the track

  track <- get_track(etuff)

  if('res_out' %in% args){
    res_out <- args$res_out
  } else{
    res_out <- 6
  }

  if (fG){
    df <- track
    df$id <- 1
    df <- df[,c('id','DateTime','id','longitude','latitude')]
    names(df) <- c('id','date','lc','lon','lat')
    df$lc <- 'B'

    #ssm_fit <- foieGras::fit_ssm(move$raw, model='crw', time.step = 24, vmax=10))
    time_step = 6; bb=6
    df.locs <- split(df, df$id)
    ssm_fit <- lapply(df.locs, function(x) foieGras::fit_ssm(x, model='crw', time.step = time_step, vmax=10))
    idx <- lapply(ssm_fit, function(x) x$converged) %>% do.call(rbind, .)
    if (any(!idx)){
      for (ii in which(!idx)){
        print('Some initial use of fit_ssm() did not converge. Trying other time_step values.')
        #df.ii <- foieGras::grab(ssm_fit[[ii]], 'data', as_sf=F)
        df.ii <- df.locs[[ii]]
        #df.ii <- ssm_fit[[ii]]$ssm[[1]]$data
        df.ii <- data.frame(df.ii[,names(df)])

        # iterate through some reasonable time steps
        poss.tstep <- c(1,3,6,12,24)
        poss.tstep <- poss.tstep[-which(poss.tstep %in% time_step)]
        for (bb in poss.tstep){
          try_fit <- foieGras::fit_ssm(df.ii, model='crw', time.step = bb, vmax=10, optim='nlminb')
          if (try_fit$converged){
            ssm_fit[[ii]] <- try_fit
            print(paste('Converged on time step', bb, 'hours.'))
            break
          } else if (!try_fit$converged & bb == 24){
            print('None of the alternative time steps resulted in fit_ssm() model convergence.')
            print('Cancelling simulations.')
            sim <- FALSE; tmp_out <- NULL
          } else{
            next
          }
        } # bb
      } # which idx
    } # if any

    ## grab predicted locations output from fit_ssm
    plocs <- lapply(ssm_fit, FUN=function(x) foieGras::grab(x, what = "p", as_sf = FALSE)) %>%
      do.call(rbind, .) %>%
      tbl_df() %>%
      mutate(id = as.character(id)) %>% group_by(id)

    ## subsample from predicted locations, if applicable
    if (!is.null(res_out)){
      plocs <- split(plocs, plocs$id)

      for (tt in 1:length(plocs)){
        res.tt <- as.numeric(difftime(plocs[[tt]]$date[2], plocs[[tt]]$date[1], units = 'hours'))
        res_slice <- res_out / res.tt
        #if (res_slice < 1) res_slice <- 1
        plocs[[tt]] <- plocs[[tt]] %>% do(slice(., seq(1, n(), by = res_slice)))
      }

      plocs <- plocs %>%
        do.call(rbind, .) %>%
        tbl_df() %>%
        mutate(id = as.character(id)) %>% group_by(id)

    }
    track <- data.frame(plocs[,c('date','lat','lon')])
    names(track) <- c('DateTime','latitude','longitude')

  } else{
    track <- track[,c('DateTime','latitude','longitude')]

  }

  track_interval <- lubridate::interval(track$DateTime[1:(nrow(track)-1)], track$DateTime[2:(nrow(track))])



  ## sort out series
  if (is.null(series)) series <- get_series(etuff)
  series <- series[which(!is.na(series$DateTime)),]

  idx <- lapply(series$DateTime, FUN=function(x){
    i <- which(x %within% track_interval)
    #print(i)
    if (length(i) == 0) i <- NA
    if (length(i) == 2) i <- i[1]
    i
    }) %>% unlist()

  if (length(idx) != nrow(series)) stop('Series and matching track interval are not of same length.')


  ## then combine
  series$idx <- idx
  track$idx <- 1:nrow(track)
  series <- merge(series, track, by='idx', all.x = TRUE)
  series$track_dt <- as.POSIXct(series$track_dt, origin='1970-01-01', tz='UTC')
  series <- series[,which(!(names(series) %in% 'idx'))]

  return(series)
}
