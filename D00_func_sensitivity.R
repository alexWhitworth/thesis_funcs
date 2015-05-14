#-------------------------------------------------------------------
# SENSITIVITY ANALYSIS FUNCTIONS
#-------------------------------------------------------------------

impute_2levels <- function(dat, pct, type, group= "title_format2", impute.val= 0) {
  require(lubridate)
  # xmas dates
  if (type == "physical") {xmas.dt <- c(10,11,22,23)} else {xmas.dt <- c(11,12,23,24)}
  mf.dt <- c(4,5,16,17,28,29)
  if (group == "title_format2") {
    nm <- as.character(dat$title_format2[1])   # which album / format
  } else if (group == "title") {
    nm <- as.character(dat$title[1]) 
  }
  
  # get integers of missing months and determine which to impute
  # exit if no imputation
  dts.miss <- as.integer(names(which(table(c(0:29, as.integer(names(table(dat$dt_scale))))) == 1)))
  if (length(dts.miss) == 0) {
    return(list(group= nm, n.miss= 0, n.imp= 0, dat= dat))
  } else {
    imp.mos  <- sample(dts.miss, size= ceiling(pct * length(dts.miss)))
  }
  
  # checks: (1) Min catalog date
  rel.dt <- min(dat$dt_scale) -
    round(dat[dat$dt_scale == min(dat$dt_scale), "yr_since_rel"][1] * 12, 0)
  min.cat.dt <- rel.dt + 24
  imp.mos <- imp.mos[imp.mos > min.cat.dt]
  
  # if none to impute --> return
  if (length(imp.mos) == 0) { 
    return(list(group= nm, n.miss= length(dts.miss[dts.miss > min.cat.dt]), n.imp= 0, dat= dat))
  }
  # else, impute 
  if ("geography" %in% names(dat)) {
    geo <- rep(as.character(dat$geography[1], length(imp.mos)))
    dat2 <- data.frame(geography= geo,
                       date2= as.Date("2012-01-16", format="%Y-%m-%d") %m+% months(imp.mos),
                       dt_scale= imp.mos,
                       albumid= rep(dat$albumid[1], length(imp.mos)),
                       title= rep(dat$title[1], length(imp.mos)),
                       format2= rep(dat$format2[1],length(imp.mos)),
                       yr_since_rel= round((imp.mos - rel.dt) / 12, 2),
                       decade_rel= rep(NA, length(imp.mos)),
                       mf_day= ifelse(imp.mos %in% mf.dt, TRUE, FALSE),
                       xmas= ifelse(imp.mos %in% xmas.dt, TRUE, FALSE))
  } else {
    dat2 <- data.frame(date2= as.Date("2012-01-16", format="%Y-%m-%d") %m+% months(imp.mos),
                       dt_scale= imp.mos,
                       albumid= rep(dat$albumid[1], length(imp.mos)),
                       title= rep(dat$title[1], length(imp.mos)),
                       format2= rep(dat$format2[1],length(imp.mos)),
                       yr_since_rel= round((imp.mos - rel.dt) / 12, 2),
                       decade_rel= rep(NA, length(imp.mos)),
                       mf_day= ifelse(imp.mos %in% mf.dt, TRUE, FALSE),
                       xmas= ifelse(imp.mos %in% xmas.dt, TRUE, FALSE))
  }
  dat2$title_format2 <- interaction(dat2$title, dat2$format2)
  if (type == "physical") {
    dat2$physical  <- rep(impute.val, length(imp.mos))
    if ("geography" %in% names(dat)) {
      dat2 <- dat2[c(names(dat2)[1:8], "physical", "title_format2", "mf_day", "xmas")]
    } else {
      dat2 <- dat2[c(names(dat2)[1:7], "physical", "title_format2", "mf_day", "xmas")]
    }
  } else if (type == "digital") {
    dat2$digital   <- rep(impute.val, length(imp.mos))
    if ("geography" %in% names(dat)) {
      dat2 <- dat2[c(names(dat2)[1:8], "digital", "title_format2", "mf_day", "xmas")]
    } else {
      dat2 <- dat2[c(names(dat2)[1:7], "digital", "title_format2", "mf_day", "xmas")]
    }
  } else if (type == "streaming") {
    dat2$streaming <- rep(impute.val, length(imp.mos))
    if ("geography" %in% names(dat)) {
      dat2 <- dat2[c(names(dat2)[1:8], "streaming", "title_format2", "mf_day", "xmas")]
    } else {
      dat2 <- dat2[c(names(dat2)[1:7], "streaming", "title_format2", "mf_day", "xmas")]
    }
  }
  
  # combine w/ original data and return
  dat2 <- data.frame(rbind(dat, dat2))
  return(list(group= nm, n.miss= length(dts.miss[dts.miss > min.cat.dt]), 
              n.imp= length(imp.mos), dat=dat2))
}

# wrapper to impute_2levels for geographies / level 3
impute_zeros <- function(dat, pct, type, group= "title_format2", seed= NULL, levels= 2,
                         impute.val= 0) {
  # source needed function
  impute_2levels <- get("impute_2levels", envir= .GlobalEnv)
  # set seed
  if (is.null(seed))  {
    seed <- sample(1:10^6, size=1); set.seed(seed)
  } else {set.seed(seed)}
  
  # impute
  if (levels == 2) {
    imp2 <- impute_2levels(dat= dat, pct= pct, type= type, group= group, impute.val= impute.val)
    return(list(group= imp2[[1]], seed= seed, n.miss= imp2[[2]], m.imp= imp2[[3]],
                dat= imp2[[4]]))
  } else if (levels == 3) {
    require(plyr)
    imp2 <- dlply(dat, .var= "geography", .fun= impute_2levels, pct= pct, type=type,
                  group= group, impute.val= impute.val)
    
    # patch back together      
    dat2 <- data.frame(do.call("rbind", lapply(imp2, "[[", 4))) # row bind data together
    # return
    return(list(group= imp2[[1]]$group, seed= seed, 
                n.miss= sum(sapply(imp2, "[[", 2)), n.imp= sum(sapply(imp2, "[[", 3)),
                dat=dat2))
  }
}

# wrapper to impute_zeros for all title_format2 groups
impute_zeros_all <- function(dat, pct, type, group= "title_format2", seed= NULL, levels= 2,
                             impute.val= 0) {
  # error checking
  if (!type %in% c("physical", "digital", "streaming") | 
        (!type %in% names(dat))) {
    stop("Please correct revenue type input")
  }
  if (pct <= 0 | pct > 1) {
    stop("pct must be in (0,1].")
  }
  
  # load libraries / source needed function
  require(plyr); require(lubridate)
  impute_zeros <- get("impute_zeros", envir= .GlobalEnv)
  
  # impute
  if (group == "title_format2") {
    imp_all <- dlply(dat, .var= "title_format2", .fun= impute_zeros, pct= pct, 
                     type= type, seed= seed,levels= levels, impute.val= impute.val)
    # piece data back together
    dat2 <- data.frame(do.call("rbind", lapply(imp_all, "[[", 5)))
    # capture seeds used
    sds  <- lapply(imp_all, "[[", 2); names(sds) <- sapply(imp_all, "[[", 1)
    return(list(seed= sds, n.miss= sum(sapply(imp_all, "[[", 3)), 
                n.imp= sum(sapply(imp_all, "[[", 4)), dat=dat2))  
  } else if (group == "title") {
    imp_all <- dlply(dat, .var= "title", .fun= impute_zeros, pct= pct, 
                     type= type, seed= seed, levels= levels, impute.val= impute.val)
    # piece data back together
    dat2 <- data.frame(do.call("rbind", lapply(imp_all, "[[", 5)))
    # capture seeds used
    sds  <- lapply(imp_all, "[[", 2); names(sds) <- sapply(imp_all, "[[", 1)
    return(list(seed= sds, n.miss= sum(sapply(imp_all, "[[", 3)), 
                n.imp= sum(sapply(imp_all, "[[", 4)), dat=dat2))  
  }
}

# test -- working
# t  <- ccr.d[ccr.d$title_format2 == ccr.d$title_format2[1],]
# t2 <- t[t$geography == "JAPAN", ]
# t3 <- impute_2levels(t2, pct= .2, type= "digital", group= "title_format2")
# t3 <- impute_zeros(t2, pct= .2, type= "digital", group= "title_format2", levels=2)
# t3 <- impute_zeros(t, pct= .2, type= "digital", group= "title_format2", levels= 3)
# t4 <- impute_zeros_all(dat= ccr.d, pct=.2, type= "digital", group= "title_format2", levels=3)
