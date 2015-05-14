

#' @title Generate simulation data for a single format
#' @description 
#' This function generates data for a specified number of groups and group size,
#' when given a single format2 type. A meaningful random slope (xmas) can also be used
#' in the data generation. It is assumed that this function will not be called directly.
#' @param grps The number of groups
#' @param grp.size A vector specifying group size. If `length(grp.size) == 1` uniform group size is 
#' assumed. If `length == length(grp.size) == grps` group sizes may be specifed for each group
#' @param xmas Logical. Whether the simulated data should include a meaningful random slope
#' @param xmas.pct Numeric \eqn{\in [0,1]}. The percentage of groups with a meaningful random
#' slope for xmas
#' @param outliers Logical. Do you wish for the simulation to include level-2 outliers?
#' @param out.pct Numeric \eqn{\in [0,1]}. The percentage of level-2 groups that are outliers
#' @param out.scale Numeric. A multiple for `hyper[1]` to scale the mean of outlying level-2 groups. 
#' @param format2 Character. A format2 label for the simulated data.
#' @param hyper A vector of length 2. The mean and standard deviation for the hyper parameter.
gen.format <- function(grps= 100, grp.size= 30, xmas= FALSE, xmas.pct= 0.1,
                       outliers= FALSE, out.pct= 0.1, out.scale= 3,
                       format2= "CD", hyper= c(4.4, 1.5))  {
  
  # initialize x.mas dates
  x.dt  <- c(1, rep(0, 10), 1,1, rep(0,10), 1,1, rep(0,5)) # xmas dates in mo's 0,11,12,23,24
  
  # 01. initialize parameters w/ or w/o L2 outliers
  if (outliers == FALSE) {
    g00 <- rnorm(grps, hyper[1], hyper[2]);   
  } else {
    out <- runif(grps)
    g00 <- sapply(out, function(x, out.pct, out.scale, hyper) {
      ifelse(x < out.pct, rnorm(1, hyper[1] * out.scale, hyper[2]), 
             rnorm(1, hyper[1], hyper[2]))
    }, out.pct= out.pct, out.scale= out.scale, hyper= hyper)
  }
  
  # xmas random slopes
  if (xmas == TRUE) {
    x.slope <- ifelse(runif(grps) < xmas.pct, rnorm(grps, hyper[1], .5),
                      rnorm(grps, .5, .5))
  } else {
    x.slope <- rnorm(grps, .5, .5)
  }
  names(g00) <- paste0(format2, 1:grps)
  
  # 02. Generate groups, depending on grp.size parameter (single value or vector)
  if (length(grp.size) == 1) {
    # initialize needed vectors
    yij <- vector("numeric", length= grps * grp.size) # uniform group size
    grp.name <- c(sapply(paste0(format2, 1:grps), rep, grp.size))
    xmas.vec <- as.logical(rep(x.dt[1:grp.size], grps))
    for (i in 1:grps) {
      yij[((i-1) * grp.size + 1):(i * grp.size)] <- 
        g00[i] + x.dt[1:grp.size] * x.slope[i] + rnorm(grp.size, 0, .75)
    }
    format2.vec <- rep(format2, grps * grp.size)
  } else if (length(grp.size) == grps) {
    # initialize needed vectors
    xmas.vec <- yij <- vector("numeric", length= sum(grp.size)) # differing group size
    gname <- apply(cbind(grp.size, 1:grps), 1, function(x) {rep(paste0(format2, x[2]), x[1])})
    if (is.list(gname) == TRUE) {
      grp.name <- do.call("c", gname)
    } else if (is.matrix(gname) == TRUE) {
      grp.name <- as.vector(gname)
    }
    
    for (i in 1:grps) {
      if (i == 1) {
        yij[1:grp.size[1]] <- g00[i] + x.dt[1:grp.size[1]] * x.slope[i] + rnorm(grp.size[i], 0, .75)
        xmas.vec[1:grp.size[1]] <- x.dt[1:grp.size[1]]
      } else {
        yij[(sum(grp.size[1:(i-1)])+1):sum(grp.size[1:i])] <- 
          g00[i] + x.dt[1:grp.size[i]] * x.slope[i] + rnorm(grp.size[i], 0, .75)
        xmas.vec[(sum(grp.size[1:(i-1)])+1):sum(grp.size[1:i])] <- x.dt[1:grp.size[i]]
      }
    }
    format2.vec <- rep(format2, sum(grp.size))
  }
  # return data set
  return(list(data= data.frame(rev= yij, format2= format2.vec, grp.name= grp.name,
                               xmas= xmas.vec), 
              params=cbind(g00= g00, xmas.slope= x.slope)))
}



#' @title Generate simulation data for a multiple formats
#' @description 
#' This function does repeated calls to `gen.format()`, one for each format2 requested.
#' @param grps A vector for the number of groups in each format2.
#' @param grp.size A vector or list. If a vector, uniform group size is assumed within each format2. 
#' If as list, group sizes for each format2 can be supplied.
#' @param format2 Character vector. A format2 label for the simulated data.
#' @param xmas Logical. Whether the simulated data should include a meaningful random slope
#' @param xmas.pct Value \eqn{\in [0,1]}. The percentage of groups with a meaningful random
#' slope for xmas
#' @param outliers Logical vector. Do you wish for the simulation to include level-2 outliers?
#' @param out.pct Numeric vector with elements \eqn{\in [0,1]}. The percentage of level-2 groups that 
#' are outliers for each format2. 
#' @param out.scale Numeric vector. A multiple for `hyper[1]` to scale the mean of outlying level-2 
#' groups for each format2.
#' @param hyper A list where each component is the mean and standard deviation for each hyper parameter.
gen.mlm.dat <- function(grps= c(100,100,100,100), grp.size= c(30, 30, 30, 30),
                        format2= c("CD", "E Audio", "Other", "Super Audio"), 
                        outliers= rep(FALSE, 4), out.pct= rep(0.1, 4), out.scale= 3,
                        xmas= FALSE, xmas.pct= 0.1, hyper= list()) {
  # error checking
  if (xmas.pct < 0 | xmas.pct > 1) {
    stop("xmas.pct must be in [0,1].")
  }
  if (length(outliers) == 1) {
    outliers <- rep(outliers, length(grps))
  }
  if (length(out.pct) == 1) {
    out.pct <- rep(out.pct, length(grps))
  }
  if (length(out.scale) == 1) {
    out.scale <- rep(out.scale, length(grps))
  }
  
  # 00. initializations, calls
  gen.format <- get("gen.format") # pull needed function
  
  # 01. generate data for each format2
  dat.list <- list()
  if (is.list(grp.size) == FALSE) {
    for (j in 1:length(grps)) {
      dat.list[[j]] <- gen.format(grps= grps[j], grp.size= grp.size[j], xmas= xmas, xmas.pct= xmas.pct,
                                  outliers= outliers[j], out.pct= out.pct[j], out.scale= out.scale[j],
                             format2= format2[j], hyper= hyper[[j]])
    }
  } else if (is.list(grp.size) == TRUE) {
    for (j in 1:length(grps)) {
      dat.list[[j]] <- gen.format(grps= grps[j], grp.size= grp.size[[j]], xmas= xmas, xmas.pct= xmas.pct,
                                  outliers= outliers[j], out.pct= out.pct[j], out.scale= out.scale[j],
                                  format2= format2[j], hyper= hyper[[j]])
    }
  }
  dat2 <- data.frame(do.call("rbind", lapply(dat.list, "[[", 1))) # row bind data together
  params <- do.call(rbind, lapply(dat.list, "[[", 2)) # combine sets of parameters
  
  return(list(data= dat2, params= params))
}


#' @title Combine strinkage estimators and true parameters
#' @description 
#' Given a merMod object and parameter data-frame,  combines strinkage estimators and true parameters.
#' Not intended to be called directly.
#' @param mod A merMod object for which you wish to extract shrinkage estimators
#' @param params a data.frame or matrix to be coerced. Represents true parameter values
#' @param grps A vector for the number of groups in each format2.
#' @param format2 A character vector. Used for labeling
c.param.est <- function(mod, params, grps, format2) {
  fe <- fixef(mod)
  re <- ranef(mod)[["grp.name"]]; re$rnames <- rownames(re)
  
  # update RE estimates to parameter shrinkage estimates
  re[,2] <- re[,2] + fe[match("xmasTRUE", names(fe))]
  for (i in 1:length(grps)) {
    if(i == 1) {
      ind.a <- 1; ind.b <- grps[1]
    } else {
      ind.a <- sum(grps[1:(i-1)]) + 1; ind.b <- sum(grps[1:i])
    }
    re[ind.a:ind.b, 1] <- re[ind.a:ind.b, 1] + 
      fe[match(paste0("format2", format2[i]), names(fe))]
  }
  
  # merge and return
  params <- data.frame(params); params$rnames <- rownames(params)
  eval <- merge(params, re)
  colnames(eval) <- c(colnames(eval)[1:3], "g00.est", "xmas.est")
  eval$format2 <- gsub("[0123456789]", "", eval$rnames)
  return(eval)
}

#' @title A general purpose function for simulating MLM datasets and models
#' @description 
#' This function is the workhose for simulation of MLM datasets for specified 
#' groups and group sizes. It generates `num` datasets, fits a MLM, calculates
#' efficiency statistics across L2 and L3 parameters, and reports average
#' efficiency statistics across the datasets.
#' @param num The number of desired simulations.
#' @param grps A vector for the number of groups in each format2.
#' @param grp.size A vector or list. If a vector, uniform group size is assumed within each format2. 
#' If as list, group sizes for each format2 can be supplied.
#' @param format2 Character vector. A format2 label for the simulated data.
#' @param outliers Logical vector. Do you wish for the simulation to include level-2 outliers?
#' @param out.pct Numeric vector with elements \eqn{\in [0,1]}. The percentage of level-2 groups that 
#' are outliers for each format2. 
#' @param out.scale Numeric vector. A multiple for `hyper[1]` to scale the mean of outlying level-2 
#' groups for each format2.
#' @param xmas Logical. Whether the simulated data should include a meaningful random slope
#' @param xmas.pct Value \eqn{\in [0,1]}. The percentage of groups with a meaningful random
#' slope for xmas
#' @param hyper A list where each component is the mean and standard deviation for each hyper parameter.
#' @param seed An integer to set random seed before data generation.
mlm.sim <- function(num, grps= c(100,100,100,100), grp.size= c(30, 30, 30, 30),
                    format2= c("CD", "E Audio", "Other", "Super Audio"), 
                    outliers= rep(FALSE, 4), out.pct= rep(0.1, 4), out.scale= 3,
                    xmas= FALSE, xmas.pct= 0.1, hyper= list(), seed= 1000) {

  # 01. generate data and fit models
  g2 <- function(x) {
    get.mlm.dat <- get("gen.mlm.dat", envir= .GlobalEnv)
    return(get.mlm.dat(grps= get("grps"), grp.size= get("grp.size"), 
             format2= get("format2"), outliers= get("outliers"), out.pct= get("out.pct"),
             out.scale= get("out.scale"), xmas= get("xmas"), xmas.pct= get("xmas.pct"),
             hyper= get("hyper")))
  }
  dat.mod.list <- list(data= vector("list", num), mod= vector("list", num))
  set.seed(seed)
  dat.mod.list[[1]] <- lapply(dat.mod.list[[1]], g2) # create num datasets
  
  dat.mod.list[[2]] <- lapply(dat.mod.list[[1]], function(dat) {
    require(lme4)
    mod <- lmer(rev ~ 0 + format2 + xmas + (1+ xmas|grp.name), data= dat[[1]], REML= TRUE)
    return(mod)
  })  
  
  # 02a. approrpriately combine true parameter values and estimates and
  # calculate efficiency statistics for each dataset
  c.param.est <- get("c.param.est")
  eval.list <- vector("list", num)
  for (i in 1:num) {
    eval.list[[i]] <- c.param.est(mod= dat.mod.list[[2]][[i]], params= dat.mod.list[[1]][[i]][[2]],
                                  grps= grps, format2= format2)
    
    g00.means <- tapply(eval.list[[i]]$g00, eval.list[[i]]$format2, mean)
    g00.var   <- tapply(eval.list[[i]]$g00.est, eval.list[[i]]$format2, var)
    
    eval.list[[i]]$g00.pct.bias <- apply(eval.list[[i]], 1, function(x) {
      g00.means <- get("g00.means")
      switch(x[6],
        "CD"         = (as.numeric(x[4]) - as.numeric(x[2])) / g00.means[match("CD", names(g00.means))],
        "E Audio"    = (as.numeric(x[4]) - as.numeric(x[2])) / g00.means[match("E Audio", names(g00.means))],
        "Other"      = (as.numeric(x[4]) - as.numeric(x[2])) / g00.means[match("Other", names(g00.means))],
        "Super Audio"= (as.numeric(x[4]) - as.numeric(x[2])) / g00.means[match("Super Audio", names(g00.means))])
    })
    eval.list[[i]]$g00.mse <- apply(eval.list[[i]], 1, function(x) {
      g00.var <- get("g00.var")
      switch(x[6],
        "CD"         = (as.numeric(x[4]) - as.numeric(x[2]))^2 + g00.var[match("CD", names(g00.means))],
        "E Audio"    = (as.numeric(x[4]) - as.numeric(x[2]))^2 + g00.var[match("E Audio", names(g00.means))],
        "Other"      = (as.numeric(x[4]) - as.numeric(x[2]))^2 + g00.var[match("Other", names(g00.means))],
        "Super Audio"= (as.numeric(x[4]) - as.numeric(x[2]))^2 + g00.var[match("Super Audio", names(g00.means))])
    })
  
    # eval.list[[i]]$xmas.pct.bias <- (eval.list[[i]]$xmas.est - eval.list[[i]]$xmas.slope) / eval.list[[i]]$xmas.slope
    eval.list[[i]]$xmas.mse <- (eval.list[[i]]$xmas.slope - eval.list[[i]]$xmas.est)^2 + var(eval.list[[i]]$g00.est)
  }
  
  # 02b. calculate OVERALL average efficiency statistics
  hypers <- do.call("c",lapply(get("hyper"), "[[", 1))
  
  l2.bias <- lapply(eval.list, function(x) {tapply(x$g00.pct.bias, x$format2,  quantile, c(0.025, .5, .975), na.rm=T)})
  l2.mse  <- lapply(eval.list, function(x) {tapply(x$g00.mse, x$format2,  quantile, c(0.025, .5, .975), na.rm=T)})
  # extract by format
  l2.q.bias <- matrix(NA, nrow= 3, ncol= length(format2), dimnames= list(c("2.5%", "50%", "97.5%"), format2))
  l2.q.mse  <- matrix(NA, nrow= 3, ncol= length(format2), dimnames= list(c("2.5%", "50%", "97.5%"), format2))
  for (i in 1:length(format2)) {
    l2.q.bias[,i] <- apply(do.call("rbind", lapply(l2.bias, "[[", i)) , 2, mean)
    l2.q.mse[,i] <- apply(do.call("rbind", lapply(l2.mse, "[[", i)) , 2, mean)
  }
  
  # xmas MSE
  xmas.mse <- lapply(eval.list, function(x) {quantile(x$xmas.mse , c(0.025, .5, .975), na.rm=T)})
  xmas.q.mse  <- apply(do.call("rbind", xmas.mse), 2, mean) 
    
  # level 3 bias  
  l3.avg.pct.bias <- apply((sapply(dat.mod.list[[2]], function(x) {
    fixef(x)[-length(fixef(x))] }) - hypers) / hypers, 1, quantile, c(0.025, .5, .975), na.rm=T)
    
  # 03. return
  return(list(num.datasets= num, l2.avg.pct.bias= l2.q.bias, l2.avg.mse= l2.q.mse, 
              xmas.avg.mse= xmas.q.mse, l3.avg.pct.bias= l3.avg.pct.bias)) 
}