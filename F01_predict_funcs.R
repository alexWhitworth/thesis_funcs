
# take an input list of models, newdata, RE slope declaration, and inverse transformation function
# return set of predicted values and SE which have been transformed to original scale.
# Note: assumes that variance structure is homogenous at level 2 (intercepts)
pred.se.set <- function(obj.list, newdata, allow.new.levels= TRUE,
                        trans.func, alpha= 0.05, R.slope= "none",
                        revtype= NULL, title= FALSE, level3= FALSE) 
{  
  require(lme4)
  # 01. Remove obs from newdata with new FE levels in format2
  if (level3 == FALSE) {
    fe.names <- names(fixef(obj.list[[1]]))
    dat.levels <- names(table(newdata$format2))
    dat.levels2 <- paste0("format2", dat.levels)
    dat.levels <- dat.levels[!(dat.levels2 %in% fe.names)] # extract problematic levels
    newdata <- newdata[!(newdata$format2 %in% dat.levels),]
  } else if (level3 == TRUE) {
    fe.names <- names(fixef(obj.list[[1]]))
    dat.levels <- names(table(interaction(paste0("geography", newdata$geography) , paste0("format2", newdata$format2))))
    newdata$l3 <- interaction(paste0("geography", newdata$geography) , paste0("format2", newdata$format2))
    dat.levels <- gsub("[.]", ":", dat.levels)
    dat.levels2 <- dat.levels[!(dat.levels %in% fe.names)] # extract problematic levels
    dat.levels2 <- gsub("[:]",".", dat.levels2)
    newdata <- newdata[!(newdata$l3 %in% dat.levels2),]
  }
  
  # 01(b) -- preliminaries
  n <- nrow(newdata)
  p <- length(obj.list)
  pred.dat <- list()
  
  
  # 02. get predictions and SE for each model
  for (i in 1:length(obj.list)) {
    # return DF
    pdat1 <- data.frame(l= rep(NA, n), y_hat= rep(NA, n), u= rep(NA,n))
    
    # *** control for bug -- non-conformable arguments ***
    # RE: Ben Bolker (email: 3/23/2015)
    mf <- model.frame(obj.list[[i]])
    if (title == FALSE) {
      newdata2 <- transform(newdata, format2= factor(format2, levels= unique(mf$format2)))
    } else if (title == TRUE) {
      newdata2 <- transform(newdata, title= factor(title, levels= unique(mf$title)))
    }

    # point prediction
    yhat <- predict(obj.list[[i]], newdata= newdata2,
                    allow.new.levels= allow.new.levels)
    # make RE model matrix
    if (R.slope == "none") {
      z.mm <- rep(1, length= n)
    } else if (R.slope == "xmas") {
      z.mm <- cbind(rep(1, length= n), newdata$xmas)
    } else if (R.slope == "mf_day") {
      z.mm <- cbind(rep(1, length= n), newdata$mf_day)
    } else if (R.slope == "both") {
      z.mm <- cbind(rep(1, length= n), newdata$mf_day, newdata$xmas)
    }
    
    # SE of prediction
    V2   <- z.mm %*% VarCorr(obj.list[[i]])[[1]] %*% t(z.mm) # level 2 covariance matrix
    V1  <- diag(attributes(VarCorr(obj.list[[i]]))$sc^2, nrow= n, ncol= n) # level 1 variance
    V <- V2 + V1 # overall variance matrix
    y_se <- sqrt(diag(V)) * qnorm(1-alpha/2) # SE of prediction estimate
    
    # return
    pdat1[,1] <- trans.func(yhat - y_se)
    pdat1[,2] <- trans.func(yhat)
    pdat1[,3]   <- trans.func(yhat + y_se)
    pred.dat[[i]] <- pdat1
  }
  
  # 03. attach actual values to predictions. Return output
  pred.dat <- lapply(pred.dat, function(x) {
    if (revtype == "physical") {
      x$y <- newdata$physical
    } else if (revtype == "digital") {
      x$y <- newdata$digital
    } else if (revtype == "streaming") {
      x$y <- newdata$streaming
    }
    return(x)
  })
  
  return(list(newdata, pred.dat))
}


# Evaluation function
#-------------------------------------------------------
eval.pred <- function(obj.list, true.val= "y", pred.val= "y_hat", low= "l", upp= "u") {
  # take a list of predicted values and bounds (from pred.se.fit above) and
  # evaluate the predictions via a set of evaluation metrics: 
  #  --- RMSE, Mean Abs Dev (mnAD), Wtd Mean Abs Dev (wmnAD), Max Abs Dev (mxAD), 
  # % classified accurately
  
  n <- length(obj.list)
  mod.eval <- matrix(NA, nrow= n, ncol= 5, dimnames= list(paste("mod", 1:n, sep="-"), 
                        c("rmse", "Mean AD", "wt Mean AD", "Max AD", "Pct Class")))
  # 01. define evaluation functions
  rmse <- function(obs, pred) {sqrt(mean(obs - pred, na.rm=T)^2)}
  mnAD <- function(obs, pred) {mean(abs(obs - pred), na.rm=T)}
  wmnAD <- function(obs, pred) {
    wts <- log(abs(pred) + 2)
    return(1/ sum(wts, na.rm=T) * sum(wts * abs(obs - pred), na.rm=T))
  }
  mxAD <- function(obs, pred) {max(abs(obs - pred), na.rm=T)}
  class.pct <- function(obs, low, upp) {
    y <- ifelse(obs >= low & obs <= upp, 1, 0)
    return(sum(y, na.rm=T) / length(y[!is.na(y)]))
  }
  
  # 02. Evaluate
  for (i in 1:n) {
    x <- obj.list[[i]]
    obs <- x[[true.val]]; pred <- x[[pred.val]]
    mod.eval[i, 1] <- rmse(obs= obs, pred= pred)
    mod.eval[i, 2] <- mnAD(obs= obs, pred= pred)
    mod.eval[i, 3] <- wmnAD(obs= obs, pred= pred)
    mod.eval[i, 4] <- mxAD(obs= obs, pred= pred)
    mod.eval[i, 5] <- class.pct(obs= obs, low= x[[low]], upp= x[[upp]])
  }
  # 03. return
  return(mod.eval)
}

