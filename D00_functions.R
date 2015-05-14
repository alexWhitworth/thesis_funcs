
# level 1 residuals for a single data set and model. 
# also does some ancillary clean up to the data and provides additional output
# input = list(data, model)
level1_resid <- function(lst, type= "EB") {
  dat <- lst[[1]]; mod <- lst[[2]]
  require(HLMdiag); require(lme4)
  # remove problematic level two groups
  dat$title_format2 <- factor(as.character(dat$title_format2))
  grps <- as.integer(table(dat$title_format2))
  nms <- names(table(dat$title_format2)[which(grps > 30)]) # errors
  nms2 <- names(table(dat$title_format2)[which(grps < 30)]) # which, missing data
  indices <- which(dat$title_format2 %in% nms)
  
  # fix/remove problem points
  dat <- dat[!is.na(dat$title_format2), ]
  dat$yr_since_rel <- ifelse(dat$yr_since_rel >=60, 60, dat$yr_since_rel)
    
  # remove problem titles and refit
  if (length(indices) > 0 ) {dat <- dat[-indices, ]}
  mod <- lmer(mod@call[2], data= dat)
  # table missingness
  grps <- as.integer(table(dat$title_format2))
  cnts <- ifelse(grps == 30, "30", 
                 ifelse(grps > 24 & grps <= 29, "25-29",
                        ifelse(grps > 19 & grps <= 24, "20-24", "0-19")))
  
  # get level 1 residuals
  res_std  <- HLMresid(mod, level= 1, type= type, standardize= TRUE) # similar
  res_semi <- HLMresid(mod, level= 1, type= type, standardize= "semi") 
  
  # return
  return(list(data= dat, mod= mod, res_std= res_std, res_semi= res_semi, miss= nms2,
              miss_bins= table(cnts)))
}

# wrapper for above function to apply to multiple datasets / models
all_level1_resid <- function(dat_list, mod_list, type= "EB") {
  if (length(dat_list) != length(mod_list)) {
    stop("Error: data and model lists have different lengths.")
  }
  cnt_i <- length(dat_list)
  level1_resid <- get("level1_resid", envir= .GlobalEnv)
  
  inp <- list()
  for (i in 1:cnt_i) {
    inp[[i]] <- list(dat= dat_list[[i]], mod= mod_list[[i]])
  }
  # try, catch error messages
  outp <- lapply(inp, function(x) tryCatch(level1_resid(lst= x, type= type),
                        error= function(c) conditionMessage(c),
                        warning= function(c) conditionMessage(c)))
  
  # maintain naming and return
  names(outp) <- names(mod_list) # assumes models are named
  return(outp)
}

# alternate version of above -- for 3 level models
level1_resid2 <- function(lst, type= "EB") {
  dat <- lst[[1]]; mod <- lst[[2]]
  require(HLMdiag); require(lme4)
  # remove problematic level two groups
  dat$title_format2 <- factor(as.character(dat$title_format2))
  dat$geo_title_format2 <- interaction(dat$geography, dat$title_format2)
  grps <- as.integer(table(dat$geo_title_format2))
  nms <- names(table(dat$geo_title_format2)[which(grps > 30)]) # errors
  nms2 <- names(table(dat$geo_title_format2)[which(grps < 30)]) # which, missing data
  indices <- which(dat$geo_title_format2 %in% nms)
  
  # fix/remove problem points
  dat <- dat[!is.na(dat$geo_title_format2), ]
  dat$yr_since_rel <- ifelse(dat$yr_since_rel >=60, 60, dat$yr_since_rel)
  
  # remove problem titles and refit
  if (length(indices) > 0 ) {dat <- dat[-indices, ]}
  mod <- lmer(mod@call[2], data= dat)
  # table missingness
  grps <- as.integer(table(dat$geo_title_format2))
  cnts <- ifelse(grps == 30, "30", 
                 ifelse(grps > 24 & grps <= 29, "25-29",
                        ifelse(grps > 19 & grps <= 24, "20-24", "0-19")))
  
  # get level 1 residuals
  res_std  <- HLMresid(mod, level= 1, type= type, standardize= TRUE) # similar
  res_semi <- HLMresid(mod, level= 1, type= type, standardize= "semi") 
  
  # return
  return(list(data= dat, mod= mod, res_std= res_std, res_semi= res_semi, miss= nms2,
              miss_bins= table(cnts)))
}

# alternate version of above -- for 3 level models
all_level1_resid2 <- function(dat_list, mod_list, type= "EB") {
  if (length(dat_list) != length(mod_list)) {
    stop("Error: data and model lists have different lengths.")
  }
  cnt_i <- length(dat_list)
  level1_resid <- get("level1_resid", envir= .GlobalEnv)
  
  inp <- list()
  for (i in 1:cnt_i) {
    inp[[i]] <- list(dat= dat_list[[i]], mod= mod_list[[i]])
  }
  # try, catch error messages
  outp <- lapply(inp, function(x) tryCatch(level1_resid2(lst= x, type= type),
                                           error= function(c) conditionMessage(c),
                                           warning= function(c) conditionMessage(c)))
  
  # maintain naming and return
  names(outp) <- names(mod_list) # assumes models are named
  return(outp)
}

# test1 <- level1_resid(list(dat= usa_p[[1]], mod= mod_usa$physical[[1]]), type= "LS)
# test2 <- all_level1_resid(dat_list= usa_p, mod_list= mod_usa$physical)
# both working

# test1 <- level1_resid2(list(ccr[[1]], mod_ccr[[1]]))
# test2 <- all_level1_resid2(dat_list= ccr, mod_list= mod_ccr)


# look at groupwise residuals via boxplot stats
group_iqr <- function(dat, res) {
  dat2 <- data.frame(title_format2=dat$title_format2, res=res)
  b.stats <- ddply(dat2, .var= "title_format2", .fun= function(x) {
    boxplot.stats(x$res)$stats})
  names(b.stats) <- c("title_format2", "min", "q25", "q50", "q75", "max")
  b.stats$iqr <- b.stats[,5] - b.stats[,3]
  b.stats <- b.stats[order(b.stats$iqr, decreasing=T),]
  return(b.stats)
}

# 1 liner for plotting
res_v_fit <- function(var, res) {
  qplot(x= var, y= res, geom= c("point", "smooth"))
}

# level 2 residuals and cooks distance
res_cook <- function(x, grp= "title_format2") {
  x$res2 <- HLMresid(object= x$mod, level= grp)
  x$cooks1 <- cooks.distance(x$mod)
  x$cooks2 <- cooks.distance(x$mod, group= grp)
  return(x)
}


