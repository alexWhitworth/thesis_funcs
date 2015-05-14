
setwd("//cmgfs/users/alewit/2014 Projects/2015-03 Sim Study1")
source("./F01_sim_funcs.R")

# all functions are functional!!
# test <- gen.format(grps= 110, grp.size= c(rep(30,50), rep(10,60)), xmas= FALSE, xmas.pct= 0.1,
#                    format2= "CD", hyper= c(4.4, 1.5))
# test2 <- gen.mlm.dat(grps= c(100,100,100,100), grp.size= c(30,30,30,30),
#          format2= c("CD", "E Audio", "Other", "Super Audio"), 
#          xmas= FALSE, xmas.pct= 0.1, hyper= list(c(4.4,2), c(3.2,1.5), c(1.5,1), c(4,1.5)))
# test2 <- gen.mlm.dat(grps= c(100,100,100,100), grp.size= list(c(rep(30,50), rep(20, 50)), c(rep(30,50), rep(20, 50)),
#                                                               c(rep(30,50), rep(20, 50)), c(rep(30,50), rep(20, 50))),
#                      format2= c("CD", "E Audio", "Other", "Super Audio"), 
#                      xmas= FALSE, xmas.pct= 0.1, hyper= list(c(4.4,2), c(3.2,1.5), c(1.5,1), c(4,1.5)))
# test3 <- mlm.sim(num= 20, grps= rep(100,4), grp.size= rep(30,4), xmas=TRUE, xmas.pct= 0.1,
#                  format2= c("CD", "E Audio", "Other", "Super Audio"),
#                  hyper= list(c(4.4,1), c(3.2,1), c(1.5,1), c(4,1)))  
# 
# test3 <- mlm.sim(num= 20, grps= rep(100,4), grp.size= list(c(rep(30,50), rep(20, 50)), c(rep(30,50), rep(20, 50)),
#                                                            c(rep(30,50), rep(20, 50)), c(rep(30,50), rep(20, 50))), 
#                  xmas=TRUE, xmas.pct= 0.1,
#                  format2= c("CD", "E Audio", "Other", "Super Audio"),
#                  hyper= list(c(4.4,1), c(3.2,1), c(1.5,1), c(4,1)))

### outliers test; all functional
# test <- gen.format(grps= 110, grp.size= c(rep(30,50), rep(10,60)), xmas= FALSE, xmas.pct= 0.1,
#                    outliers= TRUE,
#                    format2= "CD", hyper= c(4.4, 1.5))
# test2 <- gen.mlm.dat(grps= c(100,100,100,100), grp.size= c(30,30,30,30),
#                      format2= c("CD", "E Audio", "Other", "Super Audio"), 
#                      outliers=TRUE,
#                      xmas= FALSE, xmas.pct= 0.1, hyper= list(c(4.4,2), c(3.2,1.5), c(1.5,1), c(4,1.5)))
# test2 <- gen.mlm.dat(grps= c(100,100,100,100), grp.size= list(c(rep(10, 25), rep(15, 25), rep(20, 25), rep(30, 25)),
#                                                              rep(30, 100), rep(30, 100), rep(30, 100)),
#                      format2= c("CD", "E Audio", "Other", "Super Audio"), 
#                      outliers=TRUE,
#                      xmas= FALSE, xmas.pct= 0.1, hyper= list(c(4.4,2), c(3.2,1.5), c(1.5,1), c(4,1.5)))

# test3 <- mlm.sim(num= 20, grps= rep(100,4), grp.size= rep(30,4), xmas=TRUE, xmas.pct= 0.1,
#                  format2= c("CD", "E Audio", "Other", "Super Audio"),
#                  outliers= TRUE,
#                  hyper= list(c(4.4,1), c(3.2,1), c(1.5,1), c(4,1)))  


# test <- mlm.sim(num= 100, grps= rep(100,4), grp.size= rep(30,4), xmas=TRUE, xmas.pct= 0.1,
#                 format2= c("CD", "E Audio", "Other", "Super Audio"),
#                 hyper= list(c(4.4,1), c(3.2,1), c(1.5,1), c(4,1)))