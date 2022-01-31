rm(list=ls()) #install packages
# dataset
data("BtheB", package = "HSAUR2")
BtheB$subject <- factor(rownames(BtheB))
nobs <- nrow(BtheB)
BtheB_long <- reshape(BtheB, idvar = "subject",
                         varying = c("bdi.2m", "bdi.3m", "bdi.5m", "bdi.8m"),
                         direction = "long")
BtheB_long$time <- rep(c(2, 3, 5, 8), rep(nobs, 4))
# such that the data are now in the form (here shown for the first three subjects)
subset(BtheB_long, subject %in% c("1", "2", "3"))
# The resulting data.frame BtheB_long contains a number of missing values

# GEE for longitodinal regression
# install.packages("gee")
library(gee)
# The gee function is used in a similar way to the lme function with the addition of the features 
# of the glm function that specify the appropriate error distribution for the response and the implied 
# link function, and an argument to specify the structure of the working correlation matrix. 
# Here we will fit an independence structure and then an exchangeable structure.
# The R code for fitting generalised estimation equations to the BtheB_long data with identity working 
# correlation matrix is as follows (note that the gee function assumes the rows of the data.frame BtheB_long to be ordered 
#with respect to subjects):
osub <- order(as.integer(BtheB_long$subject))
BtheB_long <- BtheB_long[osub,]
btb_gee <- gee(bdi ~ bdi.pre + treatment + length + drug, 
                      data = BtheB_long, id = subject, family = gaussian, corstr = "independence")
# and with exchangeable correlation matrix:
btb_gee1 <- gee(bdi ~ bdi.pre + treatment + length + drug,
                     data = BtheB_long, id = subject, family = gaussian, corstr = "exchangeable")
# summary of the model  
summary(btb_gee)
  