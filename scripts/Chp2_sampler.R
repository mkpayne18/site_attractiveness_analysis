
## This script runs a short analysis to generate out-of-sample predictions of
#estimated number of strays in streams within 40 km of a new release site
#releasing 20 million fish/year at different levels of CV flow. The end result
#is an estimate of the number of strays and a supplemental figure with bootstrapped
#prediction intervals for error.
# May 15, 2025

require(lme4)
require(tidyverse)

## read in final model object to extract its coefs:
bm <- readRDS("~/Documents/CHUM_THESIS/Chp1_analysis/output/best_model.rds")
summary(bm)$coef

eq <- function(wma = 0, cv = 0){
  y = exp(0.481 + (0.364*wma) + (0.463*cv) + (0.800*(cv^2)))
  return(y)
}

#determine SDs of covariates
stray_dat <- readRDS("~/Documents/CHUM_THESIS/Chp1_analysis/output/stray_dat.rds")
sd_wma <- sd(stray_dat$WMA_Releases_by_Yr)
sd_cv <- sd(stray_dat$CV_flow)

## Estimate % increase of strays based on 20 million fish new release site ###
# 40km release covariate
(20-mean(stray_dat$WMA_Releases_by_Yr))/sd_wma #0.13
#calculate % increase
(eq(wma = 0.13) - eq())/eq() #5% increase in strays if CV_flow constant at mean val

## CV_flow < 0.45 and WMA_40km = 20 million fish:
(0.45 - mean(stray_dat$CV_flow))/sd_cv #-1.45
(eq(wma = 0.13, cv = -1.45) - eq())/eq() #187% increase

## CV_flow > 0.58 at 20 million fish:
(0.58 - mean(stray_dat$CV_flow))/sd_cv #0.818
(eq(wma = 0.13, cv = 0.818) - eq())/eq() #161% increase



### Use bootstrapping to generate more robust predictions and prediction intervals
## Bootstrap step 1: generate distributions =================================
summary(bm) #create random normal distributions using the mean and sd for
#each model covariate

### Covariate distributions
rWMA_releas <- rnorm(1000, 0.364, 0.093)
rCV_f <- rnorm(1000, 0.463, 0.088)
rCV_f2 <- rnorm(1000, 0.800, 0.081)

### Random effect distribution
#Create df containing random intercepts
re <- as.data.frame(ranef(bm)) #specifies random EFFECTS, not intercepts
re2 <- re[,c(3:4)]
names(re2) <- c("Year", "R.Intercept")
summary(bm) #mean intercept = 0.48075
re2$R.Intercept <- re2$R.Intercept + 0.48075
sd(re2$R.Intercept) #sd of the random intercepts is 0.71179
#Random intercept distribution:
rRE <- rnorm(1000, 0.48075, 0.71179)


## Bootstrap step 2: make predictions, sampling from covariate dists ===========
#2a. Predictions for CV of flow constant at mean val:
eq2 <- function(R.Intercept, WMA, CVf, CVf2, cv = 0){
  y = exp(R.Intercept + (WMA*0.13) + (CVf*cv) + (CVf2*(cv^2)))
  return(y)
}

#df to store predictions:
mod_preds_meanCV <- vector(mode = "numeric", length = 1000L)

#For loop to run model!
for (i in 1:1000) {
  R.Intercept <- sample(rRE, 1, replace = T)
  WMA <- sample(rWMA_releas, 1, replace = T)
  CVf <- sample(rCV_f, 1, replace = T)
  CVf2 <- sample(rCV_f2, 1, replace = T)
  mod_preds_meanCV[i] <- eq2(R.Intercept, WMA, CVf, CVf2)
}

mean(mod_preds_meanCV)
sd(mod_preds_meanCV)
result <- data.frame(CV_flow = "0.53 (Mean)", pred_val = mean(mod_preds_meanCV),
           sd = sd(mod_preds_meanCV))


#2b. CV of flow less than 0.45 at 20 million fish release:
#df to store predictions:
mod_preds_0.45CV <- vector(mode = "numeric", length = 1000L)

#For loop to run model!
for (i in 1:1000) {
  R.Intercept <- sample(rRE, 1, replace = T)
  WMA <- sample(rWMA_releas, 1, replace = T)
  CVf <- sample(rCV_f, 1, replace = T)
  CVf2 <- sample(rCV_f2, 1, replace = T)
  mod_preds_0.45CV[i] <- eq2(R.Intercept, WMA, CVf, CVf2,
                             cv = (0.45 - mean(stray_dat$CV_flow))/sd_cv)
}

mean(mod_preds_0.45CV)
sd(mod_preds_0.45CV)
result2 <- data.frame(CV_flow = "< 0.45", pred_val = mean(mod_preds_0.45CV),
                     sd = sd(mod_preds_0.45CV))

#2c. CV of flow greater than 0.58 at 20 million fish release:
mod_preds_0.58CV <- vector(mode = "numeric", length = 1000L)

#For loop to run model!
for (i in 1:1000) {
  R.Intercept <- sample(rRE, 1, replace = T)
  WMA <- sample(rWMA_releas, 1, replace = T)
  CVf <- sample(rCV_f, 1, replace = T)
  CVf2 <- sample(rCV_f2, 1, replace = T)
  mod_preds_0.58CV[i] <- eq2(R.Intercept, WMA, CVf, CVf2,
                             cv = (0.58 - mean(stray_dat$CV_flow))/sd_cv)
}

mean(mod_preds_0.58CV)
sd(mod_preds_0.58CV)
result3 <- data.frame(CV_flow = "> 0.58", pred_val = mean(mod_preds_0.58CV),
                      sd = sd(mod_preds_0.58CV))

pred_strays <- rbind.data.frame(result2, result, result3)
pred_strays$lower <- pred_strays$pred_val - pred_strays$sd
pred_strays$upper <- pred_strays$pred_val + pred_strays$sd

#re-order factor
pred_strays <- pred_strays %>%
  mutate(CV_flow = factor(CV_flow, levels = c("< 0.45", "0.53 (Mean)", "> 0.58"),
                          labels = c("< 0.45", "0.53 (Mean)", "> 0.58")))



## Plot ========================================================================
p <- ggplot(pred_strays, aes(CV_flow, pred_val)) +
  geom_col() + geom_errorbar(ymin = pred_strays$lower, ymax = pred_strays$upper,
                  width = 0.2, linewidth = 0.2) +
  theme_classic() +
  ylim(c(0,13)) +
  labs(x = "CV of flow",
       y = "Predicted number of hatchery strays in nearby streams") +
  theme(axis.text = element_text(size =6),
        axis.title = element_text(size = 7),
        text=element_text(family="Times New Roman"),
        axis.line = element_line(linewidth = 0.2),
        axis.ticks = element_line(linewidth = 0.2))

# Export as high-res figure
tiff("figs/supp_chp2_sample_fig.tiff", width = 8.5, height = 7, pointsize = 12,
     units = 'cm', res = 600)
p
dev.off()
                     