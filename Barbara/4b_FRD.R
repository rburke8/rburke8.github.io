## 	Barbara Sianesi
##  (barbarasianesi@gmail.com)
##	POLICY EVALUATION METHODS Course
##	
##	PRACTICAL SESSION 4b 
##	REGRESSION DISCONTINUITY DESIGN: FUZZY  
##  ************************************************************************

##	Some initial R code kindly contributed by Anas Abu (formerly at DCMS)
##  Thank you also to Alex Stroud (DCMS) & Leo Gada (DWP)
## 	Any mistakes remain mine (Barbara)
##	In the spirit of R, if you can improve this script, please let me know :)


#install.packages("rdrobust")
#install.packages("estimatr")
#install.packages("summarytools")
library(summarytools)
library(rdrobust)
library(tidyverse)
library(dplyr)
library(MatchIt)
library(estimatr)
library(jtools)
library(ivreg)
library(AER)


#setwd("your working directory")

nsw_psid_rdd <- read.csv("nsw_psid_rdd.csv")
freq(nsw_psid_rdd$treated, report.nas = FALSE)

# a) P(D=1 | Z)

rdplot(nsw_psid_rdd$treated, nsw_psid_rdd$re75, c = 1000, subset = nsw_psid_rdd$re75 < 2000 )



# b) Parametric - IV via 2SLS
# ************************************************************************

nsw_psid_rdd <- nsw_psid_rdd %>%
  mutate(jump = ifelse(re75<=1000, 1, 0),
  		 re75_2 = re75^2, 
  		 re75_3 = re75^3, 
  		 re75_4 = re75^4)

summary(ivreg(re78 ~ treated + re75 | re75 + jump, data = nsw_psid_rdd))
summ(lm(treated ~ jump + re75, data = nsw_psid_rdd), digits = 3)

# first stage quartic
stage1 <- lm(treated ~ jump + re75 + re75_2 + re75_3 + re75_4, data = nsw_psid_rdd)
summ(stage1, digits = 5)
linearHypothesis(stage1, c("jump=0"))
	
summary(ivreg(re78 ~ treated + re75 + re75_2 | re75 + re75_2 + jump, data = nsw_psid_rdd))
# doesn't seem to work for this or any higher order IV model

# I think this could be due to some 'high' correlation between the re* terms?
# And/or the significant but de facto 0 effect of higher order re terms in both stages?
cor(nsw_psid_rdd[, c("treated", "re75", "re75_2", "re75_3", "re75_4", "jump")])


# However in Stata it works (maybe a different tolerance?).. 
# .. and it works manually (!):
stage1 <- lm(treated ~ re75 + re75_2 + jump, data = nsw_psid_rdd)
summ(stage1, digits = 3)
tr.hat <- fitted.values(stage1)
summ(lm(re78 ~ tr.hat + re75 + re75_2, data = nsw_psid_rdd), digits = 3)

summ(lm(treated ~ re75 + re75_2 + re75_3 + jump,data = nsw_psid_rdd), digits = 7)
tr.hat <- fitted.values(stage1)
summ(lm(re78 ~ tr.hat + re75 + re75_2 + re75_3, data = nsw_psid_rdd), digits = 7)


# Covariates
summary(ivreg(re78 ~ treated + re75 + age + black | re75 + age + black + jump,data = nsw_psid_rdd))	   
# Again doesn't seem to work with further polynomial terms
summ(lm(treated ~ re75 + re75_2 + age + black + jump,data = nsw_psid_rdd), digits = 3)
tr.hat <- fitted.values(stage1)
summ(lm(re78 ~ tr.hat + re75 + re75_2 + age + black, data = nsw_psid_rdd), digits = 3)



# c) 2SLS within discountinuity samples
# ************************************************************************

ds5_subset <- nsw_psid_rdd %>% 
  filter(re75>=(1000-500) & re75<=(1000+500))

ds2_subset <- nsw_psid_rdd %>% 
  filter(re75>=(1000-200) & re75<=(1000+200))

ds1_subset <- nsw_psid_rdd %>% 
  filter(re75>=(1000-100) & re75<=(1000+100))

nsw_psid_rdd <- nsw_psid_rdd %>%
  mutate(jump = ifelse(re75<=1000, 1, 0),
         re75_2 = re75^2, 
         re75_3 = re75^3, 
         re75_4 = re75^4)


summary(ivreg(re78 ~ treated + re75 | re75 + jump, data = ds5_subset))
summary(ivreg(re78 ~ treated + re75 | re75 + jump, data = ds1_subset))
summary(ivreg(re78 ~ treated | jump, data = ds1_subset))



# d) Non-parametric Wald estimator (local LATE)
# ************************************************************************

summary(rdrobust(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000, fuzzy = nsw_psid_rdd$treated))
summary(rdrobust(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000, fuzzy = nsw_psid_rdd$treated, bwselect = "msetwo"))
summary(rdrobust(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000, fuzzy = nsw_psid_rdd$treated, bwselect = "certwo"))

# Note here we get the right sign as Wald ratio will deal with it




