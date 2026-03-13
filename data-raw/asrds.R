## Code to prepare `asrds` (Adapted Self-Report Delinquency Scale) dataset

# We imagine that we want to estimate the association between family income when
# a child is age 5 years, and child's Adapted Self-Report Delinquency Scale
# (ASRDS) value at age 17 years.

# We assume this association is confounded by maternal education (which we also
# treat as a proxy for SEP) and mediated by child's ASRDS value at 14 years.

# These variables are generated as follows (for a sample of 1000 children):

# maternal education (mated) ~ Bernoulli(0.6) (post-16 qual or not)

# log of family income (log_income) = 2 * mated + 8.5 + ε , where ε ~ N(0,0.5^2)
# with parameters chosen to give a median, IQR of log(family income) of approx
# 10 and 9-11, in line with published values (Noonan et al, 2018,
# https://doi.org/10.1016/j.ssmph.2018.03.002)

# Parameters for both ASRDS data-generating models were chosen to
# give marginal means and SDs of approx 65 and 15 (for both scores).

# ASRDS at 14 years (asrds14), calculated as a % of the maximum possible
# square-root value of ASRDS, such that asrds14 depends on
# mated and log_income:
# asrds14 = -4 * mated - 7 * log_income + 110 + ε , where ε ~ N(0,8^2)

# ASRDS at 17 years (asrds17), calculated as a % of the maximum possible
# square-root value of ASRDS, such that asrds17 depends on mated, log_income and
# asrds14:
# asrds17 = 0.5 * asrds14 - 3 * mated - 7 * log_income - 30 + ε ,
# where ε ~ N(0,6^2)

set.seed(5668)

mated <- rbinom(1000,1,0.6)

log_income5 <- rnorm(1000, 2*mated + 8.5, 0.5)
summary(log_income)

asrds14 <- rnorm(1000, -4*mated - 7*log_income + 115, 8)
summary(asrds14)

asrds17 <- rnorm(1000, 0.5*asrds14 - 3*mated - 6*log_income + 90, 6)
summary(asrds17)

# Missing mechanism for log_income: MAR | mated and asrds14
# Generate missing values of log_income - depending on mated and asrds14

log_income_m1 <- log_income5
for (i in 1:1000){
  log_income_m1[i] <- ifelse(rbinom(1,1,exp(1.5*mated[i] - 0.8*asrds14[i] + 40)/
                                (1+exp(1.5*mated[i] - 0.8*asrds14[i] + 40)))==0,
                              NA, log_income5[i])
}
summary(log_income_m1)

#Create data frame
asrds<-data.frame(asrds17=asrds17, log_income5=log_income_m1,mated=mated,
                asrds14=asrds14)

#Create missing indicator - note this is also the complete_record indicator
asrds$r_cra <- ifelse(apply(asrds,1,anyNA)==F,1,0)

# Check if there is an interaction between gcse_score and log_income in the
# log-additive model for selection under the chosen missingness mechanism
r_cra <- asrds$r_cra
summary(glm(r_cra~asrds17*log_income5, family=poisson(log)))
# There is an interaction so expecting bias in CRA of 0.026
# (Gkatzionis  et al, 2025, https://doi.org/10.1177/09622802241306860)

#Define binary variables as factors
asrds$mated <- as.factor(asrds$mated)
asrds$r_cra <- as.factor(asrds$r_cra)

summary(asrds)

# Check predictors of missingness are as expected
summary(glm(r_cra~mated+asrds14+asrds17, family=binomial(logit),
            data=asrds))
# As expected

# Add asdrs dataset to the 'midoc' package
usethis::use_data(asrds, overwrite = TRUE)


############################################################################
# 'asrds' dataset exploration
library(midoc)

data(asrds)

#descMissData
descMissData(y="asrds17",covs=c("log_income5","mated","asrds14"), data=asrds)
descMissData(y="asrds17",covs=c("log_income5","asrds14"), by="mated", data=asrds)


# Full data estimate (using variables derived in asrds.R)
full <- lm(asrds17~log_income5+mated)
c(full$coefficients[2], confint(full)[2,])
#log_income5       2.5 %      97.5 %
#  -10.28141   -11.14901    -9.41381

# Interpretation of the assoc with asrds17 in terms of income (rather than
# log_income) is helpful here

# For every unit increase in log_income, there is a 9.72 point increase in
# standardised ASRDS, after adjusting for maternal education level
range(log_income5)
# 7.011097 12.121579
hist(log_income5)

x<-c(7:12)
plot(x=x, y=c(-10.28*(x-8.99)),type='o',pch=18,xlab="Log income",
     ylab="")
title(ylab="Mean difference in scaled ASRDS at age 17 relative \nto a child with log(family income) of 9",
      mgp=c(2,1,0),cex.lab=0.9)
# On the income scale:
plot(x=exp(x), y=c(-10.28*(x-8.99)),type='o',pch=18,xlab="Income (£ pa)",
     ylab="")
title(ylab="Mean difference in scaled ASRDS at age 17 relative \nto a child with family income of ~£8000",
      mgp=c(2,1,0),cex.lab=0.9)

# CRA - attenuated and less precise
cra <- lm(asrds17~log_income5+mated, data=asrds)
summary(cra)
c(cra$coefficients[2], confint(cra)[2,])
#log_income      2.5 %     97.5 %
#  -9.274852  -10.329291   -8.220413

# CRA estimate of marginal association is even more attenuated
fullm <- lm(asrds17~log_income5)
c(fullm$coefficients[2], confint(full)[2,])
#log_income      2.5 %     97.5 %
#-11.35305  -11.14901   -9.41381
cram <- lm(asrds17~log_income5, data=asrds)
c(cram$coefficients[2], confint(cra)[2,])
#log_income      2.5 %     97.5 %
#-9.529001  -10.329291   -8.220413

# Define mDAG
asrds_mdag <- 'dag {
  asrds14 [pos="0.3,-0.5"]
  asrds17 [pos="1.7,0.7"]
  log_income [pos="-1.5,0.7"]
  mated [pos="-2,-1"]
  r_cra [pos="1.7,-1"]
  asrds14 -> asrds17
  asrds14 -> r_cra
  log_income -> asrds14
  log_income -> asrds17
  mated -> asrds14
  mated -> asrds17
  mated -> log_income
  mated -> r_cra
}'
plot(dagitty::dagitty(asrds_mdag))

checkCRA("asrds17","log_income","r_cra",asrds_mdag)
checkCRA("asrds17","log_income mated asrds14","r_cra",asrds_mdag)

checkMI("log_income","asrds17 mated","r_cra", asrds_mdag)
checkMI("log_income","asrds17 mated asrds14","r_cra", asrds_mdag)

logincome_mod_aux <- checkModSpec("log_income5 ~ asrds17 + mated + asrds14","gaussian(identity)",asrds)
# Random scatter though clearly bi-modal
modfit_mated0 <- lm(log_income5~asrds17+asrds14, data=asrds, subset=(mated=="0"))
plot(y=modfit_mated0[["residuals"]],x=modfit_mated0[["fitted.values"]],xlab="",ylab="",
     main="Residuals versus fitted values")
modfit_mated1 <- lm(log_income5~asrds17+asrds14, data=asrds, subset=(mated=="1"))
plot(y=modfit_mated1[["residuals"]],x=modfit_mated1[["fitted.values"]],xlab="",ylab="",
     main="Residuals versus fitted values")
#Could be an argument for stratified MI although no of observations with mated==0
# is quite small which could be an argument for a combined model

#Note that if we had imputed income instead, our model would be mis-specified
income_mod_aux <- checkModSpec("I(exp(log_income5))~ asrds17 + mated + asrds14","gaussian(identity)",asrds)

# Formulate best mice options
propMI_aux <- proposeMI(mimodobj=logincome_mod_aux, data=asrds)
# Note the difference in the distribution between observed and imputed values
# does not imply data are MNAR - may be a consequence of data MAR

doMImice(propMI_aux,123,"lm(asrds17~log_income5+mated)")
# log_income5   -10.129416 -11.151069  -9.1077640

# And without aux variable
logincome_mod <- checkModSpec("log_income5~asrds17+mated","gaussian(identity)",asrds)

propMI <- proposeMI(mimodobj=logincome_mod, data=asrds)

doMImice(propMI,123,"lm(asrds17~log_income5+mated)")
#Only very slightly biased but larger SE
# log_income   -10.018579 -11.122812  -8.914345

