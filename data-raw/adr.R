## Code to prepare `adr` (Administrative Data for Research) dataset

# We imagine that we want to estimate the association between family income when
# a child is age 5 years, and child's GCSE score (at approx. age 16 years).

# We assume this association is confounded by maternal education (which we also
# treat as a proxy for SEP) and mediated by child's Key Stage 2 score (at
# approx. age 11 years).

# These variables are generated as follows (for a sample of 1000 children):

# maternal education (mated) ~ Bernoulli(0.6) (post-16 qual or not)

# log of family income (log_income) = 2 * mated + 8.5 + ε , where ε ~ N(0,0.5^2)
# with parameters chosen to give a median, IQR of log(family income) of approx
# 10 and 9-11, in line with published values (Noonan et al, 2018,
# https://doi.org/10.1016/j.ssmph.2018.03.002)

# Parameters for both KS2 and GCSE score data-generating models were chosen to
# give marginal means and SDs of approx 65 and 15 (for both scores), in line
# with published values (Cornish et al, 2015 and 2017,
# https://doi.org/10.1093/ije/dyv035, https://doi.org/10.1186/s12982-017-0068-0)

# KS2 score (ks2_score), defined as the total of an individual's English, maths
# and science scores, calculated as a %, such that ks2_score depends on
# mated and log_income:
# ks2_score = 4 * mated + 7 * log_income - 15 + ε , where ε ~ N(0,8^2)

# GCSE score (gcse_score), defined as the total of an individual’s top eight GCSE
# qualifications, ranked in terms of points, then calculated as a %, such that
# gcse_score depends on mated, log_income and ks2_score:
# gcse_score = 0.5 * ks2_score + 3 * mated + 7 * log_income - 38 + ε ,
# where ε ~ N(0,6^2)

set.seed(5668)

mated <- rbinom(1000,1,0.6)

log_income <- rnorm(1000, 2*mated + 8.5, 0.5)
summary(log_income)

ks2_score <- rnorm(1000, 4*mated + 7*log_income - 15, 8)
summary(ks2_score)

gcse_score <- rnorm(1000, 0.5*ks2_score + 3*mated + 7*log_income - 38, 6)
summary(gcse_score)

# Missing mechanism for log_income: MAR | mated and ks2_score
# Generate missing values of log_income - depending on mated and ks2_score

log_income_m1 <- log_income
for (i in 1:1000){
  log_income_m1[i] <- ifelse(rbinom(1,1,exp(3*mated[i] + 0.5*ks2_score[i]-30)/
                                (1+exp(3*mated[i]+ 0.5*ks2_score[i]-30)))==0,
                              NA, log_income[i])
}
summary(log_income_m1)

#Create data frame
adr<-data.frame(gcse_score=gcse_score, log_income=log_income_m1,mated=mated,
                ks2_score=ks2_score)

#Create complete_record indicator
adr$r_cra <- ifelse(apply(adr,1,anyNA)==F,1,0)

#Create indiv variable missingness indicator for log_income
adr$r_log_income <- ifelse(is.na(adr$log_income)==F,1,0)

# Check if there is an interaction between gcse_score and log_income in the
# log-additive model for selection under the chosen missingness mechanism
r_cra <- adr$r_cra
summary(glm(r_cra~gcse_score*log_income, family=poisson(log)))
# There is an interaction so expecting bias in CRA of -0.04
# (Gkatzionis  et al, 2025, https://doi.org/10.1177/09622802241306860)

#Define binary variables as factors
adr$mated <- as.factor(adr$mated)
adr$r_cra <- as.factor(adr$r_cra)
adr$r_log_income <- as.factor(adr$r_log_income)

summary(adr)

# Check predictors of missingness are as expected
summary(glm(r_cra~mated+ks2_score+gcse_score, family=binomial(logit),
            data=adr))
# As expected

# Add adr dataset to the 'midoc' package
usethis::use_data(adr, overwrite = TRUE)


############################################################################
# 'adr' dataset exploration
library(midoc)

data(adr)
vars<-names(adr)

#descMissData
descMissData(vars[1],vars[2:4],adr)


# Full data estimate (using variables derived in adr.R)
full <- lm(gcse_score~log_income+mated)
c(full$coefficients[2], confint(full)[2,])
# log_income      2.5 %     97.5 %
#  9.718591   8.850992  10.586190

# Interpretation of the assoc with gcse_score in terms of income (rather than
# log_income) is helpful here

# For every unit increase in log_income, there is a 9.72 point increase in GCSE
# score, after adjusting for maternal education level
range(log_income)
# 7.011097 12.121579
hist(log_income)

x<-c(7:12)
plot(x=x, y=c(9.72*(x-8.99)),type='o',pch=18,xlab="Log income",
     ylab="")
title(ylab="Mean difference in GCSE score relative \nto a child with log(family income) of 9",
      mgp=c(2,1,0),cex.lab=0.9)
# On the income scale:
plot(x=exp(x), y=c(9.72*(x-8.99)),type='o',pch=18,xlab="Income (£ pa)",
     ylab="Mean difference in GCSE score relative to a child with family income of ~£8000")
#title(ylab="", mgp=c(2,1,0),cex.lab=0.9)

# CRA - attenuated and less precise
cra <- lm(gcse_score~log_income+mated, data=adr)
summary(cra)
c(cra$coefficients[2], confint(cra)[2,])
# log_income      2.5 %     97.5 %
#  8.029733   6.786605   9.272861

# CRA estimate of marginal association is even more attenuated
fullm <- lm(gcse_score~log_income)
c(fullm$coefficients[2], confint(full)[2,])
#log_income      2.5 %     97.5 %
#12.516766   8.850992  10.586190
cram <- lm(gcse_score~log_income, data=adr)
c(cram$coefficients[2], confint(cra)[2,])
#log_income      2.5 %     97.5 %
#8.855280   6.786605   9.272861

# Define mDAG
adr_mdag <- " log_income -> gcse_score
              log_income -> ks2_score
              mated -> log_income
              mated -> gcse_score
              mated -> ks2_score
              ks2_score -> gcse_score
              mated -> r_cra
              ks2_score -> r_cra"
exploreDAG(adr_mdag,adr)

checkCRA("gcse_score","log_income","r_cra",adr_mdag)
checkCRA("gcse_score","log_income mated ks2_score","r_cra",adr_mdag)

checkMI("log_income","gcse_score mated ks2_score","r_cra", adr_mdag)

logincome_mod_aux <- checkModSpec("log_income~gcse_score+mated+ks2_score","gaussian(identity)",adr)

# Useful to illustrate this with a plot
modfit <- lm(log_income~gcse_score+mated+ks2_score, data=adr)
plot(y=modfit[["residuals"]],x=modfit[["fitted.values"]],xlab="",ylab="",
    main="Residuals versus fitted values")
# Random scatter though clearly bi-modal
modfit_mated0 <- lm(log_income~gcse_score+ks2_score, data=adr, subset=(mated=="0"))
plot(y=modfit_mated0[["residuals"]],x=modfit_mated0[["fitted.values"]],xlab="",ylab="",
     main="Residuals versus fitted values")
modfit_mated1 <- lm(log_income~gcse_score+ks2_score, data=adr, subset=(mated=="1"))
plot(y=modfit_mated1[["residuals"]],x=modfit_mated1[["fitted.values"]],xlab="",ylab="",
     main="Residuals versus fitted values")
#Could be an argument for stratified MI although no of observations with mated==0
# is quite small which could be an argument for a combined model

#Note that if we had imputed income instead, our model would be mis-specified
income_mod_aux <- checkModSpec("I(exp(log_income))~gcse_score+mated+ks2_score","gaussian(identity)",adr)

# Formulate best mice options
propMI_aux <- proposeMI(logincome_mod_aux, adr)
# Note the difference in the distribution between observed and imputed values
# does not imply data are MNAR - may be a consequence of data MAR

doMImice(propMI_aux,123,"lm(gcse_score~log_income+mated)")
# log_income   9.341205 8.201510  10.48090

# And without aux variable
logincome_mod <- checkModSpec("log_income~gcse_score+mated","gaussian(identity)",adr)

propMI <- proposeMI(logincome_mod, adr)

doMImice(propMI,123,"lm(gcse_score~log_income+mated)")
#Unbiased but larger SE
# log_income   9.285316 8.066820  10.50381

