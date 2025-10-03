## Code to prepare `qol` dataset

# We imagine that we want to estimate the effect of a treatment on quality of
# life (QOL) using data from a randomised controlled trial (RCT).

# We assume quality of life (QOL) is measured using the EQ-VAS score, which is
# commonly used (Yan et al. 2025. https://doi.org/10.1016/j.jval.2025.02.001)

# We assume that the joint distribution of age0 (age at baseline), qol0,
# qol3, qol12 (quality of life at baseline, 3 months, 12 months) per group is
# multivariate normal (MVN).

# We could simulate from MVN within each group to allow for unequal variance
# but here we use a common residual variance across groups

# Note one possible sampling strategy would be to directly sample QOL across
# time points from the joint MVN distribution, given a mean vector and var-covar
# matrix, e.g. as per the sample_mvnorm function from the 'rbmi' package:
# https://github.com/insightsengineering/rbmi/blob/main/R/utilities.R
# https://rdrr.io/cran/rbmi/src/R/simulate_data.R

# However, since we want QOL to depend on age at baseline, it is more
# convenient to sample from the associated univariate normal distributions.
# These are generated as follows:

# age0 and qol0 are independent of (treatment) group but qol0 depends on age0,
# such that group ~ Bernoulli(0.5), age0 ~ N(61,9), qol0 has a mean of approx 70

# group was simulated so that pts were approx 50:50 split into Active and
# Placebo arms
# age0 was simulated so that pts were mainly between the ages of approx 55-65
# qol0 was simulated to have a weakly quadratic relationship with age0, with
# coeffs and res variance chosen so that mean & SD of qol0 were realistic values
# (e.g. see Cheng et al. 2024, https://doi.org/10.1016/j.jclinepi.2024.111487)

set.seed(2329465)
#add an ID var
id=c(1:1000)
#sex <- rbinom(1000,1,0.8)
group <- rbinom(1000,1,0.5)
age0 <- rnorm(1000,61,9)
summary(age0)
aggregate(age0, by=list(Group=group), function(x) c(mean = mean(x), sd = sd(x)))

qol0 <- round(rnorm(1000,90+0.3*age0-0.01*age0^2,3))
summary(qol0)
sd(qol0)
plot(x=age0,y=qol0)
aggregate(qol0, by=list(Group=group), function(x) c(mean = mean(x), sd = sd(x)))
summary(lm(qol0~age0+I(age0^2)))
midoc::checkModSpec(qol0~age0,family="gaussian(identity)",
                    data=data.frame(age0,qol0))
#checkModSpec() highlights that model may be incorrect when specified as linear

# Assume qol3 (at 3 months) is caused by group & qol0 (but not directly by age0)
# qol3 (at 3 months) caused by group, and (linearly) by qol0
# Coeffs and res variance chosen so that mean & SD of qol3 were realistic values
# Variance of QOL has been allowed to increase slightly over time
for (i in 1:1000){
  qol3 <- round(rnorm(1000,5*group+0.8*qol0+10,3))
}
summary(qol3)
sd(qol3)
plot(x=age0,y=qol3)
aggregate(cbind(qol0,qol3), by=list(Group=group),
          function(x) c(mean = mean(x), sd = sd(x)))
summary(lm(qol3~age0+I(age0^2)))
# The non-linear relationship is propagated

# Assume qol12 (at 12 months) is caused by group, and linear relationship with
#both qol0 and qol3 (no interactions)
# Coeffs and res variance chosen so that mean & SD of qol12 were realistic and
# (direct) treatment effect was 7 points at 12m
for (i in 1:1000){
  qol12 <- round(rnorm(1000,7*group+0.3*qol0+0.6*qol3+10,3))
}
age0 <- round(age0)
summary(qol12)
sd(qol12)
plot(x=age0,y=qol12)
aggregate(cbind(qol0,qol3,qol12), by=list(Group=group),
          function(x) c(mean = mean(x), sd = sd(x)))
summary(lm(qol12~age0+I(age0^2)))
# The non-linear relationship is propagated

#Estimated (total) treatment effect, adjusted for baseline QOL
summary(lm(qol12~group+qol0))

# Missing mechanism: MAR | qol0, qol3
#Generate missing values of qol12 - depending (linearly) on qol0 and qol3
## Alternative mechanisms which would will also induce an interaction on the
# log-additive scale (and hence selection bias) include: adding a non-linear
# term in qol0 or qol3, or an interaction by group
qol12_m1 <- qol12
for (i in 1:1000){
  qol12_m1[i] <- ifelse(rbinom(1,1,exp(-0.2*qol0[i]+0.5*qol3[i]-18)/
                                (1+exp(-0.2*qol0[i]+0.5*qol3[i]-18)))==0,
                              NA, qol12[i])
}
summary(qol12_m1)
aggregate(cbind(qol0,qol3,qol12,qol12_m1), by=list(Group=group),
          function(x) c(mean = mean(x,na.rm=T), sd = sd(x,na.rm=T),
                        missing=sum(is.na(x))))
#Create data frame
qol<-data.frame(id=id,group=group,age0=age0,qol0=qol0,qol3=qol3,qol12=qol12_m1)

#Create complete_record indicator/missing qol12 indicator
qol$r_qol12 <- ifelse(apply(qol,1,anyNA)==F,1,0)
#qol$r_cra <- ifelse(apply(qol,1,anyNA)==F,1,0)

# Check there is an interaction between group and qol12 in the log-additive
# model for selection under the chosen missingness mechanism
r_qol12 <- qol$r_qol12
summary(glm(r_qol12~group*qol12, family=poisson(log)))
# As expected

#sort by group
qol<-qol[order(qol$group),]

#Define binary variables as factors
qol$group <- as.factor(qol$group)
qol$r_qol12 <- as.factor(qol$r_qol12)
#qol$r_cra <- as.factor(qol$r_cra)

summary(qol)

# Check predictors of missingness are as expected
summary(glm(as.factor(r_qol12)~group+age0+qol0+qol3, family=binomial(logit),
            data=qol))
# As expected

# Add qol dataset to the 'midoc' package
usethis::use_data(qol, overwrite = TRUE)


############################################################################
# RCT 'qol' dataset exploration
library(mice)
library(RefBasedMI)
library(midoc)
library(lattice)

data(qol)
vars<-names(qol)

summary(qol[qol$group==0,])
summary(qol[qol$group==1,])

#descMissData
descMissData(vars[6],vars[2:5],qol)
#by group
descMissData(vars[6],vars[2:5],qol[qol$group==0,])
descMissData(vars[6],vars[2:5],qol[qol$group==1,])
# Greater dropout in the placebo arm

# Table of mean, sd, missingness of outcome by arm, by time
aggregate(qol[,4:6], by=list(Group=qol$group),
                       function(x) c(mean = mean(x,na.rm=T), sd = sd(x,na.rm=T),
                                     missing=sum(is.na(x))))
#Group qol0.mean   qol0.sd qol0.missing qol3.mean   qol3.sd qol3.missing qol12.mean   qol12.sd qol12.missing
#1     0 70.418033  9.176204     0.000000 66.440574  7.901638     0.000000  74.129924   6.666518    172.000000
#2     1 70.955078  8.568358     0.000000 71.757812  7.429245     0.000000  82.591424   6.532383     59.000000
aggregate(qol[,4:6], by=list(Group=qol$group),
          function(x) c(mean = round(mean(x,na.rm=T)),missing=round(sum(is.na(x)))))

# Table of mean outcome by arm, by time, by dropout
summtable <- aggregate(qol[,4:6], by=list(Group=qol$group, Response_12m=qol$r_qol12),
          function(x) mean = mean(x,na.rm=T))
summtable

# Plot of mean outcome by arm, by time, by dropout
summplotdata <- rbind(cbind(summtable[,1:2], mean=summtable[,3], time=0),
                      cbind(summtable[,1:2], mean=summtable[,4], time=3),
                      cbind(summtable[,1:2], mean=summtable[,5], time=12))
summplotdata$plotgroup <- factor(paste(summplotdata$Group, summplotdata$Response_12m),
        labels = c("Placebo dropout", "Placebo", "Active dropout", "Active"))
old.pars <- trellis.par.get()
trellis.par.set(superpose.symbol=list(pch = 18:15),
                superpose.line=list(lty = 4:1, col="black"))
xyplot(mean ~ time, groups=plotgroup, data = summplotdata,
       xlab='Time since randomisation', ylab='Mean EQ-VAS Score',
       ylim=c(40,90), type='o',
       lty=4:1, pch=18:15, col="black",
       auto.key = list(corner=c(0,0)))
trellis.par.set(old.pars)

# Full data estimate (using variables derived in qol.R)
full <- lm(qol12~group+qol0)
c(full$coefficients[2], confint(full)[2,])
#    group     2.5 %    97.5 %
#  9.993481  9.548494 10.438468
# This is in line with published clinically important difference for EQ-VAS

# CRA - slightly attenuated
cra <- lm(qol12~group+qol0+age0, data=qol)
c(cra$coefficients[2], confint(cra)[2,])
#  group1    2.5 %   97.5 %
#  9.532509  9.024534 10.040483

#MI using mice - assuming MAR given age at baseline, QOL at baseline (and 3 months)
names(qol)
#qol0
mi_mar_qol0 <- mice(qol[,c(2:4,6)],method="norm",seed=123,printFlag=FALSE,m=200)
summary(pool(with(mi_mar_qol0,lm(qol12~group+qol0))),conf.int=TRUE)[2,c(1:2,7:8)]
#     term estimate    2.5 %   97.5 %
#    group1  9.37072 8.851583 9.889857

#mi_mar_qol3 <- mice(qol[,c(1,4,5)],method="norm",seed=123,printFlag=FALSE,m=200)
#summary(pool(with(mi_mar_qol3,lm(qol12~group))),conf.int=TRUE)[2,c(1:2,7:8)]

#qol0 and qol3 - recovers full data estiate as expected
mi_mar_qol03 <- mice(qol[,c(2,4:6)],method="norm",seed=123,printFlag=FALSE,m=200)
summary(pool(with(mi_mar_qol03,lm(qol12~group+qol0))),conf.int=TRUE)[2,c(1:2,7:8)]
#   term estimate    2.5 %   97.5 %
# group1 9.991963 9.476504 10.50742

#MI using mice - assuming MNAR given QOL at baseline
#Using NARFCS - this approach over-estimates the treatment effect
# With delta=-5
mi_mnar_qol0_5 <- mice(qol[,c(2,4,6)],method="mnar.norm",
                       blots=list(qol12=list(ums="-5")),seed=123,printFlag=FALSE,m=200)
summary(pool(with(mi_mnar_qol0_5,lm(qol12~group+qol0))),conf.int=TRUE)[2,c(1:2,7:8)]
#    term estimate   2.5 %   97.5 %
# group1 10.86244 10.2893 11.43558

# With delta=-10
mi_mnar_qol0_10 <- mice(qol[,c(2,4,6)],method="mnar.norm",
                        blots=list(qol12=list(ums="-10")),seed=123,printFlag=FALSE,m=200)
summary(pool(with(mi_mnar_qol0_10,lm(qol12~group+qol0))),conf.int=TRUE)[2,c(1:2,7:8)]
#     term estimate    2.5 %   97.5 %
# group1 12.35416 11.64463 13.06368

#Delta differentially by group
mi_mnar_qol0_5group <- mice(qol[,c(2,4,6)],method="mnar.norm",
                            blots=list(qol12=list(ums="5*group1-10")),seed=123,printFlag=FALSE,m=200)
summary(pool(with(mi_mnar_qol0_5group,lm(qol12~group+qol0))),conf.int=TRUE)[2,c(1:2,7:8)]
#    term estimate    2.5 %   97.5 %
#  group1 12.87647 12.20102 13.55191

#RefBasedMI - assuming MNAR given QOL at baseline
#First need to reorganise data in 'long' format
qol_long <- rbind(cbind(qol[,c(1:2,4)],qol=qol[,5],time=3),
                  cbind(qol[,c(1:2,4)],qol=qol[,6],time=12))
#sort by id
qol_long<-qol_long[order(qol_long$id),]

#change group to numeric
qol_long$group <- as.numeric(qol_long$group)

#Can allow method to vary by pt using methodvar and/or referencevar

#J2R (ref=1)
mi_ref_j2r <- RefBasedMI(data=qol_long, depvar=qol, covar=qol0, treatvar=group, idvar=id,
                         timevar=time, method="J2R", reference=1, M=200,
                         seed=123)
summary(pool(with(as.mids(mi_ref_j2r),lm(qol~factor(group)+qol0, subset=(time==12)))),
        conf.int=TRUE)[2,c(1:2,7:8)]
#           term estimate    2.5 %   97.5 %
# factor(group)2 8.948895 8.343216 9.554574

#CR
mi_ref_cr <- RefBasedMI(data=qol_long, depvar=qol, covar=qol0, treatvar=group, idvar=id,
                        timevar=time, method="CR", reference=1, M=200,
                        seed=123)
summary(pool(with(as.mids(mi_ref_cr),lm(qol~factor(group)+qol0, subset=(time==12)))),
        conf.int=TRUE)[2,c(1:2,7:8)]
#           term estimate    2.5 %   97.5 %
# factor(group)2 9.269636 8.685876 9.853395

#CIR - closer to full data estimate
mi_ref_cir <- RefBasedMI(data=qol_long, covar=qol0, depvar=qol, treatvar=group, idvar=id,
                         timevar=time, method="CIR", reference=1, M=200,
                         seed=123)
summary(pool(with(as.mids(mi_ref_cir),lm(qol~factor(group)+qol0, subset=(time==12)))),
        conf.int=TRUE)[2,c(1:2,7:8)]
#            term estimate   2.5 %   97.5 %
# factor(group)2  9.48417 8.94166 10.02668

#MAR - check that this gives similar results to mice() under MAR with group, qol0 and qol3 as predictors
# It does, bar some sampling variation
mi_ref_mar <- RefBasedMI(data=qol_long, depvar=qol, covar=qol0, treatvar=group, idvar=id,
                         timevar=time, method="MAR", M=200, seed=123)
summary(pool(with(as.mids(mi_ref_mar),lm(qol~factor(group)+qol0, subset=(time==12)))),
        conf.int=TRUE)[2,c(1:2,7:8)]
#           term estimate    2.5 %   97.5 %
# factor(group)2 10.06438 9.519927 10.60884

#MAR-MI inc age assuming quad relationship
formulas_list <- as.list(c(as.formula(qol12~group+qol0+age0+I(age0^2)+qol3)))
mi_mar_quadage <- mice(qol[,c(2:6)],method="norm",seed=123,printFlag=FALSE,formulas=formulas_list,m=200)
summary(pool(with(mi_mar_quadage,lm(qol12~group+qol0))),conf.int=TRUE)[2,c(1:2,7:8)]
#    term estimate    2.5 %   97.5 %
# group1 10.00972 9.503554 10.51588

#MAR-MI inc age assuming linear relationship
mi_mar_linage  <- mice(qol[,c(2:6)],method="norm",seed=123,printFlag=FALSE,m=200)
summary(pool(with(mi_mar_linage,lm(qol12~group))),conf.int=TRUE)[2,c(1:2,7:8)]
#    term estimate    2.5 %   97.5 %
# group1 10.26234 9.555648 10.96904

#CI is wider and estimate is slightly over-estimated compared with model with quad age term

