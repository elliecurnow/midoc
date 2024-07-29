## code to prepare `headache` dataset goes here
set.seed(23290)
# age, sex and migraine are generated independently, assuming zero correlation with each other,
# such that group~Bernoulli(0.5), age ~ N(45,11), hs0 ~ N(25,15), sex ~ Bernoulli(0.8) and migraine ~ Bernoulli(0.8)
group <- rbinom(1000,1,0.5)
age <- rnorm(1000,43,7)
summary(age)
hs0 <- round(rnorm(1000,26,12))
summary(hs0)
#rescale slightly and round
hs0<-round(hs0+12)
summary(hs0)
sex <- rbinom(1000,1,0.8)
migraine <- rbinom(1000,1,0.8)

# hs3 caused by group, linear r'ship with age, hs0, (for simplicity) #not sex and #not migraine
#for (i in 1:1000){
#  hs3 <- rnorm(1000,-5*group+0.5*age-0.01*age^2+hs0,2)
#}
for (i in 1:1000){
  hs3 <- rnorm(1000,-5*group+0.3*age+hs0-10,2)
}
summary(hs3)
hs3[hs3<0]
#set neg values to 0 - only 1 so will not skew dist
hs3<-ifelse(hs3<0,0,round(hs3))
summary(hs3)

# hs12 caused by group, quadratic r'ship with age, hs0, #not sex, #not migraine, and hs3 (no interactions)
for (i in 1:1000){
  hs12 <- rnorm(1000,-6*group-0.9*age+0.1*age^2+0.01*hs0+0.5*hs3-20,10)
}
summary(hs12)
mean(hs12)
sd(hs12)
#rescale
hs12_sc=((hs12-147)/57)*13+34
summary(hs12_sc)
hs12_sc[hs12_sc<0]
#set neg values to 0 - only 5 so will not skew dist
hs12_sc<-ifelse(hs12_sc<0,0,round(hs12_sc))
summary(hs12_sc)

#Generate missing values of hs12 - depending on hs0 and hs3
hs12_m1 <- hs12_sc
for (i in 1:1000){
  hs12_m1[i] <- ifelse(rbinom(1,1,exp(-2*hs0[i]+4*hs3[i]-90)/
                                (1+exp(-2*hs0[i]+4*hs3[i]-90)))==0,
                              NA, hs12_sc[i])
}
summary(hs12_m1)

#Add hs12 for investigation purposes?#not sex, migraine - ,sex=sex,migraine=migraine
headache<-data.frame(group=group,age0=age,hs0=hs0,hs3=hs3,hs12=hs12_m1)
#sort by group
headache<-headache[order(headache$group),]
#Create complete_record indicator and missing hs12 indicator - this should be a separate function
headache$r_hs12 <- ifelse(apply(headache,1,anyNA)==F,1,0)
#headache$r_cra <- ifelse(apply(headache,1,anyNA)==F,1,0)

#Define binary variables
#headache$sex <- as.factor(headache$sex)
#headache$migraine <- as.factor(headache$migraine)
headache$group <- as.factor(headache$group)
headache$r_hs12 <- as.factor(headache$r_hs12)
#headache$r_cra <- as.factor(headache$r_cra)

summary(headache)
summary(glm(as.factor(r_hs12)~group+age0+hs0+hs3, family=binomial(logit), data=headache))
#All as expected

usethis::use_data(headache, overwrite = TRUE)

#Run analyses for workshop results table
#Full data
fit <- lm(hs12_sc~group+hs0+age+I(age^2))
summary(fit)
confint(fit)
#CRA
fit1 <- lm(hs12~group+hs0+age0+I(age0^2),data=headache)
summary(fit1)
confint(fit1)
#MI assuming quad relationship no aux
formulas_list <- as.list(c(as.formula(hs12~group+hs0+age0+I(age0^2))))
#headache$agesq<-headache$age0^2
fit1b <- mice(headache[,c(1:3,5)],method="norm",seed=123,printFlag=FALSE,formulas=formulas_list,m=200)
summary(pool(with(fit1b,lm(hs12~group+hs0+age0+I(age0^2)))),conf.int=TRUE)
#MI assuming quad relationship plus aux
formulas_list <- as.list(c(as.formula(hs12~group+hs0+age0+I(age0^2)+hs3)))
fit1c <- mice(headache[,c(1:5)],method="norm",seed=123,printFlag=FALSE,formulas=formulas_list,m=200)
summary(pool(with(fit1c,lm(hs12~group+hs0+age0+I(age0^2)))),conf.int=TRUE)
#MI assuming linear relationship no aux
fit1d <- mice(headache[,c(1:3,5)],method="norm",seed=123,printFlag=FALSE,m=200)
summary(pool(with(fit1d,lm(hs12~group+hs0+age0+I(age0^2)))),conf.int=TRUE)
#MI assuming linear relationship with aux
fit1e <- mice(headache[,c(1:5)],method="norm",seed=123,printFlag=FALSE,m=200)
summary(pool(with(fit1e,lm(hs12~group+hs0+age0+I(age0^2)))),conf.int=TRUE)
