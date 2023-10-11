## code to prepare `bmi` dataset goes here
set.seed(567433)
# pregsize and U are generated independently, assuming zero correlation with each other,
# such that pregsize ~ Bernoulli(0.1) and U ~ N(0.4,1)
pregsize <- rbinom(1000,1,0.1)
U <- rnorm(1000,0.4,1)
# mated is only caused by U
for (i in 1:1000){
  mated <- rbinom(1000,1,exp(1+2*U[i])/(1+exp(1+2*U[i])))
}
#Specify DGM for matage: matage = mated + ε , where ε ~ N(0,1).
matage <- rnorm(1000,mated,1)
# bmi = α + γ1pregsize + β1matage + γ2mated + β2matage^2 + ε
bmi7 <- rnorm(1000, -0.4 - pregsize + 0.4*matage - 0.8*mated + 0.6*matage^2,1)
# bwt - strongly associated with U and pregsize
bwt <- rnorm(1000, 10*pregsize + 10*U, 1)

#rescale bmi7, bwt, and matage to more realistic values (x scaled so par est interprested as per 1 SD change relative to mean age of 30)
mean(bmi7)
sd(bmi7)
bmi7_sc=((bmi7-0.27)/1.84)*2+18
summary(bmi7_sc)

mean(bwt)
sd(bwt)
bwt_sc=((bwt-5.42)/10.5)*.5 + 3.4
summary(bwt_sc)

mean(matage)
sd(matage)
matage_sc=(matage - 0.44)/1.15

#Generate missing values of bmi7_sc
bmi7_m1 <- bmi7_sc
for (i in 1:1000){
  bmi7_m1[i] <- ifelse(rbinom(1,1,exp(-2+10*U[i])/(1+exp(-2+10*U[i])))==0, NA, bmi7_sc[i])
}
summary(bmi7_m1)

#Add bmi_full for investigation purposes?
bmi<-data.frame(bmi7=bmi7_m1,matage=matage_sc,mated,pregsize,bwt=bwt_sc)
#Create complete_record indicator - this should be a separate function
bmi$r <- ifelse(apply(bmi,1,anyNA)==F,1,0)
summary(bmi$r)

usethis::use_data(bmi, overwrite = TRUE)

#Run analyses for vignette results table - marginal model
#Full data
fit <- lm(bmi7_sc~matage_sc+mated+I(matage_sc^2))
summary(fit)
confint(fit)
#CRA
fit1 <- lm(bmi7~matage+mated+ I(matage^2),data=bmi)
summary(fit1)
confint(fit1)
#MI assuming quad relationship no aux
formulas_list <- as.list(c(as.formula(bmi7 ~ matage + I(matage^2) + mated)))
fit1b <- mice(bmi[,1:3],method="norm",seed=123,printFlag=FALSE,formulas=formulas_list,m=100)
summary(pool(with(fit1b,lm(bmi7 ~ matage + I(matage^2) + mated))),conf.int=TRUE)
#MI assuming linear relationship no aux
fit1c <- mice(bmi[,1:3],method="norm",seed=123,printFlag=FALSE,m=100)
summary(pool(with(fit1c,lm(bmi7 ~ matage + mated + I(matage^2)))),conf.int=TRUE)
#MI assuming quad relationship inc pregsize
formulas_list <- as.list(c(as.formula(bmi7 ~ matage + I(matage^2) + mated + pregsize)))
fit2 <- mice(bmi[,1:4],method="norm",seed=123,printFlag=FALSE,formulas=formulas_list,m=100)
summary(pool(with(fit2,lm(bmi7 ~ matage + I(matage^2) + mated))),conf.int=TRUE)
#MI assuming linear relationship inc pregsize
fit3 <- mice(bmi[,1:4],method="norm",seed=123,printFlag=FALSE,m=100)
summary(pool(with(fit3,lm(bmi7 ~ matage + mated + I(matage^2)))),conf.int=TRUE)
#MI assuming linear relationship inc bwt
fit4 <- mice(bmi[,c(1:3,5)],method="norm",seed=123,printFlag=FALSE,m=50)
summary(pool(with(fit4,lm(bmi7 ~ matage + mated + I(matage^2)))),conf.int=TRUE)
#MI assuming quad relationship inc bwt
formulas_list2 <- as.list(c(as.formula(bmi7 ~ matage + I(matage^2) + mated + bwt)))
fit5 <- mice(bmi[,c(1:3,5)],method="norm",seed=123,printFlag=FALSE,formulas=formulas_list2,m=50)
summary(pool(with(fit5,lm(bmi7 ~ matage + I(matage^2) + mated))),conf.int=TRUE)
