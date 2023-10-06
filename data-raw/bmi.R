## code to prepare `bmi` dataset goes here
set.seed(567433)
# pregsize and U are generated independently, assuming zero correlation with each other,
# such that pregsize ~ Bernoulli(0.1) and U ~ N(0.4,1)
pregsize <- rbinom(1000,1,0.1)
U <- rnorm(1000,0.4,1)
# mated is only caused by U
for (i in 1:1000){
  mated <- rbinom(1000,1,exp(0.5*U[i])/(1+exp(0.5*U[i])))
}
#Specify DGM for matage: matage = mated + ε , where ε ~ N(0,1).
matage <- rnorm(1000,mated,1)
# bmi = α + γ1pregsize + β1matage + γ2mated + β2matage^2 + ε
bmi7 <- rnorm(1000, -0.4 - pregsize + 0.4*matage - 0.8*mated + 0.6*matage^2,1)
# bwt - strongly associated with U and pregsize
bwt <- rnorm(1000, pregsize + U, 1)

#rescale bmi7, bwt, and matage to more realistic values (x scaled so par est interprested as per 1 SD change relative to mean age of 30)
mean(bmi7)
sd(bmi7)
bmi7_sc=((bmi7-0.1)/1.77)*2+20

mean(bwt)
sd(bwt)
bwt_sc=((bwt-0.6)/1.46)*.5 + 3.4

mean(matage)
sd(matage)
matage_sc=(matage - 0.4)/1.1

#Generate missing values of bmi7_sc
bmi7_m1 <- bmi7_sc
for (i in 1:1000){
  bmi7_m1[i] <- ifelse(rbinom(1,1,exp(30*U[i])/(1+exp(30*U[i])))==0, NA, bmi7_sc[i])
}

#Add bmi_full for investigation purposes?
bmi<-data.frame(bmi7=bmi7_m1,matage=matage_sc,mated,pregsize,bwt=bwt_sc)
#Create complete_record indicator - this should be a separate function
bmi$r <- ifelse(apply(bmi,1,anyNA)==F,1,0)
summary(bmi$r)

usethis::use_data(bmi, overwrite = TRUE)
