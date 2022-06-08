# Calculations with sleep score

library(dplyr)

Sys.setenv(LANG = "en")

# prediscore<-read.csv("P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/fatigclass4_11012022.csv", stringsAsFactors = TRUE)
prediscore<-read.csv("P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/prediscoreclass.csv", stringsAsFactors = TRUE)


dim(prediscore)
str(prediscore)
head(prediscore)
table(prediscore$.imp)


# ref https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/

library(nnet)

mean(prediscore$age_estimateyears)
sd(prediscore$age_estimateyears)
mean(prediscore$age_estimateyears[prediscore$class==1])
sd(prediscore$age_estimateyears[prediscore$class==1])
mean(prediscore$age_estimateyears[prediscore$class==2])
sd(prediscore$age_estimateyears[prediscore$class==2])
mean(prediscore$age_estimateyears[prediscore$class==3])
sd(prediscore$age_estimateyears[prediscore$class==3])


# Compute the analysis of variance
res.aov <- aov(age_estimateyears ~ class, data = prediscore)
# Summary of the analysis
summary(res.aov)


quantile(prediscore$age_estimateyears, probs = c(0.25,0.5,0.75))
prediscore$older<-ifelse(prediscore$age_estimateyears<=49, "younger 49","older50")
prediscore$older<-factor(prediscore$older)
prediscore$older<-relevel(prediscore$older,"younger 49")

prediscore$younger<-ifelse(prediscore$age_estimateyears<=30, "younger 30","older30")
prediscore$younger<-factor(prediscore$younger)
prediscore$younger<-relevel(prediscore$younger,"older30")

table(prediscore$class)


prediscore$class_f<-NA
prediscore$class_f<-factor(prediscore$class,levels = c(1,2,3,4), labels = c("c1","c2","c3","c4"))
prediscore$class_f<- relevel(prediscore$class_f, ref = "c1")
table(prediscore$class_f,useNA = "ifany")

prediscore0<-prediscore%>%
  filter(JOURn==0)
table(prediscore0$class_f,useNA = "ifany")

# Sex
table(prediscore$sexm,useNA = "ifany")

prediscore$sex<-relevel(prediscore$sex, "male")

modelsex<-multinom(class_f~sex, data = prediscore)
summary(modelsex)

z <- summary(modelsex)$coefficients/summary(modelsex)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

(est.c2.sex<-summary(modelsex)$coefficients[4])
(est.c3.sex<-summary(modelsex)$coefficients[5])
(est.c4.sex<-summary(modelsex)$coefficients[6])

(se.c2.sex<-summary(modelsex)$standard.errors[4])
(se.c3.sex<-summary(modelsex)$standard.errors[5])
(se.c4.sex<-summary(modelsex)$standard.errors[6])

(lci.c2.sex<-est.c2.sex-1.95*se.c2.sex)
(lci.c3.sex<-est.c3.sex-1.95*se.c3.sex)
(lci.c4.sex<-est.c4.sex-1.95*se.c4.sex)

(uci.c2.sex<-est.c2.sex+1.95*se.c2.sex)
(uci.c3.sex<-est.c3.sex+1.95*se.c3.sex)
(uci.c4.sex<-est.c4.sex+1.95*se.c4.sex)

(coef.c2.sex<-exp(est.c2.sex))
(clci.c2.sex<-exp(lci.c2.sex))
(cuci.c2.sex<-exp(uci.c2.sex))

(coef.c3.sex<-exp(est.c3.sex))
(clci.c3.sex<-exp(lci.c3.sex))
(cuci.c3.sex<-exp(uci.c3.sex))

(coef.c4.sex<-exp(est.c4.sex))
(clci.c4.sex<-exp(lci.c4.sex))
(cuci.c4.sex<-exp(uci.c4.sex))




x<-c("female",coef.c2.sex,clci.c2.sex,cuci.c2.sex,
     coef.c3.sex,clci.c3.sex,cuci.c3.sex,
     coef.c4.sex,clci.c4.sex,cuci.c4.sex)
sex<-data.frame(x)
sext<-t(sex)
sext<-data.frame(sext)
names(sext)<-c("var","or_c2","lci_c2","uci_c2",
               "or_c3","lci_c3","uci_c3",
               "or_c4","lci_c4","uci_c4")

# age
table(prediscore$age_estimateyears,useNA = "ifany")

modelage<-multinom(class_f~age_estimateyears, data = prediscore)
summary(modelage)

(est.c2.age<-summary(modelage)$coefficients[4])
(est.c3.age<-summary(modelage)$coefficients[5])
(est.c4.age<-summary(modelage)$coefficients[6])

(se.c2.age<-summary(modelage)$standard.errors[4])
(se.c3.age<-summary(modelage)$standard.errors[5])
(se.c4.age<-summary(modelage)$standard.errors[6])

(lci.c2.age<-est.c2.age-1.95*se.c2.age)
(lci.c3.age<-est.c3.age-1.95*se.c3.age)
(lci.c4.age<-est.c4.age-1.95*se.c4.age)

(uci.c2.age<-est.c2.age+1.95*se.c2.age)
(uci.c3.age<-est.c3.age+1.95*se.c3.age)
(uci.c4.age<-est.c4.age+1.95*se.c4.age)

(coef.c2.age<-exp(est.c2.age))
(clci.c2.age<-exp(lci.c2.age))
(cuci.c2.age<-exp(uci.c2.age))

(coef.c3.age<-exp(est.c3.age))
(clci.c3.age<-exp(lci.c3.age))
(cuci.c3.age<-exp(uci.c3.age))

(coef.c4.age<-exp(est.c4.age))
(clci.c4.age<-exp(lci.c4.age))
(cuci.c4.age<-exp(uci.c4.age))




x<-c("age",coef.c2.age,clci.c2.age,cuci.c2.age,
     coef.c3.age,clci.c3.age,cuci.c3.age,
     coef.c4.age,clci.c4.age,cuci.c4.age)
age<-data.frame(x)
aget<-t(age)
aget<-data.frame(aget)
names(aget)<-c("var","or_c2","lci_c2","uci_c2",
                 "or_c3","lci_c3","uci_c3",
                 "or_c4","lci_c4","uci_c4")


# younger
table(prediscore$ageer,useNA = "ifany")

modelyoung<-multinom(class_f~younger, data = prediscore)
summary(modelyoung)

(est.c2.young<-summary(modelyoung)$coefficients[4])
(est.c3.young<-summary(modelyoung)$coefficients[5])
(est.c4.young<-summary(modelyoung)$coefficients[6])

(se.c2.young<-summary(modelyoung)$standard.errors[4])
(se.c3.young<-summary(modelyoung)$standard.errors[5])
(se.c4.young<-summary(modelyoung)$standard.errors[6])

(lci.c2.young<-est.c2.young-1.95*se.c2.young)
(lci.c3.young<-est.c3.young-1.95*se.c3.young)
(lci.c4.young<-est.c4.young-1.95*se.c4.young)

(uci.c2.young<-est.c2.young+1.95*se.c2.young)
(uci.c3.young<-est.c3.young+1.95*se.c3.young)
(uci.c4.young<-est.c4.young+1.95*se.c4.young)

(coef.c2.young<-exp(est.c2.young))
(clci.c2.young<-exp(lci.c2.young))
(cuci.c2.young<-exp(uci.c2.young))

(coef.c3.young<-exp(est.c3.young))
(clci.c3.young<-exp(lci.c3.young))
(cuci.c3.young<-exp(uci.c3.young))

(coef.c4.young<-exp(est.c4.young))
(clci.c4.young<-exp(lci.c4.young))
(cuci.c4.young<-exp(uci.c4.young))




x<-c("young",coef.c2.young,clci.c2.young,cuci.c2.young,
     coef.c3.young,clci.c3.young,cuci.c3.young,
     coef.c4.young,clci.c4.young,cuci.c4.young)
young<-data.frame(x)
youngt<-t(young)
youngt<-data.frame(youngt)
names(youngt)<-c("var","or_c2","lci_c2","uci_c2",
               "or_c3","lci_c3","uci_c3",
               "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt)



# Older
table(prediscore$older,useNA = "ifany")
modelold<-multinom(class_f~older, data = prediscore)

summary(modelold)

(est.c2.old<-summary(modelold)$coefficients[4])
(est.c3.old<-summary(modelold)$coefficients[5])
(est.c4.old<-summary(modelold)$coefficients[6])

(se.c2.old<-summary(modelold)$standard.errors[4])
(se.c3.old<-summary(modelold)$standard.errors[5])
(se.c4.old<-summary(modelold)$standard.errors[6])

(lci.c2.old<-est.c2.old-1.95*se.c2.old)
(lci.c3.old<-est.c3.old-1.95*se.c3.old)
(lci.c4.old<-est.c4.old-1.95*se.c4.old)

(uci.c2.old<-est.c2.old+1.95*se.c2.old)
(uci.c3.old<-est.c3.old+1.95*se.c3.old)
(uci.c4.old<-est.c4.old+1.95*se.c4.old)

(coef.c2.old<-exp(est.c2.old))
(clci.c2.old<-exp(lci.c2.old))
(cuci.c2.old<-exp(uci.c2.old))

(coef.c3.old<-exp(est.c3.old))
(clci.c3.old<-exp(lci.c3.old))
(cuci.c3.old<-exp(uci.c3.old))

(coef.c4.old<-exp(est.c4.old))
(clci.c4.old<-exp(lci.c4.old))
(cuci.c4.old<-exp(uci.c4.old))




x<-c("old",coef.c2.old,clci.c2.old,cuci.c2.old,
     coef.c3.old,clci.c3.old,cuci.c3.old,
     coef.c4.old,clci.c4.old,cuci.c4.old)
old<-data.frame(x)
oldt<-t(old)
oldt<-data.frame(oldt)
names(oldt)<-c("var","or_c2","lci_c2","uci_c2",
                 "or_c3","lci_c3","uci_c3",
                 "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt)

# Education loweduc

table(prediscore$loweduc,useNA = "ifany")

prediscore$loweduc<-relevel(prediscore$loweduc,"superior")

modelloweduc<-multinom(class_f~loweduc, data = prediscore)
prediscore$loweduc<-relevel(prediscore$loweduc,"superior")

summary(modelloweduc)

(est.c2.loweduc<-summary(modelloweduc)$coefficients[4])
(est.c3.loweduc<-summary(modelloweduc)$coefficients[5])
(est.c4.loweduc<-summary(modelloweduc)$coefficients[6])

(se.c2.loweduc<-summary(modelloweduc)$standard.errors[4])
(se.c3.loweduc<-summary(modelloweduc)$standard.errors[5])
(se.c4.loweduc<-summary(modelloweduc)$standard.errors[6])

(lci.c2.loweduc<-est.c2.loweduc-1.95*se.c2.loweduc)
(lci.c3.loweduc<-est.c3.loweduc-1.95*se.c3.loweduc)
(lci.c4.loweduc<-est.c4.loweduc-1.95*se.c4.loweduc)

(uci.c2.loweduc<-est.c2.loweduc+1.95*se.c2.loweduc)
(uci.c3.loweduc<-est.c3.loweduc+1.95*se.c3.loweduc)
(uci.c4.loweduc<-est.c4.loweduc+1.95*se.c4.loweduc)

(coef.c2.loweduc<-exp(est.c2.loweduc))
(clci.c2.loweduc<-exp(lci.c2.loweduc))
(cuci.c2.loweduc<-exp(uci.c2.loweduc))

(coef.c3.loweduc<-exp(est.c3.loweduc))
(clci.c3.loweduc<-exp(lci.c3.loweduc))
(cuci.c3.loweduc<-exp(uci.c3.loweduc))

(coef.c4.loweduc<-exp(est.c4.loweduc))
(clci.c4.loweduc<-exp(lci.c4.loweduc))
(cuci.c4.loweduc<-exp(uci.c4.loweduc))




x<-c("loweduc",coef.c2.loweduc,clci.c2.loweduc,cuci.c2.loweduc,
     coef.c3.loweduc,clci.c3.loweduc,cuci.c3.loweduc,
     coef.c4.loweduc,clci.c4.loweduc,cuci.c4.loweduc)
loweduc<-data.frame(x)
loweduct<-t(loweduc)
loweduct<-data.frame(loweduct)
names(loweduct)<-c("var","or_c2","lci_c2","uci_c2",
               "or_c3","lci_c3","uci_c3",
               "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct)

# income
modelincome<-multinom(class_f~income, data = prediscore)

summary(modelincome)

(est.c2.income<-summary(modelincome)$coefficients[4])
(est.c3.income<-summary(modelincome)$coefficients[5])
(est.c4.income<-summary(modelincome)$coefficients[6])

(se.c2.income<-summary(modelincome)$standard.errors[4])
(se.c3.income<-summary(modelincome)$standard.errors[5])
(se.c4.income<-summary(modelincome)$standard.errors[6])

(lci.c2.income<-est.c2.income-1.95*se.c2.income)
(lci.c3.income<-est.c3.income-1.95*se.c3.income)
(lci.c4.income<-est.c4.income-1.95*se.c4.income)

(uci.c2.income<-est.c2.income+1.95*se.c2.income)
(uci.c3.income<-est.c3.income+1.95*se.c3.income)
(uci.c4.income<-est.c4.income+1.95*se.c4.income)

(coef.c2.income<-exp(est.c2.income))
(clci.c2.income<-exp(lci.c2.income))
(cuci.c2.income<-exp(uci.c2.income))

(coef.c3.income<-exp(est.c3.income))
(clci.c3.income<-exp(lci.c3.income))
(cuci.c3.income<-exp(uci.c3.income))

(coef.c4.income<-exp(est.c4.income))
(clci.c4.income<-exp(lci.c4.income))
(cuci.c4.income<-exp(uci.c4.income))




x<-c("income",coef.c2.income,clci.c2.income,cuci.c2.income,
     coef.c3.income,clci.c3.income,cuci.c3.income,
     coef.c4.income,clci.c4.income,cuci.c4.income)
income<-data.frame(x)
incomet<-t(income)
incomet<-data.frame(incomet)
names(incomet)<-c("var","or_c2","lci_c2","uci_c2",
                     "or_c3","lci_c3","uci_c3",
                     "or_c4","lci_c4","uci_c4")

# Low income lowincome

median(prediscore$income, na.rm=TRUE)
lowquant<-quantile(prediscore$income, probs=c(0,0.25,0.5,0.75,1),na.rm=TRUE)[[2]]
prediscore$lowincome<-ifelse(prediscore$income<lowquant,"low","high")
prediscore$lowincome<-factor(prediscore$lowincome)
table( prediscore$lowincome, useNA = "ifany")



table(prediscore$lowincome,useNA = "ifany")

modellowincome<-multinom(class_f~lowincome, data = prediscore)

summary(modellowincome)

(est.c2.lowincome<-summary(modellowincome)$coefficients[4])
(est.c3.lowincome<-summary(modellowincome)$coefficients[5])
(est.c4.lowincome<-summary(modellowincome)$coefficients[6])

(se.c2.lowincome<-summary(modellowincome)$standard.errors[4])
(se.c3.lowincome<-summary(modellowincome)$standard.errors[5])
(se.c4.lowincome<-summary(modellowincome)$standard.errors[6])

(lci.c2.lowincome<-est.c2.lowincome-1.95*se.c2.lowincome)
(lci.c3.lowincome<-est.c3.lowincome-1.95*se.c3.lowincome)
(lci.c4.lowincome<-est.c4.lowincome-1.95*se.c4.lowincome)

(uci.c2.lowincome<-est.c2.lowincome+1.95*se.c2.lowincome)
(uci.c3.lowincome<-est.c3.lowincome+1.95*se.c3.lowincome)
(uci.c4.lowincome<-est.c4.lowincome+1.95*se.c4.lowincome)

(coef.c2.lowincome<-exp(est.c2.lowincome))
(clci.c2.lowincome<-exp(lci.c2.lowincome))
(cuci.c2.lowincome<-exp(uci.c2.lowincome))

(coef.c3.lowincome<-exp(est.c3.lowincome))
(clci.c3.lowincome<-exp(lci.c3.lowincome))
(cuci.c3.lowincome<-exp(uci.c3.lowincome))

(coef.c4.lowincome<-exp(est.c4.lowincome))
(clci.c4.lowincome<-exp(lci.c4.lowincome))
(cuci.c4.lowincome<-exp(uci.c4.lowincome))




x<-c("lowincome",coef.c2.lowincome,clci.c2.lowincome,cuci.c2.lowincome,
     coef.c3.lowincome,clci.c3.lowincome,cuci.c3.lowincome,
     coef.c4.lowincome,clci.c4.lowincome,cuci.c4.lowincome)
lowincome<-data.frame(x)
lowincomet<-t(lowincome)
lowincomet<-data.frame(lowincomet)
names(lowincomet)<-c("var","or_c2","lci_c2","uci_c2",
                   "or_c3","lci_c3","uci_c3",
                   "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct,lowincomet)


# Unemployed work
table(prediscore$work,useNA = "ifany")

prediscore$work<-relevel(prediscore$work,"work")

modelwork<-multinom(class_f~work, data = prediscore)

summary(modelwork)

(est.c2.work<-summary(modelwork)$coefficients[4])
(est.c3.work<-summary(modelwork)$coefficients[5])
(est.c4.work<-summary(modelwork)$coefficients[6])

(se.c2.work<-summary(modelwork)$standard.errors[4])
(se.c3.work<-summary(modelwork)$standard.errors[5])
(se.c4.work<-summary(modelwork)$standard.errors[6])

(lci.c2.work<-est.c2.work-1.95*se.c2.work)
(lci.c3.work<-est.c3.work-1.95*se.c3.work)
(lci.c4.work<-est.c4.work-1.95*se.c4.work)

(uci.c2.work<-est.c2.work+1.95*se.c2.work)
(uci.c3.work<-est.c3.work+1.95*se.c3.work)
(uci.c4.work<-est.c4.work+1.95*se.c4.work)

(coef.c2.work<-exp(est.c2.work))
(clci.c2.work<-exp(lci.c2.work))
(cuci.c2.work<-exp(uci.c2.work))

(coef.c3.work<-exp(est.c3.work))
(clci.c3.work<-exp(lci.c3.work))
(cuci.c3.work<-exp(uci.c3.work))

(coef.c4.work<-exp(est.c4.work))
(clci.c4.work<-exp(lci.c4.work))
(cuci.c4.work<-exp(uci.c4.work))




x<-c("work",coef.c2.work,clci.c2.work,cuci.c2.work,
     coef.c3.work,clci.c3.work,cuci.c3.work,
     coef.c4.work,clci.c4.work,cuci.c4.work)
work<-data.frame(x)
workt<-t(work)
workt<-data.frame(workt)
names(workt)<-c("var","or_c2","lci_c2","uci_c2",
                     "or_c3","lci_c3","uci_c3",
                     "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt) 


# Owner
table(prediscore$owner,useNA = "ifany")
prediscore$owner<-relevel(prediscore$owner,"yes")

modelowner<-multinom(class_f~owner, data = prediscore)

summary(modelowner)

(est.c2.owner<-summary(modelowner)$coefficients[4])
(est.c3.owner<-summary(modelowner)$coefficients[5])
(est.c4.owner<-summary(modelowner)$coefficients[6])

(se.c2.owner<-summary(modelowner)$standard.errors[4])
(se.c3.owner<-summary(modelowner)$standard.errors[5])
(se.c4.owner<-summary(modelowner)$standard.errors[6])

(lci.c2.owner<-est.c2.owner-1.95*se.c2.owner)
(lci.c3.owner<-est.c3.owner-1.95*se.c3.owner)
(lci.c4.owner<-est.c4.owner-1.95*se.c4.owner)

(uci.c2.owner<-est.c2.owner+1.95*se.c2.owner)
(uci.c3.owner<-est.c3.owner+1.95*se.c3.owner)
(uci.c4.owner<-est.c4.owner+1.95*se.c4.owner)

(coef.c2.owner<-exp(est.c2.owner))
(clci.c2.owner<-exp(lci.c2.owner))
(cuci.c2.owner<-exp(uci.c2.owner))

(coef.c3.owner<-exp(est.c3.owner))
(clci.c3.owner<-exp(lci.c3.owner))
(cuci.c3.owner<-exp(uci.c3.owner))

(coef.c4.owner<-exp(est.c4.owner))
(clci.c4.owner<-exp(lci.c4.owner))
(cuci.c4.owner<-exp(uci.c4.owner))




x<-c("owner",coef.c2.owner,clci.c2.owner,cuci.c2.owner,
     coef.c3.owner,clci.c3.owner,cuci.c3.owner,
     coef.c4.owner,clci.c4.owner,cuci.c4.owner)
owner<-data.frame(x)
ownert<-t(owner)
ownert<-data.frame(ownert)
names(ownert)<-c("var","or_c2","lci_c2","uci_c2",
                "or_c3","lci_c3","uci_c3",
                "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt, ownert) 


# bmi

table(prediscore$bmi,useNA = "ifany")
modelbmi<-multinom(class_f~bmi, data = prediscore)

summary(modelbmi)

(est.c2.bmi<-summary(modelbmi)$coefficients[4])
(est.c3.bmi<-summary(modelbmi)$coefficients[5])
(est.c4.bmi<-summary(modelbmi)$coefficients[6])

(se.c2.bmi<-summary(modelbmi)$standard.errors[4])
(se.c3.bmi<-summary(modelbmi)$standard.errors[5])
(se.c4.bmi<-summary(modelbmi)$standard.errors[6])

(lci.c2.bmi<-est.c2.bmi-1.95*se.c2.bmi)
(lci.c3.bmi<-est.c3.bmi-1.95*se.c3.bmi)
(lci.c4.bmi<-est.c4.bmi-1.95*se.c4.bmi)

(uci.c2.bmi<-est.c2.bmi+1.95*se.c2.bmi)
(uci.c3.bmi<-est.c3.bmi+1.95*se.c3.bmi)
(uci.c4.bmi<-est.c4.bmi+1.95*se.c4.bmi)

(coef.c2.bmi<-exp(est.c2.bmi))
(clci.c2.bmi<-exp(lci.c2.bmi))
(cuci.c2.bmi<-exp(uci.c2.bmi))

(coef.c3.bmi<-exp(est.c3.bmi))
(clci.c3.bmi<-exp(lci.c3.bmi))
(cuci.c3.bmi<-exp(uci.c3.bmi))

(coef.c4.bmi<-exp(est.c4.bmi))
(clci.c4.bmi<-exp(lci.c4.bmi))
(cuci.c4.bmi<-exp(uci.c4.bmi))




x<-c("bmi",coef.c2.bmi,clci.c2.bmi,cuci.c2.bmi,
     coef.c3.bmi,clci.c3.bmi,cuci.c3.bmi,
     coef.c4.bmi,clci.c4.bmi,cuci.c4.bmi)
bmi<-data.frame(x)
bmit<-t(bmi)
bmit<-data.frame(bmit)
names(bmit)<-c("var","or_c2","lci_c2","uci_c2",
                 "or_c3","lci_c3","uci_c3",
                 "or_c4","lci_c4","uci_c4")


# obese

table(prediscore$obese,useNA = "ifany")
modelobese<-multinom(class_f~obese, data = prediscore)

summary(modelobese)

(est.c2.obese<-summary(modelobese)$coefficients[4])
(est.c3.obese<-summary(modelobese)$coefficients[5])
(est.c4.obese<-summary(modelobese)$coefficients[6])

(se.c2.obese<-summary(modelobese)$standard.errors[4])
(se.c3.obese<-summary(modelobese)$standard.errors[5])
(se.c4.obese<-summary(modelobese)$standard.errors[6])

(lci.c2.obese<-est.c2.obese-1.95*se.c2.obese)
(lci.c3.obese<-est.c3.obese-1.95*se.c3.obese)
(lci.c4.obese<-est.c4.obese-1.95*se.c4.obese)

(uci.c2.obese<-est.c2.obese+1.95*se.c2.obese)
(uci.c3.obese<-est.c3.obese+1.95*se.c3.obese)
(uci.c4.obese<-est.c4.obese+1.95*se.c4.obese)

(coef.c2.obese<-exp(est.c2.obese))
(clci.c2.obese<-exp(lci.c2.obese))
(cuci.c2.obese<-exp(uci.c2.obese))

(coef.c3.obese<-exp(est.c3.obese))
(clci.c3.obese<-exp(lci.c3.obese))
(cuci.c3.obese<-exp(uci.c3.obese))

(coef.c4.obese<-exp(est.c4.obese))
(clci.c4.obese<-exp(lci.c4.obese))
(cuci.c4.obese<-exp(uci.c4.obese))




x<-c("obese",coef.c2.obese,clci.c2.obese,cuci.c2.obese,
     coef.c3.obese,clci.c3.obese,cuci.c3.obese,
     coef.c4.obese,clci.c4.obese,cuci.c4.obese)
obese<-data.frame(x)
obeset<-t(obese)
obeset<-data.frame(obeset)
names(obeset)<-c("var","or_c2","lci_c2","uci_c2",
                 "or_c3","lci_c3","uci_c3",
                 "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt, ownert,obeset) 




# Underweight
prediscore$underweight<-ifelse(prediscore$bmi<20,"underw","normalw")

table(prediscore$underweight,useNA = "ifany")
modelunderweight<-multinom(class_f~underweight, data = prediscore)

summary(modelunderweight)

(est.c2.underweight<-summary(modelunderweight)$coefficients[4])
(est.c3.underweight<-summary(modelunderweight)$coefficients[5])
(est.c4.underweight<-summary(modelunderweight)$coefficients[6])

(se.c2.underweight<-summary(modelunderweight)$standard.errors[4])
(se.c3.underweight<-summary(modelunderweight)$standard.errors[5])
(se.c4.underweight<-summary(modelunderweight)$standard.errors[6])

(lci.c2.underweight<-est.c2.underweight-1.95*se.c2.underweight)
(lci.c3.underweight<-est.c3.underweight-1.95*se.c3.underweight)
(lci.c4.underweight<-est.c4.underweight-1.95*se.c4.underweight)

(uci.c2.underweight<-est.c2.underweight+1.95*se.c2.underweight)
(uci.c3.underweight<-est.c3.underweight+1.95*se.c3.underweight)
(uci.c4.underweight<-est.c4.underweight+1.95*se.c4.underweight)

(coef.c2.underweight<-exp(est.c2.underweight))
(clci.c2.underweight<-exp(lci.c2.underweight))
(cuci.c2.underweight<-exp(uci.c2.underweight))

(coef.c3.underweight<-exp(est.c3.underweight))
(clci.c3.underweight<-exp(lci.c3.underweight))
(cuci.c3.underweight<-exp(uci.c3.underweight))

(coef.c4.underweight<-exp(est.c4.underweight))
(clci.c4.underweight<-exp(lci.c4.underweight))
(cuci.c4.underweight<-exp(uci.c4.underweight))




x<-c("underweight",coef.c2.underweight,clci.c2.underweight,cuci.c2.underweight,
     coef.c3.underweight,clci.c3.underweight,cuci.c3.underweight,
     coef.c4.underweight,clci.c4.underweight,cuci.c4.underweight)
underweight<-data.frame(x)
underweightt<-t(underweight)
underweightt<-data.frame(underweightt)
names(underweightt)<-c("var","or_c2","lci_c2","uci_c2",
                 "or_c3","lci_c3","uci_c3",
                 "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt, ownert,obeset,underweightt) 



# Physical activity

modelsum_all_activities<-multinom(class_f~sum_all_activities, data = prediscore)

summary(modelsum_all_activities)

(est.c2.sum_all_activities<-summary(modelsum_all_activities)$coefficients[4])
(est.c3.sum_all_activities<-summary(modelsum_all_activities)$coefficients[5])
(est.c4.sum_all_activities<-summary(modelsum_all_activities)$coefficients[6])

(se.c2.sum_all_activities<-summary(modelsum_all_activities)$standard.errors[4])
(se.c3.sum_all_activities<-summary(modelsum_all_activities)$standard.errors[5])
(se.c4.sum_all_activities<-summary(modelsum_all_activities)$standard.errors[6])

(lci.c2.sum_all_activities<-est.c2.sum_all_activities-1.95*se.c2.sum_all_activities)
(lci.c3.sum_all_activities<-est.c3.sum_all_activities-1.95*se.c3.sum_all_activities)
(lci.c4.sum_all_activities<-est.c4.sum_all_activities-1.95*se.c4.sum_all_activities)

(uci.c2.sum_all_activities<-est.c2.sum_all_activities+1.95*se.c2.sum_all_activities)
(uci.c3.sum_all_activities<-est.c3.sum_all_activities+1.95*se.c3.sum_all_activities)
(uci.c4.sum_all_activities<-est.c4.sum_all_activities+1.95*se.c4.sum_all_activities)

(coef.c2.sum_all_activities<-exp(est.c2.sum_all_activities))
(clci.c2.sum_all_activities<-exp(lci.c2.sum_all_activities))
(cuci.c2.sum_all_activities<-exp(uci.c2.sum_all_activities))

(coef.c3.sum_all_activities<-exp(est.c3.sum_all_activities))
(clci.c3.sum_all_activities<-exp(lci.c3.sum_all_activities))
(cuci.c3.sum_all_activities<-exp(uci.c3.sum_all_activities))

(coef.c4.sum_all_activities<-exp(est.c4.sum_all_activities))
(clci.c4.sum_all_activities<-exp(lci.c4.sum_all_activities))
(cuci.c4.sum_all_activities<-exp(uci.c4.sum_all_activities))




x<-c("sum_all_activities",coef.c2.sum_all_activities,clci.c2.sum_all_activities,cuci.c2.sum_all_activities,
     coef.c3.sum_all_activities,clci.c3.sum_all_activities,cuci.c3.sum_all_activities,
     coef.c4.sum_all_activities,clci.c4.sum_all_activities,cuci.c4.sum_all_activities)
sum_all_activities<-data.frame(x)
sum_all_activitiest<-t(sum_all_activities)
sum_all_activitiest<-data.frame(sum_all_activitiest)
names(sum_all_activitiest)<-c("var","or_c2","lci_c2","uci_c2",
                 "or_c3","lci_c3","uci_c3",
                 "or_c4","lci_c4","uci_c4")


# Low physical activity lowpa
table(prediscore$lowpa,useNA = "ifany")
prediscore$lowpa<-relevel(prediscore$lowpa,"nolowpa")

modellowpa<-multinom(class_f~lowpa, data = prediscore)

summary(modellowpa)

(est.c2.lowpa<-summary(modellowpa)$coefficients[4])
(est.c3.lowpa<-summary(modellowpa)$coefficients[5])
(est.c4.lowpa<-summary(modellowpa)$coefficients[6])

(se.c2.lowpa<-summary(modellowpa)$standard.errors[4])
(se.c3.lowpa<-summary(modellowpa)$standard.errors[5])
(se.c4.lowpa<-summary(modellowpa)$standard.errors[6])

(lci.c2.lowpa<-est.c2.lowpa-1.95*se.c2.lowpa)
(lci.c3.lowpa<-est.c3.lowpa-1.95*se.c3.lowpa)
(lci.c4.lowpa<-est.c4.lowpa-1.95*se.c4.lowpa)

(uci.c2.lowpa<-est.c2.lowpa+1.95*se.c2.lowpa)
(uci.c3.lowpa<-est.c3.lowpa+1.95*se.c3.lowpa)
(uci.c4.lowpa<-est.c4.lowpa+1.95*se.c4.lowpa)

(coef.c2.lowpa<-exp(est.c2.lowpa))
(clci.c2.lowpa<-exp(lci.c2.lowpa))
(cuci.c2.lowpa<-exp(uci.c2.lowpa))

(coef.c3.lowpa<-exp(est.c3.lowpa))
(clci.c3.lowpa<-exp(lci.c3.lowpa))
(cuci.c3.lowpa<-exp(uci.c3.lowpa))

(coef.c4.lowpa<-exp(est.c4.lowpa))
(clci.c4.lowpa<-exp(lci.c4.lowpa))
(cuci.c4.lowpa<-exp(uci.c4.lowpa))




x<-c("lowpa",coef.c2.lowpa,clci.c2.lowpa,cuci.c2.lowpa,
     coef.c3.lowpa,clci.c3.lowpa,cuci.c3.lowpa,
     coef.c4.lowpa,clci.c4.lowpa,cuci.c4.lowpa)
lowpa<-data.frame(x)
lowpat<-t(lowpa)
lowpat<-data.frame(lowpat)
names(lowpat)<-c("var","or_c2","lci_c2","uci_c2",
                       "or_c3","lci_c3","uci_c3",
                       "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt, ownert,obeset,underweightt,lowpat)


# Current smoker

table(prediscore$smoker,useNA = "ifany")
prediscore$smoker<-relevel(prediscore$smoker,"no")

modelsmoker<-multinom(class_f~smoker, data = prediscore)

summary(modelsmoker)

(est.c2.smoker<-summary(modelsmoker)$coefficients[4])
(est.c3.smoker<-summary(modelsmoker)$coefficients[5])
(est.c4.smoker<-summary(modelsmoker)$coefficients[6])

(se.c2.smoker<-summary(modelsmoker)$standard.errors[4])
(se.c3.smoker<-summary(modelsmoker)$standard.errors[5])
(se.c4.smoker<-summary(modelsmoker)$standard.errors[6])

(lci.c2.smoker<-est.c2.smoker-1.95*se.c2.smoker)
(lci.c3.smoker<-est.c3.smoker-1.95*se.c3.smoker)
(lci.c4.smoker<-est.c4.smoker-1.95*se.c4.smoker)

(uci.c2.smoker<-est.c2.smoker+1.95*se.c2.smoker)
(uci.c3.smoker<-est.c3.smoker+1.95*se.c3.smoker)
(uci.c4.smoker<-est.c4.smoker+1.95*se.c4.smoker)

(coef.c2.smoker<-exp(est.c2.smoker))
(clci.c2.smoker<-exp(lci.c2.smoker))
(cuci.c2.smoker<-exp(uci.c2.smoker))

(coef.c3.smoker<-exp(est.c3.smoker))
(clci.c3.smoker<-exp(lci.c3.smoker))
(cuci.c3.smoker<-exp(uci.c3.smoker))

(coef.c4.smoker<-exp(est.c4.smoker))
(clci.c4.smoker<-exp(lci.c4.smoker))
(cuci.c4.smoker<-exp(uci.c4.smoker))




x<-c("smoker",coef.c2.smoker,clci.c2.smoker,cuci.c2.smoker,
     coef.c3.smoker,clci.c3.smoker,cuci.c3.smoker,
     coef.c4.smoker,clci.c4.smoker,cuci.c4.smoker)
smoker<-data.frame(x)
smokert<-t(smoker)
smokert<-data.frame(smokert)
names(smokert)<-c("var","or_c2","lci_c2","uci_c2",
                 "or_c3","lci_c3","uci_c3",
                 "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt, ownert,obeset,
           underweightt,lowpat,smokert)


# Blood group a agroup
table(prediscore$agroup,useNA = "ifany")
prediscore$agroup<-relevel(prediscore$agroup,"other")
modelagroup<-multinom(class_f~agroup, data = prediscore)
prediscore$agroup<-relevel(prediscore$agroup,"other")

summary(modelagroup)

(est.c2.agroup<-summary(modelagroup)$coefficients[4])
(est.c3.agroup<-summary(modelagroup)$coefficients[5])
(est.c4.agroup<-summary(modelagroup)$coefficients[6])

(se.c2.agroup<-summary(modelagroup)$standard.errors[4])
(se.c3.agroup<-summary(modelagroup)$standard.errors[5])
(se.c4.agroup<-summary(modelagroup)$standard.errors[6])

(lci.c2.agroup<-est.c2.agroup-1.95*se.c2.agroup)
(lci.c3.agroup<-est.c3.agroup-1.95*se.c3.agroup)
(lci.c4.agroup<-est.c4.agroup-1.95*se.c4.agroup)

(uci.c2.agroup<-est.c2.agroup+1.95*se.c2.agroup)
(uci.c3.agroup<-est.c3.agroup+1.95*se.c3.agroup)
(uci.c4.agroup<-est.c4.agroup+1.95*se.c4.agroup)

(coef.c2.agroup<-exp(est.c2.agroup))
(clci.c2.agroup<-exp(lci.c2.agroup))
(cuci.c2.agroup<-exp(uci.c2.agroup))

(coef.c3.agroup<-exp(est.c3.agroup))
(clci.c3.agroup<-exp(lci.c3.agroup))
(cuci.c3.agroup<-exp(uci.c3.agroup))

(coef.c4.agroup<-exp(est.c4.agroup))
(clci.c4.agroup<-exp(lci.c4.agroup))
(cuci.c4.agroup<-exp(uci.c4.agroup))




x<-c("agroup",coef.c2.agroup,clci.c2.agroup,cuci.c2.agroup,
     coef.c3.agroup,clci.c3.agroup,cuci.c3.agroup,
     coef.c4.agroup,clci.c4.agroup,cuci.c4.agroup)
agroup<-data.frame(x)
agroupt<-t(agroup)
agroupt<-data.frame(agroupt)
names(agroupt)<-c("var","or_c2","lci_c2","uci_c2",
                  "or_c3","lci_c3","uci_c3",
                  "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt, ownert,obeset,
           underweightt,lowpat,smokert,agroupt)




# Medications for anxiety or sleep medanx

table(prediscore$medanx)
prediscore$medanx<-relevel(prediscore$medanx,"no")
modelmedanx<-multinom(class_f~medanx, data = prediscore)

summary(modelmedanx)

(est.c2.medanx<-summary(modelmedanx)$coefficients[4])
(est.c3.medanx<-summary(modelmedanx)$coefficients[5])
(est.c4.medanx<-summary(modelmedanx)$coefficients[6])

(se.c2.medanx<-summary(modelmedanx)$standard.errors[4])
(se.c3.medanx<-summary(modelmedanx)$standard.errors[5])
(se.c4.medanx<-summary(modelmedanx)$standard.errors[6])

(lci.c2.medanx<-est.c2.medanx-1.95*se.c2.medanx)
(lci.c3.medanx<-est.c3.medanx-1.95*se.c3.medanx)
(lci.c4.medanx<-est.c4.medanx-1.95*se.c4.medanx)

(uci.c2.medanx<-est.c2.medanx+1.95*se.c2.medanx)
(uci.c3.medanx<-est.c3.medanx+1.95*se.c3.medanx)
(uci.c4.medanx<-est.c4.medanx+1.95*se.c4.medanx)

(coef.c2.medanx<-exp(est.c2.medanx))
(clci.c2.medanx<-exp(lci.c2.medanx))
(cuci.c2.medanx<-exp(uci.c2.medanx))

(coef.c3.medanx<-exp(est.c3.medanx))
(clci.c3.medanx<-exp(lci.c3.medanx))
(cuci.c3.medanx<-exp(uci.c3.medanx))

(coef.c4.medanx<-exp(est.c4.medanx))
(clci.c4.medanx<-exp(lci.c4.medanx))
(cuci.c4.medanx<-exp(uci.c4.medanx))




x<-c("medanx",coef.c2.medanx,clci.c2.medanx,cuci.c2.medanx,
     coef.c3.medanx,clci.c3.medanx,cuci.c3.medanx,
     coef.c4.medanx,clci.c4.medanx,cuci.c4.medanx)
medanx<-data.frame(x)
medanxt<-t(medanx)
medanxt<-data.frame(medanxt)
names(medanxt)<-c("var","or_c2","lci_c2","uci_c2",
                  "or_c3","lci_c3","uci_c3",
                  "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt, ownert,obeset,
           underweightt,lowpat,smokert,agroupt, medanxt)


# Diabetes

table(prediscore$diabetes, useNA = "ifany")
prediscore$diabetes<-relevel(prediscore$diabetes,"no")
modeldiabetes<-multinom(class_f~diabetes, data = prediscore)

summary(modeldiabetes)

(est.c2.diabetes<-summary(modeldiabetes)$coefficients[4])
(est.c3.diabetes<-summary(modeldiabetes)$coefficients[5])
(est.c4.diabetes<-summary(modeldiabetes)$coefficients[6])

(se.c2.diabetes<-summary(modeldiabetes)$standard.errors[4])
(se.c3.diabetes<-summary(modeldiabetes)$standard.errors[5])
(se.c4.diabetes<-summary(modeldiabetes)$standard.errors[6])

(lci.c2.diabetes<-est.c2.diabetes-1.95*se.c2.diabetes)
(lci.c3.diabetes<-est.c3.diabetes-1.95*se.c3.diabetes)
(lci.c4.diabetes<-est.c4.diabetes-1.95*se.c4.diabetes)

(uci.c2.diabetes<-est.c2.diabetes+1.95*se.c2.diabetes)
(uci.c3.diabetes<-est.c3.diabetes+1.95*se.c3.diabetes)
(uci.c4.diabetes<-est.c4.diabetes+1.95*se.c4.diabetes)

(coef.c2.diabetes<-exp(est.c2.diabetes))
(clci.c2.diabetes<-exp(lci.c2.diabetes))
(cuci.c2.diabetes<-exp(uci.c2.diabetes))

(coef.c3.diabetes<-exp(est.c3.diabetes))
(clci.c3.diabetes<-exp(lci.c3.diabetes))
(cuci.c3.diabetes<-exp(uci.c3.diabetes))

(coef.c4.diabetes<-exp(est.c4.diabetes))
(clci.c4.diabetes<-exp(lci.c4.diabetes))
(cuci.c4.diabetes<-exp(uci.c4.diabetes))




x<-c("diabetes",coef.c2.diabetes,clci.c2.diabetes,cuci.c2.diabetes,
     coef.c3.diabetes,clci.c3.diabetes,cuci.c3.diabetes,
     coef.c4.diabetes,clci.c4.diabetes,cuci.c4.diabetes)
diabetes<-data.frame(x)
diabetest<-t(diabetes)
diabetest<-data.frame(diabetest)
names(diabetest)<-c("var","or_c2","lci_c2","uci_c2",
                  "or_c3","lci_c3","uci_c3",
                  "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt, ownert,obeset,
           underweightt,lowpat,smokert,agroupt, medanxt, diabetest)


# asthma
table(prediscore$asthma, useNA = "ifany")
prediscore$asthma<-factor(prediscore$asthma, levels = c(0,1),labels = c("no","yes"))
prediscore$asthma<-relevel(prediscore$asthma,"no")

modelasthma<-multinom(class_f~asthma, data = prediscore)

summary(modelasthma)

(est.c2.asthma<-summary(modelasthma)$coefficients[4])
(est.c3.asthma<-summary(modelasthma)$coefficients[5])
(est.c4.asthma<-summary(modelasthma)$coefficients[6])

(se.c2.asthma<-summary(modelasthma)$standard.errors[4])
(se.c3.asthma<-summary(modelasthma)$standard.errors[5])
(se.c4.asthma<-summary(modelasthma)$standard.errors[6])

(lci.c2.asthma<-est.c2.asthma-1.95*se.c2.asthma)
(lci.c3.asthma<-est.c3.asthma-1.95*se.c3.asthma)
(lci.c4.asthma<-est.c4.asthma-1.95*se.c4.asthma)

(uci.c2.asthma<-est.c2.asthma+1.95*se.c2.asthma)
(uci.c3.asthma<-est.c3.asthma+1.95*se.c3.asthma)
(uci.c4.asthma<-est.c4.asthma+1.95*se.c4.asthma)

(coef.c2.asthma<-exp(est.c2.asthma))
(clci.c2.asthma<-exp(lci.c2.asthma))
(cuci.c2.asthma<-exp(uci.c2.asthma))

(coef.c3.asthma<-exp(est.c3.asthma))
(clci.c3.asthma<-exp(lci.c3.asthma))
(cuci.c3.asthma<-exp(uci.c3.asthma))

(coef.c4.asthma<-exp(est.c4.asthma))
(clci.c4.asthma<-exp(lci.c4.asthma))
(cuci.c4.asthma<-exp(uci.c4.asthma))




x<-c("asthma",coef.c2.asthma,clci.c2.asthma,cuci.c2.asthma,
     coef.c3.asthma,clci.c3.asthma,cuci.c3.asthma,
     coef.c4.asthma,clci.c4.asthma,cuci.c4.asthma)
asthma<-data.frame(x)
asthmat<-t(asthma)
asthmat<-data.frame(asthmat)
names(asthmat)<-c("var","or_c2","lci_c2","uci_c2",
                    "or_c3","lci_c3","uci_c3",
                    "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt, ownert,obeset,
           underweightt,lowpat,smokert,agroupt, medanxt, diabetest,asthmat)



# multimorb
table(prediscore$multimorb, useNA = "ifany")
modelmultimorb<-multinom(class_f~multimorb, data = prediscore)

summary(modelmultimorb)

(est.c2.multimorb<-summary(modelmultimorb)$coefficients[4])
(est.c3.multimorb<-summary(modelmultimorb)$coefficients[5])
(est.c4.multimorb<-summary(modelmultimorb)$coefficients[6])

(se.c2.multimorb<-summary(modelmultimorb)$standard.errors[4])
(se.c3.multimorb<-summary(modelmultimorb)$standard.errors[5])
(se.c4.multimorb<-summary(modelmultimorb)$standard.errors[6])

(lci.c2.multimorb<-est.c2.multimorb-1.95*se.c2.multimorb)
(lci.c3.multimorb<-est.c3.multimorb-1.95*se.c3.multimorb)
(lci.c4.multimorb<-est.c4.multimorb-1.95*se.c4.multimorb)

(uci.c2.multimorb<-est.c2.multimorb+1.95*se.c2.multimorb)
(uci.c3.multimorb<-est.c3.multimorb+1.95*se.c3.multimorb)
(uci.c4.multimorb<-est.c4.multimorb+1.95*se.c4.multimorb)

(coef.c2.multimorb<-exp(est.c2.multimorb))
(clci.c2.multimorb<-exp(lci.c2.multimorb))
(cuci.c2.multimorb<-exp(uci.c2.multimorb))

(coef.c3.multimorb<-exp(est.c3.multimorb))
(clci.c3.multimorb<-exp(lci.c3.multimorb))
(cuci.c3.multimorb<-exp(uci.c3.multimorb))

(coef.c4.multimorb<-exp(est.c4.multimorb))
(clci.c4.multimorb<-exp(lci.c4.multimorb))
(cuci.c4.multimorb<-exp(uci.c4.multimorb))




x<-c("multimorb",coef.c2.multimorb,clci.c2.multimorb,cuci.c2.multimorb,
     coef.c3.multimorb,clci.c3.multimorb,cuci.c3.multimorb,
     coef.c4.multimorb,clci.c4.multimorb,cuci.c4.multimorb)
multimorb<-data.frame(x)
multimorbt<-t(multimorb)
multimorbt<-data.frame(multimorbt)
names(multimorbt)<-c("var","or_c2","lci_c2","uci_c2",
                  "or_c3","lci_c3","uci_c3",
                  "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt, ownert,obeset,
           underweightt,lowpat,smokert,agroupt, medanxt, diabetest,asthmat, multimorbt)


# weight_loss
table(prediscore$weight_loss,useNA = "ifany")
modelweight_loss<-multinom(class_f~weight_loss, data = prediscore)

summary(modelweight_loss)

(est.c2.weight_loss<-summary(modelweight_loss)$coefficients[4])
(est.c3.weight_loss<-summary(modelweight_loss)$coefficients[5])
(est.c4.weight_loss<-summary(modelweight_loss)$coefficients[6])

(se.c2.weight_loss<-summary(modelweight_loss)$standard.errors[4])
(se.c3.weight_loss<-summary(modelweight_loss)$standard.errors[5])
(se.c4.weight_loss<-summary(modelweight_loss)$standard.errors[6])

(lci.c2.weight_loss<-est.c2.weight_loss-1.95*se.c2.weight_loss)
(lci.c3.weight_loss<-est.c3.weight_loss-1.95*se.c3.weight_loss)
(lci.c4.weight_loss<-est.c4.weight_loss-1.95*se.c4.weight_loss)

(uci.c2.weight_loss<-est.c2.weight_loss+1.95*se.c2.weight_loss)
(uci.c3.weight_loss<-est.c3.weight_loss+1.95*se.c3.weight_loss)
(uci.c4.weight_loss<-est.c4.weight_loss+1.95*se.c4.weight_loss)

(coef.c2.weight_loss<-exp(est.c2.weight_loss))
(clci.c2.weight_loss<-exp(lci.c2.weight_loss))
(cuci.c2.weight_loss<-exp(uci.c2.weight_loss))

(coef.c3.weight_loss<-exp(est.c3.weight_loss))
(clci.c3.weight_loss<-exp(lci.c3.weight_loss))
(cuci.c3.weight_loss<-exp(uci.c3.weight_loss))

(coef.c4.weight_loss<-exp(est.c4.weight_loss))
(clci.c4.weight_loss<-exp(lci.c4.weight_loss))
(cuci.c4.weight_loss<-exp(uci.c4.weight_loss))




x<-c("weight_loss",coef.c2.weight_loss,clci.c2.weight_loss,cuci.c2.weight_loss,
     coef.c3.weight_loss,clci.c3.weight_loss,cuci.c3.weight_loss,
     coef.c4.weight_loss,clci.c4.weight_loss,cuci.c4.weight_loss)
weight_loss<-data.frame(x)
weight_losst<-t(weight_loss)
weight_losst<-data.frame(weight_losst)
names(weight_losst)<-c("var","or_c2","lci_c2","uci_c2",
                     "or_c3","lci_c3","uci_c3",
                     "or_c4","lci_c4","uci_c4")

all<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt, ownert,obeset,
           underweightt,lowpat,smokert,agroupt, medanxt, diabetest,asthmat, 
           multimorbt, weight_losst)


# polypharmacy medcat
table(prediscore$medcat)
modelmedcat<-multinom(class_f~medcat, data = prediscore)

summary(modelmedcat)

(est.c2.medcat<-summary(modelmedcat)$coefficients[4])
(est.c3.medcat<-summary(modelmedcat)$coefficients[5])
(est.c4.medcat<-summary(modelmedcat)$coefficients[6])

(se.c2.medcat<-summary(modelmedcat)$standard.errors[4])
(se.c3.medcat<-summary(modelmedcat)$standard.errors[5])
(se.c4.medcat<-summary(modelmedcat)$standard.errors[6])

(lci.c2.medcat<-est.c2.medcat-1.95*se.c2.medcat)
(lci.c3.medcat<-est.c3.medcat-1.95*se.c3.medcat)
(lci.c4.medcat<-est.c4.medcat-1.95*se.c4.medcat)

(uci.c2.medcat<-est.c2.medcat+1.95*se.c2.medcat)
(uci.c3.medcat<-est.c3.medcat+1.95*se.c3.medcat)
(uci.c4.medcat<-est.c4.medcat+1.95*se.c4.medcat)

(coef.c2.medcat<-exp(est.c2.medcat))
(clci.c2.medcat<-exp(lci.c2.medcat))
(cuci.c2.medcat<-exp(uci.c2.medcat))

(coef.c3.medcat<-exp(est.c3.medcat))
(clci.c3.medcat<-exp(lci.c3.medcat))
(cuci.c3.medcat<-exp(uci.c3.medcat))

(coef.c4.medcat<-exp(est.c4.medcat))
(clci.c4.medcat<-exp(lci.c4.medcat))
(cuci.c4.medcat<-exp(uci.c4.medcat))




x<-c("medcat",coef.c2.medcat,clci.c2.medcat,cuci.c2.medcat,
     coef.c3.medcat,clci.c3.medcat,cuci.c3.medcat,
     coef.c4.medcat,clci.c4.medcat,cuci.c4.medcat)
medcat<-data.frame(x)
medcatt<-t(medcat)
medcatt<-data.frame(medcatt)
names(medcatt)<-c("var","or_c2","lci_c2","uci_c2",
                       "or_c3","lci_c3","uci_c3",
                       "or_c4","lci_c4","uci_c4")

all0<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt, ownert,obeset,
            underweightt,lowpat,smokert,agroupt, medanxt, diabetest,asthmat, 
            multimorbt, weight_losst,
            medcatt)



# Eyeglass always
table(prediscore$eyeglass)
prediscore$eyeglass<-relevel(prediscore$eyeglass,"No")


modeleyeglass<-multinom(class_f~eyeglass, data = prediscore)

summary(modeleyeglass)

(est.c2.eyeglassa<-summary(modeleyeglass)$coefficients[4])
(est.c3.eyeglassa<-summary(modeleyeglass)$coefficients[5])
(est.c4.eyeglassa<-summary(modeleyeglass)$coefficients[6])

(se.c2.eyeglassa<-summary(modeleyeglass)$standard.errors[4])
(se.c3.eyeglassa<-summary(modeleyeglass)$standard.errors[5])
(se.c4.eyeglassa<-summary(modeleyeglass)$standard.errors[6])

(lci.c2.eyeglassa<-est.c2.eyeglassa-1.95*se.c2.eyeglassa)
(lci.c3.eyeglassa<-est.c3.eyeglassa-1.95*se.c3.eyeglassa)
(lci.c4.eyeglassa<-est.c4.eyeglassa-1.95*se.c4.eyeglassa)

(uci.c2.eyeglassa<-est.c2.eyeglassa+1.95*se.c2.eyeglassa)
(uci.c3.eyeglassa<-est.c3.eyeglassa+1.95*se.c3.eyeglassa)
(uci.c4.eyeglassa<-est.c4.eyeglassa+1.95*se.c4.eyeglassa)

(coef.c2.eyeglassa<-exp(est.c2.eyeglassa))
(clci.c2.eyeglassa<-exp(lci.c2.eyeglassa))
(cuci.c2.eyeglassa<-exp(uci.c2.eyeglassa))

(coef.c3.eyeglassa<-exp(est.c3.eyeglassa))
(clci.c3.eyeglassa<-exp(lci.c3.eyeglassa))
(cuci.c3.eyeglassa<-exp(uci.c3.eyeglassa))

(coef.c4.eyeglassa<-exp(est.c4.eyeglassa))
(clci.c4.eyeglassa<-exp(lci.c4.eyeglassa))
(cuci.c4.eyeglassa<-exp(uci.c4.eyeglassa))




x<-c("eyeglassa",coef.c2.eyeglassa,clci.c2.eyeglassa,cuci.c2.eyeglassa,
     coef.c3.eyeglassa,clci.c3.eyeglassa,cuci.c3.eyeglassa,
     coef.c4.eyeglassa,clci.c4.eyeglassa,cuci.c4.eyeglassa)
eyeglassa<-data.frame(x)
eyeglassat<-t(eyeglassa)
eyeglassat<-data.frame(eyeglassat)
names(eyeglassat)<-c("var","or_c2","lci_c2","uci_c2",
                  "or_c3","lci_c3","uci_c3",
                  "or_c4","lci_c4","uci_c4")




all0<-rbind(sext,youngt,oldt,loweduct,lowincomet, workt, ownert,obeset,
            underweightt,lowpat,smokert,agroupt, medanxt, diabetest,asthmat, multimorbt, 
            weight_losst,medcatt,eyeglassat)

# Eyeglass sometimes

summary(modeleyeglass)

(est.c2.eyeglasss<-summary(modeleyeglass)$coefficients[7])
(est.c3.eyeglasss<-summary(modeleyeglass)$coefficients[8])
(est.c4.eyeglasss<-summary(modeleyeglass)$coefficients[9])

(se.c2.eyeglasss<-summary(modeleyeglass)$standard.errors[7])
(se.c3.eyeglasss<-summary(modeleyeglass)$standard.errors[8])
(se.c4.eyeglasss<-summary(modeleyeglass)$standard.errors[9])

(lci.c2.eyeglasss<-est.c2.eyeglasss-1.95*se.c2.eyeglasss)
(lci.c3.eyeglasss<-est.c3.eyeglasss-1.95*se.c3.eyeglasss)
(lci.c4.eyeglasss<-est.c4.eyeglasss-1.95*se.c4.eyeglasss)

(uci.c2.eyeglasss<-est.c2.eyeglasss+1.95*se.c2.eyeglasss)
(uci.c3.eyeglasss<-est.c3.eyeglasss+1.95*se.c3.eyeglasss)
(uci.c4.eyeglasss<-est.c4.eyeglasss+1.95*se.c4.eyeglasss)

(coef.c2.eyeglasss<-exp(est.c2.eyeglasss))
(clci.c2.eyeglasss<-exp(lci.c2.eyeglasss))
(cuci.c2.eyeglasss<-exp(uci.c2.eyeglasss))

(coef.c3.eyeglasss<-exp(est.c3.eyeglasss))
(clci.c3.eyeglasss<-exp(lci.c3.eyeglasss))
(cuci.c3.eyeglasss<-exp(uci.c3.eyeglasss))

(coef.c4.eyeglasss<-exp(est.c4.eyeglasss))
(clci.c4.eyeglasss<-exp(lci.c4.eyeglasss))
(cuci.c4.eyeglasss<-exp(uci.c4.eyeglasss))




x<-c("eyeglasss",coef.c2.eyeglasss,clci.c2.eyeglasss,cuci.c2.eyeglasss,
     coef.c3.eyeglasss,clci.c3.eyeglasss,cuci.c3.eyeglasss,
     coef.c4.eyeglasss,clci.c4.eyeglasss,cuci.c4.eyeglasss)
eyeglasss<-data.frame(x)
eyeglassst<-t(eyeglasss)
eyeglassst<-data.frame(eyeglassst)
names(eyeglassst)<-c("var","or_c2","lci_c2","uci_c2",
                     "or_c3","lci_c3","uci_c3",
                     "or_c4","lci_c4","uci_c4")



# NIH severity



table(prediscore$NIH_classification_or, useNA = "ifany")

prediscore$NIH_classification_mod<-NA
prediscore$NIH_classification_mod[prediscore$NIH_classification_or=="Asymptomatic"]<-"asymp"
prediscore$NIH_classification_mod[prediscore$NIH_classification_or=="Mild illness"]<-"mild"
prediscore$NIH_classification_mod[prediscore$NIH_classification_or=="Moderate illness" |
                               prediscore$NIH_classification_or=="Hospitalised"]<-"mod-sev"
prediscore$NIH_classification_mod<-as.factor(prediscore$NIH_classification_mod)
table(prediscore$NIH_classification_mod, useNA = "ifany")

# NIH dichotomised into asmp-mild and moderate-severe
prediscore$NIH_classification_dic<-NA
prediscore$NIH_classification_dic[prediscore$NIH_classification_or=="Asymptomatic"|
                                    prediscore$NIH_classification_or=="Mild illness"]<-"asymp_mild"
prediscore$NIH_classification_dic[prediscore$NIH_classification_or=="Moderate illness"|
                                    prediscore$NIH_classification_or=="Hospitalised"]<-"mod_sev"
prediscore$NIH_classification_dic<-as.factor(prediscore$NIH_classification_dic)
table(prediscore$NIH_classification_dic, useNA = "ifany")

prediscore$NIH_classification_mod<-relevel(prediscore$NIH_classification_mod,"asymp")


modelNIH<-multinom(class_f~NIH_classification_dic, data = prediscore)

summary(modelNIH)

(est.c2.NIHdic<-summary(modelNIH)$coefficients[4])
(est.c3.NIHdic<-summary(modelNIH)$coefficients[5])
(est.c4.NIHdic<-summary(modelNIH)$coefficients[6])

(se.c2.NIHdic<-summary(modelNIH)$standard.errors[4])
(se.c3.NIHdic<-summary(modelNIH)$standard.errors[5])
(se.c4.NIHdic<-summary(modelNIH)$standard.errors[6])

(lci.c2.NIHdic<-est.c2.NIHdic-1.95*se.c2.NIHdic)
(lci.c3.NIHdic<-est.c3.NIHdic-1.95*se.c3.NIHdic)
(lci.c4.NIHdic<-est.c4.NIHdic-1.95*se.c4.NIHdic)

(uci.c2.NIHdic<-est.c2.NIHdic+1.95*se.c2.NIHdic)
(uci.c3.NIHdic<-est.c3.NIHdic+1.95*se.c3.NIHdic)
(uci.c4.NIHdic<-est.c4.NIHdic+1.95*se.c4.NIHdic)

(coef.c2.NIHdic<-exp(est.c2.NIHdic))
(clci.c2.NIHdic<-exp(lci.c2.NIHdic))
(cuci.c2.NIHdic<-exp(uci.c2.NIHdic))

(coef.c3.NIHdic<-exp(est.c3.NIHdic))
(clci.c3.NIHdic<-exp(lci.c3.NIHdic))
(cuci.c3.NIHdic<-exp(uci.c3.NIHdic))

(coef.c4.NIHdic<-exp(est.c4.NIHdic))
(clci.c4.NIHdic<-exp(lci.c4.NIHdic))
(cuci.c4.NIHdic<-exp(uci.c4.NIHdic))




x<-c("NIHdic",coef.c2.NIHdic,clci.c2.NIHdic,cuci.c2.NIHdic,
     coef.c3.NIHdic,clci.c3.NIHdic,cuci.c3.NIHdic,
     coef.c4.NIHdic,clci.c4.NIHdic,cuci.c4.NIHdic)
NIHdic<-data.frame(x)
NIHdict<-t(NIHdic)
NIHdict<-data.frame(NIHdict)
names(NIHdict)<-c("var","or_c2","lci_c2","uci_c2",
                     "or_c3","lci_c3","uci_c3",
                     "or_c4","lci_c4","uci_c4")


# Eyeglass always
table(prediscore$eyeglass)
prediscore$eyeglass<-relevel(prediscore$eyeglass,"No")


modeleyeglass<-multinom(class_f~eyeglass, data = prediscore)

summary(modeleyeglass)

(est.c2.eyeglassa<-summary(modeleyeglass)$coefficients[4])
(est.c3.eyeglassa<-summary(modeleyeglass)$coefficients[5])
(est.c4.eyeglassa<-summary(modeleyeglass)$coefficients[6])

(se.c2.eyeglassa<-summary(modeleyeglass)$standard.errors[4])
(se.c3.eyeglassa<-summary(modeleyeglass)$standard.errors[5])
(se.c4.eyeglassa<-summary(modeleyeglass)$standard.errors[6])

(lci.c2.eyeglassa<-est.c2.eyeglassa-1.95*se.c2.eyeglassa)
(lci.c3.eyeglassa<-est.c3.eyeglassa-1.95*se.c3.eyeglassa)
(lci.c4.eyeglassa<-est.c4.eyeglassa-1.95*se.c4.eyeglassa)

(uci.c2.eyeglassa<-est.c2.eyeglassa+1.95*se.c2.eyeglassa)
(uci.c3.eyeglassa<-est.c3.eyeglassa+1.95*se.c3.eyeglassa)
(uci.c4.eyeglassa<-est.c4.eyeglassa+1.95*se.c4.eyeglassa)

(coef.c2.eyeglassa<-exp(est.c2.eyeglassa))
(clci.c2.eyeglassa<-exp(lci.c2.eyeglassa))
(cuci.c2.eyeglassa<-exp(uci.c2.eyeglassa))

(coef.c3.eyeglassa<-exp(est.c3.eyeglassa))
(clci.c3.eyeglassa<-exp(lci.c3.eyeglassa))
(cuci.c3.eyeglassa<-exp(uci.c3.eyeglassa))

(coef.c4.eyeglassa<-exp(est.c4.eyeglassa))
(clci.c4.eyeglassa<-exp(lci.c4.eyeglassa))
(cuci.c4.eyeglassa<-exp(uci.c4.eyeglassa))




x<-c("eyeglassa",coef.c2.eyeglassa,clci.c2.eyeglassa,cuci.c2.eyeglassa,
     coef.c3.eyeglassa,clci.c3.eyeglassa,cuci.c3.eyeglassa,
     coef.c4.eyeglassa,clci.c4.eyeglassa,cuci.c4.eyeglassa)
eyeglassa<-data.frame(x)
eyeglassat<-t(eyeglassa)
eyeglassat<-data.frame(eyeglassat)
names(eyeglassat)<-c("var","or_c2","lci_c2","uci_c2",
                     "or_c3","lci_c3","uci_c3",
                     "or_c4","lci_c4","uci_c4")




all0<-rbind(sext,aget,oldt,loweduct,incomet,lowincomet, workt, eyeglassat, eyeglassst,bmit,obeset,
            underweightt,sum_all_activitiest,lowpat,smokert,agroupt, medanxt, diabetest,multimorbt, 
            weight_losst,medcatt, NIHdict)




write.csv(all0,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/table3multinomial_univariate.csv", row.names = FALSE)



