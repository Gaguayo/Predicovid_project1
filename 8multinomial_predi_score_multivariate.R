# Calculations with symptom score

library(dplyr)

Sys.setenv(LANG = "en")

# prediscore<-read.csv("P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/fatigclass4_11012022.csv", stringsAsFactors = TRUE)
prediscore<-read.csv("P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/prediscoreclass.csv", stringsAsFactors = TRUE)


dim(prediscore)
str(prediscore)
head(prediscore)
table(prediscore$.imp)
names(prediscore)

median(prediscore$income, na.rm=TRUE)
lowquant<-quantile(prediscore$income, probs=c(0,0.25,0.5,0.75,1),na.rm=TRUE)[[2]]
prediscore$lowincome<-ifelse(prediscore$income<lowquant,"low","high")
prediscore$lowincome<-factor(prediscore$lowincome)
table( prediscore$lowincome, useNA = "ifany")

# ref https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/

library(nnet)

mean(prediscore$age_estimateyears)

library(dplyr)
prediscore0<-prediscore%>%
        filter(JOURn==0)
dim(prediscore0)

summary(prediscore$score)
sd(prediscore0$score, na.rm = TRUE)
hist(prediscore0$score)

summary(prediscore$score)

# Correlation and multinomial regression of score with NIH classification

table(prediscore0$NIH_classification_or, useNA = "ifany")

prediscore$NIH_classification_mod<-NA
prediscore$NIH_classification_mod[prediscore$NIH_classification_or=="Asymptomatic"]<-"asymp"
prediscore$NIH_classification_mod[prediscore$NIH_classification_or=="Mild illness"]<-"mild"
prediscore$NIH_classification_mod[prediscore$NIH_classification_or=="Moderate illness" |
                                     prediscore$NIH_classification_or=="Hospitalised"]<-"mod_sev"
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



modelscore<-multinom(NIH_classification_mod~score, data = prediscore)
summary(modelscore)

(est.mild<-summary(modelscore)$coefficients[3])
(est.mod<-summary(modelscore)$coefficients[4])

(se.mild<-summary(modelscore)$standard.errors[3])
(se.mod<-summary(modelscore)$standard.errors[4])

(lci.mild<-est.mild-1.95*se.mild)
(uci.mild<-est.mild+1.95*se.mild)

(lci.mod<-est.mod-1.95*se.mod)
(uci.mod<-est.mod+1.95*se.mod)

(coef.mild<-exp(est.mild))
(clci.mild<-exp(lci.mild))
(clci.mild<-exp(uci.mild))

(coef.mod<-exp(est.mod))
(clci.mod<-exp(lci.mod))
(clci.mod<-exp(uci.mod))

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

# Medications for anxiety or sleep medanx
# Model 0

table(prediscore$medanx)
prediscore$medanx<-relevel(prediscore$medanx,"no")
modelmedanx<-multinom(class_f~medanx, data = prediscore)

summary(modelmedanx)


# Intercept

(intest.c2.medanx<-summary(modelmedanx)$coefficients[1])
(intest.c3.medanx<-summary(modelmedanx)$coefficients[2])
(intest.c4.medanx<-summary(modelmedanx)$coefficients[3])

(intse.c2.medanx<-summary(modelmedanx)$standard.errors[1])
(intse.c3.medanx<-summary(modelmedanx)$standard.errors[2])
(intse.c4.medanx<-summary(modelmedanx)$standard.errors[3])

(intlci.c2.medanx<-intest.c2.medanx-1.95*intse.c2.medanx)
(intlci.c3.medanx<-intest.c3.medanx-1.95*intse.c3.medanx)
(intlci.c4.medanx<-intest.c4.medanx-1.95*intse.c4.medanx)

(intuci.c2.medanx<-intest.c2.medanx+1.95*intse.c2.medanx)
(intuci.c3.medanx<-intest.c3.medanx+1.95*intse.c3.medanx)
(intuci.c4.medanx<-intest.c4.medanx+1.95*intse.c4.medanx)

(intcoef.c2.medanx<-exp(intest.c2.medanx))
(intclci.c2.medanx<-exp(intlci.c2.medanx))
(intcuci.c2.medanx<-exp(intuci.c2.medanx))

(intcoef.c3.medanx<-exp(intest.c3.medanx))
(intclci.c3.medanx<-exp(intlci.c3.medanx))
(intcuci.c3.medanx<-exp(intuci.c3.medanx))

(intcoef.c4.medanx<-exp(intest.c4.medanx))
(intclci.c4.medanx<-exp(intlci.c4.medanx))
(intcuci.c4.medanx<-exp(intuci.c4.medanx))

# anxiety

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




x<-c("medanx", intcoef.c2.medanx,intclci.c2.medanx,intcuci.c2.medanx,
     intcoef.c3.medanx,intclci.c3.medanx,intcuci.c3.medanx,
     intcoef.c4.medanx,intclci.c4.medanx,intcuci.c4.medanx,
     coef.c2.medanx,clci.c2.medanx,cuci.c2.medanx,
     coef.c3.medanx,clci.c3.medanx,cuci.c3.medanx,
     coef.c4.medanx,clci.c4.medanx,cuci.c4.medanx)
medanx<-data.frame(x)
medanxt<-t(medanx)
medanxt<-data.frame(medanxt)
names(medanxt)<-c("var","intor_c2","intlci_c2","intuci_c2",
                  "intor_c3","intlci_c3","intuci_c3",
                  "intor_c4","intlci_c4","intuci_c4",
                  "or_c2","lci_c2","uci_c2",
                  "or_c3","lci_c3","uci_c3",
                  "or_c4","lci_c4","uci_c4")

write.csv(medanxt,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/model0.csv", row.names = FALSE)



# Model 1 adjusted by age and sex

prediscore$sexm<-relevel(prediscore$sexm,"male")


# modelmedanx1<-multinom(class_f~medanx + older + sexm, data = prediscore)

modelmedanx1<-multinom(class_f~medanx*sexm + older, data = prediscore)
# 
# modelmedanxold<-multinom(class_f~medanx + older, data = prediscore)
# 
# summary(modelmedanxold)
summary(modelmedanx1)

# Intercept

(intest.c2.medanx1<-summary(modelmedanx1)$coefficients[1])
(intest.c3.medanx1<-summary(modelmedanx1)$coefficients[2])
(intest.c4.medanx1<-summary(modelmedanx1)$coefficients[3])

(intse.c2.medanx1<-summary(modelmedanx1)$standard.errors[1])
(intse.c3.medanx1<-summary(modelmedanx1)$standard.errors[2])
(intse.c4.medanx1<-summary(modelmedanx1)$standard.errors[3])

(intlci.c2.medanx1<-intest.c2.medanx1-1.95*intse.c2.medanx1)
(intlci.c3.medanx1<-intest.c3.medanx1-1.95*intse.c3.medanx1)
(intlci.c4.medanx1<-intest.c4.medanx1-1.95*intse.c4.medanx1)

(intuci.c2.medanx1<-intest.c2.medanx1+1.95*intse.c2.medanx1)
(intuci.c3.medanx1<-intest.c3.medanx1+1.95*intse.c3.medanx1)
(intuci.c4.medanx1<-intest.c4.medanx1+1.95*intse.c4.medanx1)

(intcoef.c2.medanx1<-exp(intest.c2.medanx1))
(intclci.c2.medanx1<-exp(intlci.c2.medanx1))
(intcuci.c2.medanx1<-exp(intuci.c2.medanx1))

(intcoef.c3.medanx1<-exp(intest.c3.medanx1))
(intclci.c3.medanx1<-exp(intlci.c3.medanx1))
(intcuci.c3.medanx1<-exp(intuci.c3.medanx1))

(intcoef.c4.medanx1<-exp(intest.c4.medanx1))
(intclci.c4.medanx1<-exp(intlci.c4.medanx1))
(intcuci.c4.medanx1<-exp(intuci.c4.medanx1))

# anxiety

(est.c2.medanx1<-summary(modelmedanx1)$coefficients[4])
(est.c3.medanx1<-summary(modelmedanx1)$coefficients[5])
(est.c4.medanx1<-summary(modelmedanx1)$coefficients[6])

(se.c2.medanx1<-summary(modelmedanx1)$standard.errors[4])
(se.c3.medanx1<-summary(modelmedanx1)$standard.errors[5])
(se.c4.medanx1<-summary(modelmedanx1)$standard.errors[6])

(lci.c2.medanx1<-est.c2.medanx1-1.95*se.c2.medanx1)
(lci.c3.medanx1<-est.c3.medanx1-1.95*se.c3.medanx1)
(lci.c4.medanx1<-est.c4.medanx1-1.95*se.c4.medanx1)

(uci.c2.medanx1<-est.c2.medanx1+1.95*se.c2.medanx1)
(uci.c3.medanx1<-est.c3.medanx1+1.95*se.c3.medanx1)
(uci.c4.medanx1<-est.c4.medanx1+1.95*se.c4.medanx1)

(coef.c2.medanx1<-exp(est.c2.medanx1))
(clci.c2.medanx1<-exp(lci.c2.medanx1))
(cuci.c2.medanx1<-exp(uci.c2.medanx1))

(coef.c3.medanx1<-exp(est.c3.medanx1))
(clci.c3.medanx1<-exp(lci.c3.medanx1))
(cuci.c3.medanx1<-exp(uci.c3.medanx1))

(coef.c4.medanx1<-exp(est.c4.medanx1))
(clci.c4.medanx1<-exp(lci.c4.medanx1))
(cuci.c4.medanx1<-exp(uci.c4.medanx1))


# Sex

(sexest.c2.medanx1<-summary(modelmedanx1)$coefficients[7])
(sexest.c3.medanx1<-summary(modelmedanx1)$coefficients[8])
(sexest.c4.medanx1<-summary(modelmedanx1)$coefficients[9])

(sexse.c2.medanx1<-summary(modelmedanx1)$standard.errors[7])
(sexse.c3.medanx1<-summary(modelmedanx1)$standard.errors[8])
(sexse.c4.medanx1<-summary(modelmedanx1)$standard.errors[9])

(sexlci.c2.medanx1<-sexest.c2.medanx1-1.95*sexse.c2.medanx1)
(sexlci.c3.medanx1<-sexest.c3.medanx1-1.95*sexse.c3.medanx1)
(sexlci.c4.medanx1<-sexest.c4.medanx1-1.95*sexse.c4.medanx1)

(sexuci.c2.medanx1<-sexest.c2.medanx1+1.95*sexse.c2.medanx1)
(sexuci.c3.medanx1<-sexest.c3.medanx1+1.95*sexse.c3.medanx1)
(sexuci.c4.medanx1<-sexest.c4.medanx1+1.95*sexse.c4.medanx1)

(sexcoef.c2.medanx1<-exp(sexest.c2.medanx1))
(sexclci.c2.medanx1<-exp(sexlci.c2.medanx1))
(sexcuci.c2.medanx1<-exp(sexuci.c2.medanx1))

(sexcoef.c3.medanx1<-exp(sexest.c3.medanx1))
(sexclci.c3.medanx1<-exp(sexlci.c3.medanx1))
(sexcuci.c3.medanx1<-exp(sexuci.c3.medanx1))

(sexcoef.c4.medanx1<-exp(sexest.c4.medanx1))
(sexclci.c4.medanx1<-exp(sexlci.c4.medanx1))
(sexcuci.c4.medanx1<-exp(sexuci.c4.medanx1))

# Age

(ageest.c2.medanx1<-summary(modelmedanx1)$coefficients[10])
(ageest.c3.medanx1<-summary(modelmedanx1)$coefficients[11])
(ageest.c4.medanx1<-summary(modelmedanx1)$coefficients[12])

(agese.c2.medanx1<-summary(modelmedanx1)$standard.errors[10])
(agese.c3.medanx1<-summary(modelmedanx1)$standard.errors[11])
(agese.c4.medanx1<-summary(modelmedanx1)$standard.errors[12])

(agelci.c2.medanx1<-ageest.c2.medanx1-1.95*agese.c2.medanx1)
(agelci.c3.medanx1<-ageest.c3.medanx1-1.95*agese.c3.medanx1)
(agelci.c4.medanx1<-ageest.c4.medanx1-1.95*agese.c4.medanx1)

(ageuci.c2.medanx1<-ageest.c2.medanx1+1.95*agese.c2.medanx1)
(ageuci.c3.medanx1<-ageest.c3.medanx1+1.95*agese.c3.medanx1)
(ageuci.c4.medanx1<-ageest.c4.medanx1+1.95*agese.c4.medanx1)

(agecoef.c2.medanx1<-exp(ageest.c2.medanx1))
(ageclci.c2.medanx1<-exp(agelci.c2.medanx1))
(agecuci.c2.medanx1<-exp(ageuci.c2.medanx1))

(agecoef.c3.medanx1<-exp(ageest.c3.medanx1))
(ageclci.c3.medanx1<-exp(agelci.c3.medanx1))
(agecuci.c3.medanx1<-exp(ageuci.c3.medanx1))

(agecoef.c4.medanx1<-exp(ageest.c4.medanx1))
(ageclci.c4.medanx1<-exp(agelci.c4.medanx1))
(agecuci.c4.medanx1<-exp(ageuci.c4.medanx1))


# Interaction anx-sex

(anx_sexest.c2.medanx1<-summary(modelmedanx1)$coefficients[13])
(anx_sexest.c3.medanx1<-summary(modelmedanx1)$coefficients[14])
(anx_sexest.c4.medanx1<-summary(modelmedanx1)$coefficients[15])

(anx_sexse.c2.medanx1<-summary(modelmedanx1)$standard.errors[13])
(anx_sexse.c3.medanx1<-summary(modelmedanx1)$standard.errors[14])
(anx_sexse.c4.medanx1<-summary(modelmedanx1)$standard.errors[15])

(anx_sexlci.c2.medanx1<-anx_sexest.c2.medanx1-1.95*anx_sexse.c2.medanx1)
(anx_sexlci.c3.medanx1<-anx_sexest.c3.medanx1-1.95*anx_sexse.c3.medanx1)
(anx_sexlci.c4.medanx1<-anx_sexest.c4.medanx1-1.95*anx_sexse.c4.medanx1)

(anx_sexuci.c2.medanx1<-anx_sexest.c2.medanx1+1.95*anx_sexse.c2.medanx1)
(anx_sexuci.c3.medanx1<-anx_sexest.c3.medanx1+1.95*anx_sexse.c3.medanx1)
(anx_sexuci.c4.medanx1<-anx_sexest.c4.medanx1+1.95*anx_sexse.c4.medanx1)

(anx_sexcoef.c2.medanx1<-exp(anx_sexest.c2.medanx1))
(anx_sexclci.c2.medanx1<-exp(anx_sexlci.c2.medanx1))
(anx_sexcuci.c2.medanx1<-exp(anx_sexuci.c2.medanx1))

(anx_sexcoef.c3.medanx1<-exp(anx_sexest.c3.medanx1))
(anx_sexclci.c3.medanx1<-exp(anx_sexlci.c3.medanx1))
(anx_sexcuci.c3.medanx1<-exp(anx_sexuci.c3.medanx1))

(anx_sexcoef.c4.medanx1<-exp(anx_sexest.c4.medanx1))
(anx_sexclci.c4.medanx1<-exp(anx_sexlci.c4.medanx1))
(anx_sexcuci.c4.medanx1<-exp(anx_sexuci.c4.medanx1))



x<-c("medanx1", intcoef.c2.medanx1,intclci.c2.medanx1,intcuci.c2.medanx1,
     intcoef.c3.medanx1,intclci.c3.medanx1,intcuci.c3.medanx1,
     intcoef.c4.medanx1,intclci.c4.medanx1,intcuci.c4.medanx1,
     coef.c2.medanx1,clci.c2.medanx1,cuci.c2.medanx1,
     coef.c3.medanx1,clci.c3.medanx1,cuci.c3.medanx1,
     coef.c4.medanx1,clci.c4.medanx1,cuci.c4.medanx1, 
     agecoef.c2.medanx1,ageclci.c2.medanx1,agecuci.c2.medanx1,
     agecoef.c3.medanx1,ageclci.c3.medanx1,agecuci.c3.medanx1,
     agecoef.c4.medanx1,ageclci.c4.medanx1,agecuci.c4.medanx1, 
     sexcoef.c2.medanx1,sexclci.c2.medanx1,sexcuci.c2.medanx1,
     sexcoef.c3.medanx1,sexclci.c3.medanx1,sexcuci.c3.medanx1,
     sexcoef.c4.medanx1,sexclci.c4.medanx1,sexcuci.c4.medanx1, 
     anx_sexcoef.c2.medanx1,anx_sexclci.c2.medanx1,anx_sexcuci.c2.medanx1,
     anx_sexcoef.c3.medanx1,anx_sexclci.c3.medanx1,anx_sexcuci.c3.medanx1,
     anx_sexcoef.c4.medanx1,anx_sexclci.c4.medanx1,anx_sexcuci.c4.medanx1)
medanx1<-data.frame(x)
medanx1t<-t(medanx1)
medanx1t<-data.frame(medanx1t)
names(medanx1t)<-c("var","intor_c2","intlci_c2","intuci_c2",
                   "intor_c3","intlci_c3","intuci_c3",
                   "intor_c4","intlci_c4","intuci_c4",
                   "or_c2","lci_c2","uci_c2",
                  "or_c3","lci_c3","uci_c3",
                  "or_c4","lci_c4","uci_c4",
                  "ageor_c2","agelci_c2","ageuci_c2",
                  "ageor_c3","agelci_c3","ageuci_c3",
                  "ageor_c4","agelci_c4","ageuci_c4",
                  "sexor_c2","sexlci_c2","sexuci_c2",
                  "sexor_c3","sexlci_c3","sexuci_c3",
                  "sexor_c4","sexlci_c4","sexuci_c4",
                  "anx_sexor_c2","anx_sexlci_c2","anx_sexuci_c2",
                  "anx_sexor_c3","anx_sexlci_c3","anx_sexuci_c3",
                  "anx_sexor_c4","anx_sexlci_c4","anx_sexuci_c4")



write.csv(medanx1t,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/model1.csv",row.names = FALSE)




prediscore$sexm<-relevel(prediscore$sexm, "male")


# Model  2 adjusted by age, sex,  interaction sex anxiety, work, smoker, low physical activity


modelmedanx2<-multinom(class_f~medanx*sexm + older + work +smoker +lowpa, data = prediscore)

summary(modelmedanx2)

# Intercept

(intest.c2.medanx2<-summary(modelmedanx2)$coefficients[1])
(intest.c3.medanx2<-summary(modelmedanx2)$coefficients[2])
(intest.c4.medanx2<-summary(modelmedanx2)$coefficients[3])

(intse.c2.medanx2<-summary(modelmedanx2)$standard.errors[1])
(intse.c3.medanx2<-summary(modelmedanx2)$standard.errors[2])
(intse.c4.medanx2<-summary(modelmedanx2)$standard.errors[3])

(intlci.c2.medanx2<-intest.c2.medanx2-1.95*intse.c2.medanx2)
(intlci.c3.medanx2<-intest.c3.medanx2-1.95*intse.c3.medanx2)
(intlci.c4.medanx2<-intest.c4.medanx2-1.95*intse.c4.medanx2)

(intuci.c2.medanx2<-intest.c2.medanx2+1.95*intse.c2.medanx2)
(intuci.c3.medanx2<-intest.c3.medanx2+1.95*intse.c3.medanx2)
(intuci.c4.medanx2<-intest.c4.medanx2+1.95*intse.c4.medanx2)

(intcoef.c2.medanx2<-exp(intest.c2.medanx2))
(intclci.c2.medanx2<-exp(intlci.c2.medanx2))
(intcuci.c2.medanx2<-exp(intuci.c2.medanx2))

(intcoef.c3.medanx2<-exp(intest.c3.medanx2))
(intclci.c3.medanx2<-exp(intlci.c3.medanx2))
(intcuci.c3.medanx2<-exp(intuci.c3.medanx2))

(intcoef.c4.medanx2<-exp(intest.c4.medanx2))
(intclci.c4.medanx2<-exp(intlci.c4.medanx2))
(intcuci.c4.medanx2<-exp(intuci.c4.medanx2))

# anxiety

(est.c2.medanx2<-summary(modelmedanx2)$coefficients[4])
(est.c3.medanx2<-summary(modelmedanx2)$coefficients[5])
(est.c4.medanx2<-summary(modelmedanx2)$coefficients[6])

(se.c2.medanx2<-summary(modelmedanx2)$standard.errors[4])
(se.c3.medanx2<-summary(modelmedanx2)$standard.errors[5])
(se.c4.medanx2<-summary(modelmedanx2)$standard.errors[6])

(lci.c2.medanx2<-est.c2.medanx2-1.95*se.c2.medanx2)
(lci.c3.medanx2<-est.c3.medanx2-1.95*se.c3.medanx2)
(lci.c4.medanx2<-est.c4.medanx2-1.95*se.c4.medanx2)

(uci.c2.medanx2<-est.c2.medanx2+1.95*se.c2.medanx2)
(uci.c3.medanx2<-est.c3.medanx2+1.95*se.c3.medanx2)
(uci.c4.medanx2<-est.c4.medanx2+1.95*se.c4.medanx2)

(coef.c2.medanx2<-exp(est.c2.medanx2))
(clci.c2.medanx2<-exp(lci.c2.medanx2))
(cuci.c2.medanx2<-exp(uci.c2.medanx2))

(coef.c3.medanx2<-exp(est.c3.medanx2))
(clci.c3.medanx2<-exp(lci.c3.medanx2))
(cuci.c3.medanx2<-exp(uci.c3.medanx2))

(coef.c4.medanx2<-exp(est.c4.medanx2))
(clci.c4.medanx2<-exp(lci.c4.medanx2))
(cuci.c4.medanx2<-exp(uci.c4.medanx2))


# Sex

(sexest.c2.medanx2<-summary(modelmedanx2)$coefficients[7])
(sexest.c3.medanx2<-summary(modelmedanx2)$coefficients[8])
(sexest.c4.medanx2<-summary(modelmedanx2)$coefficients[9])

(sexse.c2.medanx2<-summary(modelmedanx2)$standard.errors[7])
(sexse.c3.medanx2<-summary(modelmedanx2)$standard.errors[8])
(sexse.c4.medanx2<-summary(modelmedanx2)$standard.errors[9])

(sexlci.c2.medanx2<-sexest.c2.medanx2-1.95*sexse.c2.medanx2)
(sexlci.c3.medanx2<-sexest.c3.medanx2-1.95*sexse.c3.medanx2)
(sexlci.c4.medanx2<-sexest.c4.medanx2-1.95*sexse.c4.medanx2)

(sexuci.c2.medanx2<-sexest.c2.medanx2+1.95*sexse.c2.medanx2)
(sexuci.c3.medanx2<-sexest.c3.medanx2+1.95*sexse.c3.medanx2)
(sexuci.c4.medanx2<-sexest.c4.medanx2+1.95*sexse.c4.medanx2)

(sexcoef.c2.medanx2<-exp(sexest.c2.medanx2))
(sexclci.c2.medanx2<-exp(sexlci.c2.medanx2))
(sexcuci.c2.medanx2<-exp(sexuci.c2.medanx2))

(sexcoef.c3.medanx2<-exp(sexest.c3.medanx2))
(sexclci.c3.medanx2<-exp(sexlci.c3.medanx2))
(sexcuci.c3.medanx2<-exp(sexuci.c3.medanx2))

(sexcoef.c4.medanx2<-exp(sexest.c4.medanx2))
(sexclci.c4.medanx2<-exp(sexlci.c4.medanx2))
(sexcuci.c4.medanx2<-exp(sexuci.c4.medanx2))

# Age

(ageest.c2.medanx2<-summary(modelmedanx2)$coefficients[10])
(ageest.c3.medanx2<-summary(modelmedanx2)$coefficients[11])
(ageest.c4.medanx2<-summary(modelmedanx2)$coefficients[12])

(agese.c2.medanx2<-summary(modelmedanx2)$standard.errors[10])
(agese.c3.medanx2<-summary(modelmedanx2)$standard.errors[11])
(agese.c4.medanx2<-summary(modelmedanx2)$standard.errors[12])

(agelci.c2.medanx2<-ageest.c2.medanx2-1.95*agese.c2.medanx2)
(agelci.c3.medanx2<-ageest.c3.medanx2-1.95*agese.c3.medanx2)
(agelci.c4.medanx2<-ageest.c4.medanx2-1.95*agese.c4.medanx2)

(ageuci.c2.medanx2<-ageest.c2.medanx2+1.95*agese.c2.medanx2)
(ageuci.c3.medanx2<-ageest.c3.medanx2+1.95*agese.c3.medanx2)
(ageuci.c4.medanx2<-ageest.c4.medanx2+1.95*agese.c4.medanx2)

(agecoef.c2.medanx2<-exp(ageest.c2.medanx2))
(ageclci.c2.medanx2<-exp(agelci.c2.medanx2))
(agecuci.c2.medanx2<-exp(ageuci.c2.medanx2))

(agecoef.c3.medanx2<-exp(ageest.c3.medanx2))
(ageclci.c3.medanx2<-exp(agelci.c3.medanx2))
(agecuci.c3.medanx2<-exp(ageuci.c3.medanx2))

(agecoef.c4.medanx2<-exp(ageest.c4.medanx2))
(ageclci.c4.medanx2<-exp(agelci.c4.medanx2))
(agecuci.c4.medanx2<-exp(ageuci.c4.medanx2))

# work

(workest.c2.medanx2<-summary(modelmedanx2)$coefficients[13])
(workest.c3.medanx2<-summary(modelmedanx2)$coefficients[14])
(workest.c4.medanx2<-summary(modelmedanx2)$coefficients[15])

(workse.c2.medanx2<-summary(modelmedanx2)$standard.errors[13])
(workse.c3.medanx2<-summary(modelmedanx2)$standard.errors[14])
(workse.c4.medanx2<-summary(modelmedanx2)$standard.errors[15])

(worklci.c2.medanx2<-workest.c2.medanx2-1.95*workse.c2.medanx2)
(worklci.c3.medanx2<-workest.c3.medanx2-1.95*workse.c3.medanx2)
(worklci.c4.medanx2<-workest.c4.medanx2-1.95*workse.c4.medanx2)

(workuci.c2.medanx2<-workest.c2.medanx2+1.95*workse.c2.medanx2)
(workuci.c3.medanx2<-workest.c3.medanx2+1.95*workse.c3.medanx2)
(workuci.c4.medanx2<-workest.c4.medanx2+1.95*workse.c4.medanx2)

(workcoef.c2.medanx2<-exp(workest.c2.medanx2))
(workclci.c2.medanx2<-exp(worklci.c2.medanx2))
(workcuci.c2.medanx2<-exp(workuci.c2.medanx2))

(workcoef.c3.medanx2<-exp(workest.c3.medanx2))
(workclci.c3.medanx2<-exp(worklci.c3.medanx2))
(workcuci.c3.medanx2<-exp(workuci.c3.medanx2))

(workcoef.c4.medanx2<-exp(workest.c4.medanx2))
(workclci.c4.medanx2<-exp(worklci.c4.medanx2))
(workcuci.c4.medanx2<-exp(workuci.c4.medanx2))

#smoker

(smokerest.c2.medanx2<-summary(modelmedanx2)$coefficients[16])
(smokerest.c3.medanx2<-summary(modelmedanx2)$coefficients[17])
(smokerest.c4.medanx2<-summary(modelmedanx2)$coefficients[18])

(smokerse.c2.medanx2<-summary(modelmedanx2)$standard.errors[16])
(smokerse.c3.medanx2<-summary(modelmedanx2)$standard.errors[17])
(smokerse.c4.medanx2<-summary(modelmedanx2)$standard.errors[18])

(smokerlci.c2.medanx2<-smokerest.c2.medanx2-1.95*smokerse.c2.medanx2)
(smokerlci.c3.medanx2<-smokerest.c3.medanx2-1.95*smokerse.c3.medanx2)
(smokerlci.c4.medanx2<-smokerest.c4.medanx2-1.95*smokerse.c4.medanx2)

(smokeruci.c2.medanx2<-smokerest.c2.medanx2+1.95*smokerse.c2.medanx2)
(smokeruci.c3.medanx2<-smokerest.c3.medanx2+1.95*smokerse.c3.medanx2)
(smokeruci.c4.medanx2<-smokerest.c4.medanx2+1.95*smokerse.c4.medanx2)

(smokercoef.c2.medanx2<-exp(smokerest.c2.medanx2))
(smokerclci.c2.medanx2<-exp(smokerlci.c2.medanx2))
(smokercuci.c2.medanx2<-exp(smokeruci.c2.medanx2))

(smokercoef.c3.medanx2<-exp(smokerest.c3.medanx2))
(smokerclci.c3.medanx2<-exp(smokerlci.c3.medanx2))
(smokercuci.c3.medanx2<-exp(smokeruci.c3.medanx2))

(smokercoef.c4.medanx2<-exp(smokerest.c4.medanx2))
(smokerclci.c4.medanx2<-exp(smokerlci.c4.medanx2))
(smokercuci.c4.medanx2<-exp(smokeruci.c4.medanx2))

# lowpa

(lowpaest.c2.medanx2<-summary(modelmedanx2)$coefficients[19])
(lowpaest.c3.medanx2<-summary(modelmedanx2)$coefficients[20])
(lowpaest.c4.medanx2<-summary(modelmedanx2)$coefficients[21])

(lowpase.c2.medanx2<-summary(modelmedanx2)$standard.errors[19])
(lowpase.c3.medanx2<-summary(modelmedanx2)$standard.errors[20])
(lowpase.c4.medanx2<-summary(modelmedanx2)$standard.errors[21])

(lowpalci.c2.medanx2<-lowpaest.c2.medanx2-1.95*lowpase.c2.medanx2)
(lowpalci.c3.medanx2<-lowpaest.c3.medanx2-1.95*lowpase.c3.medanx2)
(lowpalci.c4.medanx2<-lowpaest.c4.medanx2-1.95*lowpase.c4.medanx2)

(lowpauci.c2.medanx2<-lowpaest.c2.medanx2+1.95*lowpase.c2.medanx2)
(lowpauci.c3.medanx2<-lowpaest.c3.medanx2+1.95*lowpase.c3.medanx2)
(lowpauci.c4.medanx2<-lowpaest.c4.medanx2+1.95*lowpase.c4.medanx2)

(lowpacoef.c2.medanx2<-exp(lowpaest.c2.medanx2))
(lowpaclci.c2.medanx2<-exp(lowpalci.c2.medanx2))
(lowpacuci.c2.medanx2<-exp(lowpauci.c2.medanx2))

(lowpacoef.c3.medanx2<-exp(lowpaest.c3.medanx2))
(lowpaclci.c3.medanx2<-exp(lowpalci.c3.medanx2))
(lowpacuci.c3.medanx2<-exp(lowpauci.c3.medanx2))

(lowpacoef.c4.medanx2<-exp(lowpaest.c4.medanx2))
(lowpaclci.c4.medanx2<-exp(lowpalci.c4.medanx2))
(lowpacuci.c4.medanx2<-exp(lowpauci.c4.medanx2))


# Interaction anx-sex

(anx_sexest.c2.medanx2<-summary(modelmedanx2)$coefficients[22])
(anx_sexest.c3.medanx2<-summary(modelmedanx2)$coefficients[23])
(anx_sexest.c4.medanx2<-summary(modelmedanx2)$coefficients[24])

(anx_sexse.c2.medanx2<-summary(modelmedanx2)$standard.errors[22])
(anx_sexse.c3.medanx2<-summary(modelmedanx2)$standard.errors[23])
(anx_sexse.c4.medanx2<-summary(modelmedanx2)$standard.errors[24])

(anx_sexlci.c2.medanx2<-anx_sexest.c2.medanx2-1.95*anx_sexse.c2.medanx2)
(anx_sexlci.c3.medanx2<-anx_sexest.c3.medanx2-1.95*anx_sexse.c3.medanx2)
(anx_sexlci.c4.medanx2<-anx_sexest.c4.medanx2-1.95*anx_sexse.c4.medanx2)

(anx_sexuci.c2.medanx2<-anx_sexest.c2.medanx2+1.95*anx_sexse.c2.medanx2)
(anx_sexuci.c3.medanx2<-anx_sexest.c3.medanx2+1.95*anx_sexse.c3.medanx2)
(anx_sexuci.c4.medanx2<-anx_sexest.c4.medanx2+1.95*anx_sexse.c4.medanx2)

(anx_sexcoef.c2.medanx2<-exp(anx_sexest.c2.medanx2))
(anx_sexclci.c2.medanx2<-exp(anx_sexlci.c2.medanx2))
(anx_sexcuci.c2.medanx2<-exp(anx_sexuci.c2.medanx2))

(anx_sexcoef.c3.medanx2<-exp(anx_sexest.c3.medanx2))
(anx_sexclci.c3.medanx2<-exp(anx_sexlci.c3.medanx2))
(anx_sexcuci.c3.medanx2<-exp(anx_sexuci.c3.medanx2))

(anx_sexcoef.c4.medanx2<-exp(anx_sexest.c4.medanx2))
(anx_sexclci.c4.medanx2<-exp(anx_sexlci.c4.medanx2))
(anx_sexcuci.c4.medanx2<-exp(anx_sexuci.c4.medanx2))



x<-c("medanx2", intcoef.c2.medanx2,intclci.c2.medanx2,intcuci.c2.medanx2,
     intcoef.c3.medanx2,intclci.c3.medanx2,intcuci.c3.medanx2,
     intcoef.c4.medanx2,intclci.c4.medanx2,intcuci.c4.medanx2,
     coef.c2.medanx2,clci.c2.medanx2,cuci.c2.medanx2,
     coef.c3.medanx2,clci.c3.medanx2,cuci.c3.medanx2,
     coef.c4.medanx2,clci.c4.medanx2,cuci.c4.medanx2, 
     sexcoef.c2.medanx2,sexclci.c2.medanx2,sexcuci.c2.medanx2,
     sexcoef.c3.medanx2,sexclci.c3.medanx2,sexcuci.c3.medanx2,
     sexcoef.c4.medanx2,sexclci.c4.medanx2,sexcuci.c4.medanx2, 
     agecoef.c2.medanx2,ageclci.c2.medanx2,agecuci.c2.medanx2,
     agecoef.c3.medanx2,ageclci.c3.medanx2,agecuci.c3.medanx2,
     agecoef.c4.medanx2,ageclci.c4.medanx2,agecuci.c4.medanx2,
     workcoef.c2.medanx2,workclci.c2.medanx2,workcuci.c2.medanx2,
     workcoef.c3.medanx2,workclci.c3.medanx2,workcuci.c3.medanx2,
     workcoef.c4.medanx2,workclci.c4.medanx2,workcuci.c4.medanx2, 
     smokercoef.c2.medanx2,smokerclci.c2.medanx2,smokercuci.c2.medanx2,
     smokercoef.c3.medanx2,smokerclci.c3.medanx2,smokercuci.c3.medanx2,
     smokercoef.c4.medanx2,smokerclci.c4.medanx2,smokercuci.c4.medanx2, 
     lowpacoef.c2.medanx2,lowpaclci.c2.medanx2,lowpacuci.c2.medanx2,
     lowpacoef.c3.medanx2,lowpaclci.c3.medanx2,lowpacuci.c3.medanx2,
     lowpacoef.c4.medanx2,lowpaclci.c4.medanx2,lowpacuci.c4.medanx2, 
     anx_sexcoef.c2.medanx2,anx_sexclci.c2.medanx2,anx_sexcuci.c2.medanx2,
     anx_sexcoef.c3.medanx2,anx_sexclci.c3.medanx2,anx_sexcuci.c3.medanx2,
     anx_sexcoef.c4.medanx2,anx_sexclci.c4.medanx2,anx_sexcuci.c4.medanx2)
medanx2<-data.frame(x)
medanx2t<-t(medanx2)
medanx2t<-data.frame(medanx2t)
names(medanx2t)<-c("var","intor_c2","intlci_c2","intuci_c2",
                   "intor_c3","intlci_c3","intuci_c3",
                   "intor_c4","intlci_c4","intuci_c4",
                   "or_c2","lci_c2","uci_c2",
                   "or_c3","lci_c3","uci_c3",
                   "or_c4","lci_c4","uci_c4",
                   "sexor_c2","sexlci_c2","sexuci_c2",
                   "sexor_c3","sexlci_c3","sexuci_c3",
                   "sexor_c4","sexlci_c4","sexuci_c4",
                   "ageor_c2","agelci_c2","ageuci_c2",
                   "ageor_c3","agelci_c3","ageuci_c3",
                   "ageor_c4","agelci_c4","ageuci_c4",
                   "workor_c2","worklci_c2","workuci_c2",
                   "workor_c3","worklci_c3","workuci_c3",
                   "workor_c4","worklci_c4","workuci_c4",
                   "smokeror_c2","smokerlci_c2","smokeruci_c2",
                   "smokeror_c3","smokerlci_c3","smokeruci_c3",
                   "smokeror_c4","smokerlci_c4","smokeruci_c4",
                   "lowpaor_c2","lowpalci_c2","lowpauci_c2",
                   "lowpaor_c3","lowpalci_c3","lowpauci_c3",
                   "lowpaor_c4","lowpalci_c4","lowpauci_c4",
                   "anx_sexor_c2","anx_sexlci_c2","anx_sexuci_c2",
                   "anx_sexor_c3","anx_sexlci_c3","anx_sexuci_c3",
                   "anx_sexor_c4","anx_sexlci_c4","anx_sexuci_c4")





write.csv(medanx2t,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/model2.csv",row.names = FALSE)

# Model 3 adjusted by age, sex,  work, smoking, lowpa, medcat and nih


modelmedanx3<-multinom(class_f~medanx*sexm + older + 
                               work +smoker +lowpa +
                               medcat + NIH_classification_dic, data = prediscore)

summary(modelmedanx3)

# Intercept

(intest.c2.medanx3<-summary(modelmedanx3)$coefficients[1])
(intest.c3.medanx3<-summary(modelmedanx3)$coefficients[2])
(intest.c4.medanx3<-summary(modelmedanx3)$coefficients[3])

(intse.c2.medanx3<-summary(modelmedanx3)$standard.errors[1])
(intse.c3.medanx3<-summary(modelmedanx3)$standard.errors[2])
(intse.c4.medanx3<-summary(modelmedanx3)$standard.errors[3])

(intlci.c2.medanx3<-intest.c2.medanx3-1.95*intse.c2.medanx3)
(intlci.c3.medanx3<-intest.c3.medanx3-1.95*intse.c3.medanx3)
(intlci.c4.medanx3<-intest.c4.medanx3-1.95*intse.c4.medanx3)

(intuci.c2.medanx3<-intest.c2.medanx3+1.95*intse.c2.medanx3)
(intuci.c3.medanx3<-intest.c3.medanx3+1.95*intse.c3.medanx3)
(intuci.c4.medanx3<-intest.c4.medanx3+1.95*intse.c4.medanx3)

(intcoef.c2.medanx3<-exp(intest.c2.medanx3))
(intclci.c2.medanx3<-exp(intlci.c2.medanx3))
(intcuci.c2.medanx3<-exp(intuci.c2.medanx3))

(intcoef.c3.medanx3<-exp(intest.c3.medanx3))
(intclci.c3.medanx3<-exp(intlci.c3.medanx3))
(intcuci.c3.medanx3<-exp(intuci.c3.medanx3))

(intcoef.c4.medanx3<-exp(intest.c4.medanx3))
(intclci.c4.medanx3<-exp(intlci.c4.medanx3))
(intcuci.c4.medanx3<-exp(intuci.c4.medanx3))

# anxiety

(est.c2.medanx3<-summary(modelmedanx3)$coefficients[4])
(est.c3.medanx3<-summary(modelmedanx3)$coefficients[5])
(est.c4.medanx3<-summary(modelmedanx3)$coefficients[6])

(se.c2.medanx3<-summary(modelmedanx3)$standard.errors[4])
(se.c3.medanx3<-summary(modelmedanx3)$standard.errors[5])
(se.c4.medanx3<-summary(modelmedanx3)$standard.errors[6])

(lci.c2.medanx3<-est.c2.medanx3-1.95*se.c2.medanx3)
(lci.c3.medanx3<-est.c3.medanx3-1.95*se.c3.medanx3)
(lci.c4.medanx3<-est.c4.medanx3-1.95*se.c4.medanx3)

(uci.c2.medanx3<-est.c2.medanx3+1.95*se.c2.medanx3)
(uci.c3.medanx3<-est.c3.medanx3+1.95*se.c3.medanx3)
(uci.c4.medanx3<-est.c4.medanx3+1.95*se.c4.medanx3)

(coef.c2.medanx3<-exp(est.c2.medanx3))
(clci.c2.medanx3<-exp(lci.c2.medanx3))
(cuci.c2.medanx3<-exp(uci.c2.medanx3))

(coef.c3.medanx3<-exp(est.c3.medanx3))
(clci.c3.medanx3<-exp(lci.c3.medanx3))
(cuci.c3.medanx3<-exp(uci.c3.medanx3))

(coef.c4.medanx3<-exp(est.c4.medanx3))
(clci.c4.medanx3<-exp(lci.c4.medanx3))
(cuci.c4.medanx3<-exp(uci.c4.medanx3))


# Sex

(sexest.c2.medanx3<-summary(modelmedanx3)$coefficients[7])
(sexest.c3.medanx3<-summary(modelmedanx3)$coefficients[8])
(sexest.c4.medanx3<-summary(modelmedanx3)$coefficients[9])

(sexse.c2.medanx3<-summary(modelmedanx3)$standard.errors[7])
(sexse.c3.medanx3<-summary(modelmedanx3)$standard.errors[8])
(sexse.c4.medanx3<-summary(modelmedanx3)$standard.errors[9])

(sexlci.c2.medanx3<-sexest.c2.medanx3-1.95*sexse.c2.medanx3)
(sexlci.c3.medanx3<-sexest.c3.medanx3-1.95*sexse.c3.medanx3)
(sexlci.c4.medanx3<-sexest.c4.medanx3-1.95*sexse.c4.medanx3)

(sexuci.c2.medanx3<-sexest.c2.medanx3+1.95*sexse.c2.medanx3)
(sexuci.c3.medanx3<-sexest.c3.medanx3+1.95*sexse.c3.medanx3)
(sexuci.c4.medanx3<-sexest.c4.medanx3+1.95*sexse.c4.medanx3)

(sexcoef.c2.medanx3<-exp(sexest.c2.medanx3))
(sexclci.c2.medanx3<-exp(sexlci.c2.medanx3))
(sexcuci.c2.medanx3<-exp(sexuci.c2.medanx3))

(sexcoef.c3.medanx3<-exp(sexest.c3.medanx3))
(sexclci.c3.medanx3<-exp(sexlci.c3.medanx3))
(sexcuci.c3.medanx3<-exp(sexuci.c3.medanx3))

(sexcoef.c4.medanx3<-exp(sexest.c4.medanx3))
(sexclci.c4.medanx3<-exp(sexlci.c4.medanx3))
(sexcuci.c4.medanx3<-exp(sexuci.c4.medanx3))

# Age

(ageest.c2.medanx3<-summary(modelmedanx3)$coefficients[10])
(ageest.c3.medanx3<-summary(modelmedanx3)$coefficients[11])
(ageest.c4.medanx3<-summary(modelmedanx3)$coefficients[12])

(agese.c2.medanx3<-summary(modelmedanx3)$standard.errors[10])
(agese.c3.medanx3<-summary(modelmedanx3)$standard.errors[11])
(agese.c4.medanx3<-summary(modelmedanx3)$standard.errors[12])

(agelci.c2.medanx3<-ageest.c2.medanx3-1.95*agese.c2.medanx3)
(agelci.c3.medanx3<-ageest.c3.medanx3-1.95*agese.c3.medanx3)
(agelci.c4.medanx3<-ageest.c4.medanx3-1.95*agese.c4.medanx3)

(ageuci.c2.medanx3<-ageest.c2.medanx3+1.95*agese.c2.medanx3)
(ageuci.c3.medanx3<-ageest.c3.medanx3+1.95*agese.c3.medanx3)
(ageuci.c4.medanx3<-ageest.c4.medanx3+1.95*agese.c4.medanx3)

(agecoef.c2.medanx3<-exp(ageest.c2.medanx3))
(ageclci.c2.medanx3<-exp(agelci.c2.medanx3))
(agecuci.c2.medanx3<-exp(ageuci.c2.medanx3))

(agecoef.c3.medanx3<-exp(ageest.c3.medanx3))
(ageclci.c3.medanx3<-exp(agelci.c3.medanx3))
(agecuci.c3.medanx3<-exp(ageuci.c3.medanx3))

(agecoef.c4.medanx3<-exp(ageest.c4.medanx3))
(ageclci.c4.medanx3<-exp(agelci.c4.medanx3))
(agecuci.c4.medanx3<-exp(ageuci.c4.medanx3))

# work

(workest.c2.medanx3<-summary(modelmedanx3)$coefficients[13])
(workest.c3.medanx3<-summary(modelmedanx3)$coefficients[14])
(workest.c4.medanx3<-summary(modelmedanx3)$coefficients[15])

(workse.c2.medanx3<-summary(modelmedanx3)$standard.errors[13])
(workse.c3.medanx3<-summary(modelmedanx3)$standard.errors[14])
(workse.c4.medanx3<-summary(modelmedanx3)$standard.errors[15])

(worklci.c2.medanx3<-workest.c2.medanx3-1.95*workse.c2.medanx3)
(worklci.c3.medanx3<-workest.c3.medanx3-1.95*workse.c3.medanx3)
(worklci.c4.medanx3<-workest.c4.medanx3-1.95*workse.c4.medanx3)

(workuci.c2.medanx3<-workest.c2.medanx3+1.95*workse.c2.medanx3)
(workuci.c3.medanx3<-workest.c3.medanx3+1.95*workse.c3.medanx3)
(workuci.c4.medanx3<-workest.c4.medanx3+1.95*workse.c4.medanx3)

(workcoef.c2.medanx3<-exp(workest.c2.medanx3))
(workclci.c2.medanx3<-exp(worklci.c2.medanx3))
(workcuci.c2.medanx3<-exp(workuci.c2.medanx3))

(workcoef.c3.medanx3<-exp(workest.c3.medanx3))
(workclci.c3.medanx3<-exp(worklci.c3.medanx3))
(workcuci.c3.medanx3<-exp(workuci.c3.medanx3))

(workcoef.c4.medanx3<-exp(workest.c4.medanx3))
(workclci.c4.medanx3<-exp(worklci.c4.medanx3))
(workcuci.c4.medanx3<-exp(workuci.c4.medanx3))

#smoker

(smokerest.c2.medanx3<-summary(modelmedanx3)$coefficients[16])
(smokerest.c3.medanx3<-summary(modelmedanx3)$coefficients[17])
(smokerest.c4.medanx3<-summary(modelmedanx3)$coefficients[18])

(smokerse.c2.medanx3<-summary(modelmedanx3)$standard.errors[16])
(smokerse.c3.medanx3<-summary(modelmedanx3)$standard.errors[17])
(smokerse.c4.medanx3<-summary(modelmedanx3)$standard.errors[18])

(smokerlci.c2.medanx3<-smokerest.c2.medanx3-1.95*smokerse.c2.medanx3)
(smokerlci.c3.medanx3<-smokerest.c3.medanx3-1.95*smokerse.c3.medanx3)
(smokerlci.c4.medanx3<-smokerest.c4.medanx3-1.95*smokerse.c4.medanx3)

(smokeruci.c2.medanx3<-smokerest.c2.medanx3+1.95*smokerse.c2.medanx3)
(smokeruci.c3.medanx3<-smokerest.c3.medanx3+1.95*smokerse.c3.medanx3)
(smokeruci.c4.medanx3<-smokerest.c4.medanx3+1.95*smokerse.c4.medanx3)

(smokercoef.c2.medanx3<-exp(smokerest.c2.medanx3))
(smokerclci.c2.medanx3<-exp(smokerlci.c2.medanx3))
(smokercuci.c2.medanx3<-exp(smokeruci.c2.medanx3))

(smokercoef.c3.medanx3<-exp(smokerest.c3.medanx3))
(smokerclci.c3.medanx3<-exp(smokerlci.c3.medanx3))
(smokercuci.c3.medanx3<-exp(smokeruci.c3.medanx3))

(smokercoef.c4.medanx3<-exp(smokerest.c4.medanx3))
(smokerclci.c4.medanx3<-exp(smokerlci.c4.medanx3))
(smokercuci.c4.medanx3<-exp(smokeruci.c4.medanx3))

# lowpa

(lowpaest.c2.medanx3<-summary(modelmedanx3)$coefficients[19])
(lowpaest.c3.medanx3<-summary(modelmedanx3)$coefficients[20])
(lowpaest.c4.medanx3<-summary(modelmedanx3)$coefficients[21])

(lowpase.c2.medanx3<-summary(modelmedanx3)$standard.errors[19])
(lowpase.c3.medanx3<-summary(modelmedanx3)$standard.errors[20])
(lowpase.c4.medanx3<-summary(modelmedanx3)$standard.errors[21])

(lowpalci.c2.medanx3<-lowpaest.c2.medanx3-1.95*lowpase.c2.medanx3)
(lowpalci.c3.medanx3<-lowpaest.c3.medanx3-1.95*lowpase.c3.medanx3)
(lowpalci.c4.medanx3<-lowpaest.c4.medanx3-1.95*lowpase.c4.medanx3)

(lowpauci.c2.medanx3<-lowpaest.c2.medanx3+1.95*lowpase.c2.medanx3)
(lowpauci.c3.medanx3<-lowpaest.c3.medanx3+1.95*lowpase.c3.medanx3)
(lowpauci.c4.medanx3<-lowpaest.c4.medanx3+1.95*lowpase.c4.medanx3)

(lowpacoef.c2.medanx3<-exp(lowpaest.c2.medanx3))
(lowpaclci.c2.medanx3<-exp(lowpalci.c2.medanx3))
(lowpacuci.c2.medanx3<-exp(lowpauci.c2.medanx3))

(lowpacoef.c3.medanx3<-exp(lowpaest.c3.medanx3))
(lowpaclci.c3.medanx3<-exp(lowpalci.c3.medanx3))
(lowpacuci.c3.medanx3<-exp(lowpauci.c3.medanx3))

(lowpacoef.c4.medanx3<-exp(lowpaest.c4.medanx3))
(lowpaclci.c4.medanx3<-exp(lowpalci.c4.medanx3))
(lowpacuci.c4.medanx3<-exp(lowpauci.c4.medanx3))

# medcat

(medcatest.c2.medanx3<-summary(modelmedanx3)$coefficients[22])
(medcatest.c3.medanx3<-summary(modelmedanx3)$coefficients[23])
(medcatest.c4.medanx3<-summary(modelmedanx3)$coefficients[24])

(medcatse.c2.medanx3<-summary(modelmedanx3)$standard.errors[22])
(medcatse.c3.medanx3<-summary(modelmedanx3)$standard.errors[23])
(medcatse.c4.medanx3<-summary(modelmedanx3)$standard.errors[24])

(medcatlci.c2.medanx3<-medcatest.c2.medanx3-1.95*medcatse.c2.medanx3)
(medcatlci.c3.medanx3<-medcatest.c3.medanx3-1.95*medcatse.c3.medanx3)
(medcatlci.c4.medanx3<-medcatest.c4.medanx3-1.95*medcatse.c4.medanx3)

(medcatuci.c2.medanx3<-medcatest.c2.medanx3+1.95*medcatse.c2.medanx3)
(medcatuci.c3.medanx3<-medcatest.c3.medanx3+1.95*medcatse.c3.medanx3)
(medcatuci.c4.medanx3<-medcatest.c4.medanx3+1.95*medcatse.c4.medanx3)

(medcatcoef.c2.medanx3<-exp(medcatest.c2.medanx3))
(medcatclci.c2.medanx3<-exp(medcatlci.c2.medanx3))
(medcatcuci.c2.medanx3<-exp(medcatuci.c2.medanx3))

(medcatcoef.c3.medanx3<-exp(medcatest.c3.medanx3))
(medcatclci.c3.medanx3<-exp(medcatlci.c3.medanx3))
(medcatcuci.c3.medanx3<-exp(medcatuci.c3.medanx3))

(medcatcoef.c4.medanx3<-exp(medcatest.c4.medanx3))
(medcatclci.c4.medanx3<-exp(medcatlci.c4.medanx3))
(medcatcuci.c4.medanx3<-exp(medcatuci.c4.medanx3))

# nih

(nihest.c2.medanx3<-summary(modelmedanx3)$coefficients[25])
(nihest.c3.medanx3<-summary(modelmedanx3)$coefficients[26])
(nihest.c4.medanx3<-summary(modelmedanx3)$coefficients[27])

(nihse.c2.medanx3<-summary(modelmedanx3)$standard.errors[25])
(nihse.c3.medanx3<-summary(modelmedanx3)$standard.errors[26])
(nihse.c4.medanx3<-summary(modelmedanx3)$standard.errors[27])

(nihlci.c2.medanx3<-nihest.c2.medanx3-1.95*nihse.c2.medanx3)
(nihlci.c3.medanx3<-nihest.c3.medanx3-1.95*nihse.c3.medanx3)
(nihlci.c4.medanx3<-nihest.c4.medanx3-1.95*nihse.c4.medanx3)

(nihuci.c2.medanx3<-nihest.c2.medanx3+1.95*nihse.c2.medanx3)
(nihuci.c3.medanx3<-nihest.c3.medanx3+1.95*nihse.c3.medanx3)
(nihuci.c4.medanx3<-nihest.c4.medanx3+1.95*nihse.c4.medanx3)

(nihcoef.c2.medanx3<-exp(nihest.c2.medanx3))
(nihclci.c2.medanx3<-exp(nihlci.c2.medanx3))
(nihcuci.c2.medanx3<-exp(nihuci.c2.medanx3))

(nihcoef.c3.medanx3<-exp(nihest.c3.medanx3))
(nihclci.c3.medanx3<-exp(nihlci.c3.medanx3))
(nihcuci.c3.medanx3<-exp(nihuci.c3.medanx3))

(nihcoef.c4.medanx3<-exp(nihest.c4.medanx3))
(nihclci.c4.medanx3<-exp(nihlci.c4.medanx3))
(nihcuci.c4.medanx3<-exp(nihuci.c4.medanx3))


# Interaction anx-sex

(anx_sexest.c2.medanx3<-summary(modelmedanx3)$coefficients[28])
(anx_sexest.c3.medanx3<-summary(modelmedanx3)$coefficients[29])
(anx_sexest.c4.medanx3<-summary(modelmedanx3)$coefficients[30])

(anx_sexse.c2.medanx3<-summary(modelmedanx3)$standard.errors[28])
(anx_sexse.c3.medanx3<-summary(modelmedanx3)$standard.errors[29])
(anx_sexse.c4.medanx3<-summary(modelmedanx3)$standard.errors[30])

(anx_sexlci.c2.medanx3<-anx_sexest.c2.medanx3-1.95*anx_sexse.c2.medanx3)
(anx_sexlci.c3.medanx3<-anx_sexest.c3.medanx3-1.95*anx_sexse.c3.medanx3)
(anx_sexlci.c4.medanx3<-anx_sexest.c4.medanx3-1.95*anx_sexse.c4.medanx3)

(anx_sexuci.c2.medanx3<-anx_sexest.c2.medanx3+1.95*anx_sexse.c2.medanx3)
(anx_sexuci.c3.medanx3<-anx_sexest.c3.medanx3+1.95*anx_sexse.c3.medanx3)
(anx_sexuci.c4.medanx3<-anx_sexest.c4.medanx3+1.95*anx_sexse.c4.medanx3)

(anx_sexcoef.c2.medanx3<-exp(anx_sexest.c2.medanx3))
(anx_sexclci.c2.medanx3<-exp(anx_sexlci.c2.medanx3))
(anx_sexcuci.c2.medanx3<-exp(anx_sexuci.c2.medanx3))

(anx_sexcoef.c3.medanx3<-exp(anx_sexest.c3.medanx3))
(anx_sexclci.c3.medanx3<-exp(anx_sexlci.c3.medanx3))
(anx_sexcuci.c3.medanx3<-exp(anx_sexuci.c3.medanx3))

(anx_sexcoef.c4.medanx3<-exp(anx_sexest.c4.medanx3))
(anx_sexclci.c4.medanx3<-exp(anx_sexlci.c4.medanx3))
(anx_sexcuci.c4.medanx3<-exp(anx_sexuci.c4.medanx3))



x<-c("medanx3", intcoef.c2.medanx3,intclci.c2.medanx3,intcuci.c2.medanx3,
     intcoef.c3.medanx3,intclci.c3.medanx3,intcuci.c3.medanx3,
     intcoef.c4.medanx3,intclci.c4.medanx3,intcuci.c4.medanx3,
     coef.c2.medanx3,clci.c2.medanx3,cuci.c2.medanx3,
     coef.c3.medanx3,clci.c3.medanx3,cuci.c3.medanx3,
     coef.c4.medanx3,clci.c4.medanx3,cuci.c4.medanx3, 
     sexcoef.c2.medanx3,sexclci.c2.medanx3,sexcuci.c2.medanx3,
     sexcoef.c3.medanx3,sexclci.c3.medanx3,sexcuci.c3.medanx3,
     sexcoef.c4.medanx3,sexclci.c4.medanx3,sexcuci.c4.medanx3, 
     agecoef.c2.medanx3,ageclci.c2.medanx3,agecuci.c2.medanx3,
     agecoef.c3.medanx3,ageclci.c3.medanx3,agecuci.c3.medanx3,
     agecoef.c4.medanx3,ageclci.c4.medanx3,agecuci.c4.medanx3,
     workcoef.c2.medanx3,workclci.c2.medanx3,workcuci.c2.medanx3,
     workcoef.c3.medanx3,workclci.c3.medanx3,workcuci.c3.medanx3,
     workcoef.c4.medanx3,workclci.c4.medanx3,workcuci.c4.medanx3, 
     smokercoef.c2.medanx3,smokerclci.c2.medanx3,smokercuci.c2.medanx3,
     smokercoef.c3.medanx3,smokerclci.c3.medanx3,smokercuci.c3.medanx3,
     smokercoef.c4.medanx3,smokerclci.c4.medanx3,smokercuci.c4.medanx3, 
     lowpacoef.c2.medanx3,lowpaclci.c2.medanx3,lowpacuci.c2.medanx3,
     lowpacoef.c3.medanx3,lowpaclci.c3.medanx3,lowpacuci.c3.medanx3,
     lowpacoef.c4.medanx3,lowpaclci.c4.medanx3,lowpacuci.c4.medanx3, 
     medcatcoef.c2.medanx3,medcatclci.c2.medanx3,medcatcuci.c2.medanx3,
     medcatcoef.c3.medanx3,medcatclci.c3.medanx3,medcatcuci.c3.medanx3,
     medcatcoef.c4.medanx3,medcatclci.c4.medanx3,medcatcuci.c4.medanx3, 
     nihcoef.c2.medanx3,nihclci.c2.medanx3,nihcuci.c2.medanx3,
     nihcoef.c3.medanx3,nihclci.c3.medanx3,nihcuci.c3.medanx3,
     nihcoef.c4.medanx3,nihclci.c4.medanx3,nihcuci.c4.medanx3, 
     anx_sexcoef.c2.medanx3,anx_sexclci.c2.medanx3,anx_sexcuci.c2.medanx3,
     anx_sexcoef.c3.medanx3,anx_sexclci.c3.medanx3,anx_sexcuci.c3.medanx3,
     anx_sexcoef.c4.medanx3,anx_sexclci.c4.medanx3,anx_sexcuci.c4.medanx3)
medanx3<-data.frame(x)
medanx3t<-t(medanx3)
medanx3t<-data.frame(medanx3t)
names(medanx3t)<-c("var","intor_c2","intlci_c2","intuci_c2",
                   "intor_c3","intlci_c3","intuci_c3",
                   "intor_c4","intlci_c4","intuci_c4",
                   "or_c2","lci_c2","uci_c2",
                   "or_c3","lci_c3","uci_c3",
                   "or_c4","lci_c4","uci_c4",
                   "sexor_c2","sexlci_c2","sexuci_c2",
                   "sexor_c3","sexlci_c3","sexuci_c3",
                   "sexor_c4","sexlci_c4","sexuci_c4",
                   "ageor_c2","agelci_c2","ageuci_c2",
                   "ageor_c3","agelci_c3","ageuci_c3",
                   "ageor_c4","agelci_c4","ageuci_c4",
                   "workor_c2","worklci_c2","workuci_c2",
                   "workor_c3","worklci_c3","workuci_c3",
                   "workor_c4","worklci_c4","workuci_c4",
                   "smokeror_c2","smokerlci_c2","smokeruci_c2",
                   "smokeror_c3","smokerlci_c3","smokeruci_c3",
                   "smokeror_c4","smokerlci_c4","smokeruci_c4",
                   "lowpaor_c2","lowpalci_c2","lowpauci_c2",
                   "lowpaor_c3","lowpalci_c3","lowpauci_c3",
                   "lowpaor_c4","lowpalci_c4","lowpauci_c4",
                   "medcator_c2","medcatlci_c2","medcatuci_c2",
                   "medcator_c3","medcatlci_c3","medcatuci_c3",
                   "medcator_c4","medcatlci_c4","medcatuci_c4",
                   "nihor_c2","nihlci_c2","nihuci_c2",
                   "nihor_c3","nihlci_c3","nihuci_c3",
                   "nihor_c4","nihlci_c4","nihuci_c4",
                   "anx_sexor_c2","anx_sexlci_c2","anx_sexuci_c2",
                   "anx_sexor_c3","anx_sexlci_c3","anx_sexuci_c3",
                   "anx_sexor_c4","anx_sexlci_c4","anx_sexuci_c4")


write.csv(medanx3t,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/model3.csv",row.names = FALSE)






