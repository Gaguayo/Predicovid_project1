# Building the analysis with pooled 40 imputed datasets

newlong<-read.csv("P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/newlong.csv", stringsAsFactors = TRUE)

classes<-read.csv("P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/prediscoreclass.csv", stringsAsFactors = TRUE)



names(classes)
table(newlong$JOURn)

library(dplyr)
classesshort<-classes %>%
  select(SUBJID, JOURn, class)

class(classes$SUBJID)
classesshort$SUBJID<-as.character(classesshort$SUBJID)
classesshort$JOURn<-as.character(classesshort$JOURn)

newlong$SUBJID<-as.character(newlong$SUBJID)
newlong$JOURn<-as.character(newlong$JOURn)


newlongc<-newlong %>%
  left_join(classesshort, by = c("SUBJID","JOURn"))





newlongc$class_f<-NA
newlongc$class_f<-factor(newlongc$class,levels = c(1,2,3,4), labels = c("c1","c2","c3","c4"))
newlongc$class_f<- relevel(newlongc$class_f, ref = "c1")
table(newlongc$class_f,useNA = "ifany")


newlongc$older<-ifelse(newlongc$age_estimateyears<=49, "younger 49","older50")
newlongc$older<-factor(newlongc$older)
newlongc$older<-relevel(newlongc$older,"younger 49")

newlongc$younger<-ifelse(newlongc$age_estimateyears<=30, "younger 30","older30")
newlongc$younger<-factor(newlongc$younger)
newlongc$younger<-relevel(newlongc$younger,"older30")

median(newlongc$income, na.rm=TRUE)
table(newlongc40$income, useNA = "ifany")
lowquant<-quantile(newlongc$income, probs=c(0,0.25,0.5,0.75,1),na.rm=TRUE)[[2]]
newlongc$lowincome<-ifelse(newlongc$income<lowquant,"low","high")
newlongc$lowincome<-factor(newlongc$lowincome)
table( newlongc$lowincome, useNA = "ifany")

# Underweight
newlongc$underweight<-ifelse(newlongc$bmi<20,"underw","normalw")

# NIH severity

table(newlongc$NIH_classification_or, useNA = "ifany")

newlongc$NIH_classification_mod<-NA
newlongc$NIH_classification_mod[newlongc$NIH_classification_or=="Asymptomatic"]<-"asymp"
newlongc$NIH_classification_mod[newlongc$NIH_classification_or=="Mild illness"]<-"mild"
newlongc$NIH_classification_mod[newlongc$NIH_classification_or=="Moderate illness" |
                                  newlongc$NIH_classification_or=="Hospitalised"]<-"mod-sev"
newlongc$NIH_classification_mod<-as.factor(newlongc$NIH_classification_mod)
table(newlongc$NIH_classification_mod, useNA = "ifany")

# NIH dichotomised into asmp-mild and moderate-severe
newlongc$NIH_classification_dic<-NA
newlongc$NIH_classification_dic[newlongc$NIH_classification_or=="Asymptomatic"|
                                  newlongc$NIH_classification_or=="Mild illness"]<-"asymp_mild"
newlongc$NIH_classification_dic[newlongc$NIH_classification_or=="Moderate illness"|
                                  newlongc$NIH_classification_or=="Hospitalised"]<-"mod_sev"
newlongc$NIH_classification_dic<-as.factor(newlongc$NIH_classification_dic)
table(newlongc$NIH_classification_dic, useNA = "ifany")





library(mice)
library(mitml)

#I transform my data in a list
set.seed(6683)
datListImp <- list()
for(i in 1:40){
  datListImp[[i]] <- newlongc[newlongc$.imp== i, ]
}
head(datListImp)

class(datListImp)
length(datListImp)


#convert to a mitml object
set.seed(6683)
datListImp2<-as.mitml.list(datListImp)

# buiding the little dataset
baseline<-newlongc %>%
  filter(JOURn=="0")

# buiding the little dataset
newlongc40<-newlongc %>%
  filter(.imp==40, JOURn=="0")

dim (newlongc40)

library(nnet)




# Anxiety
(totall<-table(newlongc40$medanx)[[2]])
(perall<-round(100*prop.table(table(newlongc40$medanx)))[[2]])

(totanx<-table(newlongc40$medanx[newlongc40$sex=="male"])[[2]])
(peranx<-round(100*prop.table(table(newlongc40$medanx[newlongc40$sex=="male"])))[[2]])

(totnoanx<-table(newlongc40$medanx[newlongc40$sex=="female"])[[2]])
(pernoanx<-round(100*prop.table(table(newlongc40$medanx[newlongc40$sex=="female"])))[[2]])


(p<-chisq.test(newlongc40$medanx,newlongc40$sexm))
(pvalue<-p$p.value)

x<-c("anxiety",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
anxiety<-data.frame(x)
anxietyt<-t(anxiety)
anxietyt<-data.frame(anxietyt)
names(anxietyt)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
               "pvalue")


# Anxiety distribution

newlongc40$medanx
library(ggplot2)

ggplot(newlongc40, aes(x=medanx, fill=sexm))+
  geom_bar() 

# Age

(totall<-round(mean(newlongc40$age_estimateyears),1))
(perall<-round(sd(newlongc40$age_estimateyears),1))

(totanx<-round(mean(newlongc40$age_estimateyears[newlongc40$sex=="male"]),1))
(peranx<-round(sd(newlongc40$age_estimateyears[newlongc40$sex=="male"]),1))

(totnoanx<-round(mean(newlongc40$age_estimateyears[newlongc40$sex=="female"]),1))
(pernoanx<-round(sd(newlongc40$age_estimateyears[newlongc40$sex=="female"]),1))

(pvalue<-summary(aov(newlongc40$age_estimateyears~newlongc40$medanx))[[1]][[5]][[1]])

x<-c("age",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
age<-data.frame(x)
aget<-t(age)
aget<-data.frame(aget)
names(aget)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
               "pvalue")

# bmi
round(mean(newlongc40$bmi),1)
round(sd(newlongc40$bmi),1)

(totall<-round(mean(newlongc40$bmi),1))
(perall<-round(sd(newlongc40$bmi),1))

(totanx<-round(mean(newlongc40$bmi[newlongc40$sex=="male"]),1))
(peranx<-round(sd(newlongc40$bmi[newlongc40$sex=="male"]),1))

(totnoanx<-round(mean(newlongc40$bmi[newlongc40$sex=="female"]),1))
(pernoanx<-round(sd(newlongc40$bmi[newlongc40$sex=="female"]),1))

(pvalue<-summary(aov(newlongc40$bmi~newlongc40$medanx))[[1]][[5]][[1]])

x<-c("bmi",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
bmi<-data.frame(x)
bmit<-t(bmi)
bmit<-data.frame(bmit)
names(bmit)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
               "pvalue")

# Physical activity

(totall<-round(mean(newlongc40$sum_all_activities),1))
(perall<-round(sd(newlongc40$sum_all_activities),1))

(totanx<-round(mean(newlongc40$sum_all_activities[newlongc40$sex=="male"]),1))
(peranx<-round(sd(newlongc40$sum_all_activities[newlongc40$sex=="male"]),1))

(totnoanx<-round(mean(newlongc40$sum_all_activities[newlongc40$sex=="female"]),1))
(pernoanx<-round(sd(newlongc40$sum_all_activities[newlongc40$sex=="female"]),1))

(pvalue<-summary(aov(newlongc40$sum_all_activities~newlongc40$medanx))[[1]][[5]][[1]])

x<-c("phactivity",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
phactivity<-data.frame(x)
phactivityt<-t(phactivity)
phactivityt<-data.frame(phactivityt)
names(phactivityt)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
               "pvalue")


newlongc40$younger<-ifelse(newlongc40$age_estimateyears<=30, "younger 30","older30")
newlongc40$younger<-factor(newlongc40$younger)
newlongc40$younger<-relevel(newlongc40$younger,"older30")


(totall<-table(newlongc40$younger)[[1]])
(perall<-round(100*prop.table(table(newlongc40$younger)))[[1]])

(totanx<-table(newlongc40$younger[newlongc40$sex=="male"])[[1]])
(peranx<-round(100*prop.table(table(newlongc40$younger[newlongc40$sex=="male"])))[[1]])


(p<-chisq.test(newlongc40$younger,newlongc40$sexm))
(pvalue<-p$p.value)

x<-c("younger",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
younger<-data.frame(x)
youngert<-t(younger)
youngert<-data.frame(youngert)
names(youngert)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                   "pvalue")


all<-rbind(anxietyt,youngert)

# Older
newlongc40$older<-ifelse(newlongc40$age_estimateyears<=49, "younger 49","older50")
newlongc40$older<-factor(newlongc40$older)
newlongc40$older<-relevel(newlongc40$older,"younger 49")

(totall<-table(newlongc40$older)[[2]])
(perall<-round(100*prop.table(table(newlongc40$older)))[[2]])

(totanx<-table(newlongc40$older[newlongc40$sex=="male"])[[2]])
(peranx<-round(100*prop.table(table(newlongc40$older[newlongc40$sex=="male"])))[[2]])

(totnoanx<-table(newlongc40$older[newlongc40$sex=="female"])[[2]])
(pernoanx<-round(100*prop.table(table(newlongc40$older[newlongc40$sex=="female"])))[[2]])

(p<-chisq.test(newlongc40$older,newlongc40$sexm))
(pvalue<-p$p.value)

x<-c("older",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
older<-data.frame(x)
oldert<-t(older)
oldert<-data.frame(oldert)
names(oldert)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                 "pvalue")

all<-rbind(anxietyt,youngert, oldert)

# Education QP4
summary(newlongc40$loweduc)

totall<-table(newlongc40$loweduc)[[1]]
perall<-round(100*prop.table(table(newlongc40$loweduc)))[[1]]

totanx<-table(newlongc40$loweduc[newlongc40$sex=="male"])[[1]]
peranx<-round(100*prop.table(table(newlongc40$loweduc[newlongc40$sex=="male"])))[[1]]

totnoanx<-table(newlongc40$loweduc[newlongc40$sex=="female"])[[1]]
pernoanx<-round(100*prop.table(table(newlongc40$loweduc[newlongc40$sex=="female"])))[[1]]


p<-chisq.test(newlongc40$loweduc,newlongc40$medanx)
pvalue<-p$p.value

x<-c("low_education",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
low_education<-data.frame(x)
low_educationt<-t(low_education)
low_educationt<-data.frame(low_educationt)
names(low_educationt)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                         "pvalue")

all<-rbind(anxietyt,youngert, oldert,low_educationt)

# Income 
median(newlongc40$income, na.rm=TRUE)
lowquant<-quantile(newlongc40$income, probs=c(0,0.25,0.5,0.75,1),na.rm=TRUE)[[2]]
newlongc40$lowincome<-ifelse(newlongc40$income<lowquant,"low","high")
newlongc40$lowincome<-factor(newlongc40$lowincome)
table( newlongc40$lowincome, useNA = "ifany")

totall<-table(newlongc40$lowincome)[[2]]
perall<-round(100*prop.table(table(newlongc40$lowincome)))[[2]]

totanx<-table(newlongc40$lowincome[newlongc40$sex=="male"])[[2]]
peranx<-round(100*prop.table(table(newlongc40$lowincome[newlongc40$sex=="male"])))[[2]]

totnoanx<-table(newlongc40$lowincome[newlongc40$sex=="female"])[[2]]
pernoanx<-round(100*prop.table(table(newlongc40$lowincome[newlongc40$sex=="female"])))[[2]]


p<-chisq.test(newlongc40$lowincome,newlongc40$medanx)
pvalue<-p$p.value

x<-c("low_income",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
low_income<-data.frame(x)
low_incomet<-t(low_income)
low_incomet<-data.frame(low_incomet)
names(low_incomet)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                      "pvalue")

all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet)

# work 

(totall<-table(newlongc40$work)[[1]])
(perall<-round(100*prop.table(table(newlongc40$work)))[[1]])

totanx<-table(newlongc40$work[newlongc40$sex=="male"])[[1]]
peranx<-round(100*prop.table(table(newlongc40$work[newlongc40$sex=="male"])))[[1]]

totnoanx<-table(newlongc40$work[newlongc40$sex=="female"])[[1]]
pernoanx<-round(100*prop.table(table(newlongc40$work[newlongc40$sex=="female"])))[[1]]


p<-chisq.test(newlongc40$work,newlongc40$medanx)
pvalue<-p$p.value

x<-c("no work",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
no_work<-data.frame(x)
no_workt<-t(no_work)
no_workt<-data.frame(no_workt)
names(no_workt)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                   "pvalue")

all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet,no_workt)



# owner 

totall<-table(newlongc40$owner)[[1]]
perall<-round(100*prop.table(table(newlongc40$owner)))[[1]]

totanx<-table(newlongc40$owner[newlongc40$sex=="male"])[[1]]
peranx<-round(100*prop.table(table(newlongc40$owner[newlongc40$sex=="male"])))[[1]]

totnoanx<-table(newlongc40$owner[newlongc40$sex=="female"])[[1]]
pernoanx<-round(100*prop.table(table(newlongc40$owner[newlongc40$sex=="female"])))[[1]]



p<-chisq.test(newlongc40$owner,newlongc40$medanx)
pvalue<-p$p.value

x<-c("no owner",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
no_owner<-data.frame(x)
no_ownert<-t(no_owner)
no_ownert<-data.frame(no_ownert)
names(no_ownert)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                    "pvalue")

all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet,no_workt, no_ownert)


# Obese


totall<-table(newlongc40$obese)[[2]]
perall<-round(100*prop.table(table(newlongc40$obese)))[[2]]

totanx<-table(newlongc40$obese[newlongc40$sex=="male"])[[2]]
peranx<-round(100*prop.table(table(newlongc40$obese[newlongc40$sex=="male"])))[[2]]

totnoanx<-table(newlongc40$obese[newlongc40$sex=="female"])[[2]]
pernoanx<-round(100*prop.table(table(newlongc40$obese[newlongc40$sex=="female"])))[[2]]


p<-chisq.test(newlongc40$obese,newlongc40$medanx)
pvalue<-p$p.value

x<-c("obese",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
obese<-data.frame(x)
obeset<-t(obese)
obeset<-data.frame(obeset)
names(obeset)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                 "pvalue")

all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet,no_workt, no_ownert, obeset)

# Underweight



totall<-table(newlongc40$underweight)[[2]]
perall<-round(100*prop.table(table(newlongc40$underweight)))[[2]]

totanx<-table(newlongc40$underweight[newlongc40$sex=="male"])[[2]]
peranx<-round(100*prop.table(table(newlongc40$underweight[newlongc40$sex=="male"])))[[2]]

totnoanx<-table(newlongc40$underweight[newlongc40$sex=="female"])[[2]]
pernoanx<-round(100*prop.table(table(newlongc40$underweight[newlongc40$sex=="female"])))[[2]]

p<-chisq.test(newlongc40$underweight,newlongc40$medanx)
pvalue<-p$p.value

x<-c("underweight",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
underweight<-data.frame(x)
underweightt<-t(underweight)
underweightt<-data.frame(underweightt)
names(underweightt)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                       "pvalue")

all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet,no_workt, no_ownert, obeset, underweightt)

# Physical activity before Covid lowpa

totall<-table(newlongc40$lowpa)[[1]]
perall<-round(100*prop.table(table(newlongc40$lowpa)))[[1]]

totanx<-table(newlongc40$lowpa[newlongc40$sex=="male"])[[1]]
peranx<-round(100*prop.table(table(newlongc40$lowpa[newlongc40$sex=="male"])))[[1]]

totnoanx<-table(newlongc40$lowpa[newlongc40$sex=="female"])[[1]]
pernoanx<-round(100*prop.table(table(newlongc40$lowpa[newlongc40$sex=="female"])))[[1]]

p<-chisq.test(newlongc40$lowpa,newlongc40$medanx)
pvalue<-p$p.value

x<-c("lowpa",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
lowpa<-data.frame(x)
lowpat<-t(lowpa)
lowpat<-data.frame(lowpat)
names(lowpat)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                 "pvalue")

all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet,no_workt, no_ownert, obeset, underweightt, lowpat)



# smoking state 

totall<-table(newlongc40$smoker)[[2]]
perall<-round(100*prop.table(table(newlongc40$smoker)))[[2]]

totanx<-table(newlongc40$smoker[newlongc40$sex=="male"])[[2]]
peranx<-round(100*prop.table(table(newlongc40$smoker[newlongc40$sex=="male"])))[[2]]

totnoanx<-table(newlongc40$smoker[newlongc40$sex=="female"])[[2]]
pernoanx<-round(100*prop.table(table(newlongc40$smoker[newlongc40$sex=="female"])))[[2]]

p<-chisq.test(newlongc40$smoker,newlongc40$medanx)
pvalue<-p$p.value

x<-c("smoker",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
smoker<-data.frame(x)
smokert<-t(smoker)
smokert<-data.frame(smokert)
names(smokert)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                  "pvalue")

all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet,no_workt, no_ownert, obeset, underweightt, lowpat,smokert)




# Blood type agroup

totall<-table(newlongc40$agroup)[[1]]
perall<-round(100*prop.table(table(newlongc40$agroup)))[[1]]

totanx<-table(newlongc40$agroup[newlongc40$sex=="male"])[[1]]
peranx<-round(100*prop.table(table(newlongc40$agroup[newlongc40$sex=="male"])))[[1]]

totnoanx<-table(newlongc40$agroup[newlongc40$sex=="female"])[[1]]
pernoanx<-round(100*prop.table(table(newlongc40$agroup[newlongc40$sex=="female"])))[[1]]

p<-chisq.test(newlongc40$agroup,newlongc40$medanx)
pvalue<-p$p.value

x<-c("agroup",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
agroup<-data.frame(x)
agroupt<-t(agroup)
agroupt<-data.frame(agroupt)
names(agroupt)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                  "pvalue")

all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet,no_workt, no_ownert, obeset, 
           underweightt, lowpat,smokert, agroupt)


# # Medications for anxiety or sleep medanx
# 
# totall<-table(newlongc40$medanx)[[2]]
# perall<-round(100*prop.table(table(newlongc40$medanx)))[[2]]
# 
# totanx<-table(newlongc40$medanx[newlongc40$sex=="male"])[[2]]
# peranx<-round(100*prop.table(table(newlongc40$medanx[newlongc40$sex=="male"])))[[2]]
# 
# totnoanx<-table(newlongc40$medanx[newlongc40$sex=="female"])[[2]]
# pernoanx<-round(100*prop.table(table(newlongc40$medanx[newlongc40$sex=="female"])))[[2]]
# 
# p<-chisq.test(newlongc40$medanx,newlongc40$medanx)
# pvalue<-p$p.value
# 
# x<-c("medanx",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
# medanx<-data.frame(x)
# medanxt<-t(medanx)
# medanxt<-data.frame(medanxt)
# names(medanxt)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
#                   "pvalue")
# 
# all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet,no_workt, no_ownert, obeset, 
#            underweightt, lowpat,smokert, agroupt,)


# Diabetes



totall<-table(newlongc40$diabetes)[[2]]
perall<-round(100*prop.table(table(newlongc40$diabetes)))[[2]]

totanx<-table(newlongc40$diabetes[newlongc40$sex=="male"])[[2]]
peranx<-round(100*prop.table(table(newlongc40$diabetes[newlongc40$sex=="male"])))[[2]]

totnoanx<-table(newlongc40$diabetes[newlongc40$sex=="female"])[[2]]
pernoanx<-round(100*prop.table(table(newlongc40$diabetes[newlongc40$sex=="female"])))[[2]]


p<-chisq.test(newlongc40$diabetes,newlongc40$medanx)
pvalue<-p$p.value

x<-c("diabetes",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
diabetes<-data.frame(x)
diabetest<-t(diabetes)
diabetest<-data.frame(diabetest)
names(diabetest)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                    "pvalue")


all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet,no_workt, no_ownert, obeset, 
           underweightt, lowpat,smokert, agroupt, diabetest)

# asthma
newlongc40$asthma_mhynmod<-NA
newlongc40$asthma_mhynmod<-factor(newlongc40$asthma_mhyn, levels = c(1,0), labels = c("yes","no"))

table(newlongc40$asthma_mhynmod)

totall<-table(newlongc40$asthma_mhynmod)[[1]]
perall<-round(100*prop.table(table(newlongc40$asthma_mhynmod)))[[1]]

totanx<-table(newlongc40$asthma_mhynmod[newlongc40$sex=="male"])[[1]]
peranx<-round(100*prop.table(table(newlongc40$asthma_mhynmod[newlongc40$sex=="male"])))[[1]]

totnoanx<-table(newlongc40$asthma_mhynmod[newlongc40$sex=="female"])[[1]]
pernoanx<-round(100*prop.table(table(newlongc40$asthma_mhynmod[newlongc40$sex=="female"])))[[1]]


p<-chisq.test(newlongc40$asthma_mhynmod,newlongc40$medanx)
pvalue<-p$p.value

x<-c("asthma_mhynmod",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
asthma_mhynmod<-data.frame(x)
asthma_mhynmodt<-t(asthma_mhynmod)
asthma_mhynmodt<-data.frame(asthma_mhynmodt)
names(asthma_mhynmodt)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                          "pvalue")


all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet,no_workt, no_ownert, obeset, 
           underweightt, lowpat,smokert, agroupt, diabetest, asthma_mhynmodt)

# Multimorbidity

totall<-table(newlongc40$multimorb)[[2]]
perall<-round(100*prop.table(table(newlongc40$multimorb)))[[2]]

totanx<-table(newlongc40$multimorb[newlongc40$sex=="male"])[[2]]
peranx<-round(100*prop.table(table(newlongc40$multimorb[newlongc40$sex=="male"])))[[2]]

totnoanx<-table(newlongc40$multimorb[newlongc40$sex=="female"])[[2]]
pernoanx<-round(100*prop.table(table(newlongc40$multimorb[newlongc40$sex=="female"])))[[2]]

p<-chisq.test(newlongc40$multimorb,newlongc40$medanx)
pvalue<-p$p.value

x<-c("multimorb",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
multimorb<-data.frame(x)
multimorbt<-t(multimorb)
multimorbt<-data.frame(multimorbt)
names(multimorbt)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                     "pvalue")

all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet,no_workt, no_ownert, obeset, 
           underweightt, lowpat,smokert, agroupt, diabetest, asthma_mhynmodt,multimorbt)



# weight loss

totall<-table(newlongc40$weight_loss)[[2]]
perall<-round(100*prop.table(table(newlongc40$weight_loss)))[[2]]

totanx<-table(newlongc40$weight_loss[newlongc40$sex=="male"])[[2]]
peranx<-round(100*prop.table(table(newlongc40$weight_loss[newlongc40$sex=="male"])))[[2]]

totnoanx<-table(newlongc40$weight_loss[newlongc40$sex=="female"])[[2]]
pernoanx<-round(100*prop.table(table(newlongc40$weight_loss[newlongc40$sex=="female"])))[[2]]

p<-chisq.test(newlongc40$weight_loss,newlongc40$medanx)
pvalue<-p$p.value

x<-c("weight_loss",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
weight_loss<-data.frame(x)
weight_losst<-t(weight_loss)
weight_losst<-data.frame(weight_losst)
names(weight_losst)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                       "pvalue")

all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet,no_workt, no_ownert, obeset, 
           underweightt, lowpat,smokert, agroupt, diabetest, asthma_mhynmodt,multimorbt, 
           weight_losst)

# Polypharmacy

(totall<-table(newlongc40$medcat)[[2]])
(perall<-round(100*prop.table(table(newlongc40$medcat)))[[2]])

(totanx<-table(newlongc40$medcat[newlongc40$sex=="male"])[[2]])
(peranx<-round(100*prop.table(table(newlongc40$medcat[newlongc40$sex=="male"])))[[2]])

(totnoanx<-table(newlongc40$medcat[newlongc40$sex=="female"])[[2]])
(pernoanx<-round(100*prop.table(table(newlongc40$medcat[newlongc40$sex=="female"])))[[2]])

(p<-chisq.test(newlongc40$medcat,newlongc40$sexm))
pvalue<-p$p.value

x<-c("medcat",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
medcat<-data.frame(x)
medcatt<-t(medcat)
medcatt<-data.frame(medcatt)
names(medcatt)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                  "pvalue")

all<-rbind(anxietyt,youngert, oldert,low_educationt,low_incomet,no_workt, no_ownert, obeset, 
           underweightt, lowpat,smokert, agroupt, diabetest, asthma_mhynmodt,
           multimorbt, weight_losst,medcatt)





# glasses always

(totall<-table(newlongc40$eyeglass)[[1]])
(perall<-round(100*prop.table(table(newlongc40$eyeglass)))[[1]])

(totanx<-table(newlongc40$eyeglass[newlongc40$sex=="male"])[[1]])
(peranx<-round(100*prop.table(table(newlongc40$eyeglass[newlongc40$sex=="male"])))[[1]])

(totnoanx<-table(newlongc40$eyeglass[newlongc40$sex=="female"])[[1]])
(pernoanx<-round(100*prop.table(table(newlongc40$eyeglass[newlongc40$sex=="female"])))[[1]])


(p<-chisq.test(newlongc40$eyeglass,newlongc40$sexm))
pvalue<-p$p.value

x<-c("eyeglassa",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
eyeglassa<-data.frame(x)
eyeglassat<-t(eyeglassa)
eyeglassat<-data.frame(eyeglassat)
names(eyeglassat)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                    "pvalue")




# glassessometimes

(totall<-table(newlongc40$eyeglass)[[3]])
(perall<-round(100*prop.table(table(newlongc40$eyeglass)))[[3]])

(totanx<-table(newlongc40$eyeglass[newlongc40$sex=="male"])[[3]])
(peranx<-round(100*prop.table(table(newlongc40$eyeglass[newlongc40$sex=="male"])))[[3]])

(totnoanx<-table(newlongc40$eyeglass[newlongc40$sex=="female"])[[3]])
(pernoanx<-round(100*prop.table(table(newlongc40$eyeglass[newlongc40$sex=="female"])))[[3]])


(p<-chisq.test(newlongc40$eyeglass,newlongc40$sexm))
pvalue<-p$p.value

x<-c("eyeglasss",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
eyeglasss<-data.frame(x)
eyeglassst<-t(eyeglasss)
eyeglassst<-data.frame(eyeglassst)
names(eyeglassst)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                    "pvalue")
# 
# all<-rbind(anxietyt,aget,low_educationt,low_incomet,no_workt, eyeglassat,eyeglassst,bmit,obeset, 
#            phactivityt, lowpat,smokert, agroupt, diabetest, 
#            multimorbt, weight_losst,medcatt, asympt,mildt,moderatet,severet)

# nih


(totall<-table(newlongc40$NIH_classification_dic)[[2]])
(perall<-round(100*prop.table(table(newlongc40$NIH_classification_dic)))[[2]])
# 
# (totanx<-table(newlongc40$NIH_classification_dic[newlongc40$sex=="male"])[[2]])
# (peranx<-round(100*prop.table(table(newlongc40$NIH_classification_dic[newlongc40$sex=="male"])))[[2]])
# 
# (totnoanx<-table(newlongc40$NIH_classification_dic[newlongc40$sex=="female"])[[2]])
# (pernoanx<-round(100*prop.table(table(newlongc40$NIH_classification_dic[newlongc40$sex=="female"])))[[2]])
# 
# (p<-chisq.test(newlongc40$NIH_classification_dic,newlongc40$sexm))
# (pvalue<-p$p.value)

# x<-c("NIH_classification_dic",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
# NIH_classification_dic<-data.frame(x)
# NIH_classification_dict<-t(NIH_classification_dic)
# NIH_classification_dict<-data.frame(NIH_classification_dict)
# names(NIH_classification_dict)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
#                   "pvalue")


all<-rbind(anxietyt,aget,oldert,low_educationt,low_incomet,no_workt, eyeglassat,eyeglassst,bmit,obeset, underweightt,
           phactivityt, lowpat,smokert, agroupt, diabetest, 
           multimorbt, weight_losst,medcatt, NIH_classification_dict)

# score

(totall<-round(mean(newlongc40$score,na.rm=TRUE),1))
(perall<-round(sd(newlongc40$score,na.rm=TRUE),1))

(totanx<-round(mean(newlongc40$score[newlongc40$sex=="male"],na.rm=TRUE),1))
(peranx<-round(sd(newlongc40$score[newlongc40$sex=="male"],na.rm=TRUE),1))

(totnoanx<-round(mean(newlongc40$score[newlongc40$sex=="female"],na.rm=TRUE),1))
(pernoanx<-round(sd(newlongc40$score[newlongc40$sex=="female"],na.rm=TRUE),1))

(pvalue<-summary(aov(newlongc40$score~newlongc40$medanx))[[1]][[5]][[1]])

x<-c("score",totall,perall,totanx,peranx,totnoanx,pernoanx,pvalue)
score<-data.frame(x)
scoret<-t(score)
scoret<-data.frame(scoret)
names(scoret)<-c("var","totall","perall","totmen","permen","totwomen","perwomennx",
                 "pvalue")


all<-rbind(anxietyt,aget,oldert,low_educationt,low_incomet,no_workt, eyeglassat,eyeglassst,bmit,obeset, underweightt,
           phactivityt, lowpat,smokert, agroupt, diabetest, 
           multimorbt, weight_losst,medcatt, scoret)




write.csv(all,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/suppltable2_all_by sex.csv",row.names = FALSE)



