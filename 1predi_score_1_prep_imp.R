# PREDICOVID new analysis latent class with a score of symptoms
Sys.setenv(LANG = "en")

# fatig 1 questionnaires weekend
# fatig 2 is classification
# fatig 3 is baseline
# fatig 4 is daily questionnaires for Predicovid
# fatig 5 is Predicovid_H, "questionnaire journalier", it seems it is only missing data.
# fatig 6 is monthly questionnaire
# fatig 7 is Predicovid, questionnaire permanent
# fatig 8 is Predicovid_H is questionnaire permanent, with plenty of missing data
# fatig 9 is Predicovid questionnaire semaine

# Downloading data
fatig2<-read.csv("P:/Documents/COVID19/PREDICOVID/Fatigue/data/2id_classification_july_hosp.csv")#suffix="_class"
fatig3<-read.csv("P:/Documents/COVID19/PREDICOVID/Fatigue/data/3PREDICOVID-GloriaAguayo09JUL202_DATA_2021-07-12_1139.csv")#suffix= "_base"  
fatig4<-read.csv("P:/Documents/COVID19/PREDICOVID/Fatigue/data/4QUESTIONNAIRE_J_PREDI_20210709_152051.csv")#suffix="_Jquest"
# fatig7<-read.csv("P:/Documents/COVID19/PREDICOVID/Fatigue/data/7QUESTIONNAIRE_PERMANENTPREDI_20210709_152051.csv")#suffix="_permq" Permanent questionnaires many missing
eyeglasses<-read.csv("P:/Documents/COVID19/PREDICOVID/Fatigue/data/PREDICOVID_lunettes_m.csv")

# Eyeglasses has partcipants with two informations: response and NA. I will delete NA
library(dplyr)
eyeglassesm<-eyeglasses %>%
  filter(!is.na(eyeglass))



fatig7<-read.csv("P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/fatig7m.csv")


names(fatig3)

fatig3m<-fatig3%>%
  filter(cohort_type==1)




fatig<-fatig3m %>%
  left_join(fatig2, by=c("subjid"="Subject_ID"))%>%
  left_join(fatig4, by=c("subjid"="SUBJID1"))%>%
  left_join(fatig7, by="SUBJID")%>%
  left_join(eyeglassesm, by="SUBJID_1")

fatig0<-fatig%>%
  filter(JOUR=="J0")


table(fatig$JOUR, useNA = "ifany")

# Then, I exclude those that did not participate in the daily questionnaires
fatig<-fatig%>%
  filter(!is.na(fatig$JOUR))

fatig0<-fatig%>%
  filter(JOUR=="J0")

table(fatig$JOUR, useNA = "ifany")

# Now I have 913 participants



fatig<-fatig %>%
  filter(age_estimateyears>=18)

fatig0<-fatig%>%
  filter(JOUR=="J0")


fatigm<-fatig%>%
  select(SUBJID,SUBJID_1,subjid)%>%
  filter(is.na(SUBJID))

fatig0<-fatig%>%
  filter(JOUR=="J0")


fatig0<-fatig%>%
  filter(JOUR=="J0")

summary(fatig$SUBJID)

fatig$SUBJID[fatig$subjid=="01-PREDI-0057"]<-57
fatig$SUBJID_1[fatig$subjid=="01-PREDI-0057"]<-57

fatig$SUBJID[fatig$subjid=="01-PREDI-0181"]<-181
fatig$SUBJID_1[fatig$subjid=="01-PREDI-0181"]<-181

fatig$SUBJID[fatig$subjid=="01-PREDI-0362"]<-362
fatig$SUBJID_1[fatig$subjid=="01-PREDI-0362"]<-362

fatig$SUBJID[fatig$subjid=="01-PREDI-0760"]<-760
fatig$SUBJID_1[fatig$subjid=="01-PREDI-0760"]<-760

fatig$SUBJID[fatig$subjid=="01-PREDI-0762"]<-762
fatig$SUBJID_1[fatig$subjid=="01-PREDI-0762"]<-762

fatig$SUBJID[fatig$subjid=="01-PREDI-0890"]<-890
fatig$SUBJID_1[fatig$subjid=="01-PREDI-0890"]<-890

fatig$SUBJID[fatig$subjid=="01-PREDI-0980"]<-980
fatig$SUBJID_1[fatig$subjid=="	 01-PREDI-0980"]<-980

fatig$SUBJID[fatig$subjid=="01-predi-1058"]<-1058
fatig$SUBJID_1[fatig$subjid=="	 01-predi-1058"]<-1058

fatig$SUBJID[fatig$subjid=="01-PREDI-1105"]<-1105
fatig$SUBJID_1[fatig$subjid=="01-PREDI-1105"]<-1105

fatig$SUBJID[fatig$subjid=="01-PREDI-1311"]<-1311
fatig$SUBJID_1[fatig$subjid=="	 01-PREDI-1311"]<-1311

fatig$SUBJID[fatig$subjid=="01-PREDI-1319"]<-1319
fatig$SUBJID_1[fatig$subjid=="01-PREDI-1319"]<-1319

fatig$SUBJID[fatig$subjid=="01-PREDI-1336"]<-1336
fatig$SUBJID_1[fatig$subjid=="01-PREDI-1336"]<-1336

fatig$SUBJID[fatig$subjid=="01-PREDI-1457"]<-1457
fatig$SUBJID_1[fatig$subjid=="01-PREDI-1457"]<-1457

fatig$SUBJID[fatig$subjid=="01-PREDI-1810"]<-1810
fatig$SUBJID_1[fatig$subjid=="01-PREDI-1810"]<-1810

fatig$SUBJID[fatig$subjid=="01-PREDI-1879"]<-1879
fatig$SUBJID_1[fatig$subjid=="01-PREDI-1879"]<-1879

fatig$SUBJID[fatig$subjid=="01-PREDI-2070"]<-2070
fatig$SUBJID_1[fatig$subjid=="01-PREDI-2070"]<-2070

fatig$SUBJID[fatig$subjid=="01-PREDI-2078"]<-2078
fatig$SUBJID_1[fatig$subjid=="01-PREDI-2078"]<-2078

fatig$SUBJID[fatig$subjid=="01-PREDI-2081"]<-2081
fatig$SUBJID_1[fatig$subjid=="01-PREDI-2081"]<-2081

fatig$SUBJID[fatig$subjid=="01-PREDI-2182"]<-2182
fatig$SUBJID_1[fatig$subjid=="01-PREDI-2182"]<-2182

fatig$SUBJID[fatig$subjid=="01-PREDI-2359"]<-2359
fatig$SUBJID_1[fatig$subjid=="01-PREDI-2359"]<-2359

fatig$SUBJID[fatig$subjid=="01-PREDI-2438"]<-2438
fatig$SUBJID_1[fatig$subjid=="01-PREDI-2438"]<-2438

fatig$SUBJID[fatig$subjid=="01-PREDI-2470"]<-2470
fatig$SUBJID_1[fatig$subjid=="01-PREDI-2470"]<-2470

fatig$SUBJID[fatig$subjid=="01-PREDI-2495"]<-2495
fatig$SUBJID_1[fatig$subjid=="01-PREDI-2495"]<-2495

fatig$SUBJID[fatig$subjid=="01-PREDI-2506"]<-2506
fatig$SUBJID_1[fatig$subjid=="01-PREDI-2506"]<-2506

fatig$SUBJID[fatig$subjid=="01-PREDI-2613"]<-2613
fatig$SUBJID_1[fatig$subjid=="01-PREDI-2613"]<-2613

fatigm<-fatig%>%
  select(SUBJID,SUBJID_1,subjid)

table(fatig$JOUR)

fatig0<-fatig%>%
  filter(JOUR=="J0")




# I define empty cases as missing and fix problems of decimals
table(fatig$temp_myhn, useNA = "ifany")
fatig$temp_myhn[which(fatig$temp_myhn == "/")] = NA
fatig$temp_myhn[which(fatig$temp_myhn == "")] = NA
fatig$temp_myhn[which(fatig$temp_myhn == "37,0")] = 37.0
fatig$temp_myhn[which(fatig$temp_myhn == "37,2")] = 37.2
fatig$temp_myhn[which(fatig$temp_myhn == "37,8")] = 37.8
fatig$temp_myhn[which(fatig$temp_myhn == "37.8-38")] = 37.9
fatig$temp_myhn[which(fatig$temp_myhn == "38-39")] = 38.5
fatig$temp_myhn[which(fatig$temp_myhn == "38,0")] = 38.0
fatig$temp_myhn[which(fatig$temp_myhn == "38.5-39")] = 38.8
fatig$temp_myhn[which(fatig$temp_myhn == "38.5-39")] = 38.8
fatig$temp_myhn[which(fatig$temp_myhn == "38°")] = 38.0
fatig$temp_myhn[which(fatig$temp_myhn == "39.2 MAX")] = 39.2
fatig$temp_myhn[which(fatig$temp_myhn == "39°")] = 39.0

fatig$APHA[which(fatig$APHA == "")] = NA
fatig$APHA[which(fatig$APHA == "/")] = NA
table(fatig$APHA, useNA = "ifany")

fatig$APHB[which(fatig$APHB == "")] = NA
fatig$APHB[which(fatig$APHB == "/")] = NA
table(fatig$APHB, useNA = "ifany")

fatig$APHA1[which(fatig$APHA1 == "")] = NA
fatig$APHA1[which(fatig$APHA1 == "/")] = NA
table(fatig$APHA1, useNA = "ifany")

fatig$APHB1[which(fatig$APHB1 == "")] = NA
fatig$APHB1[which(fatig$APHB1 == "/")] = NA
table(fatig$APHB1, useNA = "ifany")

fatig$APHA2[which(fatig$APHA2 == "")] = NA
fatig$APHA2[which(fatig$APHA2 == "/")] = NA
fatig$APHA2[which(fatig$APHA2 == "00")] = 0
fatig$APHA2[which(fatig$APHA2 == "01")] = 1
fatig$APHA2[which(fatig$APHA2 == "1H")] = 1
table(fatig$APHA2, useNA = "ifany")

fatig$APHB2[which(fatig$APHB2 == "")] = NA
fatig$APHB2[which(fatig$APHB2 == "/")] = NA
table(fatig$APHB2, useNA = "ifany")


fatig$APHA3[which(fatig$APHA3 == "")] = NA
fatig$APHA3[which(fatig$APHA3 == "/")] = NA
table(fatig$APHA3, useNA = "ifany")

fatig$APHB3[which(fatig$APHB3 == "")] = NA
fatig$APHB3[which(fatig$APHA3 == "/")] = NA
table(fatig$APHA3, useNA = "ifany")

fatig$APHA4[which(fatig$APHA4 == "")] = NA
fatig$APHA4[which(fatig$APHA4 == "/")] = NA
table(fatig$APHA4, useNA = "ifany")

fatig$APHB4[which(fatig$APHB4 == "")] = NA
fatig$APHB4[which(fatig$APHB4 == "/")] = NA
fatig$APHB4[which(fatig$APHB4 == "06")] = NA
table(fatig$APHB4, useNA = "ifany")

fatig$APHC[which(fatig$APHC == "")] = NA
fatig$APHC[which(fatig$APHC == "/")] = NA
table(fatig$APHC, useNA = "ifany")

fatig$VITAMIN4[which(fatig$VITAMIN4 == "")] = NA
fatig$VITAMIN4[which(fatig$VITAMIN4 == "/")] = NA
table(fatig$VITAMIN4, useNA = "ifany")

# I delete variables that are text
fatig$otherrisktext<-NULL
fatig$AP2<-NULL
fatig$VITAMIN7<-NULL
fatig$QM3A<-NULL
fatig$QM16<-NULL
fatig$QM18A<-NULL



# No data
fatig$comp_hostdat<-NULL

# Only one data
fatig$VISIT<-NULL
fatig$COHORT<-NULL
fatig$redcap_event_name<-NULL
fatig$cohort_type<-NULL
fatig$age_estimateyearsu<-NULL


# Dates
fatig$DATESAISIE.x<-NULL
fatig$DATESAISIE.y<-NULL



#I do a list with each variable and number of missing
missing<-colSums(is.na(fatig))


#I do a list with each variable and percent of missing
percent<-colMeans(is.na(fatig))*100

#I build a dataframe with missing data
na<-data.frame(missing,percent)
names(na)

#I order the datafram "na" by percent of missing 
naord <- na[order(-missing),] 
names(naord)
class(naord)


#I add an extra column 
library(tibble)
#naord <- add_rownames(naord, "variable")
naord <- rownames_to_column(naord, "variable")
varsexc<-naord$variable[naord$percent>=60]
dim(fatig)
summary(fatig$QM11)
7133/13600

# I exclude variables with more than 60% of missing
fatig<-fatig%>%
  select(-varsexc)

str(fatig)


# Physical activity
table(fatig$APHA, useNA = "ifany")
class(fatig$APHA)
fatig$APHAm<-as.numeric(fatig$APHA)
table(fatig$APHAm, useNA = "ifany")
summary(fatig$APHAm)

fatig$APHAm[fatig$APHAm>50]<-NA
fatig$APHBm<-as.numeric(fatig$APHB)
summary(fatig$APHBm)
fatig$APHBm[fatig$APHBm>50]<-NA
table(fatig$APHA1)
fatig$APHA1m<-as.numeric(fatig$APHA1)
summary(fatig$APHA1m)
table(fatig$APHB1)
fatig$APHB1m<-as.numeric(fatig$APHB1)
fatig$APHA2m<-as.numeric(fatig$APHA2)
fatig$APHB2m<-as.numeric(fatig$APHB2)
fatig$APHA3m<-as.numeric(fatig$APHA3)
fatig$APHB3m<-as.numeric(fatig$APHB3)
fatig$APHA4m<-as.numeric(fatig$APHA4)
fatig$APHB4m<-as.numeric(fatig$APHB4)




# average of summer and winter variables
fatig$walking = (fatig$APHAm + fatig$APHBm) / 2
fatig$cycling = (fatig$APHA1m + fatig$APHB1m) / 2
fatig$gardening = (fatig$APHA2m + fatig$APHB2m) / 2
fatig$household = (fatig$APHA3m + fatig$APHB3m) / 2
fatig$sport = (fatig$APHA4m + fatig$APHB4m) / 2


# for all activities, assume that 15, 30, 45, 60, 90 hours were reported in minutes (replaced previous rule 23/07/21) (see IPAQ rules)
fatig$walking[which(fatig$walking == 15 | fatig$walking == 30 | fatig$walking == 45 | fatig$walking == 60 | fatig$walking == 90)] = 
  fatig$walking[which(fatig$walking == 15 | fatig$walking == 30 | fatig$walking == 45 | fatig$walking == 60 | fatig$walking == 90)] / 60

fatig$cycling[which(fatig$cycling == 15 | fatig$cycling == 30 | fatig$cycling == 45 | fatig$cycling == 60 | fatig$cycling == 90)] = 
  fatig$cycling[which(fatig$cycling == 15 | fatig$cycling == 30 | fatig$cycling == 45 | fatig$cycling == 60 | fatig$cycling == 90)] / 60

fatig$gardening[which(fatig$gardening == 15 | fatig$gardening == 30 | fatig$gardening == 45 | fatig$gardening == 60 | fatig$gardening == 90)] = 
  fatig$gardening[which(fatig$gardening == 15 | fatig$gardening == 30 | fatig$gardening == 45 | fatig$gardening == 60 | fatig$gardening == 90)] / 60

fatig$household[which(fatig$household == 15 | fatig$household == 30 | fatig$household == 45 | fatig$household == 60 | fatig$household == 90)] = 
  fatig$household[which(fatig$household == 15 | fatig$household == 30 | fatig$household == 45 | fatig$household == 60 | fatig$household == 90)] / 60

fatig$sport[which(fatig$sport == 15 | fatig$sport == 30 | fatig$sport == 45 | fatig$sport == 60 | fatig$sport == 90)] = 
  fatig$sport[which(fatig$sport == 15 | fatig$sport == 30 | fatig$sport == 45 | fatig$sport == 60 | fatig$sport == 90)] / 60


# truncate values > 21h/week for specific activity to 21 (see IPAQ rules)
fatig$walking[which(fatig$walking > 21)] = 21
fatig$cycling[which(fatig$cycling > 21)] = 21
fatig$gardening[which(fatig$gardening > 21)] = 21
fatig$household[which(fatig$household > 21)] = 21
fatig$sport[which(fatig$sport > 21)] = 21

# sum of all activities should not be higher than 16h/day = 112h/week --> exclude if the case (see IPAQ rules)
fatig$sum_all_activities = fatig$walking + fatig$cycling + fatig$gardening + fatig$household + fatig$sport
summary(fatig$sum_all_activities)

lowpa<-quantile(fatig$sum_all_activities, probs = c(0.25,0.5,0.75), na.rm = TRUE)[[1]]

fatig$lowpa<-ifelse(fatig$sum_all_activities<lowpa,"lowpa","nolowpa")
fatig$lowpa<-factor(fatig$lowpa)

table(fatig$lowpa,useNA = "ifany")



# Calculation of Charson index (comorbidity)

# 1, High blood pressure
fatig$hypertention_mhyn<-ifelse(fatig$hypertention_mhyn==1|fatig$traitement_three==1,1,0)
summary(fatig$hypertention_mhyn)

# 2, CVD
fatig$chroniccard_mhyn<-ifelse(fatig$chroniccard_mhyn==1,1,0)
summary(fatig$chroniccard_mhyn)

# 3. Chronic pulmonary disease
fatig$chronicpul_mhyn<-ifelse(fatig$chronicpul_mhyn==1,1,0)
summary(fatig$chronicpul_mhyn)

# 4. Asthma

# fatig$asthma_mhyn<-ifelse(fatig$asthma_mhyn==1,1,0)
# table(fatig$asthma_mhyn)

fatig$asthma_mhyn<-ifelse(fatig$asthma_mhyn==1|fatig$asthme==1,1,0)
summary(fatig$asthma_mhyn)
table(fatig$asthma_mhyn)

fatig$asthma_mhynmod<-factor(fatig$asthma_mhyn, levels = c(1,0), labels = c("yes","no"))

# 5. Renal disease
fatig$renal_mhyn<-ifelse(fatig$renal_mhyn==1,1,0)
summary(fatig$renal_mhyn)

# 6. Dialysis
fatig$dialys_mhyn<-ifelse(fatig$dialys_mhyn==1,1,0)
summary(fatig$dialys_mhyn)

# 7. Moderate liver disease
fatig$modliver_mhyn<-ifelse(fatig$modliver_mhyn==1,1,0)
summary(fatig$modliver_mhyn)

# 8. Mild liver disease
fatig$mildliv_mhyn<-ifelse(fatig$mildliv_mhyn==1,1,0)
summary(fatig$mildliv_mhyn)

# 9. Chronic neu
fatig$chronicneu_mhyn<-ifelse(fatig$chronicneu_mhyn==1,1,0)
summary(fatig$chronicneu_mhyn)

# 10 Cancer
fatig$malignantneo_mhyn<-ifelse(fatig$malignantneo_mhyn==1,1,0)
summary(fatig$malignantneo_mhyn)

# 11 Chromohemat
fatig$chronhaemo_mhyn<-ifelse(fatig$chronhaemo_mhyn==1,1,0)
summary(fatig$chronhaemo_mhyn)

# 12 HIV
fatig$aidshiv_mhyn<-ifelse(fatig$aidshiv_mhyn==1,1,0)
summary(fatig$aidshiv_mhyn)

# 13 Obesity
fatig$obesity_mhyn<-ifelse(fatig$obesity_mhyn==1,1,0)
summary(fatig$obesity_mhyn)

# 14. Compl toux_myhn
fatig$diabetiscomp_mhyn<-ifelse(fatig$diabetiscomp_mhyn==1,1,0)
summary(fatig$diabetiscomp_mhyn)

# 15. diabetes
fatig$diabetes<-NA
fatig$diabetes[fatig$diabetes_mhyn==1 | fatig$traitement_two==1]<-1
fatig$diabetes[fatig$diabetes_mhyn!=1 & fatig$traitement_two==0]<-0


# 16. Rheumat
fatig$rheumatology_mhyr<-ifelse(fatig$rheumatology_mhyr==1,1,0)
summary(fatig$rheumatology_mhyr)

# 17. Dementia
fatig$dementia_mhyn<-ifelse(fatig$dementia_mhyn==1,1,0)
summary(fatig$dementia_mhyn)
summary(fatig$dementia_mhyn)

# 18. Malnutrition
fatig$malnutrition_mhyn<-ifelse(fatig$malnutrition_mhyn==1,1,0)
summary(fatig$malnutrition_mhyn)

# 19 COPD
fatig$copd_mhyn<-ifelse(fatig$copd_mhyn==1,1,0)
summary(fatig$copd_mhyn)


# Other
table(fatig$other_mhyn)
fatig$other_mhyn<-ifelse(fatig$other_mhyn==1,1,0)
summary(fatig$other_mhyn)


fatig$char<-NA
fatig$char<-fatig$hypertention_mhyn + fatig$chroniccard_mhyn + fatig$chronicpul_mhyn + fatig$asthma_mhyn + 
  fatig$renal_mhyn + fatig$dialys_mhyn + fatig$modliver_mhyn + fatig$mildliv_mhyn + 
  fatig$chronicneu_mhyn + fatig$malignantneo_mhyn + fatig$chronhaemo_mhyn + fatig$aidshiv_mhyn + 
  fatig$obesity_mhyn + fatig$diabetiscomp_mhyn + fatig$diabetes + fatig$rheumatology_mhyr + 
  fatig$dementia_mhyn + fatig$malnutrition_mhyn +fatig$copd_mhyn+fatig$other_mhyn
table(fatig$char, useNA = "ifany")

fatig$multimorb<-NA
fatig$multimorb<-ifelse(fatig$char<=1,"no","yes")
fatig$multimorb<-factor(fatig$multimorb)
table(fatig$multimorb,useNA = "ifany")
round(100*prop.table(table(fatig$multimorb)))

fatig$char<-NULL

fatig$lung<-NA
fatig$lung<-ifelse((fatig$chronicpul_mhyn==1|fatig$asthma_mhyn==1|fatig$asthme==1),"pulm","no pulm")
fatig$lung<-factor(fatig$lung)
table(fatig$lung, useNA = "ifany")
# Diabetes

table(fatig$diabetes_mhyn,useNA = "ifany")
table(fatig$diabetiscomp_mhyn,useNA = "ifany")
fatig$diabetes<-NA
fatig$diabetes[fatig$diabetes_mhyn==1 | fatig$traitement_two==1]<-1
fatig$diabetes[fatig$diabetes_mhyn==2 & fatig$traitement_two==0]<-0
fatig$diabetes<-factor(fatig$diabetes,levels = c(1,0), labels = c("yes","no"))
table(fatig$diabetes, useNA = "ifany")
round(100*prop.table(table(fatig$diabetes)))


fatig$medanx<-NA
fatig$medanx[fatig$traitement_seven==1|fatig$traitement_eight==1]<-1
fatig$medanx[fatig$traitement_seven==0 & fatig$traitement_eight==0]<-2
fatig$medanx<-factor(fatig$medanx, levels=c(1,2), labels = c("yes","no"))
table(fatig$medanx, useNA = "ifany")
round(100*prop.table(table(fatig$medanx)))


fatig$NIH_classification_mod<-NA
fatig$NIH_classification_mod[fatig$NIH_classification_or=="Asymptomatic"]<-"Asym"
fatig$NIH_classification_mod[fatig$NIH_classification_or=="Mild illness"]<-"Mild"
fatig$NIH_classification_mod[fatig$NIH_classification_or=="Moderate illness"|fatig$NIH_classification_or=="Hospitalised"]<-"Mod-sev"
fatig$NIH_classification_mod<-factor(fatig$NIH_classification_mod)
table(fatig$NIH_classification_mod, useNA = "ifany")
round(100*prop.table(table(fatig$NIH_classification_mod, useNA = "ifany")))



table(fatig$JOUR)
fatig$JOURn<-NA
fatig$JOURn[fatig$JOUR=="J0"]<-0
fatig$JOURn[fatig$JOUR=="J1"]<-1
fatig$JOURn[fatig$JOUR=="J2"]<-2
fatig$JOURn[fatig$JOUR=="J3"]<-3
fatig$JOURn[fatig$JOUR=="J4"]<-4
fatig$JOURn[fatig$JOUR=="J5"]<-5
fatig$JOURn[fatig$JOUR=="J6"]<-6
fatig$JOURn[fatig$JOUR=="J7"]<-7
fatig$JOURn[fatig$JOUR=="J8"]<-8
fatig$JOURn[fatig$JOUR=="J9"]<-9
fatig$JOURn[fatig$JOUR=="J10"]<-10
fatig$JOURn[fatig$JOUR=="J11"]<-11
fatig$JOURn[fatig$JOUR=="J12"]<-12
fatig$JOURn[fatig$JOUR=="J13"]<-13
fatig$JOURn[fatig$JOUR=="J14"]<-14
summary(fatig$JOURn)

fatig$JOUR<-NULL

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigué(e) | 3, Je me sens mal
table(fatig$QM2, useNA = "ifany")
fatig$fatigue<-NA
fatig$fatigue[fatig$QM2==1]<-0
fatig$fatigue[fatig$QM2==2]<-1
fatig$fatigue[fatig$QM2==3]<-2
summary(fatig$fatigue)

fatig$QM2<-NULL

# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non
table(fatig$QM3, useNA = "ifany")
fatig$sleep<-NA
fatig$sleep[fatig$QM3==0]<-1
fatig$sleep[fatig$QM3==1]<-0
summary(fatig$sleep)
table(fatig$sleep)
fatig$QM3<-NULL

# 3 Dry cough Question QM4 Avez-vous une toux sèche ?
# Answer 1, Oui | 0, Non
table(fatig$QM4, useNA = "ifany")
fatig$cough<-fatig$QM4
summary(fatig$cough)
table(fatig$cough, useNA = "ifany")
fatig$QM4<-NULL

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 
table(fatig$QM5, useNA = "ifany")
fatig$cough_plus<-fatig$QM5
summary(fatig$cough_plus)
fatig$QM5<-NULL

# 5 Sore throat QM6 
table(fatig$QM6, useNA = "ifany")
fatig$throat<-fatig$QM6
summary(fatig$throat)

fatig$QM6<-NULL

# 6 Loss of taste/smell QM7 8. Avez-vous noté une forte diminution ou perte de votre 
# goût ou de votre odorat ?
table(fatig$QM7, useNA = "ifany")
fatig$smell<-fatig$QM7
summary(fatig$smell)

fatig$QM7<-NULL

# 7. Diarrhea QM8 Avez-vous de la diarrhée ? Avec au moins 3 selles liquides/molles par jour.
table(fatig$QM8, useNA = "ifany")
fatig$diarrhea<-fatig$QM8
summary(fatig$diarrhea)

fatig$QM8<-NULL

# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?
table(fatig$QM9, useNA = "ifany")
fatig$muscle<-fatig$QM9
summary(fatig$muscle)

fatig$QM9<-NULL

# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
table(fatig$QM10, useNA = "ifany")
fatig$chest<-fatig$QM10
summary(fatig$chest)

fatig$QM10<-NULL

# 10 Pain scale Quel est votre niveau de douleur actuel ?
table(fatig$QM11, useNA = "ifany")

fatig$pain<-NA
fatig$pain[fatig$QM11<2]<-0
fatig$pain[fatig$QM11>=2 & fatig$QM11<=3]<-1
fatig$pain[fatig$QM11>3]<-2
table(fatig$pain,useNA = "ifany")
fatig$pain[is.na(fatig$pain) & fatig$chest==0]<-0
summary(fatig$pain)

fatig$QM11<-NULL


# 11 Fever  Avez-vous de la fièvre ?
table(fatig$QM12, useNA = "ifany")
fatig$fever<-fatig$QM12
summary(fatig$fever)

fatig$QM12<-NULL

# I do not use this question (do you have a thermometer?)
fatig$QM13<-NULL

# I do not use this question (measured temperature, many missing)
fatig$QM13A<-NULL

# 12 Difficulty breathing Avez-vous des difficultés respiratoires ?
table(fatig$QM14, useNA = "ifany")
fatig$breath<-fatig$QM14
summary(fatig$breath)

fatig$QM14<-NULL

# 13 Increased breath difficulties  Avez-vous vu apparaître une 
# gêne respiratoire ou une augmentation de votre gêne respiratoire habituelle ?
table(fatig$QM15, useNA = "ifany")
fatig$breath_plus<-fatig$QM15
summary(fatig$breath_plus)

fatig$QM15<-NULL



# 14 Difficulties to eat or drink ?  Avez-vous des difficultés importantes 
# pour vous alimenter ou boire ?
table(fatig$QM17, useNA = "ifany")
fatig$eat<-fatig$QM17
summary(fatig$eat)

fatig$QM17<-NULL

# 15 Avez-vous d'autres symptômes? Other symptoms ?
table(fatig$QM18, useNA = "ifany")
fatig$other<-fatig$QM18
summary(fatig$other)

fatig$QM18<-NULL

# I do not use this question: Contact avec d'autres personnes
fatig$QM19<-NULL

# 16 Avez-vous noté une apparition subite d'éruptions cutanées au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, lésions d'urticaire passagères) ?
table(fatig$QM20, useNA = "ifany")
fatig$skin<-fatig$QM20
summary(fatig$skin)

fatig$QM20<-NULL

# 17 Avez-vous noté l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, démangeaisons 
# au niveau des paupières, sensations de picotements, brûlure, larmoiement fréquent) ?
table(fatig$QM21, useNA = "ifany")
fatig$eyes<-fatig$QM21
summary(fatig$eyes)

fatig$QM21<-NULL
# 
# # 18. Vous êtes à la maison, à l'hôpital
# table(fatig$QM1, useNA = "ifany")
fatig$hospital<-NA
fatig$hospital[fatig$QM1==2]<-1
fatig$hospital[fatig$QM1==1]<-0
fatig$hospital[fatig$QM1==3]<-0
table(fatig$hospital, useNA = "ifany")

fatig$QM1<-NULL


fatig$score<-NA
fatig$score<-fatig$fatigue+fatig$sleep+fatig$cough+fatig$cough_plus+fatig$throat+
  fatig$smell+fatig$diarrhea+fatig$muscle+fatig$chest+fatig$pain +
  fatig$fever+fatig$breath+fatig$breath_plus+fatig$eat+fatig$other +
  fatig$skin + fatig$eyes
summary(fatig$score)




library(ggplot2)
ggplot(data=fatig, aes(x=score)) +
  geom_density()

fatig$sexm<-ifelse(fatig$sex==1,"male","female")
fatig$sexm<-factor(fatig$sexm)
table(fatig$sexm)

ggplot(data=fatig, aes(x=sexm,y=score))+
  geom_boxplot()

ggplot(data=fatig, aes(x=NIH_classification_mod,y=score))+
  geom_boxplot()




# Owner
table(fatig$QP1, useNA = "ifany")
fatig$owner<-fatig$QP1
fatig$owner<-factor(fatig$owner, levels=c(1,0), labels = c("yes","no"))
summary(fatig$owner)
fatig$QP1<-NULL



# Education QP4
summary(fatig$QP4)

# 1=primary school
# 2=secondary, first cycle
# 3=secondary, second cycle with certificate
# 4=post secondary, short technical 
# 5= superior short cycle, technique 
# 6 bachelor
# 7 master
# 8 PhD

table(fatig$QP4, useNA = "ifany")
# 214/nrow(fatig)
# Missing data=32%

median(fatig$QP4, na.rm=TRUE)
fatig$loweduc<-ifelse(fatig$QP4<=3,"only school","superior")
fatig$loweduc<-factor(fatig$loweduc)

totall<-table(fatig$loweduc)[[1]]
perall<-round(100*prop.table(table(fatig$loweduc)))[[1]]

totmale<-table(fatig$loweduc[fatig$sexm=="male"])[[1]]
percmale<-round(100*prop.table(table(fatig$loweduc[fatig$sexm=="male"])))[[1]]

totfemale<-table(fatig$loweduc[fatig$sexm=="female"])[[1]]
percfemale<-round(100*prop.table(table(fatig$loweduc[fatig$sexm=="female"])))[[1]]

fatig$QP4<-NULL

p<-chisq.test(fatig$loweduc,fatig$sexm)
pvalue<-p$p.value

x<-c("low_loweduc",totall,perall,totmale,percmale,totfemale,percfemale,pvalue)
low_loweduc<-data.frame(x)
low_loweduct<-t(low_loweduc)
low_loweduct<-data.frame(low_loweduct)
names(low_loweduct)<-c("var","totall","perall","totmale","percmale","totfemale","percfemale",
                         "pvalue")

 all<-rbind(oldert,low_loweduct)

 # Income QP3
 table(fatig$QP3, useNA = "ifany")
 nrow(fatig)
 # 229/657
 # Missing data=34%
 
 # median(fatig$QP3, na.rm=TRUE)
 # quantile(fatig$QP3, probs=c(0,0.25,0.5,0.75,1),na.rm=TRUE)
 # fatig$lowincome<-ifelse(fatig$QP3<5,"low","high")
 # fatig$lowincome<-factor(fatig$lowincome)
 # table( fatig$lowincome, useNA = "ifany")

fatig$income<-fatig$QP3
summary(fatig$income)

fatig$QP3<-NULL

# work QP5
table(fatig$QP5, useNA = "ifany")

fatig$work<-NA
fatig$work<-fatig$QP5
fatig$work<-factor(fatig$work, levels = c(0,1),labels = c("no work","work"))
table(fatig$work,useNA = "ifany")
round(100*prop.table(table(fatig$work,useNA = "ifany")))

fatig$QP5<-NULL

table(fatig$QP6, useNA = "ifany")

fatig$QP7<-NULL

# 
# # Physical activity before Covid AP1
# table(fatig$AP1, useNA = "ifany")
# 
# fatig$highpa<-fatig$AP1
# fatig$highpa[fatig$AP1==1]<-"yes"
# fatig$highpa[fatig$AP1==0]<-"no"
# fatig$highpa<-factor(fatig$highpa)
# table(fatig$highpa, useNA = "ifany")
fatig$AP1<-NULL

# 
# 206/nrow(fatig)
# Missing data 31%

# BMI and obesity
fatig<-fatig%>%
  mutate(bmi=poids/(taille/100)^2)
summary(fatig$bmi)



fatig$obese<-ifelse(fatig$bmi>=30,"obese","no obese")
fatig$obese<-factor(fatig$obese)
table(fatig$obese)


# Blood type gsang
table(fatig$gsang, useNA = "ifany")
fatig$gsangm<-fatig$gsang
fatig$gsangm[fatig$gsang==9]<-NA
fatig$agroup<-ifelse(fatig$gsangm==3,"a","other")
fatig$agroup<-factor(fatig$agroup)
table(fatig$agroup, useNA = "ifany")

# Fever
table(fatig$fever_ceoccur_two, useNA = "ifany")
fatig$fiverm<-ifelse(fatig$fever_ceoccur_two==1,"yes","no")
fatig$fiverm<-factor(fatig$fiverm)
fatig$fever_ceoccur_two<-NULL

# smoking state smoking_mhyn
fatig$smoker<-ifelse(fatig$smoking_mhyn==1,"yes","no")
fatig$smoker<-factor(fatig$smoker)
fatig$smoking_mhyn<-NULL


# Diabetes


table(fatig$diabetes, useNA = "ifany")
round(100*prop.table(table(fatig$diabetes)))


# Multimorbidity
 
table(fatig$multimorb, useNA = "ifany")


# Weight loss perte_poids
fatig$weight_loss<-ifelse(fatig$perte_poids==1,"yes","no")
fatig$weight_loss<-factor(fatig$weight_loss)
table(fatig$weight_loss,useNA = "ifany")




# Polypharmacy
 
fatig$med<-fatig$traitement_one+fatig$traitement_two+fatig$traitement_three+fatig$traitement_four+
  fatig$traitement_five+fatig$traitement_six+fatig$traitement_seven+fatig$traitement_eight+
  fatig$traitement_nine

fatig$medcat<-ifelse(fatig$med>=2,"yes","no")
fatig$medcat<-factor(fatig$medcat)
table(fatig$medcat, useNA = "ifany")



# Asthma 

table(fatig$asthma_mhyn)
table(fatig$asthma_mhynmod)



dim(fatig)




fatig$fatigue<-factor(fatig$fatigue,levels = c(0,1,2), labels=c("well","fatigue","bad"))
fatig$sleep<-factor(fatig$sleep, levels = c(1,0), labels = c("well sleep","bad sleep"))
fatig$cough<-factor(fatig$cough, levels = c(0,1), labels = c("no","yes"))
fatig$cough_plus<-factor(fatig$cough_plus, levels = c(0,1), labels = c("no","yes"))
fatig$throat<-factor(fatig$throat, levels = c(0,1), labels = c("no","yes"))
fatig$smell<-factor(fatig$smell, levels = c(0,1), labels = c("no","yes"))
fatig$diarrhea<-factor(fatig$diarrhea, levels = c(0,1), labels = c("no","yes"))
fatig$muscle<-factor(fatig$muscle, levels = c(0,1), labels = c("no","yes"))
fatig$chest<-factor(fatig$chest, levels = c(0,1), labels = c("no","yes"))
fatig$pain<-factor(fatig$pain, levels = c(0,1), labels = c("no","yes"))
fatig$fever<-factor(fatig$fever, levels = c(0,1), labels = c("no","yes"))
fatig$breath<-factor(fatig$breath, levels = c(0,1), labels = c("no","yes"))
fatig$breath_plus<-factor(fatig$breath_plus, levels = c(0,1), labels = c("no","yes"))
fatig$eat<-factor(fatig$eat, levels = c(0,1), labels = c("no","yes"))
fatig$other<-factor(fatig$other, levels = c(0,1), labels = c("no","yes"))
fatig$skin<-factor(fatig$skin, levels = c(0,1), labels = c("no","yes"))
fatig$eyes<-factor(fatig$eyes, levels = c(0,1), labels = c("no","yes"))

fatig$hospital<-factor(fatig$hospital, levels = c(1,0),labels = c("yes","no"))


# write.csv(fatig,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/fatig.csv", row.names = FALSE)


# Now, I transform long format in wide to better apply multiple imputation

table(fatig$JOURn, useNA = "ifany")

fatig<-fatig%>%
  filter(!is.na(JOURn))

fatig0<-fatig%>%
  filter(JOURn==0)

# write.csv(fatig0,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/fatig0.csv", row.names = FALSE)


library(stats)

fatigwm<-reshape(fatig, idvar = "SUBJID", timevar = "JOURn", 
                v.names=c("hospital","fatigue","sleep","cough","cough_plus","throat",
                          "smell","diarrhea","muscle","chest","pain",
                          "fever","breath","breath_plus","eat","other",
                          "skin","eyes","score"), direction = "wide")



dim(fatigwm)
fatigwm$included<-1

# 905

# write.csv(fatigwm,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/fatigwm.csv", row.names = FALSE)


# Excluding those that did not participate in the daily questionnaires
fatigw<-fatigwm%>%
  filter(!is.na(fatigue.0)|!is.na(sleep.0)|!is.na(cough.0)|!is.na(cough_plus.0)
         |!is.na(throat.0)|!is.na(smell.0)|!is.na(diarrhea.0)|!is.na(muscle.0)
         |!is.na(chest.0)|!is.na(pain.0)|!is.na(fever.0)|!is.na(breath.0)
         |!is.na(breath_plus.0) |!is.na(eat.0)|!is.na(other.0)|!is.na(skin.0)
         |!is.na(eyes.0)
         |!is.na(fatigue.1)|!is.na(sleep.1)|!is.na(cough.1)|!is.na(cough_plus.1)
         |!is.na(throat.1)|!is.na(smell.1)|!is.na(diarrhea.1)|!is.na(muscle.1)
         |!is.na(chest.1)|!is.na(pain.1)|!is.na(fever.1)|!is.na(breath.1)
         |!is.na(breath_plus.1) |!is.na(eat.1)|!is.na(other.1)|!is.na(skin.1)
         |!is.na(eyes.1)
         |!is.na(fatigue.2)|!is.na(sleep.2)|!is.na(cough.2)|!is.na(cough_plus.2)
         |!is.na(throat.2)|!is.na(smell.2)|!is.na(diarrhea.2)|!is.na(muscle.2)
         |!is.na(chest.2)|!is.na(pain.2)|!is.na(fever.2)|!is.na(breath.2)
         |!is.na(breath_plus.2) |!is.na(eat.2)|!is.na(other.2)|!is.na(skin.2)
         |!is.na(eyes.2)
         |!is.na(fatigue.3)|!is.na(sleep.3)|!is.na(cough.3)|!is.na(cough_plus.3)
         |!is.na(throat.3)|!is.na(smell.3)|!is.na(diarrhea.3)|!is.na(muscle.3)
         |!is.na(chest.3)|!is.na(pain.3)|!is.na(fever.3)|!is.na(breath.3)
         |!is.na(breath_plus.3) |!is.na(eat.3)|!is.na(other.3)|!is.na(skin.3)
         |!is.na(eyes.3)
         |!is.na(fatigue.4)|!is.na(sleep.4)|!is.na(cough.4)|!is.na(cough_plus.4)
         |!is.na(throat.4)|!is.na(smell.4)|!is.na(diarrhea.4)|!is.na(muscle.4)
         |!is.na(chest.4)|!is.na(pain.4)|!is.na(fever.4)|!is.na(breath.4)
         |!is.na(breath_plus.4) |!is.na(eat.4)|!is.na(other.4)|!is.na(skin.4)
         |!is.na(eyes.4)
         |!is.na(fatigue.5)|!is.na(sleep.5)|!is.na(cough.5)|!is.na(cough_plus.5)
         |!is.na(throat.5)|!is.na(smell.5)|!is.na(diarrhea.5)|!is.na(muscle.5)
         |!is.na(chest.5)|!is.na(pain.5)|!is.na(fever.5)|!is.na(breath.5)
         |!is.na(breath_plus.5) |!is.na(eat.5)|!is.na(other.5)|!is.na(skin.5)
         |!is.na(eyes.5)
         |!is.na(fatigue.6)|!is.na(sleep.6)|!is.na(cough.6)|!is.na(cough_plus.6)
         |!is.na(throat.6)|!is.na(smell.6)|!is.na(diarrhea.6)|!is.na(muscle.6)
         |!is.na(chest.6)|!is.na(pain.6)|!is.na(fever.6)|!is.na(breath.6)
         |!is.na(breath_plus.6) |!is.na(eat.6)|!is.na(other.6)|!is.na(skin.6)
         |!is.na(eyes.6)
         |!is.na(fatigue.7)|!is.na(sleep.7)|!is.na(cough.7)|!is.na(cough_plus.7)
         |!is.na(throat.7)|!is.na(smell.7)|!is.na(diarrhea.7)|!is.na(muscle.7)
         |!is.na(chest.7)|!is.na(pain.7)|!is.na(fever.7)|!is.na(breath.7)
         |!is.na(breath_plus.7) |!is.na(eat.7)|!is.na(other.7)|!is.na(skin.7)
         |!is.na(eyes.7)
         |!is.na(fatigue.8)|!is.na(sleep.8)|!is.na(cough.8)|!is.na(cough_plus.8)
         |!is.na(throat.8)|!is.na(smell.8)|!is.na(diarrhea.8)|!is.na(muscle.8)
         |!is.na(chest.8)|!is.na(pain.8)|!is.na(fever.8)|!is.na(breath.8)
         |!is.na(breath_plus.8) |!is.na(eat.8)|!is.na(other.8)|!is.na(skin.8)
         |!is.na(eyes.8)
         |!is.na(fatigue.9)|!is.na(sleep.9)|!is.na(cough.9)|!is.na(cough_plus.9)
         |!is.na(throat.9)|!is.na(smell.9)|!is.na(diarrhea.9)|!is.na(muscle.9)
         |!is.na(chest.9)|!is.na(pain.9)|!is.na(fever.9)|!is.na(breath.9)
         |!is.na(breath_plus.9) |!is.na(eat.9)|!is.na(other.9)|!is.na(skin.9)
         |!is.na(eyes.9)
         |!is.na(fatigue.10)|!is.na(sleep.10)|!is.na(cough.10)|!is.na(cough_plus.10)
         |!is.na(throat.10)|!is.na(smell.10)|!is.na(diarrhea.10)|!is.na(muscle.10)
         |!is.na(chest.10)|!is.na(pain.10)|!is.na(fever.10)|!is.na(breath.10)
         |!is.na(breath_plus.10) |!is.na(eat.10)|!is.na(other.10)|!is.na(skin.10)
         |!is.na(eyes.10)
         |!is.na(fatigue.11)|!is.na(sleep.11)|!is.na(cough.11)|!is.na(cough_plus.11)
         |!is.na(throat.11)|!is.na(smell.11)|!is.na(diarrhea.11)|!is.na(muscle.11)
         |!is.na(chest.11)|!is.na(pain.11)|!is.na(fever.11)|!is.na(breath.11)
         |!is.na(breath_plus.11) |!is.na(eat.11)|!is.na(other.11)|!is.na(skin.11)
         |!is.na(eyes.11)
         |!is.na(fatigue.12)|!is.na(sleep.12)|!is.na(cough.12)|!is.na(cough_plus.12)
         |!is.na(throat.12)|!is.na(smell.12)|!is.na(diarrhea.12)|!is.na(muscle.12)
         |!is.na(chest.12)|!is.na(pain.12)|!is.na(fever.12)|!is.na(breath.12)
         |!is.na(breath_plus.12) |!is.na(eat.12)|!is.na(other.12)|!is.na(skin.12)
         |!is.na(eyes.12)
         |!is.na(fatigue.13)|!is.na(sleep.13)|!is.na(cough.13)|!is.na(cough_plus.13)
         |!is.na(throat.13)|!is.na(smell.13)|!is.na(diarrhea.13)|!is.na(muscle.13)
         |!is.na(chest.13)|!is.na(pain.13)|!is.na(fever.13)|!is.na(breath.13)
         |!is.na(breath_plus.13) |!is.na(eat.13)|!is.na(other.13)|!is.na(skin.13)
         |!is.na(eyes.13)
         |!is.na(fatigue.14)|!is.na(sleep.14)|!is.na(cough.14)|!is.na(cough_plus.14)
         |!is.na(throat.14)|!is.na(smell.14)|!is.na(diarrhea.14)|!is.na(muscle.14)
         |!is.na(chest.14)|!is.na(pain.14)|!is.na(fever.14)|!is.na(breath.14)
         |!is.na(breath_plus.14) |!is.na(eat.14)|!is.na(other.14)|!is.na(skin.14)
         |!is.na(eyes.14))

dim(fatigw)

# Mark observation to be deleted after imputation
# These observations correspond to  a whole set of score questions that was not answer in an X day.




fatigw<-fatigw %>%
  mutate(miss0=ifelse(is.na(fatigue.0) & is.na(sleep.0)& is.na(cough.0) &is.na(cough_plus.0)
                      &is.na(throat.0) & is.na(smell.0) & is.na(diarrhea.0)&is.na(muscle.0)
                      &is.na(chest.0)&is.na(pain.0)&is.na(fever.0)&is.na(breath.0)
                      &is.na(breath_plus.0) &is.na(eat.0)&is.na(other.0)&is.na(skin.0)
                      &is.na(eyes.0),"miss","no miss")) %>%
  mutate(miss1=ifelse(is.na(fatigue.1) & is.na(sleep.1)& is.na(cough.1) &is.na(cough_plus.1)
                      &is.na(throat.1) & is.na(smell.1) & is.na(diarrhea.1)&is.na(muscle.1)
                      &is.na(chest.1)&is.na(pain.1)&is.na(fever.1)&is.na(breath.1)
                      &is.na(breath_plus.1) &is.na(eat.1)&is.na(other.1)&is.na(skin.1)
                      &is.na(eyes.1),"miss","no miss")) %>%
  mutate(miss2=ifelse(is.na(fatigue.2) & is.na(sleep.2)& is.na(cough.2) &is.na(cough_plus.2)
                      &is.na(throat.2) & is.na(smell.2) & is.na(diarrhea.2)&is.na(muscle.2)
                      &is.na(chest.2)&is.na(pain.2)&is.na(fever.2)&is.na(breath.2)
                      &is.na(breath_plus.2) &is.na(eat.2)&is.na(other.2)&is.na(skin.2)
                      &is.na(eyes.2),"miss","no miss")) %>%
  mutate(miss3=ifelse(is.na(fatigue.3) & is.na(sleep.3)& is.na(cough.3) &is.na(cough_plus.3)
                      &is.na(throat.3) & is.na(smell.3) & is.na(diarrhea.3)&is.na(muscle.3)
                      &is.na(chest.3)&is.na(pain.3)&is.na(fever.3)&is.na(breath.3)
                      &is.na(breath_plus.3) &is.na(eat.3)&is.na(other.3)&is.na(skin.3)
                      &is.na(eyes.3),"miss","no miss")) %>%
  mutate(miss4=ifelse(is.na(fatigue.4) & is.na(sleep.4)& is.na(cough.4) &is.na(cough_plus.4)
                      &is.na(throat.4) & is.na(smell.4) & is.na(diarrhea.4)&is.na(muscle.4)
                      &is.na(chest.4)&is.na(pain.4)&is.na(fever.4)&is.na(breath.4)
                      &is.na(breath_plus.4) &is.na(eat.4)&is.na(other.4)&is.na(skin.4)
                      &is.na(eyes.4),"miss","no miss")) %>%
  mutate(miss5=ifelse(is.na(fatigue.5) & is.na(sleep.5)& is.na(cough.5) &is.na(cough_plus.5)
                      &is.na(throat.5) & is.na(smell.5) & is.na(diarrhea.5)&is.na(muscle.5)
                      &is.na(chest.5)&is.na(pain.5)&is.na(fever.5)&is.na(breath.5)
                      &is.na(breath_plus.5) &is.na(eat.5)&is.na(other.5)&is.na(skin.5)
                      &is.na(eyes.5),"miss","no miss")) %>%
  mutate(miss6=ifelse(is.na(fatigue.6) & is.na(sleep.6)& is.na(cough.6) &is.na(cough_plus.6)
                      &is.na(throat.6) & is.na(smell.6) & is.na(diarrhea.6)&is.na(muscle.6)
                      &is.na(chest.6)&is.na(pain.6)&is.na(fever.6)&is.na(breath.6)
                      &is.na(breath_plus.6) &is.na(eat.6)&is.na(other.6)&is.na(skin.6)
                      &is.na(eyes.6),"miss","no miss")) %>%
  mutate(miss7=ifelse(is.na(fatigue.7) & is.na(sleep.7)& is.na(cough.7) &is.na(cough_plus.7)
                      &is.na(throat.7) & is.na(smell.7) & is.na(diarrhea.7)&is.na(muscle.7)
                      &is.na(chest.7)&is.na(pain.7)&is.na(fever.7)&is.na(breath.7)
                      &is.na(breath_plus.7) &is.na(eat.7)&is.na(other.7)&is.na(skin.7)
                      &is.na(eyes.7),"miss","no miss")) %>%
  mutate(miss8=ifelse(is.na(fatigue.8) & is.na(sleep.8)& is.na(cough.8) &is.na(cough_plus.8)
                      &is.na(throat.8) & is.na(smell.8) & is.na(diarrhea.8)&is.na(muscle.8)
                      &is.na(chest.8)&is.na(pain.8)&is.na(fever.8)&is.na(breath.8)
                      &is.na(breath_plus.8) &is.na(eat.8)&is.na(other.8)&is.na(skin.8)
                      &is.na(eyes.8),"miss","no miss")) %>%
  mutate(miss9=ifelse(is.na(fatigue.9) & is.na(sleep.9)& is.na(cough.9) &is.na(cough_plus.9)
                      &is.na(throat.9) & is.na(smell.9) & is.na(diarrhea.9)&is.na(muscle.9)
                      &is.na(chest.9)&is.na(pain.9)&is.na(fever.9)&is.na(breath.9)
                      &is.na(breath_plus.9) &is.na(eat.9)&is.na(other.9)&is.na(skin.9)
                      &is.na(eyes.9),"miss","no miss")) %>%
  mutate(miss10=ifelse(is.na(fatigue.10) & is.na(sleep.10)& is.na(cough.10) &is.na(cough_plus.10)
                      &is.na(throat.10) & is.na(smell.10) & is.na(diarrhea.10)&is.na(muscle.10)
                      &is.na(chest.10)&is.na(pain.10)&is.na(fever.10)&is.na(breath.10)
                      &is.na(breath_plus.10) &is.na(eat.10)&is.na(other.10)&is.na(skin.10)
                      &is.na(eyes.10),"miss","no miss")) %>%
  mutate(miss11=ifelse(is.na(fatigue.11) & is.na(sleep.11)& is.na(cough.11) &is.na(cough_plus.11)
                      &is.na(throat.11) & is.na(smell.11) & is.na(diarrhea.11)&is.na(muscle.11)
                      &is.na(chest.11)&is.na(pain.11)&is.na(fever.11)&is.na(breath.11)
                      &is.na(breath_plus.11) &is.na(eat.11)&is.na(other.11)&is.na(skin.11)
                      &is.na(eyes.11),"miss","no miss")) %>%
  mutate(miss12=ifelse(is.na(fatigue.12) & is.na(sleep.12)& is.na(cough.12) &is.na(cough_plus.12)
                      &is.na(throat.12) & is.na(smell.12) & is.na(diarrhea.12)&is.na(muscle.12)
                      &is.na(chest.12)&is.na(pain.12)&is.na(fever.12)&is.na(breath.12)
                      &is.na(breath_plus.12) &is.na(eat.12)&is.na(other.12)&is.na(skin.12)
                      &is.na(eyes.12),"miss","no miss")) %>%
  mutate(miss13=ifelse(is.na(fatigue.13) & is.na(sleep.13)& is.na(cough.13) &is.na(cough_plus.13)
                      &is.na(throat.13) & is.na(smell.13) & is.na(diarrhea.13)&is.na(muscle.13)
                      &is.na(chest.13)&is.na(pain.13)&is.na(fever.13)&is.na(breath.13)
                      &is.na(breath_plus.13) &is.na(eat.13)&is.na(other.13)&is.na(skin.13)
                      &is.na(eyes.13),"miss","no miss")) %>%
  mutate(miss14=ifelse(is.na(fatigue.14) & is.na(sleep.14)& is.na(cough.14) &is.na(cough_plus.14)
                      &is.na(throat.14) & is.na(smell.14) & is.na(diarrhea.14)&is.na(muscle.14)
                      &is.na(chest.14)&is.na(pain.14)&is.na(fever.14)&is.na(breath.14)
                      &is.na(breath_plus.14) &is.na(eat.14)&is.na(other.14)&is.na(skin.14)
                      &is.na(eyes.14),"miss","no miss")) 
                     
                     


dim(fatigwm)

#Comparison between included participants (675) and those excluded because they did not fill 
# at least two questionnaires (230)

#For that, I have to identify the excluded participants or to mark those included



fatigall<-fatigwm %>%
  full_join(fatig0, by="SUBJID") 


fatigall$includedm<- fatigall$included
fatigall$includedm[is.na(fatigall$included)]<-0
table(fatigall$includedm, useNA = "ifany")




write.csv(fatigw,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/fatigw751.csv", row.names = FALSE)


