
# Downloading data

# Definitive 4 classes model


wideimp<-read.csv("P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/long.csv", stringsAsFactors = TRUE)

# wideimp<-read.csv("Prediscore/generated_data/long.csv", stringsAsFactors = TRUE)

#I check if there are missing

table(wideimp$.imp)

library(dplyr)

wideimp40<-wideimp%>%
  filter(.imp==40)

anyNA(wideimp40)



#I check if there are missing in each variable that I am interested for

apply(wideimp40, 2, anyNA)


#I do a list with each variable and number of missing
missing<-colSums(is.na(wideimp40))

#I do a list with each variable and percent of missing
percent<-colMeans(is.na(wideimp40))*100
class(percent)

#I build a dataframe with missing data
na<-data.frame(missing,percent)
names(na)



#I order the dataframe "na" by percent of missing (to improve the imputation)
naordimp <- na[order(missing),] 
class(naordimp)




library(tibble)
naordimp<-rownames_to_column(naordimp,var="variables")

library(dplyr)
naordimp<-naordimp %>%
  arrange(percent)


# It did not impute dialyse: 	
# dialys_mhyn

# I will impute based on renal



# Calculation scores

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.0)
wideimp$fatigue.0n<-NA
wideimp$fatigue.0n[wideimp$fatigue.0=="bad"]<-2
wideimp$fatigue.0n[wideimp$fatigue.0=="fatigue"]<-1
wideimp$fatigue.0n[wideimp$fatigue.0=="well"]<-0
table(wideimp$fatigue.0n)
wideimp$fatiguebs<-wideimp$fatigue.0

# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.0n<-NA
wideimp$sleep.0n[wideimp$sleep.0=="bad sleep"]<-1
wideimp$sleep.0n[wideimp$sleep.0=="well sleep"]<-0
table(wideimp$sleep.0n)
wideimp$sleepbs<-wideimp$sleep.0


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.0)
wideimp$cough.0n<-NA
wideimp$cough.0n[wideimp$cough.0=="yes"]<-1
wideimp$cough.0n[wideimp$cough.0=="no"]<-0
table(wideimp$cough.0n)
wideimp$coughbs<-wideimp$cough.0

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.0)
wideimp$cough_plus.0n<-NA
wideimp$cough_plus.0n[wideimp$cough_plus.0=="yes"]<-1
wideimp$cough_plus.0n[wideimp$cough_plus.0=="no"]<-0
table(wideimp$cough_plus.0n)
wideimp$cough_plusbs<-wideimp$cough_plus.0

# 5 Sore throat QM6 

summary(wideimp$throat.0)
wideimp$throat.0n<-NA
wideimp$throat.0n[wideimp$throat.0=="yes"]<-1
wideimp$throat.0n[wideimp$throat.0=="no"]<-0
table(wideimp$throat.0n)
wideimp$throatbs<-wideimp$throat.0



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.0)
wideimp$smell.0n<-NA
wideimp$smell.0n[wideimp$smell.0=="yes"]<-1
wideimp$smell.0n[wideimp$smell.0=="no"]<-0
table(wideimp$smell.0n)
wideimp$smellbs<-wideimp$smell.0



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.0)
wideimp$diarrhea.0n<-NA
wideimp$diarrhea.0n[wideimp$diarrhea.0=="yes"]<-1
wideimp$diarrhea.0n[wideimp$diarrhea.0=="no"]<-0
table(wideimp$diarrhea.0n)
wideimp$diarrheabs<-wideimp$diarrhea.0



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.0)
wideimp$muscle.0n<-NA
wideimp$muscle.0n[wideimp$muscle.0=="yes"]<-1
wideimp$muscle.0n[wideimp$muscle.0=="no"]<-0
table(wideimp$muscle.0n)
wideimp$musclebs<-wideimp$muscle.0



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.0)
wideimp$chest.0n<-NA
wideimp$chest.0n[wideimp$chest.0=="yes"]<-1
wideimp$chest.0n[wideimp$chest.0=="no"]<-0
table(wideimp$chest.0n)
wideimp$chestbs<-wideimp$chest.0




# 10 Pain scale Quel est votre niveau de douleur actuel ?

table(wideimp$pain.0,useNA = "ifany")
wideimp$pain.0n<-NA
wideimp$pain.0n[wideimp$pain.0=="yes"]<-1
wideimp$pain.0n[wideimp$pain.0=="no"]<-0
wideimp$pain.0n[wideimp$chest.0n==0]<-0
table(wideimp$pain.0n,useNA = "ifany")
wideimp$painbs<-wideimp$pain.0

# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.0)
wideimp$fever.0n<-NA
wideimp$fever.0n[wideimp$fever.0=="yes"]<-1
wideimp$fever.0n[wideimp$fever.0=="no"]<-0
table(wideimp$fever.0n)

wideimp$feverbs<-wideimp$fever.0


# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.0)
wideimp$breath.0n<-NA
wideimp$breath.0n[wideimp$breath.0=="yes"]<-1
wideimp$breath.0n[wideimp$breath.0=="no"]<-0
table(wideimp$breath.0n)

wideimp$breathbs<-wideimp$breath.0

# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.0)
wideimp$breath_plus.0n<-NA
wideimp$breath_plus.0n[wideimp$breath_plus.0=="yes"]<-1
wideimp$breath_plus.0n[wideimp$breath_plus.0=="no"]<-0
table(wideimp$breath_plus.0n)

wideimp$breath_plusbs<-wideimp$breath_plus.0


# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.0)
wideimp$eat.0n<-NA
wideimp$eat.0n[wideimp$eat.0=="yes"]<-1
wideimp$eat.0n[wideimp$eat.0=="no"]<-0
table(wideimp$eat.0n)

wideimp$eatbs<-wideimp$eat.0

# 15 Other symptoms ?
summary(wideimp$other.0)
wideimp$other.0n<-NA
wideimp$other.0n[wideimp$other.0=="yes"]<-1
wideimp$other.0n[wideimp$other.0=="no"]<-0
table(wideimp$other.0n)

wideimp$otherbs<-wideimp$other.0

# 16 Skin rashes. Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.0)
wideimp$skin.0n<-NA
wideimp$skin.0n[wideimp$skin.0=="yes"]<-1
wideimp$skin.0n[wideimp$skin.0=="no"]<-0
table(wideimp$skin.0n)

wideimp$skinbs<-wideimp$skin.0


# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.0)
wideimp$eyes.0n[wideimp$eyes.0=="yes"]<-1
wideimp$eyes.0n[wideimp$eyes.0=="no"]<-0
table(wideimp$eyes.0n)

wideimp$eyesbs<-wideimp$eyes.0
#
# # 18. Vous etes a la maison, a l'hopital


table(wideimp$hospital.0)
wideimp$hospital0
wideimp$hospital0[wideimp$hospital.0=="yes"]<-1
wideimp$hospital0[wideimp$hospital.0=="no"]<-0
table(wideimp$hospital0)




wideimp$score0<-NA
wideimp$score0<-wideimp$fatigue.0n + wideimp$sleep.0n + wideimp$cough.0n + 
  wideimp$cough_plus.0n + wideimp$throat.0n + wideimp$smell.0n + 
  wideimp$diarrhea.0n + wideimp$muscle.0n + wideimp$chest.0n + 
  wideimp$pain.0n + wideimp$fever.0n + wideimp$breath.0n + 
  wideimp$breath_plus.0n + wideimp$eat.0n + wideimp$other.0n + 
  wideimp$skin.0n + wideimp$eyes.0n
summary(wideimp$score0)



# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.1)
wideimp$fatigue.1n<-NA
wideimp$fatigue.1n[wideimp$fatigue.1=="bad"]<-2
wideimp$fatigue.1n[wideimp$fatigue.1=="fatigue"]<-1
wideimp$fatigue.1n[wideimp$fatigue.1=="well"]<-0
table(wideimp$fatigue.1n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.1n<-NA
wideimp$sleep.1n[wideimp$sleep.1=="bad sleep"]<-1
wideimp$sleep.1n[wideimp$sleep.1=="well sleep"]<-0
table(wideimp$sleep.1n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.1)
wideimp$cough.1n<-NA
wideimp$cough.1n[wideimp$cough.1=="yes"]<-1
wideimp$cough.1n[wideimp$cough.1=="no"]<-0
table(wideimp$cough.1n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.1)
wideimp$cough_plus.1n<-NA
wideimp$cough_plus.1n[wideimp$cough_plus.1=="yes"]<-1
wideimp$cough_plus.1n[wideimp$cough_plus.1=="no"]<-0
table(wideimp$cough_plus.1n)

# 5 Sore throat QM6 

summary(wideimp$throat.1)
wideimp$throat.1n<-NA
wideimp$throat.1n[wideimp$throat.1=="yes"]<-1
wideimp$throat.1n[wideimp$throat.1=="no"]<-0
table(wideimp$throat.1n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.1)
wideimp$smell.1n<-NA
wideimp$smell.1n[wideimp$smell.1=="yes"]<-1
wideimp$smell.1n[wideimp$smell.1=="no"]<-0
table(wideimp$smell.1n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.1)
wideimp$diarrhea.1n<-NA
wideimp$diarrhea.1n[wideimp$diarrhea.1=="yes"]<-1
wideimp$diarrhea.1n[wideimp$diarrhea.1=="no"]<-0
table(wideimp$diarrhea.1n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.1)
wideimp$muscle.1n<-NA
wideimp$muscle.1n[wideimp$muscle.1=="yes"]<-1
wideimp$muscle.1n[wideimp$muscle.1=="no"]<-0
table(wideimp$muscle.1n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.1)
wideimp$chest.1n<-NA
wideimp$chest.1n[wideimp$chest.1=="yes"]<-1
wideimp$chest.1n[wideimp$chest.1=="no"]<-0
table(wideimp$chest.1n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.1)
wideimp$pain.1n<-NA
wideimp$pain.1n[wideimp$pain.1=="yes"]<-1
wideimp$pain.1n[wideimp$pain.1=="no"]<-0
table(wideimp$pain.1n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.1)
wideimp$fever.1n<-NA
wideimp$fever.1n[wideimp$fever.1=="yes"]<-1
wideimp$fever.1n[wideimp$fever.1=="no"]<-0
table(wideimp$fever.1n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.1)
wideimp$breath.1n<-NA
wideimp$breath.1n[wideimp$breath.1=="yes"]<-1
wideimp$breath.1n[wideimp$breath.1=="no"]<-0
table(wideimp$breath.1n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.1)
wideimp$breath_plus.1n<-NA
wideimp$breath_plus.1n[wideimp$breath_plus.1=="yes"]<-1
wideimp$breath_plus.1n[wideimp$breath_plus.1=="no"]<-0
table(wideimp$breath_plus.1n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.1)
wideimp$eat.1n<-NA
wideimp$eat.1n[wideimp$eat.1=="yes"]<-1
wideimp$eat.1n[wideimp$eat.1=="no"]<-0
table(wideimp$eat.1n)




# 15 Other symptoms ?
summary(wideimp$other.1)
wideimp$other.1n<-NA
wideimp$other.1n[wideimp$other.1=="yes"]<-1
wideimp$other.1n[wideimp$other.1=="no"]<-0
table(wideimp$other.1n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.1)
wideimp$skin.1n<-NA
wideimp$skin.1n[wideimp$skin.1=="yes"]<-1
wideimp$skin.1n[wideimp$skin.1=="no"]<-0
table(wideimp$skin.1n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.1)
wideimp$eyes.1n[wideimp$eyes.1=="yes"]<-1
wideimp$eyes.1n[wideimp$eyes.1=="no"]<-0
table(wideimp$eyes.1n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.1)
wideimp$hospital1[wideimp$hospital.1=="yes"]<-1
wideimp$hospital1[wideimp$hospital.1=="no"]<-0
table(wideimp$hospital1)




wideimp$score1<-NA
wideimp$score1<-wideimp$fatigue.1n + wideimp$sleep.1n + wideimp$cough.1n + 
  wideimp$cough_plus.1n + wideimp$throat.1n + wideimp$smell.1n + 
  wideimp$diarrhea.1n + wideimp$muscle.1n + wideimp$chest.1n + 
  wideimp$pain.1n + wideimp$fever.1n + wideimp$breath.1n + 
  wideimp$breath_plus.1n + wideimp$eat.1n + wideimp$other.1n + 
  wideimp$skin.1n + wideimp$eyes.1n
summary(wideimp$score1)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.2)
wideimp$fatigue.2n<-NA
wideimp$fatigue.2n[wideimp$fatigue.2=="bad"]<-2
wideimp$fatigue.2n[wideimp$fatigue.2=="fatigue"]<-1
wideimp$fatigue.2n[wideimp$fatigue.2=="well"]<-0
table(wideimp$fatigue.2n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.2n<-NA
wideimp$sleep.2n[wideimp$sleep.2=="bad sleep"]<-1
wideimp$sleep.2n[wideimp$sleep.2=="well sleep"]<-0
table(wideimp$sleep.2n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.2)
wideimp$cough.2n<-NA
wideimp$cough.2n[wideimp$cough.2=="yes"]<-1
wideimp$cough.2n[wideimp$cough.2=="no"]<-0
table(wideimp$cough.2n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.2)
wideimp$cough_plus.2n<-NA
wideimp$cough_plus.2n[wideimp$cough_plus.2=="yes"]<-1
wideimp$cough_plus.2n[wideimp$cough_plus.2=="no"]<-0
table(wideimp$cough_plus.2n)

# 5 Sore throat QM6 

summary(wideimp$throat.2)
wideimp$throat.2n<-NA
wideimp$throat.2n[wideimp$throat.2=="yes"]<-1
wideimp$throat.2n[wideimp$throat.2=="no"]<-0
table(wideimp$throat.2n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.2)
wideimp$smell.2n<-NA
wideimp$smell.2n[wideimp$smell.2=="yes"]<-1
wideimp$smell.2n[wideimp$smell.2=="no"]<-0
table(wideimp$smell.2n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.2)
wideimp$diarrhea.2n<-NA
wideimp$diarrhea.2n[wideimp$diarrhea.2=="yes"]<-1
wideimp$diarrhea.2n[wideimp$diarrhea.2=="no"]<-0
table(wideimp$diarrhea.2n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.2)
wideimp$muscle.2n<-NA
wideimp$muscle.2n[wideimp$muscle.2=="yes"]<-1
wideimp$muscle.2n[wideimp$muscle.2=="no"]<-0
table(wideimp$muscle.2n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.2)
wideimp$chest.2n<-NA
wideimp$chest.2n[wideimp$chest.2=="yes"]<-1
wideimp$chest.2n[wideimp$chest.2=="no"]<-0
table(wideimp$chest.2n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.2)
wideimp$pain.2n<-NA
wideimp$pain.2n[wideimp$pain.2=="yes"]<-1
wideimp$pain.2n[wideimp$pain.2=="no"]<-0
table(wideimp$pain.2n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.2)
wideimp$fever.2n<-NA
wideimp$fever.2n[wideimp$fever.2=="yes"]<-1
wideimp$fever.2n[wideimp$fever.2=="no"]<-0
table(wideimp$fever.2n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.2)
wideimp$breath.2n<-NA
wideimp$breath.2n[wideimp$breath.2=="yes"]<-1
wideimp$breath.2n[wideimp$breath.2=="no"]<-0
table(wideimp$breath.2n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.2)
wideimp$breath_plus.2n<-NA
wideimp$breath_plus.2n[wideimp$breath_plus.2=="yes"]<-1
wideimp$breath_plus.2n[wideimp$breath_plus.2=="no"]<-0
table(wideimp$breath_plus.2n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.2)
wideimp$eat.2n<-NA
wideimp$eat.2n[wideimp$eat.2=="yes"]<-1
wideimp$eat.2n[wideimp$eat.2=="no"]<-0
table(wideimp$eat.2n)




# 15 Other symptoms ?
summary(wideimp$other.2)
wideimp$other.2n<-NA
wideimp$other.2n[wideimp$other.2=="yes"]<-1
wideimp$other.2n[wideimp$other.2=="no"]<-0
table(wideimp$other.2n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.2)
wideimp$skin.2n<-NA
wideimp$skin.2n[wideimp$skin.2=="yes"]<-1
wideimp$skin.2n[wideimp$skin.2=="no"]<-0
table(wideimp$skin.2n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.2)
wideimp$eyes.2n[wideimp$eyes.2=="yes"]<-1
wideimp$eyes.2n[wideimp$eyes.2=="no"]<-0
table(wideimp$eyes.2n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.2)
wideimp$hospital2[wideimp$hospital.2=="yes"]<-1
wideimp$hospital2[wideimp$hospital.2=="no"]<-0
table(wideimp$hospital2)




wideimp$score2<-NA
wideimp$score2<-wideimp$fatigue.2n + wideimp$sleep.2n + wideimp$cough.2n + 
  wideimp$cough_plus.2n + wideimp$throat.2n + wideimp$smell.2n + 
  wideimp$diarrhea.2n + wideimp$muscle.2n + wideimp$chest.2n + 
  wideimp$pain.2n + wideimp$fever.2n + wideimp$breath.2n + 
  wideimp$breath_plus.2n + wideimp$eat.2n + wideimp$other.2n + 
  wideimp$skin.2n + wideimp$eyes.2n
summary(wideimp$score2)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.3)
wideimp$fatigue.3n<-NA
wideimp$fatigue.3n[wideimp$fatigue.3=="bad"]<-2
wideimp$fatigue.3n[wideimp$fatigue.3=="fatigue"]<-1
wideimp$fatigue.3n[wideimp$fatigue.3=="well"]<-0
table(wideimp$fatigue.3n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.3n<-NA
wideimp$sleep.3n[wideimp$sleep.3=="bad sleep"]<-1
wideimp$sleep.3n[wideimp$sleep.3=="well sleep"]<-0
table(wideimp$sleep.3n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.3)
wideimp$cough.3n<-NA
wideimp$cough.3n[wideimp$cough.3=="yes"]<-1
wideimp$cough.3n[wideimp$cough.3=="no"]<-0
table(wideimp$cough.3n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.3)
wideimp$cough_plus.3n<-NA
wideimp$cough_plus.3n[wideimp$cough_plus.3=="yes"]<-1
wideimp$cough_plus.3n[wideimp$cough_plus.3=="no"]<-0
table(wideimp$cough_plus.3n)

# 5 Sore throat QM6 

summary(wideimp$throat.3)
wideimp$throat.3n<-NA
wideimp$throat.3n[wideimp$throat.3=="yes"]<-1
wideimp$throat.3n[wideimp$throat.3=="no"]<-0
table(wideimp$throat.3n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.3)
wideimp$smell.3n<-NA
wideimp$smell.3n[wideimp$smell.3=="yes"]<-1
wideimp$smell.3n[wideimp$smell.3=="no"]<-0
table(wideimp$smell.3n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.3)
wideimp$diarrhea.3n<-NA
wideimp$diarrhea.3n[wideimp$diarrhea.3=="yes"]<-1
wideimp$diarrhea.3n[wideimp$diarrhea.3=="no"]<-0
table(wideimp$diarrhea.3n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.3)
wideimp$muscle.3n<-NA
wideimp$muscle.3n[wideimp$muscle.3=="yes"]<-1
wideimp$muscle.3n[wideimp$muscle.3=="no"]<-0
table(wideimp$muscle.3n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.3)
wideimp$chest.3n<-NA
wideimp$chest.3n[wideimp$chest.3=="yes"]<-1
wideimp$chest.3n[wideimp$chest.3=="no"]<-0
table(wideimp$chest.3n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.3)
wideimp$pain.3n<-NA
wideimp$pain.3n[wideimp$pain.3=="yes"]<-1
wideimp$pain.3n[wideimp$pain.3=="no"]<-0
table(wideimp$pain.3n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.3)
wideimp$fever.3n<-NA
wideimp$fever.3n[wideimp$fever.3=="yes"]<-1
wideimp$fever.3n[wideimp$fever.3=="no"]<-0
table(wideimp$fever.3n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.3)
wideimp$breath.3n<-NA
wideimp$breath.3n[wideimp$breath.3=="yes"]<-1
wideimp$breath.3n[wideimp$breath.3=="no"]<-0
table(wideimp$breath.3n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.3)
wideimp$breath_plus.3n<-NA
wideimp$breath_plus.3n[wideimp$breath_plus.3=="yes"]<-1
wideimp$breath_plus.3n[wideimp$breath_plus.3=="no"]<-0
table(wideimp$breath_plus.3n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.3)
wideimp$eat.3n<-NA
wideimp$eat.3n[wideimp$eat.3=="yes"]<-1
wideimp$eat.3n[wideimp$eat.3=="no"]<-0
table(wideimp$eat.3n)




# 15 Other symptoms ?
summary(wideimp$other.3)
wideimp$other.3n<-NA
wideimp$other.3n[wideimp$other.3=="yes"]<-1
wideimp$other.3n[wideimp$other.3=="no"]<-0
table(wideimp$other.3n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.3)
wideimp$skin.3n<-NA
wideimp$skin.3n[wideimp$skin.3=="yes"]<-1
wideimp$skin.3n[wideimp$skin.3=="no"]<-0
table(wideimp$skin.3n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.3)
wideimp$eyes.3n[wideimp$eyes.3=="yes"]<-1
wideimp$eyes.3n[wideimp$eyes.3=="no"]<-0
table(wideimp$eyes.3n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.3)
wideimp$hospital3[wideimp$hospital.3=="yes"]<-1
wideimp$hospital3[wideimp$hospital.3=="no"]<-0
table(wideimp$hospital3)




wideimp$score3<-NA
wideimp$score3<-wideimp$fatigue.3n + wideimp$sleep.3n + wideimp$cough.3n + 
  wideimp$cough_plus.3n + wideimp$throat.3n + wideimp$smell.3n + 
  wideimp$diarrhea.3n + wideimp$muscle.3n + wideimp$chest.3n + 
  wideimp$pain.3n + wideimp$fever.3n + wideimp$breath.3n + 
  wideimp$breath_plus.3n + wideimp$eat.3n + wideimp$other.3n + 
  wideimp$skin.3n + wideimp$eyes.3n
summary(wideimp$score3)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.4)
wideimp$fatigue.4n<-NA
wideimp$fatigue.4n[wideimp$fatigue.4=="bad"]<-2
wideimp$fatigue.4n[wideimp$fatigue.4=="fatigue"]<-1
wideimp$fatigue.4n[wideimp$fatigue.4=="well"]<-0
table(wideimp$fatigue.4n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.4n<-NA
wideimp$sleep.4n[wideimp$sleep.4=="bad sleep"]<-1
wideimp$sleep.4n[wideimp$sleep.4=="well sleep"]<-0
table(wideimp$sleep.4n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.4)
wideimp$cough.4n<-NA
wideimp$cough.4n[wideimp$cough.4=="yes"]<-1
wideimp$cough.4n[wideimp$cough.4=="no"]<-0
table(wideimp$cough.4n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.4)
wideimp$cough_plus.4n<-NA
wideimp$cough_plus.4n[wideimp$cough_plus.4=="yes"]<-1
wideimp$cough_plus.4n[wideimp$cough_plus.4=="no"]<-0
table(wideimp$cough_plus.4n)

# 5 Sore throat QM6 

summary(wideimp$throat.4)
wideimp$throat.4n<-NA
wideimp$throat.4n[wideimp$throat.4=="yes"]<-1
wideimp$throat.4n[wideimp$throat.4=="no"]<-0
table(wideimp$throat.4n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.4)
wideimp$smell.4n<-NA
wideimp$smell.4n[wideimp$smell.4=="yes"]<-1
wideimp$smell.4n[wideimp$smell.4=="no"]<-0
table(wideimp$smell.4n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.4)
wideimp$diarrhea.4n<-NA
wideimp$diarrhea.4n[wideimp$diarrhea.4=="yes"]<-1
wideimp$diarrhea.4n[wideimp$diarrhea.4=="no"]<-0
table(wideimp$diarrhea.4n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.4)
wideimp$muscle.4n<-NA
wideimp$muscle.4n[wideimp$muscle.4=="yes"]<-1
wideimp$muscle.4n[wideimp$muscle.4=="no"]<-0
table(wideimp$muscle.4n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.4)
wideimp$chest.4n<-NA
wideimp$chest.4n[wideimp$chest.4=="yes"]<-1
wideimp$chest.4n[wideimp$chest.4=="no"]<-0
table(wideimp$chest.4n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.4)
wideimp$pain.4n<-NA
wideimp$pain.4n[wideimp$pain.4=="yes"]<-1
wideimp$pain.4n[wideimp$pain.4=="no"]<-0
table(wideimp$pain.4n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.4)
wideimp$fever.4n<-NA
wideimp$fever.4n[wideimp$fever.4=="yes"]<-1
wideimp$fever.4n[wideimp$fever.4=="no"]<-0
table(wideimp$fever.4n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.4)
wideimp$breath.4n<-NA
wideimp$breath.4n[wideimp$breath.4=="yes"]<-1
wideimp$breath.4n[wideimp$breath.4=="no"]<-0
table(wideimp$breath.4n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.4)
wideimp$breath_plus.4n<-NA
wideimp$breath_plus.4n[wideimp$breath_plus.4=="yes"]<-1
wideimp$breath_plus.4n[wideimp$breath_plus.4=="no"]<-0
table(wideimp$breath_plus.4n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.4)
wideimp$eat.4n<-NA
wideimp$eat.4n[wideimp$eat.4=="yes"]<-1
wideimp$eat.4n[wideimp$eat.4=="no"]<-0
table(wideimp$eat.4n)




# 15 Other symptoms ?
summary(wideimp$other.4)
wideimp$other.4n<-NA
wideimp$other.4n[wideimp$other.4=="yes"]<-1
wideimp$other.4n[wideimp$other.4=="no"]<-0
table(wideimp$other.4n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.4)
wideimp$skin.4n<-NA
wideimp$skin.4n[wideimp$skin.4=="yes"]<-1
wideimp$skin.4n[wideimp$skin.4=="no"]<-0
table(wideimp$skin.4n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.4)
wideimp$eyes.4n[wideimp$eyes.4=="yes"]<-1
wideimp$eyes.4n[wideimp$eyes.4=="no"]<-0
table(wideimp$eyes.4n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.4)
wideimp$hospital4[wideimp$hospital.4=="yes"]<-1
wideimp$hospital4[wideimp$hospital.4=="no"]<-0
table(wideimp$hospital4)




wideimp$score4<-NA
wideimp$score4<-wideimp$fatigue.4n + wideimp$sleep.4n + wideimp$cough.4n + 
  wideimp$cough_plus.4n + wideimp$throat.4n + wideimp$smell.4n + 
  wideimp$diarrhea.4n + wideimp$muscle.4n + wideimp$chest.4n + 
  wideimp$pain.4n + wideimp$fever.4n + wideimp$breath.4n + 
  wideimp$breath_plus.4n + wideimp$eat.4n + wideimp$other.4n + 
  wideimp$skin.4n + wideimp$eyes.4n
summary(wideimp$score4)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.5)
wideimp$fatigue.5n<-NA
wideimp$fatigue.5n[wideimp$fatigue.5=="bad"]<-2
wideimp$fatigue.5n[wideimp$fatigue.5=="fatigue"]<-1
wideimp$fatigue.5n[wideimp$fatigue.5=="well"]<-0
table(wideimp$fatigue.5n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.5n<-NA
wideimp$sleep.5n[wideimp$sleep.5=="bad sleep"]<-1
wideimp$sleep.5n[wideimp$sleep.5=="well sleep"]<-0
table(wideimp$sleep.5n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.5)
wideimp$cough.5n<-NA
wideimp$cough.5n[wideimp$cough.5=="yes"]<-1
wideimp$cough.5n[wideimp$cough.5=="no"]<-0
table(wideimp$cough.5n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.5)
wideimp$cough_plus.5n<-NA
wideimp$cough_plus.5n[wideimp$cough_plus.5=="yes"]<-1
wideimp$cough_plus.5n[wideimp$cough_plus.5=="no"]<-0
table(wideimp$cough_plus.5n)

# 5 Sore throat QM6 

summary(wideimp$throat.5)
wideimp$throat.5n<-NA
wideimp$throat.5n[wideimp$throat.5=="yes"]<-1
wideimp$throat.5n[wideimp$throat.5=="no"]<-0
table(wideimp$throat.5n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.5)
wideimp$smell.5n<-NA
wideimp$smell.5n[wideimp$smell.5=="yes"]<-1
wideimp$smell.5n[wideimp$smell.5=="no"]<-0
table(wideimp$smell.5n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.5)
wideimp$diarrhea.5n<-NA
wideimp$diarrhea.5n[wideimp$diarrhea.5=="yes"]<-1
wideimp$diarrhea.5n[wideimp$diarrhea.5=="no"]<-0
table(wideimp$diarrhea.5n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.5)
wideimp$muscle.5n<-NA
wideimp$muscle.5n[wideimp$muscle.5=="yes"]<-1
wideimp$muscle.5n[wideimp$muscle.5=="no"]<-0
table(wideimp$muscle.5n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.5)
wideimp$chest.5n<-NA
wideimp$chest.5n[wideimp$chest.5=="yes"]<-1
wideimp$chest.5n[wideimp$chest.5=="no"]<-0
table(wideimp$chest.5n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.5)
wideimp$pain.5n<-NA
wideimp$pain.5n[wideimp$pain.5=="yes"]<-1
wideimp$pain.5n[wideimp$pain.5=="no"]<-0
table(wideimp$pain.5n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.5)
wideimp$fever.5n<-NA
wideimp$fever.5n[wideimp$fever.5=="yes"]<-1
wideimp$fever.5n[wideimp$fever.5=="no"]<-0
table(wideimp$fever.5n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.5)
wideimp$breath.5n<-NA
wideimp$breath.5n[wideimp$breath.5=="yes"]<-1
wideimp$breath.5n[wideimp$breath.5=="no"]<-0
table(wideimp$breath.5n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.5)
wideimp$breath_plus.5n<-NA
wideimp$breath_plus.5n[wideimp$breath_plus.5=="yes"]<-1
wideimp$breath_plus.5n[wideimp$breath_plus.5=="no"]<-0
table(wideimp$breath_plus.5n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.5)
wideimp$eat.5n<-NA
wideimp$eat.5n[wideimp$eat.5=="yes"]<-1
wideimp$eat.5n[wideimp$eat.5=="no"]<-0
table(wideimp$eat.5n)




# 15 Other symptoms ?
summary(wideimp$other.5)
wideimp$other.5n<-NA
wideimp$other.5n[wideimp$other.5=="yes"]<-1
wideimp$other.5n[wideimp$other.5=="no"]<-0
table(wideimp$other.5n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.5)
wideimp$skin.5n<-NA
wideimp$skin.5n[wideimp$skin.5=="yes"]<-1
wideimp$skin.5n[wideimp$skin.5=="no"]<-0
table(wideimp$skin.5n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.5)
wideimp$eyes.5n[wideimp$eyes.5=="yes"]<-1
wideimp$eyes.5n[wideimp$eyes.5=="no"]<-0
table(wideimp$eyes.5n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.5)
wideimp$hospital5[wideimp$hospital.5=="yes"]<-1
wideimp$hospital5[wideimp$hospital.5=="no"]<-0
table(wideimp$hospital5)




wideimp$score5<-NA
wideimp$score5<-wideimp$fatigue.5n + wideimp$sleep.5n + wideimp$cough.5n + 
  wideimp$cough_plus.5n + wideimp$throat.5n + wideimp$smell.5n + 
  wideimp$diarrhea.5n + wideimp$muscle.5n + wideimp$chest.5n + 
  wideimp$pain.5n + wideimp$fever.5n + wideimp$breath.5n + 
  wideimp$breath_plus.5n + wideimp$eat.5n + wideimp$other.5n + 
  wideimp$skin.5n + wideimp$eyes.5n
summary(wideimp$score5)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.6)
wideimp$fatigue.6n<-NA
wideimp$fatigue.6n[wideimp$fatigue.6=="bad"]<-2
wideimp$fatigue.6n[wideimp$fatigue.6=="fatigue"]<-1
wideimp$fatigue.6n[wideimp$fatigue.6=="well"]<-0
table(wideimp$fatigue.6n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.6n<-NA
wideimp$sleep.6n[wideimp$sleep.6=="bad sleep"]<-1
wideimp$sleep.6n[wideimp$sleep.6=="well sleep"]<-0
table(wideimp$sleep.6n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.6)
wideimp$cough.6n<-NA
wideimp$cough.6n[wideimp$cough.6=="yes"]<-1
wideimp$cough.6n[wideimp$cough.6=="no"]<-0
table(wideimp$cough.6n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.6)
wideimp$cough_plus.6n<-NA
wideimp$cough_plus.6n[wideimp$cough_plus.6=="yes"]<-1
wideimp$cough_plus.6n[wideimp$cough_plus.6=="no"]<-0
table(wideimp$cough_plus.6n)

# 5 Sore throat QM6 

summary(wideimp$throat.6)
wideimp$throat.6n<-NA
wideimp$throat.6n[wideimp$throat.6=="yes"]<-1
wideimp$throat.6n[wideimp$throat.6=="no"]<-0
table(wideimp$throat.6n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.6)
wideimp$smell.6n<-NA
wideimp$smell.6n[wideimp$smell.6=="yes"]<-1
wideimp$smell.6n[wideimp$smell.6=="no"]<-0
table(wideimp$smell.6n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.6)
wideimp$diarrhea.6n<-NA
wideimp$diarrhea.6n[wideimp$diarrhea.6=="yes"]<-1
wideimp$diarrhea.6n[wideimp$diarrhea.6=="no"]<-0
table(wideimp$diarrhea.6n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.6)
wideimp$muscle.6n<-NA
wideimp$muscle.6n[wideimp$muscle.6=="yes"]<-1
wideimp$muscle.6n[wideimp$muscle.6=="no"]<-0
table(wideimp$muscle.6n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.6)
wideimp$chest.6n<-NA
wideimp$chest.6n[wideimp$chest.6=="yes"]<-1
wideimp$chest.6n[wideimp$chest.6=="no"]<-0
table(wideimp$chest.6n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.6)
wideimp$pain.6n<-NA
wideimp$pain.6n[wideimp$pain.6=="yes"]<-1
wideimp$pain.6n[wideimp$pain.6=="no"]<-0
table(wideimp$pain.6n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.6)
wideimp$fever.6n<-NA
wideimp$fever.6n[wideimp$fever.6=="yes"]<-1
wideimp$fever.6n[wideimp$fever.6=="no"]<-0
table(wideimp$fever.6n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.6)
wideimp$breath.6n<-NA
wideimp$breath.6n[wideimp$breath.6=="yes"]<-1
wideimp$breath.6n[wideimp$breath.6=="no"]<-0
table(wideimp$breath.6n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.6)
wideimp$breath_plus.6n<-NA
wideimp$breath_plus.6n[wideimp$breath_plus.6=="yes"]<-1
wideimp$breath_plus.6n[wideimp$breath_plus.6=="no"]<-0
table(wideimp$breath_plus.6n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.6)
wideimp$eat.6n<-NA
wideimp$eat.6n[wideimp$eat.6=="yes"]<-1
wideimp$eat.6n[wideimp$eat.6=="no"]<-0
table(wideimp$eat.6n)




# 15 Other symptoms ?
summary(wideimp$other.6)
wideimp$other.6n<-NA
wideimp$other.6n[wideimp$other.6=="yes"]<-1
wideimp$other.6n[wideimp$other.6=="no"]<-0
table(wideimp$other.6n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.6)
wideimp$skin.6n<-NA
wideimp$skin.6n[wideimp$skin.6=="yes"]<-1
wideimp$skin.6n[wideimp$skin.6=="no"]<-0
table(wideimp$skin.6n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.6)
wideimp$eyes.6n[wideimp$eyes.6=="yes"]<-1
wideimp$eyes.6n[wideimp$eyes.6=="no"]<-0
table(wideimp$eyes.6n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.6)
wideimp$hospital6[wideimp$hospital.6=="yes"]<-1
wideimp$hospital6[wideimp$hospital.6=="no"]<-0
table(wideimp$hospital6)




wideimp$score6<-NA
wideimp$score6<-wideimp$fatigue.6n + wideimp$sleep.6n + wideimp$cough.6n + 
  wideimp$cough_plus.6n + wideimp$throat.6n + wideimp$smell.6n + 
  wideimp$diarrhea.6n + wideimp$muscle.6n + wideimp$chest.6n + 
  wideimp$pain.6n + wideimp$fever.6n + wideimp$breath.6n + 
  wideimp$breath_plus.6n + wideimp$eat.6n + wideimp$other.6n + 
  wideimp$skin.6n + wideimp$eyes.6n
summary(wideimp$score6)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.7)
wideimp$fatigue.7n<-NA
wideimp$fatigue.7n[wideimp$fatigue.7=="bad"]<-2
wideimp$fatigue.7n[wideimp$fatigue.7=="fatigue"]<-1
wideimp$fatigue.7n[wideimp$fatigue.7=="well"]<-0
table(wideimp$fatigue.7n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.7n<-NA
wideimp$sleep.7n[wideimp$sleep.7=="bad sleep"]<-1
wideimp$sleep.7n[wideimp$sleep.7=="well sleep"]<-0
table(wideimp$sleep.7n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.7)
wideimp$cough.7n<-NA
wideimp$cough.7n[wideimp$cough.7=="yes"]<-1
wideimp$cough.7n[wideimp$cough.7=="no"]<-0
table(wideimp$cough.7n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.7)
wideimp$cough_plus.7n<-NA
wideimp$cough_plus.7n[wideimp$cough_plus.7=="yes"]<-1
wideimp$cough_plus.7n[wideimp$cough_plus.7=="no"]<-0
table(wideimp$cough_plus.7n)

# 5 Sore throat QM6 

summary(wideimp$throat.7)
wideimp$throat.7n<-NA
wideimp$throat.7n[wideimp$throat.7=="yes"]<-1
wideimp$throat.7n[wideimp$throat.7=="no"]<-0
table(wideimp$throat.7n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.7)
wideimp$smell.7n<-NA
wideimp$smell.7n[wideimp$smell.7=="yes"]<-1
wideimp$smell.7n[wideimp$smell.7=="no"]<-0
table(wideimp$smell.7n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.7)
wideimp$diarrhea.7n<-NA
wideimp$diarrhea.7n[wideimp$diarrhea.7=="yes"]<-1
wideimp$diarrhea.7n[wideimp$diarrhea.7=="no"]<-0
table(wideimp$diarrhea.7n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.7)
wideimp$muscle.7n<-NA
wideimp$muscle.7n[wideimp$muscle.7=="yes"]<-1
wideimp$muscle.7n[wideimp$muscle.7=="no"]<-0
table(wideimp$muscle.7n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.7)
wideimp$chest.7n<-NA
wideimp$chest.7n[wideimp$chest.7=="yes"]<-1
wideimp$chest.7n[wideimp$chest.7=="no"]<-0
table(wideimp$chest.7n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.7)
wideimp$pain.7n<-NA
wideimp$pain.7n[wideimp$pain.7=="yes"]<-1
wideimp$pain.7n[wideimp$pain.7=="no"]<-0
table(wideimp$pain.7n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.7)
wideimp$fever.7n<-NA
wideimp$fever.7n[wideimp$fever.7=="yes"]<-1
wideimp$fever.7n[wideimp$fever.7=="no"]<-0
table(wideimp$fever.7n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.7)
wideimp$breath.7n<-NA
wideimp$breath.7n[wideimp$breath.7=="yes"]<-1
wideimp$breath.7n[wideimp$breath.7=="no"]<-0
table(wideimp$breath.7n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.7)
wideimp$breath_plus.7n<-NA
wideimp$breath_plus.7n[wideimp$breath_plus.7=="yes"]<-1
wideimp$breath_plus.7n[wideimp$breath_plus.7=="no"]<-0
table(wideimp$breath_plus.7n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.7)
wideimp$eat.7n<-NA
wideimp$eat.7n[wideimp$eat.7=="yes"]<-1
wideimp$eat.7n[wideimp$eat.7=="no"]<-0
table(wideimp$eat.7n)




# 15 Other symptoms ?
summary(wideimp$other.7)
wideimp$other.7n<-NA
wideimp$other.7n[wideimp$other.7=="yes"]<-1
wideimp$other.7n[wideimp$other.7=="no"]<-0
table(wideimp$other.7n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.7)
wideimp$skin.7n<-NA
wideimp$skin.7n[wideimp$skin.7=="yes"]<-1
wideimp$skin.7n[wideimp$skin.7=="no"]<-0
table(wideimp$skin.7n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.7)
wideimp$eyes.7n[wideimp$eyes.7=="yes"]<-1
wideimp$eyes.7n[wideimp$eyes.7=="no"]<-0
table(wideimp$eyes.7n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.7)
wideimp$hospital7[wideimp$hospital.7=="yes"]<-1
wideimp$hospital7[wideimp$hospital.7=="no"]<-0
table(wideimp$hospital7)




wideimp$score7<-NA
wideimp$score7<-wideimp$fatigue.7n + wideimp$sleep.7n + wideimp$cough.7n + 
  wideimp$cough_plus.7n + wideimp$throat.7n + wideimp$smell.7n + 
  wideimp$diarrhea.7n + wideimp$muscle.7n + wideimp$chest.7n + 
  wideimp$pain.7n + wideimp$fever.7n + wideimp$breath.7n + 
  wideimp$breath_plus.7n + wideimp$eat.7n + wideimp$other.7n + 
  wideimp$skin.7n + wideimp$eyes.7n
summary(wideimp$score7)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.8)
wideimp$fatigue.8n<-NA
wideimp$fatigue.8n[wideimp$fatigue.8=="bad"]<-2
wideimp$fatigue.8n[wideimp$fatigue.8=="fatigue"]<-1
wideimp$fatigue.8n[wideimp$fatigue.8=="well"]<-0
table(wideimp$fatigue.8n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.8n<-NA
wideimp$sleep.8n[wideimp$sleep.8=="bad sleep"]<-1
wideimp$sleep.8n[wideimp$sleep.8=="well sleep"]<-0
table(wideimp$sleep.8n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.8)
wideimp$cough.8n<-NA
wideimp$cough.8n[wideimp$cough.8=="yes"]<-1
wideimp$cough.8n[wideimp$cough.8=="no"]<-0
table(wideimp$cough.8n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.8)
wideimp$cough_plus.8n<-NA
wideimp$cough_plus.8n[wideimp$cough_plus.8=="yes"]<-1
wideimp$cough_plus.8n[wideimp$cough_plus.8=="no"]<-0
table(wideimp$cough_plus.8n)

# 5 Sore throat QM6 

summary(wideimp$throat.8)
wideimp$throat.8n<-NA
wideimp$throat.8n[wideimp$throat.8=="yes"]<-1
wideimp$throat.8n[wideimp$throat.8=="no"]<-0
table(wideimp$throat.8n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.8)
wideimp$smell.8n<-NA
wideimp$smell.8n[wideimp$smell.8=="yes"]<-1
wideimp$smell.8n[wideimp$smell.8=="no"]<-0
table(wideimp$smell.8n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.8)
wideimp$diarrhea.8n<-NA
wideimp$diarrhea.8n[wideimp$diarrhea.8=="yes"]<-1
wideimp$diarrhea.8n[wideimp$diarrhea.8=="no"]<-0
table(wideimp$diarrhea.8n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.8)
wideimp$muscle.8n<-NA
wideimp$muscle.8n[wideimp$muscle.8=="yes"]<-1
wideimp$muscle.8n[wideimp$muscle.8=="no"]<-0
table(wideimp$muscle.8n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.8)
wideimp$chest.8n<-NA
wideimp$chest.8n[wideimp$chest.8=="yes"]<-1
wideimp$chest.8n[wideimp$chest.8=="no"]<-0
table(wideimp$chest.8n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.8)
wideimp$pain.8n<-NA
wideimp$pain.8n[wideimp$pain.8=="yes"]<-1
wideimp$pain.8n[wideimp$pain.8=="no"]<-0
table(wideimp$pain.8n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.8)
wideimp$fever.8n<-NA
wideimp$fever.8n[wideimp$fever.8=="yes"]<-1
wideimp$fever.8n[wideimp$fever.8=="no"]<-0
table(wideimp$fever.8n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.8)
wideimp$breath.8n<-NA
wideimp$breath.8n[wideimp$breath.8=="yes"]<-1
wideimp$breath.8n[wideimp$breath.8=="no"]<-0
table(wideimp$breath.8n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.8)
wideimp$breath_plus.8n<-NA
wideimp$breath_plus.8n[wideimp$breath_plus.8=="yes"]<-1
wideimp$breath_plus.8n[wideimp$breath_plus.8=="no"]<-0
table(wideimp$breath_plus.8n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.8)
wideimp$eat.8n<-NA
wideimp$eat.8n[wideimp$eat.8=="yes"]<-1
wideimp$eat.8n[wideimp$eat.8=="no"]<-0
table(wideimp$eat.8n)




# 15 Other symptoms ?
summary(wideimp$other.8)
wideimp$other.8n<-NA
wideimp$other.8n[wideimp$other.8=="yes"]<-1
wideimp$other.8n[wideimp$other.8=="no"]<-0
table(wideimp$other.8n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.8)
wideimp$skin.8n<-NA
wideimp$skin.8n[wideimp$skin.8=="yes"]<-1
wideimp$skin.8n[wideimp$skin.8=="no"]<-0
table(wideimp$skin.8n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.8)
wideimp$eyes.8n[wideimp$eyes.8=="yes"]<-1
wideimp$eyes.8n[wideimp$eyes.8=="no"]<-0
table(wideimp$eyes.8n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.8)
wideimp$hospital8[wideimp$hospital.8=="yes"]<-1
wideimp$hospital8[wideimp$hospital.8=="no"]<-0
table(wideimp$hospital8)




wideimp$score8<-NA
wideimp$score8<-wideimp$fatigue.8n + wideimp$sleep.8n + wideimp$cough.8n + 
  wideimp$cough_plus.8n + wideimp$throat.8n + wideimp$smell.8n + 
  wideimp$diarrhea.8n + wideimp$muscle.8n + wideimp$chest.8n + 
  wideimp$pain.8n + wideimp$fever.8n + wideimp$breath.8n + 
  wideimp$breath_plus.8n + wideimp$eat.8n + wideimp$other.8n + 
  wideimp$skin.8n + wideimp$eyes.8n
summary(wideimp$score8)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.9)
wideimp$fatigue.9n<-NA
wideimp$fatigue.9n[wideimp$fatigue.9=="bad"]<-2
wideimp$fatigue.9n[wideimp$fatigue.9=="fatigue"]<-1
wideimp$fatigue.9n[wideimp$fatigue.9=="well"]<-0
table(wideimp$fatigue.9n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.9n<-NA
wideimp$sleep.9n[wideimp$sleep.9=="bad sleep"]<-1
wideimp$sleep.9n[wideimp$sleep.9=="well sleep"]<-0
table(wideimp$sleep.9n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.9)
wideimp$cough.9n<-NA
wideimp$cough.9n[wideimp$cough.9=="yes"]<-1
wideimp$cough.9n[wideimp$cough.9=="no"]<-0
table(wideimp$cough.9n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.9)
wideimp$cough_plus.9n<-NA
wideimp$cough_plus.9n[wideimp$cough_plus.9=="yes"]<-1
wideimp$cough_plus.9n[wideimp$cough_plus.9=="no"]<-0
table(wideimp$cough_plus.9n)

# 5 Sore throat QM6 

summary(wideimp$throat.9)
wideimp$throat.9n<-NA
wideimp$throat.9n[wideimp$throat.9=="yes"]<-1
wideimp$throat.9n[wideimp$throat.9=="no"]<-0
table(wideimp$throat.9n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.9)
wideimp$smell.9n<-NA
wideimp$smell.9n[wideimp$smell.9=="yes"]<-1
wideimp$smell.9n[wideimp$smell.9=="no"]<-0
table(wideimp$smell.9n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.9)
wideimp$diarrhea.9n<-NA
wideimp$diarrhea.9n[wideimp$diarrhea.9=="yes"]<-1
wideimp$diarrhea.9n[wideimp$diarrhea.9=="no"]<-0
table(wideimp$diarrhea.9n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.9)
wideimp$muscle.9n<-NA
wideimp$muscle.9n[wideimp$muscle.9=="yes"]<-1
wideimp$muscle.9n[wideimp$muscle.9=="no"]<-0
table(wideimp$muscle.9n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.9)
wideimp$chest.9n<-NA
wideimp$chest.9n[wideimp$chest.9=="yes"]<-1
wideimp$chest.9n[wideimp$chest.9=="no"]<-0
table(wideimp$chest.9n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.9)
wideimp$pain.9n<-NA
wideimp$pain.9n[wideimp$pain.9=="yes"]<-1
wideimp$pain.9n[wideimp$pain.9=="no"]<-0
table(wideimp$pain.9n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.9)
wideimp$fever.9n<-NA
wideimp$fever.9n[wideimp$fever.9=="yes"]<-1
wideimp$fever.9n[wideimp$fever.9=="no"]<-0
table(wideimp$fever.9n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.9)
wideimp$breath.9n<-NA
wideimp$breath.9n[wideimp$breath.9=="yes"]<-1
wideimp$breath.9n[wideimp$breath.9=="no"]<-0
table(wideimp$breath.9n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.9)
wideimp$breath_plus.9n<-NA
wideimp$breath_plus.9n[wideimp$breath_plus.9=="yes"]<-1
wideimp$breath_plus.9n[wideimp$breath_plus.9=="no"]<-0
table(wideimp$breath_plus.9n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.9)
wideimp$eat.9n<-NA
wideimp$eat.9n[wideimp$eat.9=="yes"]<-1
wideimp$eat.9n[wideimp$eat.9=="no"]<-0
table(wideimp$eat.9n)




# 15 Other symptoms ?
summary(wideimp$other.9)
wideimp$other.9n<-NA
wideimp$other.9n[wideimp$other.9=="yes"]<-1
wideimp$other.9n[wideimp$other.9=="no"]<-0
table(wideimp$other.9n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.9)
wideimp$skin.9n<-NA
wideimp$skin.9n[wideimp$skin.9=="yes"]<-1
wideimp$skin.9n[wideimp$skin.9=="no"]<-0
table(wideimp$skin.9n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.9)
wideimp$eyes.9n[wideimp$eyes.9=="yes"]<-1
wideimp$eyes.9n[wideimp$eyes.9=="no"]<-0
table(wideimp$eyes.9n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.9)
wideimp$hospital9[wideimp$hospital.9=="yes"]<-1
wideimp$hospital9[wideimp$hospital.9=="no"]<-0
table(wideimp$hospital9)




wideimp$score9<-NA
wideimp$score9<-wideimp$fatigue.9n + wideimp$sleep.9n + wideimp$cough.9n + 
  wideimp$cough_plus.9n + wideimp$throat.9n + wideimp$smell.9n + 
  wideimp$diarrhea.9n + wideimp$muscle.9n + wideimp$chest.9n + 
  wideimp$pain.9n + wideimp$fever.9n + wideimp$breath.9n + 
  wideimp$breath_plus.9n + wideimp$eat.9n + wideimp$other.9n + 
  wideimp$skin.9n + wideimp$eyes.9n
summary(wideimp$score9)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.10)
wideimp$fatigue.10n<-NA
wideimp$fatigue.10n[wideimp$fatigue.10=="bad"]<-2
wideimp$fatigue.10n[wideimp$fatigue.10=="fatigue"]<-1
wideimp$fatigue.10n[wideimp$fatigue.10=="well"]<-0
table(wideimp$fatigue.10n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.10n<-NA
wideimp$sleep.10n[wideimp$sleep.10=="bad sleep"]<-1
wideimp$sleep.10n[wideimp$sleep.10=="well sleep"]<-0
table(wideimp$sleep.10n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.10)
wideimp$cough.10n<-NA
wideimp$cough.10n[wideimp$cough.10=="yes"]<-1
wideimp$cough.10n[wideimp$cough.10=="no"]<-0
table(wideimp$cough.10n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.10)
wideimp$cough_plus.10n<-NA
wideimp$cough_plus.10n[wideimp$cough_plus.10=="yes"]<-1
wideimp$cough_plus.10n[wideimp$cough_plus.10=="no"]<-0
table(wideimp$cough_plus.10n)

# 5 Sore throat QM6 

summary(wideimp$throat.10)
wideimp$throat.10n<-NA
wideimp$throat.10n[wideimp$throat.10=="yes"]<-1
wideimp$throat.10n[wideimp$throat.10=="no"]<-0
table(wideimp$throat.10n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.10)
wideimp$smell.10n<-NA
wideimp$smell.10n[wideimp$smell.10=="yes"]<-1
wideimp$smell.10n[wideimp$smell.10=="no"]<-0
table(wideimp$smell.10n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.10)
wideimp$diarrhea.10n<-NA
wideimp$diarrhea.10n[wideimp$diarrhea.10=="yes"]<-1
wideimp$diarrhea.10n[wideimp$diarrhea.10=="no"]<-0
table(wideimp$diarrhea.10n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.10)
wideimp$muscle.10n<-NA
wideimp$muscle.10n[wideimp$muscle.10=="yes"]<-1
wideimp$muscle.10n[wideimp$muscle.10=="no"]<-0
table(wideimp$muscle.10n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.10)
wideimp$chest.10n<-NA
wideimp$chest.10n[wideimp$chest.10=="yes"]<-1
wideimp$chest.10n[wideimp$chest.10=="no"]<-0
table(wideimp$chest.10n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.10)
wideimp$pain.10n<-NA
wideimp$pain.10n[wideimp$pain.10=="yes"]<-1
wideimp$pain.10n[wideimp$pain.10=="no"]<-0
table(wideimp$pain.10n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.10)
wideimp$fever.10n<-NA
wideimp$fever.10n[wideimp$fever.10=="yes"]<-1
wideimp$fever.10n[wideimp$fever.10=="no"]<-0
table(wideimp$fever.10n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.10)
wideimp$breath.10n<-NA
wideimp$breath.10n[wideimp$breath.10=="yes"]<-1
wideimp$breath.10n[wideimp$breath.10=="no"]<-0
table(wideimp$breath.10n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.10)
wideimp$breath_plus.10n<-NA
wideimp$breath_plus.10n[wideimp$breath_plus.10=="yes"]<-1
wideimp$breath_plus.10n[wideimp$breath_plus.10=="no"]<-0
table(wideimp$breath_plus.10n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.10)
wideimp$eat.10n<-NA
wideimp$eat.10n[wideimp$eat.10=="yes"]<-1
wideimp$eat.10n[wideimp$eat.10=="no"]<-0
table(wideimp$eat.10n)




# 15 Other symptoms ?
summary(wideimp$other.10)
wideimp$other.10n<-NA
wideimp$other.10n[wideimp$other.10=="yes"]<-1
wideimp$other.10n[wideimp$other.10=="no"]<-0
table(wideimp$other.10n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.10)
wideimp$skin.10n<-NA
wideimp$skin.10n[wideimp$skin.10=="yes"]<-1
wideimp$skin.10n[wideimp$skin.10=="no"]<-0
table(wideimp$skin.10n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.10)
wideimp$eyes.10n[wideimp$eyes.10=="yes"]<-1
wideimp$eyes.10n[wideimp$eyes.10=="no"]<-0
table(wideimp$eyes.10n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.10)
wideimp$hospital10[wideimp$hospital.10=="yes"]<-1
wideimp$hospital10[wideimp$hospital.10=="no"]<-0
table(wideimp$hospital10)




wideimp$score10<-NA
wideimp$score10<-wideimp$fatigue.10n + wideimp$sleep.10n + wideimp$cough.10n + 
  wideimp$cough_plus.10n + wideimp$throat.10n + wideimp$smell.10n + 
  wideimp$diarrhea.10n + wideimp$muscle.10n + wideimp$chest.10n + 
  wideimp$pain.10n + wideimp$fever.10n + wideimp$breath.10n + 
  wideimp$breath_plus.10n + wideimp$eat.10n + wideimp$other.10n + 
  wideimp$skin.10n + wideimp$eyes.10n
summary(wideimp$score10)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.11)
wideimp$fatigue.11n<-NA
wideimp$fatigue.11n[wideimp$fatigue.11=="bad"]<-2
wideimp$fatigue.11n[wideimp$fatigue.11=="fatigue"]<-1
wideimp$fatigue.11n[wideimp$fatigue.11=="well"]<-0
table(wideimp$fatigue.11n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.11n<-NA
wideimp$sleep.11n[wideimp$sleep.11=="bad sleep"]<-1
wideimp$sleep.11n[wideimp$sleep.11=="well sleep"]<-0
table(wideimp$sleep.11n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.11)
wideimp$cough.11n<-NA
wideimp$cough.11n[wideimp$cough.11=="yes"]<-1
wideimp$cough.11n[wideimp$cough.11=="no"]<-0
table(wideimp$cough.11n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.11)
wideimp$cough_plus.11n<-NA
wideimp$cough_plus.11n[wideimp$cough_plus.11=="yes"]<-1
wideimp$cough_plus.11n[wideimp$cough_plus.11=="no"]<-0
table(wideimp$cough_plus.11n)

# 5 Sore throat QM6 

summary(wideimp$throat.11)
wideimp$throat.11n<-NA
wideimp$throat.11n[wideimp$throat.11=="yes"]<-1
wideimp$throat.11n[wideimp$throat.11=="no"]<-0
table(wideimp$throat.11n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.11)
wideimp$smell.11n<-NA
wideimp$smell.11n[wideimp$smell.11=="yes"]<-1
wideimp$smell.11n[wideimp$smell.11=="no"]<-0
table(wideimp$smell.11n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.11)
wideimp$diarrhea.11n<-NA
wideimp$diarrhea.11n[wideimp$diarrhea.11=="yes"]<-1
wideimp$diarrhea.11n[wideimp$diarrhea.11=="no"]<-0
table(wideimp$diarrhea.11n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.11)
wideimp$muscle.11n<-NA
wideimp$muscle.11n[wideimp$muscle.11=="yes"]<-1
wideimp$muscle.11n[wideimp$muscle.11=="no"]<-0
table(wideimp$muscle.11n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.11)
wideimp$chest.11n<-NA
wideimp$chest.11n[wideimp$chest.11=="yes"]<-1
wideimp$chest.11n[wideimp$chest.11=="no"]<-0
table(wideimp$chest.11n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.11)
wideimp$pain.11n<-NA
wideimp$pain.11n[wideimp$pain.11=="yes"]<-1
wideimp$pain.11n[wideimp$pain.11=="no"]<-0
table(wideimp$pain.11n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.11)
wideimp$fever.11n<-NA
wideimp$fever.11n[wideimp$fever.11=="yes"]<-1
wideimp$fever.11n[wideimp$fever.11=="no"]<-0
table(wideimp$fever.11n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.11)
wideimp$breath.11n<-NA
wideimp$breath.11n[wideimp$breath.11=="yes"]<-1
wideimp$breath.11n[wideimp$breath.11=="no"]<-0
table(wideimp$breath.11n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.11)
wideimp$breath_plus.11n<-NA
wideimp$breath_plus.11n[wideimp$breath_plus.11=="yes"]<-1
wideimp$breath_plus.11n[wideimp$breath_plus.11=="no"]<-0
table(wideimp$breath_plus.11n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.11)
wideimp$eat.11n<-NA
wideimp$eat.11n[wideimp$eat.11=="yes"]<-1
wideimp$eat.11n[wideimp$eat.11=="no"]<-0
table(wideimp$eat.11n)




# 15 Other symptoms ?
summary(wideimp$other.11)
wideimp$other.11n<-NA
wideimp$other.11n[wideimp$other.11=="yes"]<-1
wideimp$other.11n[wideimp$other.11=="no"]<-0
table(wideimp$other.11n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.11)
wideimp$skin.11n<-NA
wideimp$skin.11n[wideimp$skin.11=="yes"]<-1
wideimp$skin.11n[wideimp$skin.11=="no"]<-0
table(wideimp$skin.11n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.11)
wideimp$eyes.11n[wideimp$eyes.11=="yes"]<-1
wideimp$eyes.11n[wideimp$eyes.11=="no"]<-0
table(wideimp$eyes.11n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.11)
wideimp$hospital11[wideimp$hospital.11=="yes"]<-1
wideimp$hospital11[wideimp$hospital.11=="no"]<-0
table(wideimp$hospital11)




wideimp$score11<-NA
wideimp$score11<-wideimp$fatigue.11n + wideimp$sleep.11n + wideimp$cough.11n + 
  wideimp$cough_plus.11n + wideimp$throat.11n + wideimp$smell.11n + 
  wideimp$diarrhea.11n + wideimp$muscle.11n + wideimp$chest.11n + 
  wideimp$pain.11n + wideimp$fever.11n + wideimp$breath.11n + 
  wideimp$breath_plus.11n + wideimp$eat.11n + wideimp$other.11n + 
  wideimp$skin.11n + wideimp$eyes.11n
summary(wideimp$score11)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.12)
wideimp$fatigue.12n<-NA
wideimp$fatigue.12n[wideimp$fatigue.12=="bad"]<-2
wideimp$fatigue.12n[wideimp$fatigue.12=="fatigue"]<-1
wideimp$fatigue.12n[wideimp$fatigue.12=="well"]<-0
table(wideimp$fatigue.12n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.12n<-NA
wideimp$sleep.12n[wideimp$sleep.12=="bad sleep"]<-1
wideimp$sleep.12n[wideimp$sleep.12=="well sleep"]<-0
table(wideimp$sleep.12n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.12)
wideimp$cough.12n<-NA
wideimp$cough.12n[wideimp$cough.12=="yes"]<-1
wideimp$cough.12n[wideimp$cough.12=="no"]<-0
table(wideimp$cough.12n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.12)
wideimp$cough_plus.12n<-NA
wideimp$cough_plus.12n[wideimp$cough_plus.12=="yes"]<-1
wideimp$cough_plus.12n[wideimp$cough_plus.12=="no"]<-0
table(wideimp$cough_plus.12n)

# 5 Sore throat QM6 

summary(wideimp$throat.12)
wideimp$throat.12n<-NA
wideimp$throat.12n[wideimp$throat.12=="yes"]<-1
wideimp$throat.12n[wideimp$throat.12=="no"]<-0
table(wideimp$throat.12n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.12)
wideimp$smell.12n<-NA
wideimp$smell.12n[wideimp$smell.12=="yes"]<-1
wideimp$smell.12n[wideimp$smell.12=="no"]<-0
table(wideimp$smell.12n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.12)
wideimp$diarrhea.12n<-NA
wideimp$diarrhea.12n[wideimp$diarrhea.12=="yes"]<-1
wideimp$diarrhea.12n[wideimp$diarrhea.12=="no"]<-0
table(wideimp$diarrhea.12n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.12)
wideimp$muscle.12n<-NA
wideimp$muscle.12n[wideimp$muscle.12=="yes"]<-1
wideimp$muscle.12n[wideimp$muscle.12=="no"]<-0
table(wideimp$muscle.12n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.12)
wideimp$chest.12n<-NA
wideimp$chest.12n[wideimp$chest.12=="yes"]<-1
wideimp$chest.12n[wideimp$chest.12=="no"]<-0
table(wideimp$chest.12n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.12)
wideimp$pain.12n<-NA
wideimp$pain.12n[wideimp$pain.12=="yes"]<-1
wideimp$pain.12n[wideimp$pain.12=="no"]<-0
table(wideimp$pain.12n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.12)
wideimp$fever.12n<-NA
wideimp$fever.12n[wideimp$fever.12=="yes"]<-1
wideimp$fever.12n[wideimp$fever.12=="no"]<-0
table(wideimp$fever.12n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.12)
wideimp$breath.12n<-NA
wideimp$breath.12n[wideimp$breath.12=="yes"]<-1
wideimp$breath.12n[wideimp$breath.12=="no"]<-0
table(wideimp$breath.12n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.12)
wideimp$breath_plus.12n<-NA
wideimp$breath_plus.12n[wideimp$breath_plus.12=="yes"]<-1
wideimp$breath_plus.12n[wideimp$breath_plus.12=="no"]<-0
table(wideimp$breath_plus.12n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.12)
wideimp$eat.12n<-NA
wideimp$eat.12n[wideimp$eat.12=="yes"]<-1
wideimp$eat.12n[wideimp$eat.12=="no"]<-0
table(wideimp$eat.12n)




# 15 Other symptoms ?
summary(wideimp$other.12)
wideimp$other.12n<-NA
wideimp$other.12n[wideimp$other.12=="yes"]<-1
wideimp$other.12n[wideimp$other.12=="no"]<-0
table(wideimp$other.12n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.12)
wideimp$skin.12n<-NA
wideimp$skin.12n[wideimp$skin.12=="yes"]<-1
wideimp$skin.12n[wideimp$skin.12=="no"]<-0
table(wideimp$skin.12n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.12)
wideimp$eyes.12n[wideimp$eyes.12=="yes"]<-1
wideimp$eyes.12n[wideimp$eyes.12=="no"]<-0
table(wideimp$eyes.12n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.12)
wideimp$hospital12[wideimp$hospital.12=="yes"]<-1
wideimp$hospital12[wideimp$hospital.12=="no"]<-0
table(wideimp$hospital12)




wideimp$score12<-NA
wideimp$score12<-wideimp$fatigue.12n + wideimp$sleep.12n + wideimp$cough.12n + 
  wideimp$cough_plus.12n + wideimp$throat.12n + wideimp$smell.12n + 
  wideimp$diarrhea.12n + wideimp$muscle.12n + wideimp$chest.12n + 
  wideimp$pain.12n + wideimp$fever.12n + wideimp$breath.12n + 
  wideimp$breath_plus.12n + wideimp$eat.12n + wideimp$other.12n + 
  wideimp$skin.12n + wideimp$eyes.12n
summary(wideimp$score12)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.13)
wideimp$fatigue.13n<-NA
wideimp$fatigue.13n[wideimp$fatigue.13=="bad"]<-2
wideimp$fatigue.13n[wideimp$fatigue.13=="fatigue"]<-1
wideimp$fatigue.13n[wideimp$fatigue.13=="well"]<-0
table(wideimp$fatigue.13n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.13n<-NA
wideimp$sleep.13n[wideimp$sleep.13=="bad sleep"]<-1
wideimp$sleep.13n[wideimp$sleep.13=="well sleep"]<-0
table(wideimp$sleep.13n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.13)
wideimp$cough.13n<-NA
wideimp$cough.13n[wideimp$cough.13=="yes"]<-1
wideimp$cough.13n[wideimp$cough.13=="no"]<-0
table(wideimp$cough.13n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.13)
wideimp$cough_plus.13n<-NA
wideimp$cough_plus.13n[wideimp$cough_plus.13=="yes"]<-1
wideimp$cough_plus.13n[wideimp$cough_plus.13=="no"]<-0
table(wideimp$cough_plus.13n)

# 5 Sore throat QM6 

summary(wideimp$throat.13)
wideimp$throat.13n<-NA
wideimp$throat.13n[wideimp$throat.13=="yes"]<-1
wideimp$throat.13n[wideimp$throat.13=="no"]<-0
table(wideimp$throat.13n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.13)
wideimp$smell.13n<-NA
wideimp$smell.13n[wideimp$smell.13=="yes"]<-1
wideimp$smell.13n[wideimp$smell.13=="no"]<-0
table(wideimp$smell.13n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.13)
wideimp$diarrhea.13n<-NA
wideimp$diarrhea.13n[wideimp$diarrhea.13=="yes"]<-1
wideimp$diarrhea.13n[wideimp$diarrhea.13=="no"]<-0
table(wideimp$diarrhea.13n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.13)
wideimp$muscle.13n<-NA
wideimp$muscle.13n[wideimp$muscle.13=="yes"]<-1
wideimp$muscle.13n[wideimp$muscle.13=="no"]<-0
table(wideimp$muscle.13n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.13)
wideimp$chest.13n<-NA
wideimp$chest.13n[wideimp$chest.13=="yes"]<-1
wideimp$chest.13n[wideimp$chest.13=="no"]<-0
table(wideimp$chest.13n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.13)
wideimp$pain.13n<-NA
wideimp$pain.13n[wideimp$pain.13=="yes"]<-1
wideimp$pain.13n[wideimp$pain.13=="no"]<-0
table(wideimp$pain.13n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.13)
wideimp$fever.13n<-NA
wideimp$fever.13n[wideimp$fever.13=="yes"]<-1
wideimp$fever.13n[wideimp$fever.13=="no"]<-0
table(wideimp$fever.13n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.13)
wideimp$breath.13n<-NA
wideimp$breath.13n[wideimp$breath.13=="yes"]<-1
wideimp$breath.13n[wideimp$breath.13=="no"]<-0
table(wideimp$breath.13n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.13)
wideimp$breath_plus.13n<-NA
wideimp$breath_plus.13n[wideimp$breath_plus.13=="yes"]<-1
wideimp$breath_plus.13n[wideimp$breath_plus.13=="no"]<-0
table(wideimp$breath_plus.13n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.13)
wideimp$eat.13n<-NA
wideimp$eat.13n[wideimp$eat.13=="yes"]<-1
wideimp$eat.13n[wideimp$eat.13=="no"]<-0
table(wideimp$eat.13n)




# 15 Other symptoms ?
summary(wideimp$other.13)
wideimp$other.13n<-NA
wideimp$other.13n[wideimp$other.13=="yes"]<-1
wideimp$other.13n[wideimp$other.13=="no"]<-0
table(wideimp$other.13n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.13)
wideimp$skin.13n<-NA
wideimp$skin.13n[wideimp$skin.13=="yes"]<-1
wideimp$skin.13n[wideimp$skin.13=="no"]<-0
table(wideimp$skin.13n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.13)
wideimp$eyes.13n[wideimp$eyes.13=="yes"]<-1
wideimp$eyes.13n[wideimp$eyes.13=="no"]<-0
table(wideimp$eyes.13n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.13)
wideimp$hospital13[wideimp$hospital.13=="yes"]<-1
wideimp$hospital13[wideimp$hospital.13=="no"]<-0
table(wideimp$hospital13)




wideimp$score13<-NA
wideimp$score13<-wideimp$fatigue.13n + wideimp$sleep.13n + wideimp$cough.13n + 
  wideimp$cough_plus.13n + wideimp$throat.13n + wideimp$smell.13n + 
  wideimp$diarrhea.13n + wideimp$muscle.13n + wideimp$chest.13n + 
  wideimp$pain.13n + wideimp$fever.13n + wideimp$breath.13n + 
  wideimp$breath_plus.13n + wideimp$eat.13n + wideimp$other.13n + 
  wideimp$skin.13n + wideimp$eyes.13n
summary(wideimp$score13)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.14)
wideimp$fatigue.14n<-NA
wideimp$fatigue.14n[wideimp$fatigue.14=="bad"]<-2
wideimp$fatigue.14n[wideimp$fatigue.14=="fatigue"]<-1
wideimp$fatigue.14n[wideimp$fatigue.14=="well"]<-0
table(wideimp$fatigue.14n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.14n<-NA
wideimp$sleep.14n[wideimp$sleep.14=="bad sleep"]<-1
wideimp$sleep.14n[wideimp$sleep.14=="well sleep"]<-0
table(wideimp$sleep.14n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.14)
wideimp$cough.14n<-NA
wideimp$cough.14n[wideimp$cough.14=="yes"]<-1
wideimp$cough.14n[wideimp$cough.14=="no"]<-0
table(wideimp$cough.14n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.14)
wideimp$cough_plus.14n<-NA
wideimp$cough_plus.14n[wideimp$cough_plus.14=="yes"]<-1
wideimp$cough_plus.14n[wideimp$cough_plus.14=="no"]<-0
table(wideimp$cough_plus.14n)

# 5 Sore throat QM6 

summary(wideimp$throat.14)
wideimp$throat.14n<-NA
wideimp$throat.14n[wideimp$throat.14=="yes"]<-1
wideimp$throat.14n[wideimp$throat.14=="no"]<-0
table(wideimp$throat.14n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.14)
wideimp$smell.14n<-NA
wideimp$smell.14n[wideimp$smell.14=="yes"]<-1
wideimp$smell.14n[wideimp$smell.14=="no"]<-0
table(wideimp$smell.14n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.14)
wideimp$diarrhea.14n<-NA
wideimp$diarrhea.14n[wideimp$diarrhea.14=="yes"]<-1
wideimp$diarrhea.14n[wideimp$diarrhea.14=="no"]<-0
table(wideimp$diarrhea.14n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.14)
wideimp$muscle.14n<-NA
wideimp$muscle.14n[wideimp$muscle.14=="yes"]<-1
wideimp$muscle.14n[wideimp$muscle.14=="no"]<-0
table(wideimp$muscle.14n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.14)
wideimp$chest.14n<-NA
wideimp$chest.14n[wideimp$chest.14=="yes"]<-1
wideimp$chest.14n[wideimp$chest.14=="no"]<-0
table(wideimp$chest.14n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.14)
wideimp$pain.14n<-NA
wideimp$pain.14n[wideimp$pain.14=="yes"]<-1
wideimp$pain.14n[wideimp$pain.14=="no"]<-0
table(wideimp$pain.14n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.14)
wideimp$fever.14n<-NA
wideimp$fever.14n[wideimp$fever.14=="yes"]<-1
wideimp$fever.14n[wideimp$fever.14=="no"]<-0
table(wideimp$fever.14n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.14)
wideimp$breath.14n<-NA
wideimp$breath.14n[wideimp$breath.14=="yes"]<-1
wideimp$breath.14n[wideimp$breath.14=="no"]<-0
table(wideimp$breath.14n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.14)
wideimp$breath_plus.14n<-NA
wideimp$breath_plus.14n[wideimp$breath_plus.14=="yes"]<-1
wideimp$breath_plus.14n[wideimp$breath_plus.14=="no"]<-0
table(wideimp$breath_plus.14n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.14)
wideimp$eat.14n<-NA
wideimp$eat.14n[wideimp$eat.14=="yes"]<-1
wideimp$eat.14n[wideimp$eat.14=="no"]<-0
table(wideimp$eat.14n)




# 15 Other symptoms ?
summary(wideimp$other.14)
wideimp$other.14n<-NA
wideimp$other.14n[wideimp$other.14=="yes"]<-1
wideimp$other.14n[wideimp$other.14=="no"]<-0
table(wideimp$other.14n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.14)
wideimp$skin.14n<-NA
wideimp$skin.14n[wideimp$skin.14=="yes"]<-1
wideimp$skin.14n[wideimp$skin.14=="no"]<-0
table(wideimp$skin.14n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.14)
wideimp$eyes.14n[wideimp$eyes.14=="yes"]<-1
wideimp$eyes.14n[wideimp$eyes.14=="no"]<-0
table(wideimp$eyes.14n)
# 
# # 18. Vous etes a la maison, ? l'hopital


table(wideimp$hospital.14)
wideimp$hospital14[wideimp$hospital.14=="yes"]<-1
wideimp$hospital14[wideimp$hospital.14=="no"]<-0
table(wideimp$hospital14)




wideimp$score14<-NA
wideimp$score14<-wideimp$fatigue.14n + wideimp$sleep.14n + wideimp$cough.14n + 
  wideimp$cough_plus.14n + wideimp$throat.14n + wideimp$smell.14n + 
  wideimp$diarrhea.14n + wideimp$muscle.14n + wideimp$chest.14n + 
  wideimp$pain.14n + wideimp$fever.14n + wideimp$breath.14n + 
  wideimp$breath_plus.14n + wideimp$eat.14n + wideimp$other.14n + 
  wideimp$skin.14n + wideimp$eyes.14n
summary(wideimp$score14)

dim(wideimp)

# Making NA again when all responses were NA

wideimp$score0[wideimp$miss0=="miss"]<-NA
wideimp$score1[wideimp$miss1=="miss"]<-NA
wideimp$score2[wideimp$miss2=="miss"]<-NA
wideimp$score3[wideimp$miss3=="miss"]<-NA
wideimp$score4[wideimp$miss4=="miss"]<-NA
wideimp$score5[wideimp$miss5=="miss"]<-NA
wideimp$score6[wideimp$miss6=="miss"]<-NA
wideimp$score7[wideimp$miss7=="miss"]<-NA
wideimp$score8[wideimp$miss8=="miss"]<-NA
wideimp$score9[wideimp$miss9=="miss"]<-NA
wideimp$score10[wideimp$miss10=="miss"]<-NA
wideimp$score11[wideimp$miss11=="miss"]<-NA
wideimp$score12[wideimp$miss12=="miss"]<-NA
wideimp$score13[wideimp$miss13=="miss"]<-NA
wideimp$score14[wideimp$miss14=="miss"]<-NA

wideimp$diabetesn<-NA
wideimp$diabetesn[wideimp$diabetes=="yes"]<-1
wideimp$diabetesn[wideimp$diabetes=="no"]<-0


wideimp$char<-NA
wideimp$char<-wideimp$hypertention_mhyn + wideimp$chroniccard_mhyn + wideimp$chronicpul_mhyn + wideimp$asthma_mhyn + 
  wideimp$renal_mhyn + wideimp$modliver_mhyn + wideimp$mildliv_mhyn + 
  wideimp$chronicneu_mhyn + wideimp$malignantneo_mhyn + wideimp$chronhaemo_mhyn + 
  wideimp$obesity_mhyn + wideimp$diabetesn + wideimp$rheumatology_mhyr + 
  wideimp$malnutrition_mhyn +wideimp$copd_mhyn+wideimp$other_mhyn
table(wideimp$char, useNA = "ifany")

wideimp$multimorb<-NA
wideimp$multimorb<-ifelse(wideimp$char<=1,"no","yes")
wideimp$multimorb<-factor(wideimp$multimorb)
table(wideimp$multimorb,useNA = "ifany")
round(100*prop.table(table(wideimp$multimorb)))


# Diabetes

table(wideimp$diabetes_mhyn,useNA = "ifany")
table(wideimp$traitement_two, useNA = "ifany")
wideimp$diabetes<-NA
wideimp$diabetes[wideimp$diabetes_mhyn==1 | wideimp$traitement_two==1]<-1
wideimp$diabetes[wideimp$diabetes_mhyn==2 & wideimp$traitement_two==0]<-0
wideimp$diabetes<-factor(wideimp$diabetes,levels = c(1,0), labels = c("yes","no"))
table(wideimp$diabetes, useNA = "ifany")
round(100*prop.table(table(wideimp$diabetes)))

# Obesity
wideimp$obesity<-ifelse(wideimp$bmi>=30,"yes","no")
wideimp$obesity<-factor(wideimp$obesity)

# Underweigth
wideimp$underw<-ifelse(wideimp$bmi<20, "yes","no")
wideimp$underw<-factor(wideimp$underw)

# Anxiety
wideimp$medanx<-NA
wideimp$medanx[wideimp$traitement_seven==1|wideimp$traitement_eight==1]<-1
wideimp$medanx[wideimp$traitement_seven==0 & wideimp$traitement_eight==0]<-2
wideimp$medanx<-factor(wideimp$medanx, levels=c(1,2), labels = c("yes","no"))
table(wideimp$medanx, useNA = "ifany")
round(100*prop.table(table(wideimp$medanx)))

# Polypharmacy

wideimp$med<-wideimp$traitement_one+wideimp$traitement_two+wideimp$traitement_three+wideimp$traitement_four+
  wideimp$traitement_five+wideimp$traitement_six+wideimp$traitement_seven+wideimp$traitement_eight+
  wideimp$traitement_nine

wideimp$medcat<-ifelse(wideimp$med>=2,"yes","no")
wideimp$medcat<-factor(wideimp$medcat)
table(wideimp$medcat, useNA = "ifany")
round(100*prop.table(table(wideimp$medcat)))

# Physical activity

lowpa<-quantile(wideimp$sum_all_activities, probs = c(0.25,0.5,0.75), na.rm = TRUE)[[1]]

wideimp$lowpa<-ifelse(wideimp$sum_all_activities<lowpa,"lowpa","nolowpa")
wideimp$lowpa<-factor(wideimp$lowpa)

table(wideimp$lowpa,useNA = "ifany")
wideimp$underwIncome
median(wideimp$income, na.rm=TRUE)
quant<-quantile(wideimp$income, probs=c(0,0.25,0.5,0.75,1),na.rm=TRUE)[[2]]
wideimp$lowincome<-ifelse(wideimp$income<quant,"low","high")
wideimp$lowincome<-factor(wideimp$lowincome)
table( wideimp$lowincome, useNA = "ifany")

wideimp$ogroup<-ifelse(wideimp$gsang==1,"Ogroup","other")
wideimp$ogroup<-factor(wideimp$ogroup)

wideimp$hospital<-wideimp$hospital0


library(dplyr)
newwide<-wideimp%>%
  select(.imp,.id,sexm,age_estimateyears,asthma_mhyn,SUBJID,diabetes,
         medanx,fiverm,smoker,multimorb,NIH_classification_or,
         bmi,obese,underw,medcat,weight_loss,lowpa,eyeglass,
         owner,loweduc,lowincome,work,lung,agroup,ogroup,
         fatiguebs,sleepbs,coughbs,cough_plusbs,throatbs,smellbs,
         diarrheabs,musclebs,chestbs,painbs,feverbs,breathbs,
         breath_plusbs,eatbs,otherbs,skinbs,eyesbs,
         traitement_one,traitement_two,traitement_three,traitement_four,
         traitement_five,traitement_six,traitement_seven,traitement_eight,
         traitement_nine, hospital,
         score0,score1,score2,score3,
         score4,score5,score6,score7,
         score8,score9,score10,score11,
         score12,score13,score14)


names(newwide)

dim(newwide)

table(newwide$.imp)

library(dplyr)
newwide5<-newwide %>%
  filter(.imp==5)
dim(newwide5)

head(newwide5)

var3<-names(newwide[c(54:68)])

newlong5<-reshape(data=newwide5,  direction ="long", varying = var3, idvar = "SUBJID", timevar="time",sep="")



newlong5$JOURn<-newlong5$time

# write.csv(newlong5,"Prediscore/generated_data/newlong5.csv")

library(lcmm)


# 
# modlin <- lcmm(score~JOURn, random =~ JOURn, subject = 'SUBJID',
#                data=newlong5)
# summary(modlin)
# 
# modbeta <- lcmm(score~JOURn, random =~ JOURn, subject = 'SUBJID',
#                 data=newlong5, link = "beta")
# summary(modbeta)
# 
# modspline <- lcmm(score~JOURn, random =~ JOURn, subject = 'SUBJID',
#                   data=newlong5, link = "splines")
# summary(modspline)
# 
# modspline4 <- lcmm(score~JOURn, random =~ JOURn, subject = 'SUBJID',
#                    data=newlong5, link = "4-quant-splines")
# summary(modspline4)
# 
# summarytable(modlin,modbeta,modspline,modspline4,which = c("loglik", "conv", "npm", "AIC"))
# 
# 
# 
# col <- rainbow(5)
# plot(modlin, which="linkfunction", bty='l', ylab="Symptoms score", col=col[1], lwd=2, xlab="underlying latent process")
# plot(modbeta, which="linkfunction", add=TRUE, col=col[2], lwd=2)
# plot(modspline, which="linkfunction", add=TRUE, col=col[3], lwd=2)
# plot(modspline4, which="linkfunction", add=TRUE, col=col[4], lwd=2)
# legend(x="topleft", legend=c("linear", "beta","splines (equidistant)","splines (4 at quantiles)"), lty=1, col=col, bty="n", lwd=2)
# 

# Models withouth predictor
modspline4 <- lcmm(score~JOURn, random =~ JOURn, subject = 'SUBJID',
                   data=newlong5, link = "4-quant-splines")
summary(modspline4)

# set.seed(2003)
# set.seed(50)
# set.seed(100)
# set.seed(150)
# set.seed(300)
# set.seed(1500)
# set.seed(1234)
# set.seed(4321)
# set.seed(1)
# set.seed(1000)
# set.seed(2001)
# set.seed(4321)
# set.seed(1930)
# set.seed(9876)

# modspline4_2<-gridsearch(lcmm(score ~ JOURn,
#                               mixture=~ JOURn,
#                               random=~JOURn,
#                               subject='SUBJID',
#                               ng=2, 
#                               link='4-quant-splines',
#                               data=newlong5,  
#                               B=modspline4, 
#                               verbose = TRUE), 
#                          rep=30, 
#                          maxiter=15,  
#                          minit=modspline4)
# summary(modspline4_2)
# 
# set.seed(2003)
# set.seed(50)
# set.seed(100)
# set.seed(150)
# set.seed(300)
# set.seed(1500)
# set.seed(1234)
# set.seed(4321)
# set.seed(1)
# set.seed(1000)
# set.seed(2001)
# set.seed(4321)
# set.seed(1930)
# set.seed(9876)

# modspline4_3<-gridsearch(lcmm(score ~ JOURn,
#                               mixture=~ JOURn,
#                               random=~JOURn,
#                               subject='SUBJID',
#                               ng=3, 
#                               link='4-quant-splines',
#                               data=newlong5,  
#                               B=modspline4, 
#                               verbose = TRUE), 
#                          rep=30, 
#                          maxiter=15,  
#                          minit=modspline4)
# summary(modspline4_3)

# set.seed(2003)
# set.seed(50)
# set.seed(100)
# set.seed(150)
set.seed(300)
# set.seed(1500)
# set.seed(1234)
# set.seed(4321)
# set.seed(1)
# set.seed(1000)
# set.seed(2001)
# set.seed(4321)
# set.seed(1930)
# set.seed(9876)

modspline4_4<-gridsearch(lcmm(score ~ JOURn,
                              mixture=~ JOURn,
                              random=~JOURn,
                              subject='SUBJID',
                              ng=4, 
                              link='4-quant-splines',
                              data=newlong5,  
                              B=modspline4, 
                              verbose = TRUE), 
                         rep=30, 
                         maxiter=15,  
                         minit=modspline4)
summary(modspline4_4)

# set.seed(2003)
# set.seed(50)
# set.seed(100)
# set.seed(150)
# set.seed(300)
# set.seed(1500)
# set.seed(1234)
# set.seed(4321)
# set.seed(1)
# set.seed(1000)
# set.seed(2001)
# set.seed(4321)
# set.seed(1930)
# set.seed(9876)


# modspline4_5<-gridsearch(lcmm(score ~ JOURn,
#                               mixture=~ JOURn,
#                               random=~JOURn,
#                               subject='SUBJID',
#                               ng=5, 
#                               link='4-quant-splines',
#                               data=newlong5,  
#                               B=modspline4, 
#                               verbose = TRUE), 
#                          rep=30, 
#                          maxiter=15,  
#                          minit=modspline4)
# summary(modspline4_5)

# set.seed(2003)
# set.seed(50)
# set.seed(100)
# set.seed(150)
# set.seed(300)
# set.seed(1500)
# set.seed(1234)
# set.seed(4321)
# set.seed(1)
# set.seed(1000)
# set.seed(4321)
# set.seed(1930)

# Downloading data


# wideimp<-read.csv("P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/long.csv", stringsAsFactors = TRUE)

wideimp<-read.csv("Prediscore/generated_data/long.csv", stringsAsFactors = TRUE)

#I check if there are missing

table(wideimp$.imp)

library(dplyr)

wideimp40<-wideimp%>%
  filter(.imp==40)

anyNA(wideimp40)



#I check if there are missing in each variable that I am interested for

apply(wideimp40, 2, anyNA)


#I do a list with each variable and number of missing
missing<-colSums(is.na(wideimp40))

#I do a list with each variable and percent of missing
percent<-colMeans(is.na(wideimp40))*100
class(percent)

#I build a dataframe with missing data
na<-data.frame(missing,percent)
names(na)



#I order the dataframe "na" by percent of missing (to improve the imputation)
naordimp <- na[order(missing),] 
class(naordimp)




library(tibble)
naordimp<-rownames_to_column(naordimp,var="variables")

library(dplyr)
naordimp<-naordimp %>%
  arrange(percent)


# It did not impute dialyse: 	
# dialys_mhyn

# I will impute based on renal



# Calculation scores

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.0)
wideimp$fatigue.0n<-NA
wideimp$fatigue.0n[wideimp$fatigue.0=="bad"]<-2
wideimp$fatigue.0n[wideimp$fatigue.0=="fatigue"]<-1
wideimp$fatigue.0n[wideimp$fatigue.0=="well"]<-0
table(wideimp$fatigue.0n)
wideimp$fatiguebs<-wideimp$fatigue.0

# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.0n<-NA
wideimp$sleep.0n[wideimp$sleep.0=="bad sleep"]<-1
wideimp$sleep.0n[wideimp$sleep.0=="well sleep"]<-0
table(wideimp$sleep.0n)
wideimp$sleepbs<-wideimp$sleep.0


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.0)
wideimp$cough.0n<-NA
wideimp$cough.0n[wideimp$cough.0=="yes"]<-1
wideimp$cough.0n[wideimp$cough.0=="no"]<-0
table(wideimp$cough.0n)
wideimp$coughbs<-wideimp$cough.0

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.0)
wideimp$cough_plus.0n<-NA
wideimp$cough_plus.0n[wideimp$cough_plus.0=="yes"]<-1
wideimp$cough_plus.0n[wideimp$cough_plus.0=="no"]<-0
table(wideimp$cough_plus.0n)
wideimp$cough_plusbs<-wideimp$cough_plus.0

# 5 Sore throat QM6 

summary(wideimp$throat.0)
wideimp$throat.0n<-NA
wideimp$throat.0n[wideimp$throat.0=="yes"]<-1
wideimp$throat.0n[wideimp$throat.0=="no"]<-0
table(wideimp$throat.0n)
wideimp$throatbs<-wideimp$throat.0



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.0)
wideimp$smell.0n<-NA
wideimp$smell.0n[wideimp$smell.0=="yes"]<-1
wideimp$smell.0n[wideimp$smell.0=="no"]<-0
table(wideimp$smell.0n)
wideimp$smellbs<-wideimp$smell.0



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.0)
wideimp$diarrhea.0n<-NA
wideimp$diarrhea.0n[wideimp$diarrhea.0=="yes"]<-1
wideimp$diarrhea.0n[wideimp$diarrhea.0=="no"]<-0
table(wideimp$diarrhea.0n)
wideimp$diarrheabs<-wideimp$diarrhea.0



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.0)
wideimp$muscle.0n<-NA
wideimp$muscle.0n[wideimp$muscle.0=="yes"]<-1
wideimp$muscle.0n[wideimp$muscle.0=="no"]<-0
table(wideimp$muscle.0n)
wideimp$musclebs<-wideimp$muscle.0



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.0)
wideimp$chest.0n<-NA
wideimp$chest.0n[wideimp$chest.0=="yes"]<-1
wideimp$chest.0n[wideimp$chest.0=="no"]<-0
table(wideimp$chest.0n)
wideimp$chestbs<-wideimp$chest.0




# 10 Pain scale Quel est votre niveau de douleur actuel ?

table(wideimp$pain.0,useNA = "ifany")
wideimp$pain.0n<-NA
wideimp$pain.0n[wideimp$pain.0=="yes"]<-1
wideimp$pain.0n[wideimp$pain.0=="no"]<-0
wideimp$pain.0n[wideimp$chest.0n==0]<-0
table(wideimp$pain.0n,useNA = "ifany")
wideimp$painbs<-wideimp$pain.0

# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.0)
wideimp$fever.0n<-NA
wideimp$fever.0n[wideimp$fever.0=="yes"]<-1
wideimp$fever.0n[wideimp$fever.0=="no"]<-0
table(wideimp$fever.0n)

wideimp$feverbs<-wideimp$fever.0


# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.0)
wideimp$breath.0n<-NA
wideimp$breath.0n[wideimp$breath.0=="yes"]<-1
wideimp$breath.0n[wideimp$breath.0=="no"]<-0
table(wideimp$breath.0n)

wideimp$breathbs<-wideimp$breath.0

# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.0)
wideimp$breath_plus.0n<-NA
wideimp$breath_plus.0n[wideimp$breath_plus.0=="yes"]<-1
wideimp$breath_plus.0n[wideimp$breath_plus.0=="no"]<-0
table(wideimp$breath_plus.0n)

wideimp$breath_plusbs<-wideimp$breath_plus.0


# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.0)
wideimp$eat.0n<-NA
wideimp$eat.0n[wideimp$eat.0=="yes"]<-1
wideimp$eat.0n[wideimp$eat.0=="no"]<-0
table(wideimp$eat.0n)

wideimp$eatbs<-wideimp$eat.0

# 15 Other symptoms ?
summary(wideimp$other.0)
wideimp$other.0n<-NA
wideimp$other.0n[wideimp$other.0=="yes"]<-1
wideimp$other.0n[wideimp$other.0=="no"]<-0
table(wideimp$other.0n)

wideimp$otherbs<-wideimp$other.0

# 16 Skin rashes. Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.0)
wideimp$skin.0n<-NA
wideimp$skin.0n[wideimp$skin.0=="yes"]<-1
wideimp$skin.0n[wideimp$skin.0=="no"]<-0
table(wideimp$skin.0n)

wideimp$skinbs<-wideimp$skin.0


# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.0)
wideimp$eyes.0n[wideimp$eyes.0=="yes"]<-1
wideimp$eyes.0n[wideimp$eyes.0=="no"]<-0
table(wideimp$eyes.0n)

wideimp$eyesbs<-wideimp$eyes.0
#
# # 18. Vous etes a la maison, a l'hopital


table(wideimp$hospital.0)
wideimp$hospital0
wideimp$hospital0[wideimp$hospital.0=="yes"]<-1
wideimp$hospital0[wideimp$hospital.0=="no"]<-0
table(wideimp$hospital0)




wideimp$score0<-NA
wideimp$score0<-wideimp$fatigue.0n + wideimp$sleep.0n + wideimp$cough.0n + 
  wideimp$cough_plus.0n + wideimp$throat.0n + wideimp$smell.0n + 
  wideimp$diarrhea.0n + wideimp$muscle.0n + wideimp$chest.0n + 
  wideimp$pain.0n + wideimp$fever.0n + wideimp$breath.0n + 
  wideimp$breath_plus.0n + wideimp$eat.0n + wideimp$other.0n + 
  wideimp$skin.0n + wideimp$eyes.0n
summary(wideimp$score0)



# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.1)
wideimp$fatigue.1n<-NA
wideimp$fatigue.1n[wideimp$fatigue.1=="bad"]<-2
wideimp$fatigue.1n[wideimp$fatigue.1=="fatigue"]<-1
wideimp$fatigue.1n[wideimp$fatigue.1=="well"]<-0
table(wideimp$fatigue.1n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.1n<-NA
wideimp$sleep.1n[wideimp$sleep.1=="bad sleep"]<-1
wideimp$sleep.1n[wideimp$sleep.1=="well sleep"]<-0
table(wideimp$sleep.1n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.1)
wideimp$cough.1n<-NA
wideimp$cough.1n[wideimp$cough.1=="yes"]<-1
wideimp$cough.1n[wideimp$cough.1=="no"]<-0
table(wideimp$cough.1n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.1)
wideimp$cough_plus.1n<-NA
wideimp$cough_plus.1n[wideimp$cough_plus.1=="yes"]<-1
wideimp$cough_plus.1n[wideimp$cough_plus.1=="no"]<-0
table(wideimp$cough_plus.1n)

# 5 Sore throat QM6 

summary(wideimp$throat.1)
wideimp$throat.1n<-NA
wideimp$throat.1n[wideimp$throat.1=="yes"]<-1
wideimp$throat.1n[wideimp$throat.1=="no"]<-0
table(wideimp$throat.1n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.1)
wideimp$smell.1n<-NA
wideimp$smell.1n[wideimp$smell.1=="yes"]<-1
wideimp$smell.1n[wideimp$smell.1=="no"]<-0
table(wideimp$smell.1n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.1)
wideimp$diarrhea.1n<-NA
wideimp$diarrhea.1n[wideimp$diarrhea.1=="yes"]<-1
wideimp$diarrhea.1n[wideimp$diarrhea.1=="no"]<-0
table(wideimp$diarrhea.1n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.1)
wideimp$muscle.1n<-NA
wideimp$muscle.1n[wideimp$muscle.1=="yes"]<-1
wideimp$muscle.1n[wideimp$muscle.1=="no"]<-0
table(wideimp$muscle.1n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.1)
wideimp$chest.1n<-NA
wideimp$chest.1n[wideimp$chest.1=="yes"]<-1
wideimp$chest.1n[wideimp$chest.1=="no"]<-0
table(wideimp$chest.1n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.1)
wideimp$pain.1n<-NA
wideimp$pain.1n[wideimp$pain.1=="yes"]<-1
wideimp$pain.1n[wideimp$pain.1=="no"]<-0
table(wideimp$pain.1n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.1)
wideimp$fever.1n<-NA
wideimp$fever.1n[wideimp$fever.1=="yes"]<-1
wideimp$fever.1n[wideimp$fever.1=="no"]<-0
table(wideimp$fever.1n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.1)
wideimp$breath.1n<-NA
wideimp$breath.1n[wideimp$breath.1=="yes"]<-1
wideimp$breath.1n[wideimp$breath.1=="no"]<-0
table(wideimp$breath.1n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.1)
wideimp$breath_plus.1n<-NA
wideimp$breath_plus.1n[wideimp$breath_plus.1=="yes"]<-1
wideimp$breath_plus.1n[wideimp$breath_plus.1=="no"]<-0
table(wideimp$breath_plus.1n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.1)
wideimp$eat.1n<-NA
wideimp$eat.1n[wideimp$eat.1=="yes"]<-1
wideimp$eat.1n[wideimp$eat.1=="no"]<-0
table(wideimp$eat.1n)




# 15 Other symptoms ?
summary(wideimp$other.1)
wideimp$other.1n<-NA
wideimp$other.1n[wideimp$other.1=="yes"]<-1
wideimp$other.1n[wideimp$other.1=="no"]<-0
table(wideimp$other.1n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.1)
wideimp$skin.1n<-NA
wideimp$skin.1n[wideimp$skin.1=="yes"]<-1
wideimp$skin.1n[wideimp$skin.1=="no"]<-0
table(wideimp$skin.1n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.1)
wideimp$eyes.1n[wideimp$eyes.1=="yes"]<-1
wideimp$eyes.1n[wideimp$eyes.1=="no"]<-0
table(wideimp$eyes.1n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.1)
wideimp$hospital1[wideimp$hospital.1=="yes"]<-1
wideimp$hospital1[wideimp$hospital.1=="no"]<-0
table(wideimp$hospital1)




wideimp$score1<-NA
wideimp$score1<-wideimp$fatigue.1n + wideimp$sleep.1n + wideimp$cough.1n + 
  wideimp$cough_plus.1n + wideimp$throat.1n + wideimp$smell.1n + 
  wideimp$diarrhea.1n + wideimp$muscle.1n + wideimp$chest.1n + 
  wideimp$pain.1n + wideimp$fever.1n + wideimp$breath.1n + 
  wideimp$breath_plus.1n + wideimp$eat.1n + wideimp$other.1n + 
  wideimp$skin.1n + wideimp$eyes.1n
summary(wideimp$score1)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.2)
wideimp$fatigue.2n<-NA
wideimp$fatigue.2n[wideimp$fatigue.2=="bad"]<-2
wideimp$fatigue.2n[wideimp$fatigue.2=="fatigue"]<-1
wideimp$fatigue.2n[wideimp$fatigue.2=="well"]<-0
table(wideimp$fatigue.2n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.2n<-NA
wideimp$sleep.2n[wideimp$sleep.2=="bad sleep"]<-1
wideimp$sleep.2n[wideimp$sleep.2=="well sleep"]<-0
table(wideimp$sleep.2n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.2)
wideimp$cough.2n<-NA
wideimp$cough.2n[wideimp$cough.2=="yes"]<-1
wideimp$cough.2n[wideimp$cough.2=="no"]<-0
table(wideimp$cough.2n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.2)
wideimp$cough_plus.2n<-NA
wideimp$cough_plus.2n[wideimp$cough_plus.2=="yes"]<-1
wideimp$cough_plus.2n[wideimp$cough_plus.2=="no"]<-0
table(wideimp$cough_plus.2n)

# 5 Sore throat QM6 

summary(wideimp$throat.2)
wideimp$throat.2n<-NA
wideimp$throat.2n[wideimp$throat.2=="yes"]<-1
wideimp$throat.2n[wideimp$throat.2=="no"]<-0
table(wideimp$throat.2n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.2)
wideimp$smell.2n<-NA
wideimp$smell.2n[wideimp$smell.2=="yes"]<-1
wideimp$smell.2n[wideimp$smell.2=="no"]<-0
table(wideimp$smell.2n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.2)
wideimp$diarrhea.2n<-NA
wideimp$diarrhea.2n[wideimp$diarrhea.2=="yes"]<-1
wideimp$diarrhea.2n[wideimp$diarrhea.2=="no"]<-0
table(wideimp$diarrhea.2n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.2)
wideimp$muscle.2n<-NA
wideimp$muscle.2n[wideimp$muscle.2=="yes"]<-1
wideimp$muscle.2n[wideimp$muscle.2=="no"]<-0
table(wideimp$muscle.2n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.2)
wideimp$chest.2n<-NA
wideimp$chest.2n[wideimp$chest.2=="yes"]<-1
wideimp$chest.2n[wideimp$chest.2=="no"]<-0
table(wideimp$chest.2n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.2)
wideimp$pain.2n<-NA
wideimp$pain.2n[wideimp$pain.2=="yes"]<-1
wideimp$pain.2n[wideimp$pain.2=="no"]<-0
table(wideimp$pain.2n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.2)
wideimp$fever.2n<-NA
wideimp$fever.2n[wideimp$fever.2=="yes"]<-1
wideimp$fever.2n[wideimp$fever.2=="no"]<-0
table(wideimp$fever.2n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.2)
wideimp$breath.2n<-NA
wideimp$breath.2n[wideimp$breath.2=="yes"]<-1
wideimp$breath.2n[wideimp$breath.2=="no"]<-0
table(wideimp$breath.2n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.2)
wideimp$breath_plus.2n<-NA
wideimp$breath_plus.2n[wideimp$breath_plus.2=="yes"]<-1
wideimp$breath_plus.2n[wideimp$breath_plus.2=="no"]<-0
table(wideimp$breath_plus.2n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.2)
wideimp$eat.2n<-NA
wideimp$eat.2n[wideimp$eat.2=="yes"]<-1
wideimp$eat.2n[wideimp$eat.2=="no"]<-0
table(wideimp$eat.2n)




# 15 Other symptoms ?
summary(wideimp$other.2)
wideimp$other.2n<-NA
wideimp$other.2n[wideimp$other.2=="yes"]<-1
wideimp$other.2n[wideimp$other.2=="no"]<-0
table(wideimp$other.2n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.2)
wideimp$skin.2n<-NA
wideimp$skin.2n[wideimp$skin.2=="yes"]<-1
wideimp$skin.2n[wideimp$skin.2=="no"]<-0
table(wideimp$skin.2n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.2)
wideimp$eyes.2n[wideimp$eyes.2=="yes"]<-1
wideimp$eyes.2n[wideimp$eyes.2=="no"]<-0
table(wideimp$eyes.2n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.2)
wideimp$hospital2[wideimp$hospital.2=="yes"]<-1
wideimp$hospital2[wideimp$hospital.2=="no"]<-0
table(wideimp$hospital2)




wideimp$score2<-NA
wideimp$score2<-wideimp$fatigue.2n + wideimp$sleep.2n + wideimp$cough.2n + 
  wideimp$cough_plus.2n + wideimp$throat.2n + wideimp$smell.2n + 
  wideimp$diarrhea.2n + wideimp$muscle.2n + wideimp$chest.2n + 
  wideimp$pain.2n + wideimp$fever.2n + wideimp$breath.2n + 
  wideimp$breath_plus.2n + wideimp$eat.2n + wideimp$other.2n + 
  wideimp$skin.2n + wideimp$eyes.2n
summary(wideimp$score2)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.3)
wideimp$fatigue.3n<-NA
wideimp$fatigue.3n[wideimp$fatigue.3=="bad"]<-2
wideimp$fatigue.3n[wideimp$fatigue.3=="fatigue"]<-1
wideimp$fatigue.3n[wideimp$fatigue.3=="well"]<-0
table(wideimp$fatigue.3n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.3n<-NA
wideimp$sleep.3n[wideimp$sleep.3=="bad sleep"]<-1
wideimp$sleep.3n[wideimp$sleep.3=="well sleep"]<-0
table(wideimp$sleep.3n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.3)
wideimp$cough.3n<-NA
wideimp$cough.3n[wideimp$cough.3=="yes"]<-1
wideimp$cough.3n[wideimp$cough.3=="no"]<-0
table(wideimp$cough.3n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.3)
wideimp$cough_plus.3n<-NA
wideimp$cough_plus.3n[wideimp$cough_plus.3=="yes"]<-1
wideimp$cough_plus.3n[wideimp$cough_plus.3=="no"]<-0
table(wideimp$cough_plus.3n)

# 5 Sore throat QM6 

summary(wideimp$throat.3)
wideimp$throat.3n<-NA
wideimp$throat.3n[wideimp$throat.3=="yes"]<-1
wideimp$throat.3n[wideimp$throat.3=="no"]<-0
table(wideimp$throat.3n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.3)
wideimp$smell.3n<-NA
wideimp$smell.3n[wideimp$smell.3=="yes"]<-1
wideimp$smell.3n[wideimp$smell.3=="no"]<-0
table(wideimp$smell.3n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.3)
wideimp$diarrhea.3n<-NA
wideimp$diarrhea.3n[wideimp$diarrhea.3=="yes"]<-1
wideimp$diarrhea.3n[wideimp$diarrhea.3=="no"]<-0
table(wideimp$diarrhea.3n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.3)
wideimp$muscle.3n<-NA
wideimp$muscle.3n[wideimp$muscle.3=="yes"]<-1
wideimp$muscle.3n[wideimp$muscle.3=="no"]<-0
table(wideimp$muscle.3n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.3)
wideimp$chest.3n<-NA
wideimp$chest.3n[wideimp$chest.3=="yes"]<-1
wideimp$chest.3n[wideimp$chest.3=="no"]<-0
table(wideimp$chest.3n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.3)
wideimp$pain.3n<-NA
wideimp$pain.3n[wideimp$pain.3=="yes"]<-1
wideimp$pain.3n[wideimp$pain.3=="no"]<-0
table(wideimp$pain.3n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.3)
wideimp$fever.3n<-NA
wideimp$fever.3n[wideimp$fever.3=="yes"]<-1
wideimp$fever.3n[wideimp$fever.3=="no"]<-0
table(wideimp$fever.3n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.3)
wideimp$breath.3n<-NA
wideimp$breath.3n[wideimp$breath.3=="yes"]<-1
wideimp$breath.3n[wideimp$breath.3=="no"]<-0
table(wideimp$breath.3n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.3)
wideimp$breath_plus.3n<-NA
wideimp$breath_plus.3n[wideimp$breath_plus.3=="yes"]<-1
wideimp$breath_plus.3n[wideimp$breath_plus.3=="no"]<-0
table(wideimp$breath_plus.3n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.3)
wideimp$eat.3n<-NA
wideimp$eat.3n[wideimp$eat.3=="yes"]<-1
wideimp$eat.3n[wideimp$eat.3=="no"]<-0
table(wideimp$eat.3n)




# 15 Other symptoms ?
summary(wideimp$other.3)
wideimp$other.3n<-NA
wideimp$other.3n[wideimp$other.3=="yes"]<-1
wideimp$other.3n[wideimp$other.3=="no"]<-0
table(wideimp$other.3n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.3)
wideimp$skin.3n<-NA
wideimp$skin.3n[wideimp$skin.3=="yes"]<-1
wideimp$skin.3n[wideimp$skin.3=="no"]<-0
table(wideimp$skin.3n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.3)
wideimp$eyes.3n[wideimp$eyes.3=="yes"]<-1
wideimp$eyes.3n[wideimp$eyes.3=="no"]<-0
table(wideimp$eyes.3n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.3)
wideimp$hospital3[wideimp$hospital.3=="yes"]<-1
wideimp$hospital3[wideimp$hospital.3=="no"]<-0
table(wideimp$hospital3)




wideimp$score3<-NA
wideimp$score3<-wideimp$fatigue.3n + wideimp$sleep.3n + wideimp$cough.3n + 
  wideimp$cough_plus.3n + wideimp$throat.3n + wideimp$smell.3n + 
  wideimp$diarrhea.3n + wideimp$muscle.3n + wideimp$chest.3n + 
  wideimp$pain.3n + wideimp$fever.3n + wideimp$breath.3n + 
  wideimp$breath_plus.3n + wideimp$eat.3n + wideimp$other.3n + 
  wideimp$skin.3n + wideimp$eyes.3n
summary(wideimp$score3)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.4)
wideimp$fatigue.4n<-NA
wideimp$fatigue.4n[wideimp$fatigue.4=="bad"]<-2
wideimp$fatigue.4n[wideimp$fatigue.4=="fatigue"]<-1
wideimp$fatigue.4n[wideimp$fatigue.4=="well"]<-0
table(wideimp$fatigue.4n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.4n<-NA
wideimp$sleep.4n[wideimp$sleep.4=="bad sleep"]<-1
wideimp$sleep.4n[wideimp$sleep.4=="well sleep"]<-0
table(wideimp$sleep.4n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.4)
wideimp$cough.4n<-NA
wideimp$cough.4n[wideimp$cough.4=="yes"]<-1
wideimp$cough.4n[wideimp$cough.4=="no"]<-0
table(wideimp$cough.4n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.4)
wideimp$cough_plus.4n<-NA
wideimp$cough_plus.4n[wideimp$cough_plus.4=="yes"]<-1
wideimp$cough_plus.4n[wideimp$cough_plus.4=="no"]<-0
table(wideimp$cough_plus.4n)

# 5 Sore throat QM6 

summary(wideimp$throat.4)
wideimp$throat.4n<-NA
wideimp$throat.4n[wideimp$throat.4=="yes"]<-1
wideimp$throat.4n[wideimp$throat.4=="no"]<-0
table(wideimp$throat.4n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.4)
wideimp$smell.4n<-NA
wideimp$smell.4n[wideimp$smell.4=="yes"]<-1
wideimp$smell.4n[wideimp$smell.4=="no"]<-0
table(wideimp$smell.4n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.4)
wideimp$diarrhea.4n<-NA
wideimp$diarrhea.4n[wideimp$diarrhea.4=="yes"]<-1
wideimp$diarrhea.4n[wideimp$diarrhea.4=="no"]<-0
table(wideimp$diarrhea.4n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.4)
wideimp$muscle.4n<-NA
wideimp$muscle.4n[wideimp$muscle.4=="yes"]<-1
wideimp$muscle.4n[wideimp$muscle.4=="no"]<-0
table(wideimp$muscle.4n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.4)
wideimp$chest.4n<-NA
wideimp$chest.4n[wideimp$chest.4=="yes"]<-1
wideimp$chest.4n[wideimp$chest.4=="no"]<-0
table(wideimp$chest.4n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.4)
wideimp$pain.4n<-NA
wideimp$pain.4n[wideimp$pain.4=="yes"]<-1
wideimp$pain.4n[wideimp$pain.4=="no"]<-0
table(wideimp$pain.4n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.4)
wideimp$fever.4n<-NA
wideimp$fever.4n[wideimp$fever.4=="yes"]<-1
wideimp$fever.4n[wideimp$fever.4=="no"]<-0
table(wideimp$fever.4n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.4)
wideimp$breath.4n<-NA
wideimp$breath.4n[wideimp$breath.4=="yes"]<-1
wideimp$breath.4n[wideimp$breath.4=="no"]<-0
table(wideimp$breath.4n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.4)
wideimp$breath_plus.4n<-NA
wideimp$breath_plus.4n[wideimp$breath_plus.4=="yes"]<-1
wideimp$breath_plus.4n[wideimp$breath_plus.4=="no"]<-0
table(wideimp$breath_plus.4n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.4)
wideimp$eat.4n<-NA
wideimp$eat.4n[wideimp$eat.4=="yes"]<-1
wideimp$eat.4n[wideimp$eat.4=="no"]<-0
table(wideimp$eat.4n)




# 15 Other symptoms ?
summary(wideimp$other.4)
wideimp$other.4n<-NA
wideimp$other.4n[wideimp$other.4=="yes"]<-1
wideimp$other.4n[wideimp$other.4=="no"]<-0
table(wideimp$other.4n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.4)
wideimp$skin.4n<-NA
wideimp$skin.4n[wideimp$skin.4=="yes"]<-1
wideimp$skin.4n[wideimp$skin.4=="no"]<-0
table(wideimp$skin.4n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.4)
wideimp$eyes.4n[wideimp$eyes.4=="yes"]<-1
wideimp$eyes.4n[wideimp$eyes.4=="no"]<-0
table(wideimp$eyes.4n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.4)
wideimp$hospital4[wideimp$hospital.4=="yes"]<-1
wideimp$hospital4[wideimp$hospital.4=="no"]<-0
table(wideimp$hospital4)




wideimp$score4<-NA
wideimp$score4<-wideimp$fatigue.4n + wideimp$sleep.4n + wideimp$cough.4n + 
  wideimp$cough_plus.4n + wideimp$throat.4n + wideimp$smell.4n + 
  wideimp$diarrhea.4n + wideimp$muscle.4n + wideimp$chest.4n + 
  wideimp$pain.4n + wideimp$fever.4n + wideimp$breath.4n + 
  wideimp$breath_plus.4n + wideimp$eat.4n + wideimp$other.4n + 
  wideimp$skin.4n + wideimp$eyes.4n
summary(wideimp$score4)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.5)
wideimp$fatigue.5n<-NA
wideimp$fatigue.5n[wideimp$fatigue.5=="bad"]<-2
wideimp$fatigue.5n[wideimp$fatigue.5=="fatigue"]<-1
wideimp$fatigue.5n[wideimp$fatigue.5=="well"]<-0
table(wideimp$fatigue.5n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.5n<-NA
wideimp$sleep.5n[wideimp$sleep.5=="bad sleep"]<-1
wideimp$sleep.5n[wideimp$sleep.5=="well sleep"]<-0
table(wideimp$sleep.5n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.5)
wideimp$cough.5n<-NA
wideimp$cough.5n[wideimp$cough.5=="yes"]<-1
wideimp$cough.5n[wideimp$cough.5=="no"]<-0
table(wideimp$cough.5n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.5)
wideimp$cough_plus.5n<-NA
wideimp$cough_plus.5n[wideimp$cough_plus.5=="yes"]<-1
wideimp$cough_plus.5n[wideimp$cough_plus.5=="no"]<-0
table(wideimp$cough_plus.5n)

# 5 Sore throat QM6 

summary(wideimp$throat.5)
wideimp$throat.5n<-NA
wideimp$throat.5n[wideimp$throat.5=="yes"]<-1
wideimp$throat.5n[wideimp$throat.5=="no"]<-0
table(wideimp$throat.5n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.5)
wideimp$smell.5n<-NA
wideimp$smell.5n[wideimp$smell.5=="yes"]<-1
wideimp$smell.5n[wideimp$smell.5=="no"]<-0
table(wideimp$smell.5n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.5)
wideimp$diarrhea.5n<-NA
wideimp$diarrhea.5n[wideimp$diarrhea.5=="yes"]<-1
wideimp$diarrhea.5n[wideimp$diarrhea.5=="no"]<-0
table(wideimp$diarrhea.5n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.5)
wideimp$muscle.5n<-NA
wideimp$muscle.5n[wideimp$muscle.5=="yes"]<-1
wideimp$muscle.5n[wideimp$muscle.5=="no"]<-0
table(wideimp$muscle.5n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.5)
wideimp$chest.5n<-NA
wideimp$chest.5n[wideimp$chest.5=="yes"]<-1
wideimp$chest.5n[wideimp$chest.5=="no"]<-0
table(wideimp$chest.5n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.5)
wideimp$pain.5n<-NA
wideimp$pain.5n[wideimp$pain.5=="yes"]<-1
wideimp$pain.5n[wideimp$pain.5=="no"]<-0
table(wideimp$pain.5n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.5)
wideimp$fever.5n<-NA
wideimp$fever.5n[wideimp$fever.5=="yes"]<-1
wideimp$fever.5n[wideimp$fever.5=="no"]<-0
table(wideimp$fever.5n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.5)
wideimp$breath.5n<-NA
wideimp$breath.5n[wideimp$breath.5=="yes"]<-1
wideimp$breath.5n[wideimp$breath.5=="no"]<-0
table(wideimp$breath.5n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.5)
wideimp$breath_plus.5n<-NA
wideimp$breath_plus.5n[wideimp$breath_plus.5=="yes"]<-1
wideimp$breath_plus.5n[wideimp$breath_plus.5=="no"]<-0
table(wideimp$breath_plus.5n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.5)
wideimp$eat.5n<-NA
wideimp$eat.5n[wideimp$eat.5=="yes"]<-1
wideimp$eat.5n[wideimp$eat.5=="no"]<-0
table(wideimp$eat.5n)




# 15 Other symptoms ?
summary(wideimp$other.5)
wideimp$other.5n<-NA
wideimp$other.5n[wideimp$other.5=="yes"]<-1
wideimp$other.5n[wideimp$other.5=="no"]<-0
table(wideimp$other.5n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.5)
wideimp$skin.5n<-NA
wideimp$skin.5n[wideimp$skin.5=="yes"]<-1
wideimp$skin.5n[wideimp$skin.5=="no"]<-0
table(wideimp$skin.5n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.5)
wideimp$eyes.5n[wideimp$eyes.5=="yes"]<-1
wideimp$eyes.5n[wideimp$eyes.5=="no"]<-0
table(wideimp$eyes.5n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.5)
wideimp$hospital5[wideimp$hospital.5=="yes"]<-1
wideimp$hospital5[wideimp$hospital.5=="no"]<-0
table(wideimp$hospital5)




wideimp$score5<-NA
wideimp$score5<-wideimp$fatigue.5n + wideimp$sleep.5n + wideimp$cough.5n + 
  wideimp$cough_plus.5n + wideimp$throat.5n + wideimp$smell.5n + 
  wideimp$diarrhea.5n + wideimp$muscle.5n + wideimp$chest.5n + 
  wideimp$pain.5n + wideimp$fever.5n + wideimp$breath.5n + 
  wideimp$breath_plus.5n + wideimp$eat.5n + wideimp$other.5n + 
  wideimp$skin.5n + wideimp$eyes.5n
summary(wideimp$score5)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.6)
wideimp$fatigue.6n<-NA
wideimp$fatigue.6n[wideimp$fatigue.6=="bad"]<-2
wideimp$fatigue.6n[wideimp$fatigue.6=="fatigue"]<-1
wideimp$fatigue.6n[wideimp$fatigue.6=="well"]<-0
table(wideimp$fatigue.6n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.6n<-NA
wideimp$sleep.6n[wideimp$sleep.6=="bad sleep"]<-1
wideimp$sleep.6n[wideimp$sleep.6=="well sleep"]<-0
table(wideimp$sleep.6n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.6)
wideimp$cough.6n<-NA
wideimp$cough.6n[wideimp$cough.6=="yes"]<-1
wideimp$cough.6n[wideimp$cough.6=="no"]<-0
table(wideimp$cough.6n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.6)
wideimp$cough_plus.6n<-NA
wideimp$cough_plus.6n[wideimp$cough_plus.6=="yes"]<-1
wideimp$cough_plus.6n[wideimp$cough_plus.6=="no"]<-0
table(wideimp$cough_plus.6n)

# 5 Sore throat QM6 

summary(wideimp$throat.6)
wideimp$throat.6n<-NA
wideimp$throat.6n[wideimp$throat.6=="yes"]<-1
wideimp$throat.6n[wideimp$throat.6=="no"]<-0
table(wideimp$throat.6n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.6)
wideimp$smell.6n<-NA
wideimp$smell.6n[wideimp$smell.6=="yes"]<-1
wideimp$smell.6n[wideimp$smell.6=="no"]<-0
table(wideimp$smell.6n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.6)
wideimp$diarrhea.6n<-NA
wideimp$diarrhea.6n[wideimp$diarrhea.6=="yes"]<-1
wideimp$diarrhea.6n[wideimp$diarrhea.6=="no"]<-0
table(wideimp$diarrhea.6n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.6)
wideimp$muscle.6n<-NA
wideimp$muscle.6n[wideimp$muscle.6=="yes"]<-1
wideimp$muscle.6n[wideimp$muscle.6=="no"]<-0
table(wideimp$muscle.6n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.6)
wideimp$chest.6n<-NA
wideimp$chest.6n[wideimp$chest.6=="yes"]<-1
wideimp$chest.6n[wideimp$chest.6=="no"]<-0
table(wideimp$chest.6n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.6)
wideimp$pain.6n<-NA
wideimp$pain.6n[wideimp$pain.6=="yes"]<-1
wideimp$pain.6n[wideimp$pain.6=="no"]<-0
table(wideimp$pain.6n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.6)
wideimp$fever.6n<-NA
wideimp$fever.6n[wideimp$fever.6=="yes"]<-1
wideimp$fever.6n[wideimp$fever.6=="no"]<-0
table(wideimp$fever.6n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.6)
wideimp$breath.6n<-NA
wideimp$breath.6n[wideimp$breath.6=="yes"]<-1
wideimp$breath.6n[wideimp$breath.6=="no"]<-0
table(wideimp$breath.6n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.6)
wideimp$breath_plus.6n<-NA
wideimp$breath_plus.6n[wideimp$breath_plus.6=="yes"]<-1
wideimp$breath_plus.6n[wideimp$breath_plus.6=="no"]<-0
table(wideimp$breath_plus.6n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.6)
wideimp$eat.6n<-NA
wideimp$eat.6n[wideimp$eat.6=="yes"]<-1
wideimp$eat.6n[wideimp$eat.6=="no"]<-0
table(wideimp$eat.6n)




# 15 Other symptoms ?
summary(wideimp$other.6)
wideimp$other.6n<-NA
wideimp$other.6n[wideimp$other.6=="yes"]<-1
wideimp$other.6n[wideimp$other.6=="no"]<-0
table(wideimp$other.6n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.6)
wideimp$skin.6n<-NA
wideimp$skin.6n[wideimp$skin.6=="yes"]<-1
wideimp$skin.6n[wideimp$skin.6=="no"]<-0
table(wideimp$skin.6n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.6)
wideimp$eyes.6n[wideimp$eyes.6=="yes"]<-1
wideimp$eyes.6n[wideimp$eyes.6=="no"]<-0
table(wideimp$eyes.6n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.6)
wideimp$hospital6[wideimp$hospital.6=="yes"]<-1
wideimp$hospital6[wideimp$hospital.6=="no"]<-0
table(wideimp$hospital6)




wideimp$score6<-NA
wideimp$score6<-wideimp$fatigue.6n + wideimp$sleep.6n + wideimp$cough.6n + 
  wideimp$cough_plus.6n + wideimp$throat.6n + wideimp$smell.6n + 
  wideimp$diarrhea.6n + wideimp$muscle.6n + wideimp$chest.6n + 
  wideimp$pain.6n + wideimp$fever.6n + wideimp$breath.6n + 
  wideimp$breath_plus.6n + wideimp$eat.6n + wideimp$other.6n + 
  wideimp$skin.6n + wideimp$eyes.6n
summary(wideimp$score6)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.7)
wideimp$fatigue.7n<-NA
wideimp$fatigue.7n[wideimp$fatigue.7=="bad"]<-2
wideimp$fatigue.7n[wideimp$fatigue.7=="fatigue"]<-1
wideimp$fatigue.7n[wideimp$fatigue.7=="well"]<-0
table(wideimp$fatigue.7n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.7n<-NA
wideimp$sleep.7n[wideimp$sleep.7=="bad sleep"]<-1
wideimp$sleep.7n[wideimp$sleep.7=="well sleep"]<-0
table(wideimp$sleep.7n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.7)
wideimp$cough.7n<-NA
wideimp$cough.7n[wideimp$cough.7=="yes"]<-1
wideimp$cough.7n[wideimp$cough.7=="no"]<-0
table(wideimp$cough.7n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.7)
wideimp$cough_plus.7n<-NA
wideimp$cough_plus.7n[wideimp$cough_plus.7=="yes"]<-1
wideimp$cough_plus.7n[wideimp$cough_plus.7=="no"]<-0
table(wideimp$cough_plus.7n)

# 5 Sore throat QM6 

summary(wideimp$throat.7)
wideimp$throat.7n<-NA
wideimp$throat.7n[wideimp$throat.7=="yes"]<-1
wideimp$throat.7n[wideimp$throat.7=="no"]<-0
table(wideimp$throat.7n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.7)
wideimp$smell.7n<-NA
wideimp$smell.7n[wideimp$smell.7=="yes"]<-1
wideimp$smell.7n[wideimp$smell.7=="no"]<-0
table(wideimp$smell.7n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.7)
wideimp$diarrhea.7n<-NA
wideimp$diarrhea.7n[wideimp$diarrhea.7=="yes"]<-1
wideimp$diarrhea.7n[wideimp$diarrhea.7=="no"]<-0
table(wideimp$diarrhea.7n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.7)
wideimp$muscle.7n<-NA
wideimp$muscle.7n[wideimp$muscle.7=="yes"]<-1
wideimp$muscle.7n[wideimp$muscle.7=="no"]<-0
table(wideimp$muscle.7n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.7)
wideimp$chest.7n<-NA
wideimp$chest.7n[wideimp$chest.7=="yes"]<-1
wideimp$chest.7n[wideimp$chest.7=="no"]<-0
table(wideimp$chest.7n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.7)
wideimp$pain.7n<-NA
wideimp$pain.7n[wideimp$pain.7=="yes"]<-1
wideimp$pain.7n[wideimp$pain.7=="no"]<-0
table(wideimp$pain.7n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.7)
wideimp$fever.7n<-NA
wideimp$fever.7n[wideimp$fever.7=="yes"]<-1
wideimp$fever.7n[wideimp$fever.7=="no"]<-0
table(wideimp$fever.7n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.7)
wideimp$breath.7n<-NA
wideimp$breath.7n[wideimp$breath.7=="yes"]<-1
wideimp$breath.7n[wideimp$breath.7=="no"]<-0
table(wideimp$breath.7n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.7)
wideimp$breath_plus.7n<-NA
wideimp$breath_plus.7n[wideimp$breath_plus.7=="yes"]<-1
wideimp$breath_plus.7n[wideimp$breath_plus.7=="no"]<-0
table(wideimp$breath_plus.7n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.7)
wideimp$eat.7n<-NA
wideimp$eat.7n[wideimp$eat.7=="yes"]<-1
wideimp$eat.7n[wideimp$eat.7=="no"]<-0
table(wideimp$eat.7n)




# 15 Other symptoms ?
summary(wideimp$other.7)
wideimp$other.7n<-NA
wideimp$other.7n[wideimp$other.7=="yes"]<-1
wideimp$other.7n[wideimp$other.7=="no"]<-0
table(wideimp$other.7n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.7)
wideimp$skin.7n<-NA
wideimp$skin.7n[wideimp$skin.7=="yes"]<-1
wideimp$skin.7n[wideimp$skin.7=="no"]<-0
table(wideimp$skin.7n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.7)
wideimp$eyes.7n[wideimp$eyes.7=="yes"]<-1
wideimp$eyes.7n[wideimp$eyes.7=="no"]<-0
table(wideimp$eyes.7n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.7)
wideimp$hospital7[wideimp$hospital.7=="yes"]<-1
wideimp$hospital7[wideimp$hospital.7=="no"]<-0
table(wideimp$hospital7)




wideimp$score7<-NA
wideimp$score7<-wideimp$fatigue.7n + wideimp$sleep.7n + wideimp$cough.7n + 
  wideimp$cough_plus.7n + wideimp$throat.7n + wideimp$smell.7n + 
  wideimp$diarrhea.7n + wideimp$muscle.7n + wideimp$chest.7n + 
  wideimp$pain.7n + wideimp$fever.7n + wideimp$breath.7n + 
  wideimp$breath_plus.7n + wideimp$eat.7n + wideimp$other.7n + 
  wideimp$skin.7n + wideimp$eyes.7n
summary(wideimp$score7)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.8)
wideimp$fatigue.8n<-NA
wideimp$fatigue.8n[wideimp$fatigue.8=="bad"]<-2
wideimp$fatigue.8n[wideimp$fatigue.8=="fatigue"]<-1
wideimp$fatigue.8n[wideimp$fatigue.8=="well"]<-0
table(wideimp$fatigue.8n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.8n<-NA
wideimp$sleep.8n[wideimp$sleep.8=="bad sleep"]<-1
wideimp$sleep.8n[wideimp$sleep.8=="well sleep"]<-0
table(wideimp$sleep.8n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.8)
wideimp$cough.8n<-NA
wideimp$cough.8n[wideimp$cough.8=="yes"]<-1
wideimp$cough.8n[wideimp$cough.8=="no"]<-0
table(wideimp$cough.8n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.8)
wideimp$cough_plus.8n<-NA
wideimp$cough_plus.8n[wideimp$cough_plus.8=="yes"]<-1
wideimp$cough_plus.8n[wideimp$cough_plus.8=="no"]<-0
table(wideimp$cough_plus.8n)

# 5 Sore throat QM6 

summary(wideimp$throat.8)
wideimp$throat.8n<-NA
wideimp$throat.8n[wideimp$throat.8=="yes"]<-1
wideimp$throat.8n[wideimp$throat.8=="no"]<-0
table(wideimp$throat.8n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.8)
wideimp$smell.8n<-NA
wideimp$smell.8n[wideimp$smell.8=="yes"]<-1
wideimp$smell.8n[wideimp$smell.8=="no"]<-0
table(wideimp$smell.8n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.8)
wideimp$diarrhea.8n<-NA
wideimp$diarrhea.8n[wideimp$diarrhea.8=="yes"]<-1
wideimp$diarrhea.8n[wideimp$diarrhea.8=="no"]<-0
table(wideimp$diarrhea.8n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.8)
wideimp$muscle.8n<-NA
wideimp$muscle.8n[wideimp$muscle.8=="yes"]<-1
wideimp$muscle.8n[wideimp$muscle.8=="no"]<-0
table(wideimp$muscle.8n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.8)
wideimp$chest.8n<-NA
wideimp$chest.8n[wideimp$chest.8=="yes"]<-1
wideimp$chest.8n[wideimp$chest.8=="no"]<-0
table(wideimp$chest.8n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.8)
wideimp$pain.8n<-NA
wideimp$pain.8n[wideimp$pain.8=="yes"]<-1
wideimp$pain.8n[wideimp$pain.8=="no"]<-0
table(wideimp$pain.8n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.8)
wideimp$fever.8n<-NA
wideimp$fever.8n[wideimp$fever.8=="yes"]<-1
wideimp$fever.8n[wideimp$fever.8=="no"]<-0
table(wideimp$fever.8n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.8)
wideimp$breath.8n<-NA
wideimp$breath.8n[wideimp$breath.8=="yes"]<-1
wideimp$breath.8n[wideimp$breath.8=="no"]<-0
table(wideimp$breath.8n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.8)
wideimp$breath_plus.8n<-NA
wideimp$breath_plus.8n[wideimp$breath_plus.8=="yes"]<-1
wideimp$breath_plus.8n[wideimp$breath_plus.8=="no"]<-0
table(wideimp$breath_plus.8n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.8)
wideimp$eat.8n<-NA
wideimp$eat.8n[wideimp$eat.8=="yes"]<-1
wideimp$eat.8n[wideimp$eat.8=="no"]<-0
table(wideimp$eat.8n)




# 15 Other symptoms ?
summary(wideimp$other.8)
wideimp$other.8n<-NA
wideimp$other.8n[wideimp$other.8=="yes"]<-1
wideimp$other.8n[wideimp$other.8=="no"]<-0
table(wideimp$other.8n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.8)
wideimp$skin.8n<-NA
wideimp$skin.8n[wideimp$skin.8=="yes"]<-1
wideimp$skin.8n[wideimp$skin.8=="no"]<-0
table(wideimp$skin.8n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.8)
wideimp$eyes.8n[wideimp$eyes.8=="yes"]<-1
wideimp$eyes.8n[wideimp$eyes.8=="no"]<-0
table(wideimp$eyes.8n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.8)
wideimp$hospital8[wideimp$hospital.8=="yes"]<-1
wideimp$hospital8[wideimp$hospital.8=="no"]<-0
table(wideimp$hospital8)




wideimp$score8<-NA
wideimp$score8<-wideimp$fatigue.8n + wideimp$sleep.8n + wideimp$cough.8n + 
  wideimp$cough_plus.8n + wideimp$throat.8n + wideimp$smell.8n + 
  wideimp$diarrhea.8n + wideimp$muscle.8n + wideimp$chest.8n + 
  wideimp$pain.8n + wideimp$fever.8n + wideimp$breath.8n + 
  wideimp$breath_plus.8n + wideimp$eat.8n + wideimp$other.8n + 
  wideimp$skin.8n + wideimp$eyes.8n
summary(wideimp$score8)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.9)
wideimp$fatigue.9n<-NA
wideimp$fatigue.9n[wideimp$fatigue.9=="bad"]<-2
wideimp$fatigue.9n[wideimp$fatigue.9=="fatigue"]<-1
wideimp$fatigue.9n[wideimp$fatigue.9=="well"]<-0
table(wideimp$fatigue.9n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.9n<-NA
wideimp$sleep.9n[wideimp$sleep.9=="bad sleep"]<-1
wideimp$sleep.9n[wideimp$sleep.9=="well sleep"]<-0
table(wideimp$sleep.9n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.9)
wideimp$cough.9n<-NA
wideimp$cough.9n[wideimp$cough.9=="yes"]<-1
wideimp$cough.9n[wideimp$cough.9=="no"]<-0
table(wideimp$cough.9n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.9)
wideimp$cough_plus.9n<-NA
wideimp$cough_plus.9n[wideimp$cough_plus.9=="yes"]<-1
wideimp$cough_plus.9n[wideimp$cough_plus.9=="no"]<-0
table(wideimp$cough_plus.9n)

# 5 Sore throat QM6 

summary(wideimp$throat.9)
wideimp$throat.9n<-NA
wideimp$throat.9n[wideimp$throat.9=="yes"]<-1
wideimp$throat.9n[wideimp$throat.9=="no"]<-0
table(wideimp$throat.9n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.9)
wideimp$smell.9n<-NA
wideimp$smell.9n[wideimp$smell.9=="yes"]<-1
wideimp$smell.9n[wideimp$smell.9=="no"]<-0
table(wideimp$smell.9n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.9)
wideimp$diarrhea.9n<-NA
wideimp$diarrhea.9n[wideimp$diarrhea.9=="yes"]<-1
wideimp$diarrhea.9n[wideimp$diarrhea.9=="no"]<-0
table(wideimp$diarrhea.9n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.9)
wideimp$muscle.9n<-NA
wideimp$muscle.9n[wideimp$muscle.9=="yes"]<-1
wideimp$muscle.9n[wideimp$muscle.9=="no"]<-0
table(wideimp$muscle.9n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.9)
wideimp$chest.9n<-NA
wideimp$chest.9n[wideimp$chest.9=="yes"]<-1
wideimp$chest.9n[wideimp$chest.9=="no"]<-0
table(wideimp$chest.9n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.9)
wideimp$pain.9n<-NA
wideimp$pain.9n[wideimp$pain.9=="yes"]<-1
wideimp$pain.9n[wideimp$pain.9=="no"]<-0
table(wideimp$pain.9n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.9)
wideimp$fever.9n<-NA
wideimp$fever.9n[wideimp$fever.9=="yes"]<-1
wideimp$fever.9n[wideimp$fever.9=="no"]<-0
table(wideimp$fever.9n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.9)
wideimp$breath.9n<-NA
wideimp$breath.9n[wideimp$breath.9=="yes"]<-1
wideimp$breath.9n[wideimp$breath.9=="no"]<-0
table(wideimp$breath.9n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.9)
wideimp$breath_plus.9n<-NA
wideimp$breath_plus.9n[wideimp$breath_plus.9=="yes"]<-1
wideimp$breath_plus.9n[wideimp$breath_plus.9=="no"]<-0
table(wideimp$breath_plus.9n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.9)
wideimp$eat.9n<-NA
wideimp$eat.9n[wideimp$eat.9=="yes"]<-1
wideimp$eat.9n[wideimp$eat.9=="no"]<-0
table(wideimp$eat.9n)




# 15 Other symptoms ?
summary(wideimp$other.9)
wideimp$other.9n<-NA
wideimp$other.9n[wideimp$other.9=="yes"]<-1
wideimp$other.9n[wideimp$other.9=="no"]<-0
table(wideimp$other.9n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.9)
wideimp$skin.9n<-NA
wideimp$skin.9n[wideimp$skin.9=="yes"]<-1
wideimp$skin.9n[wideimp$skin.9=="no"]<-0
table(wideimp$skin.9n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.9)
wideimp$eyes.9n[wideimp$eyes.9=="yes"]<-1
wideimp$eyes.9n[wideimp$eyes.9=="no"]<-0
table(wideimp$eyes.9n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.9)
wideimp$hospital9[wideimp$hospital.9=="yes"]<-1
wideimp$hospital9[wideimp$hospital.9=="no"]<-0
table(wideimp$hospital9)




wideimp$score9<-NA
wideimp$score9<-wideimp$fatigue.9n + wideimp$sleep.9n + wideimp$cough.9n + 
  wideimp$cough_plus.9n + wideimp$throat.9n + wideimp$smell.9n + 
  wideimp$diarrhea.9n + wideimp$muscle.9n + wideimp$chest.9n + 
  wideimp$pain.9n + wideimp$fever.9n + wideimp$breath.9n + 
  wideimp$breath_plus.9n + wideimp$eat.9n + wideimp$other.9n + 
  wideimp$skin.9n + wideimp$eyes.9n
summary(wideimp$score9)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.10)
wideimp$fatigue.10n<-NA
wideimp$fatigue.10n[wideimp$fatigue.10=="bad"]<-2
wideimp$fatigue.10n[wideimp$fatigue.10=="fatigue"]<-1
wideimp$fatigue.10n[wideimp$fatigue.10=="well"]<-0
table(wideimp$fatigue.10n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.10n<-NA
wideimp$sleep.10n[wideimp$sleep.10=="bad sleep"]<-1
wideimp$sleep.10n[wideimp$sleep.10=="well sleep"]<-0
table(wideimp$sleep.10n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.10)
wideimp$cough.10n<-NA
wideimp$cough.10n[wideimp$cough.10=="yes"]<-1
wideimp$cough.10n[wideimp$cough.10=="no"]<-0
table(wideimp$cough.10n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.10)
wideimp$cough_plus.10n<-NA
wideimp$cough_plus.10n[wideimp$cough_plus.10=="yes"]<-1
wideimp$cough_plus.10n[wideimp$cough_plus.10=="no"]<-0
table(wideimp$cough_plus.10n)

# 5 Sore throat QM6 

summary(wideimp$throat.10)
wideimp$throat.10n<-NA
wideimp$throat.10n[wideimp$throat.10=="yes"]<-1
wideimp$throat.10n[wideimp$throat.10=="no"]<-0
table(wideimp$throat.10n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.10)
wideimp$smell.10n<-NA
wideimp$smell.10n[wideimp$smell.10=="yes"]<-1
wideimp$smell.10n[wideimp$smell.10=="no"]<-0
table(wideimp$smell.10n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.10)
wideimp$diarrhea.10n<-NA
wideimp$diarrhea.10n[wideimp$diarrhea.10=="yes"]<-1
wideimp$diarrhea.10n[wideimp$diarrhea.10=="no"]<-0
table(wideimp$diarrhea.10n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.10)
wideimp$muscle.10n<-NA
wideimp$muscle.10n[wideimp$muscle.10=="yes"]<-1
wideimp$muscle.10n[wideimp$muscle.10=="no"]<-0
table(wideimp$muscle.10n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.10)
wideimp$chest.10n<-NA
wideimp$chest.10n[wideimp$chest.10=="yes"]<-1
wideimp$chest.10n[wideimp$chest.10=="no"]<-0
table(wideimp$chest.10n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.10)
wideimp$pain.10n<-NA
wideimp$pain.10n[wideimp$pain.10=="yes"]<-1
wideimp$pain.10n[wideimp$pain.10=="no"]<-0
table(wideimp$pain.10n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.10)
wideimp$fever.10n<-NA
wideimp$fever.10n[wideimp$fever.10=="yes"]<-1
wideimp$fever.10n[wideimp$fever.10=="no"]<-0
table(wideimp$fever.10n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.10)
wideimp$breath.10n<-NA
wideimp$breath.10n[wideimp$breath.10=="yes"]<-1
wideimp$breath.10n[wideimp$breath.10=="no"]<-0
table(wideimp$breath.10n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.10)
wideimp$breath_plus.10n<-NA
wideimp$breath_plus.10n[wideimp$breath_plus.10=="yes"]<-1
wideimp$breath_plus.10n[wideimp$breath_plus.10=="no"]<-0
table(wideimp$breath_plus.10n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.10)
wideimp$eat.10n<-NA
wideimp$eat.10n[wideimp$eat.10=="yes"]<-1
wideimp$eat.10n[wideimp$eat.10=="no"]<-0
table(wideimp$eat.10n)




# 15 Other symptoms ?
summary(wideimp$other.10)
wideimp$other.10n<-NA
wideimp$other.10n[wideimp$other.10=="yes"]<-1
wideimp$other.10n[wideimp$other.10=="no"]<-0
table(wideimp$other.10n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.10)
wideimp$skin.10n<-NA
wideimp$skin.10n[wideimp$skin.10=="yes"]<-1
wideimp$skin.10n[wideimp$skin.10=="no"]<-0
table(wideimp$skin.10n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.10)
wideimp$eyes.10n[wideimp$eyes.10=="yes"]<-1
wideimp$eyes.10n[wideimp$eyes.10=="no"]<-0
table(wideimp$eyes.10n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.10)
wideimp$hospital10[wideimp$hospital.10=="yes"]<-1
wideimp$hospital10[wideimp$hospital.10=="no"]<-0
table(wideimp$hospital10)




wideimp$score10<-NA
wideimp$score10<-wideimp$fatigue.10n + wideimp$sleep.10n + wideimp$cough.10n + 
  wideimp$cough_plus.10n + wideimp$throat.10n + wideimp$smell.10n + 
  wideimp$diarrhea.10n + wideimp$muscle.10n + wideimp$chest.10n + 
  wideimp$pain.10n + wideimp$fever.10n + wideimp$breath.10n + 
  wideimp$breath_plus.10n + wideimp$eat.10n + wideimp$other.10n + 
  wideimp$skin.10n + wideimp$eyes.10n
summary(wideimp$score10)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.11)
wideimp$fatigue.11n<-NA
wideimp$fatigue.11n[wideimp$fatigue.11=="bad"]<-2
wideimp$fatigue.11n[wideimp$fatigue.11=="fatigue"]<-1
wideimp$fatigue.11n[wideimp$fatigue.11=="well"]<-0
table(wideimp$fatigue.11n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.11n<-NA
wideimp$sleep.11n[wideimp$sleep.11=="bad sleep"]<-1
wideimp$sleep.11n[wideimp$sleep.11=="well sleep"]<-0
table(wideimp$sleep.11n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.11)
wideimp$cough.11n<-NA
wideimp$cough.11n[wideimp$cough.11=="yes"]<-1
wideimp$cough.11n[wideimp$cough.11=="no"]<-0
table(wideimp$cough.11n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.11)
wideimp$cough_plus.11n<-NA
wideimp$cough_plus.11n[wideimp$cough_plus.11=="yes"]<-1
wideimp$cough_plus.11n[wideimp$cough_plus.11=="no"]<-0
table(wideimp$cough_plus.11n)

# 5 Sore throat QM6 

summary(wideimp$throat.11)
wideimp$throat.11n<-NA
wideimp$throat.11n[wideimp$throat.11=="yes"]<-1
wideimp$throat.11n[wideimp$throat.11=="no"]<-0
table(wideimp$throat.11n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.11)
wideimp$smell.11n<-NA
wideimp$smell.11n[wideimp$smell.11=="yes"]<-1
wideimp$smell.11n[wideimp$smell.11=="no"]<-0
table(wideimp$smell.11n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.11)
wideimp$diarrhea.11n<-NA
wideimp$diarrhea.11n[wideimp$diarrhea.11=="yes"]<-1
wideimp$diarrhea.11n[wideimp$diarrhea.11=="no"]<-0
table(wideimp$diarrhea.11n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.11)
wideimp$muscle.11n<-NA
wideimp$muscle.11n[wideimp$muscle.11=="yes"]<-1
wideimp$muscle.11n[wideimp$muscle.11=="no"]<-0
table(wideimp$muscle.11n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.11)
wideimp$chest.11n<-NA
wideimp$chest.11n[wideimp$chest.11=="yes"]<-1
wideimp$chest.11n[wideimp$chest.11=="no"]<-0
table(wideimp$chest.11n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.11)
wideimp$pain.11n<-NA
wideimp$pain.11n[wideimp$pain.11=="yes"]<-1
wideimp$pain.11n[wideimp$pain.11=="no"]<-0
table(wideimp$pain.11n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.11)
wideimp$fever.11n<-NA
wideimp$fever.11n[wideimp$fever.11=="yes"]<-1
wideimp$fever.11n[wideimp$fever.11=="no"]<-0
table(wideimp$fever.11n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.11)
wideimp$breath.11n<-NA
wideimp$breath.11n[wideimp$breath.11=="yes"]<-1
wideimp$breath.11n[wideimp$breath.11=="no"]<-0
table(wideimp$breath.11n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.11)
wideimp$breath_plus.11n<-NA
wideimp$breath_plus.11n[wideimp$breath_plus.11=="yes"]<-1
wideimp$breath_plus.11n[wideimp$breath_plus.11=="no"]<-0
table(wideimp$breath_plus.11n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.11)
wideimp$eat.11n<-NA
wideimp$eat.11n[wideimp$eat.11=="yes"]<-1
wideimp$eat.11n[wideimp$eat.11=="no"]<-0
table(wideimp$eat.11n)




# 15 Other symptoms ?
summary(wideimp$other.11)
wideimp$other.11n<-NA
wideimp$other.11n[wideimp$other.11=="yes"]<-1
wideimp$other.11n[wideimp$other.11=="no"]<-0
table(wideimp$other.11n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.11)
wideimp$skin.11n<-NA
wideimp$skin.11n[wideimp$skin.11=="yes"]<-1
wideimp$skin.11n[wideimp$skin.11=="no"]<-0
table(wideimp$skin.11n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.11)
wideimp$eyes.11n[wideimp$eyes.11=="yes"]<-1
wideimp$eyes.11n[wideimp$eyes.11=="no"]<-0
table(wideimp$eyes.11n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.11)
wideimp$hospital11[wideimp$hospital.11=="yes"]<-1
wideimp$hospital11[wideimp$hospital.11=="no"]<-0
table(wideimp$hospital11)




wideimp$score11<-NA
wideimp$score11<-wideimp$fatigue.11n + wideimp$sleep.11n + wideimp$cough.11n + 
  wideimp$cough_plus.11n + wideimp$throat.11n + wideimp$smell.11n + 
  wideimp$diarrhea.11n + wideimp$muscle.11n + wideimp$chest.11n + 
  wideimp$pain.11n + wideimp$fever.11n + wideimp$breath.11n + 
  wideimp$breath_plus.11n + wideimp$eat.11n + wideimp$other.11n + 
  wideimp$skin.11n + wideimp$eyes.11n
summary(wideimp$score11)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.12)
wideimp$fatigue.12n<-NA
wideimp$fatigue.12n[wideimp$fatigue.12=="bad"]<-2
wideimp$fatigue.12n[wideimp$fatigue.12=="fatigue"]<-1
wideimp$fatigue.12n[wideimp$fatigue.12=="well"]<-0
table(wideimp$fatigue.12n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.12n<-NA
wideimp$sleep.12n[wideimp$sleep.12=="bad sleep"]<-1
wideimp$sleep.12n[wideimp$sleep.12=="well sleep"]<-0
table(wideimp$sleep.12n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.12)
wideimp$cough.12n<-NA
wideimp$cough.12n[wideimp$cough.12=="yes"]<-1
wideimp$cough.12n[wideimp$cough.12=="no"]<-0
table(wideimp$cough.12n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.12)
wideimp$cough_plus.12n<-NA
wideimp$cough_plus.12n[wideimp$cough_plus.12=="yes"]<-1
wideimp$cough_plus.12n[wideimp$cough_plus.12=="no"]<-0
table(wideimp$cough_plus.12n)

# 5 Sore throat QM6 

summary(wideimp$throat.12)
wideimp$throat.12n<-NA
wideimp$throat.12n[wideimp$throat.12=="yes"]<-1
wideimp$throat.12n[wideimp$throat.12=="no"]<-0
table(wideimp$throat.12n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.12)
wideimp$smell.12n<-NA
wideimp$smell.12n[wideimp$smell.12=="yes"]<-1
wideimp$smell.12n[wideimp$smell.12=="no"]<-0
table(wideimp$smell.12n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.12)
wideimp$diarrhea.12n<-NA
wideimp$diarrhea.12n[wideimp$diarrhea.12=="yes"]<-1
wideimp$diarrhea.12n[wideimp$diarrhea.12=="no"]<-0
table(wideimp$diarrhea.12n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.12)
wideimp$muscle.12n<-NA
wideimp$muscle.12n[wideimp$muscle.12=="yes"]<-1
wideimp$muscle.12n[wideimp$muscle.12=="no"]<-0
table(wideimp$muscle.12n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.12)
wideimp$chest.12n<-NA
wideimp$chest.12n[wideimp$chest.12=="yes"]<-1
wideimp$chest.12n[wideimp$chest.12=="no"]<-0
table(wideimp$chest.12n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.12)
wideimp$pain.12n<-NA
wideimp$pain.12n[wideimp$pain.12=="yes"]<-1
wideimp$pain.12n[wideimp$pain.12=="no"]<-0
table(wideimp$pain.12n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.12)
wideimp$fever.12n<-NA
wideimp$fever.12n[wideimp$fever.12=="yes"]<-1
wideimp$fever.12n[wideimp$fever.12=="no"]<-0
table(wideimp$fever.12n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.12)
wideimp$breath.12n<-NA
wideimp$breath.12n[wideimp$breath.12=="yes"]<-1
wideimp$breath.12n[wideimp$breath.12=="no"]<-0
table(wideimp$breath.12n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.12)
wideimp$breath_plus.12n<-NA
wideimp$breath_plus.12n[wideimp$breath_plus.12=="yes"]<-1
wideimp$breath_plus.12n[wideimp$breath_plus.12=="no"]<-0
table(wideimp$breath_plus.12n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.12)
wideimp$eat.12n<-NA
wideimp$eat.12n[wideimp$eat.12=="yes"]<-1
wideimp$eat.12n[wideimp$eat.12=="no"]<-0
table(wideimp$eat.12n)




# 15 Other symptoms ?
summary(wideimp$other.12)
wideimp$other.12n<-NA
wideimp$other.12n[wideimp$other.12=="yes"]<-1
wideimp$other.12n[wideimp$other.12=="no"]<-0
table(wideimp$other.12n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.12)
wideimp$skin.12n<-NA
wideimp$skin.12n[wideimp$skin.12=="yes"]<-1
wideimp$skin.12n[wideimp$skin.12=="no"]<-0
table(wideimp$skin.12n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.12)
wideimp$eyes.12n[wideimp$eyes.12=="yes"]<-1
wideimp$eyes.12n[wideimp$eyes.12=="no"]<-0
table(wideimp$eyes.12n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.12)
wideimp$hospital12[wideimp$hospital.12=="yes"]<-1
wideimp$hospital12[wideimp$hospital.12=="no"]<-0
table(wideimp$hospital12)




wideimp$score12<-NA
wideimp$score12<-wideimp$fatigue.12n + wideimp$sleep.12n + wideimp$cough.12n + 
  wideimp$cough_plus.12n + wideimp$throat.12n + wideimp$smell.12n + 
  wideimp$diarrhea.12n + wideimp$muscle.12n + wideimp$chest.12n + 
  wideimp$pain.12n + wideimp$fever.12n + wideimp$breath.12n + 
  wideimp$breath_plus.12n + wideimp$eat.12n + wideimp$other.12n + 
  wideimp$skin.12n + wideimp$eyes.12n
summary(wideimp$score12)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.13)
wideimp$fatigue.13n<-NA
wideimp$fatigue.13n[wideimp$fatigue.13=="bad"]<-2
wideimp$fatigue.13n[wideimp$fatigue.13=="fatigue"]<-1
wideimp$fatigue.13n[wideimp$fatigue.13=="well"]<-0
table(wideimp$fatigue.13n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.13n<-NA
wideimp$sleep.13n[wideimp$sleep.13=="bad sleep"]<-1
wideimp$sleep.13n[wideimp$sleep.13=="well sleep"]<-0
table(wideimp$sleep.13n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.13)
wideimp$cough.13n<-NA
wideimp$cough.13n[wideimp$cough.13=="yes"]<-1
wideimp$cough.13n[wideimp$cough.13=="no"]<-0
table(wideimp$cough.13n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.13)
wideimp$cough_plus.13n<-NA
wideimp$cough_plus.13n[wideimp$cough_plus.13=="yes"]<-1
wideimp$cough_plus.13n[wideimp$cough_plus.13=="no"]<-0
table(wideimp$cough_plus.13n)

# 5 Sore throat QM6 

summary(wideimp$throat.13)
wideimp$throat.13n<-NA
wideimp$throat.13n[wideimp$throat.13=="yes"]<-1
wideimp$throat.13n[wideimp$throat.13=="no"]<-0
table(wideimp$throat.13n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.13)
wideimp$smell.13n<-NA
wideimp$smell.13n[wideimp$smell.13=="yes"]<-1
wideimp$smell.13n[wideimp$smell.13=="no"]<-0
table(wideimp$smell.13n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.13)
wideimp$diarrhea.13n<-NA
wideimp$diarrhea.13n[wideimp$diarrhea.13=="yes"]<-1
wideimp$diarrhea.13n[wideimp$diarrhea.13=="no"]<-0
table(wideimp$diarrhea.13n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.13)
wideimp$muscle.13n<-NA
wideimp$muscle.13n[wideimp$muscle.13=="yes"]<-1
wideimp$muscle.13n[wideimp$muscle.13=="no"]<-0
table(wideimp$muscle.13n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.13)
wideimp$chest.13n<-NA
wideimp$chest.13n[wideimp$chest.13=="yes"]<-1
wideimp$chest.13n[wideimp$chest.13=="no"]<-0
table(wideimp$chest.13n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.13)
wideimp$pain.13n<-NA
wideimp$pain.13n[wideimp$pain.13=="yes"]<-1
wideimp$pain.13n[wideimp$pain.13=="no"]<-0
table(wideimp$pain.13n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.13)
wideimp$fever.13n<-NA
wideimp$fever.13n[wideimp$fever.13=="yes"]<-1
wideimp$fever.13n[wideimp$fever.13=="no"]<-0
table(wideimp$fever.13n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.13)
wideimp$breath.13n<-NA
wideimp$breath.13n[wideimp$breath.13=="yes"]<-1
wideimp$breath.13n[wideimp$breath.13=="no"]<-0
table(wideimp$breath.13n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.13)
wideimp$breath_plus.13n<-NA
wideimp$breath_plus.13n[wideimp$breath_plus.13=="yes"]<-1
wideimp$breath_plus.13n[wideimp$breath_plus.13=="no"]<-0
table(wideimp$breath_plus.13n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.13)
wideimp$eat.13n<-NA
wideimp$eat.13n[wideimp$eat.13=="yes"]<-1
wideimp$eat.13n[wideimp$eat.13=="no"]<-0
table(wideimp$eat.13n)




# 15 Other symptoms ?
summary(wideimp$other.13)
wideimp$other.13n<-NA
wideimp$other.13n[wideimp$other.13=="yes"]<-1
wideimp$other.13n[wideimp$other.13=="no"]<-0
table(wideimp$other.13n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.13)
wideimp$skin.13n<-NA
wideimp$skin.13n[wideimp$skin.13=="yes"]<-1
wideimp$skin.13n[wideimp$skin.13=="no"]<-0
table(wideimp$skin.13n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.13)
wideimp$eyes.13n[wideimp$eyes.13=="yes"]<-1
wideimp$eyes.13n[wideimp$eyes.13=="no"]<-0
table(wideimp$eyes.13n)
# 
# # 18. Vous ?tes ? la maison, ? l'h?pital


table(wideimp$hospital.13)
wideimp$hospital13[wideimp$hospital.13=="yes"]<-1
wideimp$hospital13[wideimp$hospital.13=="no"]<-0
table(wideimp$hospital13)




wideimp$score13<-NA
wideimp$score13<-wideimp$fatigue.13n + wideimp$sleep.13n + wideimp$cough.13n + 
  wideimp$cough_plus.13n + wideimp$throat.13n + wideimp$smell.13n + 
  wideimp$diarrhea.13n + wideimp$muscle.13n + wideimp$chest.13n + 
  wideimp$pain.13n + wideimp$fever.13n + wideimp$breath.13n + 
  wideimp$breath_plus.13n + wideimp$eat.13n + wideimp$other.13n + 
  wideimp$skin.13n + wideimp$eyes.13n
summary(wideimp$score13)

# Bulding a symptom score

# 1. Fatigue. Question QM2: Comment vous sentez-vous aujourd'hui ?
# Answer: 1, Je me sens bien | 2, Je me sens fatigu?(e) | 3, Je me sens mal

summary(wideimp$fatigue.14)
wideimp$fatigue.14n<-NA
wideimp$fatigue.14n[wideimp$fatigue.14=="bad"]<-2
wideimp$fatigue.14n[wideimp$fatigue.14=="fatigue"]<-1
wideimp$fatigue.14n[wideimp$fatigue.14=="well"]<-0
table(wideimp$fatigue.14n)


# 2. Sleep. Question QM3 Avez-vous bien dormi ?
# The question is positive and I have to inverse to negative
# Answer 1, Oui | 0, Non

wideimp$sleep.14n<-NA
wideimp$sleep.14n[wideimp$sleep.14=="bad sleep"]<-1
wideimp$sleep.14n[wideimp$sleep.14=="well sleep"]<-0
table(wideimp$sleep.14n)


# 3 Dry cough Question QM4 Avez-vous une toux s?che ?
# Answer 1, Oui | 0, Non


summary(wideimp$cough.14)
wideimp$cough.14n<-NA
wideimp$cough.14n[wideimp$cough.14=="yes"]<-1
wideimp$cough.14n[wideimp$cough.14=="no"]<-0
table(wideimp$cough.14n)

# 4 Increased cough QM5 Avez-vous une augmentation de votre toux habituelle ces derniers jours ? 

summary(wideimp$cough_plus.14)
wideimp$cough_plus.14n<-NA
wideimp$cough_plus.14n[wideimp$cough_plus.14=="yes"]<-1
wideimp$cough_plus.14n[wideimp$cough_plus.14=="no"]<-0
table(wideimp$cough_plus.14n)

# 5 Sore throat QM6 

summary(wideimp$throat.14)
wideimp$throat.14n<-NA
wideimp$throat.14n[wideimp$throat.14=="yes"]<-1
wideimp$throat.14n[wideimp$throat.14=="no"]<-0
table(wideimp$throat.14n)



# 6 Loss of taste/smell QM7 8. Avez-vous not? une forte diminution ou perte de votre 
# go?t ou de votre odorat ?


summary(wideimp$smell.14)
wideimp$smell.14n<-NA
wideimp$smell.14n[wideimp$smell.14=="yes"]<-1
wideimp$smell.14n[wideimp$smell.14=="no"]<-0
table(wideimp$smell.14n)



# 7. Diarrhea QM8 Avez-vous de la diarrh?e ? Avec au moins 3 selles liquides/molles par jour.


summary(wideimp$diarrhea.14)
wideimp$diarrhea.14n<-NA
wideimp$diarrhea.14n[wideimp$diarrhea.14=="yes"]<-1
wideimp$diarrhea.14n[wideimp$diarrhea.14=="no"]<-0
table(wideimp$diarrhea.14n)



# 8 Muscle aches QM9 Avez-vous des douleurs musculaires ou des courbatures 
# inhabituelles ces derniers jours ?


summary(wideimp$muscle.14)
wideimp$muscle.14n<-NA
wideimp$muscle.14n[wideimp$muscle.14=="yes"]<-1
wideimp$muscle.14n[wideimp$muscle.14=="no"]<-0
table(wideimp$muscle.14n)



# # 9 Chest pain QM10 Avez-vous des douleurs thoraciques ces derniers jours ?
summary(wideimp$chest.14)
wideimp$chest.14n<-NA
wideimp$chest.14n[wideimp$chest.14=="yes"]<-1
wideimp$chest.14n[wideimp$chest.14=="no"]<-0
table(wideimp$chest.14n)




# 10 Pain scale Quel est votre niveau de douleur actuel ?

summary(wideimp$pain.14)
wideimp$pain.14n<-NA
wideimp$pain.14n[wideimp$pain.14=="yes"]<-1
wideimp$pain.14n[wideimp$pain.14=="no"]<-0
table(wideimp$pain.14n)


# 11 Fever  Avez-vous de la fi?vre ?
summary(wideimp$fever.14)
wideimp$fever.14n<-NA
wideimp$fever.14n[wideimp$fever.14=="yes"]<-1
wideimp$fever.14n[wideimp$fever.14=="no"]<-0
table(wideimp$fever.14n)




# 12 Difficulty breathing Avez-vous des difficult?s respiratoires ?
summary(wideimp$breath.14)
wideimp$breath.14n<-NA
wideimp$breath.14n[wideimp$breath.14=="yes"]<-1
wideimp$breath.14n[wideimp$breath.14=="no"]<-0
table(wideimp$breath.14n)



# 13 Increased breath difficulties  Avez-vous vu appara?tre une 
# g?ne respiratoire ou une augmentation de votre g?ne respiratoire habituelle ?
summary(wideimp$breath_plus.14)
wideimp$breath_plus.14n<-NA
wideimp$breath_plus.14n[wideimp$breath_plus.14=="yes"]<-1
wideimp$breath_plus.14n[wideimp$breath_plus.14=="no"]<-0
table(wideimp$breath_plus.14n)




# 14 Difficulties to eat or drink ?  Avez-vous des difficult?s importantes 
# pour vous alimenter ou boire ?
summary(wideimp$eat.14)
wideimp$eat.14n<-NA
wideimp$eat.14n[wideimp$eat.14=="yes"]<-1
wideimp$eat.14n[wideimp$eat.14=="no"]<-0
table(wideimp$eat.14n)




# 15 Other symptoms ?
summary(wideimp$other.14)
wideimp$other.14n<-NA
wideimp$other.14n[wideimp$other.14=="yes"]<-1
wideimp$other.14n[wideimp$other.14=="no"]<-0
table(wideimp$other.14n)



# 16 Avez-vous not? une apparition subite d'?ruptions cutan?es au niveau 
# des mains ou des pieds (par exemple engelures, rougeurs persistantes parfois 
# douloureuses, l?sions d'urticaire passag?res) ?
summary(wideimp$skin.14)
wideimp$skin.14n<-NA
wideimp$skin.14n[wideimp$skin.14=="yes"]<-1
wideimp$skin.14n[wideimp$skin.14=="no"]<-0
table(wideimp$skin.14n)




# 17 Avez-vous not? l'apparition d'une conjonctivite ou de douleurs dans 
# les yeux (rougeurs persistantes au niveau du blanc de l'oil, d?mangeaisons 
# au niveau des paupi?res, sensations de picotements, br?lure, larmoiement fr?quent) ?

summary(wideimp$eyes.14)
wideimp$eyes.14n[wideimp$eyes.14=="yes"]<-1
wideimp$eyes.14n[wideimp$eyes.14=="no"]<-0
table(wideimp$eyes.14n)
# 
# # 18. Vous etes a la maison, ? l'hopital


table(wideimp$hospital.14)
wideimp$hospital14[wideimp$hospital.14=="yes"]<-1
wideimp$hospital14[wideimp$hospital.14=="no"]<-0
table(wideimp$hospital14)




wideimp$score14<-NA
wideimp$score14<-wideimp$fatigue.14n + wideimp$sleep.14n + wideimp$cough.14n + 
  wideimp$cough_plus.14n + wideimp$throat.14n + wideimp$smell.14n + 
  wideimp$diarrhea.14n + wideimp$muscle.14n + wideimp$chest.14n + 
  wideimp$pain.14n + wideimp$fever.14n + wideimp$breath.14n + 
  wideimp$breath_plus.14n + wideimp$eat.14n + wideimp$other.14n + 
  wideimp$skin.14n + wideimp$eyes.14n
summary(wideimp$score14)

dim(wideimp)

# Making NA again when all responses were NA

wideimp$score0[wideimp$miss0=="miss"]<-NA
wideimp$score1[wideimp$miss1=="miss"]<-NA
wideimp$score2[wideimp$miss2=="miss"]<-NA
wideimp$score3[wideimp$miss3=="miss"]<-NA
wideimp$score4[wideimp$miss4=="miss"]<-NA
wideimp$score5[wideimp$miss5=="miss"]<-NA
wideimp$score6[wideimp$miss6=="miss"]<-NA
wideimp$score7[wideimp$miss7=="miss"]<-NA
wideimp$score8[wideimp$miss8=="miss"]<-NA
wideimp$score9[wideimp$miss9=="miss"]<-NA
wideimp$score10[wideimp$miss10=="miss"]<-NA
wideimp$score11[wideimp$miss11=="miss"]<-NA
wideimp$score12[wideimp$miss12=="miss"]<-NA
wideimp$score13[wideimp$miss13=="miss"]<-NA
wideimp$score14[wideimp$miss14=="miss"]<-NA

wideimp$diabetesn<-NA
wideimp$diabetesn[wideimp$diabetes=="yes"]<-1
wideimp$diabetesn[wideimp$diabetes=="no"]<-0


wideimp$char<-NA
wideimp$char<-wideimp$hypertention_mhyn + wideimp$chroniccard_mhyn + wideimp$chronicpul_mhyn + wideimp$asthma_mhyn + 
  wideimp$renal_mhyn + wideimp$modliver_mhyn + wideimp$mildliv_mhyn + 
  wideimp$chronicneu_mhyn + wideimp$malignantneo_mhyn + wideimp$chronhaemo_mhyn + 
  wideimp$obesity_mhyn + wideimp$diabetesn + wideimp$rheumatology_mhyr + 
  wideimp$malnutrition_mhyn +wideimp$copd_mhyn+wideimp$other_mhyn
table(wideimp$char, useNA = "ifany")

wideimp$multimorb<-NA
wideimp$multimorb<-ifelse(wideimp$char<=1,"no","yes")
wideimp$multimorb<-factor(wideimp$multimorb)
table(wideimp$multimorb,useNA = "ifany")
round(100*prop.table(table(wideimp$multimorb)))


# Diabetes

table(wideimp$diabetes_mhyn,useNA = "ifany")
table(wideimp$traitement_two, useNA = "ifany")
wideimp$diabetes<-NA
wideimp$diabetes[wideimp$diabetes_mhyn==1 | wideimp$traitement_two==1]<-1
wideimp$diabetes[wideimp$diabetes_mhyn==2 & wideimp$traitement_two==0]<-0
wideimp$diabetes<-factor(wideimp$diabetes,levels = c(1,0), labels = c("yes","no"))
table(wideimp$diabetes, useNA = "ifany")
round(100*prop.table(table(wideimp$diabetes)))

# Obesity
wideimp$obesity<-ifelse(wideimp$bmi>=30,"yes","no")
wideimp$obesity<-factor(wideimp$obesity)

# Underweigth
wideimp$underw<-ifelse(wideimp$bmi<20, "yes","no")
wideimp$underw<-factor(wideimp$underw)

# Anxiety
wideimp$medanx<-NA
wideimp$medanx[wideimp$traitement_seven==1|wideimp$traitement_eight==1]<-1
wideimp$medanx[wideimp$traitement_seven==0 & wideimp$traitement_eight==0]<-2
wideimp$medanx<-factor(wideimp$medanx, levels=c(1,2), labels = c("yes","no"))
table(wideimp$medanx, useNA = "ifany")
round(100*prop.table(table(wideimp$medanx)))

# Polypharmacy

wideimp$med<-wideimp$traitement_one+wideimp$traitement_two+wideimp$traitement_three+wideimp$traitement_four+
  wideimp$traitement_five+wideimp$traitement_six+wideimp$traitement_seven+wideimp$traitement_eight+
  wideimp$traitement_nine

wideimp$medcat<-ifelse(wideimp$med>=2,"yes","no")
wideimp$medcat<-factor(wideimp$medcat)
table(wideimp$medcat, useNA = "ifany")
round(100*prop.table(table(wideimp$medcat)))

# Physical activity

lowpa<-quantile(wideimp$sum_all_activities, probs = c(0.25,0.5,0.75), na.rm = TRUE)[[1]]

wideimp$lowpa<-ifelse(wideimp$sum_all_activities<lowpa,"lowpa","nolowpa")
wideimp$lowpa<-factor(wideimp$lowpa)

table(wideimp$lowpa,useNA = "ifany")
wideimp$underwIncome
median(wideimp$income, na.rm=TRUE)
quant<-quantile(wideimp$income, probs=c(0,0.25,0.5,0.75,1),na.rm=TRUE)[[2]]
wideimp$lowincome<-ifelse(wideimp$income<quant,"low","high")
wideimp$lowincome<-factor(wideimp$lowincome)
table( wideimp$lowincome, useNA = "ifany")

wideimp$ogroup<-ifelse(wideimp$gsang==1,"Ogroup","other")
wideimp$ogroup<-factor(wideimp$ogroup)

wideimp$hospital<-wideimp$hospital0


library(dplyr)
newwide<-wideimp%>%
  select(.imp,.id,sexm,age_estimateyears,asthma_mhyn,SUBJID,diabetes,
         medanx,fiverm,smoker,multimorb,NIH_classification_or,
         bmi,obese,underw,medcat,weight_loss,lowpa,eyeglass,
         owner,loweduc,lowincome,work,lung,agroup,ogroup,
         fatiguebs,sleepbs,coughbs,cough_plusbs,throatbs,smellbs,
         diarrheabs,musclebs,chestbs,painbs,feverbs,breathbs,
         breath_plusbs,eatbs,otherbs,skinbs,eyesbs,
         traitement_one,traitement_two,traitement_three,traitement_four,
         traitement_five,traitement_six,traitement_seven,traitement_eight,
         traitement_nine, hospital,
         score0,score1,score2,score3,
         score4,score5,score6,score7,
         score8,score9,score10,score11,
         score12,score13,score14)


names(newwide)

dim(newwide)

table(newwide$.imp)

library(dplyr)
newwide5<-newwide %>%
  filter(.imp==5)
dim(newwide5)

head(newwide5)

var3<-names(newwide[c(54:68)])

newlong5<-reshape(data=newwide5,  direction ="long", varying = var3, idvar = "SUBJID", timevar="time",sep="")



newlong5$JOURn<-newlong5$time


library(lcmm)




# Models withouth predictor
modspline4 <- lcmm(score~JOURn, random =~ JOURn, subject = 'SUBJID',
                   data=newlong5, link = "4-quant-splines")
summary(modspline4)


set.seed(300)


modspline4_4<-gridsearch(lcmm(score ~ JOURn,
                              mixture=~ JOURn,
                              random=~JOURn,
                              subject='SUBJID',
                              ng=4, 
                              link='4-quant-splines',
                              data=newlong5,  
                              B=modspline4, 
                              verbose = TRUE), 
                         rep=30, 
                         maxiter=15,  
                         minit=modspline4)
summary(modspline4_4)





datnew   <- data.frame(JOURn = seq(0, 14, length = 100))
plotpred <- predictY(modspline4_4, datnew, var.time ="JOURn", draws = TRUE)
plot(plotpred, lty=1, xlab="Days", ylab="Symptoms score", legend.loc = "topleft", cex=0.75, ylim=c(0,12))




tiff(filename = "P:/Documents/COVID19/PREDICOVID/symptom_score/Figures/figure2_4class.tiff",
     width = 1200, height = 1000, units = "px", pointsize = 6, res=300)
plot(plotpred, lty=1, xlab="Days", ylab="Symptoms score", legend.loc = "topleft", cex=0.75, ylim=c(0,12))
dev.off()





#Most likely class membership
data_pred <- newlong5
data_pred$id <- as.character(data_pred$SUBJID)

# data_N <- as.data.frame(modspline4_4per$pprob[,1:2])
data_N <- as.data.frame(modspline4_4$pprob[,1:2])
data_N$id <- as.character(data_N$SUBJID)
data_pred$class <- factor( data_N$class[sapply(data_pred$id, function(x) which(data_N$id==x))] )

fatigclass<-data_pred

write.csv(fatigclass,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/fatigclass4_11012022.csv", row.names = FALSE)

fatigclass0<-fatigclass%>%
  filter(JOURn==0)
dim(fatigclass0)

table(fatigclass0$class, useNA = "ifany")
table(fatigclass$class, useNA = "ifany")
round(100*prop.table(table(fatigclass$class, useNA = "ifany")))


table(fatigclass0$class, useNA = "ifany")
round(100*prop.table(table(fatigclass0$class, useNA = "ifany")))


table(fatigclass0$class, useNA = "ifany")
dim(fatigclass0)

# Calculating posterior probalility of belong to a class
# https://rdrr.io/cran/lcmm/man/postprob.html

postprob<-postprob(modspline4_4)
names(postprob)

(cl11<-postprob[2][[1]][[1]])
(cl22<-postprob[2][[1]][[6]])
(cl33<-postprob[2][[1]][[11]])
(cl44<-postprob[2][[1]][[16]])

(cl12<-postprob[2][[1]][[2]])
(cl13<-postprob[2][[1]][[3]])
(cl14<-postprob[2][[1]][[4]])

(cl21<-postprob[2][[1]][[5]])
(cl23<-postprob[2][[1]][[7]])
(cl24<-postprob[2][[1]][[8]])

(cl31<-postprob[2][[1]][[9]])
(cl32<-postprob[2][[1]][[10]])
(cl34<-postprob[2][[1]][[12]])

(cl41<-postprob[2][[1]][[13]])
(cl42<-postprob[2][[1]][[14]])
(cl43<-postprob[2][[1]][[15]])




Classes<-c("Cl1/Cl1","Cl2/Cl2","Cl3/Cl3", "Cl4/Cl4",
           "Cl1/Cl2","Cl1/Cl3","Cl1/Cl4",
           "Cl2/Cl1","Cl2/Cl3","Cl2/Cl4",
           "Cl3/Cl1","Cl3/Cl2","Cl3/Cl4",
           "Cl4/Cl1","Cl4/Cl2","Cl4/Cl3")
Probability<-c(cl11,cl22,cl33,cl44,cl12,cl13,cl14,cl21,cl23,cl24,cl31,cl32,cl34,cl41,cl42,cl43)
truepositive<-c(1,1,1,1,rep(0,12))
truepositive<-factor(truepositive, levels=c(1,0), labels = c("true positive","false positive"))
postprobability<-data.frame(Classes,Probability,truepositive)

write.csv(postprobability,"P:/Documents/COVID19/PREDICOVID/symptom_score/tables/table_postprob.csv")

library(ggplot2)

p<-ggplot(data=postprobability, aes(x=Classes, y=Probability, fill=truepositive)) +
  geom_bar(stat="identity")+
  theme_bw()
p

p + scale_fill_manual(values=c("#999999", "#E69F00"))

tiff(filename = "P:/Documents/COVID19/PREDICOVID/symptom_score/Figures/figure3Post_class.tiff",
     width = 1200, height = 1000, units = "px", pointsize = 4, res=300)
p + labs(fill = "Classification") + scale_fill_manual(values=c("green", "red")) +
  theme(legend.title = element_text(colour="black", size=8), legend.position = "bottom" ) +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=90, hjust=1)) 

dev.off()



