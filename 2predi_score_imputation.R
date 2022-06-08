# Imputation
Sys.setenv(LANG = "en")


fatig<-read.csv("P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/fatigw751.csv",stringsAsFactors=TRUE)
# fatig<-read.csv("Prediscore/generated_data/fatigw.csv",stringsAsFactors=TRUE)

dim(fatig)
library(dplyr)



names(fatig)
class(fatig$fatigue.0)
#I exclude variables that are producing problems in the imputation and I will not use


fatig$SUBJID_1<-NULL
fatig$Subject_ID<-NULL
fatig$ALIME_DEV<-NULL
fatig$ALIMG_DEV<-NULL
fatig$ALIML_DEV<-NULL
fatig$sex<-NULL

# I transform SUBJID in character
fatig$Subject_ID<-

#I check if there are missing

anyNA(fatig)



#I check if there are missing in each variable that I am interested for

apply(fatig, 2, anyNA)


#I do a list with each variable and number of missing
missing<-colSums(is.na(fatig))

#I do a list with each variable and percent of missing
percent<-colMeans(is.na(fatig))*100
class(percent)

#I build a dataframe with missing data
na<-data.frame(missing,percent)
names(na)



#I order the dataframe "na" by percent of missing (to improve the imputation)
naord <- na[order(missing),] 
class(naord)




library(dplyr)
naord<-add_rownames(naord,var="variables")




#I put my list of variables to impute according to this ordered list of missing.
vars4<-naord$variables

vars4<-as.character(vars4)

anyNA(fatig[,vars4])

#I check if there are missing in each variable that I am interested for

apply(fatig[,vars4], 2, anyNA)


#I do a list with each variable and number of missing
missing<-colSums(is.na(fatig[,vars4]))

#I do a list with each variable and percent of missing
percent<-colMeans(is.na(fatig[,vars4]))*100
class(percent)

#I build a dataframe with missing data
na<-data.frame(missing,percent)
names(na)



#I order the dataframe "na" by percent of missing (to improve the imputation)
naord <- na[order(missing),] 
class(naord)


naord<-add_rownames(naord,var="variables")

write.csv(naord,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/missing.csv",row.names = FALSE)

#md.pairs() function to calculate the frequency in each pattern for all variable pairs (I used it just to 
#test if my imputation will work).
library(mice)
p<-md.pairs(fatig[,vars4])
p$mr/(p$mr+p$mm)

#I use the function quickpred to calculate automatically the usable cases

calc<-quickpred(fatig[,vars4])
colSums(calc)
calc2<-colSums(calc)

class(calc2)
calc2<-as.data.frame(calc2)
class(calc2)

library(tibble)
calc2 <- rownames_to_column(calc2, "variable")
rank<-calc2[order(-calc2$calc2),]

rank$calc2[rank$variable=="age_estimateyears"]
rank$calc2[rank$variable=="sex"]
rank$calc2[rank$variable=="diabetes"]
rank$calc2[rank$variable=="medanx"]
rank$calc2[rank$variable=="multimorb"]


#I calculate the poor predictors to exclude them from the predictor matrix, 
#but not excluding sex 

median(rank$calc2)
quant<-quantile(rank$calc2,probs = c(0,0.25,0.5,0.75,1))[[3]]


rank2<-rank
rank2<-rank[which(rank$calc2<quant & rank$variable!="sex"&rank$variable!="diabetes" & 
                    rank$variable!="multimorb" & rank$variable!="medanx" & rank$variable!="multimorb"),]
dim(rank2)

rank3<-rank[which(rank$calc2>=quant | rank$variable =="sex"|rank$variable=="diabetes" | 
                    rank$variable=="multimorb" | rank$variable=="medanx"| rank$variable=="multimorb"),]
dim(rank3)


library(dplyr)
rank3ord<-rank3%>%
  arrange(variable)


write.csv(rank3, "P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/predshort.csv",row.names = F)

#varspoor will be all predictors that have a poor prediction and in consequence, to exclude

rank2$variable2<-as.character(rank2$variable)
class(rank2$variable2)
varspoor<-rank2$variable2
sort(varspoor)

pred2<-read.csv( "P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/predshort_chosen.csv")
# pred2<-read.csv( "Prediscore/generated_data/predshort_chosen.csv")

pred2$variable<-as.character(pred2$variable)
badpred<-pred2[which(pred2$chosen==0),]
dim(badpred)
varsdupl<-badpred$variable
length(varsdupl)
goodpred<-pred2[which(pred2$chosen==1),]
dim(goodpred)



names(goodpred)
varsgood<-goodpred$variable
length(varsgood)

# Before to impute, I perform a dry run that is a fast way to create the mids object 
#ini containing the default settings.

ini<-mice(fatig[,vars4],maxit=0,print=FALSE)
class(ini)

#I include meth in the "ini", which is a mids object

meth <- ini$meth



#I build my prediction matrix, putting pred as a mids obect with ini.
pred<-ini$pred
dim(pred)

#I build my prediction matrix, excluding these variables from the prediction model, but
#not from the imputation, they are to be imputed, but they are not to be predictors
#(they make the imputation impossible), so all the other variables that are not in this matrix are predictors


#I delete predictors variables that are in more than one wave, choosing one of them (the best predictor)


pred[,varspoor]<- 0
pred[,varsdupl]<- 0

dim(pred)

#I check my matrix
is.matrix(pred)

pred1<-as.data.frame(pred)
pred1<-t(pred1)
dim(pred1)
#View(pred1)

pred1$tot<-NA
pred1$tot<-sapply(pred1,sum)
pred1true<-pred1[pred1$tot>0]
pred1truet<-t(pred1true)
dim(pred1truet)



#### Imputation ####
library(Rcpp)
library("mice")

dim(fatig[,vars4])

#I put a seed, to ensure the imputation will be reproducible in the same way.
imp<-mice(fatig[,vars4],meth=meth,pred=pred,maxit=20,m=40,seed=2003)

# options(error=recover)

#### Extracting imputed data

long<-complete(imp,"long",include=TRUE)

write.csv(long,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/long.csv")

# write.csv(long, "Prediscore/generated_data/long.csv") 


dim(long)
names(long)

table(long$.imp)

library(dplyr)
long5<-long%>%
  filter(.imp==5)

long0<-long%>%
  filter(.imp==0)

dim(long5)

summary(long5$score.14)
summary(long5$fatigue.14)

summary(long0$score.14)
summary(long0$fatigue.0)

summary(long5$owner)     



