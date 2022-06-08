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
  left_join(classesshort, by = c("SUBJID","JOURn")) %>%
  filter(sexm=="male")

# newlongcc<-newlongc[,c(2,3,1,4:48)]

newlongc$class_f<-NA
newlongc$class_f<-factor(newlongc$class,levels = c(1,2,3,4), labels = c("c1","c2","c3","c4"))
newlongc$class_f<- relevel(newlongc$class_f, ref = "c1")
table(newlongc$class_f,useNA = "ifany")


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
# imp<-as.mids(newlongc)



library(nnet)


# Model 0
set.seed(6683)
model0<-with(datListImp2,multinom(relevel(class_f,"c1") ~ medanx))

(mod0<-testEstimates(model0))

names(mod0)

mod_0<-as.data.frame(mod0[[2]])
mod_0$mod<-"mod0"
mod_0<-add_rownames(mod_0,"var")
mod_0$var<-c("int2","int3","int4","anx2","anx3","anx4")

(coefint2<-mod_0$Estimate[1])
(seint2<-mod_0$Std.Error[1])
(coefint3<-mod_0$Estimate[2])
(seint3<-mod_0$Std.Error[2])
(coefint4<-mod_0$Estimate[3])
(seint4<-mod_0$Std.Error[3])

(coefanx2<-mod_0$Estimate[4])
(seanx2<-mod_0$Std.Error[4])
(coefanx3<-mod_0$Estimate[5])
(seanx3<-mod_0$Std.Error[5])
(coefanx4<-mod_0$Estimate[6])
(seanx4<-mod_0$Std.Error[6])

(expint2<-exp(coefint2))
(expseint2<-exp(seint2))
(expint3<-exp(coefint3))
(expseint3<-exp(seint3))
(expint4<-exp(coefint4))
(expseint4<-exp(seint4))

(expanx2<-exp(coefanx2))
(expseanx2<-exp(seanx2))
(expanx3<-exp(coefanx3))
(expseanx3<-exp(seanx3))
(expanx4<-exp(coefanx4))
(expseanx4<-exp(seanx4))

(lciint2<-expint2-1.95*expseint2)
(uciint2<-expint2+1.95*expseint2)
(lciint3<-expint3-1.95*expseint3)
(uciint3<-expint3+1.95*expseint3)
(lciint4<-expint4-1.95*expseint4)
(uciint4<-expint4+1.95*expseint4)

(lcianx2<-expanx2-1.95*expseanx2)
(ucianx2<-expanx2+1.95*expseanx2)
(lcianx3<-expanx3-1.95*expseanx3)
(ucianx3<-expanx3+1.95*expseanx3)
(lcianx4<-expanx4-1.95*expseanx4)
(ucianx4<-expanx4+1.95*expseanx4)

mod0d<-c("mod0",expanx2,lcianx2,ucianx2,expanx3,lcianx3,ucianx3,expanx4,lcianx4,ucianx4)  
mod0d<-t(mod0d)
mod0d<-as.data.frame(mod0d)

names(mod0d)<-c("mod","expanx2","lcianx2","ucianx2","expanx3","lcianx3","ucianx3","expanx4","lcianx4","ucianx4")                      



# Model 1

model1<-with(datListImp2,multinom(relevel(class_f,"c1") ~ medanx  + age_estimateyears))
summary(model1)

(mod1<-testEstimates(model1))

names(mod1)

mod_1<-as.data.frame(mod1[[2]])
mod_1$mod<-"mod1"
mod_1<-add_rownames(mod_1,"var")
mod_1$var<-c("int2","int3","int4","anx2","anx3","anx4")

(coefint2<-mod_1$Estimate[1])
(seint2<-mod_1$Std.Error[1])
(coefint3<-mod_1$Estimate[2])
(seint3<-mod_1$Std.Error[2])
(coefint4<-mod_1$Estimate[3])
(seint4<-mod_1$Std.Error[3])

(coefanx2<-mod_1$Estimate[4])
(seanx2<-mod_1$Std.Error[4])
(coefanx3<-mod_1$Estimate[5])
(seanx3<-mod_1$Std.Error[5])
(coefanx4<-mod_1$Estimate[6])
(seanx4<-mod_1$Std.Error[6])

(expint2<-exp(coefint2))
(expseint2<-exp(seint2))
(expint3<-exp(coefint3))
(expseint3<-exp(seint3))
(expint4<-exp(coefint4))
(expseint4<-exp(seint4))

(expanx2<-exp(coefanx2))
(expseanx2<-exp(seanx2))
(expanx3<-exp(coefanx3))
(expseanx3<-exp(seanx3))
(expanx4<-exp(coefanx4))
(expseanx4<-exp(seanx4))

(lciint2<-expint2-1.95*expseint2)
(uciint2<-expint2+1.95*expseint2)
(lciint3<-expint3-1.95*expseint3)
(uciint3<-expint3+1.95*expseint3)
(lciint4<-expint4-1.95*expseint4)
(uciint4<-expint4+1.95*expseint4)

(lcianx2<-expanx2-1.95*expseanx2)
(ucianx2<-expanx2+1.95*expseanx2)
(lcianx3<-expanx3-1.95*expseanx3)
(ucianx3<-expanx3+1.95*expseanx3)
(lcianx4<-expanx4-1.95*expseanx4)
(ucianx4<-expanx4+1.95*expseanx4)

mod1d<-c("mod1",expanx2,lcianx2,ucianx2,expanx3,lcianx3,ucianx3,expanx4,lcianx4,ucianx4)  
mod1d<-t(mod1d)
mod1d<-as.data.frame(mod1d)

names(mod1d)<-c("mod","expanx2","lcianx2","ucianx2","expanx3","lcianx3","ucianx3","expanx4","lcianx4","ucianx4")                      

all<-rbind(mod0d,mod1d)

# Model 2
set.seed(6683)
model2<-with(datListImp2,multinom(relevel(class_f,"c1") ~ medanx  + age_estimateyears 
                                  + income + work + smoker))


(mod2<-testEstimates(model2))

names(mod2)

mod_2<-as.data.frame(mod2[[2]])
mod_2$mod<-"mod2"
mod_2<-add_rownames(mod_2,"var")
mod_2$var<-c("int2","int3","int4","anx2","anx3","anx4")

(coefint2<-mod_2$Estimate[1])
(seint2<-mod_2$Std.Error[1])
(coefint3<-mod_2$Estimate[2])
(seint3<-mod_2$Std.Error[2])
(coefint4<-mod_2$Estimate[3])
(seint4<-mod_2$Std.Error[3])

(coefanx2<-mod_2$Estimate[4])
(seanx2<-mod_2$Std.Error[4])
(coefanx3<-mod_2$Estimate[5])
(seanx3<-mod_2$Std.Error[5])
(coefanx4<-mod_2$Estimate[6])
(seanx4<-mod_2$Std.Error[6])

(expint2<-exp(coefint2))
(expseint2<-exp(seint2))
(expint3<-exp(coefint3))
(expseint3<-exp(seint3))
(expint4<-exp(coefint4))
(expseint4<-exp(seint4))

(expanx2<-exp(coefanx2))
(expseanx2<-exp(seanx2))
(expanx3<-exp(coefanx3))
(expseanx3<-exp(seanx3))
(expanx4<-exp(coefanx4))
(expseanx4<-exp(seanx4))

(lciint2<-expint2-1.95*expseint2)
(uciint2<-expint2+1.95*expseint2)
(lciint3<-expint3-1.95*expseint3)
(uciint3<-expint3+1.95*expseint3)
(lciint4<-expint4-1.95*expseint4)
(uciint4<-expint4+1.95*expseint4)

(lcianx2<-expanx2-1.95*expseanx2)
(ucianx2<-expanx2+1.95*expseanx2)
(lcianx3<-expanx3-1.95*expseanx3)
(ucianx3<-expanx3+1.95*expseanx3)
(lcianx4<-expanx4-1.95*expseanx4)
(ucianx4<-expanx4+1.95*expseanx4)

mod2d<-c("mod2",expanx2,lcianx2,ucianx2,expanx3,lcianx3,ucianx3,expanx4,lcianx4,ucianx4)  
mod2d<-t(mod2d)
mod2d<-as.data.frame(mod2d)

names(mod2d)<-c("mod","expanx2","lcianx2","ucianx2","expanx3","lcianx3","ucianx3","expanx4","lcianx4","ucianx4")    
all<-rbind(mod0d,mod1d,mod2d)

# Model 3

model3<-with(datListImp2,multinom(relevel(class_f,"c1") ~ medanx * relevel(sexm, "male") + age_estimateyears 
                                  + income + work + smoker + diabetes + medcat))


(mod3<-testEstimates(model3))

names(mod3)

mod_3<-as.data.frame(mod3[[2]])
mod_3$mod<-"mod3"
mod_3<-add_rownames(mod_3,"var")
mod_3$var<-c("int2","int3","int4","anx2","anx3","anx4")

(coefint2<-mod_3$Estimate[1])
(seint2<-mod_3$Std.Error[1])
(coefint3<-mod_3$Estimate[2])
(seint3<-mod_3$Std.Error[2])
(coefint4<-mod_3$Estimate[3])
(seint4<-mod_3$Std.Error[3])

(coefanx2<-mod_3$Estimate[4])
(seanx2<-mod_3$Std.Error[4])
(coefanx3<-mod_3$Estimate[5])
(seanx3<-mod_3$Std.Error[5])
(coefanx4<-mod_3$Estimate[6])
(seanx4<-mod_3$Std.Error[6])

(expint2<-exp(coefint2))
(expseint2<-exp(seint2))
(expint3<-exp(coefint3))
(expseint3<-exp(seint3))
(expint4<-exp(coefint4))
(expseint4<-exp(seint4))

(expanx2<-exp(coefanx2))
(expseanx2<-exp(seanx2))
(expanx3<-exp(coefanx3))
(expseanx3<-exp(seanx3))
(expanx4<-exp(coefanx4))
(expseanx4<-exp(seanx4))

(lciint2<-expint2-1.95*expseint2)
(uciint2<-expint2+1.95*expseint2)
(lciint3<-expint3-1.95*expseint3)
(uciint3<-expint3+1.95*expseint3)
(lciint4<-expint4-1.95*expseint4)
(uciint4<-expint4+1.95*expseint4)

(lcianx2<-expanx2-1.95*expseanx2)
(ucianx2<-expanx2+1.95*expseanx2)
(lcianx3<-expanx3-1.95*expseanx3)
(ucianx3<-expanx3+1.95*expseanx3)
(lcianx4<-expanx4-1.95*expseanx4)
(ucianx4<-expanx4+1.95*expseanx4)

mod3d<-c("mod3",expanx2,lcianx2,ucianx2,expanx3,lcianx3,ucianx3,expanx4,lcianx4,ucianx4)  
mod3d<-t(mod3d)
mod3d<-as.data.frame(mod3d)

names(mod3d)<-c("mod","expanx2","lcianx2","ucianx2","expanx3","lcianx3","ucianx3","expanx4","lcianx4","ucianx4")    
all<-rbind(mod0d,mod1d,mod2d, mod3d)

write.csv(all,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/allpooled_models_table5.csv",row.names = FALSE)


# For men

newlongcmen<-newlongc%>%
  filter(sexm=="male")

#I transform my data in a list
datListImp <- list()
for(i in 1:40){
  datListImp[[i]] <- newlongc[newlongcmen$.imp== i, ]
}
head(datListImp)

class(datListImp)
length(datListImp)


#convert to a mitml object
datListImp2<-as.mitml.list(datListImp)
# imp<-as.mids(newlongc)

# Model 0

model0<-with(datListImp2,multinom(relevel(class_f,"c1") ~ medanx))

(mod0<-testEstimates(model0))

names(mod0)

mod_0<-as.data.frame(mod0[[2]])
mod_0$mod<-"mod0"
mod_0<-add_rownames(mod_0,"var")
mod_0$var<-c("int2","int3","int4","anx2","anx3","anx4")

(coefint2<-mod_0$Estimate[1])
(seint2<-mod_0$Std.Error[1])
(coefint3<-mod_0$Estimate[2])
(seint3<-mod_0$Std.Error[2])
(coefint4<-mod_0$Estimate[3])
(seint4<-mod_0$Std.Error[3])

(coefanx2<-mod_0$Estimate[4])
(seanx2<-mod_0$Std.Error[4])
(coefanx3<-mod_0$Estimate[5])
(seanx3<-mod_0$Std.Error[5])
(coefanx4<-mod_0$Estimate[6])
(seanx4<-mod_0$Std.Error[6])

(expint2<-exp(coefint2))
(expseint2<-exp(seint2))
(expint3<-exp(coefint3))
(expseint3<-exp(seint3))
(expint4<-exp(coefint4))
(expseint4<-exp(seint4))

(expanx2<-exp(coefanx2))
(expseanx2<-exp(seanx2))
(expanx3<-exp(coefanx3))
(expseanx3<-exp(seanx3))
(expanx4<-exp(coefanx4))
(expseanx4<-exp(seanx4))

(lciint2<-expint2-1.95*expseint2)
(uciint2<-expint2+1.95*expseint2)
(lciint3<-expint3-1.95*expseint3)
(uciint3<-expint3+1.95*expseint3)
(lciint4<-expint4-1.95*expseint4)
(uciint4<-expint4+1.95*expseint4)

(lcianx2<-expanx2-1.95*expseanx2)
(ucianx2<-expanx2+1.95*expseanx2)
(lcianx3<-expanx3-1.95*expseanx3)
(ucianx3<-expanx3+1.95*expseanx3)
(lcianx4<-expanx4-1.95*expseanx4)
(ucianx4<-expanx4+1.95*expseanx4)

mod0d<-c("mod0",expanx2,lcianx2,ucianx2,expanx3,lcianx3,ucianx3,expanx4,lcianx4,ucianx4)  
mod0d<-t(mod0d)
mod0d<-as.data.frame(mod0d)

names(mod0d)<-c("mod","expanx2","lcianx2","ucianx2","expanx3","lcianx3","ucianx3","expanx4","lcianx4","ucianx4")                      



# Model 1

model1<-with(datListImp2,multinom(relevel(class_f,"c1") ~ medanx * relevel(sexm, "male") + age_estimateyears))
summary(model1)

(mod1<-testEstimates(model1))

names(mod1)

mod_1<-as.data.frame(mod1[[2]])
mod_1$mod<-"mod1"
mod_1<-add_rownames(mod_1,"var")
mod_1$var<-c("int2","int3","int4","anx2","anx3","anx4")

(coefint2<-mod_1$Estimate[1])
(seint2<-mod_1$Std.Error[1])
(coefint3<-mod_1$Estimate[2])
(seint3<-mod_1$Std.Error[2])
(coefint4<-mod_1$Estimate[3])
(seint4<-mod_1$Std.Error[3])

(coefanx2<-mod_1$Estimate[4])
(seanx2<-mod_1$Std.Error[4])
(coefanx3<-mod_1$Estimate[5])
(seanx3<-mod_1$Std.Error[5])
(coefanx4<-mod_1$Estimate[6])
(seanx4<-mod_1$Std.Error[6])

(expint2<-exp(coefint2))
(expseint2<-exp(seint2))
(expint3<-exp(coefint3))
(expseint3<-exp(seint3))
(expint4<-exp(coefint4))
(expseint4<-exp(seint4))

(expanx2<-exp(coefanx2))
(expseanx2<-exp(seanx2))
(expanx3<-exp(coefanx3))
(expseanx3<-exp(seanx3))
(expanx4<-exp(coefanx4))
(expseanx4<-exp(seanx4))

(lciint2<-expint2-1.95*expseint2)
(uciint2<-expint2+1.95*expseint2)
(lciint3<-expint3-1.95*expseint3)
(uciint3<-expint3+1.95*expseint3)
(lciint4<-expint4-1.95*expseint4)
(uciint4<-expint4+1.95*expseint4)

(lcianx2<-expanx2-1.95*expseanx2)
(ucianx2<-expanx2+1.95*expseanx2)
(lcianx3<-expanx3-1.95*expseanx3)
(ucianx3<-expanx3+1.95*expseanx3)
(lcianx4<-expanx4-1.95*expseanx4)
(ucianx4<-expanx4+1.95*expseanx4)

mod1d<-c("mod1",expanx2,lcianx2,ucianx2,expanx3,lcianx3,ucianx3,expanx4,lcianx4,ucianx4)  
mod1d<-t(mod1d)
mod1d<-as.data.frame(mod1d)

names(mod1d)<-c("mod","expanx2","lcianx2","ucianx2","expanx3","lcianx3","ucianx3","expanx4","lcianx4","ucianx4")                      

all<-rbind(mod0d,mod1d)

# Model 2

model2<-with(datListImp2,multinom(relevel(class_f,"c1") ~ medanx * relevel(sexm, "male") + age_estimateyears 
                                  + income + work + smoker))


(mod2<-testEstimates(model2))

names(mod2)

mod_2<-as.data.frame(mod2[[2]])
mod_2$mod<-"mod2"
mod_2<-add_rownames(mod_2,"var")
mod_2$var<-c("int2","int3","int4","anx2","anx3","anx4")

(coefint2<-mod_2$Estimate[1])
(seint2<-mod_2$Std.Error[1])
(coefint3<-mod_2$Estimate[2])
(seint3<-mod_2$Std.Error[2])
(coefint4<-mod_2$Estimate[3])
(seint4<-mod_2$Std.Error[3])

(coefanx2<-mod_2$Estimate[4])
(seanx2<-mod_2$Std.Error[4])
(coefanx3<-mod_2$Estimate[5])
(seanx3<-mod_2$Std.Error[5])
(coefanx4<-mod_2$Estimate[6])
(seanx4<-mod_2$Std.Error[6])

(expint2<-exp(coefint2))
(expseint2<-exp(seint2))
(expint3<-exp(coefint3))
(expseint3<-exp(seint3))
(expint4<-exp(coefint4))
(expseint4<-exp(seint4))

(expanx2<-exp(coefanx2))
(expseanx2<-exp(seanx2))
(expanx3<-exp(coefanx3))
(expseanx3<-exp(seanx3))
(expanx4<-exp(coefanx4))
(expseanx4<-exp(seanx4))

(lciint2<-expint2-1.95*expseint2)
(uciint2<-expint2+1.95*expseint2)
(lciint3<-expint3-1.95*expseint3)
(uciint3<-expint3+1.95*expseint3)
(lciint4<-expint4-1.95*expseint4)
(uciint4<-expint4+1.95*expseint4)

(lcianx2<-expanx2-1.95*expseanx2)
(ucianx2<-expanx2+1.95*expseanx2)
(lcianx3<-expanx3-1.95*expseanx3)
(ucianx3<-expanx3+1.95*expseanx3)
(lcianx4<-expanx4-1.95*expseanx4)
(ucianx4<-expanx4+1.95*expseanx4)

mod2d<-c("mod2",expanx2,lcianx2,ucianx2,expanx3,lcianx3,ucianx3,expanx4,lcianx4,ucianx4)  
mod2d<-t(mod2d)
mod2d<-as.data.frame(mod2d)

names(mod2d)<-c("mod","expanx2","lcianx2","ucianx2","expanx3","lcianx3","ucianx3","expanx4","lcianx4","ucianx4")    
all<-rbind(mod0d,mod1d,mod2d)

# Model 3

model3<-with(datListImp2,multinom(relevel(class_f,"c1") ~ medanx * relevel(sexm, "male") + age_estimateyears 
                                  + income + work + smoker + diabetes + medcat))


(mod3<-testEstimates(model3))

names(mod3)

mod_3<-as.data.frame(mod3[[2]])
mod_3$mod<-"mod3"
mod_3<-add_rownames(mod_3,"var")
mod_3$var<-c("int2","int3","int4","anx2","anx3","anx4")

(coefint2<-mod_3$Estimate[1])
(seint2<-mod_3$Std.Error[1])
(coefint3<-mod_3$Estimate[2])
(seint3<-mod_3$Std.Error[2])
(coefint4<-mod_3$Estimate[3])
(seint4<-mod_3$Std.Error[3])

(coefanx2<-mod_3$Estimate[4])
(seanx2<-mod_3$Std.Error[4])
(coefanx3<-mod_3$Estimate[5])
(seanx3<-mod_3$Std.Error[5])
(coefanx4<-mod_3$Estimate[6])
(seanx4<-mod_3$Std.Error[6])

(expint2<-exp(coefint2))
(expseint2<-exp(seint2))
(expint3<-exp(coefint3))
(expseint3<-exp(seint3))
(expint4<-exp(coefint4))
(expseint4<-exp(seint4))

(expanx2<-exp(coefanx2))
(expseanx2<-exp(seanx2))
(expanx3<-exp(coefanx3))
(expseanx3<-exp(seanx3))
(expanx4<-exp(coefanx4))
(expseanx4<-exp(seanx4))

(lciint2<-expint2-1.95*expseint2)
(uciint2<-expint2+1.95*expseint2)
(lciint3<-expint3-1.95*expseint3)
(uciint3<-expint3+1.95*expseint3)
(lciint4<-expint4-1.95*expseint4)
(uciint4<-expint4+1.95*expseint4)

(lcianx2<-expanx2-1.95*expseanx2)
(ucianx2<-expanx2+1.95*expseanx2)
(lcianx3<-expanx3-1.95*expseanx3)
(ucianx3<-expanx3+1.95*expseanx3)
(lcianx4<-expanx4-1.95*expseanx4)
(ucianx4<-expanx4+1.95*expseanx4)

mod3d<-c("mod3",expanx2,lcianx2,ucianx2,expanx3,lcianx3,ucianx3,expanx4,lcianx4,ucianx4)  
mod3d<-t(mod3d)
mod3d<-as.data.frame(mod3d)

names(mod3d)<-c("mod","expanx2","lcianx2","ucianx2","expanx3","lcianx3","ucianx3","expanx4","lcianx4","ucianx4")    
all<-rbind(mod0d,mod1d,mod2d, mod3d)

write.csv(all,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/menpooled_models_table5.csv",row.names = FALSE)



# For women

newlongcwomen<-newlongc%>%
  filter(sexm=="female")

#I transform my data in a list
datListImp <- list()
for(i in 1:40){
  datListImp[[i]] <- newlongc[newlongcwomen$.imp== i, ]
}
head(datListImp)

class(datListImp)
length(datListImp)


#convert to a mitml object
datListImp2<-as.mitml.list(datListImp)
# imp<-as.mids(newlongc)

# Model 0

model0<-with(datListImp2,multinom(relevel(class_f,"c1") ~ medanx))

(mod0<-testEstimates(model0))

names(mod0)

mod_0<-as.data.frame(mod0[[2]])
mod_0$mod<-"mod0"
mod_0<-add_rownames(mod_0,"var")
mod_0$var<-c("int2","int3","int4","anx2","anx3","anx4")

(coefint2<-mod_0$Estimate[1])
(seint2<-mod_0$Std.Error[1])
(coefint3<-mod_0$Estimate[2])
(seint3<-mod_0$Std.Error[2])
(coefint4<-mod_0$Estimate[3])
(seint4<-mod_0$Std.Error[3])

(coefanx2<-mod_0$Estimate[4])
(seanx2<-mod_0$Std.Error[4])
(coefanx3<-mod_0$Estimate[5])
(seanx3<-mod_0$Std.Error[5])
(coefanx4<-mod_0$Estimate[6])
(seanx4<-mod_0$Std.Error[6])

(expint2<-exp(coefint2))
(expseint2<-exp(seint2))
(expint3<-exp(coefint3))
(expseint3<-exp(seint3))
(expint4<-exp(coefint4))
(expseint4<-exp(seint4))

(expanx2<-exp(coefanx2))
(expseanx2<-exp(seanx2))
(expanx3<-exp(coefanx3))
(expseanx3<-exp(seanx3))
(expanx4<-exp(coefanx4))
(expseanx4<-exp(seanx4))

(lciint2<-expint2-1.95*expseint2)
(uciint2<-expint2+1.95*expseint2)
(lciint3<-expint3-1.95*expseint3)
(uciint3<-expint3+1.95*expseint3)
(lciint4<-expint4-1.95*expseint4)
(uciint4<-expint4+1.95*expseint4)

(lcianx2<-expanx2-1.95*expseanx2)
(ucianx2<-expanx2+1.95*expseanx2)
(lcianx3<-expanx3-1.95*expseanx3)
(ucianx3<-expanx3+1.95*expseanx3)
(lcianx4<-expanx4-1.95*expseanx4)
(ucianx4<-expanx4+1.95*expseanx4)

mod0d<-c("mod0",expanx2,lcianx2,ucianx2,expanx3,lcianx3,ucianx3,expanx4,lcianx4,ucianx4)  
mod0d<-t(mod0d)
mod0d<-as.data.frame(mod0d)

names(mod0d)<-c("mod","expanx2","lcianx2","ucianx2","expanx3","lcianx3","ucianx3","expanx4","lcianx4","ucianx4")                      



# Model 1

model1<-with(datListImp2,multinom(relevel(class_f,"c1") ~ medanx * relevel(sexm, "male") + age_estimateyears))
summary(model1)

(mod1<-testEstimates(model1))

names(mod1)

mod_1<-as.data.frame(mod1[[2]])
mod_1$mod<-"mod1"
mod_1<-add_rownames(mod_1,"var")
mod_1$var<-c("int2","int3","int4","anx2","anx3","anx4")

(coefint2<-mod_1$Estimate[1])
(seint2<-mod_1$Std.Error[1])
(coefint3<-mod_1$Estimate[2])
(seint3<-mod_1$Std.Error[2])
(coefint4<-mod_1$Estimate[3])
(seint4<-mod_1$Std.Error[3])

(coefanx2<-mod_1$Estimate[4])
(seanx2<-mod_1$Std.Error[4])
(coefanx3<-mod_1$Estimate[5])
(seanx3<-mod_1$Std.Error[5])
(coefanx4<-mod_1$Estimate[6])
(seanx4<-mod_1$Std.Error[6])

(expint2<-exp(coefint2))
(expseint2<-exp(seint2))
(expint3<-exp(coefint3))
(expseint3<-exp(seint3))
(expint4<-exp(coefint4))
(expseint4<-exp(seint4))

(expanx2<-exp(coefanx2))
(expseanx2<-exp(seanx2))
(expanx3<-exp(coefanx3))
(expseanx3<-exp(seanx3))
(expanx4<-exp(coefanx4))
(expseanx4<-exp(seanx4))

(lciint2<-expint2-1.95*expseint2)
(uciint2<-expint2+1.95*expseint2)
(lciint3<-expint3-1.95*expseint3)
(uciint3<-expint3+1.95*expseint3)
(lciint4<-expint4-1.95*expseint4)
(uciint4<-expint4+1.95*expseint4)

(lcianx2<-expanx2-1.95*expseanx2)
(ucianx2<-expanx2+1.95*expseanx2)
(lcianx3<-expanx3-1.95*expseanx3)
(ucianx3<-expanx3+1.95*expseanx3)
(lcianx4<-expanx4-1.95*expseanx4)
(ucianx4<-expanx4+1.95*expseanx4)

mod1d<-c("mod1",expanx2,lcianx2,ucianx2,expanx3,lcianx3,ucianx3,expanx4,lcianx4,ucianx4)  
mod1d<-t(mod1d)
mod1d<-as.data.frame(mod1d)

names(mod1d)<-c("mod","expanx2","lcianx2","ucianx2","expanx3","lcianx3","ucianx3","expanx4","lcianx4","ucianx4")                      

all<-rbind(mod0d,mod1d)

# Model 2

model2<-with(datListImp2,multinom(relevel(class_f,"c1") ~ medanx * relevel(sexm, "male") + age_estimateyears 
                                  + income + work + smoker + medcat ++ underweight + weight_loss))


(mod2<-testEstimates(model2))

names(mod2)

mod_2<-as.data.frame(mod2[[2]])
mod_2$mod<-"mod2"
mod_2<-add_rownames(mod_2,"var")
mod_2$var<-c("int2","int3","int4","anx2","anx3","anx4")

(coefint2<-mod_2$Estimate[1])
(seint2<-mod_2$Std.Error[1])
(coefint3<-mod_2$Estimate[2])
(seint3<-mod_2$Std.Error[2])
(coefint4<-mod_2$Estimate[3])
(seint4<-mod_2$Std.Error[3])

(coefanx2<-mod_2$Estimate[4])
(seanx2<-mod_2$Std.Error[4])
(coefanx3<-mod_2$Estimate[5])
(seanx3<-mod_2$Std.Error[5])
(coefanx4<-mod_2$Estimate[6])
(seanx4<-mod_2$Std.Error[6])

(expint2<-exp(coefint2))
(expseint2<-exp(seint2))
(expint3<-exp(coefint3))
(expseint3<-exp(seint3))
(expint4<-exp(coefint4))
(expseint4<-exp(seint4))

(expanx2<-exp(coefanx2))
(expseanx2<-exp(seanx2))
(expanx3<-exp(coefanx3))
(expseanx3<-exp(seanx3))
(expanx4<-exp(coefanx4))
(expseanx4<-exp(seanx4))

(lciint2<-expint2-1.95*expseint2)
(uciint2<-expint2+1.95*expseint2)
(lciint3<-expint3-1.95*expseint3)
(uciint3<-expint3+1.95*expseint3)
(lciint4<-expint4-1.95*expseint4)
(uciint4<-expint4+1.95*expseint4)

(lcianx2<-expanx2-1.95*expseanx2)
(ucianx2<-expanx2+1.95*expseanx2)
(lcianx3<-expanx3-1.95*expseanx3)
(ucianx3<-expanx3+1.95*expseanx3)
(lcianx4<-expanx4-1.95*expseanx4)
(ucianx4<-expanx4+1.95*expseanx4)

mod2d<-c("mod2",expanx2,lcianx2,ucianx2,expanx3,lcianx3,ucianx3,expanx4,lcianx4,ucianx4)  
mod2d<-t(mod2d)
mod2d<-as.data.frame(mod2d)

names(mod2d)<-c("mod","expanx2","lcianx2","ucianx2","expanx3","lcianx3","ucianx3","expanx4","lcianx4","ucianx4")    
all<-rbind(mod0d,mod1d,mod2d)

# Model 3

# model3<-with(datListImp2,multinom(relevel(class_f,"c1") ~ medanx * relevel(sexm, "male") + age_estimateyears 
#                                    + income + work + smoker + medcat + underweight + weight_loss))
# 
# 
# (mod3<-testEstimates(model3))
# 
# names(mod3)
# 
# mod_3<-as.data.frame(mod3[[2]])
# mod_3$mod<-"mod3"
# mod_3<-add_rownames(mod_3,"var")
# mod_3$var<-c("int2","int3","int4","anx2","anx3","anx4")
# 
# (coefint2<-mod_3$Estimate[1])
# (seint2<-mod_3$Std.Error[1])
# (coefint3<-mod_3$Estimate[2])
# (seint3<-mod_3$Std.Error[2])
# (coefint4<-mod_3$Estimate[3])
# (seint4<-mod_3$Std.Error[3])
# 
# (coefanx2<-mod_3$Estimate[4])
# (seanx2<-mod_3$Std.Error[4])
# (coefanx3<-mod_3$Estimate[5])
# (seanx3<-mod_3$Std.Error[5])
# (coefanx4<-mod_3$Estimate[6])
# (seanx4<-mod_3$Std.Error[6])
# 
# (expint2<-exp(coefint2))
# (expseint2<-exp(seint2))
# (expint3<-exp(coefint3))
# (expseint3<-exp(seint3))
# (expint4<-exp(coefint4))
# (expseint4<-exp(seint4))
# 
# (expanx2<-exp(coefanx2))
# (expseanx2<-exp(seanx2))
# (expanx3<-exp(coefanx3))
# (expseanx3<-exp(seanx3))
# (expanx4<-exp(coefanx4))
# (expseanx4<-exp(seanx4))
# 
# (lciint2<-expint2-1.95*expseint2)
# (uciint2<-expint2+1.95*expseint2)
# (lciint3<-expint3-1.95*expseint3)
# (uciint3<-expint3+1.95*expseint3)
# (lciint4<-expint4-1.95*expseint4)
# (uciint4<-expint4+1.95*expseint4)
# 
# (lcianx2<-expanx2-1.95*expseanx2)
# (ucianx2<-expanx2+1.95*expseanx2)
# (lcianx3<-expanx3-1.95*expseanx3)
# (ucianx3<-expanx3+1.95*expseanx3)
# (lcianx4<-expanx4-1.95*expseanx4)
# (ucianx4<-expanx4+1.95*expseanx4)
# 
# mod3d<-c("mod3",expanx2,lcianx2,ucianx2,expanx3,lcianx3,ucianx3,expanx4,lcianx4,ucianx4)
# mod3d<-t(mod3d)
# mod3d<-as.data.frame(mod3d)
# 
# names(mod3d)<-c("mod","expanx2","lcianx2","ucianx2","expanx3","lcianx3","ucianx3","expanx4","lcianx4","ucianx4")
all<-rbind(mod0d,mod1d,mod2d)

write.csv(all,"P:/Documents/COVID19/PREDICOVID/symptom_score/generated_data/pooled_models_table5_male.csv",row.names = FALSE)



