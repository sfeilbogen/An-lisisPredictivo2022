base=read.csv("adult.data")

View(base)
#librerias
library(dplyr)
library(tidyverse)
#DESCRIPCION INICIAL DE LA BASE
colnames(base)

#descripcion de las variables
#cantidad de registros:
dim(base)[1]
#cantidad de variables:
dim(base)[2]

str(base)
glimpse(base)
head(base)
#cambiamos nombres de columnas
names(base)[1] = "Ages"
names(base)[2] = "WorkClass"
names(base)[3] = "TargetPopulation"
names(base)[5] = "BachelorYear"
names(base)[6] = "MaritalStatus"
names(base)[7] = "Occupation"
names(base)[8]="Family"
names(base)[9]="Ethnicity"
names(base)[10]="Gender"
names(base)[11]="CapitalGain"
names(base)[12]="CapitalLoss"
names(base)[13]="HoursPerWeek"
names(base)[14]="Country"
names(base)[15] ="Result"

colnames(base)
#VARIABLES CATEGORICAS distribucion, frecuencias

numericas=base %>% select_if(function(x)is.numeric(x))
categoricas=base %>% select_if(function(x) !is.numeric(x))

tabs = categoricas %>% map(function(x) table(x, useNA="ifany"))

tabs$WorkClass
ggplot(categoricas,aes(x=reorder(WorkClass,WorkClass,function(x) length(x))))+ geom_bar (fill = "#1465bb")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

tabs$Bachelors
ggplot(categoricas,aes(x=reorder(Bachelors,Bachelors,function(x) length(x))),size=7,alpha=0.7)+ geom_bar (fill = "#1465bb")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

tabs$MaritalStatus
ggplot(categoricas,aes(x=reorder(MaritalStatus,MaritalStatus,function(x) length(x))))+ geom_bar (fill = "#1465bb")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

tabs$Occupation
ggplot(categoricas,aes(x=reorder(Occupation,Occupation,function(x) length(x))))+ geom_bar (fill = "#1465bb")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

tabs$Family
ggplot(categoricas,aes(x=reorder(Family,Family,function(x) length(x))),size=7,alpha=0.7)+ geom_bar (fill = "#1465bb")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

tabs$Ethnicity
ggplot(categoricas,aes(x=reorder(Ethnicity,Ethnicity,function(x) length(x))),size=7,alpha=0.7)+ geom_bar (fill = "#1465bb")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

tabs$Gender
ggplot(categoricas,aes(x=reorder(Gender,Gender,function(x) length(x))),size=7,alpha=0.7)+ geom_bar (fill = "#1465bb")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

tabs$Country
ggplot(categoricas,aes(x=reorder(Country,Country,function(x) length(x))),size=7,alpha=0.7)+ geom_bar (fill = "#1465bb")+theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))

tabs$Result
ggplot(categoricas,aes(x=reorder(Result,Result,function(x) length(x))),size=7,alpha=0.7)+ geom_bar (fill = "#1465bb")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



#VARIABLES NUMERICAS
library(stats)
quantile(numericas$CapitalLoss,seq(0,1,0.1))


list_cuantiles = numericas %>% map(function(x) quantile(x, seq(0, 1, 0.25), na.rm=T) )
list_cuantiles


list_cuantiles %>% 
  as.data.frame() %>% 
  select(BachelorYear)

#relacion entre variables numericas
numericassintarget=select(numericas,-TargetPopulation)
GGally::ggcorr(
  numericassintarget, method=c("pairwise","spearman"),  
  label=T, hjust=1, label_size=2, layout.exp=10, size=3)



cor_matrix = cor(numericassintarget, method="spearman", use="pairwise")
cor_matrix[upper.tri(cor_matrix, diag=T)] = NA
df_cor = cor_matrix %>% as.table() %>% as.data.frame()
df_cor %>% 
  rename(corr = Freq) %>% 
  filter(!is.na(corr) & Var1 != Var2) %>% 
  arrange(-abs(corr)) %>% 
  head(10) %>% 
  knitr::kable() %>%
  kableExtra::kable_styling()



table(categoricas$Result,categoricas$WorkClass)
table(categoricas$Result,categoricas$Bachelors)
table(categoricas$Result,categoricas$Occupation)

#visualizamos
library(ggplot2)

ggplot(base, aes(x=HoursPerWeek, y=WorkClass, fill =..x..)) +
  ggridges::geom_density_ridges_gradient(scale=1) +
  scale_fill_viridis_c() +
  NULL

ggplot(base, aes(x=HoursPerWeek, fill=Result)) +
  geom_density(alpha=0.5, adjust=2) +
  NULL

ggplot(base, aes(x=BachelorYear, fill=Result)) +
  geom_density(alpha=0.5, adjust=2) +
  NULL



##########

#Limpiar la base, revisar NA, outliers, etc. 
library(purrr)
#Nas por columna
map_dbl(base ,function(x) mean(is.na(x))*100) %>% sort(decreasing=T) %>% as.data.frame()

#NAs por fila
na_by_row=base %>% apply(1,function(x) mean(is.na(x))*100)
names(na_by_row)=base$State.gov
na_by_row %>% sort(decreasing=T) %>% head(10) 


library(skimr)
skim(base)

skim_eda = partition(skim(base))
names(skim_eda)

skim_eda$numeric %>% 
  select(skim_variable, n_missing) %>% 
  filter(n_missing > 0)

skim_eda$character %>% 
  select(skim_variable, n_missing) %>% 
  filter(n_missing > 0)



library(Amelia) #gráfico missmap
library(questionr) # para usar freq.na

freq.na(base)
missmap(base, main = "Valores Missing vs. Observados")
md.pattern(base,rotate.names = TRUE)

#OUTLIERS

nueva=base

nueva$Ages %>%  boxplot(main="Box Plot of ages", ylab="Carat", col = "grey")
nueva1=nueva %>% filter(Ages<=75)
nueva1$Ages %>% boxplot(main="Box Plot of ages", ylab="Carat", col = "grey")

nueva1$BachelorYear %>%  boxplot(main="Box Plot of bachelor", ylab="Carat", col = "grey")
nueva2=nueva1 %>% filter(BachelorYear>=5)
nueva2$BachelorYear %>%  boxplot(main="Box Plot of bachelor", ylab="Carat", col = "grey")
options(scipen=999)

(nueva2$CapitalGain) %>%  boxplot(main="Box Plot of CapitalGain", ylab="Carat", col = "grey")
(nueva2$CapitalLoss) %>%  boxplot(main="Box Plot of CapitalLoss", ylab="Carat", col = "grey")
nueva2=nueva2 %>% filter(CapitalGain!=99999  )
unique(nueva2$CapitalLoss)


(nueva2$HoursPerWeek) %>%  boxplot(main="Box Plot of hours", ylab="Carat", col = "grey")
nueva3=nueva2 %>% filter(HoursPerWeek>=36 & HoursPerWeek<=50)
(nueva3$HoursPerWeek) %>%  boxplot(main="Box Plot of hours", ylab="Carat", col = "grey")




#Estadisticas principales 

summary(base)
str(base) 
glimpse(base)
dim(nueva3)

#Particion entrenamiento y testeo

nueva3$Result=ifelse(nueva3$Result==" <=50K",1,0)
nueva3=select(nueva3,-TargetPopulation)


#REGRESION LOGISTICA
library(caTools)
set.seed(123);split = sample.split(nueva3$Result, SplitRatio = 0.80)
training_set = subset(nueva3, split == TRUE)
testing_set = subset(nueva3, split == FALSE)
#observamos el 80% y 20%
dim(nueva3)
dim(training_set)
dim(testing_set)

classifier = glm(formula = Result~ .,
                 data = training_set, 
                 family = binomial)
summary(classifier)

testing_set[,14]
dim(testing_set)

prob_pred = predict(classifier, type = "response",
                    newdata = testing_set[,-14])
y_pred = ifelse(prob_pred> 0.5, 1, 0)

#matriz de confusion
cm = table(testing_set[, 14], y_pred)
cm


#ARBOLES
library(rpart)
library(rpart.plot)
library(caret)

set.seed(123);nueva3$Result=as.factor(nueva3$Result)
rpart=createDataPartition(y=nueva3$Result,p=0.80,list=F)
entreno=nueva3[rpart,]
testeo=nueva3[-rpart,]

arbol=rpart(Result~.,entreno,method="class")
#testeamos
rpart.plot(arbol,type=5,extra=1,cex=0.7,main="AdD")

pred=predict(arbol,testeo,type="class")
confusionMatrix(pred,testeo$Result)

#observamos el 80% y 20%
dim(nueva3)
dim(entreno)
dim(testeo)


