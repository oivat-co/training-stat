
#Case-control study concerning Congenital Heart Disease (CHD)
#The study considers a group of male individuals 
#it shows the Smoking and/or drinking coffee habits within this group
#The objective of this script is to learn about the relationship 
#in developing CHD when the after mentioned variables
#take Boolean values

#We make use of the library boot for making bootstrapping
library(boot)
#Construction of the frequency table from all the possible combinations
frec<-c(15,42,11,8,15,21,25,14)  
CHD<-c(1,0,1,0,1,0,1,0)
S<-c(0,0,1,1,0,0,1,1)
K<-c(0,0,0,0,1,1,1,1)
#Using the values of the former columns i.e CHD, Smoke and Cofee, 
#the construction of the dataframe comes next
datos<-data.frame(rep(CHD,frec),rep(S,frec),rep(K,frec))
colnames(datos)<-c("CHD","Fuma","Cafe")
#we retrieve date of the exponential coefficients using the data frame
#observe that since we are employing a logistic regression, 
exp(coef(glm(CHD~Fuma+Cafe, data = datos, family = binomial(logit))))

model_coef <- function(data, index){
  exp(coef(glm(CHD~Fuma+Cafe, data = datos, family = binomial(logit), subset = index))[[2]])
}

glm.chd<-boot(datos, model_coef, 1000)
glm.chd$t0

