rm(list=ls(all=TRUE))

install.packages("mlbench")
library("mlbench")

data(PimaIndiansDiabetes2)
data<-PimaIndiansDiabetes2
sum(is.na(data))

new.data<-na.omit(data)
sum(is.na(new.data))


#############################################################
######### ASKISI 1###########################################
#############################################################


### omadopoihsh metablhtwn age kai pregnant


colnames(new.data)
str(new.data)


new.data$age<-cut(new.data$age, breaks=c(20,31,41,50,100),labels=c("20-30","31-40","41-50","50+"))

levels(new.data$age)


new.data$pregnant<-cut(new.data$pregnant, breaks=c(-1,5,10,17),labels=c("0-5","6-10","10+"))

levels(new.data$pregnant)

str(new.data)


### logistic sto diabets



 mylogit <- glm(formula = diabetes~glucose+ pressure+ triceps+insulin+mass+pedigree+age+pregnant   , 
                family = "binomial", data = new.data)
mylogit 
 summary(mylogit)


 with(mylogit, pchisq(null.deviance - deviance,
df.null - df.residual, lower.tail = FALSE))


#############################################################
######### ASKISI 2###########################################
#############################################################


n <- dim(new.data)[1]
model_bic <- step(mylogit, trace=TRUE, direction = 'both', k = log(n))





 mylogit1 <- glm(formula =diabetes ~ glucose + mass + pedigree + age, 
                family = "binomial", data = new.data)
summary(mylogit1)





#############################################################
######### ASKISI 3###########################################
#############################################################




as.numeric(predict(mylogit1 ,
newdata=data.frame(age=factor(factor('31-40',levels=c('20-30','31-40','41-50','50+'))),
glucose =mean(new.data$glucose ),
pressure=mean(new.data$pressure),
triceps =mean(new.data$triceps ),
insulin =mean(new.data$insulin ),
mass=mean(new.data$mass),
pedigree =mean(new.data$pedigree )),type="response"))

exp(0.3591437)

as.numeric(predict(mylogit1 ,
newdata=data.frame(age=factor(factor('41-50',levels=c('20-30','31-40','41-50','50+'))),
glucose =mean(new.data$glucose ),
pressure=mean(new.data$pressure),
triceps =mean(new.data$triceps ),
insulin =mean(new.data$insulin ),
mass=mean(new.data$mass),
pedigree =mean(new.data$pedigree )),type="response"))


exp(0.3591437)/exp(0.544088)

#############################################################
######### ASKISI 4###########################################
#############################################################



n <- nrow(new.data)
train <- sample(n, n/2)


glm.fit <- glm(formula =diabetes ~ glucose + mass + pedigree + age, 
                 data = new.data,subset = train, family = binomial)

glm.probs <- predict(glm.fit, new.data[-train, ], type = "response")
glm.pred <- rep("neg", n/2)
glm.pred[glm.probs>0.5] <- "pos"

table(glm.pred)
confTab <- table(glm.pred, diabetes [-train])
confTab 


# Sensitivity
((confTab[2,2])/(confTab[1,2]+confTab[2,2]))*100

#Specificity
(1-(confTab[2,1])/(confTab[1,1]+confTab[2,1]))*100







############################################################################################
######### ASKISI 5##########################################################################
############################################################################################



newdata1<- with(new.data,
  data.frame(pregnant=factor("0-5",levels=c( "0-5" ,"6-10", "10+")),glucose=95,pressure=50,
triceps=21,insulin=70,mass=30,pedigree=0.5,
age=factor('41-50',levels=c('20-30','31-40','41-50','50+'))))


data1<- cbind(newdata1,predict(mylogit1, newdata = newdata1, type="link", se=TRUE))

newdata4<- within(data1 ,{
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
(newdata4)

##########################################################################################
##########################################################################################


newdata2<- with(new.data,
  data.frame(pregnant=factor("0-5",levels=c( "0-5" ,"6-10", "10+")),glucose=  80,pressure=62,
triceps=30,insulin=100,mass=35,pedigree=0.7,
age=factor('31-40',levels=c('20-30','31-40','41-50','50+'))))



data2<- cbind(newdata2,predict(mylogit1, newdata = newdata2, type="link", se=TRUE))

newdata5<- within(data2,{
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
(newdata5)

##########################################################################################
##########################################################################################


newdata3<- with(new.data,
  data.frame(pregnant=factor("0-5",levels=c( "0-5" ,"6-10", "10+")),glucose= 55,pressure=70,
triceps=17,insulin= 110,mass=33,pedigree=0.9,
age=factor('31-40',levels=c('20-30','31-40','41-50','50+'))))



data3<- cbind(newdata3,predict(mylogit1, newdata = newdata3, type="link", se=TRUE))

newdata6<- within(data3,{
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
(newdata6)















