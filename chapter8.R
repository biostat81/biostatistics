setwd("C:\\Users\\user\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")
setwd("C:\\Users\\KNOU_stat\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")

#### Program 8-1
setwd("C:/Users/KNOU_stat/R_codes")
dat0<-read.csv("biostat_ex_data.csv")
library(dplyr)
dat4<-dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y, 
                              post.CA19.9.binary, post.CA19.9.3grp),
                         as.factor) %>% 
  mutate(HTN=as.factor(ifelse(SBP>=140, 1, 0)), 
         CEA.grp=as.factor(ifelse(CEA>5, 1, 0)),
         post.CEA.grp=as.factor(ifelse(post.CEA>5, 1, 0)),
         log.CEA=log(CEA),
         log.post.CEA=log(post.CEA))
model.1<-glm(Recur_1y~post.CA19.9.binary, family=binomial, data=dat4)
summary(model.1)

#### Program 8-2
coef(model.1)
exp(coef(model.1))

#### Program 8-3
library(lmtest)
coefci(model.1)
exp(coefci(model.1))

#### Program 8-4
library(epiR)
epi.2by2(xtabs(~post.CA19.9.binary + Recur_1y, data=dat4)[2:1, 2:1])


#### Program 8-5
model.2<-glm(Recur_1y~age+sex+post.CA19.9, family=binomial, data=dat4)
summary(model.2)
exp(coef(model.2))
exp(coefci(model.2))


#### Program 8-6
model.3<-glm(Recur_1y~post.CA19.9.3grp+sex, family=binomial, data=dat4)
summary(model.3)
exp(coef(model.3))

#### Program 8-7
model.30<-glm(Recur_1y~sex, family=binomial, data=dat4)
lrtest(model.3, model.30)


#### Program 8-8
predict(model.2)
predict(model.2, type="response")

#### Program 8-9
predict(model.2, newdata=data.frame(age=60, sex=as.factor(0), post.CA19.9=30),
        type="response")

#### Program 8-10
library(pROC)
roc(dat4$Recur_1y~predict(model.2))




