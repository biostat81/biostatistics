setwd("C:\\Users\\user\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")
setwd("C:\\Users\\KNOU_stat\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")

#### Program 7-1
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
xtabs(~post.CA19.9.binary+Recur_1y, data=dat4)
library(epiR)
epi.tests(xtabs(~post.CA19.9.binary+Recur_1y, data=dat4)[2:1, 2:1])

#### Program 7-2
library(pROC)
fit<-roc(Recur_1y~post.CA19.9, data=dat4)
fit
plot(fit)

#### Program 7-3
coords(fit)

#### Program 7-4
coords(fit, x="best", best.method="youden")




