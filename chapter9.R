setwd("C:\\Users\\user\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")
setwd("C:\\Users\\KNOU_stat\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")

#### Program 9-1
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

dat5<-dat4 %>% mutate(OP_date=as.Date(OP_date, format="%Y-%m-%d"),
                      Recur_date=as.Date(Recur_date, format="%Y-%m-%d"),
                      rfs=as.double(Recur_date-OP_date)
                      )

#### Program 9-2
library(survival)
Surv.obj<-Surv(time=dat5$rfs, event=dat5$Recur==1)

#### Program 9-3
fit<-survfit(Surv.obj~1, data=dat5)
fit
summary(fit)

#### Program 9-4
library(survminer)
ggsurvplot(fit)

#### Program 9-5
ggsurvplot(fit, xscale=365.25, break.x.by=365.25, xlab="Year", legend="none",
           risk.table = TRUE, conf.int=FALSE, title="Recurrence Free Survival")

#### Program 9-6
survdiff(Surv.obj~stage, data=dat5)

#### Program 9-7
fit<-survfit(Surv.obj~stage, data=dat5)
ggsurvplot(fit, xscale=365.25, break.x.by=365.25, xlab="Year", 
           risk.table = TRUE, conf.int=FALSE, title="Recurrence Free Survival",
           pval=T)

#### Program 9-8
m1<-coxph(Surv(time=rfs, event=Recur==1)~age+stage, data=dat5)
summary(m1)

#### Program 9-9
m0<-coxph(Surv(time=rfs, event=Recur==1)~age, data=dat5)
anova(m0, m1)

#### Program 9-10
cox.zph(m1)
ggcoxzph(cox.zph(m1))
