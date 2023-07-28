
setwd("C:\\Users\\user\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")
setwd("C:\\Users\\KNOU_stat\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")
setwd("C:\\Users\\SYP\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")

#### Program 6-1

setwd("C:/Users/KNOU_stat/R_codes")
dat0<-read.csv("biostat_ex_data.csv")
library(dplyr)
dat3<-dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y, 
                              post.CA19.9.binary, post.CA19.9.3grp),
                         as.factor) %>% 
  mutate(HTN=as.factor(ifelse(SBP>=140, 1, 0)), 
         CEA.grp=as.factor(ifelse(CEA>5, 1, 0)),
         post.CEA.grp=as.factor(ifelse(post.CEA>5, 1, 0)))
library(ggplot2)
ggplot(dat3) + geom_point(aes(age, SBP))
cor(dat3$age, dat3$SBP)
cor(dat3$age, dat3$SBP, method="spearman")

#### Program 6-2

ggplot(dat3) + geom_point(aes(log(CEA), log(post.CEA)))
cor(log(dat3$CEA), log(dat3$post.CEA))
cor(log(dat3$CEA), log(dat3$post.CEA), method="spearman")

#### Program 6-3
dat4<-dat3 %>% mutate(log.CEA=log(CEA),
                      log.post.CEA=log(post.CEA))
obj<-lm(log.post.CEA~log.CEA, data=dat4)
summary(obj)

#### Program 6-4
ggplot(dat4, aes(log.CEA, log.post.CEA)) + geom_point() +
  geom_smooth(method="lm")

#### Program 6-5
dat.new<-data.frame(log.CEA=c(1, 2, 3))
predict(obj, newdata=dat.new)

#### Program 6-6
obj2<-lm(SBP~age+weight, data=dat4)
summary(obj2)

#### Program 6-7
anova(obj)

#### Program 6-8
anova(obj2)

#### Program 6-9
library(broom)
tidy(obj2, conf.int=TRUE)

#### Program 6-10
std.res<-rstandard(obj2)
yhat<-predict(obj2)
plot(yhat, std.res)
abline(h=0)
abline(h=2, lty=2)
abline(h=-2, lty=2)

#### Program 6-11
qqnorm(std.res)
qqline(std.res)

#### Program 6-12
levels(dat4$stage)

#### Program 6-13
model.1<-lm(log.CEA~stage, data=dat4)
summary(model.1)

#### Program 6-14
dat5<-dat4 %>% mutate(stage.new=relevel(stage, ref=2))
levels(dat5$stage.new)
model.2<-lm(log.CEA~stage.new, data=dat5)
summary(model.2)

#### Program 6-15
model.3<-lm(log.CEA~age+sex+stage, data=dat4)
summary(model.3)

#### Program 6-16
model.4<-lm(log.CEA~age+sex, data=dat4)
library(lmtest)
lrtest(model.3, model.4)

#### Program 6-17
anova(model.3)





