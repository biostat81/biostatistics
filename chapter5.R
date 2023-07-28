setwd("C:\\Users\\user\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")
setwd("C:\\Users\\KNOU_stat\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")

#### Program 5-1

setwd("C:/Users/KNOU_stat/R_codes")
dat0<-read.csv("biostat_ex_data.csv")
library(dplyr)
dat2<-dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y, 
                              post.CA19.9.binary, post.CA19.9.3grp),
                         as.factor) %>% 
               mutate(HTN=as.factor(ifelse(SBP>=140, 1, 0)))

#### Program 5-2
xtabs(~smoking+HTN, data=dat2)

#### Program 5-3
library(epiR)
epi.2by2(xtabs(~smoking+HTN, data=dat2)[2:1, 2:1])



#### Program 5-4
chisq.test(dat2$HTN, dat2$smoking)
chisq.test(dat2$HTN, dat2$smoking)$expected
chisq.test(xtabs(~smoking+HTN, data=dat2))

#### Program 5-5
fisher.test(dat2$HTN, dat2$smoking)

#### Program 5-6
dat3<-dat2 %>% mutate(CEA.grp=as.factor(ifelse(CEA>5, 1, 0)),
                      post.CEA.grp=as.factor(ifelse(post.CEA>5, 1, 0)))
xtabs(~CEA.grp+post.CEA.grp, data=dat3)
mcnemar.test(xtabs(~CEA.grp+post.CEA.grp, data=dat3))
mcnemar.test(dat3$CEA.grp, dat3$post.CEA.grp)



