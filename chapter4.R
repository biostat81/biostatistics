setwd("C:\\Users\\user\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")
setwd("C:\\Users\\KNOU_stat\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")

####
#### Program 4-1

setwd("C:/Users/KNOU_stat/R_codes")
dat0<-read.csv("biostat_ex_data.csv")
library(dplyr)
dat1<-dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y, 
                              post.CA19.9.binary, post.CA19.9.3grp),
                         as.factor)
library(ggplot2)
ggplot(dat1) + geom_histogram(aes(x=weight), color="black", fill="skyblue", 
                              breaks=seq(40, 100, 10)) 


#### Program 4-2
t.test(weight~sex, data=dat1)

#### Program 4-3
wilcox.test(weight~sex, data=dat1)

#### Program 4-4
ggplot(dat1) + geom_histogram(aes(x=CEA), color="black", fill="skyblue")

#### Program 4-5
ggplot(dat1) + geom_histogram(aes(x=log(CEA)), color="black", fill="skyblue")

#### Program 4-6
fit<-aov(log(CEA)~stage, data=dat1)
summary(fit)
oneway.test(log(CEA)~stage, data=dat1)

#### Program 4-7
kruskal.test(CEA~stage, data=dat1)

#### Program 4-8
ggplot(dat1) + 
  geom_histogram(aes(x=log(post.CEA) - log(CEA)), color="black", fill="skyblue")

#### Program 4-9
t.test(log(dat1$post.CEA), log(dat1$CEA), paired=TRUE)

#### Program 4-10
wilcox.test(log(dat1$post.CEA), log(dat1$CEA), paired=TRUE)



