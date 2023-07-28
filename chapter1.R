#setwd("C:\\Users\\user\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")

### Program 1-1
setwd("C:/Users/KNOU_stat/R_codes")

### Program 1-2
install.packages("dplyr")
library(dplyr)

### Program 1-3
setwd("C:/Users/KNOU_stat/R_codes")
dat0<-read.csv("biostat_ex_data.csv")
summary(dat0)

### Program 1-4
library(dplyr)
dat1<-dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y, 
                              post.CA19.9.binary, post.CA19.9.3grp),
                         as.factor)
summary(dat1)

### Program 1-5
summary(dat1$stage)
table(dat1$stage)

### Program 1-6
library(ggplot2)
ggplot(dat1) + geom_bar(aes(x=stage))

### Program 1-7
mean(dat1$age)
median(dat1$age)
var(dat1$age)
sd(dat1$age)
fivenum(dat1$age)
summary(dat1$age)


### Program 1-8
ggplot(dat1) + geom_histogram(aes(x=age))
ggplot(dat1) + geom_histogram(aes(x=age), breaks=seq(20, 80, 10),
                              color="black", fill="skyblue")
ggplot(dat1) + geom_boxplot(aes(x=1, y=age))+
  scale_x_continuous(breaks = NULL) +
  theme(axis.title.x = element_blank())

### Program 1-9
ggplot(dat1) + geom_histogram(aes(x=CA19.9), color="black", fill="grey")+
  geom_vline(xintercept=mean(dat1$CA19.9), color="red")+
  geom_vline(xintercept=median(dat1$CA19.9), color="blue")
mean(dat1$CA19.9)
median(dat1$CA19.9)

### Program 1-10
library(tableone)
t1<-CreateTableOne(vars=c("age", "sex", "CA19.9", "CRP", "CEA", 
                          "stage", "smoking", "obesity"), data=dat1)
summary(t1, digits=4)
print(t1)
print(t1, nonnormal=c("CA19.9", "CRP", "CEA"))



