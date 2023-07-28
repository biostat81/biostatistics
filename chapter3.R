#### Program 3-1

pop<-(-4):4
sample(pop, size=4, replace=TRUE)

#### Program 3-2
set.seed(1000)
sam<-sample(pop, size=4, replace=TRUE)
mean(sam)

#### Program 3-3
m<-10
xbar.vec<-rep(NA, m)
for(i in 1:m){
  set.seed(1000+i)
  xx<-sample(pop, size=4, replace=TRUE)
  xbar.vec[i]<-mean(xx)
}
table(xbar.vec)
hist(xbar.vec, main="n=4, m=10", xlab=bquote(bar(X)))
mean(xbar.vec)
sd(xbar.vec)


#### Figure 3-2
m<-100
xbar.vec<-rep(NA, m)
for(i in 1:m){
  set.seed(1000+i)
  xx<-sample(pop, size=4, replace=TRUE)
  xbar.vec[i]<-mean(xx)
}
table(xbar.vec)
hist(xbar.vec, main="n=4, m=100", xlab=bquote(bar(X)))
mean(xbar.vec)
sd(xbar.vec)

m<-10000
xbar.vec<-rep(NA, m)
for(i in 1:m){
  set.seed(1000+i)
  xx<-sample(pop, size=4, replace=TRUE)
  xbar.vec[i]<-mean(xx)
}
table(xbar.vec)
hist(xbar.vec, main="n=4, m=10000", xlab=bquote(bar(X)))
mean(xbar.vec)
sd(xbar.vec)

#### Program 3-4
m<-10000
xbar.vec<-rep(NA, m)
for(i in 1:m){
  set.seed(1000+i)
  xx<-sample(pop, size=40, replace=TRUE)
  xbar.vec[i]<-mean(xx)
}
table(xbar.vec)
hist(xbar.vec, main="n=40, m=10000", xlab=bquote(bar(X)), xlim=c(-4, 4))
mean(xbar.vec)
sd(xbar.vec)



#setwd("C:\\Users\\KNOU_stat\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")
#setwd("C:\\Users\\user\\Dropbox\\KNOU_강의개편\\학부_바이오통계학\\R")

#### Program 3-5
setwd("C:/Users/KNOU_stat/R_codes")
dat0<-read.csv("biostat_ex_data.csv")
library(dplyr)
dat1<-dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y, 
                              post.CA19.9.binary, post.CA19.9.3grp),
                         as.factor)
mean(dat1$weight)
t.test(dat1$weight)
t.test(dat1$weight, conf.level=0.99)$conf.int

#### Program 3-6
table(dat1$sex)
prop.test(x=sum(dat1$sex==1), n=nrow(dat1))
binom.test(x=sum(dat1$sex==1), n=nrow(dat1))

#### Program 3-7
t.test(dat1$weight, mu=65)
t.test(dat1$weight, mu=68, alternative = "greater")

#### Program 3-8
prop.test(x=sum(dat1$sex==1), n=nrow(dat1), p=0.6)
binom.test(x=sum(dat1$sex==1), n=nrow(dat1), p=0.6)
binom.test(x=sum(dat1$sex==1), n=nrow(dat1), p=0.6, alternative="greater")







