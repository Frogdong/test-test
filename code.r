# 1
# [1]
getwd()
setwd("/Users/frog_dong/Desktop/2018生物统计第三次作业及答案/")
dir()

#[2]
data <- read.csv('homework3_data.csv', header = T, stringsAsFactors = F)
#data <- read.table('homework3_data.csv',header = T,sep=",")
data$Birthweight = data$Birthweight + 5

#[3]
head(data,3)
data[1:3,]
nrow(data)
ncol(data)
dim(data)
class(data)

#[3]
summary(data$Birthweight)
boxplot(data$Birthweight)

#[5]
#1)
?t.test
#2)
install.packages("pwr")
library(pwr)
?pwr

# 2

m <- mean(data$Birthweight)
mu = 122
sd <- 12
n <- 20
p.upper <- pnorm((126.0-mu)/(sd/sqrt(n)))
p.lower <- pnorm((100.0-mu)/(sd/sqrt(n)))
p <- p.upper - p.lower
p


#(2) 6?? R????4?֣?????2??
s <- sd(data$Birthweight)
left<- m-qt(0.975,n-1)*s/sqrt(n) 
right<-m+qt(0.975,n-1)*s/sqrt(n)
left;right


left<- mean(data$Birthweight)-qt(0.975,n-1)*s/sqrt(n) 
right<-mean(data$Birthweight)+qt(0.975,n-1)*s/sqrt(n)
left;right
t.test(data$Birthweight,alternative = "two.sided",mu=122)


#(3) 7?? ??ʽ2?֣?R????3?֣?????2??
#ʹ?õ?β????
t<-(m-122)/s*sqrt(n)
p<-1-pt(t,n-1)
p

##ʹ??t.test()##
t.test(data$Birthweight, alternative = "greater", mu= 122)

#??4??7?? ??ʽ2?? R????3?? ????2??
#ʹ??˫β????
#????Ϊt>0
p<-(1-pt(t,n-1))*2
p
#[1] 0.4656597
#P=0.4656597>0.05

##ʹ??t.test()##
t.test(data$Birthweight, mu = 122)



#(5) 6?? R????4?? ????2??
power<-pt(qt(0.025,n-1)+abs(m-122)/s*sqrt(n),n-1) 
power
pwr.t.test(d = abs( m - 122) / s, sig.level = 0.05, n = 20,
           type="one.sample" ,alternative="two.sided")

#(6) 7?? R????4?? ????3??
#ʹ??t-test????????��ʱ??ʹ??z-test??̬?ֲ????й???

n<-(qnorm(1-0.05)+qnorm(1-0.01/2))^2*s^2/(m-122)^2
n

#[1] 642.7163
# n=643

#ʹ??pwr??
library(pwr)
pwr.t.test(d=abs(m-122)/s,sig.level=0.01,power=1-0.05,type="one.sample",alternative="two.sided")
