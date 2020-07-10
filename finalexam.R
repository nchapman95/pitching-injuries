##########################################
############# Problem 4 ##################
##########################################

x <- c(1,2,3,4,5)
y <- c(5,4,3,2,-10)


cor(x,y,method='spearman')
cor(x,y,method='pearson')


##########################################
############# Problem 5 ##################
##########################################
library(clinfun)
library(jmuOutlier) 

twenty_cm <- c(42.0, 18.0, 14.0, 36.0, 11.6, 19.0)
ten_cm <- c(15.6, 23.8, 24.4, 24.0, 21.0, 21.2)
five_cm <- c(35.3, 22.5, 16.9, 25.0, 23.1, 26.0)


## K-Sample Problem. 

values=c(twenty_cm,ten_cm, five_cm)
type=rep(c('Twenty CM','Ten CM', 'Five CM'),c(length(twenty_cm),length(ten_cm), length(five_cm)))

boxplot(values~type,xlab='Mowing Height',ylab='Phosphorous Content')

groups=rep(1:3,c(6,6,6))


kruskal.test(values,groups) #Kruskal-Wallis test.

jonckheere.test(values,groups) #Jonckheere-Terpstra test. (clinfun)

perm.f.test(values,groups)


## Scales are different. Does not appear that the Shift Model is reasonable here. 


range(twenty_cm)
range(ten_cm)
range(five_cm)


set.seed(0)
nruns=1000 
n=10 #Common sample size.
groups=rep(1:3,each=10)
nrej1=0 #For ANOVA F test.
nrej2=0 #For Kruskal-Wallis test.
mvec=double(3) #For sample means.
svec=double(3) #For sample variances.
for (run in 1:nruns){
  data=rnorm(30)*rep(c(1,10,100),each=10) #Specifying the alternative.  
  test=aov(data~factor(groups))
  if (summary(test)[[1]][["Pr(>F)"]][1]<0.05){nrej1=nrej1+1}
  if (kruskal.test(data,groups)$p.value<0.05){nrej2=nrej2+1}}
nrej1/nruns
nrej2/nruns

##########################################
############# Problem 7 ##################
##########################################


set.seed(0)

n=10
nruns=1000
nrej1=0
nrej2=0
nrej3=0
for (run in 1:nruns){
  samp1=rnorm(n,mean=0,sd=1)
  samp2=rnorm(n,mean=1,sd=2)
  if (wilcox.test(samp1,samp2)$p.value<0.05){nrej1=nrej1+1}
  if (ansari.test(samp1,samp2)$p.value<0.05){nrej2=nrej2+1}
  if (ks.test(samp1,samp2)$p.value<0.05){nrej3=nrej3+1}}
nrej1/nruns
nrej2/nruns
nrej3/nruns


values=c(samp1,samp2)
type=rep(c('Sample 1','Sample 2'),c(length(samp1),length(samp2)))

boxplot(values~type,xlab='Sample',ylab='Values')

##########################################
############# Problem 8 ##################
##########################################
library(BSDA)
dbinom(0:8,8,0.5) #Needed binomial probabilities.


x <- c(.045,.381,.505, .529, .697, .773, .842, .980)

## Checking Work
SIGN.test(x, md = 0.5, alternative = "greater")

predIntNpar(x,1)

knitr::stitch('/Users/nchapm742/Documents/MSAS/MAT 8452/Final Exam/FinalExam.R')

rmarkdown::render('/Users/nchapm742/Documents/MSAS/MAT 8452/Final Exam/FinalExam.R',
                  "pdf_document")
