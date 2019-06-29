#Final Project David Jan-Liu dal2111@bu.edu
#Data Set - Mid-Career Salaries by college majors
#Wallstreet Journal source

#Preparing the data
setwd("C:/Users/dal21/OneDrive/BU Summer 2019/Final Project") 
x <- read.csv("C:/Users/dal21/OneDrive/BU Summer 2019/Final Project/degrees-that-pay-back.csv")
head(x,n=2)
View(x)

install.packages("prob")
library(prob)
nrow(x) #50 rows
df<- data.frame(x$Undergraduate.Major)
boolean_vector<- df$x.Undergraduate.Major %in% c("Business Management", "Communications", "Economics", "Psychology", 
                                                 "Health Care Administration", "Information Technology (IT)", "Biology")
boolean_df<- data.frame(boolean_vector)
boolean_df<- data.frame(ifelse(boolean_vector==TRUE,1,0))
colnames(boolean_df)<- c("BU Popular Major")
x<- cbind(x,boolean_df) #indicating which majors are popular at BU

#changing salary factor class into actual numbers and renaming the colnames
x$Undergraduate.Major<- data.frame(x$Undergraduate.Major)
colnames(x$Undergraduate.Major)<- c("Major")
x$Starting.Median.Salary <- data.frame(as.numeric(gsub('[$,]', '', x$Starting.Median.Salary)))
colnames(x$Starting.Median.Salary)<-c("Starting_Median")
x$Mid.Career.Median.Salary <- data.frame(as.numeric(gsub('[$,]', '', x$Mid.Career.Median.Salary)))
colnames(x$Mid.Career.Median.Salary)<-c("Mid-Career Median")
x$Percent.change.from.Starting.to.Mid.Career.Salary<-data.frame(x$Percent.change.from.Starting.to.Mid.Career.Salary)
colnames(x$Percent.change.from.Starting.to.Mid.Career.Salary)<-c("% Change to Mid-Career")
x$Mid.Career.10th.Percentile.Salary <- data.frame(as.numeric(gsub('[$,]', '', x$Mid.Career.10th.Percentile.Salary)))
colnames(x$Mid.Career.10th.Percentile.Salary)<-c("Mid-Career 10th PCTL")
x$Mid.Career.25th.Percentile.Salary <- data.frame(as.numeric(gsub('[$,]', '', x$Mid.Career.25th.Percentile.Salary)))
colnames(x$Mid.Career.25th.Percentile.Salary)<-c("Mid-Career 25th PCTL")
x$Mid.Career.75th.Percentile.Salary <- data.frame(as.numeric(gsub('[$,]', '', x$Mid.Career.75th.Percentile.Salary)))
colnames(x$Mid.Career.75th.Percentile.Salary)<-c("Mid-Career 75th PCTL")
x$Mid.Career.90th.Percentile.Salary <- data.frame(as.numeric(gsub('[$,]', '', x$Mid.Career.90th.Percentile.Salary)))
colnames(x$Mid.Career.90th.Percentile.Salary)<-c("Mid-Career 90th PCTL")
x
x$Major_Category<- data.frame((c("Business","STEM","STEM","Social Science","Arts & Humanities","Arts & Humanities","STEM","Business","STEM","STEM","STEM","Social Science","STEM","STEM","STEM","Social Science","Arts & Humanities","Social Science","Social Science","STEM","Arts & Humanities","Arts & Humanities","Business","STEM","STEM","STEM","Arts & Humanities","Health","Social Science","Social Science","STEM","STEM","Arts & Humanities","Social Science","Arts & Humanities","Business","Business","STEM","STEM","Arts & Humanities","Health","Health","Arts & Humanities","Health","STEM","Social Science","Social Science","Arts & Humanities","Social Science","Arts & Humanities")))
colnames(x$Major_Category)<-c("Major Category")

#order rows by Major Category
x[order(x$Major_Category,x$Undergraduate.Major,x$Starting.Median.Salary,x$Mid.Career.Median.Salary),]

#Analyzing the Data

#Categorical Barplots
a<- aggregate(x$Starting.Median.Salary,by = x$Major_Category,FUN = median)
a<- a[order(a$Starting_Median),]
a$Starting_Median

b<- aggregate(x$Mid.Career.Median.Salary,by = x$Major_Category,FUN = median)
b<- b[order(b$`Mid-Career Median`),]

par(las=2)
barplot(a$Starting_Median/1000, names=c("Arts","SocialSci","Business","Health","STEM"), 
        horiz=TRUE, col=rainbow(7), cex.names = .75, main ="Starting Median Income by Degree Category",
        xlab="Income in 1,000s")
barplot(b$`Mid-Career Median`/1000, names=c("SocialSci","Arts","Health","Business","STEM"), 
        horiz=TRUE, col=rainbow(7), cex.names = .75, cex.main=1, 
        main ="Mid-Career Median Income by Degree Category", xlab="Income in 1,000s")

#Categorical Barplots for Popular BU Majors
BU_majors<- subset(x,x$`BU Popular Major`==1)
class(BU_majors)
BU_majors[order(BU_majors$Starting.Median.Salary),]
BU_majors[order(BU_majors$Mid.Career.Median.Salary),]

BU_majors_st<- data.matrix(BU_majors$Starting.Median.Salary/1000)

BU_majors_st<- BU_majors_st[order(BU_majors_st$Starting_Median),]
BU_majors_st<- sort(BU_majors_st)

BU_majors_med<- data.matrix(BU_majors$Mid.Career.Median.Salary/1000)
BU_majors_med<- sort(BU_majors_med)

par(las=2,mai=c(1.5,2.5,.25,.5)) 
barplot(BU_majors_st, beside=T, names=c("Psychology","Comm.","Health Care","Biology","Business Mgt","IT", "Econ"),
        col=rainbow(7), horiz=TRUE, cex.names = .75, 
        main ="Starting Median Income by BU Popular Majors", xlab="Income in 1,000s")
barplot(BU_majors_med, beside=T, names=c("Psychology","Health Care","Biology","Comm.","Business Mgt","IT", "Econ"),
        col=rainbow(7), horiz=TRUE, cex.names = .75, 
        main ="Mid-Career Median Income by BU Popular Majors", xlab="Income in 1,000s")

 
#Numerical Analysis

#Boxplot for all listed majors - Starting Median Income
d<- x$Starting.Median.Salary
d<- as.numeric(unlist(x$Starting.Median.Salary))
d<- sort(d)                      
fivenum.var1<- fivenum(d) #per R bloggers, use fivenum instead of summary, due to different ways of calculating quantiles 
fivenum.var1<- fivenum.var1/1000
summary(fivenum.var1)

par(las=2,mai=c(1.5,2.5,.25,.5)) 
boxplot(d/1000, horizontal=FALSE, xaxt="n", 
        main="Starting Income for all 50 Majors")
axis(side=2, at=fivenum.var1, labels=FALSE)

#Boxplot for all listed majors - Mid-Career Median Income
e<- as.numeric(unlist(x$Mid.Career.Median.Salary))
e<- sort(e)                      
fivenum.var2<- fivenum(e) #per R bloggers, use fivenum instead of summary, due to different ways of calculating quantiles 
fivenum.var2<- fivenum.var2/1000
summary(fivenum.var2)

par(las=2,mai=c(1.5,2.5,.25,.5)) 
boxplot(e/1000, horizontal=FALSE, xaxt="n", 
        main="Mid-Career Income for all 50 Majors")
axis(side=2, at=fivenum.var1, labels=FALSE)

#Dot Plot with STEM Freshmen Enrollment by Year
#https://nsf.gov/nsb/sei/edTool/data/college-09.html - needed more education data to expand on
y<- data.matrix(c(32.4,34.2,34.2,32.0,33.7,33.1,33.5,33.5,32.6,33.1,30.9,32.0,31.9,34.7,36.2,38.4,40.1,39.2))
#% of freshmen that have intended on majoring in a STEM major since 1995
colnames(y)<- c("PCT STEM Freshmen")

plot(1995:2012,y,main="% of College Freshmen majoring in STEM",xlab="Year",ylab="% of College Freshmen")

#Multivariable Data - Mosaic Plot
f<- as.numeric(unlist(b$`Mid-Career Median`))
f<- sort(f)            
class(f)

a1<- aggregate(x$Starting.Median.Salary,by = x$Major_Category,FUN = median)
a1<- a[order(a$`Major Category`),]

b1<- aggregate(x$Mid.Career.Median.Salary,by = x$Major_Category,FUN = median)
b1<- b[order(b$`Major Category`),]

f<- cbind(a1$Starting_Median,b1$`Mid-Career Median`)
rownames(f)<- c("Arts","Business","Health","SocialSci","STEM") 
colnames(f)<- c("Starting Median","Mid-Career Median")
f<- t(f)
mosaicplot(f, color=rainbow(8),main="Degree Categories and Income",cex.axis = .8)

#Distribution of Data

#Density of 50 Majors Beginning Career
h<- as.numeric(unlist(x$Starting.Median.Salary/1000))
h<- density(h)
plot(h,main="50 Majors Starting Incomes", xlab = "Incomes in 1000s", cex.axis = .8)
polygon(h,col="red",border="blue")

#Density of 50 Majors in Mid-Career
par(mar=c(5,4,2,10)+0.1)
h<- as.numeric(unlist(x$Mid.Career.Median.Salary/1000))
h<- density(h)
plot(h,main="50 Majors Mid-Career Incomes", xlab = "Incomes in 1000s", cex.axis = .8)
polygon(h,col="red",border="blue")

#Drawing random samples of a variable to show the applicability of the Central Limit Theorem
#The Central Limit Theorem states that the distribution of the sample means for a given 
#sample size of the population has the shape of the normal distribution.

#Using the variable x$Mid.Career.Median.Salary same as 50 Majors in Mid-Career Density Plot
#Average of Mid Career Incomes - $74,786
#Standard Deviation of Mid Career Incomes - 15,927
#Judging from the Density Plot of the 50 Majors in Mid-Career Incomes, the distribution is close to normal,
#so running a large number of trials of the average of x sample size means will approximate a normal distribution
k<- as.numeric(unlist(x$Mid.Career.Median.Salary))
k<- as.numeric(as.character(k))
#k is the variable I am using to demonstrate the CLT

par(mfrow = c(2,2))

#Sample Size = 2
set.seed(6456)
samples<- 1000
sample.size<- 2
xbar1<- numeric(samples)

for (i in 1:samples) {
    
    xbar1[i] <- mean(sample(k, size=sample.size,replace=TRUE))
    
}

xbar1
hist(xbar1/1000, prob=TRUE, breaks=15,
     main="Sample Size = 2", xlim=c(40,110), ylim=c(0,.2), xlab= "Means of Mid-Career Incomes in 1000s", cex.axis=.8)
mean(xbar1)


#Sample Size = 3
set.seed(6456)
samples<- 1000
sample.size<- 3
xbar2<- numeric(samples)

for (i in 1:samples) {
    
    xbar2[i] <- mean(sample(k, size=sample.size,replace=TRUE))
    
}

xbar2
hist(xbar2/1000, prob=TRUE, breaks=15,
     main="Sample Size = 3", xlim=c(40,110), 
     ylim=c(0,.2), xlab= "Means of Mid-Career Incomes in 1000s", cex.axis=.8)

#Sample Size = 4
set.seed(6456)
samples<- 1000
sample.size<- 4
xbar3<- numeric(samples)

for (i in 1:samples) {
    
    xbar3[i] <- mean(sample(k, size=sample.size,replace=TRUE))
    
}

xbar3
hist(xbar3/1000, prob=TRUE, breaks=15,
     main="Sample Size = 4", xlim=c(40,110), 
     ylim=c(0,.2), xlab= "Means of Mid-Career Incomes in 1000s", cex.axis=.8)

#Sample Size = 5
set.seed(6456)
samples<- 1000
sample.size<- 5
xbar4<- numeric(samples)

for (i in 1:samples) {
    
    xbar4[i] <- mean(sample(k, size=sample.size,replace=TRUE))
    
}

xbar4
hist(xbar4/1000, prob=TRUE, breaks=15,
     main="Sample Size = 5", xlim=c(40,110), 
     ylim=c(0,.2), xlab= "Means of Mid-Career Incomes in 1000s", cex.axis=.8)

mean(xbar1)
mean(xbar2)
mean(xbar3)
mean(xbar4)
sd(xbar1)
sd(xbar2)
sd(xbar3)
sd(xbar4)

#Sampling Methods on data - https://collegescorecard.ed.gov/data/ - new data to make things more interesting
#with a larger dataset that simulates more of a population dataset to perform sampling on

#we can compare dataset mean, variance, and standard deviation versus sampling characteristics

#Tuition cost for 7k+ schools in the U.S.
x1 <- read.csv("C:/Users/dal21/OneDrive/BU Summer 2019/Final Project/2017_tuitions.csv")
head(x1)
nrow(x1)
x1<- x1[x1$COSTT4_A !="NULL",]

#Cost of Education Column (Main Dataset)
x1$COSTT4_A<- data.frame(as.character(x1$COSTT4_A))
x1$COSTT4_A<- as.numeric(t(x1$COSTT4_A)) #factor to data frame to numeric
mean(x1$COSTT4_A) #23,464.12
var(x1$COSTT4_A)  #225,615,879 
sd(x1$COSTT4_A)   #15,020.52

#Simple Random Sampling with Replacement
set.seed(53534)
n<- 70
N<- nrow(x1)
s<- srswr(n,N)
s[s != 0]
rows<- (1:nrow(x1))[s!=0]
length(rows)
rows<- rep(rows,s[s!=0])

sample.1<- x1[rows,]
m1<- sample.1$COSTT4_A
nrow(sample.1) #70 samples taken

hist(m1/1000,breaks="Sturges",prob="FALSE",xlim=(c(0,65)), main="Simple Random with Replacement (n=70)", 
     xlab="Tuition Cost in 1000s",ylim=(c(0,30)))

mean(m1) #23,464.13
var(m1)  #172,783,661
sd(m1)   #13,144.72

##Simple Random Sampling without Replacement
set.seed(53534)
s<- srswor(n,N) #simple random sample without replacement (srswor)
s[s != 0]
rows<- (1:nrow(x1))[s!=0]
length(rows)
rows<- rep(rows,s[s!=0])
sample.2<- x1[rows,]
m2<- sample.2$COSTT4_A
nrow(sample.2) #70 samples taken

hist(m2/1000,breaks="Sturges",prob="FALSE",xlim=(c(0,70)), main="Simple Random w/o Replacement (n=70)", 
     xlab="Tuition Cost in 1000s",ylim=(c(0,40)))

mean(sample.2$COSTT4_A) #25,335.9
var(sample.2$COSTT4_A)  #200,377,740
sd(sample.2$COSTT4_A)   #14,155.48

#Systematic Sampling
N<- nrow(x1)
n<- 70
#items in each group
k<- ceiling(N/n)
k
#The next step is to select an item at random from the first group of k items. 
#Based on this selection, the sample of size n is drawn by selecting every kth item.

#random item from the first group of 70 items, then base the kth item from that number
set.seed(536745)
r<- sample(k,1) #first item from the first 70
s<- seq(r,by = k,length= n)
sample.3<- na.omit(x1[s,]) #remove NA
m3<- sample.3$COSTT4_A
nrow(sample.3)

hist(m3/1000,breaks="Sturges",prob="FALSE",xlim=(c(0,70)), main="Systematic Sampling (n=70)", 
     xlab="Tuition Cost in 1000s",ylim=(c(0,40)))

mean(sample.3$COSTT4_A) #26,206.38
var(sample.3$COSTT4_A)  #230,916,544
sd(sample.3$COSTT4_A)   #15,195.94

#Linear Regression - correlation between starting median salary and mid-career median salary by major
#Mid-Career Income as the outcome variable
#d = Starting median income
#e = Mid-career median income

par(mfrow = c(1,1))
plot(d,e, main="Linear Regression", 
     xlab="Starting median income (dependent variable)",pch=19,cex.axis=.6,
     ylab="Mid-Career median income")
cor(d,e)
model1<- lm(e ~ d)
summary(model1)
attributes(model1)
abline(model1)

