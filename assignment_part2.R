library(dplyr)

#Load Data
toothgrowthdata <- datasets::ToothGrowth

#Group the data by supp and then dose.
groupeddata <- toothgrowthdata %>% group_by(supp, dose)

#Basic Structure of the data
dim(toothgrowthdata)
str(toothgrowthdata)
summary(toothgrowthdata)

#Exploratory Plot
boxplot(len ~ supp + dose, data = ToothGrowth)

#Confirm What the PLot Shows, Calulate Mean, Sd, Var
summarizedgroupdata <- summarise(groupeddata, mean = round(mean(len),2), sd = round(sd(len),2), var = round(var(len),2))
summarizedgroupdata

#Test if OJ is better than VC for each dose level

##Statistical Tests
#Ho = muOJ = muVC  
#Ha = muOJ != to muVC
#The difference in means is not significant (confidence interval includes 0), and the p-value > 0.05, so we fail to reject the null hypothesis.
t1 <- t.test(len ~ supp, paired = FALSE, var.equal = TRUE, data = toothgrowthdata)
t1$conf.int
t1$p.value

#Since we didn't find that overall there was a significant differenct between delivery methods, I will check to each dose individually.
#Subset data for the following hypothesis tests
dose0.5 <- subset(toothgrowthdata, dose == 0.5)
dose1.0 <- subset(toothgrowthdata, dose == 1.0)
dose2.0 <- subset(toothgrowthdata, dose == 2.0)

################################Check to see, for each dose level, if there is a difference between delivery methods
#Ho muOJ0.5 = muVC0.5
#Ha muOJ0.5 > muVC0.5
t2 <- t.test(dose0.5[dose0.5$supp=="OJ",1], dose0.5[dose0.5$supp=="VC",1], lower.tail = FALSE, paired = FALSE, 
             var.equal = TRUE, data = dose0.5)
t2$conf.int
t2$p.value

#Check to see, for each dose level, if there is a difference between delivery methods
#Ho muOJ1.0 = muVC1.0
#Ha muOJ1.0 > muVC1.0
t3 <- t.test(dose1.0[dose1.0$supp=="OJ",1], dose1.0[dose1.0$supp=="VC",1], lower.tail = FALSE, paired = FALSE, 
             var.equal = TRUE, data = dose1.0)
t3$conf.int
t3$p.value

#Check to see, for each dose level, if there is a difference between delivery methods
#Ho muOJ2.0 = muVC2.0
#Ha muOJ2.0 > muVC2.0
t4 <- t.test(dose2.0[dose2.0$supp=="OJ",1], dose2.0[dose2.0$supp=="VC",1], lower.tail = FALSE, paired = FALSE, 
             var.equal = TRUE, data = dose2.0)
t4$conf.int
t4$p.value

###########################Now, lets test to see if the increase is doses is significant for the OJ delivery method
#Ho = mu0.5 = mu1.0
#Ha = mu1.0 > mu0.5 
#At a 95% significance level, the difference of means is significant, and the p-value < 0.05, so we will reject the null hypothesis
t5 <- t.test(toothgrowthdata[toothgrowthdata$dose==1.0,1], toothgrowthdata[toothgrowthdata$dose==0.5,1], lower.tail = FALSE, paired = FALSE, 
             var.equal = TRUE, data = toothgrowthdata)
t5$conf.int
t5$p.value

#Ho = mu1.0 = mu2.0
#Ha = mu2.0 > mu0.5 
#At a 95% significance level, the difference of means is significant, and the p-value < 0.05, so we will reject the null hypothesis
t6 <- t.test(toothgrowthdata[toothgrowthdata$dose==2.0,1], toothgrowthdata[toothgrowthdata$dose==1.0,1], lower.tail = FALSE, paired = FALSE, 
             var.equal = TRUE, data = toothgrowthdata)
t6$conf.int
t6$p.value


#Appendix
#Checking for the rate of false positives.  Didn't correct for a false positive rate, due to the limited number of hypothesis tests.  
#For the amount of tests conducted, only 0.3 tests could be false positive, which I view as an acceptable level. 
numhypotheistests <- 6
levelsignificance <- 0.05
numhypotheistests * levelsignificance