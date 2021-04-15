#Load in the necessary libraries
library("dplyr")
library("rcompanion")
library("car")
library("mvnormtest")
library("IDPmisc")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")

# Rename data for ease of typing
CFA <- CFA_data

#OBJECTIVE 1: To reflect on how successful CFA has evolved to fit the new norm (drive-thru only)
CFA$Outside <- CFA$'3rdParty' #recode to make easier to use

#Check for normality
plotNormalHistogram(CFA$Outside) #graph is normal
plotNormalHistogram(CFA$Sales) #graph is normal
plotNormalHistogram(CFA$NormDT) #graph is normal
plotNormalHistogram(CFA$MobDT) #normal, but could be better
  #MobDT sales increased so quickly that it caused the curve to be a little flatter

#All variables seemed to be normally distributed, so will use Bartlett's test to test for Homogeneity of Variance
bartlett.test(Sales ~ Time, data = CFA) #Passes the test

bartlett.test(NormDT ~ Time, data = CFA) #Passes the test

bartlett.test(MobDT ~ Time, data = CFA) #Does not pass the test
  #Abnormal due to the sales increasing so quickly, did not have a lot of weeks between 10 and 20 thousand in sales.
  #Weekly MobDT sales quickly rose from between 5 and 10 thousand to more than 20 thousand a week.

bartlett.test(Outside ~ Time, data = CFA) #Passes the test

#ANALYZE Sales before during and after.
  #Going to try to run an ANOVA with no adjustments.
CFA_ANOVA <- aov(CFA$Sales ~ CFA$Time)
summary(CFA_ANOVA) #The test is significant.
pairwise.t.test(CFA$Sales, CFA$Time, p.adjust="none")
  #Looking at the results, we see that there is a significant difference in sales during the lockdown compared to before and after.
    #Significant, because p = .0000000089 & .0000003, which is very strong.

  #Determine the means in order to draw conclusions:
SALESmeans <- CFA %>% group_by(Time) %>% summarize(Mean = mean(Sales))
SALESmeans
  #CONCLUSION: We see that before and after are fairly equal, meaning we haven't increased overall sales by too much, but did not lose any.
    # We can also see that there was a substantial decrease in overall sales during the 6 week lockdown period (about $30,000). 

#ANALYZE Traditional Drive-Thru(NormDT) sales for before during and after.
  #No adjustments needed
CFA_ANOVA <- aov(CFA$NormDT ~ CFA$Time)
summary(CFA_ANOVA) #The test is significant.
pairwise.t.test(CFA$NormDT, CFA$Time, p.adjust="none")

DTmeans <- CFA %>% group_by(Time) %>% summarize(Mean = mean(NormDT))
DTmeans
  #CONCLUSION: The average increased by ~$20,000 a week in Traditional Drive-Thru sales from before the lockdown to present. 

#ANALYZE amount of sales done through the mobile app and picked up in drive thru before during and after.

  #Going to try to run the ANOVA with the bonferroni adjustment, because it did not meet the assumption with the bartlett test.
ANOVA <- lm(MobDT ~ Time, data=CFA)
Anova(ANOVA, type = "II", white.adjust = TRUE)
pairwise.t.test(CFA$MobDT, CFA$Time, p.adjust="bonferroni")

MOBmeans <- CFA %>% group_by(Time) %>% summarize(Mean = mean(MobDT))
MOBmeans
  #CONCLUSION: The average increased from $7,300/day before Covid to $25,500/day after the lockdown.

#ANALYZE 3rd party delivery sales growth from before COVID to after.

CFA_ANOVA <- aov(CFA$Outside ~ CFA$Time)
summary(CFA_ANOVA)
pairwise.t.test(CFA$Outside, CFA$Time, p.adjust="none")

OUTmeans <- CFA %>% group_by(Time) %>% summarize(Mean = mean(Outside))
OUTmeans
  #CONCLUSION: Went from less than $1,000/day in sales before to over $5,000/day after!
##########################################################################################

###  OBJECTIVE 2: ANALYZE HOW LABOR COSTS HAVE BEEN AFFECTED BY COVID. 
  # This will be calculated by looking at the percent of sales were spent on labor 
  # and by seeing how productivity for each month changes over time.

# recode Time to be numeric

CFA$TimeR=NA
CFA$TimeR[CFA$Time=="Before"]=1
CFA$TimeR[CFA$Time=="During"]=2
CFA$TimeR[CFA$Time=="After"]=3
CFA

## Test the assumptions
# Linearity
scatter.smooth(x=CFA$TimeR,y=CFA$Productivity, main="Poductivity through Covid")
# it is not linear

#Homoscedasticity

lmMod = lm(Productivity ~ TimeR,CFA)

par(mfrow=c(2,2))
plot(lmMod)

lmtest::bptest(lmMod) # we have met the assumption of Homoscedasticity

#Test for Homogeneity of Variance

gvlma(lmMod)  #met assumption test was not significant

# Test for outliers

CookD(lmMod, group=NULL,plot=TRUE, idn=3,newwd=TRUE) # three outliers, 20,25,29

lev=hat(model.matrix(lmMod))
plot(lev)
# no outliers in x space

car::outlierTest(lmMod)
#two outliers in y space: 20,29

summary(influence.measures(lmMod))
#there are no influential outliers in the data

### summarize the analysis

summary(lmMod)
## Conclusion:  Productivity did not change significantly over Covid
#############################################################################

###  OBJECTIVE 3: SHOW THE DIFFERENCE IN HOW PEOPLE ARE ORDING.

# a little data wrangling for Obj 3

names(CFA)[names(CFA)=="3rdParty"]="ThirdParty"
head(CFA)

# make sure DV's are numeric

str(CFA$MobCO)
str(CFA$MobDI)
str(CFA$MobDT)
str(CFA$NormDT)
str(CFA$CarryO)
str(CFA$ThirdParty)
str(CFA$DineIn)
str(CFA$Catering)

# subset and make a matrix

keeps=c("MobCO","MobDI","MobDT",'NormDT',"CarryO","ThirdParty","DineIn","Catering")
CFA1=CFA[keeps]
CFA1mx=as.matrix(CFA1)

## Test assumptions

## Sample size

## Multivariate Normality

mshapiro.test(t(CFA1mx))  #did not pass - p was significant

## Homogeneity of Variance

leveneTest(MobCO~Time,data=CFA)  # did not pass
leveneTest(MobDI~Time,data=CFA)  # did not pass
leveneTest(MobDT~Time,data=CFA)  # did not pass
leveneTest(NormDT~Time,data=CFA)  # passed 
leveneTest(CarryO~Time,data=CFA)  # did not pass
leveneTest(ThirdParty~Time,data=CFA)  # passed
leveneTest(DineIn~Time,data=CFA)  # did not pass
leveneTest(Catering~Time,data=CFA)  # passed

## Absense of Multicolinearity

cor.test(CFA$MobCO,CFA$MobDI,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobCO,CFA$MobDT,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobCO,CFA$ThirdParty,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobCO,CFA$NormDT,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobCO,CFA$CarryO,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobCO,CFA$DineIn,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobCO,CFA$Catering,method="pearson",use="complete.obs") # passed
cor.test(CFA$MobDI,CFA$MobDT,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobDI,CFA$ThirdParty,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobDI,CFA$NormDT,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobDI,CFA$CarryO,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobDI,CFA$DineIn,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobDI,CFA$Catering,method="pearson",use="complete.obs") # passed
cor.test(CFA$MobDT,CFA$ThirdParty,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobDT,CFA$NormDT,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobDT,CFA$CarryO,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobDT,CFA$DineIn,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$MobDT,CFA$Catering,method="pearson",use="complete.obs") # passed
cor.test(CFA$ThirdParty,CFA$NormDT,method="pearson",use="complete.obs") ## did not pass
cor.test(CFA$ThirdParty,CFA$CarryO,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$ThirdParty,CFA$DineIn,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$ThirdParty,CFA$Catering,method="pearson",use="complete.obs") # passed
cor.test(CFA$NormDT,CFA$CarryO,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$NormDT,CFA$DineIn,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$NormDT,CFA$Catering,method="pearson",use="complete.obs") # passed
cor.test(CFA$CarryO,CFA$DineIn,method="pearson",use="complete.obs") # did not pass
cor.test(CFA$CarryO,CFA$Catering,method="pearson",use="complete.obs") # passed
cor.test(CFA$DineIn,CFA$Catering,method="pearson",use="complete.obs") # passed


## MANOVA Analysis

MANOVA = manova(cbind(MobCO,MobDI,MobDT,NormDT,CarryO,ThirdParty,DineIn,Catering)~Time, data = CFA)
summary(MANOVA)

## Post Hoc

summary.aov(MANOVA, test="wilks")