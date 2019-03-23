#Scottish Foot Racing Data

race<- read.table(header = TRUE, "http://www.statsci.org/data/general/
hills.txt")

#EDA
tail(race)
identify(race$Time, race$Climb, labels = "outliers")
#7 11 17 18
outlier1<- race[c(7, 11, 17, 18),]
outlier1

plot(race$Time, race$Distance)
identify(race$Time, race$Distance, labels = "outliers")
#7 11 18
outlier2<- race[c(7, 11, 18),]
outlier2


plot(race$Time, race$Race)
identify(race$Time, race$Race, labels = "outliers")
outlier3<- race[c(4,5,7,11,17,18,31,33,25),]
outlier3

#In class analysis (Data Integrity)

#Regression Diagnostics (before we do any hypothesis test or prediction we want to check to see if good data)

out.hills<- lm(Time~ Distance + Climb, data=race)

#Compute the leverage
leverage.hills<- lm.influence(out.hills)$hat
subset(leverage.hills,race$Race == "BenNevis")
#If very little leverage it should contribute 1/35 (an equal amount for all observations) this is an okay number 

#Compute cook's distance

cd.hills<- cooks.distance(out.hills)
subset(cd.hills, race$Race== "MoffatChase")
#Already rates things by standard error of the measurments. We don't really know what this number means 

#R Studentized residuals 
R.hills<- rstudent(out.hills)
subset(R.hills, race$Race == "CairnTable")
#Looks a lot like a z-score; 2 or 3 is a high number, this is fine 


#Validate Normality Assumption
hist(R.hills)
ks.test(R.hills, "pnorm")
#Pvalue = .03657- The KS Test isn't very critical 

#Kildcon Hill: we do not know if three minutes are good or bad according to the model because we need to take into account the variance
#We need to measure the R-Studentized Residuals. Approximately <2 or <-2 

subset(R.hills, race$Race=="KildconHill")
#.2054, which is not that extreme. Pretty common difference 

#Is moffet chase an influencial observation (leverage .171, cooks distance .121)
subset(leverage.hills, race$Race == "MoffatChase")
#Looks unusual in the leverage
subset(cd.hills, race$Race=="MoffatChase")
#Passes the rule of thumb for the cooks distance

#Is it a good or bad influencial?
par(mfrow= c(1,2))
plot(race$Distance, race$Time)
points(race$Distance[35], race$Time[35], col="red", pch=19)
plot(race$Climb, race$Time)
points(race$Climb[35], race$Time[35], col="red", pch=19)
par(mfrow=c(1,2))

#it is a good influential because we don't have very much data in the space, but the one observation provides significant value

# Is Lairig Ghru an influencial observation?
subset(leverage.hills, race$Race == "LairigGhru")
#Looks unusual in the leverage
subset(cd.hills, race$Race=="LairigGhru")
#Looks unusual in cooks test as well 

#Is it a good or bad influencial?
par(mfrow= c(1,2))
plot(race$Distance, race$Time)
points(race$Distance[11], race$Time[11], col="red", pch=19)
plot(race$Climb, race$Time)
points(race$Climb[11], race$Time[11], col="red", pch=19)
par(mfrow=c(1,2))

#It appears to be a bad influencial observation, especially on the climb variable
#Is cow Hill an outlier?

subset(R.hills, race$Race== "CowHill")
#.314 doesn't seem too extreme 
#Pvalue of H-0 cow hill is not an outlier
2*(1-pnorm(.314))

#Pvalue for H0:knockHill is not an outlier 
subset(R.hills, race$Race=="KnockHill")
2*(1-pnorm(7.61))
#Looks like this is the wrong value recorded; we should probably throw this data out 
