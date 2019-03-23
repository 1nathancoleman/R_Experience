# Montgomery County Traffic Stops Data

library(data.table)
stops.full <-fread("Traffic_Violations.csv")
stops.full<-read.csv("Traffic_Violations.csv",
                     header=TRUE,as.is=TRUE)

#stops.full<-read.csv("http://data.montgomerycountymd.gov/api/views/4mse-ku6q/rows.csv?accessType=DOWNLOAD",
                    # header=TRUE,as.is=TRUE)

# subset to last year
last.year<-2017
stops.full$AutoYear<-as.numeric(stops.full$Year)
stops.full$Year<-as.numeric(substr(stops.full$Date.Of.Stop,7,10))
stops.last<-subset(stops.full,Year==last.year)
# delete the really big data set ... don't need to tie up the memory
rm(stops.full)

# Create Month and Hour variables
stops.last$Month<-as.numeric(substr(stops.last$Date.Of.Stop,1,2))
stops.last$Hour<-as.numeric(substr(stops.last$Time.Of.Stop,1,2))

# clean up dataset
stops.last$AutoState<-stops.last$State
stops.last$Out.of.State<-(stops.last$AutoState!="MD")

stops.last$Color<-as.character(stops.last$Color)
stops.last$Color[stops.last$Color %in% c("CAMOUFLAGE","CHROME","COPPER","CREAM","MULTICOLOR","N/A","PINK")]<-"OTHER"
stops.last$Color<-factor(stops.last$Color)

# other filters
stops.last<-subset(stops.last,Color != "N/A")
stops.last<-subset(stops.last,Color != "")
stops.last<-subset(stops.last,Gender != "U")
stops.last<-subset(stops.last,HAZMAT == "No")
stops.last<-subset(stops.last,AutoYear > 1990 & AutoYear < last.year+2)

# convert character variables to factors
stops.last$SubAgency<-factor(stops.last$SubAgency)
stops.last$Accident<-factor(stops.last$Accident)
stops.last$Belts<-factor(stops.last$Belts)
stops.last$Personal.Injury<-factor(stops.last$Personal.Injury)
stops.last$Property.Damage<-factor(stops.last$Property.Damage)
stops.last$Commercial.License<-factor(stops.last$Commercial.License)
stops.last$Commercial.Vehicle<-factor(stops.last$Commercial.Vehicle)
stops.last$Alcohol<-factor(stops.last$Alcohol)
stops.last$Work.Zone<-factor(stops.last$Work.Zone)
stops.last$Contributed.To.Accident<-factor(stops.last$Contributed.To.Accident)
stops.last$Race<-factor(stops.last$Race)
stops.last$Gender<-factor(stops.last$Gender)
stops.last$Out.of.State<-factor(stops.last$Out.of.State)


# Create dataset for Speeding
#  example: EXCEEDING MAXIMUM SPEED: 49 MPH IN A POSTED 40 MPH ZONE
speed.last1<-subset(stops.last,substr(Description,1,23)=="EXCEEDING MAXIMUM SPEED")
# difference between cited speed and posted speed limit
speed.last1$speed<-as.numeric(substr(speed.last1$Description,26,27))-as.numeric(substr(speed.last1$Description,45,46))
speed.last1<-subset(speed.last1,!is.na(speed))
#  example: EXCEEDING POSTED MAXIMUM SPEED LIMIT: 39 MPH IN A POSTED 30 MPH ZONE
speed.last2<-subset(stops.last,substr(Description,1,30)=="EXCEEDING POSTED MAXIMUM SPEED")
# difference between cited speed and posted speed limit
speed.last2$speed<-as.numeric(substr(speed.last2$Description,39,40))-as.numeric(substr(speed.last2$Description,58,59))
speed.last2<-subset(speed.last2,!is.na(speed))
# combine and subset to columns of interest
speed.last<-rbind(speed.last1,speed.last2)
speed.last<-speed.last[,c(4,9:12,14,16:18,24,28:30,36:38,40,41)]


# Create dataset for Ticket/Warning
ticket.last<-subset(stops.last,Violation.Type %in% c("Citation","Warning") )
ticket.last$Ticket<-factor(ticket.last$Violation.Type=="Citation")
# subset to columns of interest
ticket.last<-ticket.last[,c(4,9:12,14,17,18,24,28:30,36:38,40,41)]


# make a prediction at a new observation
#  note: grab an observation in the dataset that is very similar to my situation and change it
new.obs<-speed.last[25,]
new.obs$AutoYear<-2017
new.obs$Month<-8
new.obs$Hour<-18

#//////////////////////////////////////////////////////////////////////////////////////////////////////////////

#Pre Class Assignment 
tail(speed.last)
summary(speed.last$speed)
hist(speed.last$speed)
dim(speed.last)



#In class Analysis

#1.Create train and test datasets 

#Make sure that you clear off all the data information(usually split like 80% 20%)
set.seed(12)
dim(speed.last)
#train should be about 8000, test should be the rest
train.rows<- sample(9773,8000)
#we want all the columns so we want to leave just a ,
speed.train<- speed.last[train.rows, ] 
speed.test<- speed.last[-train.rows, ]

#Validate similarity between train and test; check quartiles and averages and stuff to make sure it is similar
summary(speed.train$speed)
summary(speed.test$speed)
#confirmed that summary statistics are similar 


#2. Grow a Random Forest on train (don't forget to instal the package in R) 
library(randomForest)

#Fit Model (have to put in train, test, and the tuning parameters)
#you can figure out which columns to include and not include by saying names(speed.last)
#we use train to fit the model and test to validate our predictions
out.speed<- randomForest(x=speed.train[,-18], y=speed.train$speed,
                         xtest = speed.test[,-18],ytest = speed.test$speed, replace = TRUE, #bootstrap samples for tree
                         keep.forest = TRUE, 
                         ntree = 50, 
                         #number of trees made
                         mtry = 5, 
                         #how many columns you want to test 25% ish number of explanatory variables the tree looks at
                         nodesize = 25) #we don't want it at one because then it is overfit


#prediction performance (will return the mean of squared residuals)
out.speed
#we look at the two RMSEs (need to take the square root) and ask did we over fit? If they are close we are good 
sqrt(49.2496) #train RMSE
sqrt(50.42) #test RMSE
#with this information we can compute a confidence interval with our prediction (7.018 train, 7.1 test)


#3. Model Insight (interpretation) which number gets used the most?
importance(out.speed)
#make a chart as to which one is most important; you should refresh your prediction whenever you change location or time
varImpPlot(out.speed)

#Three most important explanatory variables: Hour, SubAgency, Color

#4.predict for new observation (14.81462)

predict(out.speed, newdata= new.obs)


#Research Task: Predict how fast somebody can go before they get a ticket and interpret which factors are most important in being pulled over for speeding
#Data features: Random Tree model that uses a given new observation to predict how fast that observation could go given several factors. Also describes most impactful factors
#This model predicts and also gives some information on what information is impactful

#Analysis Weaknesses: not perfectly reproducable because each time the model is run, different trees will be created, which will have different estimates
#Furthermore, not really sure how heavily each factor affects the response variable, just if it appears often in the trees

#Challenge: Responsal variable(Inches away from the lane lines); Cops often check for drunk drivers by observing how close they drive to the lane lines. 
#So we could measure (we would have to estimate it) how close the driver is to the lane lines. We could then test the explanatory factors to see what would cause this (probably alcohol would be important)

#////////////////////////////////////////////////////////////////////////////////////////////////////////////

