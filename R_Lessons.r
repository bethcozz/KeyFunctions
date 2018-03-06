
library(readr)
#Read in data set
nola <- read_csv("~/Desktop/code/nolabblistings.csv")
#Look at data
print(nola)
head(nola)
describe(nola)

#Scatterplot reviews x price
plot(nola$reviews, nola$price,
     main = "number of reviews by price",
     xlab = "number of reviews",
     ylab = "price",
     col = "grey")

#Create subset of data w/ only focal vars
keyvars <- c("price", "reviews", "overall_satisfaction", "bedrooms", "accommodates", "minstay", "room_type")
nolaTrim <- nola[keyvars]
nolaTrim
head(nolaTrim)
describe(nolaTrim)

nomiss <- na.omit(nolaTrim)

head(nomiss)
describe(nomiss)

#here we specify dataset separately instead of nola$ for all
pairs(~price + overall_satisfaction + bedrooms + accommodates + reviews,
      data = nomiss,
      pch = 20,
      main = "simple scatterplot matrix")
#what this means: price x sat, then rooms, then accommodates, and each by each other

#Chi2

library(MASS)
rtsat <- table(nomiss$room_type, nomiss$overall_satisfaction)
rtsat
chisq.test(rtsat)
#room type, satisfaction

#Create dummies
nomiss$entirehome <- ifelse(nomiss$room_type=="Entire home/apt", 1, 0)
nomiss$pvtroom <- ifelse(nomiss$room_type=="Private room", 1, 0)
nomiss$shared <- ifelse(nomiss$room_type=="Shared room", 1, 0)

#REGRESSION
#Make sure DV is numeric this way:
#is.factor(nomiss$price)
#nomiss$price<-as.numeric(nomiss$price)

#Equation/command:
pricereg <-lm(price~reviews + overall_satisfaction + bedrooms + accommodates + minstay + pvtroom + shared, data=nomiss)
summary(pricereg)

#How to recode variables - this case, from 1/2 to m/f
my_data_frame$Sex[my_data_frame$Sex==1]<-"Female"
my_data_frame$Sex[my_data_frame$Sex==2]<-"Male"

#Crosstabs
table(pewdata$race)
#Now that we've made tables of each, we can run a crosstab:
table(pewdata$partyln, pewdata$race)
#Save it for later
save(pewdata, file="Pew Data.Rdata")

#Average age by race
aggregate(pewdata$age, by=list(pewdata$race), FUN=mean)
#Average age by both race and party
aggregate(pewdata$age, by=list(pewdata$race, pewdata$partyln), FUN=mean)
#Save this column as a table
age_by_race<-aggregate(pewdata$age, by=list(pewdata$race), FUN=mean)
age_by_race

#MERGING DATA
#can do it in base R, but plyr is better

install.packages("plyr")
library(plyr)

#The command for merging datasets is called "join"
merged_data<-join(age_by_race, race_income_data)
#*just like sql
merged_data
#it didn't work tho (N/As)

colnames(age_by_race)
colnames(race_income_data)
#the problem is that in age_by_race, we don't call it race - called Group.1
colnames(age_by_race)[colnames(age_by_race)=="Group.1"]<-"race"
age_by_race

merged_data<-join(age_by_race, race_income_data)
#Okay, it merged - but only has income value for whites
