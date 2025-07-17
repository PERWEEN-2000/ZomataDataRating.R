################## RESTAURANT RATING PREDICTION CASE STUDY##############
options(scipen = 100000000)
path<-("C:/Users/LENOVO/Downloads/Python IVY Feb-2024/Python IVY Feb-2024/Python Datasets/Python Datasets/Regression Datasets")
setwd(path)
getwd()

#Getting the data
install.packages("readxl")
library(readxl)
zomato<-read.csv("ZomatoData.csv",encoding= "LATIN-1")
head(zomato)

#Exploring the data
str(zomato) #RestaurantID, name, address, locality , locality verbose are qualitative data
summary(zomato)

#Exploring Multiple Continuous variables
contcols<-c("Longitude","Latitude","Votes","Average.Cost.for.two","Rating")#Rating is the Target Variabel!!
par(mfrow=c(2,3))

# library to generate professional colors
install.packages('RColorBrewer')
library(RColorBrewer)
for (col in contcols){
  hist(zomato[,c(col)],main=paste("The histogram of: ", col), col=brewer.pal(8,"Paired") )
}

#Outliers seen in votes and average cost for two
#for "Votes"
outliesr<-sum(zomato$Votes>4000)
#Sorting to get the nearest value to 4000
head(sort(decreasing = TRUE, zomato$Votes[zomato$Votes<=4000]),10)
zomato$Votes[zomato$Votes>4000]<-3986

#for "Average CostFor Two"
outliers<-sum(zomato$Average.Cost.for.two>50000)
##Sorting to get the nearest value to 50000
head(sort(decreasing = TRUE, zomato$Average.Cost.for.two[zomato$Average.Cost.for.two<=50000]),10)
#Substituting the values greater than 50000 with 8000
zomato$Average.Cost.for.two[zomato$Average.Cost.for.two>50000]<-8000

#Visualizing again the Votes and average cost for two column
cols<-c("Votes","Average.Cost.for.two")
par(mfrow=c(2,2))
for (col in cols){
  hist(zomato[,c(col)],main=paste("The histogram of: ", col), col=brewer.pal(8,"Paired") )
}

#For now all the columns are selected

#Exploring Multiple Categorical variables
par(mfrow=c(2,2))
catcol<-c("Country.Code","City","Currency ","Has.Table.booking")
catcol<-c("Has.Online.delivery","Is.delivering.now","Switch.to.order.menu","Price.range")
for (col in catcol){
  barplot(table(zomato[,c(col)]), main=paste('Barplot of:', col), 
          col=brewer.pal(8,"Paired"))
}

barplot(table(zomato$City))
#from the bar plot it is evident that country code, city, is delivering now, switch to order menu 
#are not useful for the model. since one category is dominant and there are not enough data available

-------------------------------------------------------------------------------------------------
  #Exploring relationship between Continuous and Continuous Variable
  cols<-c("Rating","Votes","Average.Cost.for.two","Latitude","Longitude")
plot(zomato[,cols], col="blue")

#Correlation Analysis between target and continuous independant variables
cols<-c("Rating","Votes","Average.Cost.for.two","Latitude","Longitude")
CorrData<-cor(zomato[,cols], use="complete.obs")

#Selcting the variable which have corr value >0.5, if not then reducing the threshold to 0.2
names(CorrData[,'Rating'][abs(CorrData[,'Rating'])>0.2])
----------------------------------------------------------------------------------------------
  #Boxplot to explore the relation bwteen the target and categorical variable
  catcol<-c("Has.Table.booking","Has.Online.delivery","Price.range")

for (cols in catcol){
  boxplot(Rating~zomato[[cols]], data = zomato,main=paste("The boxplot for: ", cols), col=brewer.pal(8,"Paired"))
}

#Exploring relationship between Continuous and Categorical Variable- ANOVA Test
catcol<-c("Has.Table.booking","Has.Online.delivery","Price.range")

#We need to reject the H0, the p-value <0.05
for (col in catcol){
  anovadata=zomato[,c("Rating", col)]
  print(str(anovadata))
  print(summary(aov(Rating~., anovadata)))
}
#ALL the three columns are selected
--------------------------------------------------------------
  #Checking and treating missing values
  colSums(is.na(zomato)) #No missing values found

# Generating the Data frame for machine learning
inpdata<-zomato
Targetname<-c("Rating")

# Choosing multiple Predictors which may have relation with Target Variable
# Based on the exploratory data analysis
predictorname<-c("Has.Table.booking","Has.Online.delivery","Price.range","Votes",
                 "Average.Cost.for.two")
target<-inpdata[, Targetname]
predictor<-inpdata[, predictorname]
str(predictor)

DataForML=data.frame(target,predictor)
str(DataForML)
head(DataForML)

DataForML$Has.Online.delivery=as.factor(DataForML$Has.Online.delivery)
DataForML$Has.Table.booking=as.factor(DataForML$Has.Table.booking)
str(DataForML)
------------------------------------------------------------------
  set.seed(989)
install.packages("caTools")
library(caTools)
spl=sample.split(DataForML$target, 0.7)

train_ml<-subset(DataForML, spl==TRUE) #traing 70% data
test_ml<- subset(DataForML, spl==FALSE) #testing 30% data
str(train_ml)
str(test_ml)
dim(train_ml)
dim(test_ml)
------------------------------------------------------------
  lm0<-lm(target~., data=train_ml)
summary(lm0)

hist(lm0$residuals)

#Predicting the value on test data
test_ml$Pred_Rating=predict(lm0, test_ml)
head(test_ml, 10)

#Calculating MAPE
APE<-100*(abs(test_ml$Pred_Rating - test_ml$target)/test_ml$target)
test_ml$APE<-APE
print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - mean(APE)))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - median(APE)))