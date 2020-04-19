###############################################################################################
#Programmer <- Ajeet Pandey                                                                   #
#Problem Statement -->                                                                        #
#Predicting Losses of Car Insurance Company by its historical data                            #
#The data contains demographic information and the losses incurred by the insurance company.  #
#objective is to predict those losses for future clients using their demographic information  #
###############################################################################################

#####	First Code and Data Import

#Set the working directory
setwd("D:/r shiny/code/") #Make sure all the slashes are forward

#Import the raw data file (csv) into R format of file
IncData <- read.csv("Inc_Data_Lin_Reg_R.csv",header = TRUE ,sep = ',') 

#####	Data Exploration
#Check if the data has been imported fine
head(IncData) #Prints few top rows of the data
tail(IncData) #Prints few bottom rows of the data

#Test for Outliers in the dependent variable
quantile(IncData$Losses,c(0,.01,0.05,0.25,0.5,0.75,.99,1)) 
# Plot Percentiles of the variable to check for outliers

# Descriptive Statistics of all variables
summary(IncData) #Prints important descriptive statistics of all variables


#####	Data Manipulation and Data Export

#Cap Losses to treat Outliers, creating new variable Capped_Losses 
IncData$Capped_Losses <- ifelse(IncData$Losses > 1204,1204,ifelse(IncData$Losses < 32,32, IncData$Losses))

quantile(IncData$Capped_Losses,c(0,.01,0.05,0.5,0.75,.99,1)) # Check he new distribution of Losses


#Transforming Categorical Variables into Dummy numeric variables

IncData$Gender_Dummy <- ifelse(IncData$Gender == "M",1,0)
IncData$Married_Dummy <- ifelse(IncData$Married == "Married",1,0)
IncData$Fuel_Dummy <- ifelse(IncData$Fuel.Type == "P",1,0)

summary(IncData)


#####	Bi-Variate Plots to understand independent variables
#Create a new data set "tempdata" with the average of Capped_Losses for each age point
tempdata<-aggregate(IncData$Capped_Losses,list(Age = IncData$Age),mean) 

head(tempdata) #Check details of tempdata
tail(tempdata)  #As age Increase Losses Decrease
summary(tempdata) #Check details of tempdata

attach(tempdata) #Attach tempdata to memory for plotting charts

plot(Age,x,type = 'o') 
#Plot the chart with Age of x-axis and Capped_Losses (denoted by 'x') on y-axis. Chart type 'o' denotes overplotted points and lines
detach(tempdata)


#####	Write a Macro function to plot Bi-Variate Plots for all independent variables

BiVar <- function(var){
  newdataset <- data.frame(tempvar = IncData[,var],Capped_losses = IncData$Capped_Losses)
 # tempdata<-aggregate(IncData$Capped_Losses,list(Age = IncData$Age),mean) 
  tempplotdata <- aggregate(newdataset$Capped_losses,list(varnew = newdataset$tempvar),mean)
  attach(tempplotdata)
  plot(varnew,x,type = 'o')
  detach(tempplotdata)
}

BiVar("Age")
BiVar("Gender_Dummy")
BiVar("Married_Dummy")
BiVar("Fuel_Dummy")
BiVar("Vehicle.Age")
BiVar("Years.of.Driving.Experience")


#####	Plot a correlation matrix among independent variables to check for multi colinearity

#cor(IncData) # Plot correlation matrix to see if any variables are correlated with each other. But, this will throw an ERROR since all variables are not numeric

cor(IncData[sapply(IncData, is.numeric)]) #Plot correlation for only numberice variables. sapply function separates the numeric columns in the dataset



library(car)

#####	Fit the regression equation

fit <- lm(Capped_Losses ~ Age + Years.of.Driving.Experience + Number.of.Vehicles + Vehicle.Age + Gender_Dummy + Married_Dummy + Fuel_Dummy,data=IncData)
# lm function fits the regression equation using the dependent and independent variables mentioned. The regression output is stored in fit

vif(fit) #vif funtion computes the Variance Inflation Factor to understand multicolinearlity among variables.
#Please note this function requires the package "car" to be loaded. Use <<library(car)>> if you get an error

summary(fit) #summary function provides all the regression results

pred <- predict(fit, type = "response") # store predicted values
resi <- residuals(fit, type = "deviance") # store residuals or errors

plot(IncData$Capped_Losses,pred) # Plot Actual v/s predicted values
plot(IncData$Capped_Losses,resi) # Plot Actual v/s Error term

newdata <- data.frame(IncData$Policy.Number,IncData$Capped_Losses,pred) #Create a new dataset with the Acutal, predicted values and policy number
write.table(newdata, "newdata.csv", sep = ",") # Export data the predicted data in CSV