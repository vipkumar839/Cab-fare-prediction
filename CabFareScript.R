#Including required libraries
library("ggplot2")
library("scales")
library(e1071)
library(randomForest)

#Reading the given data into train_data
train_data=read.csv("train_cab.csv",header=TRUE,sep = ",",na.strings=c(""),stringsAsFactors = FALSE)
test_data=read.csv("test.csv",header=TRUE,sep = ",",na.strings=c(""))

##Missing Value Analysis
#Check if any value is missing in the data.
missinig_value= data.frame(apply(train_data,2,function(x){sum(is.na(x))}))

##Removing rows with missing values
train_data=na.omit(train_data)

str(train_data)
#Removing invalid values from fare_amount and pickup_datetime.
train_data<-train_data[!(train_data$fare_amount=="430-"),]
train_data<-train_data[!(train_data$pickup_datetime=="43"),]

##Converting fare_amount to numeric variable
train_data[1]= sapply(train_data[1],as.numeric)
##Converting pickup_datetime to datetime variable
train_data[2]= sapply(train_data[2],as.POSIXlt)
##Converting fare_amount to integer variable
train_data[7]= sapply(train_data[7],as.integer)

summary(train_data[1])
# We can see that fare_amount variable have min value in negative, which is not possible.
#We know that fare should be greater than zero
train_data<- train_data[!(train_data$fare_amount<=0),]

summary(train_data[7])
##As the cab can be maximum 6 seater 
#Deleting the entry with invalid Number of passengers
train_data<-train_data[!(train_data$passenger_count>6),]

#Outlier Analysis
#Only continuos variables are checked for outliers.
#Saving the names of continuos variables in cnames.
cnames= c('fare_amount','pickup_longitude','pickup_latitude','dropoff_longitude','dropoff_latitude')

#Box plot
for (i in 1:5)
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(train_data))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=3, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i]))
}

## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,ncol=5)

#from the box plots we can clearly conclude that all the variables have outliers.
#Removing outliers

for(i in cnames){
  
  val = train_data[,i][train_data[,i] %in% boxplot.stats(train_data[,i])$out]
  train_data = train_data[which(!train_data[,i] %in% val),]
}


##Feature Engineering
#Extracting new features from given attributes
#Extracting hourand weekday from pickup_datetime
train_data$hour <- train_data$pickup_datetime$hour
train_data$weekday <- train_data$pickup_datetime$wday

distance <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){
  radians <- pi/180
  lat_to <- lat_to * radians
  lat_from <- lat_from * radians
  lon_to <- lon_to * radians
  lon_from <- lon_from * radians
  dLat <- (lat_to - lat_from)
  dLon <- (lon_to - lon_from)
  a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
  return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
}
#Extracting distacne travelled from pickup and dropoff coordinates
train_data$distance <- distance(train_data$pickup_latitude,train_data$pickup_longitude,train_data$dropoff_latitude,train_data$dropoff_longitude)

#Dropping column pickup_datetime as the required information has been extracted from our datetime variable
train_data$pickup_datetime <-NULL

##NORMALISATION
for(i in c('distance','pickup_longitude','pickup_latitude','dropoff_longitude','dropoff_latitude')){
  print(i)
  train_data[,i] = (train_data[,i] - min(train_data[,i]))/
    (max(train_data[,i] - min(train_data[,i])))
}

str(train_data)
#Converting hour and weekday into factor variables
col=c('hour','weekday')
train_data[,col] <- lapply(train_data[,col], factor)


#Dividing Data into training and testing data
train_index = sample(1:nrow(train_data), 0.8 * nrow(train_data))
training = train_data[train_index,]
testing = train_data[-train_index,]

##MODELLING
#Multiple Linear Regression model
model1 = lm(fare_amount ~., data = training)
summary(model1)
predictions_LR = predict(model1, testing[,2:9])

#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}

MAPE(testing[,1], predictions_LR)
#Error Rate=19.07

#Support Vector Regression(eps-regression)
fitepslinear<-svm(fare_amount~., data=training,type="eps-regression",kernel="linear",cross=10)
summary(fitepslinear)
predepsrad<-predict(fitepslinear,testing[,2:9])

MAPE(testing[,1], predepsrad)
#Error Rate 17.61


##Random Forest Regression

RanFor <-  randomForest(fare_amount ~ ., data= training, ntree=1000)
predRanFor<-predict(RanFor,testing[,2:9])

MAPE(testing[,1], predRanFor)
#Error Rate 18.37
