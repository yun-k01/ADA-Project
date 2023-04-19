
'''
library(ggmap)
install.packages('ggmap')
setwd('C:/Users/Administrator/Downloads')

getwd()
bigdata<-read.csv("listings.csv")
tdata<-read.csv("listings.csv")
#vancuver  center lat long, 49.2820 -123.1171
names(bigdata)
selected_vars <- c("host_response_time", "host_is_superhost", "latitude"                                    
                   ,"longitude",
                   "minimum_nights", "price", "availability_365",
                    "instant_bookable", "number_of_reviews")

bigdata<-bigdata[,selected_vars]



# Load necessary packages
library(dplyr)
library(dplyr)
# Define Haversine formula function
deg2rad <- function(degrees) {
  radians <- degrees * pi / 180
  return(radians)
}
haversine <- function(lat1, lon1, lat2, lon2) {
  # Convert latitude and longitude to radians
  lat1 <- deg2rad(lat1)
  lon1 <- deg2rad(lon1)
  lat2 <- deg2rad(lat2)
  lon2 <- deg2rad(lon2)
  # Haversine formula
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2) ^ 2 + cos(lat1) * cos(lat2) * sin(dlon / 2) ^ 2
  c <- 2 * asin(sqrt(a))
  # Radius of earth in kilometers. Use 3956 for miles
  r <- 6371
  # Calculate the distance
  return(c * r)
}

# Example usage
#creating distance variable
# the following point is the center of vancuver downtown
point_lat <- 49.2820
point_lon <- -123.1171
bigdata['distance']<-haversine(bigdata['latitude'], bigdata['longitude'], point_lat, point_lon)
#dummy coding the instant bookable status


bigdata$instat <- ifelse(bigdata$instant_bookable == 't', 1, 0)
bigdata$instaf <- ifelse(bigdata$instant_bookable == 'f', 1, 0)
#remove repetited columns
bigdata[10,]
A<-c(3,4,8)
bigdata<-bigdata[,-A]

#creating dummy variable for response time
bigdata[1,]
bigdata$resp_1<-ifelse(bigdata$host_response_time=='within an hour',1,0)
bigdata$resp_2<-ifelse(bigdata$host_response_time=='within a few hour',1,0)
bigdata$resp_3<-ifelse(bigdata$host_response_time=='within a day',1,0)
bigdata$resp_4<-ifelse(bigdata$host_response_time=='a few day or more',1,0)
bigdata$host_is_superhost<-ifelse(bigdata$host_is_superhost=='t',1,0)
bigdata<-bigdata[,-1]
bigdata
table(bigdata$host_response_time)

bigdata$price <- as.numeric(gsub("\\$", "", bigdata$price))
bigdata[,-1]

'''



getwd()
setwd('C:/Users/Administrator/Downloads')
#write.csv(bigdata, "my_data.csv", row.names = FALSE)
bigdata<-read.csv("my_data.csv")
bigdata <- na.omit(bigdata)
names(bigdata)
library(caret)
train_idx <- createDataPartition(bigdata$host_is_superhost, p = 0.8, list = FALSE, times = 1)

# Split the data into training and testing sets
bigdatan <- bigdata[train_idx, ]
bigdatat <- bigdata[-train_idx, ]



#install.packages("xgboost")
library(xgboost)
y<-bigdatan[,1]
X<-bigdatan[,-1]
Xt<-bigdatat[,-1]
yt<-bigdatat[,1]
nrow(X)



acc<-function(predictions,test_data=bigdatat){
  class_labels <- ifelse(predictions >= 0.5, 1,0)
  num <- mean(class_labels == bigdatat[,1])
  print(num)
  
}
set.seed(123)
XGB<-function(eta=0.1,nrounds){
  
  model <<- xgboost(data = as.matrix(X), label = y, nrounds = nrounds, objective = "reg:logistic",  eta = eta)
  
  y_pred<-predict(model,as.matrix(Xt))
  accuracy<-acc(y_pred)
  return(accuracy)
}
XGB(eta=0.005,nrounds=50)
input_df[1,]
sds<-apply(input_df0.5[1:7,],MARGIN=1,FUN=function(x) XGB(nrounds=x['nrounds'],eta=x['eta']))

sds
input_df0.5
MM<-xgboost(data = as.matrix(X), label = y, nrounds = 30, objective = "reg:logistic",  eta = 0.005)
MMP<-predict(MM,as.matrix(Xt))
acc(MMP)
#eta=seq(0.01,0.35, by=0.02)
#nrounds=seq(300,2000,by=100)
#first try, the best was 0.77115 with eta=0.03 nrounds=800

eta=seq(0.015,0.045,by=0.005)
nrounds=seq(760,860,by=20)
input_df <- expand.grid(eta = eta, nrounds = nrounds)

#input_df$VALUE<-apply(input_df, MARGIN=1,FUN=function(x) XGB(nrounds=x['nrounds'],eta=x['eta']))
#input_df[which.max(input_df$VALUE),]
#input_df
#Conclusion, 0.03, with 780 rounds produces the highest accuracy 0.7729297

XGB(0.03,780)
#sample to find out the reason for repeated accocy
#!!!!!!!!!!!!!!!!!!!!!!!!!
Table<-xgb.importance(model=model)
write.csv(Table,'Table.csv')
xgb.importance(model=model)
#from xgb.importance we can see the rank of the most influencial features when 
#making the classiccation
library(dplyr)
#num of reviews
bigdata$host_is_superhost<-factor(bigdata$host_is_superhost)
bigdata%>%
  group_by(host_is_superhost)%>%
  ggplot(aes(number_of_reviews,color=host_is_superhost))+
  geom_histogram()
bigdata%>%
  group_by(host_is_superhost)%>%
  ggplot(aes(number_of_reviews,color=host_is_superhost))+
  geom_density()

#distance

bigdata%>%
  group_by(host_is_superhost)%>%
  ggplot(aes(distance,color=host_is_superhost))+
  geom_histogram()

  is<-filter(bigdata,host_is_superhost==1)
  isnot<-filter(bigdata,host_is_superhost==0)
mean(is$distance)
mean(isnot$distance)
#since there isnt anydifference in distribution of distance, maybe it is used with the
#number of review to determinine superhost status

is<-filter(bigdata,host_is_superhost==1)
isnot<-filter(bigdata,host_is_superhost==0)


mean(is$number_of_reviews)
#63
mean(isnot$number_of_reviews)
#25.64


bigdata%>%
  group_by(host_is_superhost)%>%
  ggplot(aes(distance,number_of_reviews,color=host_is_superhost))+
  geom_smooth()

#avaliability
bigdata$availability_365

bigdata1<-(filter(bigdata,availability_365>0))
#removing bizarad 0 availability lists.
is<-filter(bigdata1,host_is_superhost==1)
isnot<-filter(bigdata1,host_is_superhost==0)

1-mean(is$availability_365)/mean(isnot$availability_365)
#11 percent more booking rate

#resp_1
is<-filter(bigdata,host_is_superhost==1)
isnot<-filter(bigdata,host_is_superhost==0)
mean(is$resp_1)/mean(isnot$resp_1)
#36% more response on first hour.

#minimum night

mean(is$minimum_nights)
#12.57
mean(isnot$minimum_nights)
#19.45

19.45/12.57
names(bigdatan)
#================================================================================
#Next model
library(e1071)

# Train the SVM model on the training set
?svm()
svm_model <- svm(host_is_superhost ~ ., data = bigdatan,type='C-classification')
svm_model$coefs
summary(svm_model)


# Make predictions on the testing set
predictions2 <- predict(svm_model, bigdatat)
mean(predictions2==bigdatat[,1])
acc(predictions2)#0.6545

#===============================================
# Set the parameters to tune
C_values <- c(0.1, 1, 10)
kernel_types <- c("linear", "polynomial", "radial", "sigmoid")
degree_values <- c(2, 3, 4)
gamma_values <- c(0.1, 1, 10)

# Create a function that returns the accuracy of SVM with given parameters
svm_accuracy <- function(C, kernel_type, degree, gamma){
  svm_model <- svm(host_is_superhost ~ ., data = bigdatan, cost = C, kernel = kernel_type, degree = degree, gamma = gamma )
  pred <- predict(svm_model, bigdatat)
  accu<-acc(pred)
  return(accu)
}

# Create a parameter grid
input_df2 <- expand.grid(C = C_values, kernel_type = kernel_types, degree = degree_values, gamma = gamma_values)

svm(host_is_superhost ~ ., data = bigdatan ,, kernel = 'linear',max.iter=100)

input_df2$accuracy<-apply(input_df,MARGIN=1,FUN=function(x) svm_accuracy(C=x['C'],kernel_type=x['kernel_type'],degree=x['degree'],gamma=x['gamma']))

input_df2







library(randomForest)

rf_model <- randomForest(host_is_superhost ~ ., data = bigdatan, ntree = 2000, mtry = 3, type='classification')

rf_model$importance
# Make predictions on the test set
predictions <- predict(rf_model, newdata = bigdatat[,-1])

# Evaluate the performance of the model
acc(predictions)
#=============================================================
n_trees <- seq(50,2500,by=100)
m_try <- c(2, 3)
nodesize <- c(1,3, 5, 10)

# Create a function that returns the accuracy of random forest with given parameters
rf_accuracy <- function(n_trees, m_try, nodesize){
  rf_model <- randomForest(host_is_superhost ~ ., data = bigdatan, ntree = n_trees, mtry = m_try, nodesize = nodesize)
  pred <- predict(rf_model, x)
  acc<-acc(pred)
  return(acc)
}

bigdatan
# Create a parameter grid
input_df3 <- expand.grid(n_trees = n_trees, m_try = m_try, nodesize = nodesize)


input_df3$acc<-apply(input_df3,MARGIN=1,FUN= function(x) rf_accuracy(x['n_trees'],x['m_try'],x['nodesize']))





#0.7684Rdf
#0.7476 xgb, 0.6545 svm




