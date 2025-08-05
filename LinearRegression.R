##Data preparation
data<-read.csv("HousePricePrediction.csv")
head(data)

dim(data)

##Data Cleaning

##Create a user defined to find the missing values

getMissingValues <- function(data){
  missing_values <- colSums(is.na(data))
  sorted_missing_values <- sort(missing_values, decreasing = TRUE)
  for(col_name in names(sorted_missing_values)) {
    cat("column: ", col_name, "\tMissing Values:",sorted_missing_values[col_name],"\n")
  }
}
getMissingValues(data)

##Omit the missing values
data <- na.omit(data)
sum(is.na(data))

##Since Id Field has no impact on the house price we remove it.
data <- subset(data, select = -Id)
colnames(data)

##Performing Label Encoding

data$MSZoning <- as.numeric(factor(data$MSZoning))
data$LotConfig <- as.numeric(factor(data$LotConfig))
data$BldgType <- as.numeric(factor(data$BldgType))
data$Exterior1st <- as.numeric(factor(data$Exterior1st))
subset(data, select = c('MSZoning', 'LotConfig', 'BldgType', 'Exterior1st'))

library(ggplot2)
library(ggcorrplot)

ggcorrplot(cor(data), method = "square", hc.order = TRUE,lab = TRUE, colors = c("purple","skyblue","brown"), outline.color = "red")


library(ggplot2)

ggplot(data, aes(x = SalePrice))+
  geom_histogram(binwidth = 50000, fill="purple", color='red')+
  labs(title = "Distribution of Price", x ="Price")+
  theme_minimal()

##Plotting the Regression Line using "lm" function
ggplot(data, aes(x = LotArea, y = SalePrice))+
  geom_point(color = "orange")+
  theme_minimal()+
  labs(title = "ScatterPlot: Lot area vs. Sale Price", x= "Lotarea", y="Saleprice")+
  geom_smooth(method = lm)

##Splitting data into train and test
set.seed(123)
train_proportion <- 0.8
train_indices <- sample(1:nrow(data),size = round(train_proportion*nrow(data)))

train_Data <- data[train_indices,]
test_Data <- data[-train_indices,]

dim(test_Data)
dim(train_Data)

##Train a Multiple Linear Regression Model on the train data
model = lm(SalePrice ~ ., data=train_Data)
summary(model)

##Model Prediction:
predictions <- predict(model, newdata = test_Data)
head(predictions)

##Predicting on Sample Data:
  test_case <- data.frame(MSSubClass = 60,MSZoning = 4,LotArea = 8450,
                          LotConfig = 5, BldgType = 1, OverallCond = 5,
                          YearBuilt = 2003, YearRemodAdd = 2003,
                          Exterior1st = 13, BsmtFinSF2 = 0,TotalBsmtSF = 856)
predicted_price <- predict(model, newdata = test_case)
print(predicted_price)