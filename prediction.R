library("data.table")
library("ggplot2")
library("zoo")
library(reshape)

##

summary(train_store)

ggplot(train_store[Open != 0 & (Store == "1" | Store == "500" | Store == "300" | Store == "800"  | Store == "1000")], aes(x = Date, y = Sales, color = as.factor(Store))) +
  geom_point() + geom_smooth()

## regression

?glm
model = glm(Sales ~ as.factor(Store) + as.factor(DayOfWeek) + Open + Promo + StateHoliday + SchoolHoliday + StoreType + Assortment + CompetitionDistance + Promo2, train_store, family = gaussian)

model = lm(Sales ~ . - Date, data=train_store, family = gaussian)

model

summary(model)

summary(train_store)

## Linear Model Learning
## from https://www.kaggle.com/c/rossmann-store-sales/forums/t/17038/linear-regression-model-field-not-present-in-test-set
## Caret package의 trainControl과 train 함수.
install.packages("caret")
library(caret)

ctrl <- trainControl(method = "cv",number = 10)
mod1 <- train(Sales ~ . - Date ,data = train_store,method = "lm",
              trControl = ctrl, metric="Rsquared")
summary(mod1)

ctrl <- trainControl(method = "cv",number = 10)
mod2 <- train(Sales ~ . , data = train_store,method = "lm",
              trControl = ctrl, metric="Rsquared")
summary(mod2)

?train

## CausalImpact package를 이용한 time series forecasting
## http://datum.io/time-series-prediction-with-r-and-google/
pre.peroid <- c("2013-01-01", "2015-05-31")
post.peroid <- c("2015-06-01", "2015-07-31")
impact <- CausalImpact(train_store, pre.period, post.period)

