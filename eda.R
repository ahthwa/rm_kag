library("data.table")
library("ggplot2")
library("zoo")

#https://www.kaggle.com/thie1e/rossmann-store-sales/exploratory-analysis-rossmann/notebook 세세한 팁. 이거 보고 배울 것.

train <- fread("data/train.csv", sep=",", header = T)
test <- fread("data/test.csv", sep=",", header = T)
store <- fread("data/store.csv", sep=",", header = T)

train$Date = as.Date(train$Date)
test$Date = as.Date(test$Date)

summary(train)
summary(test)
summary(store)

head(train)
head(store)

# 날짜순으로 Train, Test 정렬
train <- train[order(train$Date)] 
test <- test[order(test$Date)]

# Train Data
## Store
length(unique(train$Store))

## 누락된 날짜가 있는가
max(train$Date) - min(train$Date) + 1
length(unique(train$Date))

## 날짜 순 정렬이 잘 되었나
plot(train$Date, type = 'l')
plot(test$Date, type='l')

## 일별 데이터 건수의 차이가 있는가
## 2014년 7월부터 2015년 1월 까지 데이터 양이 적다.
ggplot(train) + geom_histogram(aes(x = Date))
ggplot(test) + geom_histogram(aes(x = Date))

## 요일별 데이터 건수
table(train$DayOfWeek)
table(train$DayOfWeek) / nrow(train)
table(test$DayOfWeek)
table(test$DayOfWeek) / nrow(test)

## Sales
## sales 0인 애들이 꽤 있음.
ggplot(train) + geom_histogram(aes(x = Sales), binwidth=500)
## sales 0빼고
ggplot(train[Sales != 0]) + geom_histogram(aes(x = Sales), binwidth=500)
## 상점별 평균 Sales
ggplot(train[Sales != 0, .(meanSales = mean(Sales)), by = (Store)]) +
  geom_histogram(aes(x = meanSales), binwidth = 500)
## 요일별 sales
ggplot(train[Sales != 0, .(sumSales = sum(Sales)), by = (DayOfWeek)]) +
  geom_bar(aes(x = factor(DayOfWeek), y = sumSales))

train[Sales != 0, .(sumSales = sum(Sales)), by = (DayOfWeek)]

ggplot(train) + geom_bar(aes(x = factor(DayOfWeek), y = Sales, fill = DayOfWeek))

ggplot(train, aes(x=factor(1), y=sales, fill = schoolholiday)) + 
  geom_bar(width = 1, stat="identity") + coord_polar(theta="y")




# Store
# Store별 로그수, 매출 휴일 등은 원래 다를것임.
length(unique(train$Store))

## Sales 분포
ggplot(data = train_data) + geom_histogram(aes(x = Sales), binwidth=500)
ggplot(data = train_data[Sales < 500,]) + geom_histogram(aes(x = Sales), binwidth=10) # Sale 0인 상점-날짜가 꽤 있음.

# DayOfWeek 분포
# Day of week == 7 일때 Sales median ~= 0 임.
ggplot(data = train_data) + geom_boxplot(aes(x = as.factor(DayOfWeek), y = Sales))

# Date min/max
min(train_data$Date) # 2013-01-01
max(train_data$Date) # 2013-07-31

# Customer 분포
ggplot(data = train_data) + geom_histogram(aes(x = Customers), binwidth=100)

# Customer ~ Sales
# 우상향 plot. 종모양으로 흩뿌려짐.
ggplot(data = train_data, aes(x = Customers, y = Sales, colours(SchoolHoliday))) + geom_point()

# Open, Promo, StateHoliday, SchoolHoliday
open_summary = train_data[, .(sales = sum(as.numeric(Sales)), mean_sales = mean(as.numeric(Sales)), customers = sum(as.numeric(Customers)), mean_customers = mean(as.numeric(Customers))), by = (open = factor(Open))]
ggplot(open_summary, aes(x=factor(1), y=sales, fill = factor(open))) + 
  geom_bar(width = 1, stat="identity")

promo_summary = train_data[, .(sales = sum(as.numeric(Sales)), mean_sales = mean(as.numeric(Sales)), customers = sum(as.numeric(Customers)), mean_customers = mean(as.numeric(Customers))), by = (promo = factor(Promo))]
ggplot(promo_summary, aes(x=factor(1), y=sales, fill = promo)) + 
  geom_bar(width = 1, stat="identity") + coord_polar(theta="y")

# Holiday sales의 절대양은 적지만, mean_sales는 상대적으로 높다. 그렇다고 평일보다 평균이 높다는 것은 아니다.
stateholiday_summary = train_data[, .(sales = sum(as.numeric(Sales)), mean_sales = mean(as.numeric(Sales)), customers = sum(as.numeric(Customers)), mean_customers = mean(as.numeric(Customers))), by = (stateholiday = factor(StateHoliday))]
ggplot(stateholiday_summary, aes(x=factor(1), y=sales, fill = stateholiday)) + 
  geom_bar(width = 1, stat="identity") + coord_polar(theta="y")
ggplot(stateholiday_summary, aes(x=factor(1), y=mean_sales, fill = stateholiday)) + 
  geom_bar(width = 1, stat="identity") + coord_polar(theta="y")
stateholiday_summary

# School Holiday는 mean_sales가 높다.
schoolholiday_summary = train_data[, .(sales = sum(as.numeric(Sales)), mean_sales = mean(as.numeric(Sales)), customers = sum(as.numeric(Customers)), mean_customers = mean(as.numeric(Customers))), by = (schoolholiday = factor(SchoolHoliday))]
ggplot(schoolholiday_summary, aes(x=factor(1), y=sales, fill = schoolholiday)) + 
  geom_bar(width = 1, stat="identity") + coord_polar(theta="y")
ggplot(schoolholiday_summary, aes(x=factor(1), y=mean_sales, fill = schoolholiday)) + 
  geom_bar(width = 1, stat="identity") + coord_polar(theta="y")
schoolholiday_summary

# Store 정보와 결합.
# Competition year/month, Promo2 year/week, PromoInterval을 join data의 각 row에 적용하도록 코딩할 것.


