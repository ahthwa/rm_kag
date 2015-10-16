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

# Train, Test Data
## Store: 1115개, 856개
length(unique(train$Store))
length(unique(test$Store))
length(intersect(unique(test$Store), unique(train$Store)))

## 누락된 날짜가 있는가
## 942일, 48일
max(train$Date) - min(train$Date) + 1
length(unique(train$Date))
max(test$Date) - min(test$Date) + 1
length(unique(test$Date))


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
## sales 0인 데이터가 있음.
ggplot(train) + geom_histogram(aes(x = Sales), binwidth=500)
## sales 0제외 데이터
ggplot(train[Sales != 0]) + geom_histogram(aes(x = Sales), binwidth=500)
## 상점별 평균 Sales
ggplot(train[Sales != 0, .(meanSales = mean(Sales)), by = (Store)]) +
  geom_histogram(aes(x = meanSales), binwidth = 500)
## 요일별 sales
## 요일 7의 세일즈가 극히 낮음. 일요일인가?
ggplot(train[Sales != 0, .(sumSales = sum(Sales)), by = (DayOfWeek)]) +
  geom_bar(aes(x = factor(DayOfWeek), y = sumSales), stat="identity")
ggplot(train[Sales != 0 & Open == 1, .(sumSales = sum(Sales)), by = (DayOfWeek)]) +
  geom_bar(aes(x = factor(DayOfWeek), y = sumSales), stat="identity")

## Customer
## customer 0 인 데이터 다수 있음.
ggplot(train) + geom_histogram(aes(x = Customers), binwidth=100)
## Customer 0 제외
ggplot(train[Customers !=0, ]) + geom_histogram(aes(x = Customers), binwidth=100)
## 상점별 평균 Customer
ggplot(train[Customers != 0, .(meanCustomer = mean(Customers)), by =.(Store)]) +
  geom_histogram(aes(x = meanCustomer), binwidth=100)
## 요일별 Customer
ggplot(train[Customers != 0, .(sumCustomer = sum(Customers)), by = .(DayOfWeek)]) +
  geom_bar(aes(x = factor(DayOfWeek), y = sumCustomer), stat = 'identity')
## Customer - Sales
ggplot(train[Customers !=0 & Sales != 0]) + 
  geom_point(aes(x = Customers, y = Sales))

theme_set( theme_bw( base_family = "NanumGothic" ) )
ggplot(train[Customers != 0 & Sales != 0]) +
  ggtitle("고객-매출") +
  geom_point(aes(x = log(Customers), y = log(Sales))) +
  geom_smooth(aes(x = log(Customers), y = log(Sales)))


## Open
nrow(train)
table(train$Open) / nrow(train)
ggplot(train[, .(storeMeanSales = mean(Sales)), by = .(Open, Store)]) +
  geom_histogram(aes(x = storeMeanSales, fill = as.factor(Open)), binwidth = 500)
ggplot(train[, .(storeMeanCustomers = mean(Customers)), by = .(Open, Store)]) +
  geom_histogram(aes(x = storeMeanCustomers, fill = as.factor(Open)),
                 binwidth = 100, position = "identity", alpha = 0.7)

## Promo
table(train$Promo)/ nrow(train)
ggplot(train[, .(storeMeanSales = mean(Sales)), by = .(Promo, Store)]) +
  geom_histogram(aes(x = storeMeanSales, fill = as.factor(Promo)), binwidth = 500,
                 position = "identity", alpha = 0.7)
ggplot(train[, .(storeMeanCustomers = mean(Customers)), by = .(Promo, Store)]) +
  geom_histogram(aes(x = storeMeanCustomers, fill = as.factor(Promo)),
                 binwidth = 100, position = "identity", alpha = 0.7)

## StateHoliday
table(train$StateHoliday)/ nrow(train)
ggplot(train[, .(storeMeanSales = mean(Sales)), by = .(StateHoliday, Store)]) +
  geom_histogram(aes(x = storeMeanSales, fill = as.factor(StateHoliday)), binwidth = 1000,
                 position = "identity", alpha = 0.7)
ggplot(train[, .(storeMeanCustomers = mean(Customers)), by = .(StateHoliday, Store)]) +
  geom_histogram(aes(x = storeMeanCustomers, fill = as.factor(StateHoliday)),
                 binwidth = 100, position = "identity", alpha = 0.7)
ggplot(train[Sales != 0]) + geom_boxplot(aes(x = as.factor(StateHoliday), y = Sales), position = "identity", alpha = 0.7)
train[Sales != 0, .(storeMeanSales = mean(Sales)), by = .(StateHoliday)]

## SchoolHoliday
table(train$SchoolHoliday)/ nrow(train)
ggplot(train[, .(storeMeanSales = mean(Sales)), by = .(SchoolHoliday, Store)]) +
  geom_histogram(aes(x = storeMeanSales, fill = as.factor(SchoolHoliday)), binwidth = 500,
                 position = "identity", alpha = 0.7)
ggplot(train[, .(storeMeanCustomers = mean(Customers)), by = .(SchoolHoliday, Store)]) +
  geom_histogram(aes(x = storeMeanCustomers, fill = as.factor(SchoolHoliday)),
                 binwidth = 100, position = "identity", alpha = 0.7)
ggplot(train[Sales != 0]) + geom_boxplot(aes(x = as.factor(SchoolHoliday), y = Sales), position = "identity", alpha = 0.7)
train[Sales != 0, .(storeMeanSales = mean(Sales)), by = .(SchoolHoliday)]

## ----
  
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


