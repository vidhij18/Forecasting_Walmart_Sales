## RMSSE Custom Function ####

rmsse = function(actual, predicted, train){
  num = 0
  for (i in 1:length(actual)){
    num = num + (actual[i] - predicted[i])**2
  }
  num = num/length(actual)
  den = 0
  for (i in 2:length(train)){
    den = den + (train[i] - train[i-1])**2
  }
  den = den/(length(train)-1)
  return((num/den)**0.5)
}



library(fpp)
library(fpp3)
library(lubridate)

data = read.csv(file = 'C:/Users/Abhinav Singh/OneDrive/Desktop/Term 1/Forecasting and Predictive Analytics/Project and data-20211130/Projectdata.csv',header=TRUE,sep=",")
head(data)
data$Ã¯..Date = as.Date(data$Ã¯..Date, "%m/%d/%Y")

HobbiesCA1 = data[c('Ã¯..Date', 'Hobbies_CA_1')]
head(HobbiesCA1)
plot(HobbiesCA1)
print(data$Ã¯..Date[1])

HobbiesCA1_ts = ts(data$Hobbies_CA_1, frequency = 365, start = c(2011, 1, 29))
print(HobbiesCA1_ts)
autoplot(HobbiesCA1_ts)
acf(HobbiesCA1_ts)
pacf(HobbiesCA1_ts)


training = subset(HobbiesCA1_ts, end = length(HobbiesCA1_ts)-537)
testing = subset(HobbiesCA1_ts, start = length(HobbiesCA1_ts)-536)

length(training)
length(testing)



library(forecast)
#Naive Method
HobbiesCA1_Naive = naive(training)
fit2 = naive(testing, model = HobbiesCA1_Naive)
onestep = fitted(fit2)
print(onestep[1])
length(fit2)

#sNaive Method
HobbiesCA1_sNaive = snaive(trainig)

#Exponential Smoothing
HobbiesCA1_ES = ses(training, alpha = 0.2)


#Moving Average
HobbiesCA1_MA = Arima(training, order = c(0,0,1))
fit2 = Arima(testing, model = HobbiesCA1_MA)
onestep = fitted(fit2)
length(training)
print(onestep[1:5])

loss = rmsse(testing, onestep, training)
print(loss)


#ES with Explonatory Variable 
HobbiesCA1_ESX =
  
  #TBATS 
  HobbiesCA1_tbats = tbats(training)
fit2 = tbats(testing, model = HobbiesCA1_tbats)
onestep = fitted(fit2)
loss = rmsse(testing, onestep, training)
print(loss)

#Aggregation for Question 4
#At Item Level
data$Hobbies_agg = data$Hobbies_CA_1 + data$Hobbies_CA_2 + data$Hobbies_CA_3
data$Household_1_agg = data$Household_1_CA_1 + data$Household_1_CA_2 + data$Household_1_CA_3
data$Household_2_agg = data$Household_2_CA_1 + data$Household_2_CA_2 + data$Household_2_CA_3
data$Foods_1_agg = data$Foods_1_CA_1 + data$Foods_1_CA_2 + data$Foods_1_CA_3
data$Foods_2_agg = data$Foods_2_CA_1 + data$Foods_2_CA_2 + data$Foods_2_CA_3
data$Foods_3_agg = data$Foods_3_CA_1 + data$Foods_3_CA_2 + data$Foods_3_CA_3

#At Store Level
data$CA_1_agg = data$Hobbies_CA_1 + data$Household_1_CA_1 + data$Household_2_CA_1 + data$Foods_1_CA_1 + data$Foods_2_CA_1 + data$Foods_3_CA_1
data$CA_2_agg = data$Hobbies_CA_2 + data$Household_1_CA_2 + data$Household_2_CA_2 + data$Foods_1_CA_2 + data$Foods_2_CA_2 + data$Foods_3_CA_2
data$CA_3_agg = data$Hobbies_CA_3 + data$Household_1_CA_3 + data$Household_2_CA_3 + data$Foods_1_CA_3 + data$Foods_2_CA_3 + data$Foods_3_CA_3
print(head(data))

#Aggregation for Question 5

#Aggregation of dataframe directly
data$WeekYear = strftime(data$Ã¯..Date, format = "%Y-W%V")

data_WeeklyAggregate = data %>%
  group_by(WeekYear) %>%
  summarise(Hobbies_CA_1_wagg = sum(Hobbies_CA_1), Hobbies_CA_2_wagg = sum(Hobbies_CA_2), Hobbies_CA_3_wagg = sum(Hobbies_CA_3),
            Household_1_CA_1_wagg = sum(Household_1_CA_1), Household_1_CA_2_wagg = sum(Household_1_CA_2), Household_1_CA_3_wagg = sum(Household_1_CA_3),
            Household_2_CA_1_wagg = sum(Household_2_CA_1), Household_2_CA_2_wagg = sum(Household_2_CA_2), Household_2_CA_3_wagg = sum(Household_2_CA_3),
            Foods_1_CA_1_wagg = sum(Foods_1_CA_1), Foods_1_CA_2_wagg = sum(Foods_1_CA_2), Foods_1_CA_3_wagg = sum(Foods_1_CA_3), 
            Foods_2_CA_1_wagg = sum(Foods_2_CA_1), Foods_2_CA_2_wagg = sum(Foods_2_CA_2), Foods_2_CA_3_wagg = sum(Foods_2_CA_3),
            Foods_3_CA_1_wagg = sum(Foods_3_CA_1), Foods_3_CA_2_wagg = sum(Foods_3_CA_2), Foods_3_CA_3_wagg = sum(Foods_3_CA_3))

print(head(data_WeeklyAggregate))












