#install.packages("lubridate")
#install.packages("ggfortify")

install.packages("smooth")
install.packages("greybox")
install.packages("Mcomp")


library(lubridate)
library(ggfortify)
library(xts)
library(smooth)
library(greybox)
library(Mcomp)

#data = read.csv('C:/Users/Aashima/T1/Forecasting/Project Material/Projectdata.csv', header=TRUE,sep=",")

data = read.csv('C:/Users/Abhinav Singh/OneDrive/Desktop/Term 1/Forecasting and Predictive Analytics/Project and data-20211130/Projectdata.csv', header=TRUE,sep=",")
print(head(data))

data$ï..Date = as.Date(data$ï..Date, "%m/%d/%Y")

#calender = read.csv("C:/Users/Aashima/T1/Forecasting/Project Material/calendar.csv",header=TRUE,sep=",")
#Calender File for Exogenous Variables
calender = read.csv(file = 'C:/Users/Abhinav Singh/OneDrive/Desktop/Term 1/Forecasting and Predictive Analytics/Project and data-20211130/calendar.csv',header=TRUE,sep=",")
tail(calender)
calender$IsEvent[calender$event_name_1 == ""] = 0
calender$IsEvent[calender$event_name_1 != ""] = 1

x_reg = cbind(calender$snap_CA, calender$IsEvent)

#Store 1
#Hobbies_CA_1
data2 = data.frame(data[1],data[2]) 
Hobbies_CA_1 = ts(data$Hobbies_CA_1, frequency = 365, start = c(2011, 1, 29))
acf(Hobbies_CA_1)

#Household_1_CA_1
Household_1_CA_1 = ts(data$Household_1_CA_1, frequency = 365, start = c(2011, 1, 29))
acf(Household_1_CA_1)

#Household_2_CA_1
Household_2_CA_1 = ts(data$Household_2_CA_1, frequency = 365, start = c(2011, 1, 29))
acf(Household_2_CA_1)

#Foods_1_CA_1
Foods_1_CA_1 = ts(data$Foods_1_CA_1, frequency = 365, start = c(2011, 1, 29))
acf(Foods_1_CA_1)

#Foods_2_CA_1
Foods_2_CA_1 = ts(data$Foods_2_CA_1, frequency = 365, start = c(2011, 1, 29))
acf(Foods_2_CA_1)

#Foods_3_CA_1
Foods_3_CA_1 =  ts(data$Foods_3_CA_1, frequency = 365, start = c(2011, 1, 29))
acf(Foods_3_CA_1)


#Store 2
#Hobbies_CA_2
Hobbies_CA_2 =ts(data$Hobbies_CA_2, frequency = 365, start = c(2011, 1, 29))
acf(Hobbies_CA_2)

#Household_1_CA_2
Household_1_CA_2 = ts(data$Household_1_CA_2, frequency = 365, start = c(2011, 1, 29))
acf(Household_1_CA_2)

#Household_2_CA_2
Household_2_CA_2 = ts(data$Household_2_CA_2, frequency = 365, start = c(2011, 1, 29))
acf(Household_2_CA_2)

#Foods_1_CA_2
Foods_1_CA_2 = ts(data$Foods_1_CA_2, frequency = 365, start = c(2011, 1, 29))
acf(Foods_1_CA_2)

#Foods_2_CA_2
Foods_2_CA_2 = ts(data$Foods_2_CA_2, frequency = 365, start = c(2011, 1, 29))
acf(Foods_2_CA_2)

#Foods_3_CA_2
Foods_3_CA_2 = ts(data$Foods_3_CA_2, frequency = 365, start = c(2011, 1, 29))
acf(Foods_3_CA_2)


#Store 3
#Hobbies_CA_3
Hobbies_CA_3 = ts(data$Hobbies_CA_3, frequency = 365, start = c(2011, 1, 29))
acf(Hobbies_CA_3)

#Household_1_CA_3
Household_1_CA_3 = ts(data$Household_1_CA_3, frequency = 365, start = c(2011, 1, 29))
acf(Household_1_CA_3)

#Household_2_CA_3
Household_2_CA_3 = ts(data$Household_2_CA_3, frequency = 365, start = c(2011, 1, 29))
acf(Household_2_CA_3)

#Foods_1_CA_3
Foods_1_CA_3 = ts(data$Foods_1_CA_3, frequency = 365, start = c(2011, 1, 29))
acf(Foods_1_CA_3)

#Foods_2_CA_3
Foods_2_CA_3 = ts(data$Foods_2_CA_3, frequency = 365, start = c(2011, 1, 29))
acf(Foods_2_CA_3)

#Foods_3_CA_3
Foods_3_CA_3 =ts(data$Foods_3_CA_3, frequency = 365, start = c(2011, 1, 29))
acf(Foods_3_CA_3)


#1.1.b
options(warn=-1)
library(tseries)

#ADF Test
#Store 1
test1 = adf.test(Hobbies_CA_1)
test2 = adf.test(Household_1_CA_1)
test3 = adf.test(Household_2_CA_1)
test4 = adf.test(Foods_1_CA_1)
test5 = adf.test(Foods_2_CA_1)
test6 = adf.test(Foods_3_CA_1)

#Store 2
test7 = adf.test(Hobbies_CA_2)
test8 = adf.test(Household_1_CA_2)
test9 = adf.test(Household_2_CA_2)
test10 = adf.test(Foods_1_CA_2)
test11 = adf.test(Foods_2_CA_2)
test12 = adf.test(Foods_3_CA_2)

#Store 3
test13 = adf.test(Hobbies_CA_3)
test14 = adf.test(Household_1_CA_3)
test15 = adf.test(Household_2_CA_3)
test16 = adf.test(Foods_1_CA_3)
test17 = adf.test(Foods_2_CA_3)
test18 = adf.test(Foods_3_CA_3)


#KPSS Test
kpss.test(Hobbies_CA_1, null="Trend")
kpss.test(Household_1_CA_1, null="Trend")
kpss.test(Household_2_CA_1, null="Trend")
kpss.test(Foods_1_CA_1, null="Trend")
kpss.test(Foods_2_CA_1, null="Trend")
kpss.test(Foods_3_CA_1, null="Trend")

#Store 2, null="Trend")
kpss.test(Hobbies_CA_2, null="Trend")
kpss.test(Household_1_CA_2, null="Trend")
kpss.test(Household_2_CA_2, null="Trend")
kpss.test(Foods_1_CA_2, null="Trend")
kpss.test(Foods_2_CA_2, null="Trend")
kpss.test(Foods_3_CA_2, null="Trend")

#Store 3, null="Trend")
kpss.test(Hobbies_CA_3, null="Trend")
kpss.test(Household_1_CA_3, null="Trend")
kpss.test(Household_2_CA_3, null="Trend")
kpss.test(Foods_1_CA_3, null="Trend")
kpss.test(Foods_2_CA_3, null="Trend")
kpss.test(Foods_3_CA_3, null="Trend")


par(mfrow = c(1, 1))
#Making the data stationary
plot(Hobbies_CA_1)
plot(diff(Hobbies_CA_1))
autoplot(diff(log(Hobbies_CA_1)))
acf(diff(Hobbies_CA_1),na.action = na.pass)
diff1 = diff(Hobbies_CA_1)
diff2 = diff(diff1)
plot(diff2)
adf.test(diff1)
acf(diff2,na.action = na.pass)
acf(diff(diff2),na.action = na.pass)


#Question 2
#2. We now want to assess the set of benchmark in the M5 guidelines. For several sample splits of your choice.

#Train-test split
#Hobbies_CA_1
#Split 1
training_Hobbies_CA_1 = head(Hobbies_CA_1, 1433)
testing_Hobbies_CA_1 = tail(Hobbies_CA_1, 536)

#split 2
training2_Hobbies_CA_1 = head(Hobbies_CA_1, 1614)
testing2_Hobbies_CA_1 = tail(Hobbies_CA_1, 355)

#split 3
training3_Hobbies_CA_1 = head(Hobbies_CA_1, 1798)
testing3_Hobbies_CA_1 = tail(Hobbies_CA_1, 171)

#Household_1_CA_1
#Split 1
training_Household_1_CA_1 = head(Household_1_CA_1, 1433)
testing_Household_1_CA_1 = tail(Household_1_CA_1, 536)

#split 2
training2_Household_1_CA_1 = head(Household_1_CA_1, 1614)
testing2_Household_1_CA_1 = tail(Household_1_CA_1, 355)

#split 3
training3_Household_1_CA_1 = head(Household_1_CA_1, 1798)
testing3_Household_1_CA_1 = tail(Household_1_CA_1, 171)

#Household_2_CA_1
#Split 1
training_Household_2_CA_1 = head(Household_2_CA_1, 1433)
testing_Household_2_CA_1 = tail(Household_2_CA_1, 536)

#split 2
training2_Household_2_CA_1 = head(Household_2_CA_1, 1614)
testing2_Household_2_CA_1 = tail(Household_2_CA_1, 355)

#split 3
training3_Household_2_CA_1 = head(Household_2_CA_1, 1798)
testing3_Household_2_CA_1 = tail(Household_2_CA_1, 171)

#Foods_1_CA_1
#Split 1
training_Foods_1_CA_1 = head(Foods_1_CA_1, 1433)
testing_Foods_1_CA_1 = tail(Foods_1_CA_1, 536)

#split 2
training2_Foods_1_CA_1 = head(Foods_1_CA_1, 1614)
testing2_Foods_1_CA_1 = tail(Foods_1_CA_1, 355)

#split 3
training3_Foods_1_CA_1 = head(Foods_1_CA_1, 1798)
testing3_Foods_1_CA_1 = tail(Foods_1_CA_1, 171)

#Foods_2_CA_1
#Split 1
training_Foods_2_CA_1 = head(Foods_2_CA_1, 1433)
testing_Foods_2_CA_1 = tail(Foods_2_CA_1, 536)

#split 2
training2_Foods_2_CA_1 = head(Foods_2_CA_1, 1614)
testing2_Foods_2_CA_1 = tail(Foods_2_CA_1, 355)

#split 3
training3_Foods_2_CA_1 = head(Foods_2_CA_1, 1798)
testing3_Foods_2_CA_1 = tail(Foods_2_CA_1, 171)

#Foods_3_CA_1
#Split 1
training_Foods_3_CA_1 = head(Foods_3_CA_1, 1433)
testing_Foods_3_CA_1 = tail(Foods_3_CA_1, 536)

#split 2
training2_Foods_3_CA_1 = head(Foods_3_CA_1, 1614)
testing2_Foods_3_CA_1 = tail(Foods_3_CA_1, 355)

#split 3
training3_Foods_3_CA_1 = head(Foods_3_CA_1, 1798)
testing3_Foods_3_CA_1 = tail(Foods_3_CA_1, 171)

#Hobbies_CA_2
#Split 1
training_Hobbies_CA_2 = head(Hobbies_CA_2, 1433)
testing_Hobbies_CA_2 = tail(Hobbies_CA_2, 536)

#split 2
training2_Hobbies_CA_2 = head(Hobbies_CA_2, 1614)
testing2_Hobbies_CA_2 = tail(Hobbies_CA_2, 355)

#split 3
training3_Hobbies_CA_2 = head(Hobbies_CA_2, 1798)
testing3_Hobbies_CA_2 = tail(Hobbies_CA_2, 171)

#Household_1_CA_2
#Split 1
training_Household_1_CA_2 = head(Household_1_CA_2, 1433)
testing_Household_1_CA_2 = tail(Household_1_CA_2, 536)

#split 2
training2_Household_1_CA_2 = head(Household_1_CA_2, 1614)
testing2_Household_1_CA_2 = tail(Household_1_CA_2, 355)

#split 3
training3_Household_1_CA_2 = head(Household_1_CA_2, 1798)
testing3_Household_1_CA_2 = tail(Household_1_CA_2, 171)


#Household_2_CA_2
#Split 1
training_Household_2_CA_2 = head(Household_2_CA_2, 1433)
testing_Household_2_CA_2 = tail(Household_2_CA_2, 536)

#split 2
training2_Household_2_CA_2 = head(Household_2_CA_2, 1614)
testing2_Household_2_CA_2 = tail(Household_2_CA_2, 355)

#split 3
training3_Household_2_CA_2 = head(Household_2_CA_2, 1798)
testing3_Household_2_CA_2 = tail(Household_2_CA_2, 171)


#Foods_1_CA_2
#Split 1
training_Foods_1_CA_2 = head(Foods_1_CA_2, 1433)
testing_Foods_1_CA_2 = tail(Foods_1_CA_2, 536)

#split 2
training2_Foods_1_CA_2 = head(Foods_1_CA_2, 1614)
testing2_Foods_1_CA_2 = tail(Foods_1_CA_2, 355)

#split 3
training3_Foods_1_CA_2 = head(Foods_1_CA_2, 1798)
testing3_Foods_1_CA_2 = tail(Foods_1_CA_2, 171)

#Foods_2_CA_2
#Split 1
training_Foods_2_CA_2 = head(Foods_2_CA_2, 1433)
testing_Foods_2_CA_2 = tail(Foods_2_CA_2, 536)

#split 2
training2_Foods_2_CA_2 = head(Foods_2_CA_2, 1614)
testing2_Foods_2_CA_2 = tail(Foods_2_CA_2, 355)

#split 3
training3_Foods_2_CA_2 = head(Foods_2_CA_2, 1798)
testing3_Foods_2_CA_2 = tail(Foods_2_CA_2, 171)

#Foods_3_CA_2
#Split 1
training_Foods_3_CA_2 = head(Foods_3_CA_2, 1433)
testing_Foods_3_CA_2 = tail(Foods_3_CA_2, 536)

#split 2
training2_Foods_3_CA_2 = head(Foods_3_CA_2, 1614)
testing2_Foods_3_CA_2 = tail(Foods_3_CA_2, 355)

#split 3
training3_Foods_3_CA_2 = head(Foods_3_CA_2, 1798)
testing3_Foods_3_CA_2 = tail(Foods_3_CA_2, 171)


#Hobbies_CA_3
#Split 1
training_Hobbies_CA_3 = head(Hobbies_CA_3, 1433)
testing_Hobbies_CA_3 = tail(Hobbies_CA_3, 536)

#split 2
training2_Hobbies_CA_3 = head(Hobbies_CA_3, 1614)
testing2_Hobbies_CA_3 = tail(Hobbies_CA_3, 355)

#split 3
training3_Hobbies_CA_3 = head(Hobbies_CA_3, 1798)
testing3_Hobbies_CA_3 = tail(Hobbies_CA_3, 171)


#Household_1_CA_3
#Split 1
training_Household_1_CA_3 = head(Household_1_CA_3, 1433)
testing_Household_1_CA_3 = tail(Household_1_CA_3, 536)

#split 2
training2_Household_1_CA_3 = head(Household_1_CA_3, 1614)
testing2_Household_1_CA_3 = tail(Household_1_CA_3, 355)

#split 3
training3_Household_1_CA_3 = head(Household_1_CA_3, 1798)
testing3_Household_1_CA_3 = tail(Household_1_CA_3, 171)

#Household_2_CA_3
#Split 1
training_Household_2_CA_3 = head(Household_2_CA_3, 1433)
testing_Household_2_CA_3 = tail(Household_2_CA_3, 536)

#split 2
training2_Household_2_CA_3 = head(Household_2_CA_3, 1614)
testing2_Household_2_CA_3 = tail(Household_2_CA_3, 355)

#split 3
training3_Household_2_CA_3 = head(Household_2_CA_3, 1798)
testing3_Household_2_CA_3 = tail(Household_2_CA_3, 171)


#Foods_1_CA_3
#Split 1
training_Foods_1_CA_3 = head(Foods_1_CA_3, 1433)
testing_Foods_1_CA_3 = tail(Foods_1_CA_3, 536)

#split 2
training2_Foods_1_CA_3 = head(Foods_1_CA_3, 1614)
testing2_Foods_1_CA_3 = tail(Foods_1_CA_3, 355)

#split 3
training3_Foods_1_CA_3 = head(Foods_1_CA_3, 1798)
testing3_Foods_1_CA_3 = tail(Foods_1_CA_3, 171)

#Foods_2_CA_3
#Split 1
training_Foods_2_CA_3 = head(Foods_2_CA_3, 1433)
testing_Foods_2_CA_3 = tail(Foods_2_CA_3, 536)

#split 2
training2_Foods_2_CA_3 = head(Foods_2_CA_3, 1614)
testing2_Foods_2_CA_3 = tail(Foods_2_CA_3, 355)

#split 3
training3_Foods_2_CA_3 = head(Foods_2_CA_3, 1798)
testing3_Foods_2_CA_3 = tail(Foods_2_CA_3, 171)

#Foods_3_CA_3
#Split 1
training_Foods_3_CA_3 = head(Foods_3_CA_3, 1433)
testing_Foods_3_CA_3 = tail(Foods_3_CA_3, 536)

#split 2
training2_Foods_3_CA_3 = head(Foods_3_CA_3, 1614)
testing2_Foods_3_CA_3 = tail(Foods_3_CA_3, 355)

#split 3
training3_Foods_3_CA_3 = head(Foods_3_CA_3, 1798)
testing3_Foods_3_CA_3 = tail(Foods_3_CA_3, 171)

###########################################################################################################################
#par(mfrow = c(1, 1))
#acf(diff(log(Hobbies_CA_1)),na.action = na.pass)
#acf(log(Hobbies_CA_1),na.action = na.pass)
#acf(Hobbies_CA_1)


#2.(a)
#Fit Statistical benchmarks 1 (Naive), 2 (sNaive), 3 (ES), 4 (MA), 13 (ESX), as well
#as SARIMA, SARIMAX (using X as in ESX) and Holt-Winters up to the end of the training subsample.

#Loss Function

##RMSSE Custom Function####

rmsse = function(actual, predicted, train){
  num = 0
  for (i in 2:length(actual)){
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


#Naives, Snaives, rmsse, MA

data_train1 = data.frame(training_Hobbies_CA_1,training_Household_1_CA_1,training_Household_2_CA_1,training_Foods_1_CA_1,training_Foods_2_CA_1,training_Foods_3_CA_1,training_Hobbies_CA_2,training_Household_1_CA_2,training_Household_2_CA_2,training_Foods_1_CA_2,training_Foods_2_CA_2,training_Foods_3_CA_2,training_Hobbies_CA_3,training_Household_1_CA_3,training_Household_2_CA_3,training_Foods_1_CA_3,training_Foods_2_CA_3,training_Foods_3_CA_3)
data_test1 = data.frame(testing_Hobbies_CA_1,testing_Household_1_CA_1,testing_Household_2_CA_1,testing_Foods_1_CA_1,testing_Foods_2_CA_1,testing_Foods_3_CA_1,testing_Hobbies_CA_2,testing_Household_1_CA_2,testing_Household_2_CA_2,testing_Foods_1_CA_2,testing_Foods_2_CA_2,testing_Foods_3_CA_2,testing_Hobbies_CA_3,testing_Household_1_CA_3,testing_Household_2_CA_3,testing_Foods_1_CA_3,testing_Foods_2_CA_3,testing_Foods_3_CA_3)

data_train2 = data.frame(training2_Hobbies_CA_1,training2_Household_1_CA_1,training2_Household_2_CA_1,training2_Foods_1_CA_1,training2_Foods_2_CA_1,training2_Foods_3_CA_1,training2_Hobbies_CA_2,training2_Household_1_CA_2,training2_Household_2_CA_2,training2_Foods_1_CA_2,training2_Foods_2_CA_2,training2_Foods_3_CA_2,training2_Hobbies_CA_3,training2_Household_1_CA_3,training2_Household_2_CA_3,training2_Foods_1_CA_3,training2_Foods_2_CA_3,training2_Foods_3_CA_3)
data_test2 = data.frame(testing2_Hobbies_CA_1,testing2_Household_1_CA_1,testing2_Household_2_CA_1,testing2_Foods_1_CA_1,testing2_Foods_2_CA_1,testing2_Foods_3_CA_1,testing2_Hobbies_CA_2,testing2_Household_1_CA_2,testing2_Household_2_CA_2,testing2_Foods_1_CA_2,testing2_Foods_2_CA_2,testing2_Foods_3_CA_2,testing2_Hobbies_CA_3,testing2_Household_1_CA_3,testing2_Household_2_CA_3,testing2_Foods_1_CA_3,testing2_Foods_2_CA_3,testing2_Foods_3_CA_3)

data_train3 = data.frame(training3_Hobbies_CA_1,training3_Household_1_CA_1,training3_Household_2_CA_1,training3_Foods_1_CA_1,training3_Foods_2_CA_1,training3_Foods_3_CA_1,training3_Hobbies_CA_2,training3_Household_1_CA_2,training3_Household_2_CA_2,training3_Foods_1_CA_2,training3_Foods_2_CA_2,training3_Foods_3_CA_2,training3_Hobbies_CA_3,training3_Household_1_CA_3,training3_Household_2_CA_3,training3_Foods_1_CA_3,training3_Foods_2_CA_3,training3_Foods_3_CA_3)
data_test3 = data.frame(testing3_Hobbies_CA_1,testing3_Household_1_CA_1,testing3_Household_2_CA_1,testing3_Foods_1_CA_1,testing3_Foods_2_CA_1,testing3_Foods_3_CA_1,testing3_Hobbies_CA_2,testing3_Household_1_CA_2,testing3_Household_2_CA_2,testing3_Foods_1_CA_2,testing3_Foods_2_CA_2,testing3_Foods_3_CA_2,testing3_Hobbies_CA_3,testing3_Household_1_CA_3,testing3_Household_2_CA_3,testing3_Foods_1_CA_3,testing3_Foods_2_CA_3,testing3_Foods_3_CA_3)


results = data.frame(matrix(ncol = 18, nrow = 1))
x <- c("training_Hobbies_CA_1","training_Household_1_CA_1","training_Household_2_CA_1","training_Foods_1_CA_1","training_Foods_2_CA_1","training_Foods_3_CA_1","training_Hobbies_CA_2","training_Household_1_CA_2","training_Household_2_CA_2","training_Foods_1_CA_2","training_Foods_2_CA_2","training_Foods_3_CA_2","training_Hobbies_CA_3","training_Household_1_CA_3","training_Household_2_CA_3","training_Foods_1_CA_3","training_Foods_2_CA_3","training_Foods_3_CA_3")
colnames(results) <- x


############# Issues - 
# Holts Winter not taking input model in second step

########################################### Split 1 ###################################################

train_results = data.frame(matrix(ncol = 18, nrow = 1))
x <- c("training_Hobbies_CA_1","training_Household_1_CA_1","training_Household_2_CA_1","training_Foods_1_CA_1","training_Foods_2_CA_1","training_Foods_3_CA_1","training_Hobbies_CA_2","training_Household_1_CA_2","training_Household_2_CA_2","training_Foods_1_CA_2","training_Foods_2_CA_2","training_Foods_3_CA_2","training_Hobbies_CA_3","training_Household_1_CA_3","training_Household_2_CA_3","training_Foods_1_CA_3","training_Foods_2_CA_3","training_Foods_3_CA_3")
colnames(train_results) <- x

test_results = data.frame(matrix(ncol = 18, nrow = 1))
x <- c("testing_Hobbies_CA_1","testing_Household_1_CA_1","testing_Household_2_CA_1","testing_Foods_1_CA_1","testing_Foods_2_CA_1","testing_Foods_3_CA_1","testing_Hobbies_CA_2","testing_Household_1_CA_2","testing_Household_2_CA_2","testing_Foods_1_CA_2","testing_Foods_2_CA_2","testing_Foods_3_CA_2","testing_Hobbies_CA_3","testing_Household_1_CA_3","testing_Household_2_CA_3","testing_Foods_1_CA_3","testing_Foods_2_CA_3","testing_Foods_3_CA_3")
colnames(test_results) <- x

testpredictions = data.frame(matrix(nrow = 536))

for(i in 1:18) {
  ### 1. NAIVE
  model1 = naive(c(data_train1[,i]))
  training_fit1 = fitted(model1) #Training fit of the model
  fit1 = naive(c(data_test1[,i]), model = model1)
  onestep1 = fitted(fit1) #Testing fit of the model
  trainloss1 = rmsse(data_train1[,i], training_fit1, data_train1[,i])
  train_results[1,i] = trainloss1
  testloss1 = rmsse(data_test1[,i], onestep1,data_train1[,i])
  test_results[1,i] = testloss1
  testpredictions[paste(colnames(test_results)[i],"Naive")] = c(onestep1)

  ### 2. SNAIVE
  model2 = snaive(c(data_train1[,i]))
  training_fit2 = fitted(model2) #Training fit of the model
  fit2 = snaive(c(data_test1[,i]), model = model2)
  onestep2 = fitted(fit2) #Testing fit of the model
  trainloss2 = rmsse(data_train1[,i], training_fit2, data_train1[,i])
  train_results[2,i] = trainloss2
  testloss2 = rmsse(data_test1[,i], onestep2,data_train1[,i])
  test_results[2,i] = testloss2
  testpredictions[paste(colnames(test_results)[i],"SNaive")] = c(onestep2)
  
  ### 3. ES
  model3 = es(c(data_train1[,i]))
  training_fit3 = fitted(model3) #Training fit of the model
  fit3 = es(c(data_test1[,i]), model = model3)
  onestep3 = fitted(fit3) #Testing fit of the model
  trainloss3 = rmsse(data_train1[,i], training_fit3, data_train1[,i])
  train_results[3,i] = trainloss3
  testloss3 = rmsse(data_test1[,i], onestep3, data_train1[,i])
  test_results[3,i] = testloss3
  testpredictions[paste(colnames(test_results)[i],"ES")] = c(onestep3)
  
  ## 5. ESX
  model5 = es(c(data_train1[,i]), xreg = x_reg)
  training_fit5 = fitted(model5) #Training fit of the model
  fit5 = naive(c(data_test1[,i]), model = model5)
  onestep5 = fitted(fit5) #Testing fit of the model
  trainloss5 = rmsse(data_train1[,i], training_fit5, data_train1[,i])
  train_results[4,i] = trainloss5
  testloss5 = rmsse(data_test1[,i], onestep5,data_train1[,i])
  test_results[4,i] = testloss5
  testpredictions[paste(colnames(test_results)[i],"ESX")] = c(onestep5)
  
  ## 8. Holts Winter
  

  ## 9. TBATS #to check if its state space model where latent variable follows random walk.
  model9 = tbats(c(data_train1[,i]))
  training_fit9 = fitted(model9) #Training fit of the model
  fit9 = tbats(c(data_test1[,i]), model = model9)
  onestep9 = fitted(fit9) #Testing fit of the model
  trainloss9 = rmsse(data_train1[,i], training_fit9, data_train1[,i])
  train_results[5,i] = trainloss9
  testloss9 = rmsse(data_test1[,i], onestep9,data_train1[,i])
  test_results[5,i] = testloss9
  testpredictions[paste(colnames(test_results)[i],"Naive")] = c(onestep9)
  
  #  final= data.frame(cbind(model1$fitted,model2$fitted,model3$fitted,model5$fitted))
}

print(results)
print(head(testpredictions))

############ ASHIMA - ####################

## 4. MA ####Order of MA resolve

#MA Models for all 18 time series - split 1

model10 = Arima(data_train1[,1],order = c(0,0,2))
training_fit10 = fitted(model10) #Training fit of the model
fit10 = Arima(c(data_test1[,1]), model = model10)
onestep10 = fitted(fit10) #Testing fit of the model
trainloss10 = rmsse(data_train1[,1], training_fit10, data_train1[,1])
train_results[7,1] = trainloss10
testloss10 = rmsse(data_test1[,1], onestep10,data_train1[,1])
test_results[7,1] = testloss10
testpredictions[paste(colnames(test_results)[1],"MA")] = c(onestep10)


model11 = Arima(data_train1[,2],order = c(0,0,2))
training_fit11 = fitted(model11) #Training fit of the model
fit11 = Arima(c(data_test1[,2]), model = model11)
onestep11 = fitted(fit11) #Testing fit of the model
trainloss11 = rmsse(data_train1[,2], training_fit11, data_train1[,2])
train_results[7,2] = trainloss11
testloss11 = rmsse(data_test1[,2], onestep11,data_train1[,2])
test_results[7,2] = testloss11
testpredictions[paste(colnames(test_results)[2],"MA")] = c(onestep11)

model12 = Arima(data_train1[,3],order = c(0,0,1))
training_fit12 = fitted(model12) #Training fit of the model
fit12 = Arima(c(data_test1[,3]), model = model12)
onestep12 = fitted(fit12) #Testing fit of the model
trainloss12 = rmsse(data_train1[,3], training_fit12, data_train1[,3])
train_results[7,3] = trainloss12
testloss12 = rmsse(data_test1[,3], onestep12,data_train1[,3])
test_results[7,3] = testloss12
testpredictions[paste(colnames(test_results)[3],"MA")] = c(onestep12)

model13 = Arima(data_train1[,4],order = c(0,0,2))
training_fit13 = fitted(model13) #Training fit of the model
fit13 = Arima(c(data_test1[,4]), model = model13)
onestep13 = fitted(fit13) #Testing fit of the model
trainloss13 = rmsse(data_train1[,4], training_fit13, data_train1[,4])
train_results[7,4] = trainloss13
testloss13 = rmsse(data_test1[,4], onestep13,data_train1[,4])
test_results[7,4] = testloss13
testpredictions[paste(colnames(test_results)[4],"MA")] = c(onestep13)

model14 = Arima(data_train1[,5],order = c(0,0,2))
training_fit14 = fitted(model14) #Training fit of the model
fit14 = Arima(c(data_test1[,5]), model = model14)
onestep14 = fitted(fit14) #Testing fit of the model
trainloss14 = rmsse(data_train1[,5], training_fit14, data_train1[,5])
train_results[7,5] = trainloss14
testloss14 = rmsse(data_test1[,5], onestep14,data_train1[,5])
test_results[7,5] = testloss14
testpredictions[paste(colnames(test_results)[5],"MA")] = c(onestep14)

model15 = Arima(data_train1[,6],order = c(0,0,2))
training_fit15 = fitted(model15) #Training fit of the model
fit15 = Arima(c(data_test1[,6]), model = model15)
onestep15 = fitted(fit15) #Testing fit of the model
trainloss15 = rmsse(data_train1[,6], training_fit15, data_train1[,6])
train_results[7,6] = trainloss15
testloss15 = rmsse(data_test1[,6], onestep15,data_train1[,6])
test_results[7,6] = testloss15
testpredictions[paste(colnames(test_results)[6],"MA")] = c(onestep15)

model16 = Arima(data_train1[,7],order = c(0,0,1))
training_fit16 = fitted(model16) #Training fit of the model
fit16 = Arima(c(data_test1[,7]), model = model16)
onestep16 = fitted(fit16) #Testing fit of the model
trainloss16 = rmsse(data_train1[,7], training_fit16, data_train1[,7])
train_results[7,7] = trainloss16
testloss16 = rmsse(data_test1[,7], onestep16,data_train1[,7])
test_results[7,7] = testloss16
testpredictions[paste(colnames(test_results)[7],"MA")] = c(onestep16)

model17 = Arima(data_train1[,8],order = c(0,0,1))
training_fit17 = fitted(model17) #Training fit of the model
fit17 = Arima(c(data_test1[,8]), model = model17)
onestep17 = fitted(fit17) #Testing fit of the model
trainloss17 = rmsse(data_train1[,8], training_fit17, data_train1[,8])
train_results[7,8] = trainloss17
testloss17 = rmsse(data_test1[,8], onestep17,data_train1[,8])
test_results[7,8] = testloss17
testpredictions[paste(colnames(test_results)[8],"MA")] = c(onestep17)

model18 = Arima(data_train1[,9],order = c(0,0,1))
training_fit18 = fitted(model18) #Training fit of the model
fit18 = Arima(c(data_test1[,9]), model = model18)
onestep18 = fitted(fit18) #Testing fit of the model
trainloss18 = rmsse(data_train1[,9], training_fit18, data_train1[,9])
train_results[7,9] = trainloss18
testloss18 = rmsse(data_test1[,9], onestep18,data_train1[,9])
test_results[7,9] = testloss18
testpredictions[paste(colnames(test_results)[9],"MA")] = c(onestep18)

model19 = Arima(data_train1[,10],order = c(0,0,2))
training_fit19 = fitted(model19) #Training fit of the model
fit19 = Arima(c(data_test1[,10]), model = model19)
onestep19 = fitted(fit19) #Testing fit of the model
trainloss19 = rmsse(data_train1[,10], training_fit19, data_train1[,10])
train_results[7,10] = trainloss19
testloss19 = rmsse(data_test1[,10], onestep19,data_train1[,10])
test_results[7,10] = testloss19
testpredictions[paste(colnames(test_results)[10],"MA")] = c(onestep19)

model20 = Arima(data_train1[,11],order = c(0,0,2))
training_fit20 = fitted(model20) #Training fit of the model
fit20 = Arima(c(data_test1[,11]), model = model20)
onestep20 = fitted(fit20) #Testing fit of the model
trainloss20 = rmsse(data_train1[,11], training_fit20, data_train1[,11])
train_results[7,11] = trainloss20
testloss20 = rmsse(data_test1[,11], onestep20,data_train1[,11])
test_results[7,11] = testloss20
testpredictions[paste(colnames(test_results)[11],"MA")] = c(onestep20)

model21 = Arima(data_train1[,12],order = c(0,0,1))
training_fit21 = fitted(model21) #Training fit of the model
fit21 = Arima(c(data_test1[,12]), model = model21)
onestep21 = fitted(fit21) #Testing fit of the model
trainloss21 = rmsse(data_train1[,12], training_fit21, data_train1[,12])
train_results[7,12] = trainloss21
testloss21 = rmsse(data_test1[,12], onestep21,data_train1[,12])
test_results[7,12] = testloss21
testpredictions[paste(colnames(test_results)[12],"MA")] = c(onestep21)


model22 = Arima(data_train1[,13],order = c(0,0,1))
training_fit22 = fitted(model22) #Training fit of the model
fit22 = Arima(c(data_test1[,13]), model = model22)
onestep22 = fitted(fit22) #Testing fit of the model
trainloss22 = rmsse(data_train1[,13], training_fit22, data_train1[,13])
train_results[7,13] = trainloss22
testloss22 = rmsse(data_test1[,13], onestep22,data_train1[,13])
test_results[7,13] = testloss22
testpredictions[paste(colnames(test_results)[13],"MA")] = c(onestep22)

model23 = Arima(data_train1[,14],order = c(0,0,1))
training_fit23 = fitted(model23) #Training fit of the model
fit23 = Arima(c(data_test1[,14]), model = model23)
onestep23 = fitted(fit23) #Testing fit of the model
trainloss23 = rmsse(data_train1[,14], training_fit23, data_train1[,14])
train_results[7,14] = trainloss23
testloss23 = rmsse(data_test1[,14], onestep23,data_train1[,14])
test_results[7,14] = testloss23
testpredictions[paste(colnames(test_results)[14],"MA")] = c(onestep23)

model24 = Arima(data_train1[,15],order = c(0,0,2))
training_fit24 = fitted(model24) #Training fit of the model
fit24 = Arima(c(data_test1[,15]), model = model24)
onestep24 = fitted(fit24) #Testing fit of the model
trainloss24 = rmsse(data_train1[,15], training_fit24, data_train1[,15])
train_results[7,15] = trainloss24
testloss24 = rmsse(data_test1[,15], onestep24,data_train1[,15])
test_results[6,15] = testloss24
testpredictions[paste(colnames(test_results)[15],"MA")] = c(onestep24)

model25 = Arima(data_train1[,16],order = c(0,0,2))
training_fit25 = fitted(model25) #Training fit of the model
fit25 = Arima(c(data_test1[,16]), model = model25)
onestep25 = fitted(fit25) #Testing fit of the model
trainloss25 = rmsse(data_train1[,16], training_fit25, data_train1[,16])
train_results[7,16] = trainloss25
testloss25 = rmsse(data_test1[,16], onestep25,data_train1[,16])
test_results[7,16] = testloss25
testpredictions[paste(colnames(test_results)[16],"MA")] = c(onestep25)

model26 = Arima(data_train1[,17],order = c(0,0,1))
training_fit26 = fitted(model26) #Training fit of the model
fit26 = Arima(c(data_test1[,17]), model = model26)
onestep26 = fitted(fit26) #Testing fit of the model
trainloss26 = rmsse(data_train1[,17], training_fit26, data_train1[,17])
train_results[6,17] = trainloss26
testloss26 = rmsse(data_test1[,17], onestep26,data_train1[,17])
test_results[7,17] = testloss26
testpredictions[paste(colnames(test_results)[17],"MA")] = c(onestep26)

model27 = Arima(data_train1[,18],order = c(0,0,2))
training_fit27 = fitted(model27) #Training fit of the model
fit27 = Arima(c(data_test1[,18]), model = model27)
onestep27 = fitted(fit27) #Testing fit of the model
trainloss27 = rmsse(data_train1[,18], training_fit27, data_train1[,18])
train_results[7,18] = trainloss27
testloss27 = rmsse(data_test1[,18], onestep27,data_train1[,18])
test_results[7,18] = testloss27
testpredictions[paste(colnames(test_results)[18],"MA")] = c(onestep27)


#Sarima for all 18 time series - Data Split 1

model44 = Arima(data_train1[,1],order = c(0,1,2),seasonal=list(order=c(2,1,1),period=7))
training_fit44 = fitted(model44) #Training fit of the model
fit44 = Arima(c(data_test1[,1]), model = model44)
onestep44 = fitted(fit44) #Testing fit of the model
trainloss44 = rmsse(data_train1[,1], training_fit44, data_train1[,1])
train_results[6,1] = trainloss44
testloss44 = rmsse(data_test1[,1], onestep44,data_train1[,1])
test_results[6,1] = testloss44
testpredictions[paste(colnames(test_results)[1],"SARIMA")] = c(onestep44)

model45 = Arima(data_train1[,2],order = c(0,1,2),seasonal=list(order=c(2,1,1),period=7))
training_fit45 = fitted(model45) #Training fit of the model
fit45 = Arima(c(data_test1[,2]), model = model45)
onestep45 = fitted(fit45) #Testing fit of the model
trainloss45 = rmsse(data_train1[,2], training_fit45, data_train1[,1])
train_results[6,1] = trainloss45
testloss45 = rmsse(data_test1[,2], onestep45,data_train1[,2])
test_results[6,1] = testloss45
testpredictions[paste(colnames(test_results)[2],"SARIMA")] = c(onestep45)


model28 = Arima(data_train1[,3],order = c(1,1,1),seasonal=list(order=c(2,1,1),period=7))
training_fit28 = fitted(model28) #Training fit of the model
fit28 = Arima(c(data_test1[,3]), model = model28)
onestep28 = fitted(fit28) #Testing fit of the model
trainloss28 = rmsse(data_train1[,3], training_fit28, data_train1[,3])
train_results[6,3] = trainloss28
testloss28 = rmsse(data_test1[,3], onestep28,data_train1[,3])
test_results[6,3] = testloss28
testpredictions[paste(colnames(test_results)[3],"SARIMA")] = c(onestep28)

model29 = Arima(data_train1[,4],order = c(0,1,2),seasonal=list(order=c(1,1,1),period=7))
training_fit29 = fitted(model29) #Training fit of the model
fit29 = Arima(c(data_test1[,4]), model = model29)
onestep29 = fitted(fit29) #Testing fit of the model
trainloss29 = rmsse(data_train1[,4], training_fit29, data_train1[,3])
train_results[6,4] = trainloss29
testloss29 = rmsse(data_test1[,4], onestep29,data_train1[,4])
test_results[6,4] = testloss29
testpredictions[paste(colnames(test_results)[4],"SARIMA")] = c(onestep29)

model30 = Arima(data_train1[,5],order = c(3,1,2),seasonal=list(order=c(2,1,1),period=7))
training_fit30 = fitted(model30) #Training fit of the model
fit30 = Arima(c(data_test1[,5]), model = model30)
onestep30 = fitted(fit30) #Testing fit of the model
trainloss30 = rmsse(data_train1[,5], training_fit30, data_train1[,3])
train_results[5,5] = trainloss30
testloss30 = rmsse(data_test1[,5], onestep30,data_train1[,5])
test_results[5,5] = testloss30
testpredictions[paste(colnames(test_results)[3],"SARIMA")] = c(onestep30)

model31 = Arima(data_train1[,6],order = c(1,1,2),seasonal=list(order=c(1,1,2),period=7))
training_fit31 = fitted(model31) #Training fit of the model
fit31 = Arima(c(data_test1[,6]), model = model31)
onestep31 = fitted(fit31) #Testing fit of the model
trainloss31 = rmsse(data_train1[,6], training_fit31, data_train1[,6])
train_results[6,6] = trainloss31
testloss31 = rmsse(data_test1[,6], onestep31,data_train1[,6])
test_results[6,6] = testloss31
testpredictions[paste(colnames(test_results)[6],"SARIMA")] = c(onestep31)

model32 = Arima(data_train1[,7],order = c(1,1,1),seasonal=list(order=c(1,1,1),period=7))
training_fit32 = fitted(model32) #Training fit of the model
fit32 = Arima(c(data_test1[,7]), model = model32)
onestep32 = fitted(fit32) #Testing fit of the model
trainloss32 = rmsse(data_train1[,7], training_fit32, data_train1[,7])
train_results[7,7] = trainloss32
testloss32 = rmsse(data_test1[,7], onestep32,data_train1[,7])
test_results[7,7] = testloss32
testpredictions[paste(colnames(test_results)[7],"SARIMA")] = c(onestep32)

model33 = Arima(data_train1[,8],order = c(1,1,1),seasonal=list(order=c(2,1,2),period=7))
training_fit33 = fitted(model33) #Training fit of the model
fit33 = Arima(c(data_test1[,8]), model = model33)
onestep33 = fitted(fit33) #Testing fit of the model
trainloss33 = rmsse(data_train1[,8], training_fit33, data_train1[,8])
train_results[6,8] = trainloss33
testloss33 = rmsse(data_test1[,8], onestep33,data_train1[,8])
test_results[6,8] = testloss33
testpredictions[paste(colnames(test_results)[8],"SARIMA")] = c(onestep33)

model34 = Arima(data_train1[,9],order = c(1,1,1),seasonal=list(order=c(0,1,2),period=7))
training_fit34 = fitted(model34) #Training fit of the model
fit34 = Arima(c(data_test1[,9]), model = model34)
onestep34 = fitted(fit34) #Testing fit of the model
trainloss34 = rmsse(data_train1[,9], training_fit34, data_train1[,9])
train_results[6,9] = trainloss34
testloss34 = rmsse(data_test1[,9], onestep34,data_train1[,9])
test_results[6,9] = testloss34
testpredictions[paste(colnames(test_results)[9],"SARIMA")] = c(onestep34)

model35 = Arima(data_train1[,10],order = c(2,1,2),seasonal=list(order=c(0,1,1),period=7))
training_fit35 = fitted(model35) #Training fit of the model
fit35 = Arima(c(data_test1[,10]), model = model35)
onestep35 = fitted(fit35) #Testing fit of the model
trainloss35 = rmsse(data_train1[,10], training_fit35, data_train1[,10])
train_results[6,10] = trainloss35
testloss35 = rmsse(data_test1[,10], onestep35,data_train1[,10])
test_results[6,10] = testloss35
testpredictions[paste(colnames(test_results)[10],"SARIMA")] = c(onestep35)

model36 = Arima(data_train1[,11],order = c(1,1,2),seasonal=list(order=c(0,1,2),period=7))
training_fit36 = fitted(model36) #Training fit of the model
fit36 = Arima(c(data_test1[,11]), model = model36)
onestep36 = fitted(fit36) #Testing fit of the model
trainloss36 = rmsse(data_train1[,11], training_fit36, data_train1[,11])
train_results[6,11] = trainloss36
testloss36 = rmsse(data_test1[,11], onestep36,data_train1[,11])
test_results[6,11] = testloss36
testpredictions[paste(colnames(test_results)[11],"SARIMA")] = c(onestep36)

model37 = Arima(data_train1[,12],order = c(2,1,1),seasonal=list(order=c(1,1,2),period=7))
training_fit37 = fitted(model37) #Training fit of the model
fit37 = Arima(c(data_test1[,12]), model = model37)
onestep37 = fitted(fit37) #Testing fit of the model
trainloss37 = rmsse(data_train1[,12], training_fit37, data_train1[,12])
train_results[6,12] = trainloss37
testloss37 = rmsse(data_test1[,12], onestep37,data_train1[,12])
test_results[6,12] = testloss37
testpredictions[paste(colnames(test_results)[12],"SARIMA")] = c(onestep37)

model38 = Arima(data_train1[,13],order = c(1,1,1),seasonal=list(order=c(0,1,1),period=7))
training_fit38 = fitted(model38) #Training fit of the model
fit38 = Arima(c(data_test1[,13]), model = model38)
onestep38 = fitted(fit38) #Testing fit of the model
trainloss38 = rmsse(data_train1[,13], training_fit38, data_train1[,13])
train_results[6,13] = trainloss38
testloss38 = rmsse(data_test1[,13], onestep38,data_train1[,13])
test_results[6,13] = testloss38
testpredictions[paste(colnames(test_results)[13],"SARIMA")] = c(onestep38)

model39 = Arima(data_train1[,14],order = c(1,1,1),seasonal=list(order=c(2,1,2),period=7))
training_fit39 = fitted(model39) #Training fit of the model
fit39 = Arima(c(data_test1[,14]), model = model39)
onestep39 = fitted(fit39) #Testing fit of the model
trainloss39 = rmsse(data_train1[,14], training_fit39, data_train1[,14])
train_results[6,14] = trainloss39
testloss39 = rmsse(data_test1[,14], onestep39,data_train1[,14])
test_results[6,14] = testloss39
testpredictions[paste(colnames(test_results)[14],"SARIMA")] = c(onestep39)

model40 = Arima(data_train1[,15],order = c(0,1,2),seasonal=list(order=c(2,1,1),period=7))
training_fit40 = fitted(model40) #Training fit of the model
fit40 = Arima(c(data_test1[,15]), model = model40)
onestep40 = fitted(fit40) #Testing fit of the model
trainloss40 = rmsse(data_train1[,15], training_fit40, data_train1[,15])
train_results[6,15] = trainloss40
testloss40 = rmsse(data_test1[,15], onestep40,data_train1[,15])
test_results[6,15] = testloss40
testpredictions[paste(colnames(test_results)[15],"SARIMA")] = c(onestep40)

model41 = Arima(data_train1[,16],order = c(0,1,2),seasonal=list(order=c(0,1,1),period=7))
training_fit41 = fitted(model41) #Training fit of the model
fit41 = Arima(c(data_test1[,16]), model = model41)
onestep41 = fitted(fit41) #Testing fit of the model
trainloss41 = rmsse(data_train1[,16], training_fit41, data_train1[,16])
train_results[6,16] = trainloss41
testloss41 = rmsse(data_test1[,16], onestep41,data_train1[,16])
test_results[6,16] = testloss41
testpredictions[paste(colnames(test_results)[16],"SARIMA")] = c(onestep41)

model42 = Arima(data_train1[,17],order = c(2,1,1),seasonal=list(order=c(0,1,2),period=7))
training_fit42 = fitted(model42) #Training fit of the model
fit42 = Arima(c(data_test1[,17]), model = model42)
onestep42 = fitted(fit42) #Testing fit of the model
trainloss42 = rmsse(data_train1[,17], training_fit42, data_train1[,17])
train_results[6,17] = trainloss42
testloss42 = rmsse(data_test1[,17], onestep42,data_train1[,17])
test_results[6,17] = testloss42
testpredictions[paste(colnames(test_results)[17],"SARIMA")] = c(onestep42)

model43 = Arima(data_train1[,18],order = c(1,1,2),seasonal=list(order=c(0,1,2),period=7))
training_fit43 = fitted(model43) #Training fit of the model
fit43 = Arima(c(data_test1[,18]), model = model43)
onestep43 = fitted(fit43) #Testing fit of the model
trainloss43 = rmsse(data_train1[,18], training_fit43, data_train1[,18])
train_results[6,18] = trainloss43
testloss43 = rmsse(data_test1[,18], onestep43,data_train1[,18])
test_results[6,18] = testloss43
testpredictions[paste(colnames(test_results)[18],"SARIMA")] = c(onestep43)

##SARIMAX

model46 = Arima(data_train1[,1],order = c(0,1,2),seasonal=list(order=c(2,1,1),period=7), xreg = x_reg)
training_fit46 = fitted(model46) #Training fit of the model
fit46 = Arima(c(data_test1[,1]), model = model46)
onestep46 = fitted(fit46) #Testing fit of the model
trainloss46 = rmsse(data_train1[,1], training_fit46, data_train1[,1])
train_results[7,1] = trainloss46
testloss46 = rmsse(data_test1[,1], onestep46,data_train1[,1])
test_results[7,1] = testloss46
testpredictions[paste(colnames(test_results)[1],"SARIMAX")] = c(onestep46)

model47 = Arima(data_train1[,2],order = c(0,1,2),seasonal=list(order=c(2,1,1),period=7), xreg = x_reg)
training_fit47 = fitted(model47) #Training fit of the model
fit47 = Arima(c(data_test1[,2]), model = model47)
onestep47 = fitted(fit47) #Testing fit of the model
trainloss47 = rmsse(data_train1[,2], training_fit47, data_train1[,1])
train_results[7,1] = trainloss47
testloss47 = rmsse(data_test1[,2], onestep47,data_train1[,2])
test_results[7,1] = testloss47
testpredictions[paste(colnames(test_results)[2],"SARIMAX")] = c(onestep47)


model48 = Arima(data_train1[,3],order = c(1,1,1),seasonal=list(order=c(2,1,1),period=7), xreg = x_reg)
training_fit48 = fitted(model48) #Training fit of the model
fit48 = Arima(c(data_test1[,3]), model = model48)
onestep48 = fitted(fit48) #Testing fit of the model
trainloss48 = rmsse(data_train1[,3], training_fit48, data_train1[,3])
train_results[7,3] = trainloss48
testloss48 = rmsse(data_test1[,3], onestep48,data_train1[,3])
test_results[7,3] = testloss48
testpredictions[paste(colnames(test_results)[3],"SARIMAX")] = c(onestep48)

model49 = Arima(data_train1[,4],order = c(0,1,2),seasonal=list(order=c(1,1,1),period=7) ,xreg = x_reg)
training_fit49 = fitted(model49) #Training fit of the model
fit49 = Arima(c(data_test1[,4]), model = model49)
onestep49 = fitted(fit49) #Testing fit of the model
trainloss49 = rmsse(data_train1[,4], training_fit49, data_train1[,3])
train_results[7,4] = trainloss49
testloss49 = rmsse(data_test1[,4], onestep49,data_train1[,4])
test_results[7,4] = testloss49
testpredictions[paste(colnames(test_results)[4],"SARIMAX")] = c(onestep49)

model50 = Arima(data_train1[,5],order = c(3,1,2),seasonal=list(order=c(2,1,1),period=7),xreg = x_reg)
training_fit50 = fitted(model50) #Training fit of the model
fit50 = Arima(c(data_test1[,5]), model = model50)
onestep50 = fitted(fit50) #Testing fit of the model
trainloss50 = rmsse(data_train1[,5], training_fit50, data_train1[,3])
train_results[5,5] = trainloss50
testloss50 = rmsse(data_test1[,5], onestep50,data_train1[,5])
test_results[5,5] = testloss50
testpredictions[paste(colnames(test_results)[3],"SARIMAX")] = c(onestep50)

model51 = Arima(data_train1[,6],order = c(1,1,2),seasonal=list(order=c(1,1,2),period=7),xreg = x_reg)
training_fit51 = fitted(model51) #Training fit of the model
fit51 = Arima(c(data_test1[,6]), model = model51)
onestep51 = fitted(fit51) #Testing fit of the model
trainloss51 = rmsse(data_train1[,6], training_fit51, data_train1[,6])
train_results[7,6] = trainloss51
testloss51 = rmsse(data_test1[,6], onestep51,data_train1[,6])
test_results[7,6] = testloss51
testpredictions[paste(colnames(test_results)[7],"SARIMAX")] = c(onestep51)

model52 = Arima(data_train1[,7],order = c(1,1,1),seasonal=list(order=c(1,1,1),period=7),xreg = x_reg)
training_fit52 = fitted(model52) #Training fit of the model
fit52 = Arima(c(data_test1[,7]), model = model52)
onestep52 = fitted(fit52) #Testing fit of the model
trainloss52 = rmsse(data_train1[,7], training_fit52, data_train1[,7])
train_results[7,7] = trainloss52
testloss52 = rmsse(data_test1[,7], onestep52,data_train1[,7])
test_results[7,7] = testloss52
testpredictions[paste(colnames(test_results)[7],"SARIMAX")] = c(onestep52)

model53 = Arima(data_train1[,8],order = c(1,1,1),seasonal=list(order=c(2,1,2),period=7),xreg = x_reg)
training_fit53 = fitted(model53) #Training fit of the model
fit53 = Arima(c(data_test1[,8]), model = model53)
onestep53 = fitted(fit53) #Testing fit of the model
trainloss53 = rmsse(data_train1[,8], training_fit53, data_train1[,8])
train_results[7,8] = trainloss53
testloss53 = rmsse(data_test1[,8], onestep53,data_train1[,8])
test_results[7,8] = testloss53
testpredictions[paste(colnames(test_results)[8],"SARIMAX")] = c(onestep53)

model54 = Arima(data_train1[,9],order = c(1,1,1),seasonal=list(order=c(0,1,2),period=7),xreg = x_reg)
training_fit54 = fitted(model54) #Training fit of the model
fit54 = Arima(c(data_test1[,9]), model = model54)
onestep54 = fitted(fit54) #Testing fit of the model
trainloss54 = rmsse(data_train1[,9], training_fit54, data_train1[,9])
train_results[7,9] = trainloss54
testloss54 = rmsse(data_test1[,9], onestep54,data_train1[,9])
test_results[7,9] = testloss54
testpredictions[paste(colnames(test_results)[9],"SARIMAX")] = c(onestep54)

model55 = Arima(data_train1[,10],order = c(2,1,2),seasonal=list(order=c(0,1,1),period=7),xreg = x_reg)
training_fit55 = fitted(model55) #Training fit of the model
fit55 = Arima(c(data_test1[,10]), model = model55)
onestep55 = fitted(fit55) #Testing fit of the model
trainloss55 = rmsse(data_train1[,10], training_fit55, data_train1[,10])
train_results[7,10] = trainloss55
testloss55 = rmsse(data_test1[,10], onestep55,data_train1[,10])
test_results[7,10] = testloss55
testpredictions[paste(colnames(test_results)[10],"SARIMAX")] = c(onestep55)

model56 = Arima(data_train1[,11],order = c(1,1,2),seasonal=list(order=c(0,1,2),period=7),xreg = x_reg)
training_fit56 = fitted(model56) #Training fit of the model
fit56 = Arima(c(data_test1[,11]), model = model56)
onestep56 = fitted(fit56) #Testing fit of the model
trainloss56 = rmsse(data_train1[,11], training_fit56, data_train1[,11])
train_results[7,11] = trainloss56
testloss56 = rmsse(data_test1[,11], onestep56,data_train1[,11])
test_results[7,11] = testloss56
testpredictions[paste(colnames(test_results)[11],"SARIMAX")] = c(onestep56)

model57 = Arima(data_train1[,12],order = c(2,1,1),seasonal=list(order=c(1,1,2),period=7),xreg = x_reg)
training_fit57 = fitted(model57) #Training fit of the model
fit57 = Arima(c(data_test1[,12]), model = model57)
onestep57 = fitted(fit57) #Testing fit of the model
trainloss57 = rmsse(data_train1[,12], training_fit57, data_train1[,12])
train_results[7,12] = trainloss57
testloss57 = rmsse(data_test1[,12], onestep57,data_train1[,12])
test_results[7,12] = testloss57
testpredictions[paste(colnames(test_results)[12],"SARIMAX")] = c(onestep57)

model58 = Arima(data_train1[,13],order = c(1,1,1),seasonal=list(order=c(0,1,1),period=7),xreg = x_reg)
training_fit58 = fitted(model58) #Training fit of the model
fit58 = Arima(c(data_test1[,13]), model = model58)
onestep58 = fitted(fit58) #Testing fit of the model
trainloss58 = rmsse(data_train1[,13], training_fit58, data_train1[,13])
train_results[7,13] = trainloss58
testloss58 = rmsse(data_test1[,13], onestep58,data_train1[,13])
test_results[7,13] = testloss58
testpredictions[paste(colnames(test_results)[13],"SARIMAX")] = c(onestep58)

model59 = Arima(data_train1[,14],order = c(1,1,1),seasonal=list(order=c(2,1,2),period=7),xreg = x_reg)
training_fit59 = fitted(model59) #Training fit of the model
fit59 = Arima(c(data_test1[,14]), model = model59)
onestep59 = fitted(fit59) #Testing fit of the model
trainloss59 = rmsse(data_train1[,14], training_fit59, data_train1[,14])
train_results[7,14] = trainloss59
testloss59 = rmsse(data_test1[,14], onestep59,data_train1[,14])
test_results[7,14] = testloss59
testpredictions[paste(colnames(test_results)[14],"SARIMAX")] = c(onestep59)

model60 = Arima(data_train1[,15],order = c(0,1,2),seasonal=list(order=c(2,1,1),period=7),xreg = x_reg)
training_fit60 = fitted(model60) #Training fit of the model
fit60 = Arima(c(data_test1[,15]), model = model60)
onestep60 = fitted(fit60) #Testing fit of the model
trainloss60 = rmsse(data_train1[,15], training_fit60, data_train1[,15])
train_results[7,15] = trainloss60
testloss60 = rmsse(data_test1[,15], onestep60,data_train1[,15])
test_results[7,15] = testloss60
testpredictions[paste(colnames(test_results)[15],"SARIMAX")] = c(onestep60)

model61 = Arima(data_train1[,16],order = c(0,1,2),seasonal=list(order=c(0,1,1),period=7),xreg = x_reg)
training_fit61 = fitted(model61) #Training fit of the model
fit61 = Arima(c(data_test1[,16]), model = model61)
onestep61 = fitted(fit61) #Testing fit of the model
trainloss61 = rmsse(data_train1[,16], training_fit61, data_train1[,16])
train_results[7,16] = trainloss61
testloss61 = rmsse(data_test1[,16], onestep61,data_train1[,16])
test_results[7,16] = testloss61
testpredictions[paste(colnames(test_results)[16],"SARIMAX")] = c(onestep61)

model62 = Arima(data_train1[,17],order = c(2,1,1),seasonal=list(order=c(0,1,2),period=7),xreg =x_reg)
training_fit62 = fitted(model62) #Training fit of the model
fit62 = Arima(c(data_test1[,17]), model = model62)
onestep62 = fitted(fit62) #Testing fit of the model
trainloss62 = rmsse(data_train1[,17], training_fit62, data_train1[,17])
train_results[7,17] = trainloss62
testloss62 = rmsse(data_test1[,17], onestep62,data_train1[,17])
test_results[7,17] = testloss62
testpredictions[paste(colnames(test_results)[17],"SARIMAX")] = c(onestep62)

model63 = Arima(data_train1[,18],order = c(1,1,2),seasonal=list(order=c(0,1,2),period=7),xreg = x_reg)
training_fit63 = fitted(model63) #Training fit of the model
fit63 = Arima(c(data_test1[,18]), model = model63)
onestep63 = fitted(fit63) #Testing fit of the model
trainloss63 = rmsse(data_train1[,18], training_fit63, data_train1[,18])
train_results[7,18] = trainloss63
testloss63 = rmsse(data_test1[,18], onestep63,data_train1[,18])
test_results[7,18] = testloss63
testpredictions[paste(colnames(test_results)[18],"SARIMAX")] = c(onestep63)


autoarima = auto.arima(Hobbies_CA_1, max.p = 5, max.q=5, trace=TRUE,stepwise=F)

autoplot(diff(diff(log(Hobbies_CA_1)),7))

#for loop for optimal parameters of SARIMA
d=1
D=1
s=7

for(p in 1:3)
{for(q in 1:3)
{for(P in 1:3)
{for(Q in 1:3)
{if(p+d+q+P+D+Q<=20)
{model<-arima(x=data_train1[,18],order=c(p-1,d,q-1),seasonal=list(order=c(P-1,D,Q-1),period=s))
sse=sum(model$residuals^2)
cat(p-1,d,q-1,P-1,D,Q-1, "AIC:",model$aic,"SSE:",sse,'\n')

}
}
}
}
}


par(mfrow=c(1,1))
model.fit<-arima(x=training,order=c(1,1,2),seasonal=list(order=c(1,1,2),period=7))
plot(forecast(model.fit))

predicted =forecast(model.fit)
actual = testing[2]
model.fit


#Holt Winters
HW1 <- HoltWinters(training_Hobbies_CA_1)
#Visually evaluate the fits
plot(training_Hobbies_CA_1,xlim=c(2011,2016))
lines(HW1$fitted[,1], lty=2, col="blue")
lines(HW2$fitted[,1], lty=2, col="red")


library(forecast)
HW1_for <- forecast(HW1, h=24)
#visualize our predictions:
plot(HW1_for, xlim=c(2011, 2017))
lines(HW1_for$fitted, lty=2, col="purple")

######################### ASHIMA #################################

# e. Combination of Forecasting Techniques

#1. Combine two ES techniques - ES and ESX Results
testpredictions$`training_Hobbies_CA_1 ES&ESX` = testpredictions$`training_Hobbies_CA_1 ES`/2 + testpredictions$`training_Hobbies_CA_1 ESX`/2
loss = rmsse(data_test1$testing_Hobbies_CA_1, testpredictions$`training_Hobbies_CA_1 ES&ESX`, data_train1$training_Hobbies_CA_1)
print(loss) #Need to append as new row in of the test results

testpredictions$`training_Hobbies_CA_2 ES&ESX` = testpredictions$`training_Hobbies_CA_2 ES`/2 + testpredictions$`training_Hobbies_CA_2 ESX`/2
loss = rmsse(data_test1$testing_Hobbies_CA_2, testpredictions$`training_Hobbies_CA_2 ES&ESX`, data_train1$training_Hobbies_CA_2)
print(loss)

testpredictions$`training_Hobbies_CA_3 ES&ESX` = testpredictions$`training_Hobbies_CA_3 ES`/2 + testpredictions$`training_Hobbies_CA_3 ESX`/2
loss = rmsse(data_test1$testing_Hobbies_CA_3, testpredictions$`training_Hobbies_CA_3 ES&ESX`, data_train1$training_Hobbies_CA_3)
print(loss) #Need to append as new row in of the test results

#Need to replicate this for other 15 time series


#2. Another Combination - SARIMA and SARIMAX Results


# f. Compare the in-sample (training) vs out-of-sample (testing) fit of the models
## Compare the train_results and test_results tables

# g. Do you find similarities in terms of forecast performance across stores or types of items?
# Compare the final results across stores and types of items.


#Q3 - Obtain forecast for horizon h = 1,2,3,...28.
# Compare the testpredictions table and data_test table for first 28 rows.

#Q4 - Aggregate the data at Store or type of item level.
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

#Convert these aggregate level variables to time-series format
Hobbies_agg = ts(data$Hobbies_agg, frequency = 365, start = c(2011, 1, 29))
Household_1_agg = ts(data$Household_1_agg, frequency = 365, start = c(2011, 1, 29))
Household_2_agg = ts(data$Household_2_agg, frequency = 365, start = c(2011, 1, 29))
Foods_1_agg = ts(data$Foods_1_agg, frequency = 365, start = c(2011, 1, 29))
Foods_2_agg = ts(data$Foods_2_agg, frequency = 365, start = c(2011, 1, 29))
Foods_3_agg = ts(data$Foods_3_agg, frequency = 365, start = c(2011, 1, 29))


#Train-test split
#Hobbies_agg
#Split 1
training_Hobbies_agg_1 = head(Hobbies_agg, 1433)
testing_Hobbies_agg_1 = tail(Hobbies_agg, 536)

#split 2
training_Hobbies_agg_2 = head(Hobbies_agg, 1614)
testing_Hobbies_agg_2 = tail(Hobbies_agg, 355)

#split 3
training_Hobbies_agg_3 = head(Hobbies_agg, 1798)
testing_Hobbies_agg_3 = tail(Hobbies_agg, 171)

#Household_1_agg
#Split 1
training_Household_1_agg_1 = head(Household_1_agg, 1433)
testing_Household_1_agg_1 = tail(Houshold_1_agg, 536)

#split 2
training_Household_1_agg_2 = head(Household_1_agg, 1614)
testing_Household_1_agg_2 = tail(Houshold_1_agg, 355)

#split 3
training_Household_1_agg_3 = head(Household_1_agg, 1798)
testing_Household_1_agg_3 = tail(Houshold_1_agg, 171)

#Household_2_agg
#Split 1
training_Household_2_agg_1 = head(Household_2_agg, 1433)
testing_Household_2_agg_1 = tail(Houshold_2_agg, 536)

#split 2
training_Household_2_agg_2 = head(Household_2_agg, 1614)
testing_Household_2_agg_2 = tail(Houshold_2_agg, 355)

#split 3
training_Household_3_agg_3 = head(Household_2_agg, 1798)
testing_Household_3_agg_3 = tail(Houshold_2_agg, 171)

#Foods_1_agg
#Split 1
training_Foods_1_agg_1 = head(Foods_1_agg, 1433)
testing_Foods_1_agg_1 = tail(Foods_1_agg, 536)

#split 2
training_Foods_1_agg_2 = head(Foods_1_agg, 1614)
testing_Foods_1_agg_2 = tail(Foods_1_agg, 355)

#split 3
training_Foods_1_agg_3 = head(Foods_1_agg, 1798)
testing_Foods_1_agg_3 = tail(Foods_1_agg, 171)

#Foods_2_agg
#Split 1
training_Foods_2_agg_1 = head(Foods_2_agg, 1433)
testing_Foods_2_agg_1 = tail(Foods_2_agg, 536)

#split 2
training_Foods_2_agg_2 = head(Foods_2_agg, 1614)
testing_Foods_2_agg_2 = tail(Foods_2_agg, 355)

#split 3
training_Foods_2_agg_3 = head(Foods_2_agg, 1798)
testing_Foods_2_agg_3 = tail(Foods_2_agg, 171)

#Foods_3_agg
#Split 1
training_Foods_3_agg_1 = head(Foods_3_agg, 1433)
testing_Foods_3_agg_1 = tail(Foods_3_agg, 536)

#split 2
training_Foods_3_agg_2 = head(Foods_3_agg, 1614)
testing_Foods_3_agg_2 = tail(Foods_3_agg, 355)

#split 3
training_Foods_3_agg_3 = head(Foods_3_agg, 1798)
testing_Foods_3_agg_3 = tail(Foods_3_agg, 171)

# 4.a Produce forecast of these aggregates using the previous methods
# Waiting for above part to get completed with all the models

# 4.b Compare the forecast of the aggregate to the aggregate of the forecast 


#Q5 - Aggregate the data at weekly frequency.
data$WeekYear = strftime(data$ï..Date, format = "%Y-W%V")

library(dplyr)
data_WeeklyAggregate = data %>%
  group_by(WeekYear) %>%
  summarise(Hobbies_CA_1_wagg = sum(Hobbies_CA_1), Hobbies_CA_2_wagg = sum(Hobbies_CA_2), Hobbies_CA_3_wagg = sum(Hobbies_CA_3),
            Household_1_CA_1_wagg = sum(Household_1_CA_1), Household_1_CA_2_wagg = sum(Household_1_CA_2), Household_1_CA_3_wagg = sum(Household_1_CA_3),
            Household_2_CA_1_wagg = sum(Household_2_CA_1), Household_2_CA_2_wagg = sum(Household_2_CA_2), Household_2_CA_3_wagg = sum(Household_2_CA_3),
            Foods_1_CA_1_wagg = sum(Foods_1_CA_1), Foods_1_CA_2_wagg = sum(Foods_1_CA_2), Foods_1_CA_3_wagg = sum(Foods_1_CA_3), 
            Foods_2_CA_1_wagg = sum(Foods_2_CA_1), Foods_2_CA_2_wagg = sum(Foods_2_CA_2), Foods_2_CA_3_wagg = sum(Foods_2_CA_3),
            Foods_3_CA_1_wagg = sum(Foods_3_CA_1), Foods_3_CA_2_wagg = sum(Foods_3_CA_2), Foods_3_CA_3_wagg = sum(Foods_3_CA_3))

data_WeeklyAggregate = as.data.frame(data_WeeklyAggregate)
print(head(data_WeeklyAggregate))

#split into train and test set

#Split 1
data_train1_wagg = head(data_WeeklyAggregate, 205)
data_test1_wagg = tail(data_WeeklyAggregate, 78)

#Split 2
data_train2 = head(data_WeeklyAggregate, 229)
data_test2 = tail(data_WeeklyAggregate, 54)

#Split 3
data_train3 = head(data_WeeklyAggregate, 258)
data_test_3 = tail(data_WeeklyAggregate, 25)

# 5.a Produce forecast of these aggregates using the previous methods
# Waiting for above part to get completed with all the models

# 5.b Compare the forecast of the aggregate to the aggregate of the forecast 


#7(a,b)
#Probabalistic Forecasts
#Custom Function of Pinball Loss
pinball_loss <- function(tau, train, test, pred, horizon) {
  # tau is the level of quantile that you want
  # test is the test data
  # pred is the predicted data
  # horizon is the number of predicted forecasts
  # train is the train data
  num = 0
  pl_df = data.frame(tau = tau,
                     test = test,
                     pred = pred)
  pl_df = pl_df %>%
    mutate(L = ifelse(test>=pred,tau/horizon * (test-pred),((1-tau)/horizon) * (pred-test)))
  num = sum(pl_df$L)
  den = 0
  for (i in 2:length(train)){
    den = den + abs(train[i] - train[i-1])
  }
  den = den/(length(train)-1)
  
  return(num/den)
}

#Data Split 1
for (i in 1:18){
  y  = c(0.005, 0.025, 0.165, 0.25, 0.5, 0.75, 0.835, 0.975, 0.995)
  colnames(ses) <- y
  
  
  ses = ses(data_train1[,i], h = 28, level = c(50, 67, 95, 99))
  ses = data.frame(cbind(ses$lower[,4], ses$lower[,3], ses$lower[,2], ses$lower[,1],
                         ses$mean,
                         ses$upper[,1], ses$upper[,2], ses$upper[,3],ses$upper[,4]))

  print(cat(i,"ses"))
  average_ses = print(cat(pinball_loss(rep(0.005,28),data_train1[,i],head(data_test1[,i],28),ses$ses.lower...4.,28),
                 pinball_loss(rep(0.025,28),data_train1[,i],head(data_test1[,i],28),ses$ses.lower...3.,28),
                 pinball_loss(rep(0.165,28),data_train1[,i],head(data_test1[,i],28),ses$ses.lower...2.,28),
                 pinball_loss(rep(0.25,28),data_train1[,i],head(data_test1[,i],28),ses$ses.lower...1.,28),
                 pinball_loss(rep(0.75,28),data_train1[,i],head(data_test1[,i],28),ses$ses.mean,28),
                 pinball_loss(rep(0.835,28),data_train1[,i],head(data_test1[,i],28),ses$ses.upper...1.,28),
                 pinball_loss(rep(0.975,28),data_train1[,i],head(data_test1[,i],28),ses$ses.upper...2.,28),
                 pinball_loss(rep(0.995,28),data_train1[,i],head(data_test1[,i],28),ses$ses.upper...3.,28),
                 pinball_loss(rep(0.995,28),data_train1[,i],head(data_test1[,i],28),ses$ses.upper...4.,28)))

  
  #NAIVE
  naive = naive(data_train1[,i], h = 28, level = c(50, 67, 95, 99))
  naive = data.frame(cbind(naive$lower[,4], naive$lower[,3], naive$lower[,2], naive$lower[,1],
                           naive$mean,
                           naive$upper[,1], naive$upper[,2], naive$upper[,3], naive$upper[,4]))
  print(cat(i,"naive"))
  
  average_naive = print(cat(pinball_loss(rep(0.005,28),data_train1[,i],head(data_test1[,i],28),naive$naive.lower...4.,28),
                       pinball_loss(rep(0.025,28),data_train1[,i],head(data_test1[,i],28),naive$naive.lower...3.,28),
                       pinball_loss(rep(0.165,28),data_train1[,i],head(data_test1[,i],28),naive$naive.lower...2.,28),
                       pinball_loss(rep(0.25,28),data_train1[,i],head(data_test1[,i],28),naive$naive.lower...1.,28),
                       pinball_loss(rep(0.75,28),data_train1[,i],head(data_test1[,i],28),naive$naive.mean,28),
                       pinball_loss(rep(0.835,28),data_train1[,i],head(data_test1[,i],28),naive$naive.upper...1.,28),
                       pinball_loss(rep(0.975,28),data_train1[,i],head(data_test1[,i],28),naive$naive.upper...2.,28),
                       pinball_loss(rep(0.995,28),data_train1[,i],head(data_test1[,i],28),naive$naive.upper...3.,28),
                       pinball_loss(rep(0.995,28),data_train1[,i],head(data_test1[,i],28),naive$naive.upper...4.,28)))

  
  #sNaive
  snaive = snaive(data_train1[,i], h = 28, level = c(50, 67, 95, 99))
  snaive = data.frame(cbind(snaive$lower[,4], snaive$lower[,3], snaive$lower[,2], snaive$lower[,1],
                            snaive$mean,
                            snaive$upper[,1], snaive$upper[,2], snaive$upper[,3], snaive$upper[,4]))
  
  print(cat(i,"arima"))
  average_snaive = print(cat(pinball_loss(rep(0.005,28),data_train1[,i],head(data_test1[,i],28),snaive$snaive.lower...4.,28),
                        pinball_loss(rep(0.025,28),data_train1[,i],head(data_test1[,i],28),snaive$snaive.lower...3.,28),
                        pinball_loss(rep(0.165,28),data_train1[,i],head(data_test1[,i],28),snaive$snaive.lower...2.,28),
                        pinball_loss(rep(0.25,28),data_train1[,i],head(data_test1[,i],28),snaive$snaive.lower...1.,28),
                        pinball_loss(rep(0.75,28),data_train1[,i],head(data_test1[,i],28),snaive$snaive.mean,28),
                        pinball_loss(rep(0.835,28),data_train1[,i],head(data_test1[,i],28),snaive$snaive.upper...1.,28),
                        pinball_loss(rep(0.975,28),data_train1[,i],head(data_test1[,i],28),snaive$snaive.upper...2.,28),
                        pinball_loss(rep(0.995,28),data_train1[,i],head(data_test1[,i],28),snaive$snaive.upper...3.,28),
                        pinball_loss(rep(0.995,28),data_train1[,i],head(data_test1[,i],28),snaive$snaive.upper...4.,28)))
  
  #Exponential Smoothing (Bottom up approach)
  etsb = forecast(ets(data_train1[,i]), method="bu", fmethod="arima",h=28,level = c(50, 67, 95, 99))
  etsb = data.frame(cbind(etsb$lower[,4], etsb$lower[,3], etsb$lower[,2], etsb$lower[,1],
                         etsb$mean,
                         etsb$upper[,1], etsb$upper[,2], etsb$upper[,3], etsb$upper[,4]))
  print(cat(i,"Exp Smoothing"))
  average_etsb = print(cat(pinball_loss(rep(0.005,28),data_train1[,i],head(data_test1[,i],28),etsb$etsb.lower...4.,28),
                      pinball_loss(rep(0.025,28),data_train1[,i],head(data_test1[,i],28),etsb$etsb.lower...3.,28),
                      pinball_loss(rep(0.165,28),data_train1[,i],head(data_test1[,i],28),etsb$etsb.lower...2.,28),
                      pinball_loss(rep(0.25,28),data_train1[,i],head(data_test1[,i],28),etsb$etsb.lower...1.,28),
                      pinball_loss(rep(0.75,28),data_train1[,i],head(data_test1[,i],28),etsb$etsb.mean,28),
                      pinball_loss(rep(0.835,28),data_train1[,i],head(data_test1[,i],28),etsb$etsb.upper...1.,28),
                      pinball_loss(rep(0.975,28),data_train1[,i],head(data_test1[,i],28),etsb$etsb.upper...2.,28),
                      pinball_loss(rep(0.995,28),data_train1[,i],head(data_test1[,i],28),etsb$etsb.upper...3.,28),
                      pinball_loss(rep(0.995,28),data_train1[,i],head(data_test1[,i],28),etsb$etsb.upper...4.,28)))

  
  #AutoRegressive Integrated Moving Average (ARIMA)
  arima = forecast(auto.arima(data_train1[,i]), h = 28, level = c(50, 67, 95, 99))
  arima = data.frame(cbind(arima$lower[,4], arima$lower[,3], arima$lower[,2], arima$lower[,1],
                           arima$mean,
                           arima$upper[,1], arima$upper[,2], arima$upper[,3], arima$upper[,4]))
  print(cat(i,"ARIMA"))
  average_arima = print(cat(pinball_loss(rep(0.005,28),data_train1[,i],head(data_test1[,i],28),arima$arima.lower...4.,28),
                       pinball_loss(rep(0.025,28),data_train1[,i],head(data_test1[,i],28),arima$arima.lower...3.,28),
                       pinball_loss(rep(0.165,28),data_train1[,i],head(data_test1[,i],28),arima$arima.lower...2.,28),
                       pinball_loss(rep(0.25,28),data_train1[,i],head(data_test1[,i],28),arima$arima.lower...1.,28),
                       pinball_loss(rep(0.75,28),data_train1[,i],head(data_test1[,i],28),arima$arima.mean,28),
                       pinball_loss(rep(0.835,28),data_train1[,i],head(data_test1[,i],28),arima$arima.upper...1.,28),
                       pinball_loss(rep(0.975,28),data_train1[,i],head(data_test1[,i],28),arima$arima.upper...2.,28),
                       pinball_loss(rep(0.995,28),data_train1[,i],head(data_test1[,i],28),arima$arima.upper...3.,28),
                       pinball_loss(rep(0.995,28),data_train1[,i],head(data_test1[,i],28),arima$arima.upper...4.,28)))

  
  #Kernel Density Function
  quants = as.numeric(quantile(data_train1[,i],c(0.005, 0.025, 0.165, 0.25, 0.5, 0.75, 0.835, 0.975, 0.995)))
  kernel = data.frame(cbind(rep(quants[1],28), rep(quants[2],28), rep(quants[3],28),
                            rep(quants[4],28), rep(quants[5],28), rep(quants[6],28),
                            rep(quants[7],28), rep(quants[8],28), rep(quants[9],28)))
  print(cat(i,"Kernel"))
  
  average_kernel = print(cat(pinball_loss(rep(0.005,28),data_train1[,i],head(data_test1[,i],28),kernel$X1,28),
                             pinball_loss(rep(0.025,28),data_train1[,i],head(data_test1[,i],28),kernel$X2,28),
                             pinball_loss(rep(0.165,28),data_train1[,i],head(data_test1[,i],28),kernel$X3,28),
                             pinball_loss(rep(0.25,28),data_train1[,i],head(data_test1[,i],28),kernel$X4,28),
                             pinball_loss(rep(0.75,28),data_train1[,i],head(data_test1[,i],28),kernel$X5,28),
                             pinball_loss(rep(0.835,28),data_train1[,i],head(data_test1[,i],28),kernel$X6,28),
                             pinball_loss(rep(0.975,28),data_train1[,i],head(data_test1[,i],28),kernel$X7,28),
                             pinball_loss(rep(0.995,28),data_train1[,i],head(data_test1[,i],28),kernel$X8,28),
                             pinball_loss(rep(0.995,28),data_train1[,i],head(data_test1[,i],28),kernel$X8,28)))


}

#Data Split 2
for (i in 1:18){
  y  = c(0.005, 0.025, 0.165, 0.25, 0.5, 0.75, 0.835, 0.975, 0.995)
  colnames(ses) <- y
  
  
  ses = ses(data_train2[,i], h = 28, level = c(50, 67, 95, 99))
  ses = data.frame(cbind(ses$lower[,4], ses$lower[,3], ses$lower[,2], ses$lower[,1],
                         ses$mean,
                         ses$upper[,1], ses$upper[,2], ses$upper[,3],ses$upper[,4]))
  
  print(cat(i,"ses"))
  average_ses = print(cat(pinball_loss(rep(0.005,28),data_train2[,i],head(data_test2[,i],28),ses$ses.lower...4.,28),
                          pinball_loss(rep(0.025,28),data_train2[,i],head(data_test2[,i],28),ses$ses.lower...3.,28),
                          pinball_loss(rep(0.165,28),data_train2[,i],head(data_test2[,i],28),ses$ses.lower...2.,28),
                          pinball_loss(rep(0.25,28),data_train2[,i],head(data_test2[,i],28),ses$ses.lower...1.,28),
                          pinball_loss(rep(0.75,28),data_train2[,i],head(data_test2[,i],28),ses$ses.mean,28),
                          pinball_loss(rep(0.835,28),data_train2[,i],head(data_test2[,i],28),ses$ses.upper...1.,28),
                          pinball_loss(rep(0.975,28),data_train2[,i],head(data_test2[,i],28),ses$ses.upper...2.,28),
                          pinball_loss(rep(0.995,28),data_train2[,i],head(data_test2[,i],28),ses$ses.upper...3.,28),
                          pinball_loss(rep(0.995,28),data_train2[,i],head(data_test2[,i],28),ses$ses.upper...4.,28)))
  
  
  #NAIVE
  naive = naive(data_train2[,i], h = 28, level = c(50, 67, 95, 99))
  naive = data.frame(cbind(naive$lower[,4], naive$lower[,3], naive$lower[,2], naive$lower[,1],
                           naive$mean,
                           naive$upper[,1], naive$upper[,2], naive$upper[,3], naive$upper[,4]))
  print(cat(i,"naive"))
  
  average_naive = print(cat(pinball_loss(rep(0.005,28),data_train2[,i],head(data_test2[,i],28),naive$naive.lower...4.,28),
                            pinball_loss(rep(0.025,28),data_train2[,i],head(data_test2[,i],28),naive$naive.lower...3.,28),
                            pinball_loss(rep(0.165,28),data_train2[,i],head(data_test2[,i],28),naive$naive.lower...2.,28),
                            pinball_loss(rep(0.25,28),data_train2[,i],head(data_test2[,i],28),naive$naive.lower...1.,28),
                            pinball_loss(rep(0.75,28),data_train2[,i],head(data_test2[,i],28),naive$naive.mean,28),
                            pinball_loss(rep(0.835,28),data_train2[,i],head(data_test2[,i],28),naive$naive.upper...1.,28),
                            pinball_loss(rep(0.975,28),data_train2[,i],head(data_test2[,i],28),naive$naive.upper...2.,28),
                            pinball_loss(rep(0.995,28),data_train2[,i],head(data_test2[,i],28),naive$naive.upper...3.,28),
                            pinball_loss(rep(0.995,28),data_train2[,i],head(data_test2[,i],28),naive$naive.upper...4.,28)))
  
  
  #sNaive
  snaive = snaive(data_train2[,i], h = 28, level = c(50, 67, 95, 99))
  snaive = data.frame(cbind(snaive$lower[,4], snaive$lower[,3], snaive$lower[,2], snaive$lower[,1],
                            snaive$mean,
                            snaive$upper[,1], snaive$upper[,2], snaive$upper[,3], snaive$upper[,4]))
  
  print(cat(i,"arima"))
  average_snaive = print(cat(pinball_loss(rep(0.005,28),data_train2[,i],head(data_test2[,i],28),snaive$snaive.lower...4.,28),
                             pinball_loss(rep(0.025,28),data_train2[,i],head(data_test2[,i],28),snaive$snaive.lower...3.,28),
                             pinball_loss(rep(0.165,28),data_train2[,i],head(data_test2[,i],28),snaive$snaive.lower...2.,28),
                             pinball_loss(rep(0.25,28),data_train2[,i],head(data_test2[,i],28),snaive$snaive.lower...1.,28),
                             pinball_loss(rep(0.75,28),data_train2[,i],head(data_test2[,i],28),snaive$snaive.mean,28),
                             pinball_loss(rep(0.835,28),data_train2[,i],head(data_test2[,i],28),snaive$snaive.upper...1.,28),
                             pinball_loss(rep(0.975,28),data_train2[,i],head(data_test2[,i],28),snaive$snaive.upper...2.,28),
                             pinball_loss(rep(0.995,28),data_train2[,i],head(data_test2[,i],28),snaive$snaive.upper...3.,28),
                             pinball_loss(rep(0.995,28),data_train2[,i],head(data_test2[,i],28),snaive$snaive.upper...4.,28)))
  
  #Exponential Smoothing (Bottom up approach)
  etsb = forecast(ets(data_train2[,i]), method="bu", fmethod="arima",h=28,level = c(50, 67, 95, 99))
  etsb = data.frame(cbind(etsb$lower[,4], etsb$lower[,3], etsb$lower[,2], etsb$lower[,1],
                          etsb$mean,
                          etsb$upper[,1], etsb$upper[,2], etsb$upper[,3], etsb$upper[,4]))
  print(cat(i,"Exp Smoothing"))
  average_etsb = print(cat(pinball_loss(rep(0.005,28),data_train2[,i],head(data_test2[,i],28),etsb$etsb.lower...4.,28),
                           pinball_loss(rep(0.025,28),data_train2[,i],head(data_test2[,i],28),etsb$etsb.lower...3.,28),
                           pinball_loss(rep(0.165,28),data_train2[,i],head(data_test2[,i],28),etsb$etsb.lower...2.,28),
                           pinball_loss(rep(0.25,28),data_train2[,i],head(data_test2[,i],28),etsb$etsb.lower...1.,28),
                           pinball_loss(rep(0.75,28),data_train2[,i],head(data_test2[,i],28),etsb$etsb.mean,28),
                           pinball_loss(rep(0.835,28),data_train2[,i],head(data_test2[,i],28),etsb$etsb.upper...1.,28),
                           pinball_loss(rep(0.975,28),data_train2[,i],head(data_test2[,i],28),etsb$etsb.upper...2.,28),
                           pinball_loss(rep(0.995,28),data_train2[,i],head(data_test2[,i],28),etsb$etsb.upper...3.,28),
                           pinball_loss(rep(0.995,28),data_train2[,i],head(data_test2[,i],28),etsb$etsb.upper...4.,28)))
  
  
  #AutoRegressive Integrated Moving Average (ARIMA)
  arima = forecast(auto.arima(data_train2[,i]), h = 28, level = c(50, 67, 95, 99))
  arima = data.frame(cbind(arima$lower[,4], arima$lower[,3], arima$lower[,2], arima$lower[,1],
                           arima$mean,
                           arima$upper[,1], arima$upper[,2], arima$upper[,3], arima$upper[,4]))
  print(cat(i,"ARIMA"))
  average_arima = print(cat(pinball_loss(rep(0.005,28),data_train2[,i],head(data_test2[,i],28),arima$arima.lower...4.,28),
                            pinball_loss(rep(0.025,28),data_train2[,i],head(data_test2[,i],28),arima$arima.lower...3.,28),
                            pinball_loss(rep(0.165,28),data_train2[,i],head(data_test2[,i],28),arima$arima.lower...2.,28),
                            pinball_loss(rep(0.25,28),data_train2[,i],head(data_test2[,i],28),arima$arima.lower...1.,28),
                            pinball_loss(rep(0.75,28),data_train2[,i],head(data_test2[,i],28),arima$arima.mean,28),
                            pinball_loss(rep(0.835,28),data_train2[,i],head(data_test2[,i],28),arima$arima.upper...1.,28),
                            pinball_loss(rep(0.975,28),data_train2[,i],head(data_test2[,i],28),arima$arima.upper...2.,28),
                            pinball_loss(rep(0.995,28),data_train2[,i],head(data_test2[,i],28),arima$arima.upper...3.,28),
                            pinball_loss(rep(0.995,28),data_train2[,i],head(data_test2[,i],28),arima$arima.upper...4.,28)))
  
  
  #Kernel Density Function
  quants = as.numeric(quantile(data_train2[,i],c(0.005, 0.025, 0.165, 0.25, 0.5, 0.75, 0.835, 0.975, 0.995)))
  kernel = data.frame(cbind(rep(quants[1],28), rep(quants[2],28), rep(quants[3],28),
                            rep(quants[4],28), rep(quants[5],28), rep(quants[6],28),
                            rep(quants[7],28), rep(quants[8],28), rep(quants[9],28)))
  print(cat(i,"Kernel"))
  
  average_kernel = print(cat(pinball_loss(rep(0.005,28),data_train2[,i],head(data_test2[,i],28),kernel$X1,28),
                             pinball_loss(rep(0.025,28),data_train2[,i],head(data_test2[,i],28),kernel$X2,28),
                             pinball_loss(rep(0.165,28),data_train2[,i],head(data_test2[,i],28),kernel$X3,28),
                             pinball_loss(rep(0.25,28),data_train2[,i],head(data_test2[,i],28),kernel$X4,28),
                             pinball_loss(rep(0.75,28),data_train2[,i],head(data_test2[,i],28),kernel$X5,28),
                             pinball_loss(rep(0.835,28),data_train2[,i],head(data_test2[,i],28),kernel$X6,28),
                             pinball_loss(rep(0.975,28),data_train2[,i],head(data_test2[,i],28),kernel$X7,28),
                             pinball_loss(rep(0.995,28),data_train2[,i],head(data_test2[,i],28),kernel$X8,28),
                             pinball_loss(rep(0.995,28),data_train2[,i],head(data_test2[,i],28),kernel$X8,28)))
  
  
}

#Data Split 3
for (i in 1:18){
  y  = c(0.005, 0.025, 0.165, 0.25, 0.5, 0.75, 0.835, 0.975, 0.995)
  colnames(ses) <- y
  
  
  ses = ses(data_train3[,i], h = 28, level = c(50, 67, 95, 99))
  ses = data.frame(cbind(ses$lower[,4], ses$lower[,3], ses$lower[,2], ses$lower[,1],
                         ses$mean,
                         ses$upper[,1], ses$upper[,2], ses$upper[,3],ses$upper[,4]))
  
  print(cat(i,"ses"))
  average_ses = print(cat(pinball_loss(rep(0.005,28),data_train3[,i],head(data_test3[,i],28),ses$ses.lower...4.,28),
                          pinball_loss(rep(0.025,28),data_train3[,i],head(data_test3[,i],28),ses$ses.lower...3.,28),
                          pinball_loss(rep(0.165,28),data_train3[,i],head(data_test3[,i],28),ses$ses.lower...2.,28),
                          pinball_loss(rep(0.25,28),data_train3[,i],head(data_test3[,i],28),ses$ses.lower...1.,28),
                          pinball_loss(rep(0.75,28),data_train3[,i],head(data_test3[,i],28),ses$ses.mean,28),
                          pinball_loss(rep(0.835,28),data_train3[,i],head(data_test3[,i],28),ses$ses.upper...1.,28),
                          pinball_loss(rep(0.975,28),data_train3[,i],head(data_test3[,i],28),ses$ses.upper...2.,28),
                          pinball_loss(rep(0.995,28),data_train3[,i],head(data_test3[,i],28),ses$ses.upper...3.,28),
                          pinball_loss(rep(0.995,28),data_train3[,i],head(data_test3[,i],28),ses$ses.upper...4.,28)))
  
  
  #NAIVE
  naive = naive(data_train3[,i], h = 28, level = c(50, 67, 95, 99))
  naive = data.frame(cbind(naive$lower[,4], naive$lower[,3], naive$lower[,2], naive$lower[,1],
                           naive$mean,
                           naive$upper[,1], naive$upper[,2], naive$upper[,3], naive$upper[,4]))
  print(cat(i,"naive"))
  
  average_naive = print(cat(pinball_loss(rep(0.005,28),data_train3[,i],head(data_test3[,i],28),naive$naive.lower...4.,28),
                            pinball_loss(rep(0.025,28),data_train3[,i],head(data_test3[,i],28),naive$naive.lower...3.,28),
                            pinball_loss(rep(0.165,28),data_train3[,i],head(data_test3[,i],28),naive$naive.lower...2.,28),
                            pinball_loss(rep(0.25,28),data_train3[,i],head(data_test3[,i],28),naive$naive.lower...1.,28),
                            pinball_loss(rep(0.75,28),data_train3[,i],head(data_test3[,i],28),naive$naive.mean,28),
                            pinball_loss(rep(0.835,28),data_train3[,i],head(data_test3[,i],28),naive$naive.upper...1.,28),
                            pinball_loss(rep(0.975,28),data_train3[,i],head(data_test3[,i],28),naive$naive.upper...2.,28),
                            pinball_loss(rep(0.995,28),data_train3[,i],head(data_test3[,i],28),naive$naive.upper...3.,28),
                            pinball_loss(rep(0.995,28),data_train3[,i],head(data_test3[,i],28),naive$naive.upper...4.,28)))
  
  
  #sNaive
  snaive = snaive(data_train3[,i], h = 28, level = c(50, 67, 95, 99))
  snaive = data.frame(cbind(snaive$lower[,4], snaive$lower[,3], snaive$lower[,2], snaive$lower[,1],
                            snaive$mean,
                            snaive$upper[,1], snaive$upper[,2], snaive$upper[,3], snaive$upper[,4]))
  
  print(cat(i,"arima"))
  average_snaive = print(cat(pinball_loss(rep(0.005,28),data_train3[,i],head(data_test3[,i],28),snaive$snaive.lower...4.,28),
                             pinball_loss(rep(0.025,28),data_train3[,i],head(data_test3[,i],28),snaive$snaive.lower...3.,28),
                             pinball_loss(rep(0.165,28),data_train3[,i],head(data_test3[,i],28),snaive$snaive.lower...2.,28),
                             pinball_loss(rep(0.25,28),data_train3[,i],head(data_test3[,i],28),snaive$snaive.lower...1.,28),
                             pinball_loss(rep(0.75,28),data_train3[,i],head(data_test3[,i],28),snaive$snaive.mean,28),
                             pinball_loss(rep(0.835,28),data_train3[,i],head(data_test3[,i],28),snaive$snaive.upper...1.,28),
                             pinball_loss(rep(0.975,28),data_train3[,i],head(data_test3[,i],28),snaive$snaive.upper...2.,28),
                             pinball_loss(rep(0.995,28),data_train3[,i],head(data_test3[,i],28),snaive$snaive.upper...3.,28),
                             pinball_loss(rep(0.995,28),data_train3[,i],head(data_test3[,i],28),snaive$snaive.upper...4.,28)))
  
  #Exponential Smoothing (Bottom up approach)
  etsb = forecast(ets(data_train3[,i]), method="bu", fmethod="arima",h=28,level = c(50, 67, 95, 99))
  etsb = data.frame(cbind(etsb$lower[,4], etsb$lower[,3], etsb$lower[,2], etsb$lower[,1],
                          etsb$mean,
                          etsb$upper[,1], etsb$upper[,2], etsb$upper[,3], etsb$upper[,4]))
  print(cat(i,"Exp Smoothing"))
  average_etsb = print(cat(pinball_loss(rep(0.005,28),data_train3[,i],head(data_test3[,i],28),etsb$etsb.lower...4.,28),
                           pinball_loss(rep(0.025,28),data_train3[,i],head(data_test3[,i],28),etsb$etsb.lower...3.,28),
                           pinball_loss(rep(0.165,28),data_train3[,i],head(data_test3[,i],28),etsb$etsb.lower...2.,28),
                           pinball_loss(rep(0.25,28),data_train3[,i],head(data_test3[,i],28),etsb$etsb.lower...1.,28),
                           pinball_loss(rep(0.75,28),data_train3[,i],head(data_test3[,i],28),etsb$etsb.mean,28),
                           pinball_loss(rep(0.835,28),data_train3[,i],head(data_test3[,i],28),etsb$etsb.upper...1.,28),
                           pinball_loss(rep(0.975,28),data_train3[,i],head(data_test3[,i],28),etsb$etsb.upper...2.,28),
                           pinball_loss(rep(0.995,28),data_train3[,i],head(data_test3[,i],28),etsb$etsb.upper...3.,28),
                           pinball_loss(rep(0.995,28),data_train3[,i],head(data_test3[,i],28),etsb$etsb.upper...4.,28)))
  
  
  #AutoRegressive Integrated Moving Average (ARIMA)
  arima = forecast(auto.arima(data_train3[,i]), h = 28, level = c(50, 67, 95, 99))
  arima = data.frame(cbind(arima$lower[,4], arima$lower[,3], arima$lower[,2], arima$lower[,1],
                           arima$mean,
                           arima$upper[,1], arima$upper[,2], arima$upper[,3], arima$upper[,4]))
  print(cat(i,"ARIMA"))
  average_arima = print(cat(pinball_loss(rep(0.005,28),data_train3[,i],head(data_test3[,i],28),arima$arima.lower...4.,28),
                            pinball_loss(rep(0.025,28),data_train3[,i],head(data_test3[,i],28),arima$arima.lower...3.,28),
                            pinball_loss(rep(0.165,28),data_train3[,i],head(data_test3[,i],28),arima$arima.lower...2.,28),
                            pinball_loss(rep(0.25,28),data_train3[,i],head(data_test3[,i],28),arima$arima.lower...1.,28),
                            pinball_loss(rep(0.75,28),data_train3[,i],head(data_test3[,i],28),arima$arima.mean,28),
                            pinball_loss(rep(0.835,28),data_train3[,i],head(data_test3[,i],28),arima$arima.upper...1.,28),
                            pinball_loss(rep(0.975,28),data_train3[,i],head(data_test3[,i],28),arima$arima.upper...2.,28),
                            pinball_loss(rep(0.995,28),data_train3[,i],head(data_test3[,i],28),arima$arima.upper...3.,28),
                            pinball_loss(rep(0.995,28),data_train3[,i],head(data_test3[,i],28),arima$arima.upper...4.,28)))
  
  
  #Kernel Density Function
  quants = as.numeric(quantile(data_train3[,i],c(0.005, 0.025, 0.165, 0.25, 0.5, 0.75, 0.835, 0.975, 0.995)))
  kernel = data.frame(cbind(rep(quants[1],28), rep(quants[2],28), rep(quants[3],28),
                            rep(quants[4],28), rep(quants[5],28), rep(quants[6],28),
                            rep(quants[7],28), rep(quants[8],28), rep(quants[9],28)))
  print(cat(i,"Kernel"))
  
  average_kernel = print(cat(pinball_loss(rep(0.005,28),data_train3[,i],head(data_test3[,i],28),kernel$X1,28),
                             pinball_loss(rep(0.025,28),data_train3[,i],head(data_test3[,i],28),kernel$X2,28),
                             pinball_loss(rep(0.165,28),data_train3[,i],head(data_test3[,i],28),kernel$X3,28),
                             pinball_loss(rep(0.25,28),data_train3[,i],head(data_test3[,i],28),kernel$X4,28),
                             pinball_loss(rep(0.75,28),data_train3[,i],head(data_test3[,i],28),kernel$X5,28),
                             pinball_loss(rep(0.835,28),data_train3[,i],head(data_test3[,i],28),kernel$X6,28),
                             pinball_loss(rep(0.975,28),data_train3[,i],head(data_test3[,i],28),kernel$X7,28),
                             pinball_loss(rep(0.995,28),data_train3[,i],head(data_test3[,i],28),kernel$X8,28),
                             pinball_loss(rep(0.995,28),data_train3[,i],head(data_test3[,i],28),kernel$X8,28)))
  
}
