# The libraries to be used  ###########################################################################################################

library(readr)
library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(ggplot2)
library(xts)

source("/Users/prajwallohan/Documents/ESSEC-CS/Courses/Term 1/Core courses/Forecasting & Predictive Analytics/Assignments/Project and data/Forecasting_Walmart_Sales/Forecasting_Walmart_Sales/CustomFunctions.R")

# Read the dataset  ###########################################################################################################

project_data = read.csv("/Users/prajwallohan/Documents/ESSEC-CS/Courses/Term 1/Core courses/Forecasting & Predictive Analytics/Assignments/Project and data/Projectdata.csv")
project_data$date = as.Date(project_data$date, format = "%d/%m/%y")
print(head(project_data))

# Create time series objects  ###########################################################################################################
# Time series for each variable
hobbies_ca_1_ts = xts(project_data$Hobbies_CA_1, order.by = project_data$date)
household_1_ca_1_ts = xts(project_data$Household_1_CA_1, order.by = project_data$date)
household_2_ca_1_ts = xts(project_data$Household_2_CA_1, order.by = project_data$date)
foods_1_ca_1_ts = xts(project_data$Foods_1_CA_1, order.by = project_data$date)
foods_2_ca_1_ts = xts(project_data$Foods_2_CA_1, order.by = project_data$date)
foods_3_ca_1_ts = xts(project_data$Foods_3_CA_1, order.by = project_data$date)

hobbies_ca_2_ts = xts(project_data$Hobbies_CA_2, order.by = project_data$date)
household_1_ca_2_ts = xts(project_data$Household_1_CA_2, order.by = project_data$date)
household_2_ca_2_ts = xts(project_data$Household_2_CA_2, order.by = project_data$date)
foods_1_ca_2_ts = xts(project_data$Foods_1_CA_2, order.by = project_data$date)
foods_2_ca_2_ts = xts(project_data$Foods_2_CA_2, order.by = project_data$date)
foods_3_ca_2_ts = xts(project_data$Foods_3_CA_2, order.by = project_data$date)

hobbies_ca_3_ts = xts(project_data$Hobbies_CA_3, order.by = project_data$date)
household_1_ca_3_ts = xts(project_data$Household_1_CA_3, order.by = project_data$date)
household_2_ca_3_ts = xts(project_data$Household_2_CA_3, order.by = project_data$date)
foods_1_ca_3_ts = xts(project_data$Foods_1_CA_3, order.by = project_data$date)
foods_2_ca_3_ts = xts(project_data$Foods_2_CA_3, order.by = project_data$date)
foods_3_ca_3_ts = xts(project_data$Foods_3_CA_3, order.by = project_data$date)

# Time series for aggregated variables at store level
ca_1_ts = hobbies_ca_1_ts + household_1_ca_1_ts + household_2_ca_1_ts + 
  foods_1_ca_1_ts + foods_2_ca_1_ts + foods_3_ca_1_ts

ca_2_ts = hobbies_ca_2_ts + household_1_ca_2_ts + household_2_ca_2_ts + 
  foods_1_ca_2_ts + foods_2_ca_2_ts + foods_3_ca_2_ts

ca_3_ts = hobbies_ca_3_ts + household_1_ca_3_ts + household_2_ca_3_ts + 
  foods_1_ca_3_ts + foods_2_ca_3_ts + foods_3_ca_3_ts

autoplot(cbind(ca_1_ts, ca_2_ts, ca_3_ts))
# Time series for aggregated variables at product type level
hobbies_ts = hobbies_ca_1_ts + hobbies_ca_2_ts + hobbies_ca_3_ts

households_ts = household_1_ca_1_ts + household_2_ca_1_ts + household_1_ca_2_ts + 
  household_2_ca_2_ts + household_1_ca_3_ts + household_2_ca_3_ts

foods_ts = foods_1_ca_1_ts + foods_2_ca_1_ts + foods_3_ca_1_ts + 
  foods_1_ca_2_ts + foods_2_ca_2_ts + foods_3_ca_2_ts + 
  foods_1_ca_3_ts + foods_2_ca_3_ts + foods_3_ca_3_ts

autoplot(cbind(hobbies_ts,households_ts,foods_ts))

# Calender File for Exogenous Variables
calender = read.csv(file = '/Users/prajwallohan/Documents/ESSEC-CS/Courses/Term 1/Core courses/Forecasting & Predictive Analytics/Assignments/Project and data/calendar.csv',header=TRUE,sep=",")
calender$IsEvent[calender$event_name_1 == ""] = 0
calender$IsEvent[calender$event_name_1 != ""] = 1

cal_exog = cbind(snap_CA = calender$snap_CA, isEvent = calender$IsEvent)

# Create training/testing split ################################################################################################

# Split 1
test_days_start = 171
# Split 2
# test_days_start = 355
# Split 3
# test_days_start = 536

test_length = test_days_start
horizon = 1

cal_exog_train = head(cal_exog, (length(cal_exog[,1])-test_days_start))
length(cal_exog_train)
cal_exog_dumvar = head(tail(cal_exog, test_days_start), horizon)
cal_exog = cal_exog_train

# Splits for all time series
training_hobbies_ca_1_ts = head(hobbies_ca_1_ts, (length(hobbies_ca_1_ts)-test_days_start))
testing_hobbies_ca_1_ts = head(tail(hobbies_ca_1_ts, test_days_start), test_length)
hobbies_ca_1_ts = training_hobbies_ca_1_ts

training_hobbies_ca_2_ts = head(hobbies_ca_2_ts, (length(hobbies_ca_2_ts)-test_days_start))
testing_hobbies_ca_2_ts = head(tail(hobbies_ca_2_ts, test_days_start), test_length)
hobbies_ca_2_ts = training_hobbies_ca_2_ts

training_hobbies_ca_3_ts = head(hobbies_ca_3_ts, (length(hobbies_ca_3_ts)-test_days_start))
testing_hobbies_ca_3_ts = head(tail(hobbies_ca_3_ts, test_days_start), test_length)
hobbies_ca_3_ts = training_hobbies_ca_3_ts

training_household_1_ca_1_ts = head(household_1_ca_1_ts, (length(household_1_ca_1_ts)-test_days_start))
testing_household_1_ca_1_ts = head(tail(household_1_ca_1_ts, test_days_start), test_length)
household_1_ca_1_ts = training_household_1_ca_1_ts

training_household_2_ca_1_ts = head(household_2_ca_1_ts, (length(household_2_ca_1_ts)-test_days_start))
testing_household_2_ca_1_ts = head(tail(household_2_ca_1_ts, test_days_start), test_length)
household_2_ca_1_ts = training_household_2_ca_1_ts

training_household_1_ca_2_ts = head(household_1_ca_2_ts, (length(household_1_ca_2_ts)-test_days_start))
testing_household_1_ca_2_ts = head(tail(household_1_ca_2_ts, test_days_start), test_length)
household_1_ca_2_ts = training_household_1_ca_2_ts

training_household_2_ca_2_ts = head(household_2_ca_2_ts, (length(household_2_ca_2_ts)-test_days_start))
testing_household_2_ca_2_ts = head(tail(household_2_ca_2_ts, test_days_start), test_length)
household_2_ca_2_ts = training_household_2_ca_2_ts

training_household_1_ca_3_ts = head(household_1_ca_3_ts, (length(household_1_ca_3_ts)-test_days_start))
testing_household_1_ca_3_ts = head(tail(household_1_ca_3_ts, test_days_start), test_length)
household_1_ca_3_ts = training_household_1_ca_3_ts

training_household_2_ca_3_ts = head(household_2_ca_3_ts, (length(household_2_ca_3_ts)-test_days_start))
testing_household_2_ca_3_ts = head(tail(household_2_ca_3_ts, test_days_start), test_length)
household_2_ca_3_ts = training_household_2_ca_3_ts

training_foods_1_ca_1_ts = head(foods_1_ca_1_ts, (length(foods_1_ca_1_ts)-test_days_start))
testing_foods_1_ca_1_ts = head(tail(foods_1_ca_1_ts, test_days_start), test_length)
foods_1_ca_1_ts = training_foods_1_ca_1_ts

training_foods_2_ca_1_ts = head(foods_2_ca_1_ts, (length(foods_2_ca_1_ts)-test_days_start))
testing_foods_2_ca_1_ts = head(tail(foods_2_ca_1_ts, test_days_start), test_length)
foods_2_ca_1_ts = training_foods_2_ca_1_ts

training_foods_3_ca_1_ts = head(foods_3_ca_1_ts, (length(foods_3_ca_1_ts)-test_days_start))
testing_foods_3_ca_1_ts = head(tail(foods_3_ca_1_ts, test_days_start), test_length)
foods_3_ca_1_ts = training_foods_3_ca_1_ts

training_foods_1_ca_2_ts = head(foods_1_ca_2_ts, (length(foods_1_ca_2_ts)-test_days_start))
testing_foods_1_ca_2_ts = head(tail(foods_1_ca_2_ts, test_days_start), test_length)
foods_1_ca_2_ts = training_foods_1_ca_2_ts

training_foods_2_ca_2_ts = head(foods_2_ca_2_ts, (length(foods_2_ca_2_ts)-test_days_start))
testing_foods_2_ca_2_ts = head(tail(foods_2_ca_2_ts, test_days_start), test_length)
foods_2_ca_2_ts = training_foods_2_ca_2_ts

training_foods_3_ca_2_ts = head(foods_3_ca_2_ts, (length(foods_3_ca_2_ts)-test_days_start))
testing_foods_3_ca_2_ts = head(tail(foods_3_ca_2_ts, test_days_start), test_length)
foods_3_ca_2_ts = training_foods_3_ca_2_ts

training_foods_1_ca_3_ts = head(foods_1_ca_3_ts, (length(foods_1_ca_3_ts)-test_days_start))
testing_foods_1_ca_3_ts = head(tail(foods_1_ca_3_ts, test_days_start), test_length)
foods_1_ca_3_ts = training_foods_1_ca_3_ts

training_foods_2_ca_3_ts = head(foods_2_ca_3_ts, (length(foods_2_ca_3_ts)-test_days_start))
testing_foods_2_ca_3_ts = head(tail(foods_2_ca_3_ts, test_days_start), test_length)
foods_2_ca_3_ts = training_foods_2_ca_3_ts

training_foods_3_ca_3_ts = head(foods_3_ca_3_ts, (length(foods_3_ca_3_ts)-test_days_start))
testing_foods_3_ca_3_ts = head(tail(foods_3_ca_3_ts, test_days_start), test_length)
foods_3_ca_3_ts = training_foods_3_ca_3_ts


# Splits for aggregated time series
training_ca_1_ts = head(ca_1_ts, (length(ca_1_ts)-test_days_start))
testing_ca_1_ts = head(tail(ca_1_ts, test_days_start), test_length)
ca_1_ts = training_ca_1_ts

training_ca_2_ts = head(ca_2_ts, (length(ca_2_ts)-test_days_start))
testing_ca_2_ts = head(tail(ca_2_ts, test_days_start), test_length)
ca_2_ts = training_ca_2_ts

training_ca_3_ts = head(ca_3_ts, (length(ca_3_ts)-test_days_start))
testing_ca_3_ts = head(tail(ca_3_ts, test_days_start), test_length)
ca_3_ts = training_ca_3_ts



training_hobbies_ts = head(hobbies_ts, (length(hobbies_ts)-test_days_start))
testing_hobbies_ts = head(tail(hobbies_ts, test_days_start), test_length)
hobbies_ts = training_hobbies_ts

training_households_ts = head(households_ts, (length(households_ts)-test_days_start))
testing_households_ts = head(tail(households_ts, test_days_start), test_length)
households_ts = training_households_ts

training_foods_ts = head(foods_ts, (length(foods_ts)-test_days_start))
testing_foods_ts = head(tail(foods_ts, test_days_start), test_length)
foods_ts = training_foods_ts


# Find optimum lag before running the VAR models ############################################################################################################

store_agg_data.bv = cbind(ca_1_ts,ca_2_ts,ca_3_ts)
print(head(store_agg_data.bv))

lagselect = VARselect(store_agg_data.bv, lag.max = 100, type="both", exogen = cal_exog)
lagselect$selection

# Building VAR model for store level aggregated data  ############################################################################################################

store_var = VAR(store_agg_data.bv, p = 29, type = "both", exogen = cal_exog)
summary(store_var)

#Checking for serial correlation  #############
store_var_serial = serial.test(store_var, type = "PT.asymptotic")
store_var_serial

# Heteroskedasticity
store_var_arch = arch.test(store_var, multivariate.only = TRUE)
store_var_arch

# Normal distribution of residuals
store_var_norm = normality.test(store_var, multivariate.only = TRUE)
store_var_norm

# Structural break in residuals
store_var_stability = stability(store_var, type = "OLS-CUSUM")
plot(store_var_stability)


# Granger causality
store_var_granger_ca_1 = causality(store_var, cause = "ca_1_ts")
store_var_granger_ca_1

store_var_granger_ca_2 = causality(store_var, cause = "ca_2_ts")
store_var_granger_ca_2

store_var_granger_ca_3 = causality(store_var, cause = "ca_3_ts")
store_var_granger_ca_3

# Impulse Response Function
ca_1_2_irf = irf(store_var, impulse = "ca_1_ts", response = "ca_2_ts", n.ahead = 30, boot = TRUE)
#plot(ca_1_2_rf)


ca_1_3_irf = irf(store_var, impulse = "ca_1_ts", response = "ca_3_ts", n.ahead = 30, boot = TRUE)
plot(ca_1_3_irf)

ca_2_1_irf = irf(store_var, impulse = "ca_2_ts", response = "ca_1_ts", n.ahead = 30, boot = TRUE)
plot(ca_2_1_irf)

ca_2_3_irf = irf(store_var, impulse = "ca_2_ts", response = "ca_3_ts", n.ahead = 30, boot = TRUE)
plot(ca_2_3_irf)

ca_3_1_irf = irf(store_var, impulse = "ca_3_ts", response = "ca_1_ts", n.ahead = 30, boot = TRUE)
plot(ca_3_1_irf)

ca_3_2_irf = irf(store_var, impulse = "ca_3_ts", response = "ca_2_ts", n.ahead = 30, boot = TRUE)
plot(ca_3_2_irf)


# Variance decomposition
store_var_fevd = fevd(store_var, n.ahead = 30)
plot(store_var_fevd)


# Forecasting  ###########
store_var_forecast = predict(store_var, n.ahead = horizon, ci = 0.95, dumvar = cal_exog_dumvar)
store_var_forecast

# Finding RMSSE for comparison  ############################################################################

store_var_rmsse=cbind(ca_1=0, ca_2=0, ca_3=0)

store_var_ca_1_preds = store_var_forecast$fcst$ca_1_ts[1:horizon]
store_var_rmsse[1] = rmsse(as.numeric(testing_ca_1_ts), store_var_ca_1_preds, as.numeric(ca_1_ts))

store_var_ca_2_preds = store_var_forecast$fcst$ca_2_ts[1:horizon]
store_var_rmsse[2] = rmsse(as.numeric(testing_ca_2_ts), store_var_ca_2_preds, as.numeric(ca_2_ts))

store_var_ca_3_preds = store_var_forecast$fcst$ca_3_ts[1:horizon]
store_var_rmsse[3] = rmsse(as.numeric(testing_ca_3_ts), store_var_ca_3_preds, as.numeric(ca_3_ts))

store_var_rmsse

# Probabilistic forecasts
cis=cbind(0.99, 0.95, 0.67, 0.5)
total_pl=cbind(ca_1=0,ca_2=0,ca_3=0)

# median case
new_forecast = predict(store_var, n.ahead = horizon, ci = 0.95, dumvar = cal_exog_dumvar)

median_preds_ca_1 = new_forecast$fcst$ca_1_ts[1:horizon]
median_preds_ca_2 = new_forecast$fcst$ca_2_ts[1:horizon]
median_preds_ca_3 = new_forecast$fcst$ca_3_ts[1:horizon]

total_pl[1] = total_pl[1] + pinball_loss(0.5,as.numeric(ca_1_ts),as.numeric(testing_ca_1_ts[1:horizon]),median_preds_ca_1,horizon)
total_pl[2] = total_pl[2] + pinball_loss(0.5,as.numeric(ca_2_ts),as.numeric(testing_ca_2_ts[1:horizon]),median_preds_ca_2,horizon)
total_pl[3] = total_pl[3] + pinball_loss(0.5,as.numeric(ca_3_ts),as.numeric(testing_ca_3_ts[1:horizon]),median_preds_ca_3,horizon)

for (i in 1:length(cis)){

  tau = (1-cis[i])/2
  
  new_forecast = predict(store_var, n.ahead = horizon, ci = cis[i], dumvar = cal_exog_dumvar)
  new_forecast$fcst$ca_1_ts[2:2]
  lower_preds_ca_1 = new_forecast$fcst$ca_1_ts[(horizon+1):(2*horizon)]
  upper_preds_ca_1 = new_forecast$fcst$ca_1_ts[(2*horizon+1):(3*horizon)]
  total_pl[1] = total_pl[1] + pinball_loss(tau,as.numeric(ca_1_ts),as.numeric(testing_ca_1_ts[1:horizon]),lower_preds_ca_1,horizon)
  total_pl[1] = total_pl[1] + pinball_loss(1-tau,as.numeric(ca_1_ts),as.numeric(testing_ca_1_ts[1:horizon]),upper_preds_ca_1,horizon)
  
  lower_preds_ca_2 = new_forecast$fcst$ca_2_ts[(horizon+1):(2*horizon)]
  upper_preds_ca_2 = new_forecast$fcst$ca_2_ts[(2*horizon+1):(3*horizon)]
  total_pl[2] = total_pl[2] + pinball_loss(tau,as.numeric(ca_2_ts),as.numeric(testing_ca_2_ts[1:horizon]),lower_preds_ca_2,horizon)
  total_pl[2] = total_pl[2] + pinball_loss(1-tau,as.numeric(ca_2_ts),as.numeric(testing_ca_2_ts[1:horizon]),upper_preds_ca_2,horizon)
  
  lower_preds_ca_3 = new_forecast$fcst$ca_3_ts[(horizon+1):(2*horizon)]
  upper_preds_ca_3 = new_forecast$fcst$ca_3_ts[(2*horizon+1):(3*horizon)]
  total_pl[3] = total_pl[3] + pinball_loss(tau,as.numeric(ca_3_ts),as.numeric(testing_ca_3_ts[1:horizon]),lower_preds_ca_3,horizon)
  total_pl[3] = total_pl[3] + pinball_loss(1-tau,as.numeric(ca_3_ts),as.numeric(testing_ca_3_ts[1:horizon]),upper_preds_ca_3,horizon)
  
}
total_pl = total_pl/((2*length(cis))+1)
print(total_pl)

# Find optimum lag before running the VAR models ############################################################################################################

prod_agg_data.bv = cbind(hobbies_ts,households_ts,foods_ts)
print(head(prod_agg_data.bv))

lagselect = VARselect(prod_agg_data.bv, lag.max = 100, type="both")
lagselect$selection


# Building VAR model for product level aggregated data  ###########################################################################################################

prod_var = VAR(prod_agg_data.bv, p = 29, type = "both", exogen = cal_exog)
summary(prod_var)


# Checking for serial correlation  ##############
prod_var_serial = serial.test(prod_var, type = "PT.asymptotic")
prod_var_serial

# Heteroskedasticity
prod_var_arch = arch.test(prod_var, multivariate.only = TRUE)
prod_var_arch

# Normal distribution of residuals
prod_var_norm = normality.test(prod_var, multivariate.only = TRUE)
prod_var_norm

# Structural break in residuals
prod_var_stability = stability(prod_var, type = "OLS-CUSUM")
plot(prod_var_stability)


# Granger causality
prod_var_granger_hobbies = causality(prod_var, cause = "hobbies_ts")
prod_var_granger_hobbies

prod_var_granger_households = causality(prod_var, cause = "households_ts")
prod_var_granger_households

prod_var_granger_foods = causality(prod_var, cause = "foods_ts")
prod_var_granger_foods

# Impulse Response Function
hobbies_foods_irf = irf(prod_var, impulse = "hobbies_ts", response = "foods_ts", n.ahead = 30, boot = TRUE)
plot(hobbies_foods_irf)

hobbies_households_irf = irf(prod_var, impulse = "hobbies_ts", response = "households_ts", n.ahead = 30, boot = TRUE)
plot(hobbies_households_irf)

households_foods_irf = irf(prod_var, impulse = "households_ts", response = "foods_ts", n.ahead = 30, boot = TRUE)
plot(households_foods_irf)

households_hobbies_irf = irf(prod_var, impulse = "households_ts", response = "hobbies_ts", n.ahead = 30, boot = TRUE)
plot(households_hobbies_irf)

foods_households_irf = irf(prod_var, impulse = "foods_ts", response = "households_ts", n.ahead = 30, boot = TRUE)
plot(foods_households_irf)

foods_hobbies_irf = irf(prod_var, impulse = "foods_ts", response = "hobbies_ts", n.ahead = 30, boot = TRUE)
plot(foods_hobbies_irf)



# Variance decomposition
prod_var_fevd = fevd(prod_var, n.ahead = 30)
plot(prod_var_fevd)


# Forecasting #################
prod_var_forecast = predict(prod_var, n.ahead = horizon, ci = 0.95, dumvar = cal_exog_dumvar)
prod_var_forecast

# Finding RMSSE for comparison  ############################################################################

prod_var_rmsse = cbind(hobbies_ts=0, households_ts=0, foods_ts=0)

prod_var_hobbies_preds = prod_var_forecast$fcst$hobbies_ts[1:horizon]
prod_var_rmsse[1] = rmsse(as.numeric(testing_hobbies_ts), prod_var_hobbies_preds, as.numeric(hobbies_ts))

prod_var_households_preds = prod_var_forecast$fcst$households_ts[1:horizon]
prod_var_rmsse[2] = rmsse(as.numeric(testing_households_ts), prod_var_households_preds, as.numeric(households_ts))

prod_var_foods_preds = prod_var_forecast$fcst$foods_ts[1:horizon]
prod_var_rmsse[3] = rmsse(as.numeric(testing_foods_ts), prod_var_foods_preds, as.numeric(foods_ts))

prod_var_rmsse

# Probabilistic forecasts

cis=cbind(0.99, 0.95, 0.67, 0.5)
total_pl=cbind(hobbies=0,households=0,foods=0)

# median case
new_forecast = predict(prod_var, n.ahead = horizon, ci = 0.95, dumvar = cal_exog_dumvar)

median_preds_hobbies = new_forecast$fcst$hobbies_ts[1:horizon]
median_preds_households = new_forecast$fcst$households_ts[1:horizon]
median_preds_foods = new_forecast$fcst$foods_ts[1:horizon]

total_pl[1] = total_pl[1] + pinball_loss(0.5,as.numeric(hobbies_ts),as.numeric(testing_hobbies_ts[1:horizon]),median_preds_hobbies,horizon)
total_pl[2] = total_pl[2] + pinball_loss(0.5,as.numeric(households_ts),as.numeric(testing_households_ts[1:horizon]),median_preds_households,horizon)
total_pl[3] = total_pl[3] + pinball_loss(0.5,as.numeric(foods_ts),as.numeric(testing_foods_ts[1:horizon]),median_preds_foods,horizon)

for (i in 1:length(cis)){
  
  tau = (1-cis[i])/2
  
  new_forecast = predict(prod_var, n.ahead = horizon, ci = cis[i], dumvar = cal_exog_dumvar)
  new_forecast$fcst$hobbies_ts[2:2]
  lower_preds_hobbies = new_forecast$fcst$hobbies_ts[(horizon+1):(2*horizon)]
  upper_preds_hobbies = new_forecast$fcst$hobbies_ts[(2*horizon+1):(3*horizon)]
  total_pl[1] = total_pl[1] + pinball_loss(tau,as.numeric(hobbies_ts),as.numeric(testing_hobbies_ts[1:horizon]),lower_preds_hobbies,horizon)
  total_pl[1] = total_pl[1] + pinball_loss(1-tau,as.numeric(hobbies_ts),as.numeric(testing_hobbies_ts[1:horizon]),upper_preds_hobbies,horizon)
  
  lower_preds_households = new_forecast$fcst$households_ts[(horizon+1):(2*horizon)]
  upper_preds_households = new_forecast$fcst$households_ts[(2*horizon+1):(3*horizon)]
  total_pl[2] = total_pl[2] + pinball_loss(tau,as.numeric(households_ts),as.numeric(testing_households_ts[1:horizon]),lower_preds_households,horizon)
  total_pl[2] = total_pl[2] + pinball_loss(1-tau,as.numeric(households_ts),as.numeric(testing_households_ts[1:horizon]),upper_preds_households,horizon)
  
  lower_preds_foods = new_forecast$fcst$foods_ts[(horizon+1):(2*horizon)]
  upper_preds_foods = new_forecast$fcst$foods_ts[(2*horizon+1):(3*horizon)]
  total_pl[3] = total_pl[3] + pinball_loss(tau,as.numeric(foods_ts),as.numeric(testing_foods_ts[1:horizon]),lower_preds_foods,horizon)
  total_pl[3] = total_pl[3] + pinball_loss(1-tau,as.numeric(foods_ts),as.numeric(testing_foods_ts[1:horizon]),upper_preds_foods,horizon)
  
}
total_pl = total_pl/((2*length(cis))+1)
print(total_pl)


# Find optimum lag before running the Large VAR model ############################################################################################################

complete_data.bv = cbind(hobbies_ca_1_ts, hobbies_ca_2_ts, hobbies_ca_3_ts, 
                         household_1_ca_1_ts, household_2_ca_1_ts, household_1_ca_2_ts, 
                         household_2_ca_2_ts, household_1_ca_3_ts, household_2_ca_3_ts, 
                         foods_1_ca_1_ts, foods_2_ca_1_ts, foods_3_ca_1_ts, 
                         foods_1_ca_2_ts, foods_2_ca_2_ts, foods_3_ca_2_ts, 
                         foods_1_ca_3_ts, foods_2_ca_3_ts, foods_3_ca_3_ts)
print(head(complete_data.bv))

lagselect = VARselect(complete_data.bv, lag.max = 30, type="both")
lagselect$selection


# Building VAR model for Large VAR model  ###########################################################################################################

complete_var = VAR(complete_data.bv, p = 7, type = "both", exogen = cal_exog)
summary(complete_var)

##################
#Checking for serial correlation
complete_var_serial = serial.test(complete_var, type = "PT.asymptotic")
complete_var_serial


# Heteroskedasticity
complete_var_arch = arch.test(complete_var, multivariate.only = TRUE)
complete_var_arch


# Normal distribution of residuals
complete_var_norm = normality.test(complete_var, multivariate.only = TRUE)
complete_var_norm


# Structural break in residuals
complete_var_stability = stability(complete_var, type = "OLS-CUSUM")
plot(complete_var_stability)



# Granger causality
complete_var_granger_hobbies_ca_1 = causality(complete_var, cause = "hobbies_ca_1_ts")
complete_var_granger_hobbies_ca_1

complete_var_granger_hobbies_ca_2 = causality(complete_var, cause = "hobbies_ca_2_ts")
complete_var_granger_hobbies_ca_2

complete_var_granger_hobbies_ca_3 = causality(complete_var, cause = "hobbies_ca_3_ts")
complete_var_granger_hobbies_ca_3

complete_var_granger_household_1_ca_1 = causality(complete_var, cause = "household_1_ca_1_ts")
complete_var_granger_household_1_ca_1

complete_var_granger_household_2_ca_1 = causality(complete_var, cause = "household_2_ca_1_ts")
complete_var_granger_household_2_ca_1

complete_var_granger_household_1_ca_2 = causality(complete_var, cause = "household_1_ca_2_ts")
complete_var_granger_household_1_ca_2

complete_var_granger_household_2_ca_2 = causality(complete_var, cause = "household_2_ca_2_ts")
complete_var_granger_household_2_ca_2

complete_var_granger_household_1_ca_3 = causality(complete_var, cause = "household_1_ca_3_ts")
complete_var_granger_household_1_ca_3

complete_var_granger_household_2_ca_3 = causality(complete_var, cause = "household_2_ca_3_ts")
complete_var_granger_household_2_ca_3

complete_var_granger_foods_1_ca_1 = causality(complete_var, cause = "foods_1_ca_1_ts")
complete_var_granger_foods_1_ca_1

complete_var_granger_foods_2_ca_1 = causality(complete_var, cause = "foods_2_ca_1_ts")
complete_var_granger_foods_2_ca_1

complete_var_granger_foods_3_ca_1 = causality(complete_var, cause = "foods_3_ca_1_ts")
complete_var_granger_foods_3_ca_1

complete_var_granger_foods_1_ca_2 = causality(complete_var, cause = "foods_1_ca_2_ts")
complete_var_granger_foods_1_ca_2

complete_var_granger_foods_2_ca_2 = causality(complete_var, cause = "foods_2_ca_2_ts")
complete_var_granger_foods_2_ca_2

complete_var_granger_foods_3_ca_2 = causality(complete_var, cause = "foods_3_ca_2_ts")
complete_var_granger_foods_3_ca_2

complete_var_granger_foods_1_ca_3 = causality(complete_var, cause = "foods_1_ca_3_ts")
complete_var_granger_foods_1_ca_3

complete_var_granger_foods_2_ca_3 = causality(complete_var, cause = "foods_2_ca_3_ts")
complete_var_granger_foods_2_ca_3

complete_var_granger_foods_3_ca_3 = causality(complete_var, cause = "foods_3_ca_3_ts")
complete_var_granger_foods_3_ca_3



# Forecasting ##################
complete_var_forecast = predict(complete_var, n.ahead = horizon, ci = 0.95, dumvar = cal_exog_dumvar)

complete_var_forecast


# Finding RMSSE for comparison  ############################################################################

complete_rmsse=cbind(hobbies_ca_1=0, household_1_ca_1=0, household_2_ca_1=0,
               foods_1_ca_1=0, foods_2_ca_1=0, foods_3_ca_1=0,
               hobbies_ca_2=0, household_1_ca_2=0, household_2_ca_2=0,
               foods_1_ca_2=0, foods_2_ca_2=0, foods_3_ca_2=0,
               hobbies_ca_3=0, household_1_ca_3=0, household_2_ca_3=0,
               foods_1_ca_3=0, foods_2_ca_3=0, foods_3_ca_3=0)

complete_var_hobbies_ca_1_preds = complete_var_forecast$fcst$hobbies_ca_1_ts[1:horizon]
complete_rmsse[1] = rmsse(as.numeric(testing_hobbies_ca_1_ts), complete_var_hobbies_ca_1_preds, as.numeric(hobbies_ca_1_ts))

complete_var_household_1_ca_1_preds = complete_var_forecast$fcst$household_1_ca_1_ts[1:horizon]
complete_rmsse[2] = rmsse(as.numeric(testing_household_1_ca_1_ts), complete_var_household_1_ca_1_preds, as.numeric(household_1_ca_1_ts))


complete_var_household_2_ca_1_preds = complete_var_forecast$fcst$household_2_ca_1_ts[1:horizon]
complete_rmsse[3] = rmsse(as.numeric(testing_household_2_ca_1_ts), complete_var_household_2_ca_1_preds, as.numeric(household_2_ca_1_ts))


complete_var_foods_1_ca_1_preds = complete_var_forecast$fcst$foods_1_ca_1_ts[1:horizon]
complete_rmsse[4] = rmsse(as.numeric(testing_foods_1_ca_1_ts), complete_var_foods_1_ca_1_preds, as.numeric(foods_1_ca_1_ts))


complete_var_foods_2_ca_1_preds = complete_var_forecast$fcst$foods_2_ca_1_ts[1:horizon]
complete_rmsse[5] = rmsse(as.numeric(testing_foods_2_ca_1_ts), complete_var_foods_2_ca_1_preds, as.numeric(foods_2_ca_1_ts))


complete_var_foods_3_ca_1_preds = complete_var_forecast$fcst$foods_3_ca_1_ts[1:horizon]
complete_rmsse[6] = rmsse(as.numeric(testing_foods_3_ca_1_ts), complete_var_foods_3_ca_1_preds, as.numeric(foods_3_ca_1_ts))





complete_var_hobbies_ca_2_preds = complete_var_forecast$fcst$hobbies_ca_2_ts[1:horizon]
complete_rmsse[7] = rmsse(as.numeric(testing_hobbies_ca_2_ts), complete_var_hobbies_ca_2_preds, as.numeric(hobbies_ca_2_ts))


complete_var_household_1_ca_2_preds = complete_var_forecast$fcst$household_1_ca_2_ts[1:horizon]
complete_rmsse[8] = rmsse(as.numeric(testing_household_1_ca_2_ts), complete_var_household_1_ca_2_preds, as.numeric(household_1_ca_2_ts))


complete_var_household_2_ca_2_preds = complete_var_forecast$fcst$household_2_ca_2_ts[1:horizon]
complete_rmsse[9] = rmsse(as.numeric(testing_household_2_ca_2_ts), complete_var_household_2_ca_2_preds, as.numeric(household_2_ca_2_ts))


complete_var_foods_1_ca_2_preds = complete_var_forecast$fcst$foods_1_ca_2_ts[1:horizon]
complete_rmsse[10] = rmsse(as.numeric(testing_foods_1_ca_2_ts), complete_var_foods_1_ca_2_preds, as.numeric(foods_1_ca_2_ts))


complete_var_foods_2_ca_2_preds = complete_var_forecast$fcst$foods_2_ca_2_ts[1:horizon]
complete_rmsse[11] = rmsse(as.numeric(testing_foods_2_ca_2_ts), complete_var_foods_2_ca_2_preds, as.numeric(foods_2_ca_2_ts))


complete_var_foods_3_ca_2_preds = complete_var_forecast$fcst$foods_3_ca_2_ts[1:horizon]
complete_rmsse[12] = rmsse(as.numeric(testing_foods_3_ca_2_ts), complete_var_foods_3_ca_2_preds, as.numeric(foods_3_ca_2_ts))





complete_var_hobbies_ca_3_preds = complete_var_forecast$fcst$hobbies_ca_3_ts[1:horizon]
complete_rmsse[13] = rmsse(as.numeric(testing_hobbies_ca_3_ts), complete_var_hobbies_ca_3_preds, as.numeric(hobbies_ca_3_ts))


complete_var_household_1_ca_3_preds = complete_var_forecast$fcst$household_1_ca_3_ts[1:horizon]
complete_rmsse[14] = rmsse(as.numeric(testing_household_1_ca_3_ts), complete_var_household_1_ca_3_preds, as.numeric(household_1_ca_3_ts))


complete_var_household_2_ca_3_preds = complete_var_forecast$fcst$household_2_ca_3_ts[1:horizon]
complete_rmsse[15] = rmsse(as.numeric(testing_household_2_ca_3_ts), complete_var_household_2_ca_3_preds, as.numeric(household_2_ca_3_ts))


complete_var_foods_1_ca_3_preds = complete_var_forecast$fcst$foods_1_ca_3_ts[1:horizon]
complete_rmsse[16] = rmsse(as.numeric(testing_foods_1_ca_3_ts), complete_var_foods_1_ca_3_preds, as.numeric(foods_1_ca_3_ts))


complete_var_foods_2_ca_3_preds = complete_var_forecast$fcst$foods_2_ca_3_ts[1:horizon]
complete_rmsse[17] = rmsse(as.numeric(testing_foods_2_ca_3_ts), complete_var_foods_2_ca_3_preds, as.numeric(foods_2_ca_3_ts))


complete_var_foods_3_ca_3_preds = complete_var_forecast$fcst$foods_3_ca_3_ts[1:horizon]
complete_rmsse[18] = rmsse(as.numeric(testing_foods_3_ca_3_ts), complete_var_foods_3_ca_3_preds, as.numeric(foods_3_ca_3_ts))

complete_rmsse


# Probabilistic forecasts

cis=cbind(0.99, 0.95, 0.67, 0.5)
total_pl=cbind(hobbies_ca_1=0, household_1_ca_1=0, household_2_ca_1=0,
               foods_1_ca_1=0, foods_2_ca_1=0, foods_3_ca_1=0,
               hobbies_ca_2=0, household_1_ca_2=0, household_2_ca_2=0,
               foods_1_ca_2=0, foods_2_ca_2=0, foods_3_ca_2=0,
               hobbies_ca_3=0, household_1_ca_3=0, household_2_ca_3=0,
               foods_1_ca_3=0, foods_2_ca_3=0, foods_3_ca_3=0)

# median case
new_forecast = predict(complete_var, n.ahead = horizon, ci = 0.95, dumvar = cal_exog_dumvar)

median_preds_hobbies_ca_1 = new_forecast$fcst$hobbies_ca_1_ts[1:horizon]
median_preds_household_1_ca_1 = new_forecast$fcst$household_1_ca_1_ts[1:horizon]
median_preds_household_2_ca_1 = new_forecast$fcst$household_2_ca_1_ts[1:horizon]
median_preds_foods_1_ca_1 = new_forecast$fcst$foods_1_ca_1_ts[1:horizon]
median_preds_foods_2_ca_1 = new_forecast$fcst$foods_2_ca_1_ts[1:horizon]
median_preds_foods_3_ca_1 = new_forecast$fcst$foods_3_ca_1_ts[1:horizon]

median_preds_hobbies_ca_2 = new_forecast$fcst$hobbies_ca_2_ts[1:horizon]
median_preds_household_1_ca_2 = new_forecast$fcst$household_1_ca_2_ts[1:horizon]
median_preds_household_2_ca_2 = new_forecast$fcst$household_2_ca_2_ts[1:horizon]
median_preds_foods_1_ca_2 = new_forecast$fcst$foods_1_ca_2_ts[1:horizon]
median_preds_foods_2_ca_2 = new_forecast$fcst$foods_2_ca_2_ts[1:horizon]
median_preds_foods_3_ca_2 = new_forecast$fcst$foods_3_ca_2_ts[1:horizon]

median_preds_hobbies_ca_3 = new_forecast$fcst$hobbies_ca_3_ts[1:horizon]
median_preds_household_1_ca_3 = new_forecast$fcst$household_1_ca_3_ts[1:horizon]
median_preds_household_2_ca_3 = new_forecast$fcst$household_2_ca_3_ts[1:horizon]
median_preds_foods_1_ca_3 = new_forecast$fcst$foods_1_ca_3_ts[1:horizon]
median_preds_foods_2_ca_3 = new_forecast$fcst$foods_2_ca_3_ts[1:horizon]
median_preds_foods_3_ca_3 = new_forecast$fcst$foods_3_ca_3_ts[1:horizon]

total_pl[1] = total_pl[1] + pinball_loss(0.5,as.numeric(hobbies_ca_1_ts),as.numeric(testing_hobbies_ca_1_ts[1:horizon]),median_preds_hobbies_ca_1,horizon)
total_pl[2] = total_pl[2] + pinball_loss(0.5,as.numeric(household_1_ca_1_ts),as.numeric(testing_household_1_ca_1_ts[1:horizon]),median_preds_household_1_ca_1,horizon)
total_pl[3] = total_pl[3] + pinball_loss(0.5,as.numeric(household_2_ca_1_ts),as.numeric(testing_household_2_ca_1_ts[1:horizon]),median_preds_household_2_ca_1,horizon)
total_pl[4] = total_pl[4] + pinball_loss(0.5,as.numeric(foods_1_ca_1_ts),as.numeric(testing_foods_1_ca_1_ts[1:horizon]),median_preds_foods_1_ca_1,horizon)
total_pl[5] = total_pl[5] + pinball_loss(0.5,as.numeric(foods_2_ca_1_ts),as.numeric(testing_foods_2_ca_1_ts[1:horizon]),median_preds_foods_2_ca_1,horizon)
total_pl[6] = total_pl[6] + pinball_loss(0.5,as.numeric(foods_3_ca_1_ts),as.numeric(testing_foods_3_ca_1_ts[1:horizon]),median_preds_foods_3_ca_1,horizon)

total_pl[7] = total_pl[7] + pinball_loss(0.5,as.numeric(hobbies_ca_2_ts),as.numeric(testing_hobbies_ca_2_ts[1:horizon]),median_preds_hobbies_ca_2,horizon)
total_pl[8] = total_pl[8] + pinball_loss(0.5,as.numeric(household_1_ca_2_ts),as.numeric(testing_household_1_ca_2_ts[1:horizon]),median_preds_household_1_ca_2,horizon)
total_pl[9] = total_pl[9] + pinball_loss(0.5,as.numeric(household_2_ca_2_ts),as.numeric(testing_household_2_ca_2_ts[1:horizon]),median_preds_household_2_ca_2,horizon)
total_pl[10] = total_pl[10] + pinball_loss(0.5,as.numeric(foods_1_ca_2_ts),as.numeric(testing_foods_1_ca_2_ts[1:horizon]),median_preds_foods_1_ca_2,horizon)
total_pl[11] = total_pl[11] + pinball_loss(0.5,as.numeric(foods_2_ca_2_ts),as.numeric(testing_foods_2_ca_2_ts[1:horizon]),median_preds_foods_2_ca_2,horizon)
total_pl[12] = total_pl[12] + pinball_loss(0.5,as.numeric(foods_3_ca_2_ts),as.numeric(testing_foods_3_ca_2_ts[1:horizon]),median_preds_foods_3_ca_2,horizon)

total_pl[13] = total_pl[13] + pinball_loss(0.5,as.numeric(hobbies_ca_3_ts),as.numeric(testing_hobbies_ca_3_ts[1:horizon]),median_preds_hobbies_ca_3,horizon)
total_pl[14] = total_pl[14] + pinball_loss(0.5,as.numeric(household_1_ca_3_ts),as.numeric(testing_household_1_ca_3_ts[1:horizon]),median_preds_household_1_ca_3,horizon)
total_pl[15] = total_pl[15] + pinball_loss(0.5,as.numeric(household_2_ca_3_ts),as.numeric(testing_household_2_ca_3_ts[1:horizon]),median_preds_household_2_ca_3,horizon)
total_pl[16] = total_pl[16] + pinball_loss(0.5,as.numeric(foods_1_ca_3_ts),as.numeric(testing_foods_1_ca_3_ts[1:horizon]),median_preds_foods_1_ca_3,horizon)
total_pl[17] = total_pl[17] + pinball_loss(0.5,as.numeric(foods_2_ca_3_ts),as.numeric(testing_foods_2_ca_3_ts[1:horizon]),median_preds_foods_2_ca_3,horizon)
total_pl[18] = total_pl[18] + pinball_loss(0.5,as.numeric(foods_3_ca_3_ts),as.numeric(testing_foods_3_ca_3_ts[1:horizon]),median_preds_foods_3_ca_3,horizon)

for (i in 1:length(cis)){
  
  tau = (1-cis[i])/2
  
  new_forecast = predict(complete_var, n.ahead = horizon, ci = cis[i], dumvar = cal_exog_dumvar)
  
  lower_preds_hobbies_ca_1_ts = new_forecast$fcst$hobbies_ca_1_ts[(horizon+1):(2*horizon)]
  upper_preds_hobbies_ca_1_ts = new_forecast$fcst$hobbies_ca_1_ts[(2*horizon+1):(3*horizon)]
  total_pl[1] = total_pl[1] + pinball_loss(tau,as.numeric(hobbies_ca_1_ts),as.numeric(testing_hobbies_ca_1_ts[1:horizon]),lower_preds_hobbies_ca_1_ts,horizon)
  total_pl[1] = total_pl[1] + pinball_loss(1-tau,as.numeric(hobbies_ca_1_ts),as.numeric(testing_hobbies_ca_1_ts[1:horizon]),upper_preds_hobbies_ca_1_ts,horizon)
  
  lower_preds_household_1_ca_1 = new_forecast$fcst$household_1_ca_1_ts[(horizon+1):(2*horizon)]
  upper_preds_household_1_ca_1 = new_forecast$fcst$household_1_ca_1_ts[(2*horizon+1):(3*horizon)]
  total_pl[2] = total_pl[2] + pinball_loss(tau,as.numeric(household_1_ca_1_ts),as.numeric(testing_household_1_ca_1_ts[1:horizon]),lower_preds_household_1_ca_1,horizon)
  total_pl[2] = total_pl[2] + pinball_loss(1-tau,as.numeric(household_1_ca_1_ts),as.numeric(testing_household_1_ca_1_ts[1:horizon]),upper_preds_household_1_ca_1,horizon)
  
  lower_preds_household_2_ca_1 = new_forecast$fcst$household_2_ca_1_ts[(horizon+1):(2*horizon)]
  upper_preds_household_2_ca_1 = new_forecast$fcst$household_2_ca_1_ts[(2*horizon+1):(3*horizon)]
  total_pl[3] = total_pl[3] + pinball_loss(tau,as.numeric(household_2_ca_1_ts),as.numeric(testing_household_2_ca_1_ts[1:horizon]),lower_preds_household_2_ca_1,horizon)
  total_pl[3] = total_pl[3] + pinball_loss(1-tau,as.numeric(household_2_ca_1_ts),as.numeric(testing_household_2_ca_1_ts[1:horizon]),upper_preds_household_2_ca_1,horizon)
  
  lower_preds_foods_1_ca_1 = new_forecast$fcst$foods_1_ca_1_ts[(horizon+1):(2*horizon)]
  upper_preds_foods_1_ca_1 = new_forecast$fcst$foods_1_ca_1_ts[(2*horizon+1):(3*horizon)]
  total_pl[4] = total_pl[4] + pinball_loss(tau,as.numeric(foods_1_ca_1_ts),as.numeric(testing_foods_1_ca_1_ts[1:horizon]),lower_preds_foods_1_ca_1,horizon)
  total_pl[4] = total_pl[4] + pinball_loss(1-tau,as.numeric(foods_1_ca_1_ts),as.numeric(testing_foods_1_ca_1_ts[1:horizon]),upper_preds_foods_1_ca_1,horizon)
  
  lower_preds_foods_2_ca_1 = new_forecast$fcst$foods_2_ca_1_ts[(horizon+1):(2*horizon)]
  upper_preds_foods_2_ca_1 = new_forecast$fcst$foods_2_ca_1_ts[(2*horizon+1):(3*horizon)]
  total_pl[5] = total_pl[5] + pinball_loss(tau,as.numeric(foods_2_ca_1_ts),as.numeric(testing_foods_2_ca_1_ts[1:horizon]),lower_preds_foods_2_ca_1,horizon)
  total_pl[5] = total_pl[5] + pinball_loss(1-tau,as.numeric(foods_2_ca_1_ts),as.numeric(testing_foods_2_ca_1_ts[1:horizon]),upper_preds_foods_2_ca_1,horizon)
  
  lower_preds_foods_3_ca_1 = new_forecast$fcst$foods_3_ca_1_ts[(horizon+1):(2*horizon)]
  upper_preds_foods_3_ca_1 = new_forecast$fcst$foods_3_ca_1_ts[(2*horizon+1):(3*horizon)]
  total_pl[6] = total_pl[6] + pinball_loss(tau,as.numeric(foods_3_ca_1_ts),as.numeric(testing_foods_3_ca_1_ts[1:horizon]),lower_preds_foods_3_ca_1,horizon)
  total_pl[6] = total_pl[6] + pinball_loss(1-tau,as.numeric(foods_3_ca_1_ts),as.numeric(testing_foods_3_ca_1_ts[1:horizon]),upper_preds_foods_3_ca_1,horizon)
  

  
  lower_preds_hobbies_ca_2_ts = new_forecast$fcst$hobbies_ca_2_ts[(horizon+1):(2*horizon)]
  upper_preds_hobbies_ca_2_ts = new_forecast$fcst$hobbies_ca_2_ts[(2*horizon+1):(3*horizon)]
  total_pl[7] = total_pl[7] + pinball_loss(tau,as.numeric(hobbies_ca_2_ts),as.numeric(testing_hobbies_ca_2_ts[1:horizon]),lower_preds_hobbies_ca_2_ts,horizon)
  total_pl[7] = total_pl[7] + pinball_loss(1-tau,as.numeric(hobbies_ca_2_ts),as.numeric(testing_hobbies_ca_2_ts[1:horizon]),upper_preds_hobbies_ca_2_ts,horizon)
  
  lower_preds_household_1_ca_2 = new_forecast$fcst$household_1_ca_2_ts[(horizon+1):(2*horizon)]
  upper_preds_household_1_ca_2 = new_forecast$fcst$household_1_ca_2_ts[(2*horizon+1):(3*horizon)]
  total_pl[8] = total_pl[8] + pinball_loss(tau,as.numeric(household_1_ca_2_ts),as.numeric(testing_household_1_ca_2_ts[1:horizon]),lower_preds_household_1_ca_2,horizon)
  total_pl[8] = total_pl[8] + pinball_loss(1-tau,as.numeric(household_1_ca_2_ts),as.numeric(testing_household_1_ca_2_ts[1:horizon]),upper_preds_household_1_ca_2,horizon)
  
  lower_preds_household_2_ca_2 = new_forecast$fcst$household_2_ca_2_ts[(horizon+1):(2*horizon)]
  upper_preds_household_2_ca_2 = new_forecast$fcst$household_2_ca_2_ts[(2*horizon+1):(3*horizon)]
  total_pl[9] = total_pl[9] + pinball_loss(tau,as.numeric(household_2_ca_2_ts),as.numeric(testing_household_2_ca_2_ts[1:horizon]),lower_preds_household_2_ca_2,horizon)
  total_pl[9] = total_pl[9] + pinball_loss(1-tau,as.numeric(household_2_ca_2_ts),as.numeric(testing_household_2_ca_2_ts[1:horizon]),upper_preds_household_2_ca_2,horizon)
  
  lower_preds_foods_1_ca_2 = new_forecast$fcst$foods_1_ca_2_ts[(horizon+1):(2*horizon)]
  upper_preds_foods_1_ca_2 = new_forecast$fcst$foods_1_ca_2_ts[(2*horizon+1):(3*horizon)]
  total_pl[10] = total_pl[10] + pinball_loss(tau,as.numeric(foods_1_ca_2_ts),as.numeric(testing_foods_1_ca_2_ts[1:horizon]),lower_preds_foods_1_ca_2,horizon)
  total_pl[10] = total_pl[10] + pinball_loss(1-tau,as.numeric(foods_1_ca_2_ts),as.numeric(testing_foods_1_ca_2_ts[1:horizon]),upper_preds_foods_1_ca_2,horizon)
  
  lower_preds_foods_2_ca_2 = new_forecast$fcst$foods_2_ca_2_ts[(horizon+1):(2*horizon)]
  upper_preds_foods_2_ca_2 = new_forecast$fcst$foods_2_ca_2_ts[(2*horizon+1):(3*horizon)]
  total_pl[11] = total_pl[11] + pinball_loss(tau,as.numeric(foods_2_ca_2_ts),as.numeric(testing_foods_2_ca_2_ts[1:horizon]),lower_preds_foods_2_ca_2,horizon)
  total_pl[11] = total_pl[11] + pinball_loss(1-tau,as.numeric(foods_2_ca_2_ts),as.numeric(testing_foods_2_ca_2_ts[1:horizon]),upper_preds_foods_2_ca_2,horizon)
  
  lower_preds_foods_3_ca_2 = new_forecast$fcst$foods_3_ca_2_ts[(horizon+1):(2*horizon)]
  upper_preds_foods_3_ca_2 = new_forecast$fcst$foods_3_ca_2_ts[(2*horizon+1):(3*horizon)]
  total_pl[12] = total_pl[12] + pinball_loss(tau,as.numeric(foods_3_ca_2_ts),as.numeric(testing_foods_3_ca_2_ts[1:horizon]),lower_preds_foods_3_ca_2,horizon)
  total_pl[12] = total_pl[12] + pinball_loss(1-tau,as.numeric(foods_3_ca_2_ts),as.numeric(testing_foods_3_ca_2_ts[1:horizon]),upper_preds_foods_3_ca_2,horizon)
  
  
  
  
  lower_preds_hobbies_ca_3_ts = new_forecast$fcst$hobbies_ca_3_ts[(horizon+1):(2*horizon)]
  upper_preds_hobbies_ca_3_ts = new_forecast$fcst$hobbies_ca_3_ts[(2*horizon+1):(3*horizon)]
  total_pl[13] = total_pl[13] + pinball_loss(tau,as.numeric(hobbies_ca_3_ts),as.numeric(testing_hobbies_ca_3_ts[1:horizon]),lower_preds_hobbies_ca_3_ts,horizon)
  total_pl[13] = total_pl[13] + pinball_loss(1-tau,as.numeric(hobbies_ca_3_ts),as.numeric(testing_hobbies_ca_3_ts[1:horizon]),upper_preds_hobbies_ca_3_ts,horizon)
  
  lower_preds_household_1_ca_3 = new_forecast$fcst$household_1_ca_3_ts[(horizon+1):(2*horizon)]
  upper_preds_household_1_ca_3 = new_forecast$fcst$household_1_ca_3_ts[(2*horizon+1):(3*horizon)]
  total_pl[14] = total_pl[14] + pinball_loss(tau,as.numeric(household_1_ca_3_ts),as.numeric(testing_household_1_ca_3_ts[1:horizon]),lower_preds_household_1_ca_3,horizon)
  total_pl[14] = total_pl[14] + pinball_loss(1-tau,as.numeric(household_1_ca_3_ts),as.numeric(testing_household_1_ca_3_ts[1:horizon]),upper_preds_household_1_ca_3,horizon)
  
  lower_preds_household_2_ca_3 = new_forecast$fcst$household_2_ca_3_ts[(horizon+1):(2*horizon)]
  upper_preds_household_2_ca_3 = new_forecast$fcst$household_2_ca_3_ts[(2*horizon+1):(3*horizon)]
  total_pl[15] = total_pl[15] + pinball_loss(tau,as.numeric(household_2_ca_3_ts),as.numeric(testing_household_2_ca_3_ts[1:horizon]),lower_preds_household_2_ca_3,horizon)
  total_pl[15] = total_pl[15] + pinball_loss(1-tau,as.numeric(household_2_ca_3_ts),as.numeric(testing_household_2_ca_3_ts[1:horizon]),upper_preds_household_2_ca_3,horizon)
  
  lower_preds_foods_1_ca_3 = new_forecast$fcst$foods_1_ca_3_ts[(horizon+1):(2*horizon)]
  upper_preds_foods_1_ca_3 = new_forecast$fcst$foods_1_ca_3_ts[(2*horizon+1):(3*horizon)]
  total_pl[16] = total_pl[16] + pinball_loss(tau,as.numeric(foods_1_ca_3_ts),as.numeric(testing_foods_1_ca_3_ts[1:horizon]),lower_preds_foods_1_ca_3,horizon)
  total_pl[16] = total_pl[16] + pinball_loss(1-tau,as.numeric(foods_1_ca_3_ts),as.numeric(testing_foods_1_ca_3_ts[1:horizon]),upper_preds_foods_1_ca_3,horizon)
  
  lower_preds_foods_2_ca_3 = new_forecast$fcst$foods_2_ca_3_ts[(horizon+1):(2*horizon)]
  upper_preds_foods_2_ca_3 = new_forecast$fcst$foods_2_ca_3_ts[(2*horizon+1):(3*horizon)]
  total_pl[17] = total_pl[17] + pinball_loss(tau,as.numeric(foods_2_ca_3_ts),as.numeric(testing_foods_2_ca_3_ts[1:horizon]),lower_preds_foods_2_ca_3,horizon)
  total_pl[17] = total_pl[17] + pinball_loss(1-tau,as.numeric(foods_2_ca_3_ts),as.numeric(testing_foods_2_ca_3_ts[1:horizon]),upper_preds_foods_2_ca_3,horizon)
  
  lower_preds_foods_3_ca_3 = new_forecast$fcst$foods_3_ca_3_ts[(horizon+1):(2*horizon)]
  upper_preds_foods_3_ca_3 = new_forecast$fcst$foods_3_ca_3_ts[(2*horizon+1):(3*horizon)]
  total_pl[18] = total_pl[18] + pinball_loss(tau,as.numeric(foods_3_ca_3_ts),as.numeric(testing_foods_3_ca_3_ts[1:horizon]),lower_preds_foods_3_ca_3,horizon)
  total_pl[18] = total_pl[18] + pinball_loss(1-tau,as.numeric(foods_3_ca_3_ts),as.numeric(testing_foods_3_ca_3_ts[1:horizon]),upper_preds_foods_3_ca_3,horizon)
  
  
  }
total_pl = total_pl/((2*length(cis))+1)
print(total_pl)



############################################################################################################

############################################################################################################
