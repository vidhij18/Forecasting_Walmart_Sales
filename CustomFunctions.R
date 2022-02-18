

rmsse = function(actual, predicted, train){
  num = 0

  # for (i in 1:length(actual)){
  #   num = num + (actual[i] - predicted[i])**2
  # }
  # num = num/length(actual)

  for (i in 1:length(predicted)){
    num = num + (actual[i] - predicted[i])**2
  }
  num = num/length(predicted)
  
  den = 0
  for (i in 2:length(train)){
    den = den + (train[i] - train[i-1])**2
  }
  den = den/(length(train)-1)
  #return((num/den)**0.5)
  return(round((num/den)**0.5, digits = 4))
}




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
  
  return(round((num/den),digits = 4))
}
