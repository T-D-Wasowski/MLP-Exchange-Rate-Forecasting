library("readxl")
library("neuralnet")
library("Metrics")

#-------------- Functions -----------------------------------------------------

#Function which splits the time-series data into input / output vectors
splitExchangeRates <-  function(data, steps) {
  
  m <- matrix(ncol = steps+1)
  
  if (steps == 2) {
    colnames(m) <- c("input1", "input2", "output")
  } else if (steps == 3) {
    colnames(m) <- c("input1", "input2", "input3", "output")
  } else if (steps == 4) {
    colnames(m) <- c("input1", "input2", "input3", "input4", "output")
  } else if (steps == 5) {
    colnames(m) <- c("input1", "input2", "input3", "input4", "input5", "output")
  } 
  
  for (i in 1:(length(data)-(steps+1))) {
    
    v <- c(data[i:(i+steps)])
    
    m <- rbind(m, v)
    
  }
  
  return(m[-1,])
  
}

#Funciton for normalisisng data
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#Function to undo normalisation
unnormalise <- function(x, min, max) { 
  return( (max - min)*x + min )
}

#Function to calculate RMSE
###rmse <- function(error)
###{
###  sqrt(mean(error^2))
###}

#-------------- Initial Data Frames -------------------------------------------

#Load data from Excel
exchangeRateData = read_excel("ExchangeUSD.xlsx")

#Just the column which only has the exchange rate value
rates <- exchangeRateData$`USD/EUR`

#-------------- Input/Output Matrices & Normalisation ----------------------------

#--- m = 2
splitRates_2 <- as.data.frame(splitExchangeRates(rates, 2))
splitRates_2
splitRatesNormalised_2 <- as.data.frame(lapply(splitRates_2, normalise))
splitRatesNormalised_2

splitRates_2_train <- splitRates_2[1:395,] #~80%
splitRates_2_test <- splitRates_2[396:497,] #~20%

splitRatesNormalised_2_train <- splitRatesNormalised_2[1:395,] #~80%
splitRatesNormalised_2_test <- splitRatesNormalised_2[396:497,] #~20%

#--- m = 3
splitRates_3 <- as.data.frame(splitExchangeRates(rates, 3))
splitRatesNormalised_3 <- as.data.frame(lapply(splitRates_3, normalise))

splitRates_3_train <- splitRates_3[1:395,] #~80%
splitRates_3_test <- splitRates_3[396:496,] #~20%

splitRatesNormalised_3_train <- splitRatesNormalised_3[1:395,] #~80%
splitRatesNormalised_3_test <- splitRatesNormalised_3[396:496,] #~20%

#--- m = 4
splitRates_4 <- as.data.frame(splitExchangeRates(rates, 4))
splitRatesNormalised_4 <- as.data.frame(lapply(splitRates_4, normalise))

splitRates_4_train <- splitRates_4[1:395,] #~80%
splitRates_4_test <- splitRates_4[396:495,] #~20%

splitRatesNormalised_4_train <- splitRatesNormalised_4[1:395,] #~80%
splitRatesNormalised_4_test <- splitRatesNormalised_4[396:495,] #~20%

#--- m = 5
splitRates_5 <- as.data.frame(splitExchangeRates(rates, 5))
splitRates_5
splitRatesNormalised_5 <- as.data.frame(lapply(splitRates_5, normalise))

splitRates_5_train <- splitRates_5[1:395,] #~80%
splitRates_5_test <- splitRates_5[396:494,] #~20%

splitRatesNormalised_5_train <- splitRatesNormalised_5[1:395,] #~80%
splitRatesNormalised_5_test <- splitRatesNormalised_5[396:494,] #~20%


#-------------- NN Models & Evaluation -----------------------------------------------------

#--- hn(l1) = 1

#- i=2

#Create the NN model
set.seed(100)
exchangeModel_2_1 <- neuralnet(output ~ input1 + input2,
                           data = splitRatesNormalised_2_train,
                           hidden = 1)

#Plot the NN model
plot(exchangeModel_2_1)

#Obtain the NN model results
exchangeResults_2_1 <- compute(exchangeModel_2_1, splitRatesNormalised_2_test[1:2])

#Corellation between prediction and actual results
cor(exchangeResults_2_1$net.result, splitRatesNormalised_2_test$output) 

#Find Min and Max Value
testRates_2_min <- min(splitRates_2_train$output)
testRates_2_max <- max(splitRates_2_train$output)

exchangePrediction_2_1 <- unnormalise(exchangeResults_2_1$net.result,
                                      testRates_2_min,
                                      testRates_2_max)
exchangePrediction_2_1

#RMSE
rmse_2_1 <- rmse(splitRates_2_test$output, exchangePrediction_2_1)
rmse_2_1

#MAE
mae_2_1 <- mae(splitRates_2_test$output, exchangePrediction_2_1)
mae_2_1

#MAPE
mape_2_1 <- mape(splitRates_2_test$output, exchangePrediction_2_1)
mape_2_1

#- i=3

#Create the NN model
set.seed(100)
exchangeModel_3_1 <- neuralnet(output ~ input1 + input2 + input3,
                               data = splitRatesNormalised_3_train,
                               hidden = 1)

#Plot the NN model
plot(exchangeModel_3_1)

#Obtain the NN model results
exchangeResults_3_1 <- compute(exchangeModel_3_1, splitRatesNormalised_3_test[1:3])

#Corellation between prediction and actual results
cor(exchangeResults_3_1$net.result, splitRatesNormalised_3_test$output) 

#Find Min and Max Value
testRates_3_min <- min(splitRates_3_train$output)
testRates_3_max <- max(splitRates_3_train$output)

exchangePrediction_3_1 <- unnormalise(exchangeResults_3_1$net.result, testRates_3_min, testRates_3_max)
exchangePrediction_3_1

#RMSE
rmse_3_1 <- rmse(splitRates_3_test$output, exchangePrediction_3_1)
rmse_3_1

#MAE
mae_3_1 <- mae(splitRates_3_test$output, exchangePrediction_3_1)
mae_3_1

#MAPE
mape_3_1 <- mape(splitRates_3_test$output, exchangePrediction_3_1)
mape_3_1

#- i=4

#Create the NN model
set.seed(100)
exchangeModel_4_1 <- neuralnet(output ~ input1 + input2 + input3 + input4,
                               data = splitRatesNormalised_4_train,
                               hidden = 1)

#Plot the NN model
plot(exchangeModel_4_1)

#Obtain the NN model results
exchangeResults_4_1 <- compute(exchangeModel_4_1, splitRatesNormalised_4_test[1:4])

#Corellation between prediction and actual results
cor(exchangeResults_4_1$net.result, splitRatesNormalised_4_test$output) 

#Find Min and Max Value
testRates_4_min <- min(splitRates_4_train$output)
testRates_4_max <- max(splitRates_4_train$output)

exchangePrediction_4_1 <- unnormalise(exchangeResults_4_1$net.result, testRates_4_min, testRates_4_max)
exchangePrediction_4_1

#RMSE
rmse_4_1 <- rmse(splitRates_4_test$output, exchangePrediction_4_1)
rmse_4_1

#MAE
mae_4_1 <- mae(splitRates_4_test$output, exchangePrediction_4_1)
mae_4_1

#MAPE
mape_4_1 <- mape(splitRates_4_test$output, exchangePrediction_4_1)
mape_4_1

#Plot Results
par(mfrow=c(1,1))
plot(splitRates_4_test$output, exchangePrediction_4_1 ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')

plot(splitRates_4_test$output, type = "l", col="black", main = "Real vs Predicted")
lines(exchangePrediction_4_1, lty=2, lwd=2, col="red")

#- i=5

#Create the NN model
set.seed(100)
exchangeModel_5_1 <- neuralnet(output ~ input1 + input2 + input3 + input4 + input5,
                               data = splitRatesNormalised_5_train,
                               hidden = 1)

#Plot the NN model
plot(exchangeModel_5_1)

#Obtain the NN model results
exchangeResults_5_1 <- compute(exchangeModel_5_1, splitRatesNormalised_5_test[1:5])

#Corellation between prediction and actual results
cor(exchangeResults_5_1$net.result, splitRatesNormalised_5_test$output) 

#Find Min and Max Value
testRates_5_min <- min(splitRates_5_train$output)
testRates_5_max <- max(splitRates_5_train$output)

exchangePrediction_5_1 <- unnormalise(exchangeResults_5_1$net.result, testRates_5_min, testRates_5_max)
exchangePrediction_5_1

#RMSE
rmse_5_1 <- rmse(splitRates_5_test$output, exchangePrediction_5_1)
rmse_5_1

#MAE
mae_5_1 <- mae(splitRates_5_test$output, exchangePrediction_5_1)
mae_5_1

#MAPE
mape_5_1 <- mape(splitRates_5_test$output, exchangePrediction_5_1)
mape_5_1

#--- hn(l1) = 3

#- i=2

#Create the NN model
set.seed(100)
exchangeModel_2_3 <- neuralnet(output ~ input1 + input2,
                               data = splitRatesNormalised_2_train,
                               hidden = 3)

#Plot the NN model
plot(exchangeModel_2_3)

#Obtain the NN model results
exchangeResults_2_3 <- compute(exchangeModel_2_3, splitRatesNormalised_2_test[1:2])

#Corellation between prediction and actual results
cor(exchangeResults_2_3$net.result, splitRatesNormalised_2_test$output) 

#Find Min and Max Value
testRates_2_min <- min(splitRates_2_train$output)
testRates_2_max <- max(splitRates_2_train$output)

exchangePrediction_2_3 <- unnormalise(exchangeResults_2_3$net.result, testRates_2_min, testRates_2_max)
exchangePrediction_2_3

#RMSE
rmse_2_3 <- rmse(splitRates_2_test$output, exchangePrediction_2_3)
rmse_2_3

#MAE
mae_2_3 <- mae(splitRates_2_test$output, exchangePrediction_2_3)
mae_2_3

#MAPE
mape_2_3 <- mape(splitRates_2_test$output, exchangePrediction_2_3)
mape_2_3

#- i=3

#Create the NN model
set.seed(100)
exchangeModel_3_3 <- neuralnet(output ~ input1 + input2 + input3,
                               data = splitRatesNormalised_3_train,
                               hidden = 3)

#Plot the NN model
plot(exchangeModel_3_3)

#Obtain the NN model results
exchangeResults_3_3 <- compute(exchangeModel_3_3, splitRatesNormalised_3_test[1:3])

#Corellation between prediction and actual results
cor(exchangeResults_3_3$net.result, splitRatesNormalised_3_test$output) 

#Find Min and Max Value
testRates_3_min <- min(splitRates_3_train$output)
testRates_3_max <- max(splitRates_3_train$output)

exchangePrediction_3_3 <- unnormalise(exchangeResults_3_3$net.result, testRates_3_min, testRates_3_max)
exchangePrediction_3_3

#RMSE
rmse_3_3 <- rmse(splitRates_3_test$output, exchangePrediction_3_3)
rmse_3_3

#MAE
mae_3_3 <- mae(splitRates_3_test$output, exchangePrediction_3_3)
mae_3_3

#MAPE
mape_3_3 <- mape(splitRates_3_test$output, exchangePrediction_3_3)
mape_3_3

#- i=4

#Create the NN model
set.seed(100)
exchangeModel_4_3 <- neuralnet(output ~ input1 + input2 + input3 + input4,
                               data = splitRatesNormalised_4_train,
                               hidden = 3)

#Plot the NN model
plot(exchangeModel_4_3)

#Obtain the NN model results
exchangeResults_4_3 <- compute(exchangeModel_4_3, splitRatesNormalised_4_test[1:4])

#Corellation between prediction and actual results
cor(exchangeResults_4_3$net.result, splitRatesNormalised_4_test$output) 

#Find Min and Max Value
testRates_4_min <- min(splitRates_4_train$output)
testRates_4_max <- max(splitRates_4_train$output)

exchangePrediction_4_3 <- unnormalise(exchangeResults_4_3$net.result, testRates_4_min, testRates_4_max)
exchangePrediction_4_3

#RMSE
rmse_4_3 <- rmse(splitRates_4_test$output, exchangePrediction_4_3)
rmse_4_3

#MAE
mae_4_3 <- mae(splitRates_4_test$output, exchangePrediction_4_3)
mae_4_3

#MAPE
mape_4_3 <- mape(splitRates_4_test$output, exchangePrediction_4_3)
mape_4_3

#- i=5

#Create the NN model
set.seed(100)
exchangeModel_5_3 <- neuralnet(output ~ input1 + input2 + input3 + input4 + input5,
                               data = splitRatesNormalised_5_train,
                               hidden = 3)

#Plot the NN model
plot(exchangeModel_5_3)

#Obtain the NN model results
exchangeResults_5_3 <- compute(exchangeModel_5_3, splitRatesNormalised_5_test[1:5])

#Corellation between prediction and actual results
cor(exchangeResults_5_3$net.result, splitRatesNormalised_5_test$output) 

#Find Min and Max Value
testRates_5_min <- min(splitRates_5_train$output)
testRates_5_max <- max(splitRates_5_train$output)

exchangePrediction_5_3 <- unnormalise(exchangeResults_5_3$net.result, testRates_5_min, testRates_5_max)
exchangePrediction_5_3

#RMSE
rmse_5_3 <- rmse(splitRates_5_test$output, exchangePrediction_5_3)
rmse_5_3

#MAE
mae_5_3 <- mae(splitRates_5_test$output, exchangePrediction_5_3)
mae_5_3

#MAPE
mape_5_3 <- mape(splitRates_5_test$output, exchangePrediction_5_3)
mape_5_3

#--- hn(l1) = 5

#- i=2

#Create the NN model
set.seed(100)
exchangeModel_2_5 <- neuralnet(output ~ input1 + input2,
                               data = splitRatesNormalised_2_train,
                               hidden = 5)

#Plot the NN model
plot(exchangeModel_2_5)

#Obtain the NN model results
exchangeResults_2_5 <- compute(exchangeModel_2_5, splitRatesNormalised_2_test[1:2])

#Corellation between prediction and actual results
cor(exchangeResults_2_5$net.result, splitRatesNormalised_2_test$output) 

#Find Min and Max Value
testRates_2_min <- min(splitRates_2_train$output)
testRates_2_max <- max(splitRates_2_train$output)

exchangePrediction_2_5 <- unnormalise(exchangeResults_2_5$net.result, testRates_2_min, testRates_2_max)
exchangePrediction_2_5

#RMSE
rmse_2_5 <- rmse(splitRates_2_test$output, exchangePrediction_2_5)
rmse_2_5

#MAE
mae_2_5 <- mae(splitRates_2_test$output, exchangePrediction_2_5)
mae_2_5

#MAPE
mape_2_5 <- mape(splitRates_2_test$output, exchangePrediction_2_5)
mape_2_5

#- i=3

#Create the NN model
set.seed(100)
exchangeModel_3_5 <- neuralnet(output ~ input1 + input2 + input3,
                               data = splitRatesNormalised_3_train,
                               hidden = 5)

#Plot the NN model
plot(exchangeModel_3_5)

#Obtain the NN model results
exchangeResults_3_5 <- compute(exchangeModel_3_5, splitRatesNormalised_3_test[1:3])

#Corellation between prediction and actual results
cor(exchangeResults_3_5$net.result, splitRatesNormalised_3_test$output) 

#Find Min and Max Value
testRates_3_min <- min(splitRates_3_train$output)
testRates_3_max <- max(splitRates_3_train$output)

exchangePrediction_3_5 <- unnormalise(exchangeResults_3_5$net.result, testRates_3_min, testRates_3_max)
exchangePrediction_3_5

#RMSE
rmse_3_5 <- rmse(splitRates_3_test$output, exchangePrediction_3_5)
rmse_3_5

#MAE
mae_3_5 <- mae(splitRates_3_test$output, exchangePrediction_3_5)
mae_3_5

#MAPE
mape_3_5 <- mape(splitRates_3_test$output, exchangePrediction_3_5)
mape_3_5

#- i=4

#Create the NN model
set.seed(100)
exchangeModel_4_5 <- neuralnet(output ~ input1 + input2 + input3 + input4,
                               data = splitRatesNormalised_4_train,
                               hidden = 5)

#Plot the NN model
plot(exchangeModel_4_5)

#Obtain the NN model results
exchangeResults_4_5 <- compute(exchangeModel_4_5, splitRatesNormalised_4_test[1:4])

#Corellation between prediction and actual results
cor(exchangeResults_4_5$net.result, splitRatesNormalised_4_test$output) 

#Find Min and Max Value
testRates_4_min <- min(splitRates_4_train$output)
testRates_4_max <- max(splitRates_4_train$output)

exchangePrediction_4_5 <- unnormalise(exchangeResults_4_5$net.result, testRates_4_min, testRates_4_max)
exchangePrediction_4_5

#RMSE
rmse_4_5 <- rmse(splitRates_4_test$output, exchangePrediction_4_5)
rmse_4_5

#MAE
mae_4_5 <- mae(splitRates_4_test$output, exchangePrediction_4_5)
mae_4_5

#MAPE
mape_4_5 <- mape(splitRates_4_test$output, exchangePrediction_4_5)
mape_4_5

#- i=5

#Create the NN model
set.seed(100)
exchangeModel_5_5 <- neuralnet(output ~ input1 + input2 + input3 + input4 + input5,
                               data = splitRatesNormalised_5_train,
                               hidden = 5)

#Plot the NN model
plot(exchangeModel_5_5)

#Obtain the NN model results
exchangeResults_5_5 <- compute(exchangeModel_5_5, splitRatesNormalised_5_test[1:5])

#Corellation between prediction and actual results
cor(exchangeResults_5_5$net.result, splitRatesNormalised_5_test$output) 

#Find Min and Max Value
testRates_5_min <- min(splitRates_5_train$output)
testRates_5_max <- max(splitRates_5_train$output)

exchangePrediction_5_5 <- unnormalise(exchangeResults_5_5$net.result, testRates_5_min, testRates_5_max)
exchangePrediction_5_5

#RMSE
rmse_5_5 <- rmse(splitRates_5_test$output, exchangePrediction_5_5)
rmse_5_5

#MAE
mae_5_5 <- mae(splitRates_5_test$output, exchangePrediction_5_5)
mae_5_5

#MAPE
mape_5_5 <- mape(splitRates_5_test$output, exchangePrediction_5_5)
mape_5_5

#--- hn(l1) = 1 | hn(l2) = 1

#- i=2

#Create the NN model
set.seed(100)
exchangeModel_2_1_1 <- neuralnet(output ~ input1 + input2,
                               data = splitRatesNormalised_2_train,
                               hidden = c(1, 1))

#Plot the NN model
plot(exchangeModel_2_1_1)

#Obtain the NN model results
exchangeResults_2_1_1 <- compute(exchangeModel_2_1_1, splitRatesNormalised_2_test[1:2])

#Corellation between prediction and actual results
cor(exchangeResults_2_1_1$net.result, splitRatesNormalised_2_test$output) 

#Find Min and Max Value
testRates_2_min <- min(splitRates_2_train$output)
testRates_2_max <- max(splitRates_2_train$output)

exchangePrediction_2_1_1 <- unnormalise(exchangeResults_2_1_1$net.result, testRates_2_min, testRates_2_max)
exchangePrediction_2_1_1

#RMSE
rmse_2_1_1 <- rmse(splitRates_2_test$output, exchangePrediction_2_1_1)
rmse_2_1_1

#MAE
mae_2_1_1 <- mae(splitRates_2_test$output, exchangePrediction_2_1_1)
mae_2_1_1

#MAPE
mape_2_1_1 <- mape(splitRates_2_test$output, exchangePrediction_2_1_1)
mape_2_1_1

#- i=3

#Create the NN model
set.seed(100)
exchangeModel_3_1_1 <- neuralnet(output ~ input1 + input2 + input3,
                               data = splitRatesNormalised_3_train,
                               hidden = c(1, 1))

#Plot the NN model
plot(exchangeModel_3_1_1)

#Obtain the NN model results
exchangeResults_3_1_1 <- compute(exchangeModel_3_1_1, splitRatesNormalised_3_test[1:3])

#Corellation between prediction and actual results
cor(exchangeResults_3_1_1$net.result, splitRatesNormalised_3_test$output) 

#Find Min and Max Value
testRates_3_min <- min(splitRates_3_train$output)
testRates_3_max <- max(splitRates_3_train$output)

exchangePrediction_3_1_1 <- unnormalise(exchangeResults_3_1_1$net.result, testRates_3_min, testRates_3_max)
exchangePrediction_3_1_1

#RMSE
rmse_3_1_1 <- rmse(splitRates_3_test$output, exchangePrediction_3_1_1)
rmse_3_1_1

#MAE
mae_3_1_1 <- mae(splitRates_3_test$output, exchangePrediction_3_1_1)
mae_3_1_1

#MAPE
mape_3_1_1 <- mape(splitRates_3_test$output, exchangePrediction_3_1_1)
mape_3_1_1

#- i=4

#Create the NN model
set.seed(100)
exchangeModel_4_1_1 <- neuralnet(output ~ input1 + input2 + input3 + input4,
                               data = splitRatesNormalised_4_train,
                               hidden = c(1, 1))

#Plot the NN model
plot(exchangeModel_4_1_1)

#Obtain the NN model results
exchangeResults_4_1_1 <- compute(exchangeModel_4_1_1, splitRatesNormalised_4_test[1:4])

#Corellation between prediction and actual results
cor(exchangeResults_4_1_1$net.result, splitRatesNormalised_4_test$output) 

#Find Min and Max Value
testRates_4_min <- min(splitRates_4_train$output)
testRates_4_max <- max(splitRates_4_train$output)

exchangePrediction_4_1_1 <- unnormalise(exchangeResults_4_1_1$net.result, testRates_4_min, testRates_4_max)
exchangePrediction_4_1_1

#RMSE
rmse_4_1_1 <- rmse(splitRates_4_test$output, exchangePrediction_4_1_1)
rmse_4_1_1

#MAE
mae_4_1_1 <- mae(splitRates_4_test$output, exchangePrediction_4_1_1)
mae_4_1_1

#MAPE
mape_4_1_1 <- mape(splitRates_4_test$output, exchangePrediction_4_1_1)
mape_4_1_1

#- i=5

#Create the NN model
set.seed(100)
exchangeModel_5_1_1 <- neuralnet(output ~ input1 + input2 + input3 + input4 + input5,
                               data = splitRatesNormalised_5_train,
                               hidden = c(1, 1))

#Plot the NN model
plot(exchangeModel_5_1_1)

#Obtain the NN model results
exchangeResults_5_1_1 <- compute(exchangeModel_5_1_1, splitRatesNormalised_5_test[1:5])

#Corellation between prediction and actual results
cor(exchangeResults_5_1_1$net.result, splitRatesNormalised_5_test$output) 

#Find Min and Max Value
testRates_5_min <- min(splitRates_5_train$output)
testRates_5_max <- max(splitRates_5_train$output)

exchangePrediction_5_1_1 <- unnormalise(exchangeResults_5_1_1$net.result, testRates_5_min, testRates_5_max)
exchangePrediction_5_1_1

#RMSE
rmse_5_1_1 <- rmse(splitRates_5_test$output, exchangePrediction_5_1_1)
rmse_5_1_1

#MAE
mae_5_1_1 <- mae(splitRates_5_test$output, exchangePrediction_5_1_1)
mae_5_1_1

#MAPE
mape_5_1_1 <- mape(splitRates_5_test$output, exchangePrediction_5_1_1)
mape_5_1_1

#Plot Results
par(mfrow=c(1,1))
plot(splitRates_5_test$output, exchangePrediction_5_1_1 ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')

plot(splitRates_5_test$output, type = "l", col="black", main = "Real vs Predicted")
lines(exchangePrediction_5_1_1, lty=2, lwd=2, col="red")


#--- hn(l1) = 3 | hn(l2) = 1

#- i=2

#Create the NN model
set.seed(100)
exchangeModel_2_3_1 <- neuralnet(output ~ input1 + input2,
                                 data = splitRatesNormalised_2_train,
                                 hidden = c(3, 1))

#Plot the NN model
plot(exchangeModel_2_3_1)

#Obtain the NN model results
exchangeResults_2_3_1 <- compute(exchangeModel_2_3_1, splitRatesNormalised_2_test[1:2])

#Corellation between prediction and actual results
cor(exchangeResults_2_3_1$net.result, splitRatesNormalised_2_test$output) 

#Find Min and Max Value
testRates_2_min <- min(splitRates_2_train$output)
testRates_2_max <- max(splitRates_2_train$output)

exchangePrediction_2_3_1 <- unnormalise(exchangeResults_2_3_1$net.result, testRates_2_min, testRates_2_max)
exchangePrediction_2_3_1

#RMSE
rmse_2_3_1 <- rmse(splitRates_2_test$output, exchangePrediction_2_3_1)
rmse_2_3_1

#MAE
mae_2_3_1 <- mae(splitRates_2_test$output, exchangePrediction_2_3_1)
mae_2_3_1

#MAPE
mape_2_3_1 <- mape(splitRates_2_test$output, exchangePrediction_2_3_1)
mape_2_3_1

#- i=3

#Create the NN model
set.seed(100)
exchangeModel_3_3_1 <- neuralnet(output ~ input1 + input2 + input3,
                                 data = splitRatesNormalised_3_train,
                                 hidden = c(3, 1))

#Plot the NN model
plot(exchangeModel_3_3_1)

#Obtain the NN model results
exchangeResults_3_3_1 <- compute(exchangeModel_3_3_1, splitRatesNormalised_3_test[1:3])

#Corellation between prediction and actual results
cor(exchangeResults_3_3_1$net.result, splitRatesNormalised_3_test$output) 

#Find Min and Max Value
testRates_3_min <- min(splitRates_3_train$output)
testRates_3_max <- max(splitRates_3_train$output)

exchangePrediction_3_3_1 <- unnormalise(exchangeResults_3_3_1$net.result, testRates_3_min, testRates_3_max)
exchangePrediction_3_3_1

#RMSE
rmse_3_3_1 <- rmse(splitRates_3_test$output, exchangePrediction_3_3_1)
rmse_3_3_1

#MAE
mae_3_3_1 <- mae(splitRates_3_test$output, exchangePrediction_3_3_1)
mae_3_3_1

#MAPE
mape_3_3_1 <- mape(splitRates_3_test$output, exchangePrediction_3_3_1)
mape_3_3_1

#- i=4

#Create the NN model
set.seed(100)
exchangeModel_4_3_1 <- neuralnet(output ~ input1 + input2 + input3 + input4,
                                 data = splitRatesNormalised_4_train,
                                 hidden = c(3, 1))

#Plot the NN model
plot(exchangeModel_4_3_1)

#Obtain the NN model results
exchangeResults_4_3_1 <- compute(exchangeModel_4_3_1, splitRatesNormalised_4_test[1:4])

#Corellation between prediction and actual results
cor(exchangeResults_4_3_1$net.result, splitRatesNormalised_4_test$output) 

#Find Min and Max Value
testRates_4_min <- min(splitRates_4_train$output)
testRates_4_max <- max(splitRates_4_train$output)

exchangePrediction_4_3_1 <- unnormalise(exchangeResults_4_3_1$net.result, testRates_4_min, testRates_4_max)
exchangePrediction_4_3_1

#RMSE
rmse_4_3_1 <- rmse(splitRates_4_test$output, exchangePrediction_4_3_1)
rmse_4_3_1

#MAE
mae_4_3_1 <- mae(splitRates_4_test$output, exchangePrediction_4_3_1)
mae_4_3_1

#MAPE
mape_4_3_1 <- mape(splitRates_4_test$output, exchangePrediction_4_3_1)
mape_4_3_1

par(mfrow=c(1,1))
plot(splitRates_4_test$output, exchangePrediction_4_3_1 ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')

plot(splitRates_4_test$output, type = "l", col="black", main = "Real vs Predicted")
lines(exchangePrediction_4_3_1, lty=2, lwd=2, col="red")

#- i=5

#Create the NN model
set.seed(100)
exchangeModel_5_3_1 <- neuralnet(output ~ input1 + input2 + input3 + input4 + input5,
                                 data = splitRatesNormalised_5_train,
                                 hidden = c(3, 1))

#Plot the NN model
plot(exchangeModel_5_3_1)

#Obtain the NN model results
exchangeResults_5_3_1 <- compute(exchangeModel_5_3_1, splitRatesNormalised_5_test[1:5])

#Corellation between prediction and actual results
cor(exchangeResults_5_3_1$net.result, splitRatesNormalised_5_test$output) 

#Find Min and Max Value
testRates_5_min <- min(splitRates_5_train$output)
testRates_5_max <- max(splitRates_5_train$output)

exchangePrediction_5_3_1 <- unnormalise(exchangeResults_5_3_1$net.result, testRates_5_min, testRates_5_max)
exchangePrediction_5_3_1

#RMSE
rmse_5_3_1 <- rmse(splitRates_5_test$output, exchangePrediction_5_3_1)
rmse_5_3_1

#MAE
mae_5_3_1 <- mae(splitRates_5_test$output, exchangePrediction_5_3_1)
mae_5_3_1

#MAPE
mape_5_3_1 <- mape(splitRates_5_test$output, exchangePrediction_5_3_1)
mape_5_3_1

#--- hn(l1) = 5 | hn(l2) = 1

#- i=2

#Create the NN model
set.seed(100)
exchangeModel_2_5_1 <- neuralnet(output ~ input1 + input2,
                                 data = splitRatesNormalised_2_train,
                                 hidden = c(5, 1))

#Plot the NN model
plot(exchangeModel_2_5_1)

#Obtain the NN model results
exchangeResults_2_5_1 <- compute(exchangeModel_2_5_1, splitRatesNormalised_2_test[1:2])

#Corellation between prediction and actual results
cor(exchangeResults_2_5_1$net.result, splitRatesNormalised_2_test$output) 

#Find Min and Max Value
testRates_2_min <- min(splitRates_2_train$output)
testRates_2_max <- max(splitRates_2_train$output)

exchangePrediction_2_5_1 <- unnormalise(exchangeResults_2_5_1$net.result, testRates_2_min, testRates_2_max)
exchangePrediction_2_5_1

#RMSE
rmse_2_5_1 <- rmse(splitRates_2_test$output, exchangePrediction_2_5_1)
rmse_2_5_1

#MAE
mae_2_5_1 <- mae(splitRates_2_test$output, exchangePrediction_2_5_1)
mae_2_5_1

#MAPE
mape_2_5_1 <- mape(splitRates_2_test$output, exchangePrediction_2_5_1)
mape_2_5_1

#- i=3

#Create the NN model
set.seed(100)
exchangeModel_3_5_1 <- neuralnet(output ~ input1 + input2 + input3,
                                 data = splitRatesNormalised_3_train,
                                 hidden = c(5, 1))

#Plot the NN model
plot(exchangeModel_3_5_1)

#Obtain the NN model results
exchangeResults_3_5_1 <- compute(exchangeModel_3_5_1, splitRatesNormalised_3_test[1:3])

#Corellation between prediction and actual results
cor(exchangeResults_3_5_1$net.result, splitRatesNormalised_3_test$output) 

#Find Min and Max Value
testRates_3_min <- min(splitRates_3_train$output)
testRates_3_max <- max(splitRates_3_train$output)

exchangePrediction_3_5_1 <- unnormalise(exchangeResults_3_5_1$net.result, testRates_3_min, testRates_3_max)
exchangePrediction_3_5_1

#RMSE
rmse_3_5_1 <- rmse(splitRates_3_test$output, exchangePrediction_3_5_1)
rmse_3_5_1

#MAE
mae_3_5_1 <- mae(splitRates_3_test$output, exchangePrediction_3_5_1)
mae_3_5_1

#MAPE
mape_3_5_1 <- mape(splitRates_3_test$output, exchangePrediction_3_5_1)
mape_3_5_1

#- i=4

#Create the NN model
set.seed(100)
exchangeModel_4_5_1 <- neuralnet(output ~ input1 + input2 + input3 + input4,
                                 data = splitRatesNormalised_4_train,
                                 hidden = c(5, 1))

#Plot the NN model
plot(exchangeModel_4_5_1)

#Obtain the NN model results
exchangeResults_4_5_1 <- compute(exchangeModel_4_5_1, splitRatesNormalised_4_test[1:4])

#Corellation between prediction and actual results
cor(exchangeResults_4_5_1$net.result, splitRatesNormalised_4_test$output) 

#Find Min and Max Value
testRates_4_min <- min(splitRates_4_train$output)
testRates_4_max <- max(splitRates_4_train$output)

exchangePrediction_4_5_1 <- unnormalise(exchangeResults_4_5_1$net.result, testRates_4_min, testRates_4_max)
exchangePrediction_4_5_1

#RMSE
rmse_4_5_1 <- rmse(splitRates_4_test$output, exchangePrediction_4_5_1)
rmse_4_5_1

#MAE
mae_4_5_1 <- mae(splitRates_4_test$output, exchangePrediction_4_5_1)
mae_4_5_1

#MAPE
mape_4_5_1 <- mape(splitRates_4_test$output, exchangePrediction_4_5_1)
mape_4_5_1

#- i=5

#Create the NN model
set.seed(100)
exchangeModel_5_5_1 <- neuralnet(output ~ input1 + input2 + input3 + input4 + input5,
                                 data = splitRatesNormalised_5_train,
                                 hidden = c(5, 1))

#Plot the NN model
plot(exchangeModel_5_5_1)

#Obtain the NN model results
exchangeResults_5_5_1 <- compute(exchangeModel_5_5_1, splitRatesNormalised_5_test[1:5])

#Corellation between prediction and actual results
cor(exchangeResults_5_5_1$net.result, splitRatesNormalised_5_test$output) 

#Find Min and Max Value
testRates_5_min <- min(splitRates_5_train$output)
testRates_5_max <- max(splitRates_5_train$output)

exchangePrediction_5_5_1 <- unnormalise(exchangeResults_5_5_1$net.result, testRates_5_min, testRates_5_max)
exchangePrediction_5_5_1

#RMSE
rmse_5_5_1 <- rmse(splitRates_5_test$output, exchangePrediction_5_5_1)
rmse_5_5_1

#MAE
mae_5_5_1 <- mae(splitRates_5_test$output, exchangePrediction_5_5_1)
mae_5_5_1

#MAPE
mape_5_5_1 <- mape(splitRates_5_test$output, exchangePrediction_5_5_1)
mape_5_5_1

#--- hn(l1) = 3 | hn(l2) = 3

#- i=2

#Create the NN model
set.seed(100)
exchangeModel_2_3_3 <- neuralnet(output ~ input1 + input2,
                                 data = splitRatesNormalised_2_train,
                                 hidden = c(3, 3))

#Plot the NN model
plot(exchangeModel_2_3_3)

#Obtain the NN model results
exchangeResults_2_3_3 <- compute(exchangeModel_2_3_3, splitRatesNormalised_2_test[1:2])

#Corellation between prediction and actual results
cor(exchangeResults_2_3_3$net.result, splitRatesNormalised_2_test$output) 

#Find Min and Max Value
testRates_2_min <- min(splitRates_2_train$output)
testRates_2_max <- max(splitRates_2_train$output)

exchangePrediction_2_3_3 <- unnormalise(exchangeResults_2_3_3$net.result, testRates_2_min, testRates_2_max)
exchangePrediction_2_3_3

#RMSE
rmse_2_3_3 <- rmse(splitRates_2_test$output, exchangePrediction_2_3_3)
rmse_2_3_3

#MAE
mae_2_3_3 <- mae(splitRates_2_test$output, exchangePrediction_2_3_3)
mae_2_3_3

#MAPE
mape_2_3_3 <- mape(splitRates_2_test$output, exchangePrediction_2_3_3)
mape_2_3_3

#- i=3

#Create the NN model
set.seed(100)
exchangeModel_3_3_3 <- neuralnet(output ~ input1 + input2 + input3,
                                 data = splitRatesNormalised_3_train,
                                 hidden = c(3, 3))

#Plot the NN model
plot(exchangeModel_3_3_3)

#Obtain the NN model results
exchangeResults_3_3_3 <- compute(exchangeModel_3_3_3, splitRatesNormalised_3_test[1:3])

#Corellation between prediction and actual results
cor(exchangeResults_3_3_3$net.result, splitRatesNormalised_3_test$output) 

#Find Min and Max Value
testRates_3_min <- min(splitRates_3_train$output)
testRates_3_max <- max(splitRates_3_train$output)

exchangePrediction_3_3_3 <- unnormalise(exchangeResults_3_3_3$net.result, testRates_3_min, testRates_3_max)
exchangePrediction_3_3_3

#RMSE
rmse_3_3_3 <- rmse(splitRates_3_test$output, exchangePrediction_3_3_3)
rmse_3_3_3

#MAE
mae_3_3_3 <- mae(splitRates_3_test$output, exchangePrediction_3_3_3)
mae_3_3_3

#MAPE
mape_3_3_3 <- mape(splitRates_3_test$output, exchangePrediction_3_3_3)
mape_3_3_3

#- i=4

#Create the NN model
set.seed(100)
exchangeModel_4_3_3 <- neuralnet(output ~ input1 + input2 + input3 + input4,
                                 data = splitRatesNormalised_4_train,
                                 hidden = c(3, 3))

#Plot the NN model
plot(exchangeModel_4_3_3)

#Obtain the NN model results
exchangeResults_4_3_3 <- compute(exchangeModel_4_3_3, splitRatesNormalised_4_test[1:4])

#Corellation between prediction and actual results
cor(exchangeResults_4_3_3$net.result, splitRatesNormalised_4_test$output) 

#Find Min and Max Value
testRates_4_min <- min(splitRates_4_train$output)
testRates_4_max <- max(splitRates_4_train$output)

exchangePrediction_4_3_3 <- unnormalise(exchangeResults_4_3_3$net.result, testRates_4_min, testRates_4_max)
exchangePrediction_4_3_3

#RMSE
rmse_4_3_3 <- rmse(splitRates_4_test$output, exchangePrediction_4_3_3)
rmse_4_3_3

#MAE
mae_4_3_3 <- mae(splitRates_4_test$output, exchangePrediction_4_3_3)
mae_4_3_3

#MAPE
mape_4_3_3 <- mape(splitRates_4_test$output, exchangePrediction_4_3_3)
mape_4_3_3

#- i=5

#Create the NN model
set.seed(100)
exchangeModel_5_3_3 <- neuralnet(output ~ input1 + input2 + input3 + input4 + input5,
                                 data = splitRatesNormalised_5_train,
                                 hidden = c(3, 3))

#Plot the NN model
plot(exchangeModel_5_3_3)

#Obtain the NN model results
exchangeResults_5_3_3 <- compute(exchangeModel_5_3_3, splitRatesNormalised_5_test[1:5])

#Corellation between prediction and actual results
cor(exchangeResults_5_3_3$net.result, splitRatesNormalised_5_test$output) 

#Find Min and Max Value
testRates_5_min <- min(splitRates_5_train$output)
testRates_5_max <- max(splitRates_5_train$output)

exchangePrediction_5_3_3 <- unnormalise(exchangeResults_5_3_3$net.result, testRates_5_min, testRates_5_max)
exchangePrediction_5_3_3

#RMSE
rmse_5_3_3 <- rmse(splitRates_5_test$output, exchangePrediction_5_3_3)
rmse_5_3_3

#MAE
mae_5_3_3 <- mae(splitRates_5_test$output, exchangePrediction_5_3_3)
mae_5_3_3

#MAPE
mape_5_3_3 <- mape(splitRates_5_test$output, exchangePrediction_5_3_3)
mape_5_3_3


#--- hn(l1) = 5 | hn(l2) = 3

#- i=2

#Create the NN model
set.seed(100)
exchangeModel_2_5_3 <- neuralnet(output ~ input1 + input2,
                                 data = splitRatesNormalised_2_train,
                                 hidden = c(5, 3))

#Plot the NN model
plot(exchangeModel_2_5_3)

#Obtain the NN model results
exchangeResults_2_5_3 <- compute(exchangeModel_2_5_3, splitRatesNormalised_2_test[1:2])

#Corellation between prediction and actual results
cor(exchangeResults_2_5_3$net.result, splitRatesNormalised_2_test$output) 

#Find Min and Max Value
testRates_2_min <- min(splitRates_2_train$output)
testRates_2_max <- max(splitRates_2_train$output)

exchangePrediction_2_5_3 <- unnormalise(exchangeResults_2_5_3$net.result, testRates_2_min, testRates_2_max)
exchangePrediction_2_5_3

#RMSE
rmse_2_5_3 <- rmse(splitRates_2_test$output, exchangePrediction_2_5_3)
rmse_2_5_3

#MAE
mae_2_5_3 <- mae(splitRates_2_test$output, exchangePrediction_2_5_3)
mae_2_5_3

#MAPE
mape_2_5_3 <- mape(splitRates_2_test$output, exchangePrediction_2_5_3)
mape_2_5_3

#- i=3

#Create the NN model
set.seed(100)
exchangeModel_3_5_3 <- neuralnet(output ~ input1 + input2 + input3,
                                 data = splitRatesNormalised_3_train,
                                 hidden = c(5, 3))

#Plot the NN model
plot(exchangeModel_3_5_3)

#Obtain the NN model results
exchangeResults_3_5_3 <- compute(exchangeModel_3_5_3, splitRatesNormalised_3_test[1:3])

#Corellation between prediction and actual results
cor(exchangeResults_3_5_3$net.result, splitRatesNormalised_3_test$output) 

#Find Min and Max Value
testRates_3_min <- min(splitRates_3_train$output)
testRates_3_max <- max(splitRates_3_train$output)

exchangePrediction_3_5_3 <- unnormalise(exchangeResults_3_5_3$net.result, testRates_3_min, testRates_3_max)
exchangePrediction_3_5_3

#RMSE
rmse_3_5_3 <- rmse(splitRates_3_test$output, exchangePrediction_3_5_3)
rmse_3_5_3

#MAE
mae_3_5_3 <- mae(splitRates_3_test$output, exchangePrediction_3_5_3)
mae_3_5_3

#MAPE
mape_3_5_3 <- mape(splitRates_3_test$output, exchangePrediction_3_5_3)
mape_3_5_3

#- i=4

#Create the NN model
set.seed(100)
exchangeModel_4_5_3 <- neuralnet(output ~ input1 + input2 + input3 + input4,
                                 data = splitRatesNormalised_4_train,
                                 hidden = c(5, 3))

#Plot the NN model
plot(exchangeModel_4_5_3)

#Obtain the NN model results
exchangeResults_4_5_3 <- compute(exchangeModel_4_5_3, splitRatesNormalised_4_test[1:4])

#Corellation between prediction and actual results
cor(exchangeResults_4_5_3$net.result, splitRatesNormalised_4_test$output) 

#Find Min and Max Value
testRates_4_min <- min(splitRates_4_train$output)
testRates_4_max <- max(splitRates_4_train$output)

exchangePrediction_4_5_3 <- unnormalise(exchangeResults_4_5_3$net.result, testRates_4_min, testRates_4_max)
exchangePrediction_4_5_3

#RMSE
rmse_4_5_3 <- rmse(splitRates_4_test$output, exchangePrediction_4_5_3)
rmse_4_5_3

#MAE
mae_4_5_3 <- mae(splitRates_4_test$output, exchangePrediction_4_5_3)
mae_4_5_3

#MAPE
mape_4_5_3 <- mape(splitRates_4_test$output, exchangePrediction_4_5_3)
mape_4_5_3

par(mfrow=c(1,1))
plot(splitRates_4_test$output, exchangePrediction_4_5_3 ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')

plot(splitRates_4_test$output, type = "l", col="black", main = "Real vs Predicted")
lines(exchangePrediction_4_5_3, lty=2, lwd=2, col="red")

#- i=5

#Create the NN model
set.seed(100)
exchangeModel_5_5_3 <- neuralnet(output ~ input1 + input2 + input3 + input4 + input5,
                                 data = splitRatesNormalised_5_train,
                                 hidden = c(5, 3))

#Plot the NN model
plot(exchangeModel_5_5_3)

#Obtain the NN model results
exchangeResults_5_5_3 <- compute(exchangeModel_5_5_3, splitRatesNormalised_5_test[1:5])

#Corellation between prediction and actual results
cor(exchangeResults_5_5_3$net.result, splitRatesNormalised_5_test$output) 

#Find Min and Max Value
testRates_5_min <- min(splitRates_5_train$output)
testRates_5_max <- max(splitRates_5_train$output)

exchangePrediction_5_5_3 <- unnormalise(exchangeResults_5_5_3$net.result, testRates_5_min, testRates_5_max)
exchangePrediction_5_5_3

#RMSE
rmse_5_5_3 <- rmse(splitRates_5_test$output, exchangePrediction_5_5_3)
rmse_5_5_3

#MAE
mae_5_5_3 <- mae(splitRates_5_test$output, exchangePrediction_5_5_3)
mae_5_5_3

#MAPE
mape_5_5_3 <- mape(splitRates_5_test$output, exchangePrediction_5_5_3)
mape_5_5_3

#--- hn(l1) = 5 | hn(l2) = 5

#- i=2

#Create the NN model
set.seed(100)
exchangeModel_2_5_5 <- neuralnet(output ~ input1 + input2,
                                 data = splitRatesNormalised_2_train,
                                 hidden = c(5, 5))

#Plot the NN model
plot(exchangeModel_2_5_5)

#Obtain the NN model results
exchangeResults_2_5_5 <- compute(exchangeModel_2_5_5, splitRatesNormalised_2_test[1:2])

#Corellation between prediction and actual results
cor(exchangeResults_2_5_5$net.result, splitRatesNormalised_2_test$output) 

#Find Min and Max Value
testRates_2_min <- min(splitRates_2_train$output)
testRates_2_max <- max(splitRates_2_train$output)

exchangePrediction_2_5_5 <- unnormalise(exchangeResults_2_5_5$net.result, testRates_2_min, testRates_2_max)
exchangePrediction_2_5_5

#RMSE
rmse_2_5_5 <- rmse(splitRates_2_test$output, exchangePrediction_2_5_5)
rmse_2_5_5

#MAE
mae_2_5_5 <- mae(splitRates_2_test$output, exchangePrediction_2_5_5)
mae_2_5_5

#MAPE
mape_2_5_5 <- mape(splitRates_2_test$output, exchangePrediction_2_5_5)
mape_2_5_5

#- i=3

#Create the NN model
set.seed(100)
exchangeModel_3_5_5 <- neuralnet(output ~ input1 + input2 + input3,
                                 data = splitRatesNormalised_3_train,
                                 hidden = c(5, 5))

#Plot the NN model
plot(exchangeModel_3_5_5)

#Obtain the NN model results
exchangeResults_3_5_5 <- compute(exchangeModel_3_5_5, splitRatesNormalised_3_test[1:3])

#Corellation between prediction and actual results
cor(exchangeResults_3_5_5$net.result, splitRatesNormalised_3_test$output) 

#Find Min and Max Value
testRates_3_min <- min(splitRates_3_train$output)
testRates_3_max <- max(splitRates_3_train$output)

exchangePrediction_3_5_5 <- unnormalise(exchangeResults_3_5_5$net.result, testRates_3_min, testRates_3_max)
exchangePrediction_3_5_5

#RMSE
rmse_3_5_5 <- rmse(splitRates_3_test$output, exchangePrediction_3_5_5)
rmse_3_5_5

#MAE
mae_3_5_5 <- mae(splitRates_3_test$output, exchangePrediction_3_5_5)
mae_3_5_5

#MAPE
mape_3_5_5 <- mape(splitRates_3_test$output, exchangePrediction_3_5_5)
mape_3_5_5

#- i=4

#Create the NN model
set.seed(100)
exchangeModel_4_5_5 <- neuralnet(output ~ input1 + input2 + input3 + input4,
                                 data = splitRatesNormalised_4_train,
                                 hidden = c(5, 5))

#Plot the NN model
plot(exchangeModel_4_5_5)

#Obtain the NN model results
exchangeResults_4_5_5 <- compute(exchangeModel_4_5_5, splitRatesNormalised_4_test[1:4])

#Corellation between prediction and actual results
cor(exchangeResults_4_5_5$net.result, splitRatesNormalised_4_test$output) 

#Find Min and Max Value
testRates_4_min <- min(splitRates_4_train$output)
testRates_4_max <- max(splitRates_4_train$output)

exchangePrediction_4_5_5 <- unnormalise(exchangeResults_4_5_5$net.result, testRates_4_min, testRates_4_max)
exchangePrediction_4_5_5

#RMSE
rmse_4_5_5 <- rmse(splitRates_4_test$output, exchangePrediction_4_5_5)
rmse_4_5_5

#MAE
mae_4_5_5 <- mae(splitRates_4_test$output, exchangePrediction_4_5_5)
mae_4_5_5

#MAPE
mape_4_5_5 <- mape(splitRates_4_test$output, exchangePrediction_4_5_5)
mape_4_5_5

#- i=5

#Create the NN model
set.seed(100)
exchangeModel_5_5_5 <- neuralnet(output ~ input1 + input2 + input3 + input4 + input5,
                                 data = splitRatesNormalised_5_train,
                                 hidden = c(5, 5))

#Plot the NN model
plot(exchangeModel_5_5_5)

#Obtain the NN model results
exchangeResults_5_5_5 <- compute(exchangeModel_5_5_5, splitRatesNormalised_5_test[1:5])

#Corellation between prediction and actual results
cor(exchangeResults_5_5_5$net.result, splitRatesNormalised_5_test$output) 

#Find Min and Max Value
testRates_5_min <- min(splitRates_5_train$output)
testRates_5_max <- max(splitRates_5_train$output)

exchangePrediction_5_5_5 <- unnormalise(exchangeResults_5_5_5$net.result, testRates_5_min, testRates_5_max)
exchangePrediction_5_5_5

#RMSE
rmse_5_5_5 <- rmse(splitRates_5_test$output, exchangePrediction_5_5_5)
rmse_5_5_5

#MAE
mae_5_5_5 <- mae(splitRates_5_test$output, exchangePrediction_5_5_5)
mae_5_5_5

#MAPE
mape_5_5_5 <- mape(splitRates_5_test$output, exchangePrediction_5_5_5)
mape_5_5_5

#-------------- OG ------------------------------------------------------------

#The below splits the data into input matrix
splitRates <- as.data.frame(splitExchangeRates(rates, 5))

#The below normalises the data
splitRatesNormalised <- as.data.frame(lapply(splitRates, normalise))

#Below we split the data into training and test data at about 80% and 20% respectively
splitRates_train <- splitRates[1:395,] #~80%
splitRates_test <- splitRates[396:494,] #~20%

splitRatesNormalised_train <- splitRatesNormalised[1:395,] #~80%
splitRatesNormalised_test <- splitRatesNormalised[396:494,] #~20%

####### MAKE INTO FUNCTION???? #########

#Create the NN model
set.seed(100)
exchangeModel <- neuralnet(output ~ input1 + input2 + input3 + input4 + input5,
                           data = splitRatesNormalised_train,
                           hidden = 5)
#Plot the NN model
plot(exchangeModel)

#Obtain the NN model results
exchangeResults <- compute(exchangeModel, splitRatesNormalised_test[1:5])

#Corellation between prediction and actual results
cor(exchangeResults$net.result, splitRatesNormalised_test$output) 

#Find actual predictions -------------

#Find non-normalised results -------------
head(exchangeResults$net.result)

#Find Min and Max Value
testRates_min <- min(splitRates_train$output)
testRates_max <- max(splitRates_train$output)

head(splitRates_train$output)

#Unnormalise
exchangePrediction <- unnormalise(exchangeResults$net.result, testRates_min, testRates_max)
exchangePrediction

#Performance index -----------------
error <- (splitRates_test$output - exchangePrediction)

#RMSE
predictionRMSE <- rmse(error)
predictionRMSE


#------ Plots -----------------------------------------------------------------

#Plot Results
par(mfrow=c(1,1))
plot(splitRates_test$output, exchangePrediction ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')

plot(splitRates_test$output, type = "l", col="black", main = "Real vs Predicted")
lines(exchangePrediction, lty=2, lwd=2, col="red")

