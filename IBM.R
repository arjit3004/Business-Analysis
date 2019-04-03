library(knitr)
library(ggplot2)
library(plotly)
library(dplyr)
library(plyr)
library(leaps)
library(ISLR)
library(glmnet)
library(stargazer)

# 1. Open the file ibm_return.csv in R and use the command summary to print a summary of the data.

setwd("E:/core/course/columbia course/16Fall/Business Analytics/hw/hw2")
IBMData = read.csv("ibm_return.csv")
IBMData$Date = as.Date(IBMData$Date,"%m/%d/%Y")
summary(IBMData)
#stargazer(IBMData,type='html')
#View(IBMData)
cnames = colnames(IBMData)

#attach(IBMData)

# 2. Divide your data into two parts: a training set (75%) and a test set (25%). Note that we cannot split the data randomly, why?

IBM.train = IBMData[1:round(0.75*length(IBMData$Date)),]
IBM.test = IBMData[-as.numeric(rownames(IBM.train)),]
#View(IBM.train)
summary(IBM.train)
summary(IBM.test)

# A: Since the IBM return data is like a time series, each list of the data is sorted by the date from the early to the late. 
# Random sampling will make this order meaningless, distort the relations of adjacent data,
# and ignore the daily, monthly, seasonal or yearly patterns. And most importantly, the return(today) = price(tomorrow) - price(today),
# so if the order of series is broken, the return is also meaningless. 

# 3. Create 4 validation tests where you use 4 months of data to fit the model and then
#    measure the performance on the following month. For each, use best subset selection
#    to find the best model. Consider subsets of sizes from 1 to 8. Which subset size is
#    best? What is your final model?

# Check the break date:
tail(IBM.train$Date,1)
IBM.test$Date[1]

# In section 2 We divide the data by 3(training) : 1 (test). We find that the break is after 4/2/2016 and before 4/3/2016.
# To keep the data of the whole month in a same group, break the sample by 4/1/2016.
IBM.train = subset(IBMData,IBMData$Date<"2013-4-1")
IBM.test = subset(IBMData,IBMData$Date>="2013-4-1")

# Check again.
tail(IBM.train$Date,1)
IBM.test$Date[1]

# Group the training data by month
IBM.train.split = split(IBM.train,f=strftime(IBM.train$Date,"%y-%m"))
#View(IBM.train.split[1])

# Create 4 validation tests and do the subset selection.
IBM.train.v <- list()
IBM.train.t <- list()

sselect <- list()
sselect.summary <- list()
sselect.coef <- list()
sselect.id <- c()
sselect.predict <- list()
sselect.mse <- c()
sselect.vnum <- c()
sselect.predict.all <- replicate(4, list())
sselect.mse.all <- replicate(4, list()) 

# define MSE function
mse <- function(pred,data) {
  return(mean((pred-data)^2))
}

#define predicting function to the regsubsets object
predict.regsubsets <- function(object, newdata, id, ...) {
  # object is the regsubsets object we want to predict from
  # id is the id of the model
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  mat[,names(coefi)] %*% coefi
}

for (i in 1:4) {
  # From the train and validation datasets:
  IBM.train.t[[i]] <- do.call("rbind",lapply(IBM.train.split[i:(i+3)],data.frame))
  IBM.train.v[[i]] <- do.call("rbind",lapply(IBM.train.split[i+4],data.frame))
  
  attach(IBM.train.t[[i]])
  
  # perform best subset selection
  sselect[[i]] <- regsubsets(Return ~. -Date, data = IBM.train.t[[i]])
  sselect.summary[[i]] <- summary(sselect[[i]])
  sselect.id[i] <- which.max(sselect.summary[[i]]$adjr2)
  sselect.coef[[i]] <- coef(sselect[[i]],sselect.id[i])
  
  sselect.predict[[i]] <- predict.regsubsets(sselect[[i]],IBM.train.v[[i]],sselect.id[i])
  sselect.mse[i] <- mse(sselect.predict[[i]],IBM.train.v[[i]]$Return)
  sselect.vnum[i] <- length(sselect.coef[[i]]) - 1
  
  
  for (j in 1:8) {
    sselect.predict.all[[i]][[j]] <- predict.regsubsets(sselect[[i]],IBM.train.v[[i]],j)
    sselect.mse.all[[i]][[j]] <- mse(sselect.predict.all[[i]][[j]],IBM.train.v[[i]]$Return)
  }
  
  detach(IBM.train.t[[i]])
}

sselect.mse
sselect.id.best = which.min(sselect.mse)
sselect.vnum
sselect.coef[[sselect.id.best]]
sselect.mse.matrix = do.call("rbind",sselect.mse.all)
sselect.mse.matrix
min(unlist(sselect.mse.all))
sselect.order = which.min(unlist(sselect.mse.all))
sselect.id_x = ceiling(sselect.order/8)
sselect.id_y = sselect.order%%8

# 4. On the same 4 validation tests, use lasso regression to find the best model. Consider
#    the values 0, .001, .01, .1, 1, 10, 100, 1000 for lambda. Which choice of lambda is the best? What
#    is your final model?

lasso <- list()
x <- list()
y <- list()
grid <- c()
cv.lasso <- list()
best_lambda <- list()
grid=append(0,10^seq(-3,3))
lasso.predict <- replicate(length(IBM.train.t), list())
lasso.mse <- replicate(length(IBM.train.t), list())

predict.lasso <- function(object, newdata, id, ...) {
  # object is the lasso object we want to predict from
  # id is the id of the model
  form = as.formula(object$call[[2]][[2]][[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object)[,id]
  mat[,names(coefi)] %*% coefi
}

for (i in 1:4) {
  attach(IBM.train.t[[i]])
  # perform lasso:
  x[[i]] <- model.matrix(Return ~ . - Date, IBM.train.t[[i]])[,-1]
  y[[i]] <- IBM.train.t[[i]]$Return
  lasso[[i]] <- glmnet(model.matrix(Return ~ . - Date, IBM.train.t[[i]])[,-1],y[[i]],alpha=1,lambda=grid)
  
  for (j in 1:length(grid)) {
    lasso.predict[[i]][[j]] <- predict.lasso(lasso[[i]],IBM.train.v[[i]],j)
    lasso.mse[[i]][[j]] <- mse(lasso.predict[[i]][[j]],IBM.train.v[[i]]$Return)
  }
  
  cv.lasso[[i]] <- cv.glmnet(x[[i]],y[[i]],alpha=1)
  best_lambda[[i]] <- cv.lasso[[i]]$lambda.min
  detach(IBM.train.t[[i]])
}

lasso.mse.matrix = do.call("rbind",lasso.mse)
lasso.mse.matrix
min(unlist(lasso.mse))
lasso.order = which.min(unlist(lasso.mse))
lasso.id_x = ceiling(lasso.order/length(grid))
lasso.id_y = lasso.order %% length(grid)

# 5. Pick one of the two models final models from the previous two questions. What is
#    the MSE of your model on the test data? How does that compare to the MSE on the
#    validation tests?

min(unlist(lasso.mse))
min(unlist(sselect.mse.all))

test.predict1 <- predict.regsubsets(sselect[[sselect.id_x]],IBM.test,sselect.id_y)
test.mse1 <- mse(test.predict1,IBM.test$Return)

test.predict2 <- predict.lasso(lasso[[lasso.id_x]],IBM.test,lasso.id_y)
test.mse2 <- mse(test.predict2,IBM.test$Return)

# Return for just holding IBM over the test peri

#6. Create a trading strategy from the model you picked. Start with $1 of investment and
#   every day select to go either long or short according to the prediction of the model.
#   What is the return of your trading strategy on the test data? Based on the results,
#   should you invest using this strategy?
attach(IBM.test)

# constract portfolio choices from expected return
# the Long-Short porfolio is 1 (long) when predict return is positive, and -1 when it is negative
porfolio_Long_Short = sign(test.predict2)

# Return for our portfolio over the test period
# daily return is porfolio_Long_Short * Return, and we need to translate that to pct change and multiply across all days
outOfSamplePerfomance = 1+porfolio_Long_Short * Return/100
APR.port = prod(outOfSamplePerfomance)

#length(outOfSamplePerfomance)

# Return for just holding IBM over the test period
IBM_long_return_train= 1+Return/100
APR.hold = prod(IBM_long_return_train)
#length(IBM_long_return_train)
#length(IBM.test$Date)

myport <- c()
hold <- c()
for (i in 1:length(outOfSamplePerfomance)) {
  myport[i] = prod(outOfSamplePerfomance[1:i])
  hold[i] = prod(IBM_long_return_train[1:i])
}

IBM.return <- data.frame(Date, hold, myport)


plot_ly(data = IBM.return, x = ~Date, y = ~hold, type = 'scatter', mode = 'lines+markers', name = 'Holding IBM') %>%
  add_trace(y = ~myport, name = 'My portfolio') %>%
  layout(title = "Performance: My portfolio VS Just holding", yaxis = list (title = "Return"))

