library(knitr)
library(ggplot2)
library(plotly)
library(dplyr)

# 1. Load the data from Sugar.csv into R. You can use setwd() to set the current working directory. Print a summary of the variables.

setwd("E:/core/course/columbia course/16Fall/Business Analytics/hw/hw1")
SugarData = read.csv("Sugar.csv")
summary(SugarData)
View(SugarData)

attach(SugarData)

# 2. Run a regression to predict the rating using sugar level. Explain the results.

lm1 = lm(rating ~ sugar)
summary(lm1)
predict1 = predict(lm1,SugarData)

# 3. Run a regression to predict the rating using all the variables. Explain the results.

lm2 = lm(rating ~ sugar+rowing_team+track_team+chess_club)
summary(lm2)
predict2 = predict(lm2, SugarData)

# 4. To check whethere the results make sense, make a plot of rating vs sugar consuption. Is the plot consistent with the regression results?
plot_ly(data = SugarData, x = sugar, y = rating, mode = "markers", color = sugar) %>%
layout(title = 'Rating VS Sugar Level')

plot_ly(data = SugarData, x = sugar, y = rating, mode = "markers",name = 'data') %>%
  add_trace(data = SugarData, x = sugar, y = predict1, mode = "markers", name = 'prediction 1') %>%
  add_trace(data = SugarData, x = sugar, y = predict2, mode = "markers", name = 'prediction 2') %>%
  layout(title = 'Rating VS Sugar Level')

# 5. The marketing research suggests that people who are active in sports may react differently
# to sugary drinks. Create a new variable that indicates whether the student is
# active in at least one sports club (that is, either track or rowing). Split the data into
# two subsamples based on the value of the new variable you created and run a separate
# regression for each. What do these two regressions suggest?

sport = rowing_team | track_team
#View(sport)
summary(sport)
SugarData.adv = data.frame(SugarData,sport)
#View(SugarData.adv)
SugarData.sport = subset(SugarData.adv, sport == TRUE)
#View(SugarData.sport)
SugarData.nosport = subset(SugarData.adv, sport == FALSE)

lm3.sport = lm(SugarData.sport$rating ~ SugarData.sport$sugar)
summary(lm3.sport)
predict.sport = predict(lm3.sport,data.frame(SugarData.sport$sugar))

lm3.nosport = lm(SugarData.nosport$rating ~ SugarData.nosport$sugar)
summary(lm3.nosport)
predict.nosport = predict(lm3.nosport,data.frame(SugarData.nosport$sugar))

plot_ly(data = SugarData.sport, x = sugar, y = rating, mode = "markers", name = 'sport active') %>%
  add_trace(data = SugarData.nosport, x = sugar, y = rating, mode = "markers", name = 'sport inactive') %>%
  add_trace(data = SugarData.sport, x = sugar, y = predict.sport, mode = "lines", name = 'sport active prediction') %>%
  add_trace(data = SugarData.nosport, x = sugar, y = predict.nosport, mode = "lines", name = 'sport inactive prediction') %>%
  layout(title = 'Rating VS Sugar Level: Sport active and inactive')

# 6. Run a single regression on the entire dataset that allows different students to react
# differently to sugar depending on whether they are active in sports. (hint: use an interaction term)

lm4 = lm(rating ~ sugar*sport, data = SugarData.adv)
summary(lm4)
predict4 = predict(lm4, SugarData.adv)
plot_ly(data = SugarData, x = sugar, y = rating, mode = 'markers', name = 'data') %>%
  add_trace(data = SugarData, x = sugar, y = predict4, mode = 'markers', name = 'prediction') %>%
  layout(title = 'Rating VS Sugar Level')

# 7. Randomly and evenly divide the data into a training and test dataset. What is the
# best model you would use to predict drink ratings? Can you give a reason why?

Sugar.train <- sample_frac(SugarData.adv, 0.5)
sid <- as.numeric(rownames(Sugar.train))
Sugar.test <- SugarData.adv[-sid,]

# 7.1 traditional approach

# model 1 rating ~ sugar

lm1_t = lm(rating ~ sugar, data = Sugar.train)
lm1_t.summ = summary(lm1_t)

# model 2 rating ~ all
lm2_t = lm(rating ~., data = Sugar.train)
lm2_t.summ = summary(lm2_t)

# model 3
#sport_t = Sugar.train$rowing_team | Sugar.train$track_team
#View(sport_t)
Sugar.train.sport = subset(Sugar.train, sport == TRUE)
Sugar.train.nosport = subset(Sugar.train, sport == FALSE)

plot_ly(data = Sugar.train.sport, x = Sugar.train.sport$sugar, y = Sugar.train.sport$rating, mode = 'markers', name = 'sport') %>%
  add_trace(data = Sugar.train.nosport, x = Sugar.train.nosport$sugar, y = Sugar.train.nosport$rating, mode = 'markers', name = 'nosport') %>%
  layout(title = 'Training data: Rating VS Sugar level for Sport active and inactive')

lm3_t.sport = lm(rating ~ sugar, data = Sugar.train.sport)
lm3_t.sport.summ = summary(lm3_t.sport)

lm3_t.nosport = lm(rating ~ sugar, data = Sugar.train.nosport)
lm3_t.nosport.summ = summary(lm3_t.nosport)

# model 4

lm4_t = lm(rating ~ sugar*sport, data = Sugar.train)
lm4_t.summ = summary(lm4_t)
summary(lm4_t)
lm4_t.summ$adj.r.squared
predict4_t = predict(lm4_t,Sugar.train)
mse4_t = mean((predict4_t-Sugar.train$rating)^2)
mse4_t

# pick the bigest Adj_R_sq
c('model','Adjusted R^2')
mo = c('1','2','3-sport','3-nosport','4')
adR_sq.t = c(lm1_t.summ$adj.r.squared,lm2_t.summ$adj.r.squared,lm3_t.sport.summ$adj.r.squared,lm3_t.nosport.summ$adj.r.squared,lm4_t.summ$adj.r.squared)
Adj_R_sq.t = data.frame(mo,adR_sq.t)
colnames(Adj_R_sq.t) = c('model','Adjusted R^2')
#View(Adj_R_sq.t)

# test the model
predict_t = predict(lm4_t,Sugar.test)
plot_ly(data = Sugar.test, x = Sugar.test$sugar, y = Sugar.test$rating, mode = 'markers', name = 'test data') %>%
  add_trace(data = Sugar.test, x = Sugar.test$sugar, y = predict_t, mode = 'markers', name = 'prediction')
mse_t = mean((predict_t-Sugar.test$rating)^2)
mse_t

mse <- function(pred,data) {
  return(mean((pred-data)^2))
  }


# 7.2 train-validation-test approach

# deviding the training data to 60% (train) and 40% (validation):
Sugar.train.t <- sample_frac(Sugar.train, 0.6)
sid.v <- setdiff(rownames(Sugar.train),rownames(Sugar.train.t))
Sugar.train.v <- Sugar.train[sid.v,]

mse.tv<-c()
# model 1 rating ~ sugar

lm1_tv = lm(rating ~ sugar, data = Sugar.train.t)
summary(lm1_tv)
predict1_tv = predict(lm1_tv, Sugar.train.v)
mse.tv[1] = mse(predict1_tv, Sugar.train.v$rating)

# model 2 rating ~ all
lm2_tv = lm(rating ~ sugar+rowing_team+track_team+chess_club, data = Sugar.train.t)
summary(lm2_tv)
predict2_tv = predict(lm2_tv, Sugar.train.v)
mse.tv[2] = mse(predict2_tv, Sugar.train.v$rating)

# model 3
Sugar.train.t.sport = subset(Sugar.train.t, sport == TRUE)
Sugar.train.t.nosport = subset(Sugar.train.t, sport == FALSE)

plot_ly(data = Sugar.train.t.sport, x = Sugar.train.t.sport$sugar, y = Sugar.train.t.sport$rating, mode = 'markers', name = 'sport') %>%
  add_trace(data = Sugar.train.t.nosport, x = Sugar.train.t.nosport$sugar, y = Sugar.train.t.nosport$rating, mode = 'markers', name = 'nosport') %>%
  layout(title = 'Training data: Rating VS Sugar level for Sport active and inactive')

lm3_tv.sport = lm(rating ~ sugar, data = Sugar.train.t.sport)
lm3_tv.sport.summ = summary(lm3_tv.sport)
lm3_tv.sport.summ

lm3_tv.nosport = lm(rating ~ sugar, data = Sugar.train.t.nosport)
lm3_tv.nosport.summ = summary(lm3_tv.nosport)
lm3_tv.nosport.summ

Sugar.train.v.sport = subset(Sugar.train.v, sport == TRUE)
Sugar.train.v.nosport = subset(Sugar.train.v, sport == FALSE)
predict3_tv.sport = predict(lm3_tv.sport,Sugar.train.v.sport)
predict3_tv.nosport = predict(lm3_tv.nosport,Sugar.train.v.nosport)

mse.tv[3] = (mse(predict3_tv.sport,Sugar.train.v.sport$rating)*length(Sugar.train.v.sport$rating) + mse(predict3_tv.nosport,Sugar.train.v.nosport$rating)*length(Sugar.train.v.nosport$rating))/(length(Sugar.train.v$rating))

# model 4

lm4_tv = lm(rating ~ sugar*sport, data = Sugar.train.t)
lm4_tv.summ = summary(lm4_tv)
lm4_tv.summ
predict4_tv = predict(lm4_tv,Sugar.train.v)
mse.tv[4] = mse(predict4_tv,Sugar.train.v$rating)

# pick the least MSE: MODEL 4

mo.tv = c('1','2','3','4')
model.mse.tv = data.frame(mo.tv,mse.tv)
colnames(model.mse.tv) = c('model','MSE')
View(model.mse.tv)

# test the model

predict_tvt = predict(lm4_tv,Sugar.test)
plot_ly(data = Sugar.test, x = Sugar.test$sugar, y = Sugar.test$rating, mode = 'markers', name = 'test data') %>%
  add_trace(data = Sugar.test, x = Sugar.test$sugar, y = predict_tvt, mode = 'markers', name = 'prediction')
mse_tvt = mean((predict_tvt-Sugar.test$rating)^2)
mse_tvt

# 8. For your best model, what is a 99% confidence interval for the regression coefficients. Interpret the results.
confint(lm4_tv,level = 0.99)

# 9. For your best model, what is a 90% prediction interval if the sugar level was 25 and the student was not a member of any team. Interpret the results.
predict(lm4_tv,data.frame(sugar=25,sport=FALSE),interval="prediction", level = 0.9)
