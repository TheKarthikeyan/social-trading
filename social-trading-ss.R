#Delete all loaded data and values

rm(list = ls())

#Set working directory
setwd("~/Documents/PC/social-trading")
getwd()

library(readxl)
library(ggplot2)
library(dplyr)
library(car)
library(naniar)
library(pscl)
library(caret)
library(stringr)
library(InformationValue)
library(Metrics)
library(openxlsx)
library(BBmisc)


zt1 <- read_excel("Dataset/Excel/Data/SecondStage/zt1_ss.xlsx")
zt2 <- read_excel("Dataset/Excel/Data/SecondStage/zt2_ss.xlsx")
zt3 <- read_excel("Dataset/Excel/Data/SecondStage/zt3_ss.xlsx")
zt4 <- read_excel("Dataset/Excel/Data/SecondStage//zt4_ss.xlsx")

#========================================================================#
# Utility function for second stage model - End                          #
#========================================================================#

getNormalizeValues <- function(z) {
  P = z$Follow.Prob
  Fa = z$Follow
  Fs = z$Followers
  z$Follow.Prob = NULL
  z$Follow = NULL
  z$Followers = NULL
  z <-  as.data.frame(apply(z, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
  z$Follow.Prob = P
  z$Follow = Fa
  z$Followers = Fs
  return(z)
}

zt <- getNormalizeValues(zt4)

getMetrics <- function(actual, predicted){
  cat("MAE:",mae(actual, predicted)," MAPE:",mape(actual, predicted)," RMSE:",rmse(actual,predicted))
}
  
#========================================================================#
# Utility function for second stage model - End                          #
#========================================================================#


#========================================================================#
# Data Preparation for second stage model - Begin                        #
#========================================================================#

set.seed(777)

train.rows <- sample(rownames(zt),dim(zt)[1]*0.8)
test.rows <- setdiff(rownames(zt),train.rows)

train.data <- zt[train.rows,]
test.data <- zt[test.rows,]

#========================================================================#
# Data Preparation for second stage model - End                          #
#========================================================================#



#
#========================================================================#
# Second stage Model building - Begin                                    #
#========================================================================#


model <- zeroinfl(Followers ~ Ranking + Pips.Profit + Trades + Open.Positions.Pips + Max.Draw.Down. + Follow.Prob | 
                  1,
                  dist = "poisson", 
                  link = "probit",
                  data = train.data)
summary(model)
pred <- predict(model, newdata = test.data)
getMetrics(test.data$Followers, pred)

#========================================================================#
# Second stage Model building - End                                      #
#========================================================================#


