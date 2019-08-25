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


#========================================================================#
# Data Preparation for second stage model - Begin                        #
#========================================================================#

zt1 <- read_excel("Dataset/Excel/Data/SecondStage/zt1_ss.xlsx")
zt2 <- read_excel("Dataset/Excel/Data/SecondStage/zt2_ss.xlsx")
zt3 <- read_excel("Dataset/Excel/Data/SecondStage/zt3_ss.xlsx")
zt4 <- read_excel("Dataset/Excel/Data/SecondStage//zt4_ss.xlsx")

set.seed(777)

train.rows <- sample(rownames(zt),dim(zt)[1]*0.8)
test.rows <- setdiff(rownames(zt),train.rows)

train.data <- zt[train.rows,]
test.data <- zt[test.rows,]

#========================================================================#
# Data Preparation for second stage model - End                          #
#========================================================================#

#========================================================================#
# Data Preparation for second stage model - End                          #
#========================================================================#

getNormalizeValues <- function(z) {
  P = z$Follow.Prob
  Fa = z$Follow
  Fs = z$Followers
  z$Follow.Prob = NULL
  z$Follow = NULL
  z$Followers = NULL
  z <- as.data.frame(scale(z,center = TRUE, scale = TRUE))
  z$Follow.Prob = P
  z$Follow = Fa
  z$Followers = Fs
  return(z)
}

zt <- getNormalizeValues(zt1)

#========================================================================#
# Data Preparation for second stage model - End                          #
#========================================================================#

#========================================================================#
# Static definition - Begin                                              #
#========================================================================#

formula <- train.data$Followers ~ .
family <- poisson

# Some static definition
cook_distance_threshold <- 1
gvif_threshold <- 4
p_val_threshold <- 0.1

#========================================================================#
# Static definition - End                                                #
#========================================================================#

#========================================================================#
# Utility Functions - Begin                                              #
#========================================================================#

getGBMModel <- function(formula, family, data) {
  model <- glm(formula = formula,
               family = family,
               data = data)
  return(model)
}

getMulticollinearColumns <- function(data) {
  del_v_coloumns <- vector('character')
  
  while(TRUE) {
    ds = train.data[,setdiff(names(train.data),del_v_coloumns)]
    vfit <- vif(getGBMModel(formula, family, data = ds))
    if(max(vfit)>gvif_threshold) {
      del_v_coloumns <- append(del_v_coloumns,names(ds)[which(vfit == max(vfit))]) 
      cat("Removing coloumn ",tail(del_v_coloumns,1)," with GVIF:",max(vfit),"\n")
    } else {
      break;
    }
  }
  return(del_v_coloumns)
}

getInsignificantColumns <- function(data) {
  del_p_coloumns <- vector('numeric')
  ds = data
  model <- getGBMModel(formula, family, data = ds)
  while(TRUE) {
    li <- summary(model)$coefficients[,4]
    if(max(li[2:length(li)])>p_val_threshold) {
      del_p_coloumns <- append(del_p_coloumns,which(colnames(train.data)==names(li[li == max(li[2:length(li)])])))
      cat("Removing coloumn ",names(which(li == max(li[2:length(li)])))," with P-value:",max(li[2:length(li)]),"\n")
      # Recalculating model
      ds = train.data[,-del_p_coloumns]
      model <- getGBMModel(formula, family, data = ds)
    } else {
      break;
    }
  }
  return(del_p_coloumns)
}

#========================================================================#
# Utility Functions - End                                                #
#========================================================================#

#========================================================================#
# Second stage Model building - Begin                                     #
#========================================================================#

getMulticollinearColumns(train.data)
# Multi-collinearity check begin
model <- hurdle(Followers ~ . | 1, family = "poisson", data= train.data)
# + Trades + Winning.Trades. + Open.Positions.Pips + Amount_Following_Log + Max.Draw.Down. + Follow.Prob
# + Winning.Trades. + Average.Pips.Per.Trade + Amount_Following_Log + Max.Draw.Down. + Followers
F <- train.data$Followers
train.data$Followers <- F
Z <- train.data$Follow
train.data$Follow <- Z

train.data <- as.data.frame(scale(train.data))
train.data$Pips.Profit <- (train.data$Pips.Profit/1000)

model <- zeroinfl(Followers ~ Ranking + Pips.Profit + Trades + Winning.Trades. + Open.Positions.Pips + Amount_Following_Log + Max.Draw.Down. + Follow.Prob | 
                  1,
                  dist = "poisson", 
                  zero.dist = "poisson",
                  data = train.data)
summary(model)

pred <- predict(model, newdata = test.data)

mae(test.data$Followers, pred)


multiCollinearColumns <- getMulticollinearColumns(train.data)

# Removing multi-collinear columns
train.data <- subset(train.data,select = names(train.data)[!names(train.data) %in% multiCollinearColumns])

# Multi-collinearity check end

# Model fitting begin

inSignificantColumns <- getInsignificantColumns(train.data)
train.data = train.data[,-inSignificantColumns]
model <- getGBMModel(formula, family, data = train.data)
summary(model)

# Model fitting end

# Outlier and influential observation check begin

plot(cooks.distance(model)) # All observations are within cutoff 1

# Outlier and influential observation check end

# ROC with plot
pred <- predict(model, type='response')
optCutOff <- optimalCutoff(train.data$Follow, pred)[1]
table(train.data$Follow,pred>optCutOff)


ROCRpred <- prediction(pred, train.data$Follow)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

# AUC - 0.9430
auc(actual = train.data$Follow,predicted = pred)

#========================================================================#
# First stage Model building - End                                       #
#========================================================================#


