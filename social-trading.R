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
library(ROCR)
library(Metrics)
library(openxlsx)

# zt1 <- read_excel("Dataset/Excel/ZuluTrade 2014-04-06 Summary Data.xlsx", na = "N/A")
# zt2 <- read_excel("Dataset/Excel/ZuluTrade Summary Data 2013-07-28.xlsx", na = "N/A")
# zt3 <- read_excel("Dataset/Excel/ZuluTrade Summary Data 2014-01-05.xlsx", na = "N/A")
# zt4 <- read_excel("Dataset/Excel/ZuluTrade Summary Data 2014-02-09.xlsx", na = "N/A")
# 
# # Let us clean the data columns for spaces, commas
# names(zt1)<-str_replace_all(names(zt1), c(" " = "." , "," = "" , "%" = "" ))
# names(zt2)<-str_replace_all(names(zt2), c(" " = "." , "," = "" , "%" = ""  ))
# names(zt3)<-str_replace_all(names(zt3), c(" " = "." , "," = "" , "%" = ""  ))
# names(zt4)<-str_replace_all(names(zt4), c(" " = "." , "," = "" , "%" = ""  ))
# 
# zt1 <- zt1[-16828,]
# zt4 <- zt4[-14404,]

zt1 <- read_excel("Dataset/Excel/Data/FirstStage/zt1.xlsx")
zt2 <- read_excel("Dataset/Excel/Data/FirstStage/zt2.xlsx")
zt3 <- read_excel("Dataset/Excel/Data/FirstStage/zt3.xlsx")
zt4 <- read_excel("Dataset/Excel/Data/FirstStage/zt4.xlsx")

#========================================================================#
# Data Exploration - Begin                                               #
#========================================================================#

# vis_miss(zt1)
# gg_miss_upset(zt2)

gg_miss_upset(zt3, nsets = n_var_miss(zt3))
gg_miss_var(zt1,show_pct = TRUE)

# Checking repeated members in datasets
length(intersect(zt1$Provider.ID,zt2$Provider.ID))
length(intersect(zt2$Provider.ID,zt3$Provider.ID))
length(intersect(zt3$Provider.ID,zt4$Provider.ID))

Reduce(intersect, list(zt1$Provider.ID,zt2$Provider.ID,zt3$Provider.ID,zt4$Provider.ID))

#========================================================================#
# Data Exploration - End                                                 #
#========================================================================#


#========================================================================#
# Data Preparation - Begin                                               #
#========================================================================#
# 
# cleanZTData <- function(zt) {
#   ztClean <- zt[,c(2,4:13,16:20,22:24,37:42)]
#   str(ztClean)
#   # ztClean$Ranking <- factor(ztClean$Ranking, ordered = T)
#   ztClean$Amount_Following_Log <- log(ztClean$Amount.Following+1)
#   ztClean$Has.Live.Followers[ztClean$Has.Live.Followers=="Yes"] <- 1
#   ztClean$Has.Live.Followers[ztClean$Has.Live.Followers=="No"] <- 0
#   ztClean$Has.Live.Followers <- as.numeric(ztClean$Has.Live.Followers)
#   ztClean$Economic.Event.Trade[ztClean$Economic.Event.Trade==TRUE] <-1
#   ztClean$Economic.Event.Trade[ztClean$Economic.Event.Trade==FALSE] <-0
#   #ztClean$`Has Live Followers` <- factor(ztClean$`Has Live Followers`)
#   ztClean$Viewed_Log <- log(ztClean$Viewed+1)
#   #ztClean$`Trading Own Money` <- factor(ztClean$`Trading Own Money`)
#   ztClean$Is.EA <- as.numeric(ztClean$Is.EA)
#   #ztClean$`Economic Event Trade` <- factor(as.numeric(ztClean$`Economic Event Trade`))
#   ztClean$Follow <- vector('numeric', length = dim(ztClean)[1])
#   ztClean$Follow[ztClean$Followers>0] <- 1
#   ztClean$Follow[ztClean$Followers<=0] <- 0
#   ztClean$Amount.Following <- NULL
#   ztClean$Viewed <- NULL
#   ztClean$Country.ISO.Code <- NULL
#   return(ztClean)
# }
# z <- cleanZTData(zt1)
# write.xlsx(z, 'Dataset/Excel/Data/zt1.xlsx')
# z <- cleanZTData(zt2)
# write.xlsx(z, 'Dataset/Excel/Data/zt2.xlsx')
# z <- cleanZTData(zt3)
# write.xlsx(z, 'Dataset/Excel/Data/zt3.xlsx')
# z <- cleanZTData(zt4)
# write.xlsx(z, 'Dataset/Excel/Data/zt4.xlsx')


#========================================================================#
# Data Preparation - End                                                 #
#========================================================================#

#========================================================================#
# Data Separation - Begin                                                #
#========================================================================#

set.seed(777)

train.rows <- sample(rownames(zt),dim(zt)[1]*0.8)
test.rows <- setdiff(rownames(zt),train.rows)

train.data <- zt[train.rows,]
test.data <- zt[test.rows,]

#========================================================================#
# Data Separation - End                                                  #
#========================================================================#

#========================================================================#
# Static definition - Begin                                              #
#========================================================================#

formula <- train.data$Follow ~ .
#family <- poisson(link = "log")
family <- quasipoisson(link = "log")

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
# First stage Model building - Begin                                     #
#========================================================================#


# Multi-collinearity check begin

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

#========================================================================#
# First stage fitting model - Begin                                      #
#========================================================================#

pred <- predict(model,type='response', newdata=zt)
confMatrix <- table(test.data$Follow,pred>optCutOff)

print(confMatrix)

accuracy <- sum(diag(confMatrix))/sum(confMatrix) # 0.9495
sens <- sensitivity(test.data$Follow,pred,threshold = optCutOff) # 0.77684
spec <- specificity(test.data$Follow,pred,threshold = optCutOff) # 0.9987981

cat("Accuracy: ",accuracy,"\nSensitivity: ",sens,"\nSpecificity: ",spec)

#========================================================================#
# First stage fitting model - End                                        #
#========================================================================#
