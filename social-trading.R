#Delete all loaded data and values

rm(list = ls())

#Set working directory
setwd("~/Documents/PC/social-trading")
getwd()

library(readxl)
library(ggplot2)
library(naniar)
library(dplyr)
library(car)
library(maps)

zt1 <- read_excel("Dataset/Excel/ZuluTrade 2014-04-06 Summary Data.xlsx", na = "N/A")
zt2 <- read_excel("Dataset/Excel/ZuluTrade Summary Data 2013-07-28.xlsx", na = "N/A")
zt3 <- read_excel("Dataset/Excel/ZuluTrade Summary Data 2014-01-05.xlsx", na = "N/A")
zt4 <- read_excel("Dataset/Excel/ZuluTrade Summary Data 2014-02-09.xlsx", na = "N/A")

#========================================================================#
# Data Exploration - Begin                                               #
#========================================================================#

str(zt1)

vis_miss(zt1)
gg_miss_upset(zt1)
n_var_miss(zt1)

gg_miss_upset(zt1, nsets = n_var_miss(zt1))

zt1$Ranking[is.na(zt1$`Open Positions Pips`)]

zt1 <- zt1[-16828,]
gg_miss_upset(zt1, nsets = n_var_miss(zt1))


plot(zt1$`Pips Profit`)
fivenum(zt1$`Pips Profit`)
boxplot(zt1$`Pips Profit`)
gg_miss_var(zt1,show_pct = TRUE)


# Checking repeated members in datasets
length(intersect(zt1$`Provider ID`,zt2$`Provider ID`))
length(intersect(zt2$`Provider ID`,zt3$`Provider ID`))
length(intersect(zt3$`Provider ID`,zt4$`Provider ID`))

#========================================================================#
# Data Exploration - End                                                 #
#========================================================================#


#========================================================================#
# Data Preparation - Begin                                               #
#========================================================================#

zt1_non_NA <- zt1[,c(2,4:13,16:20,22:24,37:42)]

str(zt1_non_NA)
zt1_non_NA$Ranking <- factor(zt1_non_NA$Ranking, ordered = T)
zt1_non_NA$Amount_Following_Log <- log(zt1_non_NA$`Amount Following`)
zt1_non_NA$`Has Live Followers` <- factor(as.numeric(zt1_non_NA$`Has Live Followers`))
zt1_non_NA$Viewed_Log <- log(zt1_non_NA$Viewed)
zt1_non_NA$`Trading Own Money` <- factor(zt1_non_NA$`Trading Own Money`)
zt1_non_NA$`Is EA` <- factor(as.numeric(zt1_non_NA$`Is EA`))
zt1_non_NA$`Economic Event Trade` <- factor(as.numeric(zt1_non_NA$`Economic Event Trade`))
zt1_non_NA$`Amount Following` <- NULL
zt1_non_NA$Viewed <- NULL
zt1_non_NA$`Country ISO Code` <- NULL

#========================================================================#
# Data Preparation - End                                                 #
#========================================================================#




