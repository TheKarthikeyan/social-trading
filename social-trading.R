#Delete all loaded data and values

rm(list = ls())

#Set working directory
setwd("~/Documents/PC/social-trading")
getwd()

library(readxl)

zt1 <- read_excel("Dataset/Excel/ZuluTrade 2014-04-06 Summary Data.xlsx", na = "N/A")
zt2 <- read_excel("Dataset/Excel/ZuluTrade Summary Data 2013-07-28.xlsx", na = "N/A")
zt3 <- read_excel("Dataset/Excel/ZuluTrade Summary Data 2014-01-05.xlsx", na = "N/A")
zt4 <- read_excel("Dataset/Excel/ZuluTrade Summary Data 2014-02-09.xlsx", na = "N/A")

# Checking repeated members in datasets
length(intersect(zt1$`Provider ID`,zt2$`Provider ID`))
length(intersect(zt2$`Provider ID`,zt3$`Provider ID`))
length(intersect(zt3$`Provider ID`,zt4$`Provider ID`))


