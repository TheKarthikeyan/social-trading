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
library(caret)
library(stringr)

zt1 <- read_excel("Dataset/Excel/ZuluTrade 2014-04-06 Summary Data.xlsx", na = "N/A")
zt2 <- read_excel("Dataset/Excel/ZuluTrade Summary Data 2013-07-28.xlsx", na = "N/A")
zt3 <- read_excel("Dataset/Excel/ZuluTrade Summary Data 2014-01-05.xlsx", na = "N/A")
zt4 <- read_excel("Dataset/Excel/ZuluTrade Summary Data 2014-02-09.xlsx", na = "N/A")

#========================================================================#
# Data Exploration - Begin                                               #
#========================================================================#

str(zt1)

# Let us clean the data columns for spaces, commas
names(zt1)<-str_replace_all(names(zt1), c(" " = "." , "," = "" , "%" = "" ))
names(zt2)<-str_replace_all(names(zt2), c(" " = "." , "," = "" , "%" = ""  ))
names(zt3)<-str_replace_all(names(zt3), c(" " = "." , "," = "" , "%" = ""  ))
names(zt4)<-str_replace_all(names(zt4), c(" " = "." , "," = "" , "%" = ""  ))

vis_miss(zt1)
gg_miss_upset(zt1)
n_var_miss(zt1)

gg_miss_upset(zt1, nsets = n_var_miss(zt1))

zt1$Ranking[is.na(zt1$Open.Positions.Pips)]

zt1 <- zt1[-16828,]
gg_miss_upset(zt1, nsets = n_var_miss(zt1))
gg_miss_var(zt1,show_pct = TRUE)

plot(zt1$Pips.Profit)
fivenum(zt1$Pips.Profit)
boxplot(zt1$Pips.Profit)



# Checking repeated members in datasets
length(intersect(zt1$Provider.ID,zt2$Provider.ID))
length(intersect(zt2$Provider.ID,zt3$Provider.ID))
length(intersect(zt3$Provider.ID,zt4$Provider.ID))

#========================================================================#
# Data Exploration - End                                                 #
#========================================================================#


#========================================================================#
# Data Preparation - Begin                                               #
#========================================================================#

zt1_non_NA <- zt1[,c(2,4:13,16:20,22:24,37:42)]
str(zt1_non_NA)
# zt1_non_NA$Ranking <- factor(zt1_non_NA$Ranking, ordered = T)
zt1_non_NA$Amount_Following_Log <- log(zt1_non_NA$Amount.Following+1)
zt1_non_NA$Has.Live.Followers[zt1_non_NA$Has.Live.Followers=="Yes"] <- 1
zt1_non_NA$Has.Live.Followers[zt1_non_NA$Has.Live.Followers=="No"] <- 0
zt1_non_NA$Has.Live.Followers <- as.numeric(zt1_non_NA$Has.Live.Followers)
zt1_non_NA$Economic.Event.Trade[zt1_non_NA$Economic.Event.Trade==TRUE] <-1
zt1_non_NA$Economic.Event.Trade[zt1_non_NA$Economic.Event.Trade==FALSE] <-0
#zt1_non_NA$`Has Live Followers` <- factor(zt1_non_NA$`Has Live Followers`)
zt1_non_NA$Viewed_Log <- log(zt1_non_NA$Viewed+1)
#zt1_non_NA$`Trading Own Money` <- factor(zt1_non_NA$`Trading Own Money`)
zt1_non_NA$Is.EA <- as.numeric(zt1_non_NA$Is.EA)
#zt1_non_NA$`Economic Event Trade` <- factor(as.numeric(zt1_non_NA$`Economic Event Trade`))
zt1_non_NA$Follow <- vector('numeric', length = dim(zt1_non_NA)[1])
zt1_non_NA$Follow[zt1_non_NA$Followers>0] <- 1
zt1_non_NA$Follow[zt1_non_NA$Followers<=0] <- 0
zt1_non_NA$Amount.Following <- NULL
zt1_non_NA$Viewed <- NULL
zt1_non_NA$Country.ISO.Code <- NULL
#========================================================================#
# Data Preparation - End                                                 #
#========================================================================#

#========================================================================#
# Data Separation - Begin                                                #
#========================================================================#

set.seed(777)

train.rows <- sample(rownames(zt1_non_NA),dim(zt1_non_NA)[1]*0.8)
test.rows <- setdiff(rownames(zt1_non_NA),train.rows)

train.data <- zt1_non_NA[train.rows,]
test.data <- zt1_non_NA[test.rows,]

#========================================================================#
# Data Separation - End                                                  #
#========================================================================#

#========================================================================#
# Model building - Begin                                                 #
#========================================================================#

# Some static definition
cook_distance_threshold <- 1
gvif_threshold <- 2
p_val_threshold <- 0.1

# Multi-collinearity check begin

del_v_coloumns <- vector('character')


while(TRUE) {
  ds = train.data[,setdiff(names(train.data),del_v_coloumns)]
  vfit <- vif(glm(train.data$Follow ~ .,
                  data = ds,
                  family = poisson(link = "log")))
  if(max(vfit)>gvif_threshold) {
    del_v_coloumns <- append(del_v_coloumns,names(ds)[which(vfit == max(vfit))]) 
    cat("Removing coloumn ",tail(del_v_coloumns,1)," with GVIF:",max(vfit),"\n")
  } else {
    break;
  }
}

# Removing multi-collinear columns
train.data <- subset(train.data,select = names(train.data)[!names(train.data) %in% del_v_coloumns])

# Multi-collinearity check end

# Model fitting begin

model <- glm(Follow ~ .,
             data = train.data,
             family = poisson(link = "log"))


del_p_coloumns <- vector('numeric')
del_p_coloumns
which(colnames(train.data)==names(li[li == max(li[2:length(li)])]))

while(TRUE) {
  li <- summary(model)$coefficients[,4]
  if(max(li[2:length(li)])>p_val_threshold) {
    del_p_coloumns <- append(del_p_coloumns,which(colnames(train.data)==names(li[li == max(li[2:length(li)])])))
    cat("Removing coloumn ",names(which(li == max(li[2:length(li)])))," with P-value:",max(li[2:length(li)]),"\n")
    # Recalculating model
    ds = train.data[,-del_p_coloumns]
    model <- glm(Follow ~ .,
                    data = ds,
                    family=poisson(link = "log"))
  } else {
    break;
  }
}

train.data = train.data[,-del_p_coloumns]


model <- glm(Follow ~ .,
             data = train.data,
             family = poisson(link = "log"))

summary(model)
# Model fitting end

exp(coef(model))
# Outlier and influential observation check begin

plot(cooks.distance(model)) # All observations are within cutoff 1
train.data <- train.data[cooks.distance(model)<cook_distance_threshold,]

# Outlier and influential observation check end


#========================================================================#
# Model building - End                                                   #
#========================================================================#