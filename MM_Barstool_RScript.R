library(tidyverse)
library(dplyr)

# import datasets
barstool<- read.csv("pizza_barstool.csv", header = T, stringsAsFactors = FALSE)
datafiniti<- read.csv("pizza_datafiniti.csv", header = T, stringsAsFactors = FALSE)
jared<- read.csv("pizza_jared.csv", header = T, stringsAsFactors = FALSE)


# Check structure of each dataset
str(barstool)
str(datafiniti)
str(jared)

# variable names for each dataset
names(barstool)
names(datafiniti)
names(jared)

# example data from each dataset
head(barstool)
head(datafiniti)
head(jared)

# missing variables in each dataset and percentages
colSums(is.na(barstool))
colSums(is.na(barstool))/463*100
colSums(is.na(datafiniti))
colSums(is.na(datafiniti))/10000*100
colSums(is.na(jared))
colSums(is.na(jared))/375*100

# Duplicated pizza shops by name column
sum(duplicated(barstool$name))
sum(duplicated(datafiniti$name))
sum(duplicated(jared$place))


# Unique values for each variable in each dataset

## this takes treats each column in your DF as 'x' function(x) 
### and applys length(unique()) to each column of your DF
### the 2 specifies the number of row outputs that you want
apply(barstool, 2, function(x) length(unique(x)))       
apply(datafiniti, 2, function(x) length(unique(x)))  
apply(jared, 2, function(x) length(unique(x)))  

setwd("/Users/sidne/Documents/Data Wrangling")
library(readr)
library(dplyr)
library(ggplot2)
barstool <- read_csv("pizza barstool.csv")

#Attributes#
attributes(barstool)

#Table#
barstool_names <- table(barstool$name)
barstool_names[barstool_names > 1]

barstool_addresses <- table(barstool$address1)
barstool_addresses[barstool_addresses > 1]

table(barstool$city) #checked
table(barstool$zip) #checked
table(barstool$country) #checked
table(barstool$price_level)
table(barstool$provider_rating)
table(barstool$provider_review_count)
table(barstool$latitude)
table(barstool$longitude)
table(barstool$review_stats_all_average_score)
table(barstool$review_stats_all_count)

#Boxplot#
#needs to probably be updated to ggplot#
boxplot(barstool$price_level)
boxplot(barstool$provider_rating)
boxplot(barstool$provider_review_count)
boxplot(barstool$review_stats_all_average_score)
boxplot(barstool$review_stats_all_count)

#Summary#
summary(barstool$price_level)
summary(barstool$provider_rating)
summary(barstool$provider_review_count)
summary(barstool$review_stats_all_average_score)
summary(barstool$review_stats_all_count)

#Read in the infiniti file#
datafiniti <- read_csv("pizza_datafiniti.csv")

#attributes#
attributes(datafiniti)

#remove duplicates#
datafiniti_dup_removed <- datafiniti[!duplicated(datafiniti, nmax = 1), ]
view(datafiniti_dup_removed)

#table#
table(datafiniti_dup_removed$name)




