library(tidyverse)
library(dplyr)
setwd("~/School papers/Graduate/Data Wrangling/BANA_7025_001")


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

#'barstool' Attributes#
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
table(barstool$review_stats_all_average_score) #checked (could simplify to rounded 2 decimals)
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

#'datafiniti' Attributes#
attributes(datafiniti)

#Table#
table(datafiniti$name)
table(datafiniti$address)
table(datafiniti$city)
table(datafiniti$country)
table(datafiniti$province)
table(datafiniti$latitude)
table(datafiniti$longitude)
table(datafiniti$categories)# checked (multiple values in the same column, could separate into it's own dataframe, could create binary flags for each distinct category, tbd)
table(datafiniti$price_range_min)
table(datafiniti$price_range_max)

#remove duplicates#
datafiniti_dup_removed <- datafiniti[!duplicated(datafiniti, nmax = 1), ]
view(datafiniti_dup_removed)

#table#
table(datafiniti_dup_removed$name)



#read in jared file#
jared<- read.csv("pizza_jared.csv")

#'jared' Attributes#
attributes(jared)

#Table#
table(jared$polla_qid)#checked (ID's duplicated x5, assuming it corresponds to each poll data may need reformatted)
table(jared$answer)#checked
table(jared$votes)#checked
table(jared$pollq_id)#checked
table(jared$question)#checked (might be some discrepancies with the q_id, and questions, several questions had more than 5 dups and each q_id had only 5 dups)
table(jared$place)#checked (might have similar issue as question, but could be different locations with the same name)
table(jared$time)#checked
table(jared$total_votes)#checked (might want to convert time to a more readable format)
table(jared$percent)#checked (not bad, lots of 0's compared to others, next largest is 21)

#spread columns votes are labeled in each column#

#select and spread responses count#
df1 <- jared %>% 
  select(polla_qid, answer, votes) %>%
  spread(answer, votes, fill = 0)

#select and spread responses percent#
df2 <- jared %>%
  select(polla_qid, answer, percent) %>%
  spread(answer, percent, fill = 0)
names(df2) <- c("polla_qid", "%_Average", "%_Excellent", "%_Fair", "%_Good", "%_Never_Again", "%_Poor")

#merge df1 and df2
dfm <- merge(x = df1, y = df2, by = "polla_qid", all.x = TRUE)

# select remaning columns and dedup question by poll#
df3 <- jared %>%
  select(pollq_id, question, place, time, total_votes)
df3 <- df3[!duplicated(df3, nmax = 1,), ]

#merge votes df and question/location df
jared_clean <- merge(x = df3, y = dfm, by.x = "pollq_id", by.y = "polla_qid", all.x = TRUE)  
