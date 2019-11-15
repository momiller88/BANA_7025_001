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

############################################################

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
barstool <- read_csv("pizza_barstool.csv")

#'barstool' Attributes#
attributes(barstool)
names(barstool)[2] <- "address"
head(barstool)

#change data types#
barstool_data_type <- transform(barstool, zip = as.character(zip), price_level = as.integer(price_level), provider_review_count = as.integer(provider_review_count), review_stats_all_count = as.integer(review_stats_all_count), review_stats_community_count = as.integer(review_stats_community_count), review_stats_critic_count = as.integer(review_stats_critic_count), review_stats_dave_count = as.integer(review_stats_dave_count))
head(barstool_data_type)
tibble(barstool_data_type)

#Table#
barstool_names <- table(barstool$name)
barstool_names[barstool_names > 1]
barstool_data_type[barstool_data_type$name == "Joe's Pizza",]
barstool_data_type[422,]$name <- "Joe's Pizza - 8th"
barstool_data_type[422,]$name
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

######################################################3

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

#table after dedup#
table(datafiniti_dup_removed$name)
table(datafiniti_dup_removed$address)
table(datafiniti_dup_removed$city)
table(datafiniti_dup_removed$country)
table(datafiniti_dup_removed$province)
table(datafiniti_dup_removed$latitude)
table(datafiniti_dup_removed$longitude)
table(datafiniti_dup_removed$categories)# checked (multiple values in the same column, could separate into it's own dataframe, could create binary flags for each distinct category, tbd)
table(datafiniti_dup_removed$price_range_min)
table(datafiniti_dup_removed$price_range_max)

#select name and categories from datafiniti#
library(stringr)
df4 <- datafiniti_dup_removed %>%
  select(name, categories)
#select only records containing "Pizza Place" in categories#
df4 %>%
  filter(str_detect(categories, "Pizza Place"))

########################################################

#read in jared file#
jared<- read_csv("pizza_jared.csv")

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
names(df2) <- c("polla_qid", "Pct_Average", "Pct_Excellent", "Pct_Fair", "Pct_Good", "Pct_Never_Again", "Pct_Poor")

#merge df1 and df2
dfm <- merge(x = df1, y = df2, by = "polla_qid", all.x = TRUE)

# select remaning columns and dedup question by poll#
df3 <- jared %>%
  select(pollq_id, question, place, time, total_votes)
df3 <- df3[!duplicated(df3, nmax = 1,), ]

#merge votes df and question/location df
jared_clean <- merge(x = df3, y = dfm, by.x = "pollq_id", by.y = "polla_qid", all.x = TRUE)  
jared_clean <- select(jared_clean, 
                      place, 
                      question, 
                      total_votes, 
                      `Never Again`, 
                      Poor, Fair, 
                      Average, 
                      Good, 
                      Excellent, 
                      Pct_Never_Again, 
                      Pct_Poor, 
                      Pct_Fair, 
                      Pct_Average, 
                      Pct_Good, 
                      Pct_Excellent)

head(jared_clean)

#Table jared_clean#
table(jared_clean$pollq_id)
table(jared_clean$question)
table(jared_clean$place)
table(jared_clean$time)
table(jared_clean$total_votes)
table(jared_clean$Average)
table(jared_clean$Excellent)
table(jared_clean$Fair)
table(jared_clean$Good)
table(jared_clean$`Never Again`)
table(jared_clean$Poor)
table(jared_clean$`%_Average`)
table(jared_clean$`%_Excellent`)
table(jared_clean$`%_Fair`)
table(jared_clean$`%_Average`)
table(jared_clean$`%_Never_Again`)
table(jared_clean$`%_Poor`)

#identifying columns that have duplicated names in jared_clean#
jared_dups <- data.frame(table(jared_clean$place))

#shorten list to only dup records#
jared_dups[jared_dups$Freq > 1, ]
jared_dups <- arrange(jared_clean[jared_clean$place %in% jared_dups$Var1[jared_dups$Freq > 1], ], desc(place))
jared_dups

#sum duplicate records votes#
jared_dups_combined <- 
  jared_dups %>%
    group_by(place, question) %>%
    summarise(total_votes = sum(total_votes), 
              `Never Again` = sum(`Never Again`),
              Poor = sum(Poor),
              Fair = sum(Fair),
              Average = sum(Average),
              Good = sum(Good),
              Excellent = sum(Excellent))
jared_dups_combined

#recalculate percents columns
jared_dups_pct <- 
  jared_dups_combined %>%
  mutate( Pct_Never_Again = `Never Again` / total_votes, 
          Pct_Poor = Poor / total_votes,
          Pct_Fair = Fair / total_votes,
          Pct_Average = Average / total_votes,
          Pct_Good = Good / total_votes,
          Pct_Excellent = Excellent / total_votes)
jared_dups_pct <- as_data_frame(jared_dups_pct)
colnames(jared_dups_pct)[4] <- "Never Again"
jared_dups_pct

#remove duplicated records from jared_clean#
jd_dup_names <- jared_dups_pct$place

jc_wo_dupnames <- jared_clean[ ! jared_clean$place %in% jd_dup_names, ]

head(jc_wo_dupnames)
head(jared_dups_pct)

jared2 <- rbind(jc_wo_dupnames, jared_dups_pct)
jared2



#test on inner joining jared2 with barstool by place = name#
merge <- merge(x = jared2, y = barstool, by.x = "place", by.y = "name")
  #27 matches, with 5 duplicated names w/ different addresses

dup <- duplicated(merge[,1])
merge[dup,]

#test on inner joining jared2 with datafiniti by place = name#
merge2 <- merge(x = jared2, y = datafiniti_dup_removed, by.x = "place", by.y = "name")
merge2
  #9 results all unique

#test on inner joining datafiniti with datafiniti by name = name#
merge3 <- merge(x = datafiniti_dup_removed, y = datafiniti_dup_removed, by.x = "name", by.y = "name")
str(merge3)
merge3
  #7698 results, 5862 duplicated names, 5411 duplicated addresses
dup2 <- duplicated(merge3[,1])
dups2
str(merge3[dup2, ])
dup3 <- duplicated(merge3[ ,2])
dup3
str(merge3[dup3, ])
