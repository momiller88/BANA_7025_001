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











