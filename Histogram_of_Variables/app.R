#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
library(shiny)
library(readr)
knitr::opts_knit$set(root.dir = getwd())
barstool <- read_csv("pizza_barstool.csv")
barstool_miss <- na.omit(barstool)
barstool_data_type <- transform(barstool_miss, zip = as.character(zip), price_level = as.integer(price_level), provider_review_count = as.integer(provider_review_count), review_stats_all_count = as.integer(review_stats_all_count), review_stats_community_count = as.integer(review_stats_community_count), review_stats_critic_count = as.integer(review_stats_critic_count), review_stats_dave_count = as.integer(review_stats_dave_count))
barstool_data_type[c(198,238,239,248,249,265,266,310,311),]$name <- c("Joe's Pizza - 8th","Little Italy Pizza - 45th", "Little Italy Pizza - Fulton","Lucali - Henry", "Lucali - Bay", "Mariella Pizza - Lexington", "Mariella Pizza - 8th", "Patsy's Pizzeria - 1st", "Patsy's Pizzeria - 2nd")
barstool_variable_name <- barstool_data_type[-c(250),]
barstool_variable_name[barstool_variable_name$address == "MGM Hotel & Casino Food Court", ]$address <- "3799 S Las Vegas Blvd"
barstool_variable_name[barstool_variable_name$address == "Home Depot Plz", ]$address <- "3039 NY-50"
barstool_variable_name[barstool_variable_name$address == "Pier 86 W 46th St 12th Ave", ]$address <- "Pier 86, W 46th St"
barstool_variable_name[barstool_variable_name$address == "The Metlife Building", ]$address <- "200 Park Ave"
barstool_variable_name[barstool_variable_name$city == "DUMBO", ]$city <- "Brooklyn"
barstool_variable_name[barstool_variable_name$city == "Edina", ]$city <- "Minneapolis"
barstool_variable_name[barstool_variable_name$city == "New York", ]$city <- "New York City"
barstool_variable_name[barstool_variable_name$city == "Mashantucket", ]$city <- "Ledyard"
barstool_variable_name[barstool_variable_name$city == "Chilmark", ]$city <- "Aquinnah"
barstool_variable_name[barstool_variable_name$city == "Orange", ]$city <- "City of Orange"
barstool_variable_name[barstool_variable_name$zip == "1748", ]$zip <- "01748"
barstool_variable_name[barstool_variable_name$zip == "1801", ]$zip <- "01801"
barstool_variable_name[barstool_variable_name$zip == "1902", ]$zip <- "01902"
barstool_variable_name[barstool_variable_name$zip == "2072", ]$zip <- "02072"
barstool_variable_name[barstool_variable_name$zip == "2108", ]$zip <- "02108"
barstool_variable_name[barstool_variable_name$zip == "2113", ]$zip <- "02113"
barstool_variable_name[barstool_variable_name$zip == "2114", ]$zip <- "02114"
barstool_variable_name[barstool_variable_name$zip == "2115", ]$zip <- "02115"
barstool_variable_name[barstool_variable_name$zip == "2116", ]$zip <- "02116"
barstool_variable_name[barstool_variable_name$zip == "2128", ]$zip <- "02128"
barstool_variable_name[barstool_variable_name$zip == "2132", ]$zip <- "02132"
barstool_variable_name[barstool_variable_name$zip == "2151", ]$zip <- "02151"
barstool_variable_name[barstool_variable_name$zip == "2184", ]$zip <- "02184"
barstool_variable_name[barstool_variable_name$zip == "2199", ]$zip <- "02199"
barstool_variable_name[barstool_variable_name$zip == "2301", ]$zip <- "02301"
barstool_variable_name[barstool_variable_name$zip == "2347", ]$zip <- "02347"
barstool_variable_name[barstool_variable_name$zip == "2359", ]$zip <- "02359"
barstool_variable_name[barstool_variable_name$zip == "2368", ]$zip <- "02368"
barstool_variable_name[barstool_variable_name$zip == "2467", ]$zip <- "02467"
barstool_variable_name[barstool_variable_name$zip == "2535", ]$zip <- "02535"
barstool_variable_name[barstool_variable_name$zip == "2539", ]$zip <- "02539"
barstool_variable_name[barstool_variable_name$zip == "2554", ]$zip <- "02554"
barstool_variable_name[barstool_variable_name$zip == "2557", ]$zip <- "02557"
barstool_variable_name[barstool_variable_name$zip == "2563", ]$zip <- "02563"
barstool_variable_name[barstool_variable_name$zip == "2564", ]$zip <- "02564"
barstool_variable_name[barstool_variable_name$zip == "2568", ]$zip <- "02568"
barstool_variable_name[barstool_variable_name$zip == "2601", ]$zip <- "02601"
barstool_variable_name[barstool_variable_name$zip == "2639", ]$zip <- "02639"
barstool_variable_name[barstool_variable_name$zip == "6338", ]$zip <- "06338"
barstool_variable_name[barstool_variable_name$zip == "6511", ]$zip <- "06511"
barstool_variable_name[barstool_variable_name$zip == "6902", ]$zip <- "06902"
barstool_variable_name[barstool_variable_name$zip == "6907", ]$zip <- "06907"
barstool_variable_name[barstool_variable_name$zip == "7014", ]$zip <- "07014"
barstool_variable_name[barstool_variable_name$zip == "7030", ]$zip <- "07030"
barstool_variable_name[barstool_variable_name$zip == "7031", ]$zip <- "07031"
barstool_variable_name[barstool_variable_name$zip == "7050", ]$zip <- "07050"
barstool_variable_name[barstool_variable_name$zip == "7070", ]$zip <- "07070"
barstool_variable_name[barstool_variable_name$zip == "7073", ]$zip <- "07073"
barstool_variable_name[barstool_variable_name$zip == "7110", ]$zip <- "07110"
barstool_variable_name[barstool_variable_name$zip == "7202", ]$zip <- "07202"
barstool_variable_name[barstool_variable_name$zip == "7302", ]$zip <- "07302"
barstool_variable_name[barstool_variable_name$zip == "7407", ]$zip <- "07407"
barstool_variable_name[barstool_variable_name$zip == "7446", ]$zip <- "07446"
barstool_for_histogram <- barstool_variable_name[ , c(8:22)]
shinyApp(
    ui = fluidPage(
        varSelectInput("variable", "Variable:", barstool_for_histogram),
        plotOutput("data")
    ),
    server = function(input, output) {
        output$data <- renderPlot({
            ggplot(barstool_for_histogram, aes(!!input$variable)) + geom_histogram()})
    }
)

