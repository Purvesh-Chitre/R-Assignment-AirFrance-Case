####################################################################

####################################################################
################## A1: Business Case Presentation ##################
####### Visualizing & Analyzing Data with R: Methods & Tools #######
############################ DAT-5323 ##############################
############################# DDMBAN ###############################
############################# TEAM 7 ###############################
####################################################################

####################################################################

## importing the library readxl 
library(readxl)

# importing Air France excel dataset into air_france_data
air_france <- read_excel("Documents/MBAN/Visualizing & Analyzing Data with R-Methods & Tools/Air France Case Spreadsheet Supplement-1.xls", 
           sheet = "DoubleClick")

# understanding the dataset by count of observations and variables
my_obs <- nrow(air_france)
my_var <- ncol(air_france)
my_dim <- c(my_obs, my_var)

# checking which columns have missing values
colSums(is.na(air_france))

# bid strategy has 1224 missing values
# so we decided to remove that column entirely


# data massaging for any blank 
# this gives us an understandding about the length of observations
# and the length of the variables in the air france dataset

# importing the dplyr library
library(dplyr)

# Select only numeric columns
numeric_cols <- air_france %>%
  select_if(is.numeric)

# Summary of the numeric columns
summary(numeric_cols)


# group the dataset by Publisher Name and calculate the revenue and cost 
# for each publisher
publisher_summary <- air_france %>%
  group_by(`Publisher Name`) %>%
  summarize(total_revenue = sum(Amount),
            total_cost = sum(`Total Cost`),
            profit = sum(Amount) - sum(`Total Cost`),
            ROMI = round((profit/total_cost) * 100, 2))

# sort the publishers by their profit in descending order
publisher_summary <- publisher_summary[order(publisher_summary$profit, decreasing = TRUE),]

# print the top 10 publishers by profit
head(publisher_summary, 10)

# checking for the count of Status in air_france dataset
air_france %>%
  count(Status)

# creating a column for Profits for each observation in rows
air_france$Profit <- air_france$`Amount` - air_france$`Total Cost`

# group the dataset by Status and calculate the sum of profit for each status
status_summary <- air_france %>%
  group_by(Status) %>%
  summarize(total_profit = sum(Profit))

# print the status_summary
status_summary

## campaign and profits %
## click-through and sales

# group the dataset by Campaign and calculate the sum of profit for each campaign
campaign_summary <- air_france %>%
  group_by(Campaign) %>%
  summarize(total_profit = sum(Profit))

# sort the campaigns by profit in descending order
campaign_summary <- campaign_summary[order(campaign_summary$total_profit, decreasing = TRUE),]

# print the top 10 campaigns by profit
head(campaign_summary, 10)

# renaming the column name for total bookings
colnames(air_france)[colnames(air_france) == "Total Volume of Bookings"] <- "bookings"

# group the dataset by Keyword and calculate the sum of bookings for each keyword
keyword_summary <- air_france %>%
  group_by(Keyword) %>%
  summarize(total_bookings = sum(bookings))

# sort the keywords by their bookings in descending order
keyword_summary <- keyword_summary[order(keyword_summary$total_bookings, decreasing = TRUE),]

# print the top 10 keywords by bookings
head(keyword_summary, 10)

# group the dataset by Campaign and Keyword and calculate the sum of bookings for each combination of Campaign and Keyword
campaign_keyword_summary <- air_france %>%
  group_by(Campaign, Keyword) %>%
  summarize(total_bookings = sum(bookings))

# sort the campaigns and keywords by their bookings in descending order
campaign_keyword_summary <- campaign_keyword_summary[order(campaign_keyword_summary$total_bookings, decreasing = TRUE),]

# print the top 10 campaigns and keywords by bookings
head(campaign_keyword_summary, 10)

# group the dataset by Campaign and Keyword and calculate the sum of bookings for each keyword within each campaign
campaign_keyword_summary <- air_france %>%
  group_by(Campaign, Keyword) %>%
  summarize(total_bookings = sum(bookings))

# sort the campaigns and keywords by their bookings in descending order
campaign_keyword_summary <- campaign_keyword_summary[order(campaign_keyword_summary$total_bookings, decreasing = TRUE),]

# print the top 10 campaigns and keywords by bookings
head(campaign_keyword_summary, 10)

# group the dataset by Publisher Name, Campaign, Keyword, and Status
# and calculate the sum of bookings and profits for each combination
summary_table <- air_france %>%
  group_by(`Publisher Name`, Campaign, Keyword, Status) %>%
  summarize(total_bookings = sum(bookings),
            total_profit = sum(Profit))

# sort the table by profits in descending order
summary_table <- summary_table[order(summary_table$total_profit, decreasing = TRUE),]

# select the top 10 rows by profits
summary_table <- head(summary_table, 10)

# print the table
summary_table

sapply(air_france, is.factor)

is.factor(air_france$Campaign)

class(air_france$Campaign)

# Convert Campaign, Publisher Name and Keyword to a factor variable
air_france$Campaign <- factor(air_france$Campaign)
air_france$`Publisher Name` <- as.factor(air_france$`Publisher Name`)
air_france$Keyword <- as.factor(air_france$Keyword)

# load required packages
library(rpart)
library(rpart.plot)


# filter the data to include only the top 100 performing keywords
top_keywords <- air_france %>%
  group_by(Keyword) %>%
  summarize(total_bookings = sum(bookings)) %>%
  arrange(desc(total_bookings)) %>%
  head(100)

air_france_top_keywords <- air_france %>%
  filter(Keyword %in% top_keywords$Keyword)

# create a binary variable for booking status
air_france_top_keywords$booking_status <- ifelse(air_france_top_keywords$bookings > 0, 1, 0)

# split data into training and testing sets
train_indices <- sample(1:nrow(air_france_top_keywords), size = 0.8 * nrow(air_france_top_keywords))
train_data <- air_france_top_keywords[train_indices, ]
test_data <- air_france_top_keywords[-train_indices, ]

# fit a decision tree model to predict booking status
air_france_tree <- rpart(booking_status ~ `Publisher Name` + Campaign + Keyword + Impressions + Clicks,
                         data = train_data, 
                         method = "class",
                         cp = 0.05)

# visualize the decision tree
rpart.plot(air_france_tree, type = 1, extra = 1)

# use the predict function to generate predictions on the test data
air_france_predict <- predict(air_france_tree, test_data, type = "class")

library(ggplot2)

# create vertical bar plot of profit by publisher
ggplot(publisher_summary, aes(y=profit, x=`Publisher Name`)) +
  geom_bar(stat="identity", fill="lightgreen") +
  xlab("Publisher Name") +
  ylab("Profit (in Euros)") +
  ggtitle("Profit by Publisher") +
  theme(plot.title = element_text(hjust = 0.5))

# create the barplot
ggplot(status_summary, aes(x = Status, y = total_profit)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(x = "Status", y = "Total Profit", title = "Total Profit by Status")

# create a horizontal bar plot of top 10 keywords by bookings
ggplot(head(keyword_summary, 10), aes(x = total_bookings, y = Keyword)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Total Bookings") +
  ylab("Keyword") +
  ggtitle("Top 10 Keywords by Bookings")

# group the dataset by Campaign and calculate the sum of profit for each campaign
campaign_summary <- air_france %>%
  group_by(Campaign) %>%
  summarize(total_profit = sum(Profit))

# sort the campaigns by profit in descending order and select the top 3 campaigns
top_campaigns <- head(campaign_summary[order(campaign_summary$total_profit, decreasing = TRUE),], 3)

# plot the top 3 campaigns by profit using a bar chart
ggplot(top_campaigns, aes(x = Campaign, y = total_profit)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Top 3 Campaigns by Profit") +
  xlab("Campaign") +
  ylab("Total Profit")




