# Modeling Crunchbase Rank

## Load data
fintech <- read.csv("./data/fintech.csv",
                    header = TRUE,
                    sep = ",",
                    na.strings = c("", "#VALUE!"))

## Select Columns
numeric_columns <- c("Age",
                     "Number.of.Investors",
                     "Number.of.Funding.Rounds",
                     "Total.Funding.Amount.Currency..in.USD.")
factor_columns <- c("Company.Type",
                    "Estimated.Revenue.Range",
                    "Last.Funding.Type",
                    "Number.of.Employees")
list_columns <- c("Industry.Groups")

fintech <- fintech[c(numeric_columns, factor_columns, list_columns)]

## Clean data

### Set is last.funding.type Undisclosed to NA
fintech$Last.Funding.Type[fintech$Last.Funding.Type == "Undisclosed"] <- NA

### Set is number.of.employees 10-Jan to 1-10
fintech$Number.of.Employees[fintech$Number.of.Employees == "10-Jan"] <- "1-10"

### Set is number.of.employees Nov-50 to 11-50
fintech$Number.of.Employees[fintech$Number.of.Employees == "Nov-50"] <- "11-50"

### Delete companies older than 21 years
fintech <- fintech[fintech$Age <21, ]
summary(fintech)

### Delete non profit companies
fintech <- fintech[fintech$Company.Type == 'For Profit', ]
summary(fintech)

### Remove NA rows
fintech <- na.omit(fintech)
summary(fintech)


## Set Types

### Set numeric types
fintech$CB.Rank..Company. <- gsub(",", "", fintech$CB.Rank..Company.)
fintech[numeric_columns] <- sapply(fintech[numeric_columns], as.numeric)

### Set factor types
fintech$Company.Type <- relevel(as.factor(fintech$Company.Type),
                                ref = "For Profit")
fintech$Estimated.Revenue.Range <- relevel(as.factor(fintech$Estimated.Revenue.Range),
                                           ref = "Less than $1M")
fintech$Last.Funding.Type <- relevel(as.factor(fintech$Last.Funding.Type),
                                     ref = "Pre-Seed")
fintech$Number.of.Employees <- relevel(as.factor(fintech$Number.of.Employees),
                                       ref = "1-10")

### Set list types
fintech$Industry.Groups <- as.character(fintech$Industry.Groups)

## Split industry groups

### Create industry group columns
for (idx in 1:nrow(fintech)) {
  groups <- unlist(strsplit(fintech[idx, "Industry.Groups"], ", "))
  
  for (group in groups) {
    fintech[idx, gsub(" ", ".", group)] <- 1
  }
}

### Set empty columns to 0
fintech[, 17:ncol(fintech)][is.na(fintech[, 17:ncol(fintech)])] <- 0


summary(fintech)

### Remove industry group & financial.services column
### fintech <- subset(fintech, select = -c(Industry.Groups, Financial.Services))
# grouping industry groups
library(stringr)
str_split(fintech$Industry.Groups,',')
unlist(str_split(fintech$Industry.Groups,','))
unique(unlist(str_split(fintech$Industry.Groups,',')))
unique(unlist(str_split(fintech$Industry.Groups,',\\s')))

# creation of new columns
# fintech$Finance-sales-marketing-communication
# fintech$Software-videos-gaming-data_science
# fintech$Art-tourism-events-lifestyle-services
# fintech$Hardware-manufacturing-clothes-food_beverages
# fintech$State_departments-main_industries-national_state_sectors

# Finance-sales-marketing-communication = 1
dic1=c(
"Financial Services",
"Payments",
"Commerce and Shopping",
"Lending and Investments",
"Advertising",
"Sales and Marketing",
"sales",
"Content and Publishing",
"Messaging and Telecommunications")

# Software-videos-gaming-data science = 2
dic2=c(
"Software",
"Mobile",
"Internet Services",
"Information Technology",
"Apps",
"Platforms",
"Artificial Intelligence",
"Data and Analytics",
"Video",
"Gaming")

# Art-tourism-events-lifestyle-services = 3
dic3=c(
"Design",
"Real Estate",
"Media and Entertainment",
"Professional Services",
"Community and Lifestyle",
"Travel and Tourism",
"Events",
"Music and Audio",
"Sports",
"Navigation and Mapping")

# Hardware-manufacturing-clothes-food & beverages = 4
dic4=c(
"Hardware",
"Clothing and Apparel",
"Consumer Electronics",
"Consumer Goods",
"Food and Beverage",
"Manufacturing")

# State departments-main industries-national & state sectors = 5
dic5=c(
"Transportation",
"Education",                       
"Privacy and Security",
"Science and Engineering",
"Health Care",
"Biotechnology",
"Administrative Services",
"Energy",
"Natural Resources",
"Sustainability",
"Government and Military",
"Agriculture and Farming")

# for loop through all rows/companies
for (val in fintech)
{
  statement
}
# check at column "industry groups"
fintech$Industry.Groups[1]
# sparse all words in entry
unlist(str_split(fintech$Industry.Groups[1],','))

# check if each character is in list above
# val2 is each word in the list of words within "Industry.Groups"
for (val2 in unlist(str_split(fintech$Industry.Groups[1],',')))
{
  print(val2)
  print(class(val2))
  # putting one according to result
  if (val2 %in% dic1 == TRUE) {fintech$Finance-sales-marketing-communication = 1} else {
    fintech$Finance-sales-marketing-communication = 0
  }
  
  if (val2 %in% dic2 == TRUE) {fintech$Software-videos-gaming-data_science = 1} else {
    fintech$Software-videos-gaming-data_science = 0
  }
  
  if (val2 %in% dic3 == TRUE) {fintech$Art-tourism-events-lifestyle-services = 1} else {
    fintech$Art-tourism-events-lifestyle-services = 0
  }
  
  if (val2 %in% dic4 == TRUE) {fintech$Hardware-manufacturing-clothes-food_beverages = 1} else {
    fintech$Hardware-manufacturing-clothes-food_beverages = 0
  }
  
  if (val2 %in% dic5 == TRUE) {fintech$State_departments-main_industries-national_state_sectors = 1} else {
    fintech$State_departments-main_industries-national_state_sectors = 0
  }
  
}

  


## Create models --> see other document
