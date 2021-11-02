# Basic Data Analysis

## Load data
fintech <- read.csv("./data/fintech.csv",
                    header = TRUE,
                    sep = ",",
                    na.strings = c("", "#VALUE!"))

## Select Columns
character_columns <- c("Organization.Name", "Acquired.by")
numeric_columns <- c("Age",
                     "Number.of.Funding.Rounds",
                     "Number.of.Investors",
                     "Number.of.Lead.Investors",
                     "Number.of.Investments",
                     "Number.of.Lead.Investments",
                     "Number.of.Acquisitions",
                     "Number.of.Exits",
                     "Number.of.Exits..IPO.",
                     "Total.Funding.Amount",
                     "Total.Funding.Amount.Currency..in.USD.",
                     "Last.Funding.Amount",
                     "Last.Funding.Amount.Currency..in.USD.",
                     "Total.Equity.Funding.Amount",
                     "Total.Equity.Funding.Amount.Currency..in.USD.",
                     "Last.Equity.Funding.Amount",
                     "Last.Equity.Funding.Amount.Currency..in.USD.",
                     "Money.Raised.at.IPO",
                     "Money.Raised.at.IPO.Currency..in.USD.",
                     "Valuation.at.IPO",
                     "Valuation.at.IPO.Currency..in.USD.",
                     "CB.Rank..Organization.",
                     "CB.Rank..Company.",
                     "CB.Rank..School.",
                     "Trend.Score..7.Days.",
                     "Trend.Score..30.Days.",
                     "Trend.Score..90.Days.")
factor_columns <- c("Company.Type",
                    "Operating.Status",
                    "Founded.Date.Precision",
                    "Estimated.Revenue.Range",
                    "Number.of.Employees",
                    "Acquisition.Status",
                    "Acquisition.Type",
                    "Acquisition.Terms",
                    "IPO.Status",
                    "Funding.Status",
                    "Total.Funding.Amount.Currency",
                    "Last.Funding.Type",
                    "Last.Funding.Amount.Currency",
                    "Total.Equity.Funding.Amount.Currency",
                    "Last.Equity.Funding.Type",
                    "Last.Equity.Funding.Amount.Currency",
                    "Money.Raised.at.IPO.Currency",
                    "Valuation.at.IPO.Currency")
date_columns <- c("Founded.Date", "Last.Funding.Date", "IPO.Date")
list_columns <- c("Industries",
                  "Industry.Groups",
                  "Top.5.Investors",
                  "Investor.Type",
                  "Investment.Stage")

fintech <- fintech[c(character_columns,
                     numeric_columns,
                     factor_columns,
                     date_columns,
                     list_columns)]

## Set Types

### Set character types
fintech[character_columns] <- sapply(fintech[character_columns], as.character)

### Set numeric types
fintech$CB.Rank..Organization. <- gsub(",", "", fintech$CB.Rank..Organization.)
fintech$CB.Rank..Company. <- gsub(",", "", fintech$CB.Rank..Company.)
fintech$CB.Rank..School. <- gsub(",", "", fintech$CB.Rank..School.)

fintech[numeric_columns] <- sapply(fintech[numeric_columns], as.numeric)

### Set factor types
fintech[factor_columns] <- sapply(fintech[factor_columns], as.factor)

### Set date types
fintech[date_columns] <- sapply(fintech[date_columns], as.character)

### Set list types
fintech[character_columns] <- sapply(fintech[character_columns], as.character)

## Clean data

### Set is number.of.employees 10-Jan to 1-10
fintech$Number.of.Employees[fintech$Number.of.Employees == "10-Jan"] <- "1-10"

### Set is number.of.employees Nov-50 to 11-50
fintech$Number.of.Employees[fintech$Number.of.Employees == "Nov-50"] <- "11-50"

## Create Subset

### Create small subset
fintech_small <- fintech[c("Age",
                           "Company.Type",
                           "Industry.Groups",
                           "Number.of.Employees",
                           "CB.Rank..Company.",
                           "Estimated.Revenue.Range",
                           "Last.Funding.Type",
                           "Number.of.Investors",
                           "Number.of.Lead.Investors",
                           "Number.of.Funding.Rounds",
                           "Total.Funding.Amount.Currency..in.USD.",
                           "Total.Equity.Funding.Amount.Currency..in.USD.",
                           "Trend.Score..7.Days.",
                           "Trend.Score..30.Days.",
                           "Trend.Score..90.Days.")]

## First row (large)
head(fintech)

## First row (small)
head(fintech_small)

## Summary statistics (large)
summary(fintech)

bootcamp2021::descriptives(fintech, digits = 2)

## Summary statistics (small)
summary(fintech_small)

bootcamp2021::descriptives(fintech_small, digits = 2)

## Plots

### Missing data (large)
naniar::vis_miss(fintech)
naniar::gg_miss_upset(fintech)

### Missing data (small)
naniar::vis_miss(fintech_small)
naniar::gg_miss_upset(fintech_small)

### Company age: histogram / density plot
hist(fintech$Age,
     breaks = 20,
     main = "Age of companies",
     xlab = "Years",
     probability = TRUE,
     col = "peachpuff")
lines(density(fintech$Age, na.rm = TRUE), lwd = 2, col = "chocolate3")

### Company age: QQ plot
qqnorm(fintech$Age, pch = 1, frame = FALSE, na.rm = TRUE)
qqline(fintech$Age, col = "steelblue", lwd = 2, na.rm = TRUE)

### Total funding amount: histogram / density plot
hist(fintech$Total.Funding.Amount.Currency..in.USD.,
     breaks = 20,
     main = "Total funding amount",
     xlab = "USD",
     probability = TRUE,
     col = "peachpuff")
lines(density(fintech$Total.Funding.Amount.Currency..in.USD., na.rm = TRUE),
      lwd = 2,
      col = "chocolate3")

### LOG(Total funding amount): histogram / density plot
hist(log(fintech$Total.Funding.Amount.Currency..in.USD.),
     breaks = 20,
     main = "Total funding amount",
     xlab = "USD",
     probability = TRUE,
     col = "peachpuff")
lines(density(log(fintech$Total.Funding.Amount.Currency..in.USD.),
              na.rm = TRUE),
      lwd = 2,
      col = "chocolate3")

### Total funding amount: boxplot
ggplot2::ggplot(fintech) +
  ggplot2::aes(x = "",
      y = Total.Funding.Amount.Currency..in.USD.,
      na.rm = TRUE) +
  ggplot2::geom_boxplot(fill = "#0c4c8a") +
  ggplot2::theme_minimal()

### LOG(Total funding amount): boxplot
ggplot2::ggplot(fintech) +
  ggplot2::aes(x = "",
      y = log(Total.Funding.Amount.Currency..in.USD.),
      na.rm = TRUE) +
  ggplot2::geom_boxplot(fill = "#0c4c8a") +
  ggplot2::theme_minimal()

### Company type: barplot
counts <- table(fintech$Company.Type)

barplot(counts,
        main = "Company Type Distribution",
        xlab = "Company Types",
        ylab = "Frequency",
        col = "peachpuff")


### Number of employees: barplot
counts <- table(fintech$Number.of.Employees)

barplot(counts,
        main = "Number of Employees",
        xlab = "Count",
        ylab = "Frequency",
        col = "peachpuff")

### Estimated Revenue Range: barplot
counts <- table(fintech$Estimated.Revenue.Range)

barplot(counts,
        main = "Estimated Revenue Range Distribution",
        xlab = "Estimated Revenue Range",
        ylab = "Frequency",
        col = "peachpuff")

### Number of investors: barplot
counts <- table(fintech$Number.of.Investors)

barplot(counts,
        main = "Number of Investors",
        xlab = "Count",
        ylab = "Frequency",
        col = "peachpuff")

### Number of lead investors: barplot
counts <- table(fintech$Number.of.Lead.Investors)

barplot(counts,
        main = "Number of Lead Investors",
        xlab = "Count",
        ylab = "Frequency",
        col = "peachpuff")

### Number of funding rounds: barplot
counts <- table(fintech$Number.of.Funding.Rounds)

barplot(counts,
        main = "Number of Funding Rounds",
        xlab = "Count",
        ylab = "Frequency",
        col = "peachpuff")

### Fintech: correlation network
bootcamp2021::corrNetwork(na.omit(fintech_small[intersect(names(fintech_small),
                                                          numeric_columns)]),
                          main = "Correlation network of fintech (small)")

## Tests

### Total funding amount: Grubbs outliers test
outliers::grubbs.test(fintech$Total.Funding.Amount.Currency..in.USD.)
