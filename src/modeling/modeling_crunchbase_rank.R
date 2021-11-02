# Modeling Crunchbase Rank

## Load data
fintech <- read.csv("./data/fintech.csv",
                    header = TRUE,
                    sep = ",",
                    na.strings = "")

## Select Columns
numeric_columns <- c("Age",
                     "Number.of.Investors",
                     "Number.of.Lead.Investors",
                     "Number.of.Funding.Rounds",
                     "Total.Funding.Amount.Currency..in.USD.",
                     "Total.Equity.Funding.Amount.Currency..in.USD.",
                     "CB.Rank..Organization.",
                     "CB.Rank..Company.",
                     "Trend.Score..7.Days.",
                     "Trend.Score..30.Days.",
                     "Trend.Score..90.Days.")
factor_columns <- c("Company.Type",
                    "Estimated.Revenue.Range",
                    "Number.of.Employees")
list_columns <- c("Industry.Groups")

fintech <- fintech[c(numeric_columns, factor_columns, list_columns)]

## Clean data

### Remove NA's
fintech <- na.omit(fintech)

### Set is number.of.employees 10-Jan to 1-10
fintech$Number.of.Employees[fintech$Number.of.Employees == "10-Jan"] <- "1-10"

### Set is number.of.employees Nov-50 to 11-50
fintech$Number.of.Employees[fintech$Number.of.Employees == "Nov-50"] <- "11-50"

## Set Types

### Set numeric types
fintech$CB.Rank..Organization. <- gsub(",", "", fintech$CB.Rank..Organization.)
fintech$CB.Rank..Company. <- gsub(",", "", fintech$CB.Rank..Company.)
fintech[numeric_columns] <- sapply(fintech[numeric_columns], as.numeric)

### Set factor types
fintech$Company.Type <- relevel(as.factor(fintech$Company.Type),
                                ref = "For Profit")
fintech$Estimated.Revenue.Range <- relevel(as.factor(fintech$Estimated.Revenue.Range),
                                           ref = "Less than $1M")
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

### Remove industry group & financial.services column
fintech <- subset(fintech, select = -c(Industry.Groups, Financial.Services))

## Create models

### Create control variable model
m1 <- lm(CB.Rank..Company. ~ Age + Company.Type + Number.of.Employees +
           Payments + Software + Commerce.and.Shopping +
           Lending.and.Investments + Mobile + Real.Estate + Hardware +
           Internet.Services + Information.Technology + Clothing.and.Apparel +
           Design + Consumer.Electronics + Transportation + Apps + Education +
           Media.and.Entertainment + Platforms + Artificial.Intelligence +
           Data.and.Analytics + Privacy.and.Security + Science.and.Engineering +
           Health.Care + Biotechnology + Professional.Services + Advertising +
           Community.and.Lifestyle + Administrative.Services +
           Travel.and.Tourism + Sales.and.Marketing + Energy +
           Natural.Resources + Sustainability + Content.and.Publishing +
           Government.and.Military + Consumer.Goods +
           Messaging.and.Telecommunications + Agriculture.and.Farming +
           Events + Music.and.Audio + Video + Gaming + Food.and.Beverage +
           Sports + Navigation.and.Mapping,
         data = fintech)

#### Check summary
summary(m1)

#### Check assumption normality
qqnorm(m1$residuals, pch = 1, frame = FALSE)
qqline(m1$residuals, col = "steelblue", lwd = 2)

#### Check several assumptions: linearity, normality, homoscedasticity, influential cases (clockwise) 
par(mfrow = c(2, 2))
plot(m1)
title("Control Variable Model", side = 3, line = -2, outer = TRUE)

lmtest::bptest(m1)  # Breusch-Pagan test for homogeneity: p-value below 0.05 means heteroscedasticity!
car::ncvTest(m1) # NCV test for homogeneity: p-value below 0.05 means heteroscedasticity!

### Create H1 model (number of investors)
m2 <- lm(CB.Rank..Company. ~ Age + Company.Type + Number.of.Employees +
           Payments + Software + Commerce.and.Shopping +
           Lending.and.Investments + Mobile + Real.Estate + Hardware +
           Internet.Services + Information.Technology + Clothing.and.Apparel +
           Design + Consumer.Electronics + Transportation + Apps + Education +
           Media.and.Entertainment + Platforms + Artificial.Intelligence +
           Data.and.Analytics + Privacy.and.Security + Science.and.Engineering +
           Health.Care + Biotechnology + Professional.Services + Advertising +
           Community.and.Lifestyle + Administrative.Services +
           Travel.and.Tourism + Sales.and.Marketing + Energy +
           Natural.Resources + Sustainability + Content.and.Publishing +
           Government.and.Military + Consumer.Goods +
           Messaging.and.Telecommunications + Agriculture.and.Farming +
           Events + Music.and.Audio + Video + Gaming + Food.and.Beverage +
           Sports + Navigation.and.Mapping + Number.of.Investors,
         data = fintech)

#### Check summary
summary(m2)

#### Check normality
qqnorm(m2$residuals, pch = 1, frame = FALSE)
qqline(m2$residuals, col = "steelblue", lwd = 2)

#### Check several assumptions: linearity, normality, homoscedasticity, influential cases (clockwise) 
par(mfrow = c(2, 2))
plot(m2)
title("H1 Model (number of investors)", side = 3, line = -2, outer = TRUE)

lmtest::bptest(m2)  # Breusch-Pagan test for homogeneity: p-value below 0.05 means heteroscedasticity!
car::ncvTest(m2) # NCV test for homogeneity: p-value below 0.05 means heteroscedasticity!

### Create H2 model (total funding amount)
m3 <- lm(CB.Rank..Company. ~ Age + Company.Type + Number.of.Employees +
           Payments + Software + Commerce.and.Shopping +
           Lending.and.Investments + Mobile + Real.Estate + Hardware +
           Internet.Services + Information.Technology + Clothing.and.Apparel +
           Design + Consumer.Electronics + Transportation + Apps + Education +
           Media.and.Entertainment + Platforms + Artificial.Intelligence +
           Data.and.Analytics + Privacy.and.Security + Science.and.Engineering +
           Health.Care + Biotechnology + Professional.Services + Advertising +
           Community.and.Lifestyle + Administrative.Services +
           Travel.and.Tourism + Sales.and.Marketing + Energy +
           Natural.Resources + Sustainability + Content.and.Publishing +
           Government.and.Military + Consumer.Goods +
           Messaging.and.Telecommunications + Agriculture.and.Farming +
           Events + Music.and.Audio + Video + Gaming + Food.and.Beverage +
           Sports + Navigation.and.Mapping +
           Total.Funding.Amount.Currency..in.USD.,
         data = fintech)

#### Check summary
summary(m3)

#### Check normality
qqnorm(m3$residuals, pch = 1, frame = FALSE)
qqline(m3$residuals, col = "steelblue", lwd = 2)

#### Check several assumptions: linearity, normality, homoscedasticity, influential cases (clockwise) 
par(mfrow = c(2, 2))
plot(m3)
title("H2 model (total funding amount)", side = 3, line = -2, outer = TRUE)

lmtest::bptest(m3)  # Breusch-Pagan test for homogeneity: p-value below 0.05 means heteroscedasticity!
car::ncvTest(m3) # NCV test for homogeneity: p-value below 0.05 means heteroscedasticity!
