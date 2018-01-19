###############
# Dependencies
###############

library(ggplot2)
library(dplyr)
library(readr)


##################
# Import raw data
##################

happiness2017 <- read_csv("data/happiness_2017.csv")
happiness2016 <- read_csv("data/happiness_2016.csv")
happiness2015 <- read_csv("data/happiness_2015.csv")


##################
# Clean 2015 data
##################

## Get rid of empty spaces in column names
names(happiness2015) <- c("Country", "Region", "Happiness.Rank", 
                          "Happiness.Score", "Standard.Error", "Economy", 
                          "Family", "Health", "Freedom", "Government.Corruption", 
                          "Generosity", "Dystopia.Residual")

## Get rid of a column that will not be used in the app
happiness2015 <- happiness2015 %>% select(-Standard.Error)


##################
# Clean 2016 data
##################

## Get rid of empty spaces in column names
names(happiness2016) <- c("Country", "Region", "Happiness.Rank", 
                          "Happiness.Score", "Lower.Confidence.Interval", "Upper.Confidence.Interval", 
                          "Economy", "Family", "Health", "Freedom", "Government.Corruption", 
                          "Generosity", "Dystopia.Residual")

## Get rid of columns that will not be used in the app
happiness2016 <- happiness2016 %>% select(-c(Lower.Confidence.Interval, Upper.Confidence.Interval))


##################
# Clean 2017 data
##################

## Add Region column which does not exist in 2017 data
temp <- happiness2016 %>% select(Country, Region)
happiness2017 <- left_join(happiness2017, temp, by = "Country")

## Reorder columns to bring Region to the second position
refcols <- c("Country", "Region")
happiness2017 <- happiness2017[, c(refcols, setdiff(names(happiness2017), refcols))]

## Fill in for NA values that exist from left_join
temp2 <- 
  happiness2017 %>% 
  filter(is.na(Region)) %>% 
  mutate(Region = c("Eastern Asia", "Eastern Asia", "Sub-Saharan Africa", 
                                       "Sub-Saharan Africa", "Sub-Saharan Africa")) %>% 
  select(Country, Region)

happiness2017 <- left_join(happiness2017, temp2, by = "Country")
happiness2017 <- happiness2017 %>% 
  mutate(Region.x = ifelse(is.na(Region.x), Region.y, Region.x)) %>% 
  rename(Region = Region.x)

## Change names of columns to format as in 2015 and 2016 data
names(happiness2017) <- c("Country", "Region", "Happiness.Rank", 
                          "Happiness.Score", "Whisker.High", "Whisker.Low", 
                          "Economy", "Family", "Health", "Freedom", "Generosity", 
                          "Government.Corruption", "Dystopia.Residual", "Region.y")

## Get rid of columns that will not be used in the app
happiness2017 <- happiness2017 %>% select(-c(Whisker.High, Whisker.Low, Region.y))

##################
# Aggregation of countries for the filtering buttons
##################
countries_2015 <- happiness2015 %>% 
  select(Country, Region)
countries_2016 <- happiness2016 %>% 
  select(Country, Region)
countries_2017 <- happiness2017 %>% 
  select(Country, Region)

diff_2016 <- anti_join(countries_2016, countries_2015, by = "Country")
countries_1516 <- rbind(countries_2015, diff_2016)
diff_2017 <- anti_join(countries_2017, countries_1516, by = "Country")
countries_all <- rbind(countries_1516, diff_2017)

##############
# Export data
##############
write_csv(happiness2015, "results/happiness_2015_clean.csv")
write_csv(happiness2016, "results/happiness_2016_clean.csv")
write_csv(happiness2017, "results/happiness_2017_clean.csv")
write_csv(countries_all, "results/countries_aggregated.csv")

