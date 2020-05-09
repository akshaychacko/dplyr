# a4-data-wrangling

################################### Set up ###################################

# Install (if not installed) + load dplyr package 
library(dplyr)

# Set your working directory to the appropriate project folder

setwd("~/../Desktop/Info 201/a4-dplyr-akshaychacko")

# Read in `any_drinking.csv` data using a relative path

any_drinking <- read.csv("data/any_drinking.csv", stringsAsFactors=FALSE)

# Read in `binge.drinking.csv` data using a relative path

binge_drinking <- read.csv("data/binge_drinking.csv", stringsAsFactors=FALSE)

# Create a directory (using R) called "output" in your project directory
# Make sure to suppress any warnings, in case the directory already exists

dir.create("output", showWarnings=FALSE)

################################### Any drinking in 2012 ###################################

# For this first section, you will work only with the *any drinking* dataset.
# In particular, we'll focus on data from 2012, keeping track of the `state` and `location` variables

# Create a data.frame that has the `state` and `location` columns, and all columns with data from 2012

any_drinking_2012 <- select(any_drinking, state, location, both_sexes_2012, females_2012, males_2012)

# Using the 2012 data, create a column that has the difference in male and female drinking patterns

any_drinking_2012 <- mutate(any_drinking_2012, difference_2012 = males_2012 - females_2012)

# Write your 2012 data to a .csv file in your `output/` directory with an expressive filename
# Make sure to exclude rownames

write.csv(any_drinking_2012, "output/any_drinking_2012.csv", row.names=F)

# Are there any locations where females drink more than males?
## Your answer should be a *dataframe* of the locations, states, and differences for all locations (no extra
## columns)

females_greater <- filter(any_drinking_2012, difference_2012 < 0) %>% select(location, state, difference_2012)

## What is the location in which male and female drinking rates are most similar (*absolute* difference is
## smallest)?
## Your answer should be a *dataframe* of the location, state, and value of interest (no extra
## columns)

most_similar <- filter(any_drinking_2012, difference_2012 == min(abs(difference_2012))) %>% select(location, state, difference_2012)

## As you've (hopefully) noticed, the `location` column includes national, state, and county level
## estimates.
## However, many audiences may only be interested in the *state* level data. Given that, you
## should do the following:
# Create a new variable that is only the state level observations in 2012
# For the sake of this analysis, you should treat Washington D.C. as a *state*

state_level <- filter(any_drinking_2012, state == location)

# Which state had the **highest** drinking rate for both sexes combined? 
# Your answer should be a *dataframe* of the state and value of interest (no extra columns)

highest_combined <- filter(state_level, both_sexes_2012 == max(both_sexes_2012)) %>% select(state, both_sexes_2012)

# Which state had the **lowest** drinking rate for both sexes combined?
# Your answer should be a *dataframe* of the state and value of interest (no extra columns)

lowest_combined <- filter(state_level, both_sexes_2012 == min(both_sexes_2012)) %>% select(state, both_sexes_2012)

## What was the difference in (any-drinking) prevalence between the state with the highest level of
## consumption,
# and the state with the lowest level of consumption?
# Your answer should be a single value (a dataframe storing one value is fine)

prevalence_diff <- select(highest_combined, both_sexes_2012) - select(lowest_combined, both_sexes_2012)
  
# Write your 2012 state data to an appropriately named file in your `output/` directory
# Make sure to exclude rownames

write.csv(state_level, "output/state_level_2012.csv", row.names=F)

## Write a function that allows you to specify a state, then saves a .csv file with only observations from
## that state
# This includes data about the state itself, as well as the counties within the state
# You should use the entire any.drinking dataset for this function
# The file you save in the `output` directory indicates the state name
# Make sure to exclude rownames

SaveState <- function(state_name) {
  query <- filter(any_drinking, state == state_name)
  write.csv(query, paste0("output/", state_name, "_any_drinking.csv"), row.names=F)
}

# Demonstrate your function works by passing 3 states of your choice to the function

SaveState("California")
SaveState("Washington")
SaveState("North Carolina")

################################### Binge drinking Dataset ###################################
# In this section, we'll ask a variety of questions regarding our binge.drinking dataset
# Moreover, we'll be looking at a subset of the observations which is just the counties 
# (i.e., exclude state/national estimates)
# In order to ask these questions, you'll need to first prepare a subset of the data for this section:

# Create a dataframe with only the county level observations from the binge_driking dataset 
# You should (again) think of Washington D.C. as a state, and therefore *exclude it here*
# This does include "county-like" areas such as parishes and boroughs

county_level <- binge_drinking %>% filter(state != location) %>% filter(state != "National") 

# What is the average level of binge drinking in 2012 for both sexes (across the counties)?

avg_2012 <- summarize(county_level, mean = mean(both_sexes_2012))

# What is the *minimum* level of binge drinking in each state in 2012 for both sexes (across the counties)? 
## Your answer should contain roughly 50 values (one for each state), unless there are two counties in a
## state with the same value
# Your answer should be a *dataframe* with the 2012 binge drinking rate, location, and state

min_binge <- county_level %>% group_by(state) %>% filter(both_sexes_2012 == min(both_sexes_2012)) %>% select(state, location, both_sexes_2012) 

# What is the *maximum* level of binge drinking in each state in 2012 for both sexes (across the counties)? 
# Your answer should be a *dataframe* with the value of interest, location, and state

max_binge <- county_level %>% group_by(state) %>% filter(both_sexes_2012 == max(both_sexes_2012)) %>% select(state, location, both_sexes_2012) 

# What is the county with the largest increase in male binge drinking between 2002 and 2012?
# Your answer should include the county, state, and value of interest

largest_male_binge_inc <- county_level %>% mutate(increase = males_2012 - males_2002) %>% filter(increase == max(increase)) %>% select(state, location, increase)

# How many counties experienced an increase in male binge drinking between 2002 and 2012?
# Your answer should be an integer (a dataframe with only one value is fine)

male_increase_count <- nrow(county_level %>% mutate(increase = males_2012 - males_2002) %>% filter(increase > 0))

# What percentage of counties experienced an increase in male binge drinking between 2002 and 2012?
# Your answer should be a fraction or percent (we're not picky)

percent_inc_male_binge <- round(male_increase_count/nrow(county_level) * 100, 2)

# How many counties observed an increase in female binge drinking in this time period?
# Your answer should be an integer (a dataframe with only one value is fine)

female_increase_count <- nrow(county_level %>% mutate(increase = females_2012 - females_2002) %>% filter(increase > 0))

# What percentage of counties experienced an increase in FEmale binge drinking between 2002 and 2012?
# Your answer should be a fraction or percent (we're not picky)

percent_inc_female_binge <- round(female_increase_count/nrow(county_level) * 100, 2)

# How many counties experienced a rise in female binge drinking *and* a decline in male binge drinking?
# Your answer should be an integer (a dataframe with only one value is fine)

female_inc_male_dec <- nrow(county_level %>% mutate(increase_females = females_2012 - females_2002, increase_males = males_2012 - males_2002) %>% filter(increase_females > 0, increase_males < 0))

################################### Joining Data ###################################
## You'll often have to join different datasets together in order to ask more involved questions of your
## dataset.
# In order to join our datasets together, you'll have to rename their columns to differentiate them

# First, rename all prevalence columns in the any.drinking dataset to the have prefix "any."
## Hint: you can get (and set!) column names using the colnames function. This may take multiple lines of
## code.

colnames(any_drinking) = paste0("any.", colnames(any_drinking))

# Then, rename all prevalence columns in the binge.drinking dataset to the have prefix "binge."
## Hint: you can get (and set!) column names using the colnames function. This may take multiple lines of
## code.

colnames(binge_drinking) = paste0("binge.", colnames(binge_drinking))

# Then, create a dataframe with all of the columns from both datasets. 
# Think carefully about the *type* of join you want to do, and what the *identifying columns* are

joined_drinking <- any_drinking %>% full_join(binge_drinking, by = c('any.location' = 'binge.location', 'any.state' = 'binge.state'))

# Create a column of difference between `any` and `binge` drinking for both sexes in 2012

joined_drinking <- mutate(joined_drinking, any_binge_diff_2012 = any.both_sexes_2012 - binge.both_sexes_2012)

# Which location has the greatest *absolute* difference between `any` and `binge` drinking?
# Your answer should be a one row data frame with the state, location, and value of interest (difference)

max_abs_diff <- filter(joined_drinking, any_binge_diff_2012 == max(abs(any_binge_diff_2012))) %>% select(any.location, any.state, any_binge_diff_2012)

# Which location has the smallest *absolute* difference between `any` and `binge` drinking?
# Your answer should be a one row data frame with the state, location, and value of interest (difference)

min_abs_diff <- filter(joined_drinking, any_binge_diff_2012 == min(abs(any_binge_diff_2012))) %>% select(any.location, any.state, any_binge_diff_2012)

################################### Write a function to ask your own question(s) ###################################
## Even in an entry level data analyst role, people are expected to come up with their own questions of
## interest
# (not just answer the questions that other people have). For this section, you should *write a function*
# that allows you to ask the same question on different subsets of data. 
# For example, you may want to ask about the highest/lowest drinking level given a state or year. 
# The purpose of your function should be evident given the input parameters and function name. 
## After writing your function, *demonstrate* that the function works by passing in different parameters to
## your function.

# Gets all relevant COUNTY info given a state and year
GetInfoGivenYearState <- function(a_year, a_state) {
  return(any_drinking %>% filter(any.state == a_state, any.state != any.location) %>% select(any.state, any.location, contains(a_year)))
} 

GetInfoGivenYearState("2012", "California")
GetInfoGivenYearState("2003", "Oklahoma")

################################### Challenge ###################################

# Using your function from part 1 (that wrote a .csv file given a state name), write a separate file 
# for each of the 51 states (including Washington D.C.)
# The challenge is to do this in a *single line of (concise) code*

lapply(state_level$state, SaveState)

## Using a dataframe of your choice from above, write a function that allows you to specify a *year* and
## *state* of interest,
# that saves a .csv file with observations from that state's counties (and the state itself) 
# It should only write the columns `state`, `location`, and data from the specified year. 
# Before writing the .csv file, you should *sort* the data.frame in descending order
# by the both_sexes drinking rate in the specified year. 
# Again, make sure the file you save in the output directory indicates the year and state. 
# Note, this will force you to confront how dplyr uses *non-standard evaluation*
# Hint: https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
# Make sure to exclude rownames

Challenge <- function(my_state, my_year) {
  result <- any_drinking %>% filter(any.state == my_state) %>% select(any.location, any.state, contains(my_year)) %>% arrange_(paste0('-any.both_sexes_', my_year))
  write.csv(result, paste0('output/challenge_',my_state, my_year,'.csv'), row.names = FALSE)
}

# Demonstrate that your function works by passing a year and state of your interest to the function

Challenge("California", "2012")
Challenge("Washington", "2005")
