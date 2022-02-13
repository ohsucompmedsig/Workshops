### OHSU Computation in Medicine SIG
## Enrichment Week Activity/Workshop 2022-02-14

# This workshop focuses on building a 2x2 table from a dataset, and 
# calculating relevant statistics such as odds ratios, risk ratios, and 
# confidence intervals. We will also go over how to make your own, custom 
# functions in R.

# We're going to use a data set from the
# UCI data set repository available here:
# https://archive.ics.uci.edu/ml/machine-learning-databases/00296/
# Download and unzip the folder with the dataset.

# Let's take a look at the relationship between given medications and
# the rate of readmission to the hospital.

# Quick shout-out to Ali Lokhandwala for helping us plan the session!

## Part 1 (Eric)

# load data into a variable in R
diabetic_data <- read.csv("~/Downloads/dataset_diabetes/diabetic_data.csv")

# Let's get a quick look at the top of the data
head(diabetic_data)

# To make a two by two table, we need to have two variables that are 
# "binarized"- they are yes/no or true/false for given categories. Like
# "did they receive a certain medication?" or "were they readmitted to the
# hospital?"
# We'll do that here and prepare our data for some statistics in the next part.

# add a column to the data indicating if the patient has been readmitted or not
diabetic_data$any_readmission <- tolower(diabetic_data$readmitted) != "no"

# add a column to the data indicating if the patient has taken any amount of 
# the drug metformin
diabetic_data$any_metformin <- tolower(diabetic_data$metformin) != "no"

# where are the true positives, true negatives, false positives, and false
# negatives?

# Now, we are going to calculate the 2x2 table with a built-in R 
# function. The we will write our own function to calculate the risk ratio
# of a patient being readmitted if they were given any amount of metformin.

# To make a properly formatted 2x2, we need to adjust how R "perceives" the 
# data. We do this by converting the new columns of data that we made to a 
# "factor" data type, and we set the order/level of the factors (i.e. it 
# should be displayed TRUE, and then FALSE)

# make a 2x2 table!
two_by_two <- table(
  "Any Metformin" = factor(diabetic_data$any_metformin, levels = c("TRUE", "FALSE"))
  , "Hospital Readmission" = factor(diabetic_data$any_readmission, levels = c("TRUE", "FALSE"))
)

# What does our 2x2 table look like?
two_by_two

# To make our own function in R, we can use the "function" keyword, and assign
# it to a variable name. For example:
# say_my_name <- function(name){
#   cat(c("Hello, I am ", name, "!"))
# }
# What goes between the "(" and ")" after the function keyword is a 
# parameter or an argument. This is something that gets used by our function.

# To make our function, we first need to recall the formula for the relative
# risk: RR = (a/(a+b))/(c/(c+d))
# Now, let's write a function that takes the 2x2 table as an argument, and 
# spits out ("returns") the relative risk ratio.

calc_rrr <- function(tbt){
  a <- tbt[1, 1]
  b <- tbt[1, 2]
  c <- tbt[2, 1]
  d <- tbt[2, 2]
  ratio <- (a/(a+b))/(c/(c+d))
  return(ratio)
}

# calculate the relative risk ratio from our 2x2 table
metformin_rrr <- calc_rrr(two_by_two)
metformin_rrr

# What does this number mean?

# add a function to calculate the confidence interval
calc_ci <- function(tbt){
  # get a, b, c, and d
  a <- tbt[1, 1]
  b <- tbt[1, 2]
  c <- tbt[2, 1]
  d <- tbt[2, 2]
  
  # use prior function to get the relative risk ratio
  rrr <- calc_rrr(tbt)
  
  # get zscore for 95% ci
  z <- qnorm(0.05)
  
  # calculate the relative values from 2x2 table
  rv <- sqrt((b/a)/(a+b) + (d/c)/(c+d))
  
  # calculate upper and lower bounds of ci
  upr <- exp(log(rrr) + z*rv)
  lwr <- exp(log(rrr) - z*rv)
  
  # return the ci
  return(c(upr, lwr))
}
metformin_ci <- calc_ci(two_by_two)
metformin_ci

# What does it mean when the confidence interval includes or excludes 1?

## Part 2 (Sid)
# It is probably important to show our two by two table to others in our final 
# write-up, poster, or publication. So let's make a graphic that shows what our 
# table looks like.

# Or maybe a forest plot (would require we make more binarized variables)

## Bonus!

# Add a TRUE/FALSE column based on "A1Cresult" for whether the patient's A1C 
# was above 300.


# Build a 2x2 table showing whether a test based on A1C >300 predicts 
# readmission to the hospital.


# Write a function to calculate the sensitivity of this test.


# Write a function to calculate the specificity of this test.


# What is the relative risk ratio of patients with A1C >300 to patients with
# lower A1C?

## Double bonus!
# What is the prevalence of patients with A1C >300?

