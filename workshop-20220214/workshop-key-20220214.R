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
# diabetic_data <- read.csv("./dataset_diabetes/diabetic_data.csv")
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

# There are many ways to make a nice looking table, probably some easier than
# this. Here, we will use ggplot2 to make 

# Install/Load ggplot2 library
#install.package("ggplot2")
library(ggplot2)

# Write your own function to make 2x2
my_tbt_plot <- function(tbt, 
                        exposure_lab, 
                        outcome_lab, 
                        text_size,
                        box_cols) {
  
  # We'll make an empty ggplot object and add elements as we go
  ggplot() + 
    
    # Remove all traditional chart elements e.g. axes, labels, etc.
    #theme_void() + 
    
    # Set dimensions of chart
    coord_cartesian(xlim = c(0,100),
                    ylim = c(0,100)) + 
    
    # Add four squares for TP, FN, FP, TN
    annotate("rect",
             xmin = c(33, 66, 33, 66), 
             ymin = c(0, 0, 33, 33),
             xmax = c(66, 99, 66, 99), 
             ymax = c(33, 33, 66, 66),
             color = "black",
             fill = box_cols, 
             alpha = 0.2) +
    
    # Add boxes around labels
    annotate("rect",
             xmin = c(33, 10), 
             ymin = c(66, 0),
             xmax = c(99, 33), 
             ymax = c(90, 66),
             color = "black",
             alpha = 0.0) +
    
    # Add pos/neg labels
    annotate("text", 
             x = c(49.5, 82.5, 28, 28),
             y = c(71, 71, 16.5, 49.5),
             label = c("(+)", "(-)", "(-)", "(+)"),
             fontface = "bold",
             size = text_size) +
    
    # Add labels
    annotate("text",
             x = c(18, 66),
             y = c(33, 82),
             label = c(exposure_lab, 
                       outcome_lab),
             fontface = "bold",
             angle = c(90, 0), 
             size = text_size) +
    
    # Add values
    annotate("text",
             x = c(49.5, 82.5, 49.5, 82.5),
             y = c(49.5, 49.5, 16.5, 16.5),
             label = c(tbt[1, 1],
                       tbt[1, 2],
                       tbt[2, 1],
                       tbt[2, 2]),
             fontface = "bold",
             size = text_size)
}

# Run Function with my parameters
my_tbt_plot(tbt = two_by_two, 
            exposure_lab = "Metformin",
            outcome_lab = "Readmission",
            text_size = 6,
            box_cols = c("red", "blue", "blue", "red"))

sample_tbt <- my_tbt_plot(tbt = two_by_two, 
                          exposure_lab = "Metformin",
                          outcome_lab = "Readmission",
                          text_size = 6,
                          box_cols = c("red", "blue", "blue", "red"))

# Save plot
# ggsave("sample_tbt.png", sample_tbt, device = "png", dpi = 300)

# Or maybe a forest plot (would require we make more binarized variables)

# Install/load library patchwork for layering plots
library(patchwork)

df <- data.frame(
  drug = names(diabetic_data[25:42]),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(df)) {
  diabetic_data_temp <- diabetic_data
  diabetic_data_temp$readm <- tolower(diabetic_data_temp$readmitted) != "no"
  diabetic_data_temp$drug <- tolower(diabetic_data_temp[, names(diabetic_data_temp[25:42])[i]]) != "no"
  tbt <- table(
    "Any Drug" = factor(diabetic_data_temp$drug, levels = c("TRUE", "FALSE"))
    , "Hospital Readmission" = factor(diabetic_data_temp$readm, levels = c("TRUE", "FALSE"))
  )
  df[i, "drug_tp"] <- tbt[1,1]
  df[i, "drug_fn"] <- tbt[1,2]
  df[i, "drug_fp"] <- tbt[2,1]
  df[i, "drug_tn"] <- tbt[2,2]
  df[i, "drug_rrr"] <- calc_rrr(tbt)
  df[i, "drug_ci_low"] <- calc_ci(tbt)[1]
  df[i, "drug_ci_high"] <- calc_ci(tbt)[2]
}
df <- df[complete.cases(df),]

a <- ggplot() +
  theme_void() +
  coord_cartesian(xlim = c(0, 200),
                  ylim = c(0, 120)) +
  annotate("text",
           x = 0,
           y = seq.int(10, 100, length.out = 16), 
           label = stringr::str_to_title(df$drug),
           hjust = 0,
           fontface = "bold") +
  annotate("text",
           x = 50,
           y = seq.int(10, 100, length.out = 16), 
           label = prettyNum(df$drug_tp, big.mark = ","),
           hjust = 1) +
  annotate("text",
           x = 70,
           y = seq.int(10, 100, length.out = 16), 
           label = prettyNum(df$drug_tn, big.mark = ","),
           hjust = 1) +
  annotate("text",
           x = 90,
           y = seq.int(10, 100, length.out = 16), 
           label = prettyNum(df$drug_fp, big.mark = ","),
           hjust = 1) +
  annotate("text",
           x = 110,
           y = seq.int(10, 100, length.out = 16), 
           label = prettyNum(df$drug_fn, big.mark = ","),
           hjust = 1) +
  annotate("segment", 
           x = 0, 
           xend = 200,
           y = 106,
           yend = 106,
           size = 2) +
  annotate("text",
           x = 200,
           y = seq.int(10, 100, length.out = 16), 
           label = paste0(paste(round(df[,6], digits = 2), 
                                paste(round(df[,7], digits = 2), round(df[,8], 
                                                                       digits = 2), sep = "-"), sep = " ("), ")"),
           hjust = 1) +
  annotate("text", 
           x = c(0, 50, 70, 90, 110, 200),
           y = 112,
           hjust = c(0, 1, 1, 1, 1, 1),
           label = c("Medication",
                     "Drug+", "Drug-", "Drug+", "Drug-", 
                     "Relative Risk (95% CI)"), 
           fontface = "bold") +
  annotate("segment",
           x = c(38, 78),
           xend = c(71, 111),
           y = c(116, 116),
           yend = c(116, 116)) +
  annotate("text", 
           x = c(64, 108),
           y = 120,
           hjust = c(1, 1),
           label = c("Readmitted", "Not Readmitted"), 
           fontface = "bold")


b <- ggplot() +
  coord_cartesian(xlim = c(0.25, 3),
                  ylim = c(10, 100)) +
  scale_x_sqrt(breaks=seq(0,3,0.5)) +
  theme_classic() + 
  annotate("segment",
           x = 1, xend = 1,
           y = 0, yend = 115,
           linetype = "dashed") +
  annotate("pointrange", 
           y = seq.int(10, 100, length.out = 16),
           x = df$drug_rrr,
           xmin = df$drug_ci_low,
           xmax = df$drug_ci_high) +
  theme(
    axis.line.y = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# tiff("table.tiff", height = 6, width = 10, units = 'in', res = 300)
a + inset_element(b, left = 0.59, bottom = 0.04, right = 0.815, top = 0.85)
# dev.off()


## Bonus!

# Add a TRUE/FALSE column based on "A1Cresult" for whether the patient's A1C 
# was above 8.
diabetic_data$A1C_lvl <- diabetic_data$A1Cresult == ">8"

# Build a 2x2 table showing whether a test based on A1C >300 predicts 
# readmission to the hospital.
a1c_two_by_two <- two_by_two <- table(
  "A1C > 8" = factor(diabetic_data$A1C_lvl, levels = c("TRUE", "FALSE"))
  , "Hospital Readmission" = factor(diabetic_data$any_readmission, levels = c("TRUE", "FALSE"))
)
a1c_two_by_two

# Write a function to calculate the sensitivity of this test.
calc_sensitivity <- function(tbt) {
  a <- tbt[1, 1]
  c <- tbt[2, 1]
  sn <- a/(a+c)
  return(sn)
}
calc_sensitivity(a1c_two_by_two)

# Write a function to calculate the specificity of this test.
calc_specificity <- function(tbt){
  b <- tbt[1, 2]
  d <- tbt[2, 2]
  sp <- d/(b+d)
  return(sp)
}
calc_specificity(a1c_two_by_two)

# What is the relative risk ratio of patients with A1C >8 to patients with
# lower A1C?
a1c_rrr <- calc_rrr(a1c_two_by_two)
a1c_rrr

## Double bonus!
# What is the prevalence of patients with A1C >8?
prevalence <- sum(diabetic_data$A1C_lvl)/nrow(diabetic_data) * 100
prevalence
