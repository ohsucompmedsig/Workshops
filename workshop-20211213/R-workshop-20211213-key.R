### OHSU Computation in Medicine SIG
# 
# BLHD Enrichment: Getting started with R for medical projects
# This hour has also been called:
#  "Biostats/epidemiology for the lazy"
# "Crunchin' numbers and makin' graphs"
# "Make the computer do the work"
# "5 tips Apple and Microsoft don't want you to know about making graphs!"
# "You'll never use Excel again after you see this!"
# 
# Or perhaps more accurately, "R skills for scholarly projects." If you're 
# interested in learning how to make sense from the gobbledygook of 
# biomedical data, and maybe how to write a little code, the come join the 
# Computation in Medicine SIG's inaugural workshop. We'll go over how to 
# download and install R and RStudio, how to load some data, and how to 
# make a pretty graph.


### Setup R, RStudio, Install packages - Spencer

# Please download and install R and RStudio. Instructions can be found at 
# https://github.com/ohsucompmedsig/Workshops/blob/main/workshop-20211213/Set_up_R_and_Rstudio.pdf

# Variables

# Functions and Arguments

# Comments

# Install ggplot2 package
install.packages("ggplot2")

### Input dataset, split in groups, perform T test - Sid

# load the data set into a variable called "covidData"
covidData <- read.csv("~/Downloads/covid_confirmed_usafacts.csv")

# split data into two groups

# do a t-test

### Plot data with ggplot2 and save a graph - Eric

# plot a bar plot with ggplot2

# plot a line graph with ggplot2

# save a plot

### BONUS!!!

# load a new data set: nationalLevelData.csv
# plot a bar plot showing the number of covid cases in each state
# save the bar plot
