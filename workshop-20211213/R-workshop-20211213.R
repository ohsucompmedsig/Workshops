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

### Input dataset, split in groups, perform T test - Sid

# load the data set into a variable called "covidData"


# Check out some features of your data

# Simple subsetting of data


# Check features of read.csv()
?read.csv


# National level data


# Adding new columns with mathematical operations
?log10


# Add new columns with string manipulation


# T Tests
# Check regional difference


# Check voting difference


# Check NBA difference 


# Example Join


### Plot data with ggplot2 and save a graph - Eric

# Install ggplot2 package


## plot a bar plot with ggplot2


# add a main title and axis titles


# but Eric, I don't like the aesthetic. So let's add some pzazz


# that's cool, but it won't fly in an academic paper...


## plot a line graph with ggplot2
# Let's plot the cumulative number of cases in Multnomah County, OR since the pandemic began
# first isolate the cases that occured in Multnomah county
multnomahCounty <- covidData[covidData$County.Name == "Multnomah County ", ]

# quickly turning the date column into an actual date...don't worry about this yet
multnomahCounty$Date <- as.POSIXct(multnomahCounty$Date)

# time to plot


# how many cases have there been since we started school?


## save a plot
# I made a pretty picture, how do I share it on social media? (or put it in a paper. you do you.)
?ggsave


# your turn! can you save the bar plot we made?

### BONUS!!!
#install.packages("remotes")

# anyone like bernie?
remotes::install_github("R-CoderDotCom/ggbernie")


# who likes cats?
remotes::install_github("Gibbsdavidl/CatterPlots")
