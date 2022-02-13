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
covidData <- read.csv("./countyCovidData.csv")

# Check out some features of your data
dim(covidData)
nrow(covidData)
ncol(covidData)
head(covidData)
tail(covidData)
View(covidData)

# Simple subsetting of data
covidData[1,]
covidData[1:5, ]
covidData[, 1:5]
covidData[1:5, 1:5]

# Check features of read.csv()
?read.csv
covidData <- covidData[, -1]

# National level data
natData <- read.csv("./nationalLevelData.csv")
head(natData)
natData <- natData[,-1]

# Adding new columns with mathematical operations
?log10
natData$log10TotalCases <- log10(natData$TotalCases)
natData$casesPer1000 <- natData$TotalCases/(natData$Population/1000)
head(natData)

# Add new columns with string manipulation
natData$region_cap <- tools::toTitleCase(as.character(natData$region))
head(natData)

# T Tests
# Check regional difference
natData[natData$Half == "West",]
natData[natData$Half == "West", "casesPer1000"]
group_west <- natData[natData$Half == "West", "casesPer1000"]
group_east <- natData[natData$Half == "East", "casesPer1000"]
t.test(group_west, group_east)

# Check voting difference
group_trump <- natData[natData$Vote == "Trump", "casesPer1000"]
group_biden <- natData[natData$Vote == "Biden", "casesPer1000"]
t.test(group_trump, group_biden)

# Check NBA difference 
group_noTeam <- natData[natData$NBA == "None", "casesPer1000"]
group_hasTeam <- natData[natData$NBA == "West" | natData$NBA == "East", "casesPer1000"]
t.test(group_noTeam, group_hasTeam)

group_westTeam <- natData[natData$NBA == "West", "casesPer1000"]
group_eastTeam <- natData[natData$NBA == "East", "casesPer1000"]
t.test(group_westTeam, group_eastTeam)

# Example Join
test_merge <- merge(covidData, natData)
View(head(test_merge))

### Plot data with ggplot2 and save a graph - Eric

# Install ggplot2 package
#install.packages("ggplot2")
library(ggplot2)

## plot a bar plot with ggplot2
bar_plot <- ggplot(natData) +
  aes(x = State, y = casesPer1000, fill = Half) +
  geom_bar(stat = "identity")
bar_plot

# add a main title and axis titles
bar_plot <- bar_plot +
  labs(title = "US States: East vs West COVID Edition", x = "State", y = "Cases per 1000")
bar_plot

# but Eric, I don't like the aesthetic. So let's add some pzazz
bar_plot_cool <- bar_plot + 
  scale_fill_manual(
    ""
    , values = c("East" = "coral2", "West" = "cornflowerblue")
    ) +
  theme_minimal()
bar_plot_cool

# that's cool, but it won't fly in an academic paper...
bar_plot <- bar_plot + 
  geom_bar(stat = "identity", color = "black", width = 0.75) + 
  theme_minimal()
bar_plot

## plot a line graph with ggplot2
# Let's plot the cumulative number of cases in Multnomah County, OR since the pandemic began
# first isolate the cases that occured in Multnomah county
multnomahCounty <- covidData[covidData$County.Name == "Multnomah County ", ]

# quickly turning the date column into an actual date...don't worry about this yet
multnomahCounty$Date <- as.POSIXct(multnomahCounty$Date)

# time to plot
line_plot <- ggplot(multnomahCounty) +
  aes(x = Date, y = Count, group = 1) +
  geom_line(color = "red") + 
  labs(
    title = "Cumulative COVID-19 Cases in Multnomah County OR"
    , x = "Date"
    , y = "Cumulative Cases"
  ) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
  )
line_plot

# how many cases have there been since we started school?
line_plot <- line_plot +
  geom_vline(xintercept = as.numeric(as.POSIXct("2021-08-09")), color = "blue", linetype="dotted")
line_plot

## save a plot
# I made a pretty picture, how do I share it on social media? (or put it in a paper. you do you.)
?ggsave
ggsave("multnomah-county-cases.png", line_plot, dpi = 300)

# your turn! can you save the bar plot we made?

### BONUS!!!
#install.packages("remotes")

# anyone like bernie?
remotes::install_github("R-CoderDotCom/ggbernie")
library(ggbernie)

bernie_plot <- ggplot(natData) +
  aes(x = Population, y = casesPer1000) +
  geom_bernie() +
  geom_text(aes(label = State), color = "green")
bernie_plot

# who likes cats?
remotes::install_github("Gibbsdavidl/CatterPlots")
library(CatterPlots)
cat_plot <- multicat(x = 1:10, y = 1:10, type = "point", catcolor = rainbow(10), cat = 1:10)