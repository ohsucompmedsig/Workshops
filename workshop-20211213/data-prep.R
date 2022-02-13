# Process data for OHSU CompMed SIG Workshop

library(tidyverse)
library(lubridate)

rawCovidData <- read_file(
  "https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv"
) %>%
  iconv("ASCII", "UTF-8") %>%
  read.table(text = ., sep = ",", header = TRUE, stringsAsFactors = FALSE)

countyMeasuresData <- read_file(
  "https://raw.githubusercontent.com/emcramer/covid19project/master/data/countyMeasures.csv"
) %>%
  iconv("ASCII", "UTF-8") %>%
  read.table(text = ., sep = ",", header = TRUE, stringsAsFactors = FALSE)
countyMeasuresData$Local.Measures.Date <- ymd(countyMeasuresData$Local.Measures.Date)

# add when local measures were taken
covidData <- left_join(covidData, countyMeasuresData)

# pivot the data to be longer along the date dimension, and remove appended "X"
convDate <- function(x) {
  print(colnames(x))
  tmp <- gsub("X", "", x$Date)
}
countyDateData <- pivot_longer(covidData
                               , cols = starts_with("X")
                               , names_to = "Date"
                               , values_to = "Count")
countyDateData$Date <- gsub("\\."
                            , "-"
                            , gsub("X"
                                   , ""
                                   , countyDateData$Date
                            )
) %>% 
  ymd()

# writeout
write.csv(countyDateData
          , "~/OneDrive/Documents/coding-projects/ohsu-compsig/Workshops/workshop-20211213/countyCovidData.csv"
          )

nationalLevelData <- countyDateData %>%
  add_column(region= setNames(tolower(state.name), state.abb)[toupper(countyDateData$State)]) %>%
  group_by(County.Name) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  group_by(region) %>%
  summarise(TotalCases = sum(Count)) %>%
  mutate(logTotalCases = log10(TotalCases))

# writeout
write.csv(nationalLevelData
          , "~/OneDrive/Documents/coding-projects/ohsu-compsig/Workshops/workshop-20211213/nationalLevelData.csv"
)
