library(tidyverse)
library(readr)
library(readxl)

#csv file with cases per year
casesYear <- read_csv2("./data/cancerPerYear.csv", col_names = TRUE)

#year 2014
cases14 <- select(casesYear, -1) %>% #remove first column - numbered list
  filter(year == 2014) %>% 
  arrange(desc(Cancer))

saveRDS(cases14, "./Data/2014CancerCases.rds")
cases14 <- read_rds("./data/2014CancerCases.rds")


#Mortlity

#males
#csv file with cases per year
maleMort14 <- read_excel("Data/Mortality/MORT.MAS FREC.HOMBRES 2014.xls", 
                         range = "B9:D23", col_names = c("cancer","remove","males"))
#delete extra null col and remove na rows
maleMort14 <- na.omit(maleMort14[,c(1,3)])

#females
#csv file with cases per year
femaleMort14 <- read_excel("Data/Mortality/MORT.MAS FREC.Mujeres 2014.xls", 
                         range = "B9:D23", col_names = c("cancer","remove","females"))
#delete extra null col and remove na rows
femaleMort14 <- na.omit(femaleMort14[,c(1,3)])

#combine
mort14 <- left_join(maleMort14, femaleMort14, by = "cancer") %>% 
  mutate(females = if_else(is.na(females), 0, females)) %>% 
  group_by(cancer, males, females) %>% 
  summarise(total = sum(males, females)) %>% 
  #remove total row
  filter(cancer != "TOTAL") %>% 
  arrange(desc(total))


saveRDS(mort14, "./Data/2014CancerMort.rds")
