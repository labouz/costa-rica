library(tidyverse)
library(readr)
library(readxl)
library(lettercase)

#csv file with cases per year
#casesYear <- read_csv2("./data/raw/cancerPerYear.csv", col_names = TRUE)

maleCases2014 <- read_xlsx("./data/raw/INCIDENCIA_2014_DIFERENTES_CARACTERISTICAS.xlsx", 
                       sheet = "LOC. Y GR. EDAD VARONES", range = "B9:C78", 
                       col_names = FALSE)
colnames(maleCases2014) <- c("site", "males")

femaleCases2014 <- read_xlsx("./data/raw/INCIDENCIA_2014_DIFERENTES_CARACTERISTICAS.xlsx", 
                           sheet = "LOC. Y GR. EDAD MUJERES", range = "B9:C78", 
                           col_names = FALSE)
colnames(femaleCases2014) <- c("site", "females")

cases2014 <- left_join(maleCases2014, femaleCases2014, by = "site") %>% 
  group_by(site) %>% 
  summarise(people = sum(males,females)) %>% 
  mutate(site = str_ucfirst(str_decapitalize(site))) %>% 
  filter(people > 10)
  

#year 2014
cases14 <- cases2014 %>% 
  arrange(desc(people))

saveRDS(cases14, "./Data/2014CancerCases.rds")
cases14 <- read_rds("./data/2014CancerCases.rds")


#Mortality

#males
#csv file with cases per year
maleMort14 <- read_excel("./data/raw/Mortality/MORT.HOMBRES POR LOCALIZ.Y GRUP.EDAD 2014.xls",
                         sheet = "Número",
                         range = "A5:D124", col_names = c("code","cancer","remove","males"))

#delete extra null col
maleMort14 <- maleMort14 %>% 
  select(-remove) %>% 
  #filter unspecified sites
  filter(!code %in% c("C02","C06","C08","C14","C26","C39","C41","C44","C55",
                      "C57","C63","C68","C75","C76","C77","C78","C79","C80",
                      "C88","C94","C95","C96","C97")) 

  
#the names of several cancers are spread on multiple rows: collapsing them
maleMort14_betterName <- maleMort14 %>% 
  group_by(code) %>% 
  mutate(site = str_c(cancer, collapse = " ")) %>% 
  na.omit(males) %>% 
  select(-cancer) 

#females
#csv file with cases per year
femaleMort14 <- read_excel("./data/raw/Mortality/MORT.MUJERES POR LOCALIZ.Y GRUP.EDAD 2014.xls", 
                           sheet = "Número",
                           range = "A5:D124", col_names = c("code","cancer","remove","females"))

#delete extra null col
femaleMort14 <- femaleMort14 %>% 
  select(-remove) %>% 
  #filter unspecified sites
  filter(!code %in% c("C02","C06","C08","C14","C26","C39","C41","C44","C55",
                      "C57","C63","C68","C75","C76","C77","C78","C79","C80",
                      "C88","C94","C95","C96","C97")) 

#the names of several cancers are spread on multiple rows: collapsing them
femaleMort14_betterName <- femaleMort14 %>% 
  group_by(code) %>% 
  mutate(site = str_c(cancer, collapse = " ")) %>% 
  na.omit(females) %>% 
  select(-cancer) 

#combine males and females
mort14 <- left_join(maleMort14_betterName, femaleMort14_betterName[1:2], by = "code") %>% 
  select(code, site, males, females) %>% 
  mutate(females = if_else(is.na(females), 0, females)) %>% 
  group_by(code, site, males, females) %>% 
  summarise(people = sum(males, females)) %>% 
  #change site name to sentence case from all caps
  ungroup() %>% 
  mutate(site = str_ucfirst(str_decapitalize(site))) %>% 
  filter(people > 10) %>% 
  arrange(desc(people))


saveRDS(mort14, "./Data/2014CancerMort.rds")
