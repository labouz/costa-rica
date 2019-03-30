library(tidyverse)
library(readxl)


#import mortality data from 2014 for the cantons

#MALES
cantonMortM <- read_excel(path = "data/raw/Mortality/MORT.MAS FREC.POR CANTON HOMBRES 2014.xls", 
                       sheet = "TASA", range = "A8:N98",
                       col_names = TRUE) 
#remove null column
cantonMortM <- cantonMortM[,-2]

cantonMortM_wide <- cantonMortM %>% 
  #filter na rows
  filter(!is.na(`PROVINCIA Y CANTON`)) %>% 
  #keep only the rates for the cantons not cities 
  filter(`PROVINCIA Y CANTON` %in% c("SAN JOSE", "ALAJUELA", "CARTAGO", "HEREDIA",
                                     "GUANACASTE", "PUNTARENAS", "LIMON")) %>% 
  #keep the rows where the total is greater - want canton rates not rates for cities of same name
  group_by(`PROVINCIA Y CANTON`) %>% 
  slice(which.max(TOTAL)) %>% 
  #remove total columns
  select(-TOTAL)

#transform wide to long 
cantonMortM_long <- cantonMortM_wide %>% 
  gather(cancer, rate, -`PROVINCIA Y CANTON`) %>% 
  mutate(sex = "VARONES")


#FEMALES

cantonMortF <- read_excel(path = "data/raw/Mortality/MORT.MAS FREC. POR CANTON MUJERES 2014.xls", 
                          sheet = "TASA", range = "A8:N98",
                          col_names = TRUE) 
#remove null column
cantonMortF <- cantonMortF[,-2]

cantonMortF_wide <- cantonMortF %>% 
  #filter na rows
  filter(!is.na(`PROVINCIA Y CANTON`)) %>% 
  #keep only the rates for the cantons not cities 
  filter(`PROVINCIA Y CANTON` %in% c("SAN JOSE", "ALAJUELA", "CARTAGO", "HEREDIA",
                                     "GUANACASTE", "PUNTARENAS", "LIMON")) %>% 
  #keep the rows where the total is greater - want canton rates not rates for cities of same name
  group_by(`PROVINCIA Y CANTON`) %>% 
  slice(which.max(TOTAL)) %>% 
  #remove total columns
  select(-TOTAL)

#transform wide to long 
cantonMortF_long <- cantonMortF_wide %>% 
  gather(cancer, rate, -`PROVINCIA Y CANTON`) %>% 
  mutate(sex = "MUJERES") 


#JOIN MALES AND FEMALES TO ONE TABLE
cantonMort <- bind_rows(cantonMortF_long, cantonMortM_long) %>% 
  mutate(rate = round(as.numeric(rate), 2)) %>% 
  #adjust cancer names
  mutate(cancer = case_when(cancer == "DEL UTERO" ~ "CUELLO DEL UTERO",
                            cancer == "Y PULMON" ~ "PULMON",
                            TRUE ~ cancer))

saveRDS(cantonMort, file = "./Data/cantonMortality.rds")
