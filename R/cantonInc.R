library(tidyverse)
library(readxl)


#import incidence data from 2014 for the cantons

#MALES
cantonIncM <- read_xlsx(path = "./data/raw/INCIDENCIA_2014_DIFERENTES_CARACTERISTICAS.xlsx", 
                       sheet = "10 MAS FREC. CANTON VARONES", range = "A8:Y115",
                       col_names = TRUE) 
cantonIncM_wide <- cantonIncM %>% 
  #filter na rows
  filter(!is.na(TOTAL)) %>% 
  #keep only the rates for the cantons not cities 
  filter(`PROVINCIA Y CANTON` %in% c("SAN JOSE", "ALAJUELA", "CARTAGO", "HEREDIA",
                                     "GUANACASTE", "PUNTARENAS", "LIMON")) %>% 
  #keep the rows where the total is greater - want canton rates not rates for cities of same name
  group_by(`PROVINCIA Y CANTON`) %>% 
  slice(which.max(TOTAL)) 

#transform wide to long 
cantonIncM_long <- cantonIncM_wide %>% 
  gather(cancer, x, -`PROVINCIA Y CANTON`) %>% 
  mutate(varType = if_else(str_detect(cancer, "..[0-9]"), "rate", "n"))  %>% 
  mutate(cancer = case_when(cancer == "..3" ~ "TOTAL",
                            cancer == "..5" ~ "PIEL",
                            cancer == "..7" ~ "PROSTATA",
                            cancer == "..9" ~ "ESTOMAGO",
                            cancer == "..11" ~ "COLON",
                            cancer == "..13" ~ "Y RETICULOEND.",
                            cancer == "..15" ~ "VEJIGA",
                            cancer == "..17" ~ "GANGLIOS LINF.",
                            cancer == "..19" ~ "PULMON",
                            cancer == "..21" ~ "RECTO",
                            cancer == "..23" ~ "TIROIDES",
                            cancer == "..25" ~ "LOCALIZAC.",
                            TRUE ~ cancer)) 
  

cantonIncM_long <- cantonIncM_long %>% 
  spread(varType, x) %>% 
  mutate(sex = "VARONES")


#FEMALES
cantonIncF <- read_xlsx(path = "./data/raw/INCIDENCIA_2014_DIFERENTES_CARACTERISTICAS.xlsx", 
                        sheet = "10 MAS FREC. CANTON MUJERES", range = "A8:Y115",
                        col_names = TRUE) 
cantonIncF_wide <- cantonIncF %>% 
  #filter na rows
  filter(!is.na(TOTAL)) %>% 
  #keep only the rates for the cantons not cities 
  filter(`PROVINCIA Y CANTON` %in% c("SAN JOSE", "ALAJUELA", "CARTAGO", "HEREDIA",
                                     "GUANACASTE", "PUNTARENAS", "LIMON")) %>% 
  #keep the rows where the total is greater - want canton rates not rates for cities of same name
  group_by(`PROVINCIA Y CANTON`) %>% 
  slice(which.max(TOTAL)) 

#transform wide to long 
cantonIncF_long <- cantonIncF_wide %>% 
  gather(cancer, x, -`PROVINCIA Y CANTON`) %>% 
  mutate(varType = if_else(str_detect(cancer, "..[0-9]"), "rate", "n"))  %>%  
  mutate(cancer = case_when(cancer == "..3" ~ "TOTAL",
                            cancer == "..5" ~ "MAMA",
                            cancer == "..7" ~ "PIEL",
                            cancer == "..9" ~ "CUELLO DEL UTERO",
                            cancer == "..11" ~ "TIROIDES",
                            cancer == "..13" ~ "ESTOMAGO",
                            cancer == "..15" ~ "COLON",
                            cancer == "..17" ~ "CUERPO DEL UTERO",
                            cancer == "..19" ~ "GANGLIOS LINF.",
                            cancer == "..21" ~ "OVARIO",
                            cancer == "..23" ~ "PULMON",
                            cancer == "..25" ~ "LOCALIZAC.",
                            TRUE ~ cancer)) 


cantonIncF_long <- cantonIncF_long %>% 
  spread(varType, x) %>% 
  mutate(sex = "MUJERES")


#JOIN MALES AND FEMALES TO ONE TABLE
cantonInc <- bind_rows(cantonIncF_long, cantonIncM_long) %>% 
  mutate(rate = round(as.numeric(rate), 2))

saveRDS(cantonInc, file = "./data/cantonIncidence.rds")
