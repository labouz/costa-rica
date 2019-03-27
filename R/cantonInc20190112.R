library(tidyverse)
library(readxl)


#import incidence data from 2014 for the cantons

#MALES
cantonIncM <- read_xlsx(path = "./Data/INCIDENCIA_2014_DIFERENTES_CARACTERISTICAS.xlsx", 
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
  slice(which.max(TOTAL)) %>% 
  #remove total columns
  select(-TOTAL, -X__1)

#transform wide to long 
cantonIncM_long <- cantonIncM_wide %>% 
  gather(cancer, x, -`PROVINCIA Y CANTON`) %>% 
  mutate(varType = if_else(str_detect(cancer, "X__"), "rate", "n"))  %>% 
  mutate(cancer = case_when(cancer == "X__2" ~ "PIEL",
                              cancer == "X__3" ~ "PROSTATA",
                              cancer == "X__4" ~ "ESTOMAGO",
                              cancer == "X__5" ~ "COLON",
                              cancer == "X__6" ~ "Y RETICULOEND.",
                              cancer == "X__7" ~ "VEJIGA",
                              cancer == "X__8" ~ "GANGLIOS LINF.",
                              cancer == "X__9" ~ "PULMON",
                              cancer == "X__10" ~ "RECTO",
                              cancer == "X__11" ~ "TIROIDES",
                              cancer == "X__12" ~ "LOCALIZAC.",
                              TRUE ~ cancer)) 
  

cantonIncM_long <- cantonIncM_long %>% 
  spread(varType, x) %>% 
  mutate(sex = "VARONES")


#FEMALES
cantonIncF <- read_xlsx(path = "./Data/INCIDENCIA_2014_DIFERENTES_CARACTERISTICAS.xlsx", 
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
  slice(which.max(TOTAL)) %>% 
  #remove total columns
  select(-TOTAL, -X__1)

#transform wide to long 
cantonIncF_long <- cantonIncF_wide %>% 
  gather(cancer, x, -`PROVINCIA Y CANTON`) %>% 
  mutate(varType = if_else(str_detect(cancer, "X__"), "rate", "n"))  %>% 
  mutate(cancer = case_when(cancer == "X__2" ~ "MAMA",
                            cancer == "X__3" ~ "PIEL",
                            cancer == "X__4" ~ "CUELLO DEL UTERO",
                            cancer == "X__5" ~ "TIROIDES",
                            cancer == "X__6" ~ "ESTOMAGO",
                            cancer == "X__7" ~ "COLON",
                            cancer == "X__8" ~ "CUERPO DEL UTERO",
                            cancer == "X__9" ~ "GANGLIOS LINF.",
                            cancer == "X__10" ~ "OVARIO",
                            cancer == "X__11" ~ "PULMON",
                            cancer == "X__12" ~ "LOCALIZAC.",
                            TRUE ~ cancer)) 


cantonIncF_long <- cantonIncF_long %>% 
  spread(varType, x) %>% 
  mutate(sex = "MUJERES")


#JOIN MALES AND FEMALES TO ONE TABLE
cantonInc <- bind_rows(cantonIncF_long, cantonIncM_long) %>% 
  mutate(rate = round(as.numeric(rate), 2))

saveRDS(cantonInc, file = "./Data/cantonIncidence.rds")
