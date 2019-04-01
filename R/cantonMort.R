library(tidyverse)
library(readxl)


#import mortality data from 2014 for the cantons

#MALES
cantonMortM <- read_excel(path = "data/raw/Mortality/MORT.MAS FREC.POR CANTON HOMBRES 2014.xls", 
                       sheet = "NUMERO Y TASA", range = "A8:Z104",
                       col_names = TRUE) 

#remove null column
cantonMortM <- cantonMortM[,c(-2)]


cantonMortM_wide <- cantonMortM %>% 
  #filter na rows
  filter(!is.na(`PROVINCIA Y CANTON`)) %>% 
  #keep only the rates for the cantons not cities 
  filter(`PROVINCIA Y CANTON` %in% c("SAN JOSE", "ALAJUELA", "CARTAGO", "HEREDIA",
                                     "GUANACASTE", "PUNTARENAS", "LIMON")) 

#transform wide to long 
cantonMortM_long <- cantonMortM_wide %>% 
  gather(cancer, x, -`PROVINCIA Y CANTON`) %>% 
  mutate(varType = if_else(str_detect(cancer, "..[0-9]"), "rate", "n"))  %>% 
  mutate(cancer = case_when(cancer == "..4" ~ "TOTAL",
                            cancer == "..6" ~ "PROSTATA",
                            cancer == "..8" ~ "ESTOMAGO",
                            cancer == "..10" ~ "COLON",
                            cancer == "..22" ~ "ENCEFALO",
                            cancer == "..20" ~ "PANCREAS",
                            cancer == "..18" ~ "LEUCEMIAS",
                            cancer == "..12" ~ "Y PULMON",
                            cancer == "..16" ~ "LINFOMAS",
                            cancer == "..14" ~ "HIGADO",
                            cancer == "..24" ~ "RECTO",
                            cancer == "..26" ~ "LOCALIZAC.",
                            TRUE ~ cancer)) 

#transform wide to long 
cantonMortM_long <- cantonMortM_long %>% 
  spread(varType, x) %>% 
  mutate(sex = "VARONES")

#FEMALES

cantonMortF <- read_excel(path = "data/raw/Mortality/MORT.MAS FREC. POR CANTON MUJERES 2014.xls", 
                          sheet = "NUMERO Y TASA", range = "A8:Z104",
                          col_names = TRUE) 
#remove null column
cantonMortF <- cantonMortF[,-2]

cantonMortF_wide <- cantonMortF %>% 
  #filter na rows
  filter(!is.na(`PROVINCIA Y CANTON`)) %>% 
  #keep only the rates for the cantons not cities 
  filter(`PROVINCIA Y CANTON` %in% c("SAN JOSE", "ALAJUELA", "CARTAGO", "HEREDIA",
                                     "GUANACASTE", "PUNTARENAS", "LIMON"))  
 

#transform wide to long 
cantonMortF_long <- cantonMortF_wide %>% 
  gather(cancer, x, -`PROVINCIA Y CANTON`) %>% 
  mutate(varType = if_else(str_detect(cancer, "..[0-9]"), "rate", "n"))  %>% 
  mutate(cancer = case_when(cancer == "..4" ~ "TOTAL",
                            cancer == "..6" ~ "MAMA",
                            cancer == "..8" ~ "ESTOMAGO",
                            cancer == "..10" ~ "COLON",
                            cancer == "..22" ~ "OVARIO",
                            cancer == "..14" ~ "PANCREAS",
                            cancer == "..24" ~ "LEUCEMIAS",
                            cancer == "..12" ~ "CUELLO DEL UTERO",
                            cancer == "..20" ~ "LINFOMAS",
                            cancer == "..16" ~ "HIGADO",
                            cancer == "..18" ~ "PULMON",
                            cancer == "..26" ~ "LOCALIZAC.",
                            TRUE ~ cancer)) %>% 
  mutate(cancer = if_else(cancer == "DEL UTERO", "CUELLO DEL UTERO", cancer))

#transform wide to long 
cantonMortF_long <- cantonMortF_long %>% 
  spread(varType, x) %>% 
  mutate(sex = "MUJERES")


#JOIN MALES AND FEMALES TO ONE TABLE
cantonMort <- bind_rows(cantonMortF_long, cantonMortM_long) %>% 
  mutate(rate = round(as.numeric(rate), 2)) %>% 
  #adjust cancer names
  mutate(cancer = case_when(cancer == "Y PULMON" ~ "PULMON",
                            TRUE ~ cancer))

saveRDS(cantonMort, file = "./data/cantonMortality.rds")
