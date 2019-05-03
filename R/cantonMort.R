library(tidyverse)
library(readxl)

pop2011 <- readRDS("./data/cantonPeoplePop2011.rds")
pop2012 <- readRDS("./data/cantonPeoplePop2012.rds")
pop2013 <- readRDS("./data/cantonPeoplePop2013.rds")
pop2014 <- readRDS("./data/cantonPeoplePop2014.rds")

#import mortality data from 2011-2014 for the cantons

makeRate <- function(theYear) {
  #MALES
  cantonMortM <- read_excel(path = paste0("data/raw/Mortality/MORT.MAS FREC.POR CANTON HOMBRES ", theYear,".xls"), 
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
    mutate(sex = "VARONES")%>% 
    mutate_at(vars(n:rate), funs(as.numeric))
  
  #FEMALES
  
  cantonMortF <- read_excel(path = paste0("data/raw/Mortality/MORT.MAS FREC. POR CANTON MUJERES ", theYear,".xls"), 
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
    mutate(sex = "MUJERES")%>% 
    mutate_at(vars(n:rate), funs(as.numeric))
  
  #create everyone unadjusted rates
  cantonMortT <- left_join(cantonMortF_long, cantonMortM_long, 
                          by = c("PROVINCIA Y CANTON", "cancer"), 
                          suffix = c("_fem", "_male")) %>% 
    mutate_at(vars(c(n_fem:rate_fem, n_male:rate_male)), funs(as.numeric)) %>% 
    mutate(n_male = if_else(is.na(n_male) == TRUE, 0, n_male)) %>% 
    #group by and summarize by canton and cancer
    group_by(`PROVINCIA Y CANTON`, cancer) %>% 
    summarize(n = sum(n_fem, n_male)) %>% 
    mutate(sex = "TODOS")
  
  cantonPop <- eval(as.name(paste0("pop", theYear)))
  
  #calculate the un-adjusted inc rate
  cantonMortT2 <- left_join(cantonMortT, cantonPop, by = 'PROVINCIA Y CANTON') %>% 
    mutate(rate = (n/pop) * 100000)
  
  
  #JOIN MALES AND FEMALES TO ONE TABLE
  cantonMort <- bind_rows(cantonMortF_long, cantonMortM_long, cantonMortT2[,c(1:4,6)]) %>% 
    mutate(rate = round(as.numeric(rate), 2)) %>% 
    mutate(year = theYear) %>% 
  
    #adjust cancer names
    mutate(cancer = case_when(cancer == "Y PULMON" ~ "PULMON",
                              TRUE ~ cancer))
}

cantonMort <- map_dfr(c(2011, 2012, 2013, 2014), makeRate)

saveRDS(cantonMort, file = "./data/cantonMortality.rds")
