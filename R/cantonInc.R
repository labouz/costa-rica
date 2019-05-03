library(tidyverse)
library(readxl)


getCantonRate <- function(theYear) {
  #get canton MALE populations
  cantonPopM <- read_xlsx(path = paste0("./data/raw/INCIDENCIA_",theYear, "_DIFERENTES_CARACTERISTICAS.xlsx"), 
                          sheet = "10 MAS FREC. CANTON VARONES", range = "AE12:AG105",
                          col_names = FALSE)
  cantonPopM <- cantonPopM[,-2]
  colnames(cantonPopM) <- c("PROVINCIA Y CANTON", "pop") 
  
  cantonPopM <- cantonPopM %>% 
    filter(`PROVINCIA Y CANTON` %in% c("SAN JOSE", "ALAJUELA", "CARTAGO", "HEREDIA",
           "GUANACASTE", "PUNTARENAS", "LIMON"))
  #FEMALES POP
  cantonPopF <- read_xlsx(path = "./data/raw/INCIDENCIA_2014_DIFERENTES_CARACTERISTICAS.xlsx", 
                          sheet = "10 MAS FREC. CANTON MUJERES", range = "AF12:AH105",
                          col_names = FALSE)
  cantonPopF <- cantonPopF[,-2]
  colnames(cantonPopF) <- c("PROVINCIA Y CANTON", "pop") 
  cantonPopF <- cantonPopF %>% 
    filter(`PROVINCIA Y CANTON` %in% c("SAN JOSE", "ALAJUELA", "CARTAGO", "HEREDIA",
                                       "GUANACASTE", "PUNTARENAS", "LIMON"))
  #everyone populations
  cantonPop <- left_join(cantonPopF, cantonPopM, by = 'PROVINCIA Y CANTON') %>% 
    group_by(`PROVINCIA Y CANTON`) %>% 
    summarise(pop = sum(pop.x, pop.y))
  
  saveRDS(cantonPop, paste0("./data/cantonPeoplePop",theYear,".rds"))
  
  #####IMPORT CANCER STUFF
  
  #import incidence rates and numbers from 2014 for the cantons
  
  #MALES
  cantonIncM <- read_xlsx(path = paste0("./data/raw/INCIDENCIA_",theYear, "_DIFERENTES_CARACTERISTICAS.xlsx"), 
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
    mutate(sex = "VARONES")%>% 
    mutate_at(vars(n:rate), funs(as.numeric))
  
  #FEMALES
  cantonIncF <- read_xlsx(path = paste0("./data/raw/INCIDENCIA_",theYear,"_DIFERENTES_CARACTERISTICAS.xlsx"), 
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
    mutate(sex = "MUJERES") %>% 
    mutate_at(vars(n:rate), funs(as.numeric))
  
  
  #create everyone unadjusted rates
  cantonIncT <- left_join(cantonIncF_long, cantonIncM_long, 
                          by = c("PROVINCIA Y CANTON", "cancer"), 
                          suffix = c("_fem", "_male")) %>% 
    mutate_at(vars(c(n_fem:rate_fem, n_male:rate_male)), funs(as.numeric)) %>% 
    mutate(n_male = if_else(is.na(n_male) == TRUE, 0, n_male)) %>% 
    #group by and summarize by canton and cancer
    group_by(`PROVINCIA Y CANTON`, cancer) %>% 
    summarize(n = sum(n_fem, n_male)) %>% 
    mutate(sex = "TODOS")
  
  
  #calculate the un-adjusted inc rate
  cantonIncT2 <- left_join(cantonIncT, cantonPop, by = 'PROVINCIA Y CANTON') %>% 
    mutate(rate = (n/pop) * 100000)
  
  
  #JOIN MALES AND FEMALES AND EVERYONE TO ONE TABLE
  cantonInc <- bind_rows(cantonIncF_long, cantonIncM_long, cantonIncT2[,c(1:4,6)]) %>% 
    mutate(rate = round(as.numeric(rate), 2)) %>% 
    mutate(year = theYear)
}

cantonInc <- map_dfr(c(2010, 2011, 2012, 2013, 2014), getCantonRate)

saveRDS(cantonInc, file = "./data/cantonIncidence.rds")
