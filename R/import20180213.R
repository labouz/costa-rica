library(readxl)
library(tidyverse)

# FUNCTION TO READ IN THE CANCERS
source("./read_cancer.r")

# WHO STANDARD POPULATION
whoStandPop <- read_xlsx("./Data/whoStandardPop.xlsx", range = "Sheet1!A1:C23")

# COUNTS FOR THE COUNTRY BY SEX YEAR 2009
cancerMales2009  <-  read_cancer(2009, VARONES)
atRiskMales2009  <-  read_cancer(2009, VARONES, AQ8, BG8)
cancerFemales2009  <-  read_cancer(2009, MUJERES)
atRiskFemales2009  <-  read_cancer(2009, MUJERES, AQ8, BG8)

# COUNTS FOR THE COUNTRY BY SEX YEAR 2010
cancerMales2010  <-  read_cancer(2010, VARONES)
atRiskMales2010  <-  read_cancer(2010, VARONES, AQ8, BG8)
cancerFemales2010  <-  read_cancer(2010, MUJERES)
atRiskFemales2010  <-  read_cancer(2010, MUJERES, AQ8, BG8)

# COUNTS FOR THE COUNTRY BY SEX YEAR 2011
cancerMales2011  <-  read_cancer(2011, VARONES)
atRiskMales2011  <-  read_cancer(2011, VARONES, AQ8, BG8)
cancerFemales2011  <-  read_cancer(2011, MUJERES)
atRiskFemales2011  <-  read_cancer(2011, MUJERES, AQ8, BG8)

# COUNTS FOR THE COUNTRY BY SEX YEAR 2012
cancerMales2012  <-  read_cancer(2012, VARONES)
atRiskMales2012  <-  read_cancer(2012, VARONES, AQ8, BG8)
cancerFemales2012  <-  read_cancer(2012, MUJERES)
atRiskFemales2012  <-  read_cancer(2012, MUJERES, AQ8, BG8)

# COUNTS FOR THE COUNTRY BY SEX YEAR 2013
cancerMales2013  <-  read_cancer(2013, VARONES, A8, AJ78)
atRiskMales2013  <-  read_cancer(2013, VARONES, AQ8, BG8)
cancerFemales2013  <-  read_cancer(2013, MUJERES, A8, AJ78)
atRiskFemales2013  <-  read_cancer(2013, MUJERES, AQ8, BG8)

# COUNTS FOR THE COUNTRY BY SEX YEAR 2014
cancerMales2014  <-  read_cancer(2014, VARONES, A8, AJ78)
atRiskMales2014  <-  read_cancer(2014, VARONES, AQ8, BG8)
cancerFemales2014  <-  read_cancer(2014, MUJERES, A8, AJ78)
atRiskFemales2014  <-  read_cancer(2014, MUJERES, AQ8, BG8)

# JOINING COUNTS FOR THE COUNTRY BY SEX YEARS 2009 - 2014
cancerMales2009_2014 <- rbind(cancerMales2009, cancerMales2010, cancerMales2011, 
                              cancerMales2012, cancerMales2013, cancerMales2014) %>% 
  rename(total = X__3, g0_4 = X__5,  g5_9 = X__7, g10_14 = X__9, g15_19 = X__11, g20_24 = X__13,
         g25_29 = X__15, g30_34 = X__17, g35_39 = X__19, g40_44 = X__21, g45_49 = X__23, 
         g50_54 = X__25, g55_59 = X__27, g60_64 = X__29, g65_69 = X__31, g70_74 = X__33,
         g75 = X__35)

cancerMales2009_2014 <- cancerMales2009_2014 %>% 
  mutate(X__2 = ifelse(X__2 == "MAMA MASCULINA", "MAMA", X__2)) %>% 
  mutate(X__2 = ifelse(X__2 %in% c("COLON", "RECTO", "UNION RECTOSIGMOIDEA"), "COLON-RECTO", X__2)) %>% #collapse colon and rectum to be colorectal
  mutate(X__2 = ifelse(X__2 %in% c("LABIO", "BASE LENGUA", "OTRAS PARTES Y LAS NO ESPECIF. LENGUA",
                                   "ENCIA",
                                   "PISO DE LA BOCA",
                                   "PALADAR",
                                   "OTRAS PARTES Y LAS NO ESPECIF. BOCA",
                                   "GLANDULA PAROTIDA",
                                   "OTRAS GLANDULAS SALIVALES MAYORES Y LAS NO ESPECIF.",
                                   "AMIGDALA",
                                   "OROFARINGE",
                                   "NASOFARINGE",
                                   "SENO PIRIFORME",
                                   "HIPOFARINGE",
                                   "OTROS SITIOS Y LOS MAL DEF. DEL LABIO, CAVIDAD BUCAL Y FARINGE"), "OROPHARYNX", X__2)) %>% 
  mutate(X__2 = ifelse(X__2 %in% c("VESICULA BILIAR","OTRAS PARTES Y LAS NO ESPECIF. DE LAS VIAS BILIARES"), "VESICULA BILIAR", X__2)) %>%
  mutate(X__2 = ifelse(X__2 %in% c("CAVIDAD NASAL Y OIDO MEDIO", "SENOS PARANASALES"), "CAVIDAD NASAL Y SENOS", X__2)) %>%
  mutate(X__2 = ifelse(X__2 %in% c("TRAQUEA", "BRONQUIOS Y PULMON"), "PULMON", X__2)) %>%
  mutate(X__2 = ifelse(X__2 %in% c("MENINGES", "ENCEFALO"), "CEREBRO", X__2)) %>%
  filter(X__2 != "OTROS SITIOS Y LOS MAL DEF. DEL SIST. RESP. Y LOS ORG. INTRAT.") %>% 
  group_by(X__2, year) %>% 
  summarise(g0_4 = sum(g0_4), g5_9 = sum(g5_9), g10_14 = sum(g10_14), g15_19 = sum(g15_19), 
            g20_24 = sum(g20_24), g25_29 = sum(g25_29),
            g30_34 = sum(g30_34), g35_39 = sum(g35_39),
            g40_44  = sum(g40_44),         
            g45_49  = sum(g45_49),         
            g50_54  = sum(g50_54),         
            g55_59  = sum(g55_59),         
            g60_64  = sum(g60_64),         
            g65_69  = sum(g65_69),         
            g70_74  = sum(g70_74),         
            g75     = sum(g75   )       
)


atRiskMales2009_2014 <- rbind(atRiskMales2009, atRiskMales2010, atRiskMales2011, 
                              atRiskMales2012, atRiskMales2013, atRiskMales2014) %>% 
  rename(total = X__1, g0_4 = X__2,  g5_9 = X__3, g10_14 = X__4, g15_19 = X__5, g20_24 = X__6,
         g25_29 = X__7, g30_34 = X__8, g35_39 = X__9, g40_44 = X__10, g45_49 = X__11, 
         g50_54 = X__12, g55_59 = X__13, g60_64 = X__14, g65_69 = X__15, g70_74 = X__16,
         g75 = X__17)


cancerFemales2009_2014 <- rbind(cancerFemales2009, cancerFemales2010, cancerFemales2011, 
                                cancerFemales2012, cancerFemales2013, cancerFemales2014) %>%
  rename(total = X__3, g0_4 = X__5,  g5_9 = X__7, g10_14 = X__9, g15_19 = X__11, g20_24 = X__13,
         g25_29 = X__15, g30_34 = X__17, g35_39 = X__19, g40_44 = X__21, g45_49 = X__23, 
         g50_54 = X__25, g55_59 = X__27, g60_64 = X__29, g65_69 = X__31, g70_74 = X__33,
         g75 = X__35) %>% 
  mutate(X__2 = ifelse(X__2 %in% c("COLON", "RECTO", "UNION RECTOSIGMOIDEA"), "COLON-RECTO", X__2)) %>%
  mutate(X__2 = ifelse(X__2 %in% c("LABIO", "BASE LENGUA", "OTRAS PARTES Y LAS NO ESPECIF. LENGUA",
                                   "ENCIA",
                                   "PISO DE LA BOCA",
                                   "PALADAR",
                                   "OTRAS PARTES Y LAS NO ESPECIF. BOCA",
                                   "GLANDULA PAROTIDA",
                                   "OTRAS GLANDULAS SALIVALES MAYORES Y LAS NO ESPECIF.",
                                   "AMIGDALA",
                                   "OROFARINGE",
                                   "NASOFARINGE",
                                   "SENO PIRIFORME",
                                   "HIPOFARINGE",
                                   "OTROS SITIOS Y LOS MAL DEF. DEL LABIO, CAVIDAD BUCAL Y FARINGE"), "OROPHARYNX", X__2)) %>% 
  mutate(X__2 = ifelse(X__2 %in% c("VESICULA BILIAR","OTRAS PARTES Y LAS NO ESPECIF. DE LAS VIAS BILIARES"), "VESICULA BILIAR", X__2)) %>%
  mutate(X__2 = ifelse(X__2 %in% c("CAVIDAD NASAL Y OIDO MEDIO", "SENOS PARANASALES"), "CAVIDAD NASAL Y SENOS", X__2)) %>%
  mutate(X__2 = ifelse(X__2 %in% c("TRAQUEA", "BRONQUIOS Y PULMON"), "PULMON", X__2)) %>%
  mutate(X__2 = ifelse(X__2 %in% c("MENINGES", "ENCEFALO"), "CEREBRO", X__2)) %>%
  filter(X__2 != "OTROS SITIOS Y LOS MAL DEF. DEL SIST. RESP. Y LOS ORG. INTRAT.") %>% 
  group_by(X__2, year) %>% 
  summarise(g0_4 = sum(g0_4), g5_9 = sum(g5_9), g10_14 = sum(g10_14), g15_19 = sum(g15_19), 
            g20_24 = sum(g20_24), g25_29 = sum(g25_29),
            g30_34 = sum(g30_34), g35_39 = sum(g35_39),
            g40_44  = sum(g40_44),         
            g45_49  = sum(g45_49),         
            g50_54  = sum(g50_54),         
            g55_59  = sum(g55_59),         
            g60_64  = sum(g60_64),         
            g65_69  = sum(g65_69),         
            g70_74  = sum(g70_74),         
            g75     = sum(g75   )       
  )



atRiskFemales2009_2014 <- rbind(atRiskFemales2009, atRiskFemales2010, atRiskFemales2011, 
                                atRiskFemales2012, atRiskFemales2013, atRiskFemales2014) %>% 
  rename(total = X__1, g0_4 = X__2,  g5_9 = X__3, g10_14 = X__4, g15_19 = X__5, g20_24 = X__6,
         g25_29 = X__7, g30_34 = X__8, g35_39 = X__9, g40_44 = X__10, g45_49 = X__11, 
         g50_54 = X__12, g55_59 = X__13, g60_64 = X__14, g65_69 = X__15, g70_74 = X__16,
         g75 = X__17)

# RENAMING THE AGE GROUPS 
# 0 - 4, 5 - 9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39,  
# 40-44, 45-49, 50-54, 55-59, 60-64, 65-69, 70-64, 65-70, 75+
# atRiskMales2009_2014 <- atRiskMales2009_2014 %>%
#   rename(total = X__1, g0_4 = X__2,  g5_9 = X__3, g10_14 = X__4, g15_19 = X__5, g20_24 = X__6,
#          g25_29 = X__7, g30_34 = X__8, g35_39 = X__9, g40_44 = X__10, g45_49 = X__11, 
#          g50_54 = X__12, g55_59 = X__13, g60_64 = X__14, g65_69 = X__15, g70_74 = X__16,
#          g75 = X__17)
# cancerMales2009_2014 <- cancerMales2009_2014  %>%
#   rename(total = X__3, g0_4 = X__5,  g5_9 = X__7, g10_14 = X__9, g15_19 = X__11, g20_24 = X__13,
#          g25_29 = X__15, g30_34 = X__17, g35_39 = X__19, g40_44 = X__21, g45_49 = X__23, 
#          g50_54 = X__25, g55_59 = X__27, g60_64 = X__29, g65_69 = X__31, g70_74 = X__33,
#          g75 = X__35)
# atRiskFemales2009_2014 <- atRiskFemales2009_2014 %>%
#   rename(total = X__1, g0_4 = X__2,  g5_9 = X__3, g10_14 = X__4, g15_19 = X__5, g20_24 = X__6,
#          g25_29 = X__7, g30_34 = X__8, g35_39 = X__9, g40_44 = X__10, g45_49 = X__11, 
#          g50_54 = X__12, g55_59 = X__13, g60_64 = X__14, g65_69 = X__15, g70_74 = X__16,
#          g75 = X__17)
# cancerFemales2009_2014 <- cancerFemales2009_2014  %>%
#   rename(total = X__3, g0_4 = X__5,  g5_9 = X__7, g10_14 = X__9, g15_19 = X__11, g20_24 = X__13,
#          g25_29 = X__15, g30_34 = X__17, g35_39 = X__19, g40_44 = X__21, g45_49 = X__23, 
#          g50_54 = X__25, g55_59 = X__27, g60_64 = X__29, g65_69 = X__31, g70_74 = X__33,
#          g75 = X__35)

save.image(file = "./myWorkspace.RData")

