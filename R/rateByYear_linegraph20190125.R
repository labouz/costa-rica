library(tidyverse)
library(readxl)

#top 5 cancers per sex per year


importRates <- function(year) {
  female <- read_xlsx(paste0("./data/INCIDENCIA_", year, "_DIFERENTES_CARACTERISTICAS.xlsx"), 
                        sheet = "10 MAS FREC. GR. EDAD MUJERES", range = "B8:D13", 
                        col_names = TRUE) %>% 
    mutate(sex = "mujeres",
           year = year)
  names(female)[1:3] <- c("Cancer","n", "rate")
  
  male <- read_xlsx(paste0("./data/INCIDENCIA_", year, "_DIFERENTES_CARACTERISTICAS.xlsx"), 
                          sheet = "10 MAS FREC. GR. EDAD VARONES", range = "B8:D13", 
                          col_names = TRUE) %>% 
    mutate(sex = "varones",
           year = year) %>% 
    mutate(Cancer = if_else(TOTAL == "SISTEMAS HEMATOPOYETICO Y RETICULOENDOTELIAL",
                          "HEMATOPOYETICO", TOTAL))
  
  names(male)[1:3] <- c("Cancer","n", "rate")
  
  bind_rows(female, male)
}

years <- 2009:2014

topRates09_14 <- map_dfr(years, importRates)

saveRDS(topRates09_14, "./data/topRates09_14.rds")

#females
ggplot(data = topRates09_14[which(topRates09_14$sex=="mujeres"),]) +
  geom_line(aes(x = year, y = rate, color = Cancer),
            size = 1.5) +
  labs(x = "Ano", y = "Tasa por 100 000 mujeres") +
  theme_minimal()

#males

ggplot(data = topRates09_14[which(topRates09_14$sex=="varones"),]) +
  geom_line(aes(x = year, y = rate, color = Cancer),
            size = 1.5) +
  labs(x = "Ano", y = "Tasa por 100 000 varones") +
  theme_minimal()
