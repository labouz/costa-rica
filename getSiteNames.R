#getting cancer choices for incidence and mortality trends line graphs


#country 1 yr incidence rates
CR_1yrRates <- readRDS("./data/CR_incRates.rds")

#all sites
siteNames <- unique(CR_1yrRates$site)

#making the character vector a named vector
names(siteNames) <- unique(CR_1yrRates$site)

#filter out sex-specific sites
sexSpecific <- c("Glandula Prostatica",
                 "Testiculos",
                 "Pene",
                 "Mama Masculina",
                 "Mama",
                 "Vulva",
                 "Vagina",
                 "Cuello Uterino" ,                                                                 
                 "Cuerpo Uterino" ,                                                                 
                 "Ovario")

siteNames <- siteNames[!siteNames %in% sexSpecific]


#mortality sitenames
CR_1yrMortRates <- readRDS("./data/CR_mortRates.rds")

#all sites
mortNames <- unique(CR_1yrMortRates$site)

#making the character vector a named vector
names(mortNames) <- unique(CR_1yrMortRates$site)

#filter out sex-specific sites
mortSexSpecific <- c("Prostata",
                 "Testiculo",
                 "Pene",
                 "Mama",
                 "Vulva",
                 "Cuello Del Utero" ,                                                                 
                 "Cuerpo Del Utero" ,                                                                 
                 "Ovario")

mortNames <- mortNames[!mortNames %in% mortSexSpecific]

rm(sexSpecific, mortSexSpecific)