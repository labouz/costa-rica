library(shiny)
library(shinyjs)
library(shinyhelper)
library(leaflet) 
library(rgdal) 
library(leaflet.extras)
library(shinydashboard)
library(viridis)
library(shinythemes)
library(DT)
library(dplyr)
library(stringr)
library(lettercase)
library(ggplot2)
library(plotly)

#source code that gets vector of sites for incidence and mort trend plots
source("getSiteNames.R")

regions <- rgdal::readOGR("maps/CRI_adm","CRI_adm1")

# changing wierd characters
regions@data$NAME_1 <-  str_replace(regions@data$NAME_1, "Limón", "Limon")
regions@data$NAME_1 <-  str_replace(regions@data$NAME_1, "San José", "San Jose")

#country 5 yr incidence rates
countryRate <- readRDS(file = "data/rateDF.Rda")

#country 4yr mortality rates
countryMortRate <- readRDS("data/CR_4yrMortRates.rds")

#country 1 yr incidence rates
CR_1yrRates <- readRDS("./data/CR_incRates.rds")

#country 1 yr mortality rates
CR_1yrMortRates <- readRDS("./data/CR_mortRates.rds")

#UNADJUSTED RATES
cantonInc <- readRDS("./data/cantonIncidence.rds")
cantonMort <- readRDS("./data/cantonMortality.rds")

#country cases
cases14 <- readRDS("./data/2014CancerCases.rds")

#country mort cases
mort14 <- readRDS("./data/2014CancerMort.rds")


pal <- colorFactor(viridis_pal()(10), levels = regions$ID_1)


ui <- dashboardPage(skin = "black",
  
  dashboardHeader(title = tags$img(src = "logo.png", height = 50, width = 150)),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Estadísticas del país", tabName = "country"),
      menuItem("Regiones: incidencia", tabName = "incidence", icon = icon("map")),
      menuItem("Regiones: mortalidad", tabName = "mortality", icon = icon("map"))
    ) # end sidebarMenu
  ), # end dahsboardSidebar

  dashboardBody(
    tabItems(
      tabItem(tabName = "country",
              fluidRow(
                box(title = "Casos de Cáncer - 2014",
                    style = "height:500px; overflow-y: scroll;", 
                    DT::dataTableOutput("casesTable"), 
                    width = 6),
                box(title = "Casos de Muertes - 2014",
                    style = "height:500px; overflow-y: scroll;", 
                    DT::dataTableOutput("deathsTable"),
                    width = 6)
              ),  # end fluidRow
              
              fluidRow(
                box(width = 12,
                    title = "Tendencias en la incidencia: 2009-2014",
                    fluidRow(
                      column(width = 3,
                             radioButtons(inputId = "incSex", 
                                          label = "selecciona un sexo:",
                                          choices = c("Todos" = "TODOS",
                                                      "Mujeres" = "MUJERES",
                                                      "Varones" = "VARONES"))
                      ),
                      column(width = 4,
                             style = "height:200px; overflow-y: scroll;",
                             checkboxGroupInput(inputId = "incCancers",
                                                label = "selecciona un cancer:",
                                                choices = names(siteNames),
                                                selected = c("Colon","Estomago",
                                                             "Glandula Tiroides",
                                                             "Bronquios y Pulmon"))
                      ),
                      column(width = 2,
                             conditionalPanel(condition = "input.incSex == 'MUJERES'",
                                              checkboxGroupInput(inputId = "incCancers_fem",
                                                                 label = "cánceres superiores para las mujeres:",
                                                                 choices = c("Mama" = "Mama",
                                                                             "Ovario" = "Ovario",
                                                                             "Cuello Uterino" = "Cuello Uterino",
                                                                             "Cuerpo Uterino" = "Cuerpo Uterino"))),
                             conditionalPanel(condition = "input.incSex == 'VARONES'",
                                              checkboxGroupInput(inputId = "incCancers_male",
                                                                 label = "cánceres superiores para los hombres:",
                                                                 choices = c("Prostata" = "Glandula Prostatica",
                                                                             "Testiculos" = "Testiculos",
                                                                             "Pene" = "Pene",
                                                                             "Mama Masculina" = "Mama Masculina")))
                      )),
                    fluidRow(
                      column(
                      plotlyOutput("incTrend") %>% 
                        helper(size = "l",
                               type = "inline",
                               content = "this is help"),
                      width = 12
                      
                      )
                      
                    ),
                    br(),
                    br(),
                    fluidRow(
                      box(width = 12,
                          title = "Tendencias en la mortalidad: 2012-2015",
                          fluidRow(
                            column(width = 3,
                                   radioButtons(inputId = "mortSex", 
                                                label = "selecciona un sexo:",
                                                choices = c("Todos" = "TODOS",
                                                            "Mujeres" = "MUJERES",
                                                            "Varones" = "VARONES"))
                            ),
                            column(width = 4,
                                   style = "height:200px; overflow-y: scroll;",
                                   checkboxGroupInput(inputId = "mortCancers",
                                                      label = "selecciona un cancer:",
                                                      choices = names(mortNames),
                                                      selected = c("Bronquios y Pulmon","Colon","Estomago",
                                                                   "Higado y Vias Biliares Intrah."))
                            ),
                            column(width = 2,
                                   conditionalPanel(condition = "input.mortSex == 'MUJERES'",
                                                    checkboxGroupInput(inputId = "mortCancers_fem",
                                                                       label = "cánceres superiores para las mujeres:",
                                                                       choices = c("Mama" = "Mama",
                                                                                   "Ovario" = "Ovario",
                                                                                   "Cuello Del Utero" = "Cuello Del Utero",
                                                                                   "Cuerpo Del Utero" = "Cuerpo Del Utero",
                                                                                   "Vuvla" = "Vulva"))),
                                   conditionalPanel(condition = "input.mortSex == 'VARONES'",
                                                    checkboxGroupInput(inputId = "mortCancers_male",
                                                                       label = "cánceres superiores para los hombres:",
                                                                       choices = c("Prostata" = "Prostata",
                                                                                   "Testiculo" = "Testiculo",
                                                                                   "Pene" = "Pene")))
                            )),
                          fluidRow(             
                            plotlyOutput("mortTrend")%>% 
                              helper(size = "l",
                                     type = "inline",
                                     content = "this is help"),
                            width = 12
                          )
                      )#end box
                    ) #end fluidRow
                ) #end box
              ) #end fluidRow
              ), # end tabItems country
      
      tabItem(tabName = "incidence",
              fluidRow(
                box(radioButtons(inputId = "aSex",
                                 label = "Selecciona un sexo:",
                                 choices = c("Todos" = "TODOS",
                                             "Mujeres" = "MUJERES",
                                             "Varones" = "VARONES"),
                                 selected = "TODOS"),
                    width = 2),
                
                box(
                  conditionalPanel(
                    condition = "input.aSex == 'MUJERES'",
                    selectInput(inputId = "femCancer",
                                label = "Selecciona un cancer:",
                                choices = c("Total" = "TOTAL",
                                            "Piel" = "PIEL",
                                            "Cuello del Utero" = "CUELLO DEL UTERO",
                                            "Cuerpo del Utero" = "CUERPO DEL UTERO",
                                            "Estomago" = "ESTOMAGO",
                                            "Ganglios Linf." = "GANGLIOS LINF.",
                                            "Colon" = "COLON",
                                            "Mama" = "MAMA",
                                            "Pulmon" = "PULMON",
                                            "Ovario" = "OVARIO",
                                            "Tiroides" = "TIROIDES"),
                                selected = "TOTAL")),
                  
                  conditionalPanel(
                    condition = "input.aSex == 'VARONES'",
                    selectInput(inputId = "maleCancer",
                                label = "Selecciona un cancer:",
                                choices = c("Total" = "TOTAL",
                                            "Piel" = "PIEL",
                                            "Prostata" = "PROSTATA",
                                            "Estomago" = "ESTOMAGO",
                                            "Ganglios Linf." = "GANGLIOS LINF.",
                                            "Colon" = "COLON",
                                            "Pulmon" = "PULMON",
                                            "Vejiga" = "VEJIGA",
                                            "Y Reticuloend." = "Y RETICULOEND.",
                                            "Recto" = "RECTO",
                                            "Tiroides" = "TIROIDES"),
                                selected = "TOTAL")),
                  
                  conditionalPanel(
                    condition = "input.aSex == 'TODOS'",
                    selectInput(inputId = "peopleCancer",
                                label = "Selecciona un cancer:",
                                choices = c("Total" = "TOTAL",
                                            "Piel" = "PIEL",
                                            "Estomago" = "ESTOMAGO",
                                            "Ganglios Linf." = "GANGLIOS LINF.",
                                            "Colon" = "COLON",
                                            "Pulmon" = "PULMON",
                                            "Y Reticuloend." = "Y RETICULOEND.",
                                            "Recto" = "RECTO",
                                            "Tiroides" = "TIROIDES"),
                                selected = "TOTAL")),
                  
                    width = 3),
            
              box(selectInput(inputId = "incYear",
                              label = "Selecciona un Año:",
                              choices = c("2011" = 2011,
                                          "2012" = 2012,
                                          "2013" = 2013,
                                          "2014" = 2014)),
                
              width = 2)
              ), #end fluidRow
              br(),
              br(),
              fluidRow(
                box(title = HTML(paste(textOutput("incTitle"), 
                                       textOutput("countryInc"), sep = "<br/>")),
                    
                    width = 12,
                    leafletOutput(outputId = "incMap", height = 400)%>% 
                      helper(size = "l",
                             type = "inline",
                             content = "this is help"))
              ),
              br(),
              br()
            
      ), # end tabItem incidence
      
      tabItem(tabName = "mortality",
              fluidRow(
                box(radioButtons(inputId = "aSex2",
                                 label = "Selecciona un sexo:",
                                 choices = c("Todos" = "TODOS",
                                             "Mujeres" = "MUJERES",
                                             "Varones" = "VARONES"),
                                 selected = "TODOS"),
                    width = 2),
                
                box(
                  conditionalPanel(
                    condition = "input.aSex2 == 'MUJERES'",
                    selectInput(inputId = "femMort",
                                label = "Selecciona un cancer:",
                                choices = c("Total" = "TOTAL",
                                            "Pancreas" = "PANCREAS",
                                            "Cuello del Utero" = "CUELLO DEL UTERO",
                                            "Estomago" = "ESTOMAGO",
                                            "Higado" = "HIGADO",
                                            "Colon" = "COLON",
                                            "Mama" = "MAMA",
                                            "Pulmon" = "PULMON",
                                            "Ovario" = "OVARIO",
                                            "Linfomas" = "LINFOMAS",
                                            "Leucemias" = "LEUCEMIAS"),
                                selected = "TOTAL")),
                  
                  conditionalPanel(
                    condition = "input.aSex2 == 'VARONES'",
                    selectInput(inputId = "maleMort",
                                label = "Selecciona un cancer:",
                                choices = c("Total" = "TOTAL",
                                            "Pancreas" = "PANCREAS",
                                            "Prostata" = "PROSTATA",
                                            "Estomago" = "ESTOMAGO",
                                            "Higado" = "HIGADO",
                                            "Colon" = "COLON",
                                            "Encefalo" = "ENCEFALO",
                                            "Pulmon" = "PULMON",
                                            "Recto" = "RECTO",
                                            "Linfomas" = "LINFOMAS",
                                            "Leucemias" = "LEUCEMIAS"),
                                selected = "TOTAL")),
                  
                  conditionalPanel(
                    condition = "input.aSex2 == 'TODOS'",
                    selectInput(inputId = "peopleMort",
                                label = "Selecciona un cancer:",
                                choices = c("Total" = "TOTAL",
                                            "Estomago" = "ESTOMAGO",
                                            "Higado" = "HIGADO",
                                            "Colon" = "COLON",
                                            "Leucemias" = "LEUCEMIAS",
                                            "Pulmon" = "PULMON",
                                            "Linfomas" = "LINFOMAS",
                                            "Mama" = "MAMA",
                                            "Pancreas" = "PANCREAS"),
                                selected = "TOTAL")),
                  
                  width = 3),
                
                box(selectInput(inputId = "mortYear",
                                label = "Selecciona un Año:",
                                choices = c("2011" = 2011,
                                            "2012" = 2012,
                                            "2013" = 2013,
                                            "2014" = 2014)),
                    
                    width = 2)
                ),
              
              br(),
              br(),
              fluidRow(
                box(title = HTML(paste(textOutput("mortTitle"), textOutput("countryMort"), sep = "<br/>")),
                    width = 12,
                    leafletOutput(outputId = "mortMap", height = 400)%>% 
                      helper(size = "l",
                             type = "inline",
                             content = "this is help"))
              ),
              br(),
              br()
              # fluidRow(
              #   box(title = "Tasas del pais",
              #       width = 12,
              #       DTOutput(outputId = "countryRates"))
              # )
              
      )
      
      
    ) # end tabItems
  ) # end dashboardBody
) #end UI dashboardPage


server <- function(input, output, session) {
  
  observe_helpers(withMathJax = TRUE)
  
  output$incTrend <- renderPlotly({
    
    theSex <- input$incSex
    cancers <- c(input$incCancers, input$incCancers_fem, input$incCancers_male)
    theData <- filter(CR_1yrRates, sex == theSex, site %in% cancers) %>% 
      mutate(year = as.numeric(year))


   g <-  ggplot(data = theData) +
      geom_line(aes(x = year, y = adj.rate, color = site, group = site,
                    text = paste0("Año: ", theData$year, 
                                  "<br>Tasa Ajustada: ", theData$adj.rate, 
                                  "<br>Cáncer: ", theData$site)),
                size = 1.5) +
      labs(x = "Año", y = "Tasa por 100 000") +
      theme_minimal()
   
   ggplotly(g, tooltip = "text") %>% config(displayModeBar = F) 
  })
  
  output$mortTrend <- renderPlotly({
    
    theSex <- input$mortSex
    cancers <- c(input$mortCancers, input$mortCancers_fem, input$mortCancers_male)
    theData <- filter(CR_1yrMortRates, sex == theSex, site %in% cancers)
    
    g <- ggplot(data = theData) +
      geom_line(aes(x = as.numeric(year), y = adj.rate, color = site, group = site,
                    text = paste0("Año: ", theData$year, 
                                         "<br>Tasa Ajustada: ", theData$adj.rate, 
                                         "<br>Cáncer: ", theData$site)),
                size = 1.5) +
      labs(x = "Año", y = "Tasa por 100 000") +
      theme_minimal()
    
    ggplotly(g, tooltip = "text") %>% config(displayModeBar = F) 

  })
  
  
  output$casesTable <- renderDT({
   datatable(
     cases14,
     rownames = FALSE,
     colnames = c("Cancer", "Casos"),
     options = list(paging = FALSE)
   )
  })
  
  output$deathsTable <- renderDT({
    datatable(
      mort14[c(2,5)],
      colnames = c("Cancer", "Casos"),
      options = list(paging = FALSE)
    )
  })
  

  output$incTitle <- renderText({
    if(input$aSex == "MUJERES"){
      theCancer <- input$femCancer
    }
    if(input$aSex == "VARONES"){
      theCancer <- input$maleCancer
    }
    if(input$aSex == "TODOS"){
      theCancer <- input$peopleCancer
    }
    
    paste0("Incendencia de cancer por 100 000 personas -- ", "Cancer: ", theCancer, 
           ", Año: ", input$incYear)
  })
  
  output$incMap <- renderLeaflet({
    library(stringr)
    
    if(input$aSex == "MUJERES"){
      theCancer <- input$femCancer
    }
    if(input$aSex == "VARONES"){
      theCancer <- input$maleCancer
    }
    if(input$aSex == "TODOS"){
      theCancer <- input$peopleCancer
    }
    
    colorOrder <- cantonInc %>%
      filter(cancer == theCancer, sex == input$aSex, year == input$incYear) %>% 
      group_by(cancer, sex) %>% 
      arrange(cancer, sex, desc(rate)) %>% 
      mutate(rank = as.factor(rank(rate))) %>% 
      mutate(region = str_to_title(`PROVINCIA Y CANTON`)) %>% 
      ungroup() %>% 
      select(region, rate, rank, n) 
    
    regions2 <- regions
    regions2@data <- left_join(regions2@data, colorOrder, 
                               by = c("NAME_1" = "region"))
    
    cancerPal <- colorFactor(cividis(7), colorOrder$rank)
    
    theLabels = as.character(round(colorOrder$rate[order(-colorOrder$rate)]))
    theLabels[2:3] <- ""
    theLabels[5:6] <- ""
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
      setView(lat = 9.55, lng = -84, zoom = 7) %>% 
      addPolygons(data = regions2, 
                  layerId = regions2@data$ID_1,
                  weight = 2, 
                  fillColor = ~cancerPal( regions2@data$rank),
                  fillOpacity = 1, color = "black", 
                  popup = paste0(regions2@data$NAME_1, "<br><b>Tasa: </b>", regions2@data$rate, 
                                 " por 100k personas", "<br><b>Numero: </b>", regions2@data$n)) %>% 
      addLegend("bottomleft", 
                colors  = cancerPal(7:1), 
                labels = theLabels,
                opacity = 1,
                title = "Est. Rates") 
    
  }) # end renderLeaflet map
  
  output$mortTitle <- renderText({
    if(input$aSex2 == "MUJERES"){
      theCancer <- input$femMort
    }
    if(input$aSex2 == "VARONES"){
      theCancer <- input$maleMort
    }
    if(input$aSex2 == "TODOS"){
      theCancer <- input$peopleMort
    }
    
    paste0("Mortalidad de cancer por 100 000 personas -- ", "Cancer: ", theCancer, 
           ", Año: ", input$mortYear)
  })
  
  output$mortMap <- renderLeaflet({
    library(stringr)
    
    if(input$aSex2 == "MUJERES"){
      theCancer <- input$femMort
    }
    if(input$aSex2 == "VARONES"){
      theCancer <- input$maleMort
    }
    if(input$aSex == "TODOS"){
      theCancer <- input$peopleMort
    }
    
    colorOrder <- cantonMort %>%
      filter(cancer == theCancer, sex == input$aSex2, year == input$mortYear) %>% 
      group_by(cancer, sex) %>% 
      arrange(cancer, sex, desc(rate)) %>% 
      mutate(rank = as.factor(rank(rate))) %>% 
      mutate(region = str_to_title(`PROVINCIA Y CANTON`)) %>% 
      ungroup() %>% 
      select(region, rate, rank, n) 
    
    regions2 <- regions
    regions2@data <- left_join(regions2@data, colorOrder, 
                               by = c("NAME_1" = "region"))
    
    cancerPal <- colorFactor(cividis(7), colorOrder$rank)
    
    theLabels = as.character(round(colorOrder$rate[order(-colorOrder$rate)]))
    theLabels[2:3] <- ""
    theLabels[5:6] <- ""
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
      setView(lat = 9.55, lng = -84, zoom = 7) %>% 
      addPolygons(data = regions2, 
                  layerId = regions2@data$ID_1,
                  weight = 2, 
                  fillColor = ~cancerPal( regions2@data$rank),
                  fillOpacity = 1, color = "black", 
                  popup = paste0(regions2@data$NAME_1, "<br><b>Tasa: </b>", regions2@data$rate, 
                                 " por 100k personas", "<br><b>Numero: </b>", regions2@data$n)) %>% 
      addLegend("bottomleft", 
                colors  = cancerPal(7:1), 
                labels = theLabels,
                opacity = 1,
                title = "Est. Rates") 
    
  })
  
  
  output$countryInc <- renderText({
    x <- as_tibble(countryRate[, c(1, 3, 7, 11)])
    colnames(x) <- c("Cancer", "Mujeres", "Hombres", "Todos")
   
    if(input$aSex == "MUJERES"){
      theCancer <- input$femCancer
      x <- select(x, c(Cancer,Mujeres))
    }
    if(input$aSex == "VARONES"){
      theCancer <- input$maleCancer
      x <- select(x, c(Cancer,Hombres))
    }
    if(input$aSex == "TODOS"){
      theCancer <- input$peopleCancer
      x <- select(x, c(Cancer,Todos))
    }
    
    paste0("Tasa Ajustada de 5 Años de Pais: ", as.character(x[x$Cancer == theCancer,2]), " por 100 000 Personas")
    
  })
  
  
  output$countryMort <- renderText({
    #browser()
    x <- countryMortRate
    
    if(input$aSex == "MUJERES"){
      theCancer <- str_ucfirst(str_decapitalize(input$femMort))
      x <- filter(x, site == theCancer, sex == "MUJERES")
    }
    if(input$aSex == "VARONES"){
      theCancer <- str_ucfirst(str_decapitalize(input$maleMort))
      x <- filter(x, site == theCancer, sex == "VARONES")
    }
    if(input$aSex == "TODOS"){
      theCancer <- str_ucfirst(str_decapitalize(input$peopleMort))
      x <- filter(x, site == theCancer, sex == "TODOS")
    }
    
    paste0("Tasa Ajustada de 4 Años de Pais: ", as.character(x$adj.rate), " por 100 000 Personas")
    
  })
} # end server

shinyApp(ui = ui, server = server)
