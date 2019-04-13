library(shiny)
library(leaflet) 
library(rgdal) 
library(leaflet.extras)
library(shinydashboard)
library(viridis)
library(shinythemes)
library(DT)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)

regions <- rgdal::readOGR("maps/CRI_adm","CRI_adm1")

# changing wierd characters
regions@data$NAME_1 <-  str_replace(regions@data$NAME_1, "Limón", "Limon")
regions@data$NAME_1 <-  str_replace(regions@data$NAME_1, "San José", "San Jose")

#country 5 yr incidence rates
countryRate <- readRDS(file = "data/rateDF.Rda")

#country 1 yr incidence rates
CR_1yrRates <- readRDS("./data/CR_incRates.rds")

#country 1 yr mortality rates
CR_1yrMortRates <- readRDS("./data/CR_mortRates.rds")

#UNADJUSTED RATES
cantonInc <- readRDS("./Data/cantonIncidence.rds")
cantonMort <- readRDS("./Data/cantonMortality.rds")

#country cases
cases14 <- readRDS("./data/2014CancerCases.rds")

#country mort
mort14 <- readRDS("./data/2014CancerMort.rds")

#country trends
topRates09_14 <- readRDS("./data/topRates09_14.rds")


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
                box(title = "casos de cáncer", 
                    DTOutput("casesTable"), 
                    width = 6),
                box(title = "casos de muertes", 
                    DTOutput("deathsTable"), 
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
                             checkboxGroupInput(inputId = "incCancers",
                                                label = "selecciona un cancer:",
                                                choices = c("Total" = "TOTAL",
                                                            "Piel" = "PIEL",
                                                            "Estomago" = "ESTOMAGO",
                                                            "Ganglios Linfaticos" = "GANGLIOS LINFATICOS",
                                                            "Colon" = "COLON",
                                                            "Hematopoyetico y Reticuloendotelial" = "SISTEMAS HEMATOPOYETICO Y RETICULOENDOTELIAL",
                                                            "Tiroides" = "GLANDULA TIROIDES"),
                                                selected = c("PIEL","COLON","ESTOMAGO",
                                                             "GLANDULA TIROIDES",
                                                             "GANGLIOS LINFATICOS"))
                      ),
                      column(width = 2,
                             conditionalPanel(condition = "input.incSex == 'MUJERES'",
                                              checkboxGroupInput(inputId = "incCancers_fem",
                                                                 label = "cánceres superiores para las mujeres:",
                                                                 choices = c("Mama" = "MAMA",
                                                                             "Ovario" = "OVARIO",
                                                                             "Cuello Uterino" = "CUELLO UTERINO",
                                                                             "Cuerpo Uterino" = "CUERPO UTERINO"))),
                             conditionalPanel(condition = "input.incSex == 'VARONES'",
                                              checkboxGroupInput(inputId = "incCancers_male",
                                                                 label = "cánceres superiores para los hombres:",
                                                                 choices = c("Prostata" = "GLANDULA PROSTATICA",
                                                                             "Testiculos" = "TESTICULOS",
                                                                             "Rinon" = "RIÑON",
                                                                             "Vejiga" = "VEJIGA URINARIA",
                                                                             "Higado" = "HIGADO Y CONDUCTOS BILIARES INTRAHEPATICOS",
                                                                             "Bronquios y Pulmon" = "BRONQUIOS Y PULMON")))
                      )),
                    fluidRow(             
                      plotlyOutput("incTrend"),
                      width = 12
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
                                   checkboxGroupInput(inputId = "mortCancers",
                                                      label = "selecciona un cancer:",
                                                      choices = c("Total" = "TOTAL",
                                                                  "Encefalo" = "ENCEFALO",
                                                                  "Estomago" = "ESTOMAGO",
                                                                  "Pancreas" = "PANCREAS",
                                                                  "Vejiga" = "VEJIGA URINARIA",
                                                                  "Bronquis" = "BRONQUIOS Y PULMON",
                                                                  "Colon" = "COLON",
                                                                  "Leucemia" = "LEUCEMIA LINFOIDE",
                                                                  "Higado" = "HIGADO Y VIAS BILIARES INTRAH."),
                                                      selected = c("PANCREAS","COLON","ESTOMAGO",
                                                                   "ENCEFALO",
                                                                   "VEJIGA URINARIA"))
                            ),
                            column(width = 2,
                                   conditionalPanel(condition = "input.mortSex == 'MUJERES'",
                                                    checkboxGroupInput(inputId = "mortCancers_fem",
                                                                       label = "cánceres superiores para las mujeres:",
                                                                       choices = c("Mama" = "MAMA",
                                                                                   "Ovario" = "OVARIO",
                                                                                   "Cuello Uterino" = "CUELLO UTERINO"))),
                                   conditionalPanel(condition = "input.mortSex == 'VARONES'",
                                                    checkboxGroupInput(inputId = "mortCancers_male",
                                                                       label = "cánceres superiores para los hombres:",
                                                                       choices = c("Prostata" = "GLANDULA PROSTATICA")))
                            )),
                          fluidRow(             
                            plotlyOutput("mortTrend"),
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
                                 choices = c("TODOS" = "TODOS",
                                             "MUJERES" = "MUJERES",
                                             "VARONES" = "VARONES"),
                                 selected = "TODOS"),
                    width = 2),
                
                box(
                  conditionalPanel(
                    condition = "input.aSex == 'MUJERES'",
                    selectInput(inputId = "femCancer",
                                label = "Selecciona un cancer:",
                                choices = c("TOTAL" = "TOTAL",
                                            "PIEL" = "PIEL",
                                            "CUELLO DEL UTERO" = "CUELLO DEL UTERO",
                                            "CUERPO DEL UTERO" = "CUERPO DEL UTERO",
                                            "ESTOMAGO" = "ESTOMAGO",
                                            "GANGLIOS LINF." = "GANGLIOS LINF.",
                                            "COLON" = "COLON",
                                            "MAMA" = "MAMA",
                                            "PULMON" = "PULMON",
                                            "OVARIO" = "OVARIO",
                                            "LOCALIZAC." = "LOCALIZAC.",
                                            "TIROIDES" = "TIROIDES"),
                                selected = "TOTAL")),
                  
                  conditionalPanel(
                    condition = "input.aSex == 'VARONES'",
                    selectInput(inputId = "maleCancer",
                                label = "Selecciona un cancer:",
                                choices = c("TOTAL" = "TOTAL",
                                            "PIEL" = "PIEL",
                                            "PROSTATA" = "PROSTATA",
                                            "ESTOMAGO" = "ESTOMAGO",
                                            "GANGLIOS LINF." = "GANGLIOS LINF.",
                                            "COLON" = "COLON",
                                            "PULMON" = "PULMON",
                                            "VEJIGA" = "VEJIGA",
                                            "Y RETICULOEND." = "Y RETICULOEND.",
                                            "RECTO" = "RECTO",
                                            "LOCALIZAC." = "LOCALIZAC.",
                                            "TIROIDES" = "TIROIDES"),
                                selected = "TOTAL")),
                  
                  conditionalPanel(
                    condition = "input.aSex == 'TODOS'",
                    selectInput(inputId = "peopleCancer",
                                label = "Selecciona un cancer:",
                                choices = c("TOTAL" = "TOTAL",
                                            "PIEL" = "PIEL",
                                            "ESTOMAGO" = "ESTOMAGO",
                                            "GANGLIOS LINF." = "GANGLIOS LINF.",
                                            "COLON" = "COLON",
                                            "PULMON" = "PULMON",
                                            "Y RETICULOEND." = "Y RETICULOEND.",
                                            "RECTO" = "RECTO",
                                            "LOCALIZAC." = "LOCALIZAC.",
                                            "TIROIDES" = "TIROIDES"),
                                selected = "TOTAL")),
                  
                    width = 3),
            
              box(selectInput(inputId = "incYear",
                              label = "Selecciona un ano:",
                              choices = c("2010" = 2010,
                                          "2011" = 2011,
                                          "2012" = 2012,
                                          "2013" = 2013,
                                          "2014" = 2014)),
                
              width = 2)
              ), #end fluidRow
              br(),
              br(),
              fluidRow(
                box(title = "Incidencia de cancer por 100 000 personas",
                    width = 12,
                    leafletOutput(outputId = "incMap", height = 400))
              ),
              br(),
              br(),
              fluidRow(
                box(title = "Tasas del pais",
                    width = 12,
                    DTOutput(outputId = "countryRates"))
              )
            
      ), # end tabItem incidence
      
      tabItem(tabName = "mortality",
              fluidRow(
                box(radioButtons(inputId = "aSex2",
                                 label = "Selecciona un sexo:",
                                 choices = c("MUJERES" = "MUJERES",
                                             "VARONES" = "VARONES"),
                                 selected = "MUJERES"),
                    width = 2),
                
                box(
                  conditionalPanel(
                    condition = "input.aSex2 == 'MUJERES'",
                    selectInput(inputId = "femMort",
                                label = "Selecciona un cancer:",
                                choices = c("TOTAL" = "TOTAL",
                                            "PANCREAS" = "PANCREAS",
                                            "CUELLO DEL UTERO" = "CUELLO DEL UTERO",
                                            "ESTOMAGO" = "ESTOMAGO",
                                            "HIGADO" = "HIGADO",
                                            "COLON" = "COLON",
                                            "MAMA" = "MAMA",
                                            "PULMON" = "PULMON",
                                            "OVARIO" = "OVARIO",
                                            "LOCALIZAC." = "LOCALIZAC.",
                                            "LINFOMAS" = "LINFOMAS",
                                            "LEUCEMIAS" = "LEUCEMIAS"),
                                selected = "TOTAL")),
                  
                  conditionalPanel(
                    condition = "input.aSex2 == 'VARONES'",
                    selectInput(inputId = "maleMort",
                                label = "Selecciona un cancer:",
                                choices = c("TOTAL" = "TOTAL",
                                            "PANCREAS" = "PANCREAS",
                                            "PROSTATA" = "PROSTATA",
                                            "ESTOMAGO" = "ESTOMAGO",
                                            "HIGADO" = "HIGADO",
                                            "COLON" = "COLON",
                                            "ENCEFALO" = "ENCEFALO",
                                            "PULMON" = "PULMON",
                                            "RECTO" = "RECTO",
                                            "LOCALIZAC." = "LOCALIZAC.",
                                            "LINFOMAS" = "LINFOMAS",
                                            "LEUCEMIAS" = "LEUCEMIAS"),
                                selected = "TOTAL")),
                  
                  width = 3)
                
              ),
              br(),
              br(),
              fluidRow(
                box(title = "Mortalidad de cancer por 100 000 personas",
                    width = 12,
                    leafletOutput(outputId = "mortMap", height = 400))
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


server <- function(input, output) {
  
  output$incTrend <- renderPlotly({
    
    theSex <- input$incSex
    cancers <- c(input$incCancers, input$incCancers_fem, input$incCancers_male)
    theData <- filter(CR_1yrRates, sex == theSex, site %in% cancers)
    
    ggplot(data = theData) +
      geom_line(aes(x = as.numeric(year), y = adj.rate, color = site),
                size = 1.5) +
      labs(x = "Ano", y = "Tasa por 100 000") +
      theme_minimal()
  })
  
  output$mortTrend <- renderPlotly({
    
    theSex <- input$mortSex
    cancers <- c(input$mortCancers, input$mortCancers_fem, input$mortCancers_male)
    theData <- filter(CR_1yrMortRates, sex == theSex, site %in% cancers)
    
    ggplot(data = theData) +
      geom_line(aes(x = as.numeric(year), y = adj.rate, color = site),
                size = 1.5) +
      labs(x = "Ano", y = "Tasa por 100 000") +
      theme_minimal()

  })
  
  output$casesTable <- renderDT({
   datatable(
     cases14[1:5,2:3],
     colnames = c("Cancer", "Cases"),
     options = list(sDom  = '<"top">lrt<"bottom">',
                    lengthChange = FALSE, autoHideNavigation = TRUE)
   )
  })
  
  output$deathsTable <- renderDT({
    datatable(
      mort14[1:5,c(1,4)],
      colnames = c("Cancer", "Cases"),
      options = list(sDom  = '<"top">lrt<"bottom">',
                     lengthChange = FALSE, autoHideNavigation = TRUE)
    )
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
  
  output$mortMap <- renderLeaflet({
    library(stringr)
    
    if(input$aSex2 == "MUJERES"){
      theCancer <- input$femMort
    }
    if(input$aSex2 == "VARONES"){
      theCancer <- input$maleMort
    }
    
    colorOrder <- cantonMort %>%
      filter(cancer == theCancer, sex == input$aSex2) %>% 
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
                  fillColor = ~cancerPal(regions2@data$rank),
                  fillOpacity = 1, color = "black", 
                  popup = paste0(regions2@data$NAME_1, "<br><b>Tasa: </b>", regions2@data$rate, 
                                 " por 100k personas","<br><b>Numero: </b>", regions2@data$n)) %>% 
      addLegend("bottomleft", 
                colors  = cancerPal(7:1), 
                labels = theLabels,
                opacity = 1,
                title = "Est. Rates")
    
  })
  
  
  
  
    
  # site <- reactive({
  #   input$map_shape_click
  # }) # end reactive site
  # 
  # observeEvent(input$map_shape_click, {
  #   leafletProxy( mapId = "map" ) %>%
  #     addPolylines(data = regions 
  #                  , layerId = site()[1]
  #                  , color = "black"
  #                  , weight = 2
  #                  , opacity = 1
  #     ) # end addPolylines
  #   
  #   leafletProxy( mapId = "map" ) %>%
  #     addPolylines( data = regions[which(regions$NAME_1 == site()[1]), ]
  #                   , layerId = site()[1]
  #                   , color = "blue"
  #                   , weight = 2
  #                   , opacity = 1
  #     ) # end addPolylines
  #   
  # }) # end observeEvent shape click
  

  # output$regionRates <- renderText({
  #   if (!is.null(site())) { 
  #     paste("Cancer rate for ", site()[1])
  #   } else {
  #     paste("Choose a canton to see cancer details")
  #   }
  # }) # end renderText output$thePlace
  
 
  # output$theRegion <- renderText({paste0(
  #   "Incidencia de Cancer - Tasa por 100 000 personas:  ", site()[1])
  #   })
  # 
  # 
  # output$regionRates_M <- renderDT({
  #   req(site())
  #   datatable(cantonInc[cantonInc$`PROVINCIA Y CANTON` == toupper(site()[1]) &
  #                         cantonInc$sex == "VARONES", c(2,4)],
  #             colnames = c("Tumores", "Tasa"),
  #             options = list(sDom  = '<"top">lrt<"bottom">ip',
  #                            lengthChange = FALSE) # no search bar or dropdown
  #   ) # end datatable
  # })
  # 
  # 
  # output$regionRates_F <- renderDT({
  #   req(site())
  #   datatable(cantonInc[cantonInc$`PROVINCIA Y CANTON` == toupper(site()[1]) &
  #                         cantonInc$sex == "MUJERES", c(2,4)],
  #             colnames = c("Tumores", "Tasa"),
  #             options = list(sDom  = '<"top">lrt<"bottom">ip',
  #                            lengthChange = FALSE) # no search bar or dropdown
  #   ) # end datatable
  # })
  
  output$countryRates <- renderDT({
    x <- as_tibble(countryRate[, c(1, 3, 7, 11)])
    colnames(x) <- c("Cancer", "Mujeres", "Hombres", "Todos")
    
    #cancer name
    #theCancer <- if_else(is.null(input$femCancer) == FALSE, input$femCancer, input$maleCancer)
    if(input$aSex == "MUJERES"){
      theCancer <- input$femCancer
    }
    if(input$aSex == "VARONES"){
      theCancer <- input$maleCancer
    }
    
    x[x$Cancer == theCancer,]
    
  }, options = list(sDom  = '<"top">lrt<"bottom">ip', lengthChange = FALSE)) # end renderDT output$countryRates
  
  
} # end server

shinyApp(ui = ui, server = server)
