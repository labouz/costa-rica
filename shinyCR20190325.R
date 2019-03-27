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

regions <- rgdal::readOGR("maps/CRI_adm","CRI_adm1")

# changing wierd characters
regions@data$NAME_1 <-  str_replace(regions@data$NAME_1, "LimÃ³n", "Limon")
regions@data$NAME_1 <-  str_replace(regions@data$NAME_1, "San JosÃ©", "San Jose")

countryRate <- readRDS(file = "data/rateDF.Rda")


#ADJUSTED RATES
cantonInc <- readRDS("./Data/cantonIncidence.rds")

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
                box(
                  title = "Tendencias en la incidencia: mujeres, 2009-2014",
                  plotOutput("femaleTrend"),
                  width = 6
                ),
                box(
                  title = "Tendencias en la incidencia: varones, 2009-2014",
                  plotOutput("maleTrend"),
                  width = 6
                )
              )
              # fluidRow(
              #   box(title = "Incidencia de Cáncer - Tasa por 100 000 personas", 
              #       DTOutput("countryRates"), 
              #       width = 12)
              #   ) # end fluidRow 
        ), # end tabItems country
      
      tabItem(tabName = "incidence",
              fluidRow(
                box(
                  selectInput(inputId = "aCancer",
                            label = "Selecciona un cancer:",
                            choices = c("PIEL" = "PIEL",
                                        "PROSTATA" = "PROSTATA",
                                        "ESTOMAGO" = "ESTOMAGO",
                                        "GANGLIOS LINF." = "GANGLIOS LINF.",
                                        "COLON" = "COLON",
                                        "PULMON" = "PULMON",
                                        "VEJIGA" = "VEJIGA",
                                        "Y RETICULOEND." = "Y RETICULOEND.",
                                        #"HIGADO" = "HIGADO",
                                        #"TESTICULOS" = "TESTICULOS",
                                        "RECTO" = "RECTO",
                                        "LOCALIZAC." = "LOCALIZAC.",
                                        "TIROIDES" = "TIROIDES"),
                            selected = "PIEL"),
                  width = 3),
                box(radioButtons(inputId = "aSex",
                                   label = "Selecciona un sexo:",
                                   choices = c("MUJERES" = "MUJERES",
                                               "VARONES" = "VARONES"),
                                 selected = "MUJERES"),
                width = 2)
                
              ),
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
              
              # fluidRow(
              #   box(title = "Seleccione una región:",
              #       width = 12,
              #       leafletOutput(outputId = "map", height = 400))
              # ), # end fluidRow 
              # br(),
              # 
              # tags$h4(fluidRow(box(textOutput("theRegion") , width = 12))),
              # br(),
              # 
              # fluidRow(
              #   box(title = "Hombres", DTOutput("regionRates_M"), width = 6) ,
              #   box(title = "Mujeres", DTOutput("regionRates_F"), width = 6)
              # ) # end fluidRow 
      ) # end tabItem regiones
      
      # tabItem(tabName = "mortality" ,
      #         fluidRow(
      #           box(title = "Select a cancer:",
      #               width = 12,
      #               leafletOutput(outputId = "maleMap", height = 400))
      #         ) , # end fluidRow 
      #         br(),
      #         
      #         fluidRow(
      #           column(3, offset = 3,
      #           radioButtons("maleSite", "Pick a site:",
      #                        c("PROSTATA" = "PROSTATA",
      #                          "PIEL" = "PIEL",
      #                          "ESTOMAGO" = "ESTOMAGO",
      #                          "GANGLIOS LINF." = "GANGLIOS LINF.",
      #                          "COLON" = "COLON",
      #                          "PULMON" = "PULMON",
      #                          "VEJIGA" = "VEJIGA",
      #                          "Y RETICULOEND." = "Y RETICULOEND.",
      #                          #"HIGADO" = "HIGADO",
      #                          #"TESTICULOS" = "TESTICULOS",
      #                          "RECTO" = "RECTO",
      #                          "LOCALIZAC." = "LOCALIZAC.",
      #                          "TIROIDES" = "TIROIDES"
      #                        )))
      #           #box("Cancer rates for males", DTOutput("maleRate"), width = 3)
      #           
      #           
      #         ) # end fluidRow 
      #         
      # ) # end tabItem regiones
      
      
      
    ) # end tabItems
  ) # end dashboardBody
) #end UI dashboardPage


server <- function(input, output) {
  
  output$femaleTrend <- renderPlot(
    
    ggplot(data = topRates09_14[which(topRates09_14$sex=="mujeres"),]) +
      geom_line(aes(x = year, y = rate, color = Cancer),
                size = 1.5) +
      labs(x = "Ano", y = "Tasa por 100 000 mujeres") +
      theme_minimal()
  )
  
  output$maleTrend <- renderPlot(
    
    ggplot(data = topRates09_14[which(topRates09_14$sex=="varones"),]) +
      geom_line(aes(x = year, y = rate, color = Cancer),
                size = 1.5) +
      labs(x = "Ano", y = "Tasa por 100 000 varones") +
      theme_minimal()
  )
  
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
  
  # used for regions
  # output$map <- renderLeaflet({
  #   leaflet() %>%
  #     addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
  #     setView(lat = 9.55, lng = -84, zoom = 7) %>% 
  #     addPolygons(data = regions, 
  #                 layerId = regions@data$NAME_1,
  #                 weight = 2, 
  #                 fillColor = ~pal(regions$ID_1),
  #                 fillOpacity = 0.4, color = "black") 
  # }) # end renderLeaflet map
  
  # used for maleRate
  output$incMap <- renderLeaflet({
    library(stringr)
    colorOrder <- cantonInc %>%
      filter(cancer == input$aCancer, sex == input$aSex) %>% 
      #filter(cancer == "RECTO"  & region != "COSTA RICA") %>% 
      group_by(cancer, sex) %>% 
      arrange(cancer, sex, desc(rate)) %>% 
      mutate(rank = 1:length(rate)) %>% 
      # mutate(rank = as.factor(rank(rate)), 
      #        region = str_to_title(`PROVINCIA Y CANTON`)) %>% 
      mutate(region = str_to_title(`PROVINCIA Y CANTON`)) %>% 
      ungroup() %>% 
      select(region, rate, rank) 
    
    #print(colorOrder)
    
    #print(colorOrder)
    
    regions2 <- regions
    regions2@data <- left_join(regions2@data, colorOrder, 
                               by = c("NAME_1" = "region"))
        
    cancerPal <- colorFactor(cividis(7), regions2@data$rank,
                             domain = min(regions2@data$rate):max(regions2@data$rate)
                             )
    #print(cancerPal(1:7))

    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
      setView(lat = 9.55, lng = -84, zoom = 7) %>% 
      addPolygons(data = regions2, 
                  layerId = regions2@data$ID_1,
                  weight = 2, 
                  fillColor = ~cancerPal(regions2@data$rank),
                  fillOpacity = 1, color = "black", 
                  popup = paste0("<b>Tasa: </b>", regions2@data$rate, 
                                 " por 100k personas")) %>% 
      addLegend("bottomright", 
                colors  = cancerPal(regions2@data$rank), 
                labels = levels(cut(regions2@data$rate, breaks = 7, ordered_result = TRUE)),
                #values = 1:7,
                opacity = 1,
                title = "Est. Rates")
    
  }) # end renderLeaflet map
  
  # output$maleRate <- renderDT({
  #   theTable <- cantonInc[cantonInc$cancer == input$maleSite & 
  #                           cantonInc$sex == "VARONES", c(1,4)]
  #   theTable <- theTable[order(-theTable$rate),]
  #   
  #   datatable(theTable,
  #             colnames = c("Canton", "Tasa por 100k Varones"),
  #             options = list(sDom  = '<"top">lrt<"bottom">',
  #                            lengthChange = FALSE, autoHideNavigation = TRUE)  # no search bar or dropdown
  #   ) # end datatable
  # }) # end renderDT output$maleRate
  
  
    
  site <- reactive({
    input$map_shape_click
  }) # end reactive site
  
  observeEvent(input$map_shape_click, {
    leafletProxy( mapId = "map" ) %>%
      addPolylines(data = regions 
                   , layerId = site()[1]
                   , color = "black"
                   , weight = 2
                   , opacity = 1
      ) # end addPolylines
    
    leafletProxy( mapId = "map" ) %>%
      addPolylines( data = regions[which(regions$NAME_1 == site()[1]), ]
                    , layerId = site()[1]
                    , color = "blue"
                    , weight = 2
                    , opacity = 1
      ) # end addPolylines
    
  }) # end observeEvent shape click
  

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
    
    x[x$Cancer == input$aCancer,]
    
  }, options = list(sDom  = '<"top">lrt<"bottom">ip', lengthChange = FALSE)) # end renderDT output$countryRates
  
  
} # end server

shinyApp(ui = ui, server = server)
