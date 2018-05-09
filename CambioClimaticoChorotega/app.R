#Paquetes necesarios
library(shinydashboard)
library(shinyWidgets)
library(magrittr)
library(dplyr)
library(ggplot2)
#library(ggiraph)
library(data.table)
library(sf)
library(leaflet)

#Archivos y datos de entrada
dist_cant <- fread("distritos_cantones.csv")
val_distritos <- data.table(readRDS("val_distritos.rds"))
val_distritos_mes <- data.table(readRDS("val_distritos_mes.rds"))
distritos <- st_read("distritos_mapa.shp")
distritos$coddistful <- as.character(distritos$coddistful)

GCMs <- list("ccsm4_r1i1p1", 
     "ccsm4_r2i1p1", 
     "cesm1_cam5_r1i1p1",
     "cesm1_cam5_r2i1p1",
     "cmcc_cms_r1i1p1",
     "ec_earth_r2i1p1",
     "giss_e2_r_r1i1p1",
     "miroc5_r1i1p1",
     "miroc5_r3i1p1",
     "mpi_esm_lr_r1i1p1",
     "mpi_esm_lr_r2i1p1",
     "mpi_esm_lr_r3i1p1")

# UI de dashboard
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Cambio climático en la Región Chorotega",
    titleWidth = 450
  ),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    fluidRow(
      box(title = "Controles", 
          solidHeader = TRUE, 
          status = "primary",
          width = 5,
          selectInput("canton", label = "Seleccionar cantón:", 
                      choices = sort(dist_cant$canton),
                      selected = sort(dist_cant$canton)[[1]]
                      ),
          sidebarMenuOutput("distr"),
          radioButtons("cp", "Seleccionar escenario de emisiones:",
                       choices = list("RCP 4.5" = "rpc45",
                                      "RCP 8.5" = "rpc85")
                       ),
          pickerInput("modelos", "Seleccionar modelos de circulación global (GCMs):",
                      choices = GCMs,
                      selected = GCMs,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Ninguno",
                        `select-all-text` = "Todos",
                        `none-selected-text` = "No hay selección",
                        `selected-text-format` = "count",
                        `count-selected-text` = "{0} modelos seleccionados"),
                      multiple = TRUE),
          sliderInput("aNo", "Seleccionar periodo de años:",
                      min = 1980, max = 2100, value = c(2030, 2060), step = 5, sep = "")
          ),
      tabBox(
        side = "right",
        title = "Temperatura", id = "temperatura", width = 7, selected = "Anual", 
        tabPanel("Mensual", plotOutput("Tmes")
                 ),
        tabPanel("Anual", plotOutput("TaNo")
        )
        )
      ),
    fluidRow(
      tabBox(
        id = "mapa", width = 5, 
        tabPanel("Temperatura", leafletOutput("mapaTemp"),
                 ("Mapa del promedio de temperatura por distrito para el periódo de tiempo seleccionado.")
                 ),
        tabPanel("Lluvia", leafletOutput("mapaPrecip"),
                 ("Mapa del promedio de lluvia anual por distrito para el periódo de tiempo seleccionado.")
                 )
        ),
      tabBox(
        side = "right",
        title = "Lluvia", id = "precip", width = 7, selected = "Anual",
      tabPanel("Mensual", plotOutput("Pmes")
               ),
      tabPanel("Anual", plotOutput("PaNo")
      )
      )
    ),
    fluidRow(
      box( width = 12,
           p(tags$strong("Notas sobre la herramienta:")),
           ("- Los GCMs utilizados se seleccionaron con base en el estudio"),tags$a(href="http://onlinelibrary.wiley.com/doi/10.1002/joc.4216/abstract", tags$i("Skill of CMIP5 climate models in reproducing 20th century basic climate features in Central America")), 
           ("de Hidalgo y Alfaro (2015)."),
           tags$br(),
           ("- Los calculos de los valores de cada distrito se hicieron con base en GCMs reducidos en escala a 5km x 5km. La reducción de escala también fue realizada por Hidalgo y Alfaro."),
           tags$br(),
           ("- "), tags$a(href="http://www.oscc.gob.es/es/general/salud_cambio_climatico/Nuevos_escenarios_emision_RCPs.htm", "En este enlace"),(" se encuentra información en español sobre los escenarios"), tags$i("Representative Concentration Pathways"),("(RCP) del Quinto Informe del IPCC."),
           tags$br(),
           ("- El código de la herramienta se encuentra disponible en"), tags$a(href="https://github.com/GuiAlDuS", "GitHub"), ("."),
           p(""),
           ("Esta herramienta fue desarrollada en el"),  tags$a(href="http://www.cigefi.ucr.ac.cr/", "Centro de Investigaciones Geofísicas de la Universidad de Costa Rica (CIGEFI)"), ("por Guillermo Durán, Erick Alfaro y Hugo Hidalgo."),
           tags$br(),
           ("Última actualización 7-5-2018.")
      )
    )
  )
)


# Servidor
server <- function(input, output) {
  #funcion para menús de distritos
  output$distr <- renderMenu({
    mydata <- dist_cant[canton == input$canton]
    selectInput("distritos", label = "Seleccionar distrito:",
                choices = sort(mydata$distrito)
                )
  })
  
  #funciones de selección
  seleccion <- reactive({
    val_distritos[distrito == input$distritos &
                    canton == input$canton &
                    Scenario == input$cp & 
                    Model %in% input$modelos &
                    Year >= input$aNo[1] & 
                    Year <= input$aNo[2]]
  })
  
  sel_distrito <- reactive({seleccion()[1, .(distrito, canton, coddistful)]
  })
  
  sel_mes <- reactive({
    val_distritos_mes[distrito == input$distritos &
                        canton == input$canton &
                        Model %in% input$modelos &
                        Scenario == input$cp & 
                        Year >= input$aNo[1] & Year <= input$aNo[2]]
  })

  table_mapa <- reactive ({
    val_distritos[
    Model %in% input$modelos &
      Scenario == input$cp & 
      Year >= input$aNo[1] & Year <= input$aNo[2]
    , .(t_mean_y = mean(t_mean_y), p_mean_y = mean(p_y))
    , by = coddistful]
  })
  
  mapa_datos <- reactive ({
    distritos %>% left_join(table_mapa(), by = "coddistful")
  })
  
  mapa_sel_distrito <- reactive ({
    mapa_datos() %>% filter(coddistful == sel_distrito()$coddistful)
  })
  
  t_outliers <- reactive ({
    sel_mes() %>% 
      group_by(Month) %>% 
      mutate(Q1 = quantile(t_mean, probs = 0.25),
             Q3 = quantile(t_mean, probs = 0.75),
             IQR = Q3 - Q1,
             upper.limit = Q3+1.5*IQR,
             lower.limit = Q1-1.5*IQR) %>%
      filter(t_mean > upper.limit | t_mean < lower.limit) %>% 
      mutate(tooltip = paste0("Año ", Year))
  })
  
  p_outliers <- reactive ({
    sel_mes() %>% 
      group_by(Month) %>% 
      mutate(Q1 = quantile(p_mean, probs = 0.25),
             Q3 = quantile(p_mean, probs = 0.75),
             IQR = Q3 - Q1,
             upper.limit = Q3+1.5*IQR,
             lower.limit = Q1-1.5*IQR) %>%
      filter(p_mean > upper.limit | p_mean < lower.limit) %>% 
      mutate(tooltip = paste0("Año ", Year))
  })
  
  #graficos
  output$TaNo <- renderPlot({
    ggplot() + 
#      geom_boxplot_interactive(data = seleccion(), aes(x = Year, y = t_mean_y, group = Year, tooltip = Model, data_id = Model), fill = "lightyellow") +
      geom_boxplot(data = seleccion(), aes(x = Year, y = t_mean_y, group = Year), fill = "lightyellow") +
#      geom_line(data = seleccion(), aes(x = Year, y = t_mean_y, colour = Model), alpha = 0.6) +
      labs(x = "Años", y = "Promedio de temperatura (C)") + 
      scale_colour_discrete(name="Modelos") +
      theme(legend.position = "bottom")
#    ggiraph(code = print(g),
#            hover_css = "fill:red;cursor:pointer;",
#            selection_type = "single",
#            selected_css = "fill:red;", width = 1)
    })
  
  output$Tmes <- renderPlot({
    ggplot() + 
      geom_boxplot(data = sel_mes(), aes(x = Month, y = t_mean, group = Month), outlier.shape = NA, fill = "lightyellow") +
      geom_point(data = t_outliers(), 
                 aes(x = Month, y = t_mean, colour = Model, 
                     group = Month), alpha = 0.7) +
      scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
      labs(x = "Mes", y = "Promedio de temperatura (C)") +
      scale_colour_discrete(name="Modelos") +
      theme(legend.position = "bottom")
  })
  
  output$PaNo <- renderPlot({
    ggplot() + 
      labs(x = "Años", y = "Total de lluvia (mm)") +
      geom_boxplot(data = seleccion(), aes(x = Year, y = p_y, group = Year), fill = "lightyellow") +
#      geom_line(data = seleccion(), aes(x = Year, y = p_y, colour = Model), alpha = 0.6) +
      scale_colour_discrete(name="Modelos") +
      theme(legend.position = "bottom")
  })
  
  output$Pmes <- renderPlot({
    ggplot() + 
      geom_boxplot(data = sel_mes(), aes(x = Month, y = p_mean, group = Month), outlier.shape = NA, fill = "lightyellow") +
      geom_point(data = p_outliers(), 
                             aes(x = Month, y = p_mean, colour = Model, 
                                 group = Month), alpha = 0.7) +
      scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
      labs(x = "Mes", y = "Total de lluvia (mm)") +
      scale_colour_discrete(name="Modelos") +
      theme(legend.position = "bottom")
#    ggiraph(code = print(g_prec), 
#            hover_css = "fill:red;cursor:pointer;",
#            selection_type = "single",
#            selected_css = "fill:red;", width = 1)
  })
  
  pal_p <- colorNumeric(
    palette = "YlGnBu",
    domain = val_distritos$p_y)
  
  pal_t <- colorNumeric(
    palette = "Oranges",
    domain = val_distritos$t_mean_y)
  
  mapa_base <- leaflet() %>% addTiles() %>% setView(lng=-85.186, lat=10.451, zoom = 8)
  
  output$mapaTemp <- renderLeaflet(mapa_base)
  
  observe({
    leafletProxy("mapaTemp", data = table_mapa()) %>%
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(
        data =  mapa_datos(),
        fillColor = ~pal_t(t_mean_y),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7
      ) %>%
      addLegend(pal = pal_t, values = ~t_mean_y, opacity = 0.7, title = "°C", position = "bottomleft")
  })
  
  observe({
    if (input$canton != "" & input$aNo[1] > 0 & input$cp != ""){
      leafletProxy("mapaTemp", data = seleccion()) %>% 
        removeShape("borde") %>% 
        addPolylines(
          layerId = "borde",
          data = mapa_sel_distrito(),
          stroke = TRUE,
          weight = 4,
          color = "red"
        )
    } 
  })
  
  output$mapaPrecip <- renderLeaflet(mapa_base)
  #if (input$canton == ""){
    
  #}
  #output$mapaPrecip <- renderLeaflet(leaflet(mapa_datos()) %>% 
   #                                    addTiles() %>% 
  #                                     setView(lng=-85.186, lat=10.451, zoom = 8) %>% 
  #                                     addPolygons(
  #                                       fillColor = ~pal_p(p_mean_y),
  #                                       weight = 1,
  #                                       opacity = 1,
  #                                       color = "white",
  #                                       dashArray = "3",
  #                                       fillOpacity = 0.7
  #                                     ) %>%
  #                                     addLegend(pal = pal_p, values = ~p_mean_y, opacity = 0.7, title = "mm", position = "bottomleft")
   #                                  )
  
  observe({
#    if (input$canton == "" & input$aNo[1] > 0 & input$cp != ""){
    leafletProxy("mapaPrecip", data = table_mapa()) %>%
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(
        data =  mapa_datos(),
        fillColor = ~pal_p(p_mean_y),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7
      ) %>%
      addLegend(pal = pal_p, values = ~p_mean_y, opacity = 0.7, title = "mm", position = "bottomleft")
#    }
  })
  
  observe({
    if (input$canton != "" & input$aNo[1] > 0 & input$cp != ""){
      leafletProxy("mapaPrecip", data = seleccion()) %>% 
        removeShape("borde") %>% 
        addPolylines(
          layerId = "borde",
          data = mapa_sel_distrito(),
          stroke = TRUE,
          weight = 4,
          color = "red"
        )
    } 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

