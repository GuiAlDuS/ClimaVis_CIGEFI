#Paquetes necesarios
library(shinydashboard)
library(shinyWidgets)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(data.table)
library(sf)
library(leaflet)

#Archivos y datos de entrada
dist_cant <- fread("distritos_cantones.csv")
val_distritos <- data.table(readRDS("val_distritos.rds"))
val_distritos_mes <- data.table(readRDS("val_distritos_mes.rds"))
distritos <- st_read("distritos_mapa.json")

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

#Funciones


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
                      choices = sort(dist_cant$canton)
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
        tabPanel("Mensual", ggiraphOutput("Tmes")
                 ),
        tabPanel("Anual", plotOutput("TaNo")
        )
        )
      ),
    fluidRow(
      tabBox(
        id = "mapa", width = 5, 
        tabPanel("Temperatura", leafletOutput("mapaTemp")
                 ),
        tabPanel("Lluvia", leafletOutput("mapaPrecip")
                 )
        ),
      tabBox(
        side = "right",
        title = "Lluvia", id = "precip", width = 7, selected = "Anual",
      tabPanel("Mensual", ggiraphOutput("Pmes")
               ),
      tabPanel("Anual", plotOutput("PaNo")
      )
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
  
  sel_mes <- reactive({
    val_distritos_mes[distrito == input$distritos &
                        canton == input$canton &
                        Model %in% input$modelos &
                        Scenario == input$cp & 
                        Year >= input$aNo[1] & Year <= input$aNo[2]]
  })

  mapa_table <- reactive({
    val_distritos[
      Model %in% input$modelos &
        Scenario == input$cp & 
        Year >= input$aNo[1] & Year <= input$aNo[2]
      , .(t_mean_y = mean(t_mean_y), p_mean_y = mean(p_y))
      , by = coddistful]
  })
  
  mapa_datos <- reactive({
    distritos %>% left_join(mapa_table(), by = "coddistful")
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
  
  output$Tmes <- renderggiraph({
    g_temp <- ggplot() + 
      geom_boxplot(data = sel_mes(), aes(x = Month, y = t_mean, group = Month), outlier.shape = NA, fill = "lightyellow") +
      geom_point_interactive(data = t_outliers(), 
                             aes(x = Month, y = t_mean, colour = Model, 
                                 group = Month, tooltip = t_outliers()$tooltip), alpha = 0.7) +
      scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
      labs(x = "Mes", y = "Promedio de temperatura (C)") +
      scale_colour_discrete(name="Modelos") +
      theme(legend.position = "bottom")
    ggiraph(code = print(g_temp),
            hover_css = "fill:red;cursor:pointer;",
            selection_type = "single",
            selected_css = "fill:red;", width = 1)
  })
  
  output$PaNo <- renderPlot({
    ggplot() + 
      labs(x = "Años", y = "Total de lluvia (mm)") +
      geom_boxplot(data = seleccion(), aes(x = Year, y = p_y, group = Year), fill = "lightyellow") +
#      geom_line(data = seleccion(), aes(x = Year, y = p_y, colour = Model), alpha = 0.6) +
      scale_colour_discrete(name="Modelos") +
      theme(legend.position = "bottom")
  })
  
  output$Pmes <- renderggiraph({
    g_prec <- ggplot() + 
      geom_boxplot(data = sel_mes(), aes(x = Month, y = p_mean, group = Month), outlier.shape = NA, fill = "lightyellow") +
      geom_point_interactive(data = p_outliers(), 
                             aes(x = Month, y = p_mean, colour = Model, 
                                 group = Month, tooltip = p_outliers()$tooltip), alpha = 0.7) +
      scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
      labs(x = "Mes", y = "Total de lluvia (mm)") +
      scale_colour_discrete(name="Modelos") +
      theme(legend.position = "bottom")
    ggiraph(code = print(g_prec), 
            hover_css = "fill:red;cursor:pointer;",
            selection_type = "single",
            selected_css = "fill:red;", width = 1)
  })
  
  output$mapaTemp <- renderLeaflet({
    pal <- colorNumeric(
      palette = "Oranges",
      domain = mapa_datos()$t_mean_y)
    
    leaflet(mapa_datos()) %>% 
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(t_mean_y),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = ~t_mean_y) %>% 
      setView(lng=-85.186, lat=10.451, zoom = 8) %>% 
      addLegend(pal = pal, values = ~t_mean_y, opacity = 0.7, title = "C", position = "bottomleft")
  })
  
  output$mapaPrecip <- renderLeaflet({
    pal <- colorNumeric(
      palette = "Blues",
      domain = mapa_datos()$p_mean_y)
    
    leaflet(mapa_datos()) %>% 
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(p_mean_y),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7) %>% 
      setView(lng=-85.186, lat=10.451, zoom = 8) %>% 
      addLegend(pal = pal, values = ~p_mean_y, opacity = 0.7, title = "mm", position = "bottomleft")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

