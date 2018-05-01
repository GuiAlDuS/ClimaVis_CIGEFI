#Paquetes necesarios
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(leaflet)

#Archivos y datos de entrada
dist_cant <- fread("distritos_cantones.csv")
val_distritos <- fread("val_distritos.csv")

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
    titleWidth = 400
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
        tabPanel("Mensual",
                 conditionalPanel("input.aNo >=5", plotOutput("Tmes"))
                 ),
        tabPanel("Anual", plotOutput("TaNo")
        )
        )
      ),
    fluidRow(
      tabBox(
        id = "mapa", width = 5, 
        tabPanel("Temperatura",
                 leafletOutput("mapaTemp")
                 ),
        tabPanel("Lluvia",
                 conditionalPanel("input.aNo >= 5", leafletOutput("mapaPrecip"))
                 )
        ),
      tabBox(
        side = "right",
        title = "Lluvia", id = "precip", width = 7, selected = "Anual",
      tabPanel("Mensual",
               conditionalPanel("input.aNo >= 5", plotOutput("Pmes"))
               ),
      tabPanel("Anual",
               plotOutput("PaNo")
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
  seleccion <- reactive({val_distritos[distrito == input$distritos &
                                          Scenario == input$cp & 
                                          Model %in% input$modelos &
                                          Year >= input$aNo[1] & 
                                          Year <= input$aNo[2]]
    })
  
  #seleccion <- val_distritos[distrito == "Las Juntas" &
   #                            Scenario == "rpc45" &
    #                           Model == "ccsm4_r1i1p1" &
     #                          Year >= 2030 & Year <= 2060
      #                       ]
  
  #graficos
  output$TaNo <- renderPlot({
    ggplot() + geom_line(data = seleccion(), aes(x = Year, y = t_mean, colour = Model)) + 
      labs(x = "Años", y = "Temperatura (C)") + 
      scale_colour_discrete(name="Modelos")
    })
}


# Run the application 
shinyApp(ui = ui, server = server)

