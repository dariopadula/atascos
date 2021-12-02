ui <- dashboardPage(
  dashboardHeader(title = "Congestión Vial"),
  dashboardSidebar(
    fluidRow(
      selectizeInput('varFinde',label = 'Fin de semana',c(unique(datJamSegm_df$finDeSem),'ALL'),selected = 'ALL', 
                     multiple = FALSE, options = list('plugins' = list('remove_button'))),
      selectizeInput('varDiaSem',label = 'Dia semana',c(unique(datJamSegm_df$diaSem),'ALL'),selected = 'ALL', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
      selectizeInput('varDiaEsp',label = 'Dia calendario',c(levels(datJamSegm_df$diaStr),'ALL'),selected = 'ALL', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
      selectizeInput('varCalle',label = 'Seleccione Calle',c(unique(datJamSegm_df$street),'ALL'),selected = 'ALL', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
      selectizeInput('varNivel',label = 'Seleccione Niveles',c(as.character(1:5),'ALL'),selected = 'ALL', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
      hr()),
    fluidRow(
      column(12,h5('Hora inicio')),
    column(6,  
      numericInput("horaIni", "Hora:", 0, min = 0, max = 23)),
    column(6,
      numericInput("minIni", "Minuto:", 0, min = 0, max = 59))
    ),
    fluidRow(
      column(12,h5('Hora fin')),
      column(6,
      numericInput("horaFin", "Hora:", 0, min = 0, max = 23)),
      column(6,
      numericInput("minFin", "Minuto:", 0, min = 0, max = 59))
    ),
    fluidRow(
        hr(),
        actionButton('run', 'Run', icon = icon("refresh"))
      )),
  dashboardBody(
    tabsetPanel(
      tabPanel('Mapa',icon = icon("fas fa-leaf"),
        fluidRow(
          leafletOutput("distPlot",width="100%",height="700px")
        )
      ),
      tabPanel('Descriptivos',icon = icon("table"),
         fluidRow(
           DT::dataTableOutput("descTable")
         )
      ),
      tabPanel('Heat map',icon = icon("bar-chart-o"),
               fluidRow(
                 column(2,
                 selectizeInput('varArrang',label = 'Variable de orden',c('count','delayMean','level345','lengthMean'),
                                selected = 'count',multiple = FALSE, 
                                options = list('plugins' = list('remove_button'))),
                 selectizeInput('varFindeHeat',label = 'Fin de semana',c(unique(datJamSegm_df$finDeSem),'ALL'),selected = 'ALL', 
                                multiple = FALSE, options = list('plugins' = list('remove_button'))),
                 actionButton('run2', 'Run Heat', icon = icon("refresh"))
                 ),
                 column(2,
                 numericInput("nCalles", "Primeras n calles", 30, min = 5, max = 100),
                 selectizeInput('varDiaSemHeat',label = 'Dia semana',c(unique(datJamSegm_df$diaSem),'ALL'),selected = 'ALL', 
                                multiple = TRUE, options = list('plugins' = list('remove_button')))
               ),
               column(2,
                      numericInput("largoInt", "Intervalo Tiempo (m)", 15, min = 10, max = 60),
                      selectizeInput('varDiaEspHeat',label = 'Dia calendario',c(levels(datJamSegm_df$diaStr),'ALL'),selected = 'ALL', 
                                     multiple = TRUE, options = list('plugins' = list('remove_button')))
               ),
               column(2,
                      selectizeInput('varFill',label = 'Variable color',c('count','delayMean','level345','lengthMean'),
                                     selected = 'count',multiple = FALSE, 
                                     options = list('plugins' = list('remove_button'))),
                      selectizeInput('varNivelHeat',label = 'Seleccione Niveles',c(as.character(1:5),'ALL'),selected = 'ALL', 
                                     multiple = TRUE, options = list('plugins' = list('remove_button')))
                      
               )),
               fluidRow(
                 # plotOutput("heatPlot")
                 plotlyOutput("heatPlot", width = "100%",height = "100%")
               )
      )
    )
  )
)


