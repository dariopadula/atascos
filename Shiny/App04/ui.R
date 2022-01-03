ui <- dashboardPage(
  dashboardHeader(title = "Congestión Vial"),
  dashboardSidebar(width = 260,
    fluidRow(
      selectizeInput('varFinde',label = 'Fin de semana',c(unique(datJamSegm_df$finDeSem),'ALL'),selected = 'ALL', 
                     multiple = FALSE, options = list('plugins' = list('remove_button'))),
      selectizeInput('varDiaSem',label = 'Dia semana',c(levels(datJamSegm_df$diaSem),'ALL'),selected = 'ALL', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
      selectizeInput('varDiaEsp',label = 'Dia calendario',c(levels(datJamSegm_df$diaStr),'ALL'),selected = 'ALL', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
      selectizeInput('varCalle',label = 'Seleccione Calle',c(unique(datJamSegm_df$street),'ALL'),selected = 'ALL', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
      selectizeInput('varNivel',label = 'Seleccione Niveles',c(as.character(1:5),'ALL'),selected = 'ALL', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
      hr()),
    # h5('Inicio'),
    fluidRow(
      column(2,h4('Inicio')),
    column(5,  
      numericInput("horaIni", "Hora:", 0, min = 0, max = 23,width = '400px')),
    column(5,
      numericInput("minIni", "Minuto:", 0, min = 0, max = 59,width = '400px'))
    ),
    # h5('Fin'),
    fluidRow(
      column(2,h4('Fin')),
      column(5,
      numericInput("horaFin", "Hora:", 0, min = 0, max = 23,width = '400px')),
      column(5,
      numericInput("minFin", "Minuto:", 0, min = 0, max = 59,width = '400px'))
    ),
    fluidRow(
        hr(),
      column(6,  
        checkboxInput("usePalet", "Paleta estática", TRUE)),
      column(6,
        actionButton('run', 'Run', icon = icon("refresh")))
      )),
  dashboardBody(
    # tags$style("[type = 'number'] {font-size:14px;height:37px;}"),
    # tags$style(type='text/css', ".selectize-input { padding: 0px; min-height: 0;} .selectize-dropdown { line-height: 15px; }"),
    # tags$style(type='text/css', ".selectize-dropdown-content {max-height: 10px; }"), 
    # tags$head(tags$style(HTML('.navbar-brand {width: 30px; font-size:5px; text-align:center;}'))),
    
    tabsetPanel(
      tabPanel('Mapa',icon = icon("fas fa-globe-europe"),
        fluidRow(
          leafletOutput("distPlot",width="100%",height="700px")
        )
      ),
      tabPanel('Descriptivos',icon = icon("table"),
         fluidRow(
          box(title = "Descriptivos por calles para los datos filtrados", width = 16, solidHeader = F, status = "primary", 
           DT::dataTableOutput("descTable"))
         )
      ),
      tabPanel('Heat map',icon = icon("fas fa-fire"),
          box(title = "Configuración y filtros", width = 16, solidHeader = F, status = "primary",
               fluidRow(
                 column(2,
                 selectizeInput('varArrang',label = 'Variable de orden',
                                # nomVarsDF_b[c('count','delayMean','level345','lengthMean'),'nomShow'],
                                nomVarsDF_b[c('count'),'nomShow'],
                                selected = 'count',multiple = FALSE, 
                                options = list('plugins' = list('remove_button'))),
                 selectizeInput('varFindeHeat',label = 'Fin de semana',c(unique(datJamSegm_df$finDeSem),'ALL'),selected = 'ALL', 
                                multiple = FALSE, options = list('plugins' = list('remove_button'))),
                 actionButton('run2', 'Run Heat', icon = icon("refresh"))
                 ),
                 column(2,
                 numericInput("nCalles", "Primeras n calles", 30, min = 5, max = 100),
                 selectizeInput('varDiaSemHeat',label = 'Dia semana',c(levels(datJamSegm_df$diaSem),'ALL'),selected = 'ALL', 
                                multiple = TRUE, options = list('plugins' = list('remove_button')))
               ),
               column(2,
                      numericInput("largoInt", "Intervalo Tiempo (m)", 15, min = 10, max = 60),
                      selectizeInput('varDiaEspHeat',label = 'Dia calendario',c(levels(datJamSegm_df$diaStr),'ALL'),selected = 'ALL', 
                                     multiple = TRUE, options = list('plugins' = list('remove_button')))
               ),
               column(2,
                      selectizeInput('varFill',label = 'Variable color',
                                     nomVarsDF_b[c('count','delayMean','level345','lengthMean'),'nomShow'],
                                     selected = 'count',multiple = FALSE, 
                                     options = list('plugins' = list('remove_button'))),
                      selectizeInput('varNivelHeat',label = 'Seleccione Niveles',c(as.character(1:5),'ALL'),selected = 'ALL', 
                                     multiple = TRUE, options = list('plugins' = list('remove_button')))
                      
               ))
              ),
               fluidRow(
                box(title = "Mapa de calor por intervalo de tiempo", width = 16, solidHeader = F, status = "primary", 
                 plotlyOutput("heatPlot", width = "100%",height = "100%")),
                 br(),
                box(title = "% de eventos por ranking de calles", width = 16, solidHeader = F, status = "primary",
                 plotlyOutput("porcPlot", width = "100%",height = "100%"))
               )
      )
    )
  )
)


