ui <- dashboardPage(
  dashboardHeader(title = "Congestión Vial"),
  dashboardSidebar(width = 260,
 h4('Filtros Mapa'),                 
 sidebarMenu(
   menuItem("Fechas", tabName = "fechas", icon = icon("fas fa-filter"),                   
    fluidRow(
      column(6,
      selectizeInput('varAnio',label = 'Año',c(unique(dsma$anio),'ALL'),selected = 'ALL', 
                     multiple = TRUE, options = list('plugins' = list('remove_button')))),
      column(6,
             selectizeInput('varMes',label = 'Mes',c(unique(dsma$mes),'ALL'),selected = 'ALL', 
                            multiple = TRUE, options = list('plugins' = list('remove_button'))))
      ),
      selectizeInput('varFinde',label = 'Fin de semana',c(unique(dsma$finDeSem),'ALL'),selected = 'ALL', 
                     multiple = FALSE, options = list('plugins' = list('remove_button'))),
      selectizeInput('varDiaSem',label = 'Dia semana',c(levels(datJamSegm_df$diaSem),'ALL'),selected = 'ALL', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
      selectizeInput('varDiaEsp',label = 'Dia calendario',c(levels(datJamSegm_df$diaStr),'ALL'),selected = 'ALL', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
    hr()
   ),
   menuItem("Horarios", tabName = "horarios", icon = icon("fas fa-filter"),
      fluidRow(
        column(2,h4('Inicio')),
        column(5,  
               numericInput("horaIni", "Hora:", 17, min = 0, max = 23,width = '400px')),
        column(5,
               numericInput("minIni", "Minuto:", 0, min = 0, max = 59,width = '400px'))
      ),
      fluidRow(
        column(2,h4('Fin')),
        column(5,
               numericInput("horaFin", "Hora:", 19, min = 0, max = 23,width = '400px')),
        column(5,
               numericInput("minFin", "Minuto:", 0, min = 0, max = 59,width = '400px'))
      ),
    hr()          
   ),
   menuItem("Otros", tabName = "otros", icon = icon("fas fa-filter"),
            selectizeInput('varCalle',label = 'Seleccione Calle',c(unique(datJamSegm_df$street),'ALL'),selected = 'ALL', 
                           multiple = TRUE, options = list('plugins' = list('remove_button'))),
            selectizeInput('varNivel',label = 'Seleccione Niveles',c(as.character(1:5),'ALL'),selected = 'ALL', 
                           multiple = TRUE, options = list('plugins' = list('remove_button'))),
            hr()          
   )),
    fluidRow(
      hr(),
      column(7,h4('Mayores a: xx %')),
      column(5,  
             numericInput("minPorc", "", 5, min = 0, max = 100,width = '400px'))
    ),
    fluidRow(
        hr(),
      column(6,  
        checkboxInput("usePalet", "Paleta estática", TRUE)),
      column(6,
        actionButton('run', 'Run', icon = icon("refresh")))
      )),
  dashboardBody(
    tabsetPanel(
      tabPanel('Mapa',icon = icon("fas fa-globe-europe"),
        fluidRow(
          column(9,
            box(title = "Filtros aplicados", width = 16, solidHeader = F, 
                status = "primary",collapsible = T, collapsed = T, 
            tableOutput('tablaFiltros'))),
          column(3,
             box(title = "Totales días y horas", width = 16, solidHeader = F, 
                 status = "primary",collapsible = T, collapsed = T, 
                 tableOutput('tablaRangos'))),
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
          box(title = "Configuración", width = 16, solidHeader = F, status = "primary",
               fluidRow(
                 column(2,
                 selectizeInput('varArrang',label = 'Variable de orden',
                                nomVarsDF_b[c('count'),'nomShow'],
                                selected = 'count',multiple = FALSE, 
                                options = list('plugins' = list('remove_button')))
                 ),
                 column(2,
                 numericInput("nCalles", "Primeras n calles", 30, min = 5, max = 100)
               ),
               column(2,
                      numericInput("largoInt", "Intervalo Tiempo (m)", 15, min = 10, max = 60)
               ),
               column(2,
                      selectizeInput('varFill',label = 'Variable color',
                                     nomVarsDF_b[c('count','delayMean','level345','lengthMean'),'nomShow'],
                                     selected = 'count',multiple = FALSE, 
                                     options = list('plugins' = list('remove_button')))
               ))
              ),
          box(title = "Filtros", width = 16, solidHeader = F, status = "primary",
              fluidRow(
                column(2,
                       selectizeInput('varAnioHeat',label = 'Año',c(unique(dsma$anio),'ALL'),selected = 'ALL', 
                                      multiple = TRUE, options = list('plugins' = list('remove_button'))),
                       actionButton('run2', 'Run Heat', icon = icon("refresh"))
                ),
                column(2,
                       selectizeInput('varMesHeat',label = 'Mes',c(unique(dsma$mes),'ALL'),selected = 'ALL', 
                                      multiple = TRUE, options = list('plugins' = list('remove_button')))
                ),
                column(2,
                       selectizeInput('varFindeHeat',label = 'Fin de semana',c(unique(dsma$finDeSem),'ALL'),selected = 'ALL', 
                                      multiple = FALSE, options = list('plugins' = list('remove_button')))
                ),
                column(2,
                       selectizeInput('varDiaSemHeat',label = 'Dia semana',c(levels(datJamSegm_df$diaSem),'ALL'),selected = 'ALL', 
                                      multiple = TRUE, options = list('plugins' = list('remove_button')))
                ),
                column(2,
                       selectizeInput('varDiaEspHeat',label = 'Dia calendario',c(levels(datJamSegm_df$diaStr),'ALL'),selected = 'ALL', 
                                      multiple = TRUE, options = list('plugins' = list('remove_button')))
                ),
                column(2,
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


