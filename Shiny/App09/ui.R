
##############################################
###### Setting css


#1FC17B verde oscuro
#82E0AA Verde claro

#2E2EFE azul oscuro
#58ACFA azul claro

#FF8000 naranja oscuro
#FAAC58 naranja claro

#6E6E6E gris oscuro
#A4A4A4 gris claro

#Violetas
# Primario #582C83
# Secundario #B394FF

style = tags$head(tags$style(HTML('
        /* logo  recuadro superior*/
        .skin-blue .main-header .logo {
                              background-color: #582C83;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #B394FF;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #B394FF;
                              }        

        /* main sidebar (fondo del slider bar)*/
        .skin-blue .main-sidebar {
                              background-color: #E2ECE5;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #582C83;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #E2ECE5;
                              color: #000000;
        }
                              


        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #582C83;
                              color: #000000;
         }
                              
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #56D699;
         }
                              
        /* siderbar text color */ 
        .skin-blue .main-sidebar .sidebar{
          color: #000000;
        } 
        
    /* Color de fondo de los items desplegables */     
    .skin-blue .sidebar-menu>li>.treeview-menu {
    margin: 0 1px;
    background: #f7f7f7;
}

        

      .box.box-solid.box-danger>.box-header {
        color:#fff;
        background:#582C83
                }
      
      .box.box-solid.box-danger{
        border-bottom-color:#582C83;
        border-left-color:#582C83;
        border-right-color:#582C83;
        border-top-color:#582C83;
      }
      
      .box.box-danger>.box-header {
        color:#000000;
        background:#fff
                }
      
      .box.box-danger{
        border-bottom-color:#582C83;
        border-left-color:#582C83;
        border-right-color:#582C83;
        border-top-color:#582C83;
      }
      
    /*  Blue background when checked */
.custom-control-input:checked~.custom-control-label::before {
    color: #fff;
    border-color: #582C83;
    background-color: #582C83;
}


                                  ')))


### Logo 
# title = tags$img(src='IM_logo_mapa.svg', height = '60', width ='250')

### Imagen centrada en el header
header_img <- tags$img(
  src='IM_logo_mapa.svg', height = '60', width ='250',
  style = 'height: 60px; width: 250px;margin-top:-55px;margin-left:-40px;position: absolute;'
)
header <-  htmltools::tagQuery(dashboardHeader(title = ""))
header <- header$
  addAttrs(style = "position: relative")$ # add some styles to the header 
  append(header_img)$ # inject our img
  allTags()

###############################
#### Iconos tabPanels
styleIcon = "height: 15px; width: 15px;margin-top:-4px;margin-left:0px;"
##############################################
##############################################

ui <-   
  dashboardPage(title = 'Congestión',
                      header,
  dashboardSidebar(#width = 250,
    h4(HTML("<b>Filtros Mapa</b>"), 
    style="text-align:left"),
    
  sidebarMenu(
   menuItem(HTML("<b>Fechas</b>"), tabName = "fechas", icon = icon("fas fa-filter"),                   
    fluidRow(
      column(6,
      selectizeInput('varAnio',label = 'Año',c(unique(dsma$anio),'Todo'),selected = 'Todo', 
                     multiple = TRUE, options = list('plugins' = list('remove_button')))),
      column(6,
             selectizeInput('varMes',label = 'Mes',c(unique(dsma$mes),'Todo'),selected = 'Todo', 
                            multiple = TRUE, options = list('plugins' = list('remove_button'))))
      ),
      selectizeInput('varFinde',label = 'Tipo de día',c(unique(dsma$finDeSem),'Todo'),selected = 'Todo', 
                     multiple = FALSE, options = list('plugins' = list('remove_button'))),
      selectizeInput('varDiaSem',label = 'Dia semana',c(levels(datJamSegm_df$diaSem),'Todo'),selected = 'Todo', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
      selectizeInput('varDiaEsp',label = 'Dia calendario',c(levels(datJamSegm_df$diaStr),'Todo'),selected = 'Todo', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
    hr()
   ),
   menuItem(HTML("<b>Horarios</b>"), tabName = "horarios", icon = icon("fas fa-filter"),
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
   menuItem(HTML("<b>Otros</b>"), tabName = "otros", icon = icon("fas fa-filter"),
            selectizeInput('varCalle',label = 'Calles',c(unique(datJamSegm_df$street),'Todo'),selected = 'Todo', 
                           multiple = TRUE, options = list('plugins' = list('remove_button'))),
            selectizeInput('varNivel',label = 'Niveles congestión',c(as.character(1:4),'Todo'),selected = 'Todo', 
                           multiple = TRUE, options = list('plugins' = list('remove_button'))),
            hr()          
   )),
    numericInput("minPorc", "% tiempo congestión mayor a:", 5, min = 0, max = 100,width = '400px'),
    hr(),
  checkboxInput("usePalet", HTML("<b>Paleta estática</b>"), TRUE),
  fluidRow(
    column(12,
       actionButton('run', HTML('<b>Ejecutar consulta</b>'), icon = icon("refresh"),
                    style="color: #fff; background-color: #582C83; border-color: #2e6da4"))
    )),
  dashboardBody(
    #### Cambia el css 
    style,
    tags$head(tags$style(HTML("div.col-sm-6 {padding:5px}"))),
    tags$head(tags$style(HTML("div.col-sm-5 {padding:10px}"))),
    tags$head(tags$style(HTML("div.col-sm-2 {padding:15px}"))),
    tags$head(tags$style(HTML("div.col-sm-3 {padding:0px}"))),
    tags$head(tags$link(rel="shortcut icon", href="Interna_favicon.ico")),
    
    tags$head(
      tags$style(type = "text/css", "a{color: #000000;}")
      ##################
    ),
    tabsetPanel(
      tabPanel(div(img(src='inicio_icon.svg', style = styleIcon), strong('Inicio')),
               br(),
               
               div(strong("Introducción"),style = "color:#000000; font-size:18px"),
               
               # textOutput('textPeriodo'),
               
               htmlOutput('textPeriodo'),
               
               p("Se trata de una aplicación web que permite visualizar la información sobre el 
               estado del tránsito en Montevideo en los últimos 15 días. Se utilizan datos de 
               Waze consultados cada 2 minutos sobre reportes de eventos de 
                 congestionamiento. Por más información ir a: ",a("Waze Homepage", href="https://www.waze.com/es/live-map/"),
                 style = "font-size:14px"),
               
               p("El mapa presenta las calles del departamento según la ocurrencia de eventos de congestión. A su vez, posibilita configurar períodos y horarios de interés, 
               hacer zoom en las zonas que se desee o simplemente seleccionar la calle. 
               Se ofrece un ranking con las calles más congestionadas según la elección de tiempo. 
               Y por último, se despliega una matriz de calor para las calles más congestionadas a lo largo del día.",style = "font-size:14px"),
               
               # p("Se ofrece un ranking con las calles más eventos de congestión según la elección de tiempo. Y por último, se despliega 
               #   una matriz de calor para las calles más congestionadas a lo largo del día.",style = "font-size:14px"),
               
               p("La app tiene tres pestañas: Mapa de congestión, Descriptivos y Matriz de calor.",style = "font-size:14px"),
               
               
               div(strong("Mapa de congestión"),style = "color:#000000; font-size:18px"),
               
               p("Se muestra el mapa de Montevideo coloreando las vías según la ocurrencia de eventos de congestión, asociado a los filtros seleccionados por el/la usuario/a en 
                 el menú lateral: fecha, rango horario, nivel de congestionamiento, entre otros.",
                 style = "font-size:14px"),
               
               
               p("La escala de representación en el mapa está definida en función del porcentaje del 
               tiempo de congestión en el que se reportaron eventos de congestionamiento de Waze 
               (eventos en adelante). Este porcentaje de tiempo de congestión refiere a la porción 
                 de tiempo seleccionado donde hubo reportes de eventos de congestión. Cuanto mayor 
                 es la congestión, más intenso será el color.",style = "font-size:14px"),
               
               div(strong("Descriptivos"),style = "color:#000000; font-size:18px"),
               
               p("De acuerdo a los filtros seleccionados en el menú lateral, se despliegan algunas 
               medidas descriptivas como ser: cantidad de 
               reportes de eventos en ese período, tiempo  y largo medio de los eventos, 
               entre otros. Las calles se exhiben ordenadas en forma descendente según su congestión.
",style = "font-size:14px"),
               
               div(strong("Matriz de calor"),style = "color:#000000; font-size:18px"),
               
               
               p("Se visualiza la matriz de calles por intervalo de tiempo de la variable que se 
               elija representar, ya sea cantidad de eventos o solamente aquellos eventos con 
               los niveles más altos de congestión. 
                 El orden de las calles está dado por el número de eventos de congestión.",
                 style = "font-size:14px")
               
      ),
      tabPanel(div(img(src='mapa_icon.svg', style = styleIcon), strong('Mapa de congestión')),
        fluidRow(
          column(9,
            box(title = "Filtros aplicados", width = 16, solidHeader = F, 
                status = "danger",collapsible = T, collapsed = T, 
            tableOutput('tablaFiltros'))),
          column(3,
             box(title = "Totales días y horas", width = 16, solidHeader = F, 
                 status = "danger",collapsible = T, collapsed = T, 
                 tableOutput('tablaRangos'))),
          leafletOutput("distPlot",width="100%",height="700px")
        )
      ),
      tabPanel(div(img(src='descriptivos_icon.svg', style = styleIcon), strong('Descriptivos')),
          box(title = "Descriptivos por calles para los datos filtrados", width = 16, solidHeader = F, status = "danger", 
           DT::dataTableOutput("descTable")),
      div(style="display:inline-block;vertical-align:top;",
        fluidRow(      
          column(4,
                 numericInput('nFilas','Filas a descargar:',40, min = 1, max = 10000,width = '300px')),
          column(2,
                 # br(),
                 downloadButton("downloadData", HTML("<b>Descargar Tabla</b>"),
                                style="color: #fff; background-color: #582C83; border-color: #2e6da4; margin-top: 11px;"))
          )
      )
      ),
      tabPanel(div(img(src='matriz_icon.svg', style = styleIcon), strong('Matriz de calor')),
          box(title = "Configuración", width = 16, solidHeader = F, status = "danger",
              fluidRow(
                column(12,
                       p(HTML("En esta pestaña se genera la matriz de calles por intervalo de tiempo de la variable que se elija representar: cantidad de 
                       eventos o sólo aquellos eventos con los niveles más altos de congestión. <br> 
                              En primer lugar, el usuario configura la matriz seleccionando lo que se desea ver: cantidad de calles, 
partición del tiempo y variable representada en el mapa ya sea cantidad de eventos o 
eventos con niveles más altos de congestión."),style = "font-size:14px")
                )),
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
          box(title = "Filtros", width = 16, solidHeader = F, status = "danger",
              fluidRow(
                column(12,
                       p("Luego se seleccionan los filtros que desee: mes, día, 
                           nivel de congestión, entre otros.",style = "font-size:14px"))
              ),
              fluidRow(
                column(2,
                       selectizeInput('varAnioHeat',label = 'Año',c(unique(dsma$anio),'Todo'),selected = 'Todo', 
                                      multiple = TRUE, options = list('plugins' = list('remove_button'))),
                       actionButton('run2', HTML('<b>Ejecutar consulta</b>'), icon = icon("refresh"),
                                    style="color: #fff; background-color: #582C83; border-color: #2e6da4")
                ),
                column(2,
                       selectizeInput('varMesHeat',label = 'Mes',c(unique(dsma$mes),'Todo'),selected = 'Todo', 
                                      multiple = TRUE, options = list('plugins' = list('remove_button')))
                ),
                column(2,
                       selectizeInput('varFindeHeat',label = 'Tipo de día',c(unique(dsma$finDeSem),'Todo'),selected = 'Todo', 
                                      multiple = FALSE, options = list('plugins' = list('remove_button')))
                ),
                column(2,
                       selectizeInput('varDiaSemHeat',label = 'Dia semana',c(levels(datJamSegm_df$diaSem),'Todo'),selected = 'Todo', 
                                      multiple = TRUE, options = list('plugins' = list('remove_button')))
                ),
                column(2,
                       selectizeInput('varDiaEspHeat',label = 'Dia calendario',c(levels(datJamSegm_df$diaStr),'Todo'),selected = 'Todo', 
                                      multiple = TRUE, options = list('plugins' = list('remove_button')))
                ),
                column(2,
                       selectizeInput('varNivelHeat',label = 'Niveles congestión',c(as.character(1:4),'Todo'),selected = 'Todo', 
                                      multiple = TRUE, options = list('plugins' = list('remove_button')))
                       
                ))
          ),
               fluidRow(
                box(title = "Matriz de calles por intervalo de tiempo según número de eventos de congestión", width = 16, solidHeader = F, status = "danger", 
                 plotlyOutput("heatPlot", width = "100%",height = "100%")),
                 br(),
                box(title = "% de eventos por ranking de calles", width = 16, solidHeader = F, status = "danger",
                 plotlyOutput("porcPlot", width = "100%",height = "100%"))
               )
      )
    )
  )
)


