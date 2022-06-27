
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
  dashboardPage(title = 'Congestión evaluación',
                      header,
  dashboardSidebar(#width = 250,
    h4(HTML("<b>Filtros Mapa</b>"), 
    style="text-align:left"),
    
  sidebarMenu(
   menuItem(HTML("<b>Cluster</b>"), tabName = "cluster", icon = icon("fas fa-filter"),                   
      selectizeInput('clustName',label = 'Clusters',c(as.character(sort(clusterInfo$cluster)),'Todo'),selected ='Todo', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
      selectizeInput('varDiaSem',label = 'Dia semana',c(levels(dsma$diaSem),'Todo'),selected = 'Todo', 
                     multiple = TRUE, options = list('plugins' = list('remove_button'))),
               selectizeInput("horaSel", "Horas:", c(as.character(0:23),'Todo'), selected = 'Todo', 
                              multiple = TRUE, options = list('plugins' = list('remove_button')))
      ),
   menuItem(HTML("<b>Selección segmentos</b>"), tabName = "seleccion", icon = icon("fas fa-filter"),
            checkboxInput('buffer','Buffer',value = FALSE),
            numericInput('tamBuffer','Tamano buffer metros',value = 100,min = 0, max = 2000),
            hr()          
   ),
  fluidRow(
    column(12,
       actionButton('run', HTML('<b>Ejecutar consulta</b>'), icon = icon("refresh"),
                    style="color: #fff; background-color: #582C83; border-color: #2e6da4"))
    ))
  ),
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
      tabPanel(div(img(src='mapa_icon.svg', style = styleIcon), strong('Clusters segmentos')),
               fluidRow(
                 formattableOutput("table")
               )),
       tabPanel(div(img(src='mapa_icon.svg', style = styleIcon), strong('Mapa segmentos')),
        fluidRow(
          leafletOutput("map",width="100%",height="500px")
      )),
      tabPanel(div(img(src='descriptivos_icon.svg', style = styleIcon), strong('Evolución')),
          box(title = "Evolución de eventos del segmento seleccionado", width = 16, solidHeader = F, status = "danger", 
              plotlyOutput("plotSerie"),
              plotlyOutput("plotSerieb")
      )),
      tabPanel(div(img(src='matriz_icon.svg', style = styleIcon), strong('Descriptivos')),
          box(title = "Descriptivos", width = 16, solidHeader = F, status = "danger",
              fluidRow(
                tableOutput("Zona"),
                DT::dataTableOutput("widgets")))
              )
      )
    )
)



