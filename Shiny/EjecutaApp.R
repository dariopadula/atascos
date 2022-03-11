#########################################################
####### INSTALA LIBRERIAS QUE SE NECESITAN

libNecesarias = c('shiny','tidyverse','ggplot2','dplyr','ggrepel','shinydashboard','DT',
                  'formattable','gtools','MASS','sf','nngeo',
                  'mgcv','ggpubr','gridExtra','leaflet.extras','viridis','ggExtra','plotly')



pkgInstall = installed.packages()[,'Package']

pkgAux = libNecesarias[!libNecesarias %in% pkgInstall]

if(length(pkgAux) > 0) install.packages(pkgAux)

########################################################
##### LIBRERIAS
for(ii in libNecesarias) eval(bquote(library(.(ii))))

###################################################
### FUNCIONES
fun = dir('Funciones/')
fun = fun[grep('.R',fun,ignore.case = T)]
for(ii in fun) source(paste0('Funciones/',ii))


##############################################
###### COLORES
paletaMia6 = c('#01A9DB','#088A08','#D7DF01','#DBA901','#DF7401','#FF0000')
paletaMia5 = c('#088A08','#D7DF01','#DBA901','#DF7401','#FF0000')
paletaMia4 = c('#088A08','#D7DF01','#DF7401','#FF0000')
paletaMia3 = c('#088A08','#DF7401','#FF0000')

gray6 = c('#BDBDBD','#848484','#6E6E6E','#585858','#2E2E2E','#151515')
#######################################

###################################################
##### CARGA DATOS

# Base con los datos de Jam
load('Shiny/Insumos/XXX_DatosShiny.RData')
# Base con los datos de Cortes
load('Shiny/Insumos/XXX_DatosShinyCortes.RData')
# Base con los datos de AutoScop
load('Shiny/Insumos/XXX_DatosAScopShiny.RData')
# Tabla de dias, anio, meses para filtrar los días
load('Shiny/Insumos/XXX_dsma.RData')
# Lista con shapes de servicios a incluir
load('Resultados/002_listSHPServicios.RData')
# Base para hacer el resumen
load('Shiny/Insumos/XXX_datJam_resum.RData')
# datJam_df = readRDS('Shiny/Insumos/datJam_df.RDS')
# datJamSegm_df = readRDS('Shiny/Insumos/datJamSegm_df.RDS')
# segmentUnic = readRDS('Shiny/Insumos/segmentUnic.RDS')




###### Data frame nombre variables 

nomVarsDF = data.frame(nomBase = c('street','count','delayMean','level45',
                                   'level345','levelMean','speedkmhMean',
                                   'lengthMean','timeStr',
                                   'Anio','Mes','finDeSem','diaSem',
                                   'diaStr','Nivel','HoraIni','HoraFin'),
                       nomShow = c('Calle','Nro eventos','Delay Medio',
                                   'Total nivel 4 o +','Total nivel 3 o +','Nivel Medio',
                                   'Velocidad km/h Medio','Largo Medio','Hora Media',
                                   'Año','Mes','Fin de Semana','Día semana',
                                   'Día calendario','Niveles','Hora Inicio','Hora Fin')
                       )
                    


nomVarsDF_b = nomVarsDF
rownames(nomVarsDF_b) = as.character(nomVarsDF$nomBase)

nomVarsDF_s = nomVarsDF
rownames(nomVarsDF_s) = as.character(nomVarsDF$nomShow)

###########################################
###### EJECUTA APP
runApp('Shiny/App07',launch.browser = TRUE)


# https://towardsdatascience.com/create-interactive-map-applications-in-r-and-r-shiny-for-exploring-geospatial-data-96b0f9692f0f

