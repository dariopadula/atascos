#########################################################
####### INSTALA LIBRERIAS QUE SE NECESITAN

libNecesarias = c('shiny','tidyverse','ggplot2','dplyr','ggrepel','shinydashboard','DT',
                  'formattable','gtools','MASS','sf','nngeo',
                  'mgcv','ggpubr','gridExtra','leaflet.extras','viridis','ggExtra','plotly',
                  'fst','conflicted')



pkgInstall = installed.packages()[,'Package']

pkgAux = libNecesarias[!libNecesarias %in% pkgInstall]

if(length(pkgAux) > 0) install.packages(pkgAux)

########################################################
##### LIBRERIAS
for(ii in libNecesarias) eval(bquote(library(.(ii))))



conflict_prefer("select","dplyr")
conflict_prefer("filter","dplyr")
conflict_prefer("summarise","dplyr")
conflict_prefer("mutate","dplyr")
conflict_prefer("arrange","dplyr")
conflict_prefer("box","shinydashboard")

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
# load('Shiny/Insumos/XXX_DatosShiny.RData')
datJamSegm_df = read.fst("Shiny/Insumos/datJamSegm_df.fst")
datJam_df = read.fst("Shiny/Insumos/datJam_df.fst")

load('Shiny/Insumos/XXX_segmentUnic.RData')

# Base con los datos de Cortes
load('Shiny/Insumos/XXX_DatosShinyCortes.RData')
# Base con los datos de AutoScop
load('Shiny/Insumos/XXX_DatosAScopShiny.RData')

# Datos del STM por paradas
datSTM_df = read.fst("Shiny/Insumos/datSTM_df.fst")
load('Shiny/Insumos/XXX_puntSTMUnic.RData')
# Tabla de dias, anio, meses para filtrar los días
load('Shiny/Insumos/XXX_dsma.RData')
# Lista con shapes de servicios a incluir
load('Resultados/002_listSHPServicios.RData')
# Base para hacer el resumen
# load('Shiny/Insumos/XXX_datJam_resum.RData')
datJam_resum = read.fst("Shiny/Insumos/datJam_resum.fst")
# datJam_df = readRDS('Shiny/Insumos/datJam_df.RDS')
# datJamSegm_df = readRDS('Shiny/Insumos/datJamSegm_df.RDS')
# segmentUnic = readRDS('Shiny/Insumos/segmentUnic.RDS')




###### Data frame nombre variables 

nomVarsDF = data.frame(nomBase = c('street','count','delayMean','level45',
                                   'level345','levelMean','speedkmhMean',
                                   'lengthMean','timeStr',
                                   'Anio','Mes','finDeSem','diaSem',
                                   'diaStr','Nivel','HoraIni','HoraFin'),
                       nomShow = c('Calle','Nro eventos','Demora media (segundos)',
                                   'Total nivel 4 o +','Total nivel 3 o +','Nivel Medio',
                                   'Velocidad km/h Medio','Largo Medio (metros)','Hora Media',
                                   'Anio','Mes','Tipo de dia','Dia semana',
                                   'Dia calendario','Niveles','Hora Inicio','Hora Fin')
)



nomVarsDF_b = nomVarsDF
rownames(nomVarsDF_b) = as.character(nomVarsDF$nomBase)

nomVarsDF_s = nomVarsDF
rownames(nomVarsDF_s) = as.character(nomVarsDF$nomShow)


################################
##### Arregegla dias

adaptaDias = function(xx) {
  xx[grep('lunes|Monday',xx,ignore.case = T)] = 'Lunes'
  xx[grep('Martes|Tuesday',xx,ignore.case = T)] = 'Martes'
  xx[grep('rcoles|Wednesday',xx,ignore.case = T)] = 'Miercoles'
  xx[grep('Jueves|Thursday',xx,ignore.case = T)] = 'Jueves'
  xx[grep('Viernes|Friday',xx,ignore.case = T)] = 'Viernes'
  xx[grep('bado|Saturday',xx,ignore.case = T)] = 'Sabado'
  xx[grep('Domingo|Sunday',xx,ignore.case = T)] = 'Domingo'
  
  return(xx)          
}

dsma = dsma %>% mutate(diaSem = adaptaDias(weekdays(as.Date(diaStr))),
                       finDeSem = ifelse(diaSem %in% c('Sabado','Domingo'),'Fin de semana','Lunes a viernes'),
                       diaSem = factor(diaSem,levels = c('Lunes','Martes','Miercoles','Jueves','Viernes','Sabado','Domingo')),
                       levelDia = as.numeric(diaSem))



###########################################
###### EJECUTA APP
runApp('Shiny/App10',launch.browser = TRUE)


# https://towardsdatascience.com/create-interactive-map-applications-in-r-and-r-shiny-for-exploring-geospatial-data-96b0f9692f0f

