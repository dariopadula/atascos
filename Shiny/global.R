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



###################################################
##### CARGA DATOS

load('Shiny/Insumos/XXX_DatosShiny.RData')
load('Resultados/002_listSHPServicios.RData')

###### Data frame nombre variables 

nomVarsDF = data.frame(nomBase = c('street','count','delayMean','level45',
                                   'level345','levelMean','speedkmhMean',
                                   'lengthMean','timeStr'),
                       nomShow = c('Calle','Nro eventos','Delay Medio',
                                   'Total nivel 4 o +','Total nivel 3 o +','Nivel Medio',
                                   'Velocidad km/h Medio','Largo Medio','Hora Media')
                       )
                    

nomVarsDF_b = nomVarsDF
rownames(nomVarsDF_b) = as.character(nomVarsDF$nomBase)

nomVarsDF_s = nomVarsDF
rownames(nomVarsDF_s) = as.character(nomVarsDF$nomShow)


