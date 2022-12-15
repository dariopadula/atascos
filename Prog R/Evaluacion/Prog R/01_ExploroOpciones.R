
rm(list = ls())



libNecesarias = c('shiny','tidyverse','ggplot2','dplyr','ggrepel','shinydashboard','DT',
                  'formattable','gtools','MASS','sf','nngeo',
                  'mgcv','ggpubr','gridExtra','leaflet.extras','viridis','ggExtra','plotly',
                  'fst')



pkgInstall = installed.packages()[,'Package']

pkgAux = libNecesarias[!libNecesarias %in% pkgInstall]

if(length(pkgAux) > 0) install.packages(pkgAux)

########################################################
##### LIBRERIAS
for(ii in libNecesarias) eval(bquote(library(.(ii))))

###############################################
###############################################
load('Shiny/Insumos/XXX_dsma.RData')
#### Cargo la zona de interes
load('Prog R/Evaluacion/SHP/polZONA_sf.RData')
#### Cargo los segmentos unicos
load('Shiny/Insumos/XXX_segmentUnic.RData')
#### Cargo los datos asociados a los segmentos
datJamSegm_df = read.fst("Shiny/Insumos/datJamSegm_df.fst")

#### Cargo los datos sin segmentar
datJam_df = read.fst("Shiny/Insumos/datJam_df.fst")

################################
##### Encuentro los segmentos d ela zona

segmentUnic %>% mutate(largo = st_length(x = .))

segmInt = segmentUnic %>% 
  mutate(largo = as.numeric(st_length(x = .))) %>% 
  st_join(polZona,left = F)

segmIntCod = segmInt %>% st_set_geometry(NULL) %>% select(ID_segmento)
################################
#### Capturo datos
datInt = datJamSegm_df %>% inner_join(segmIntCod)

datIntID = datInt %>% select(entity_id,ID_Base) %>% 
  group_by(entity_id,ID_Base) %>% 
  slice_head() %>% 
  ungroup()


allSementZ = datJamSegm_df %>% inner_join(datIntID,by = c('entity_id','ID_Base')) %>%
  select(ID_segmento) %>% group_by(ID_segmento) %>% 
  summarise(n = n()) %>% ungroup()

allSeg = segmentUnic %>% inner_join(allSementZ)

plot(allSeg[,c('n')])
getwd()
save(allSeg,segmInt,file = 'Datos/SegmentosInter.RData')
#### Me quedo son la info 
datJamInt = datJam_df %>% inner_join(datIntID, by = c('entity_id','ID_Base'))

save(datJamInt,file = 'Datos/datJamInt.RData')

aa = unique(datJamInt$street)
bb = unique(datInt$street)

length(aa)
length(bb)

sum(!bb %in% aa)
sum(!aa %in% bb)

caa = datJamInt %>% group_by(street) %>%
  summarise(n = n(),
            largo = mean(length,na.rm = T),
            delay = mean(delay,na.rm = T),
            velocidad = mean(speedkmh,na.rm = T),
            level = mean(level,na.rm = T)) %>%
  ungroup()

#bb = datJam_df %>% group_by(entity_id,street) %>% slice_head() %>% ungroup()


# caa = datJamInt %>% group_by(street) %>%
#   summarise(n = n(),
#             largo = mean(length,na.rm = T),
#             delay = mean(delay,na.rm = T),
#             velocidad = mean(speedkmh,na.rm = T),
#             level = mean(level,na.rm = T)) %>%
#   ungroup()


# Dia intervencion
diaInt = "2022-04-19"


dsma_aux = dsma %>% mutate(diaFecha = as.Date(diaStr),
                           tratamiento = ifelse(diaFecha >= as.Date(diaInt),'Trat','NoTrat'))


totDias = dsma_aux %>% group_by(finDeSem,tratamiento) %>% 
  summarise(totDias = n()) %>% ungroup()

res = datJamInt %>% 
  left_join(dsma_aux %>% select(diaStr,tratamiento)) %>%
  left_join(totDias, by = c('finDeSem','tratamiento')) %>%
  group_by(finDeSem,hora,level,tratamiento) %>%
  summarise(n = n(),
            totDias = first(totDias),
            congPorDia = round(n/totDias),
            largo = mean(length,na.rm = T),
            retraso = mean(delay,na.rm = T),
            sum_largo = sum(length),
            sum_delay = sum(delay),
            largo_Norm = sum_largo/(totDias*30),
            delay_Norm = sum_delay/(totDias*30),
            velocidad = mean(speedkmh,na.rm = T)) %>%
  ungroup() %>% suppressMessages() %>%
  mutate(width = round(congPorDia/max(congPorDia)))


resSt = datJamInt %>% 
  left_join(dsma_aux %>% select(diaStr,tratamiento)) %>%
  left_join(totDias, by = c('finDeSem','tratamiento')) %>%
  group_by(street,finDeSem,hora,level,tratamiento) %>%
  summarise(n = n(),
            totDias = first(totDias),
            congPorDia = round(n/totDias),
            largo = mean(length,na.rm = T),
            delay = mean(delay,na.rm = T),
            largo_Norm = sum(length)/(totDias*30),
            delay_Norm = sum(delay)/(totDias*30),
            velocidad = mean(speedkmh,na.rm = T)) %>%
  ungroup() %>% suppressMessages()


roadT = datJamInt %>% 
  group_by(street,roadtype) %>%
  summarise(n = n()) %>%
ungroup()

res0 = res %>% filter(level == 1 & finDeSem != "Fin de semana")


library(lme4)

fit = lm(speedkmh ~ factor(roadtype) + factor(level) + factor(finDeSem) + factor(hora),data = datJamInt)
summary(fit)


fit01 = lmer(speedkmh ~ factor(roadtype) + (1  | street),data = datJamInt %>% filter(level == 1))
summary(fit01)
ref = ranef(fit01)$street


tt1 = datJamInt %>% filter(level == 1) %>%
  group_by(street) %>%
  summarise(n = n(),
            veloc = mean(speedkmh)) %>%
  ungroup()

ref[tt1$street,'n'] = tt1$n
ref[tt1$street,'veloc'] = tt1$veloc
#bb = datJam_df %>% group_by(entity_id,street) %>% slice_head() %>% ungroup()

# roadtype 1 = street, 2 = primary street

ggplot(res %>% filter(finDeSem != "Fin de semana")) + 
  geom_bar(stat = 'identity',aes(x = hora,y = congPorDia,fill = factor(tratamiento)),position = "dodge") + 
  facet_wrap(.~level)




ggplot(res %>% filter(finDeSem != "Fin de semana")) + 
  geom_bar(stat = 'identity',aes(x = hora,y = largo_Norm,fill = factor(tratamiento)),
                                 position = "dodge",width = 0.9,alpha = 0.8) + 
  facet_wrap(.~level)


ggplot(res %>% filter(finDeSem != "Fin de semana")) + 
  geom_bar(stat = 'identity',aes(x = hora,y = delay_Norm,fill = factor(tratamiento)),
           position = "dodge",width = 0.9,alpha = 0.8) + 
  facet_wrap(.~level)


ggplot(res %>% filter(finDeSem != "Fin de semana")) + 
  geom_bar(stat = 'identity',aes(x = hora,y = largo,fill = factor(tratamiento)),
           position = "dodge",width = 0.9,alpha = 0.8) + 
  facet_wrap(.~level)


# install.packages("scales")
library(scales) 

ggplot(res %>% filter(finDeSem != "Fin de semana")) + 
  geom_bar(stat = 'identity',aes(x = hora,y = largo,fill = factor(tratamiento),
                                 width=rescale(congPorDia,c(0.2,1))),position = "dodge",width = 0.1,alpha = 0.4) + 
  facet_wrap(.~level)
