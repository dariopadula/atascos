
dir('datos')

datos = read.table(paste0('Datos/etwazetrafficjam_','052022','.csv'),sep = ',',header = T)

datos = datos %>% filter(city == 'Montevideo') %>%
  mutate(datecreatedAux = as.POSIXct(strptime(gsub('T',' ',datecreated), "%Y-%m-%d %H:%M:%S")),
                           datemodifiedAux = as.POSIXct(strptime(gsub('T',' ',datemodified), "%Y-%m-%d %H:%M:%S")),
                           datepublishedAux = as.POSIXct(strptime(gsub('T',' ',datepublished), "%Y-%m-%d %H:%M:%S")))
dim(datos)

summary(datos$datepublishedAux)

sum(datos$datepublishedAux > as.POSIXct(strptime('2022-06-01 00:00:01', "%Y-%m-%d %H:%M:%S")))
sum(datos$datepublishedAux < as.POSIXct(strptime('2022-05-01 00:00:01', "%Y-%m-%d %H:%M:%S")))

sum(datos$datemodifiedAux > as.POSIXct(strptime('2022-06-01 00:00:01', "%Y-%m-%d %H:%M:%S")))
sum(datos$datemodifiedAux < as.POSIXct(strptime('2022-05-01 00:00:01', "%Y-%m-%d %H:%M:%S")))


varsSacar = c('datemodifiedAux','datepublishedAux','datecreatedAux')

datosP1 = datos %>% filter(datemodifiedAux <= as.POSIXct(strptime('2022-05-08 00:00:01', "%Y-%m-%d %H:%M:%S"))) %>%
  dplyr::select(-any_of(varsSacar))

datosP2 = datos %>% filter(datemodifiedAux > as.POSIXct(strptime('2022-05-08 00:00:01', "%Y-%m-%d %H:%M:%S")) & 
                             datemodifiedAux <= as.POSIXct(strptime('2022-05-15 00:00:01', "%Y-%m-%d %H:%M:%S"))) %>%
  dplyr::select(-any_of(varsSacar))

datosP3 = datos %>% filter(datemodifiedAux > as.POSIXct(strptime('2022-05-15 00:00:01', "%Y-%m-%d %H:%M:%S")) & 
                             datemodifiedAux <= as.POSIXct(strptime('2022-05-23 00:00:01', "%Y-%m-%d %H:%M:%S"))) %>%
  dplyr::select(-any_of(varsSacar))

datosP4 = datos %>% filter(datemodifiedAux > as.POSIXct(strptime('2022-05-23 00:00:01', "%Y-%m-%d %H:%M:%S"))) %>%
  dplyr::select(-any_of(varsSacar))

nrow(datosP1) + nrow(datosP2) +nrow(datosP3) +nrow(datosP4) 
nrow(datos)

write.table(datosP1, file = "Datos/etwazetrafficjam_052022_P1.csv",sep = ',',row.names = F)
write.table(datosP2, file = "Datos/etwazetrafficjam_052022_P2.csv",sep = ',',row.names = F)
write.table(datosP3, file = "Datos/etwazetrafficjam_052022_P3.csv",sep = ',',row.names = F)
write.table(datosP4, file = "Datos/etwazetrafficjam_052022_P4.csv",sep = ',',row.names = F)
