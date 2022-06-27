# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

# Load dataset from github
# data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
# data$date <- as.Date(data$date)


######################################################
######################################################
aa =  pruebaS %>% filter(ID_segmento == 26666)

grillaDiaHora = dsma  %>% mutate(nhoras = 24) %>% uncount(nhoras) %>%
  group_by(diaStr) %>%
  mutate(hora = row_number() - 1) %>%
  ungroup() %>%
  arrange(diaStr,hora)


putNa0 = function(x) {
  x[is.na(x)] = 0
  return(x)
}

bb = grillaDiaHora %>% 
  left_join(aa %>% select(diaStr,hora,n,N34,porcDH,porcDH_N34), by = c("diaStr","hora")) %>%
  mutate(across(c('n','N34','porcDH','porcDH_N34'),putNa0))


data = bb


data = data %>% mutate(date = ifelse(hora < 10,paste0(diaStr,' 0',hora,':00:00'),paste0(diaStr,' ',hora,':00:00')),
                       date = as.POSIXct(strptime(gsub('T',' ',date), "%Y-%m-%d %H:%M:%S")),
                       value = porcDH_N34,
                       diaSem = factor(diaSem,levels = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo')))


# Usual area chart
# p <- data %>% filter(finDeSem == 'Lunes a viernes') %>%
#   ggplot( aes(x=date, y=value)) +
#   geom_area(fill="#69b3a2", alpha=0.5) +
#   geom_line(color="#69b3a2") +
#   ylab("bitcoin price ($)") +
#   theme_ipsum()


p <- data %>% #filter(finDeSem == 'Lunes a viernes' & diaSem == 'viernes') %>%
  ggplot( aes(x=date, y=value)) +
  # geom_area(fill="#69b3a2", alpha=0.5) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2",size = 0.1) +
  geom_point(aes(color = diaSem),size = 1) + 
  ylab("% tiempo") +
  theme_ipsum()


# Turn it interactive with ggplotly
p <- ggplotly(p)
p
