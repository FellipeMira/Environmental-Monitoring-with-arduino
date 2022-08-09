
library(tidyverse)
library(telegram.bot)
library(arduinor)
library(viridis)

pal <- viridis::magma(6)
sjc <- sf::st_read("sjc.shp")
bairros <- sf::st_read("bairros/BAIRROS.shp")

con <- ar_init("/dev/ttyACM0", baud = 9600)

bot <- Bot("5482989767:AAF1eTWrZSmFxrr8KtAGnpsnsEVFT4cfYoo")
updater <- Updater(token = "MY_TOLKEN")
print(bot$getMe())
#Funcao start
start <- function(bot, update)
{
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Olá %s, se estiver recebendo está mensagem, significa o sistema de monitoramento está ativo",
                                 update$message$from$first_name))
}
start_handler <- CommandHandler("start", start)
updater <- updater + start_handler
#Funcao hoje
hoje <- function(bot, update)
{
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("A data de hoje é %s",
                                 format(Sys.Date(), "%d-%b-%Y")))
}
hoje_handler <- CommandHandler("hoje", hoje)
updater <- updater + hoje_handler
#Funcao echo
echo <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id, text = update$message$text)
}
updater <- updater + MessageHandler(echo, MessageFilters$text)
#Funcao histograma de uma normal
historama_normal <- function(bot, update)
{
  png("my_plot.png")
  hist(rnorm(1000))
  dev.off()
  bot$sendPhoto(chat_id = update$message$chat_id, photo = 'my_plot.png')
}
hist_norm_handler <- CommandHandler("hist_norm", historama_normal)
updater <- updater + hist_norm_handler

# função de onde está meu sistema de monitoramento
onde <- function(bot,update) {
  require(tmap)
  require(sf)
  #rm(pontos)
  #sjc <- geobr::read_municipality(code_muni = 3549904)
  #st_write(sjc,dsn = "sjc.shp")
  #bairros <- sf::st_read("bairros/BAIRROS.shp")
  #bairros <- st_simplify(bairros, preserveTopology = FALSE, dTolerance = 1000)
  #Lendo os pontos 
 # pontos <- sf::st_read("Pontos_ISA_MIR-20220803T215152Z-001/Pontos_ISA_MIR/MIRA/Pontos_MIRA_ISA.shp")
  tmap_options(check.and.fix = TRUE)
  #icon = tmap_icons(file = "casa2.png")
  #tmap_mode('plot')
  
  #con <- ar_init("/dev/ttyACM0", baud = 9600)
  pontos <- pontos_prontos()
  #pontos %>% mutate(ID_sensor = 1:length(pontos$Id)) -> pontos
  #############################################################################
  ##############################################################################
  x <- tm_shape(sjc)+
    tm_borders(col="gray3")+
    #tm_shape(bairros)+
    #tm_borders(col="gray")+
    tm_shape(pontos)+
    tm_dots(col = "umidade_solo",
            style = "fisher",
            palette = "RdBu",
            title="Amostra de Umidade do solo",
            size=0.5)+
    tm_layout(legend.width = 3)+
    tm_text("umidade_solo", just="left", xmod=.5, size = 0.7) +
    tm_legend(legend.outside=TRUE)

  tmap_save(x,"idw.png")
  
  bot$sendPhoto(chat_id = update$message$chat_id, photo = 'idw.png')
}

mapping_handler <- CommandHandler("map_umidade_solo", onde)

updater <- updater + mapping_handler

###############################################################################

onde2 <- function(bot,update) {
  require(tmap)
  require(sf)
  #rm(pontos)
  #st_write(sjc,dsn = "sjc.shp")
  #bairros <- st_simplify(bairros, preserveTopology = FALSE, dTolerance = 1000)
  #Lendo os pontos 
  #pontos <- sf::st_read("Pontos_ISA_MIR-20220803T215152Z-001/Pontos_ISA_MIR/MIRA/Pontos_MIRA_ISA.shp")
  tmap_options(check.and.fix = TRUE)
  #icon = tmap_icons(file = "casa2.png")
  #tmap_mode('plot')
  
  #con <- ar_init("/dev/ttyACM", baud = 9600)
  
  #pontos %>% mutate(ID_sensor = 1:length(pontos$Id)) -> pontos
  pontos <- pontos_prontos()
  ##############################################################################
  x2 <- tm_shape(sjc)+
    tm_borders(col="gray3")+
    #tm_shape(bairros)+
    #tm_borders(col="gray")+
    tm_shape(pontos)+
    tm_dots(col = "temperatura",
            style = "fisher",
            palette = "RdBu",
            title="Amostra de Umidade do solo",
            size=0.5)+
    tm_layout(legend.width = 3)+
  #tm_text("umidade_solo", just="left", xmod=.5, size = 0.7) +
    tm_legend(legend.outside=TRUE)
  
  tmap_save(x2,"idw2.png")
  
  bot$sendPhoto(chat_id = update$message$chat_id, photo = 'idw2.png')
}

mapping_handler2 <- CommandHandler("map_temperatura", onde2)

updater <- updater + mapping_handler2

#dados agora
agora <- function(bot, update)
{
  
   df4 <-arduinor::ar_collect(con,
                   size = 20) %>% 
    as_tibble() %>% 
    mutate(plyr::ldply(stringr::str_split(value, pattern =  ","), rbind)) %>% 
    dplyr::select(-c("value")) %>% 
    na.omit() %>%
    slice(1L) 
   
  names(df4) <- c("Hora","Data","Umidade do solo","Umidade DHT","Temperatura")

    bot$sendMessage(chat_id = update$message$chat_id,
                  text =  print(paste0("Hora:",df4$Hora," | ",
                                         "Data:",df4$Data," | ",
                                         "Umidade do solo:",df4$`Umidade do solo`," | ",
                                         "Umidade (DHT) [%]:",df4$`Umidade DHT`," | ",
                                         "Temperatura:", df4$Temperatura)))
                                  
                               
}

agora_handler <- CommandHandler("now", agora)

updater <- updater + agora_handler



# Send document

dash <- function(bot,update){
  bot$sendDocument(chat_id = update$message$chat_id,
                   document = "dash.html")
}

dash_handler <- CommandHandler("dash", dash)

updater <- updater + dash_handler

updater$start_polling()

