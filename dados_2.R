################################################################################

pontos_prontos <- function(){
  
  #5482989767:AAF1eTWrZSmFxrr8KtAGnpsnsEVFT4cfYoo
  library(tidyverse)
  library(telegram.bot)
  library(arduinor)
  
  con <- ar_init("/dev/ttyACM0", baud = 9600)
  
  df1 <-read_csv("dados_df/DADOSAMB.csv")
  df2 <-read_csv("dados_df/DADOSAMB2.TXT")
  pontos <- sf::st_read("Pontos_ISA_MIR-20220803T215152Z-001/Pontos_ISA_MIR/MIRA/Pontos_MIRA_ISA.shp")
  pontos %>% mutate(ID_sensor = 1:length(pontos$Id)) -> pontos
  
  df3 <- rbind(df1,df2)
  
  df3 %>% 
    mutate(ID_sensor = 4) %>% 
    group_by(ID_sensor) %>% 
    summarise_at(vars(umidade_solo:temperatura), mean, na.rm = TRUE) %>% 
    summarise_at(vars(umidade_solo:temperatura), round,digits=2) %>% 
    mutate(ID_sensor = 4) %>% 
    as_tibble()  -> df3
  
  
  ################################################################################
  df1 <- read_csv("dados_df/DADOSAMB.csv")
  df1 %>% 
    mutate(ID_sensor = 2) %>% 
    group_by(ID_sensor) %>% 
    summarise_at(vars(umidade_solo:temperatura), mean, na.rm = TRUE) %>% 
    summarise_at(vars(umidade_solo:temperatura), round,digits=2) %>% 
    as_tibble() %>% 
    mutate(ID_sensor = 2) -> df1
  
  
  ################################################################################
  
  df2 <-read_csv("dados_df/DADOSAMB2.TXT")
  
  df2 %>% 
    mutate(ID_sensor = 3) %>% 
    group_by(ID_sensor) %>% 
    summarise_at(vars(umidade_solo:temperatura), mean, na.rm = TRUE) %>% 
    summarise_at(vars(umidade_solo:temperatura), round,digits=2) %>%  
    as_tibble() %>% 
    mutate(ID_sensor = 3) -> df2
  
  ################################################################################
  
  
  
  
  ###############################################################################
  df4 <-    ar_collect(con, size = 15) %>% 
    
    as_tibble() %>% 
    
    mutate(plyr::ldply(stringr::str_split(value, pattern =  ","), rbind)) %>% 
    
    mutate(ID_sensor = 1) %>% 
    
    dplyr::select(c(`1`, `3`, `4`, `5`,"ID_sensor")) %>% 
    
    mutate(`3` = as.numeric(`3`), 
           `4` = as.numeric(`4`),
           `5` = as.numeric(`5`),
           ID_sensor = as_factor(ID_sensor)) %>% 
    
    group_by("ID_sensor") %>% 
    
    summarise_at(vars(`3`:`5`), mean, na.rm = TRUE) %>% 
    
    mutate(hora=1,
           ID_sensor = 1) %>% 
    
    dplyr::select(`3`, `4`, `5`,"ID_sensor")%>% 
    
    na.omit()
  
  names(df4) <- c("umidade_solo","umidade","temperatura", "ID_sensor" )
  
  df <- rbind(df1,df2,df3,df4)
  
  pontos <- left_join(pontos,
                      df, 
                      by = c("ID_sensor" = "ID_sensor"),
                      c(".x", ".y"))
  return(pontos)
  
}
