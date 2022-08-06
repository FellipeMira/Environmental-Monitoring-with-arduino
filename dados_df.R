getwd()
################################################################################

df1 <-read_csv("dados_df/DADOSAMB.csv")
df2 <-read_csv("dados_df/DADOSAMB2.TXT")

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

df4 <-ar_collect(con, size = 10) %>% 
  
  as_tibble() %>% 
  
  mutate(plyr::ldply(stringr::str_split(value, pattern =  ","), rbind)) %>% 
  
  mutate(ID_sensor = 1) %>% 
  
  select(c(`1`, `3`, `4`, `5`,"ID_sensor")) %>% 
  
  mutate(`3` = as.numeric(`3`), 
         `4` = as.numeric(`4`),
         `5` = as.numeric(`5`),
         ID_sensor = as_factor(ID_sensor)) %>% 
  
  group_by("ID_sensor") %>% 
  
  summarise_at(vars(`3`:`5`), mean, na.rm = TRUE) %>% 
  
  mutate(ID_sensor = 1) %>% 
  
  select(`3`, `4`, `5`,"ID_sensor")%>% 
  
  na.omit()

names(df4) <- c("umidade_solo","umidade","temperatura", "ID_sensor" )


################################################################################

df <- rbind(df1,df2,df3,df4)

pontos <- left_join(pontos,
                    df, 
                    by = c("ID_sensor" = "ID_sensor"),
                    c(".x", ".y"))


################################################################################
x <- tm_shape(sjc)+
  tm_borders(col="gray3")+
  tm_shape(bairros)+
  tm_borders(col="gray")+
  tm_shape(pontos)+
  tm_dots(col = "umidade_solo",
          style = "fisher",
          palette = viridis::magma(10),
          title="Amostra de Umidade do solo",
          size=0.5)+
  tm_layout(legend.width = 3)+
  tm_text("umidade_solo", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)
x
