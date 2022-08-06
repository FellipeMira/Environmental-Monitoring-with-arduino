library(tidyverse)
library(arduinor)

con <- ar_init("/dev/ttyACM1",
               baud = 9600)

ar_monitor(con)

df_rt <- ar_collect(con,
                 size = 100) %>% 
  as_tibble() %>% 
  mutate(plyr::ldply(stringr::str_split(value, pattern =  ","), rbind)) %>% 
  select(c(`1`,`2`, `3`, `4`, `5`,`6`))

  as_tibble() %>% 
  mutate(plyr::ldply(stringr::str_split(value, pattern =  ","), rbind)) %>% 
  select(c(`1`,`2`, `3`, `4`, `5`,`6`)) -> df
  
ar_plotter(con, c())
write.csv(.,file = "dados.txt")

#ar_plotter(con, c("Time", "Random"))

df
