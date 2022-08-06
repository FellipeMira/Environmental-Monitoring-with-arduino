library(arduinor)
library(serial)
library(tidyverse)
library(reticulate)



con <- ar_init("/dev/ttyACM0", baud = 9600)

ar_collect(con,15,timeout = 500)

df4 <-ar_collect(con,
                 size = 10) %>% 
  as_tibble() %>% 
  mutate(plyr::ldply(stringr::str_split(value, pattern =  ","), rbind)) %>% 
  select(c(`1`,`2`, `3`, `4`, `5`)) %>% 
  na.omit()

################################################################################
#sudo chmod a+rw /dev/ttyACM0  

arduino <-  serialConnection(
  port = "/dev/ttyACM0",
  mode = "9600,n,8,1" ,
  buffering = "none",
  newline = TRUE,
  eof = "",
  translation = "cr",
  handshake = "none",
  buffersize = 4096
  
)

summary(s) 
serial::isOpen(s)

serial::read.serialConnection(s)

################################################################################

fnc <- function(x){
  x %>% 
    as_tibble() %>% 
    mutate(plyr::ldply(stringr::str_split(value, pattern =  ","), rbind)) %>% 
    return(.)
}
