##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Tidy time series
##########################################################################


list.of.packages <- c("tibbletime", "tidyverse", "ggmap")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tibbletime)
library(tidyverse)
library(ggmap)

( files <- list.files("data/airbnb/", pattern = '*.csv', full.names = T) ) #dame todos los ficheros csv de la carpeta
airbnb <- lapply(files, read_csv) # creame una lista con cada uno de ellos
airbnb <- bind_rows(airbnb) #unemelos en un solo dataframe

airbnb <- airbnb %>% 
  as_tbl_time(last_modified) %>% # le digo que use como ídnice de la dataframe el last_modified
  arrange(last_modified) %>%
  select(last_modified, price, overall_satisfaction, latitude, longitude)

summary(airbnb)

airbnb %>%
  collapse_by(period = "1 year") %>% # collapse cambia los valores del índice de la tabla (last_modified)
  #. Collapse the index of a tbl_time object by time period.
  #The index column is altered so that all dates that fall in a specified interval share a common date
  group_by(last_modified) %>%
  summarise(median_price = median(price, na.rm = T))

#otra opcion
airbnb %>% group_by(lubridate::year(last_modified)) %>% summarize(median_price=median(price))

# Clean up
airbnb %>%
  collapse_by(period = "2 hour", clean = TRUE) %>%
  group_by(last_modified) %>%
  summarise(median_price = median(price)) %>% 
  head

# Start
airbnb %>%
  collapse_by(period = "2 hour", clean = TRUE, side = "start") %>% #el side es para decir si si la frontera es <= ó <. 
  group_by(last_modified) %>%
  summarise(median_price = median(price)) %>% 
  head

airbnb %>%
  collapse_by(period = "2 hour", clean = TRUE, side = "start", start_date = "2014-08-01 15:00:00") %>%
  group_by(last_modified) %>%
  summarise(median_price = median(price)) %>% 
  head


# Viz: ggmap

airbnb_plot <- airbnb %>% 
  drop_na() %>% 
  as_tbl_time(index = last_modified) %>% 
  # Collapse and clean
  collapse_by(period = "hour", clean = TRUE, side = "start") %>%
  # Throw out a few outliers
  filter(between(price, quantile(price, .05), quantile(price, .95))) %>% # hay nuy pocos que son muy caros
  # y la mayoría son del mismo intervalo. Eso me rompe la visuaoización. Quito los muy bajos y los muy altos.
  mutate(price = log10(price)) %>% #lo convierto a logarítimico para que estén más proximos unos de otro. 
  qmplot(longitude, latitude, data = ., geom = "blank") + #esa es la libreía que pinta. Te pide centro de los dato. 
  # Como esta función no etá pensada para pipes, se pude hacer usando ese '.'
  geom_point(aes(color = price), alpha = .2, size = .3) + #digo que el color dependerá del precio. 
  scale_color_continuous(low = "red", high = "blue")

airbnb_plot
