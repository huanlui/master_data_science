##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Tidyr
##########################################################################

library(tidyverse)
library(readr)
library(tidyr)


gapminder <- read_csv("data/gapminder.csv") #este está en formato long

#r lee csv directamente también de una web.  
gap_wide <- read_csv("https://docs.google.com/spreadsheets/d/1NTXQNoY8V0H_EZ_peFmnH1ZcGlxCPhwl2VmJNpiACMU/pub?output=csv")
#este en formato wide. 
# gap_wide <- read.csv('data/gapminder_wide.csv')
head(gap_wide)

#esto está mal y lo puso a propósitoporque es necesario indicar las columnas a transformar
gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values)
head(gap_long)
tail(gap_long) # conviene ver tail y head para ver si lo ha hecho bien. 

# Debemos indicar las columnas a transformar
gap_long <- gap_wide %>% 
  gather(key   = obstype_year, #nuevo nombre de columna donde meteresmos las títulos de las columnas a pivotar
         value = obs_values, # nuevo nombre de columna donde metermos los valores de las celdas a pivtorar
         3:38)  # ó -1:-2
head(gap_long)
tail(gap_long) 

# Alternativa sin usar índices
gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values,
         dplyr::starts_with('pop'),
         dplyr::starts_with('lifeExp'),
         dplyr::starts_with('gdpPercap'))
head(gap_long)
tail(gap_long)

# ¿Que ocurre con la variable 'obstype_year'?

gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values,
         -continent, -country) %>%
  separate(obstype_year,
           into = c('obs_type','year'), #valores tipo gdpPercap_1952 los separa en dos
           sep="_")
head(gap_long)
tail(gap_long)

# spread()
gap_normal <- gap_long %>% 
  spread(key = obs_type, value=obs_values)

head(gap_normal)
head(gapminder)

#
gapminder$country <- as.factor(gapminder$country) #mejor convertirlo a factor, será másneficiente para filtrado, etc. 
gapminder %>% head(2)
levels(gapminder$country) #para ver los valores distintos  de una categórica
gapminder$country %>% levels #otra forma más elegante. 
