##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: ggplot2
##########################################################################

library(ggplot2)

# El lenguaje ggplot ------------------------------------------------------

# https://www.rstudio.com/wp-content/uploads/2016/12/ggplot2-cheatsheet-2.1-Spanish.pdf
#ggplot va por caaps. 

ggplot() # creo qun gráfico vacíño. 

(vector_y <- sample(10)) # Variable dependiente
(vector_x <- sample(10)) # Variable independiente

ggplot(mapping = aes(y = vector_y, x = vector_x)) # ase = aesthetic. A partir de ahora pasaremos de poner mapping. 

ggplot(mapping = aes(y = vector_y, x = vector_x))  + geom_point() # aquí estoy diciendo que quiero representarlo con puntos

#el objetivo es llegar a algo parecido a esto: https://www.youtube.com/watch?v=jbkSRLYSoj
# https://www.youtube.com/watch?v=jbkSRLYSojo
library(tidyverse)
library(gapminder)
gapminder
??gapminder #esto busca no solo en las librerías que ya tenga guardadaso, sino en todos mis pauqetes del ordenador.
# más lento, pero útil cuando no sabes en qué paquete estaba esa función que tanto te gustaba y no te apetece googlear., 
?gapminder
summary(gapminder)

glimpse(gapminder) # telo muestrea en vertical y ves los primeros valores. Más cómodo para cuando tenemos muchas columnas, porque
# te lo muestra en vertical 

dim(gapminder)

View(gapminder)


ggplot(gapminder, aes(x = gdpPercap, y = lifeExp))

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() 

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + scale_x_log10() # los muestrospero en logatirmico

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + scale_x_log10() + geom_point(aes(color=continent)) #que me los pinte por contiente

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + scale_x_log10() + geom_point(aes(color=continent)) + geom_smooth() # nos aplica un modelo (GAM, Modelo Aditivo Generalizado)

#aquí aplicamos una regresíon línal como modelo#se?false ses que no me pinte los intervalos de confianza.  En el anterior los pinta en grisecillo. 
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + scale_x_log10() + geom_point(aes(color=continent)) + geom_smooth(lwd=1, se=FALSE, method="lm", col="black")

# aquí al principio ya decimos que el color sea el continente. Como lo hago al principio, ya me hace cada línea por contienente
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent))  + geom_point() + scale_x_log10()  + geom_smooth(se=F, lwd=1)


# Todo junto: otra opción, hacerlo poco a poco e ir guardandolo . Todo esto me recuerda mucho a las pipes. 
p <- ggplot(gapminder, aes(gdpPercap, lifeExp))
p <- p + aes(color = continent) + geom_point() 
p <- p + scale_x_log10() + aes(color = continent) + geom_point() + geom_smooth(se=F, lwd=1)
p

#lo de frame me permite hacer una visualición anumada en función del año. Lo veremos luego. 
gappminder_plot <- ggplot(gapminder) +
  aes(x = gdpPercap, y = lifeExp, colour = continent,
      size = pop, frame = year) + #el tamaño del punto es la población. Le hemos dicho la transparecndia del punto
  geom_point(alpha = 0.4) +
  scale_x_log10()
gappminder_plot

#' Histograma: distribución de una variable cuantitativa
ggplot(gapminder, aes(x = lifeExp)) +
  geom_histogram()

ggplot(gapminder, aes(x = lifeExp)) +
  geom_histogram(binwidth = 1)

#' Añadimos una nueva variable: continent
ggplot(gapminder, aes(x = lifeExp, fill = continent)) +
  geom_histogram()
#en áfrica la distribución es Bimodal. Hay dos picos. Hay como dos grupos de paises que podríamos
#investigar y dividir. NOTAJL: Eso no es así.  

#' Gráfico de frecuencias con líneas
ggplot(gapminder, aes(x = lifeExp, color = continent)) +
  geom_freqpoly()

#' Gráfico de densidad
ggplot(gapminder, aes(x = lifeExp)) + geom_density()

#' Gráfico de densidad con más de una variable
ggplot(gapminder, aes(x = lifeExp, color = continent)) + geom_density()

#' alpha 
ggplot(gapminder, aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.2)

#' facets : para dividirlo en distintos grafiquillos
#' facet_wrap y facet_grid te hacen lomismo pero con otro "estilo"
ggplot(gapminder, aes(x = lifeExp)) + geom_histogram() + facet_wrap(~ continent) 

ggplot(gapminder, aes(x = lifeExp)) + geom_density() + facet_wrap(~ continent) # subgráficos por (~) continente

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() + 
  scale_x_log10() + 
  geom_point(aes(color=continent)) + 
  facet_wrap(~ continent)

# así cada grafiquillo tiene su propia escala, para que se vea Oceanía mejor 
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  scale_x_log10() +
  geom_point(aes(color=continent)) + 
  facet_wrap(~ continent, scales="free_y") 

# PLOY.LY: gráficos pero dinámicos, en HTML. Verlo,porque es muy potente. https://plot.ly/
install.packages(c("Rcpp", "httpuv", "shiny"))
library(plotly)

gg <- ggplot(diamonds, aes(carat)) +
  geom_histogram()

p <- ggplotly(gg)

p

ggplot(gapminder,
       aes(x = lifeExp, fill = continent)) + geom_histogram() +
  facet_grid(continent ~ .)

#' boxplot de una variable cuantitativa y una discreta
#' ojo: year no es un factor
ggplot(gapminder, aes(x = year, y = lifeExp)) + geom_boxplot()

#' podemos solucinarlo
ggplot(gapminder, aes(x = year, y = lifeExp)) + geom_boxplot(aes(group = year))


#' geom_violin()
ggplot(gapminder, aes(x = year, y = lifeExp)) +
  geom_violin(aes(group = year)) +
  geom_jitter(alpha = 1/4) +
  geom_smooth(se = FALSE)

#' stripplots: para una variable cualitativa y una cuantitativa
ggplot(gapminder, aes(x = continent, y = lifeExp)) + geom_point() #aquí no vemos bien los puntos, porqueestán unos porencima de oros

#' geom_jitter
ggplot(gapminder, aes(x = continent, y = lifeExp)) + geom_jitter() # a cada punto le añade una separación en X aletoria para poder verlos bien

ggplot(gapminder, aes(x = continent, y = lifeExp)) + 
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4)

#' boxplot
ggplot(gapminder, aes(x = continent, y = lifeExp)) + geom_boxplot()

#' raw data AND boxplots
ggplot(gapminder, aes(x = continent, y = lifeExp)) +
  geom_boxplot(outlier.colour = "hotpink") +
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4)

#' superposición de información estadística
ggplot(gapminder, aes(x = continent, y = lifeExp)) + 
  geom_jitter(position = position_jitter(width = 0.1), alpha = 1/4) +
  stat_summary(fun.y = median, colour = "red", geom = "point", size = 5) # le digo que la mediana me lo use para los puntos rojos. 

#' podemos mejorarlo reordenando el eje. No es necesario reordenar los datos del dataset. 
ggplot(gapminder, aes(reorder(x = continent, lifeExp), y = lifeExp)) +  # para ordenaar los continenes por esperanza de vida. 
  geom_jitter(position = position_jitter(width = 0.1), alpha = 1/4) +
  stat_summary(fun.y = median, colour = "red", geom = "point", size = 5)


# themes ------------------------------------------------------------------


gappminder_plot + theme_grey()

gappminder_plot + theme_bw()

gappminder_plot + theme_minimal()

#Crear nuestros propios temas
gappminder_plot + theme(panel.border=element_rect(color = 'white', fill = NA), 
                        panel.background = element_rect(color = 'white', fill = NA) )

#aquí estamos combinando dos temás más uno personalizado. 
gappminder_plot + theme_grey()  + theme_minimal() + theme(
  axis.title.x = element_text(size = 13), 
  text = element_text(family="Arial", colour="grey50", size=12),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),  
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
) 

# Con ggthemes: library("ggthemes") : aquí hay temas personalizado s que la gente ha hecho. 
library("ggthemes") 
gappminder_plot + theme_excel() + scale_fill_excel()

gappminder_plot + theme_economist() + scale_fill_economist()

#estilo de wall street journal
gappminder_plot + theme_wsj() + scale_fill_wsj(palette = "black_green")

#tufte si te gustan los minimalista 
(gappminder_plot <- gappminder_plot + theme_tufte() + scale_fill_tableau())



# labels and legends ------------------------------------------------------

(gappminder_plot <- gappminder_plot + ylab("Esperanza de vida") + xlab("PIB per cápita"))

gappminder_plot <- gappminder_plot + labs(title="Esperanza de vida y PIB per cápita", 
                                          subtitle= "Fuente: Gapminder", 
                                          caption = "Visualización: R + ggplot2")
gappminder_plot + theme(legend.position="bottom")


# guardar un gráfico ------------------------------------------------------
ggsave("gapminder.pdf", last_plot()) # el último que se haya generado. 
ggsave("gapminder.pdf", gappminder_plot)
ggsave("gapminder.png", width = 6, height = 4) #por defecto te coge el último, pero se puede elegir uno en concreto tb





# animations --------------------------------------------------------------

# brew install imagemagick
# Source: https://github.com/dgrtwo/gganimate
# install.packages("cowplot")  # a gganimate dependency
# devtools::install_github("dgrtwo/gganimate")
library(devtools)
install.packages("cowplot")
usethis::browse_github_pat()
usethis::edit_r_environ()
devtools::install_github("dgrtwo/gganimate")
library(gganimate)
theme_set(theme_minimal())  # pre-set the bw theme.

g <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  facet_wrap(~continent, scales = "free") +
  scale_x_log10()  # convert to log scale

gganimate(g, interval=0.2)


g2 <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color=continent, frame = year)) +
  geom_point(show.legend = F) +
  scale_x_log10()  + # convert to log scale
  theme_minimal() +
  labs(title = "ffff") + 
  theme_tq() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Life expectancy, GDP per capita and population by country from 1952 to", 
       subtitle = "Color by continent and size by population",
       ylab="Life expectancy",
       xlab="GDP per capita")
  

gganimate(g2, interval=0.4, ani.width=800, ani.height=600)


# para labels: ver  https://github.com/slowkow/ggrepel




# ggpairs ------------------------------------------------------------------
library(tidyverse)
library(GGally)
gapminder %>% 
  filter(country %in% c("Spain", "Brazil", "Angola")) %>% 
  mutate(country = as.character(country)) %>% 
  ggpairs(mapping = aes(colour = country))


# googleVis ---------------------------------------------------------------

library(googleVis)
gg <- gvisMotionChart(gapminder, idvar='country', timevar='year', colorvar ='continent', xvar = 'gdpPercap', yvar = 'lifeExp', sizevar='pop')
plot(gg) # Necesita Flash


