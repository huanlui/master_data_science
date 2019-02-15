##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Data wrangling with dplyr
##########################################################################

list.of.packages <- c("R.utils", "tidyverse", "doParallel", "foreach", "sqldf", "broom", "DBI", "ggplot2", "tidyr", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(dplyr)
flights <- readr::read_csv('downloads/2008.csv')

flights %>% dim

# DPLYR -------------------------------------------------------------------

# Identify the most important data manipulation tools needed for data analysis and make them easy to use in R.
# Provide blazing fast performance for in-memory data by writing key pieces of code in C++.
# Use the same code interface to work with data no matter where it’s stored, whether in a data frame, a data table or database.

"Importante:"
# The 5 verbs of dplyr
# select – removes columns from a dataset
# filter – removes rows from a dataset
# arrange – reorders rows in a dataset
# mutate – uses the data to build new columns and values
# summarize – calculates summary statistics. Agregación. 

library(dplyr)
"Es la librería más usada y con mucha funcionalidad. Pertenece a Tidyverse, por lo que
si importáramos Tydyverse también estaría. 
"

# SELECT() ----------------------------------------------------------------------------
"Para seleccionar VARIABLES. Se suelen llamar así en lugar de COLUMNAS"


flights[c('ActualElapsedTime','ArrDelay','DepDelay')] # base R

#otra forma:
select(flights, ActualElapsedTime, ArrDelay, DepDelay) #es raro, no hay que poner el nombre entre comillas :S

"Un tibble es una forma especial de dataframe de R, que es mejor. Permite, por ejemplo, que dentro de una celda haya datos anidaddos,
como un dataframe dentro de la celda. Además, los tibble a la hora de mostrarlos no te muestran todo, sino que te muestra unas cuantas
cfil as y te dice: ...y 10000000 más"


"PAra convertir una dataframe plana de R a tibble:"
require(readr)  # for read_csv()
airports  <- read_csv('data/airports.csv')
class(airports)
select(airports,iata)
as_data_frame(airports) # deprecated. Él lo usó, pero veo que es mejor as_tibble. 
 as_tibble(airports)
 
as_tibble(iris) # iris es un conjunto de datos de flores ya precargadpo en R. Como cars, que es otro. 
plot(cars)

# Funciones de ayuda


# starts_with(“X”): every VARIABLE name that starts with “X”
# ends_with(“X”): every VARIABLE name that ends with “X”
# contains(“X”): every VARIABLE name that contains “X”
# matches(“X”): every VARIABLE name that matches “X”, where “X” can be a regular expression
# num_range(“x”, 1:5): the variables named x01, x02, x03, x04 and x05
# one_of(x): every VARIABLE name that appears in x, which should be a character vector

select(flights, Origin:Cancelled) #desde la columna Origin a Cancelled
select(flights, -(DepTime:AirTime)) #todas excepto las comprendidas entre DepTime y AirTime
select(flights, UniqueCarrier, FlightNum, contains("Tail"), ends_with("Delay")) # quiero dos columnas concretas  + las que terminan con delay + la que contengan Tail

# MUTATE() ----------------------------------------------------------------------------
"Para construir columnas nuevas"
foo <- mutate(flights, ActualGroundTime = ActualElapsedTime - AirTime)
foo <- mutate(foo, GroundTime = TaxiIn + TaxiOut) "tiempo de vuelo en tiera. "
select(foo, ActualGroundTime, GroundTime)

foo$PRUEBA_JL <- foo$GroundTime / 2
select(foo, PRUEBA_JL, GroundTime)
# Varias operaciones

foo <- mutate(flights, 
              loss = ArrDelay - DepDelay, 
              loss_percent = (loss/DepDelay) * 100 ) # ojo: ver que puedo ya usar la columna anterior. 

##########################################################################
# Exercise: 
# Mutate the data frame so that it includes a new variable that contains the average speed, 
#  avg_speed traveled by the plane for each flight (in mph). 
# Hint: Average speed can be calculated as distance divided by number of hours of travel, and note that AirTime is given in minutes
##########################################################################
mutate(foo, AvgSpeed = Distance / (AirTime  / 60)) %>% select(Distance,AirTime, AvgSpeed)


# FILTER() --------------------------------------------------------------------------

# x < y, TRUE if x is less than y
# x <= y, TRUE if x is less than or equal to y
# x == y, TRUE if x equals y
# x != y, TRUE if x does not equal y
# x >= y, TRUE if x is greater than or equal to y
# x > y, TRUE if x is greater than y
# x %in% c(a, b, c), TRUE if x is in the vector c(a, b, c)

# Print out all flights in hflights that traveled 3000 or more miles
filter(flights, Distance > 3000)

# All flights flown by one of AA or UA
filter(flights, UniqueCarrier %in% c('AA', 'UA'))

# All flights where taxiing took longer than flying
# Taxi-Out Time: The time elapsed between departure from the origin airport gate and wheels off.
# Taxi-In Time: The time elapsed between wheels-on and gate arrival at the destination airport.
filter(flights, TaxiIn + TaxiOut > AirTime)

# Combining tests using boolean operators

# All flights that departed late but arrived ahead of schedule
filter(flights, DepDelay > 0 & ArrDelay < 0)

# All flights that were cancelled after being delayed
filter(flights, Cancelled == 1, DepDelay > 0)

##########################################################################
# Exercise: 
# How many weekend flights to JFK airport flew a distance of more than 1000 miles 
# but had a total taxiing time below 15 minutes?
# 1) Select the flights that had JFK as their destination and assign the result to jfk


# 2) Combine the Year, Month and DayofMonth variables to create a Date column


# 3) Result:


# 4) Delete jfk object to free resources 


# ARRANGE() --------------------------------------------------------------------------
"Ordenación"
# Cancelled
( cancelled <- select(flights, UniqueCarrier, Dest, Cancelled, CancellationCode, DepDelay, ArrDelay) )

( cancelled <- filter(cancelled, Cancelled == 1, !is.na(DepDelay)) )

# Arrange cancelled by departure delays
arrange(cancelled, DepDelay)

# Arrange cancelled so that cancellation reasons are grouped
arrange(cancelled, CancellationCode)

# Arrange cancelled according to carrier and departure delays
arrange(cancelled, UniqueCarrier, DepDelay)

# Arrange cancelled according to carrier and decreasing departure delays
arrange(cancelled, UniqueCarrier, desc(DepDelay))

rm(cancelled)

# Arrange flights by total delay (normal order).
arrange(flights, DepDelay + ArrDelay)

# Keep flights leaving to DFW and arrange according to decreasing AirTime 
arrange(filter(flights, Dest == 'JFK'), desc(AirTime)) # concatenando funciones sin pipes. 



# SUMMARISE() -----------------------------------------------------------------------

# min(x) – minimum value of vector x.
# max(x) – maximum value of vector x.
# mean(x) – mean value of vector x.
# median(x) – median value of vector x.
# quantile(x, p) – pth quantile of vector x.
# sd(x) – standard deviation of vector x.
# var(x) – variance of vector x.
# IQR(x) – Inter Quartile Range (IQR) of vector x.

# Print out a summary with variables min_dist and max_dist
summarize(flights, min_dist = min(Distance), max_dist = max(Distance))

# Remove rows that have NA ArrDelay: temp1
na_array_delay <- filter(flights, !is.na(ArrDelay))

# Generate summary about ArrDelay column of temp1
summarise(na_array_delay, 
          earliest = min(ArrDelay), 
          average = mean(ArrDelay), 
          latest = max(ArrDelay), 
          sd = sd(ArrDelay))

hist(na_array_delay$ArrDelay, breaks = 20)# breaks similar a bins
hist(log(na_array_delay$ArrDelay), breaks = 20)  # a veces me interesa verlo en logarítimo
hist(exp(log(na_array_delay$ArrDelay)), breaks = 20)  # para revertir. Aquí no sale igual porque había números negativos. 

rm(na_array_delay)

# Keep rows that have no NA TaxiIn and no NA TaxiOut: temp2
taxi <- filter(flights, !is.na(TaxiIn), !is.na(TaxiOut))

na.omit(flights) # na.omit quita todas las filas que tengan algún valor nulo. En este caso quita  TODAS. 

##########################################################################
# Exercise: 
# Print the maximum taxiing difference of taxi with summarise()
summarise(taxi, MaxDifference= max(abs(TaxiIn - TaxiOut)))




# dplyr provides several helpful aggregate functions of its own, in addition to the ones that are already defined in R. These include:
# first(x) - The first element of vector x.
# last(x) - The last element of vector x.
# nth(x, n) - The nth element of vector x.
# n() - The number of rows in the data.frame or group of observations that summarise() describes.
# n_distinct(x) - The number of unique values in vector x.




# Filter flights to keep all American Airline flights: aa
aa <- filter(flights, UniqueCarrier == "AA")


##########################################################################
# Exercise: 
# Print out a summary of aa with the following variables:
# n_flights: the total number of flights,
# n_canc: the total number of cancelled flights,
# p_canc: the percentage of cancelled flights,
# avg_delay: the average arrival delay of flights whose delay is not NA.

summarise(aa, 
          TotalFlights= n(),
          NumberOfCancelled = sum(Cancelled), 
          PercentageOfCancelled = 100 *  NumberOfCancelled / TotalFlights, 
          AverageDelay = mean(ArrDelay, na.rm = T)) 


summarise(aa, 
          TotalFlights= n(),
          NumberOfCancelled = sum(Cancelled), 
          PercentageOfCancelled = 100 * mean(Cancelled),  #otra forma, ya que es 0 y 1
          AverageDelay = mean(ArrDelay, na.rm = T)) 

# Next to these dplyr-specific functions, you can also turn a logical test into an aggregating function with sum() or mean(). 
# A logical test returns a vector of TRUE’s and FALSE’s. When you apply sum() or mean() to such a vector, R coerces each TRUE to a 1 and each FALSE to a 0. 
# This allows you to find the total number or proportion of observations that passed the test, respectively

set.seed(1973)
(foo <- sample(1:10, 5, replace=T))
foo > 5
sum(foo > 5) # num. elementos > 5
mean(foo)
mean(foo > 5)

##########################################################################
# Exercise: 
# Print out a summary of aa with the following variables:
# n_security: the total number of cancelled flights by security reasons,
# CancellationCode: reason for cancellation (A = carrier, B = weather, C = NAS, D = security)

aa$CancellationCode
summarise(aa,CancellationsBySecurity = sum(CancellationCode == "D", na.rm=T) )

"Otra forma. Recordando table:"
table(aa$CancellationCode,)
table(aa$CancellationCode, aa$Month)

# CancellationCode == "D" es una lista de booleanos. Si yo los sumo, se tratan cono 1's y 0's


# %>% OPERATOR ----------------------------------------------------------------------

# Piping

mean(c(1, 2, 3, NA), na.rm = TRUE)

# Vs

c(1, 2, 3, NA) %>% mean(na.rm = TRUE)


summarize(filter(mutate(flights, diff = TaxiOut - TaxiIn),!is.na(diff)), avg = mean(diff))
#Para escribir %>%, ponemos Ctrl + Shift + M

# Vs

flights %>%
  mutate(diff=(TaxiOut-TaxiIn)) %>%
  filter(!is.na(diff)) %>%
  summarise(avg=mean(diff))


flights %>%
  filter(Month == 5, DayofMonth == 17, UniqueCarrier %in% c('UA', 'WN', 'AA', 'DL')) %>%
  select(UniqueCarrier, DepDelay, AirTime, Distance) %>%
  arrange(UniqueCarrier) %>%
  mutate(air_time_hours = AirTime / 60)

##########################################################################
# Exercise: 
# Use summarise() to create a summary of flioght with a single variable, n, 
# that counts the number of overnight flights. These flights have an arrival 
# time that is earlier than their departure time. Only include flights that have 
# no NA values for both DepTime and ArrTime in your count.
flights$tim
flights %>% 
  filter(!is.na(ArrTime))%>% 
  filter(!is.na(DepTime)) %>% 
  filter(ArrTime < DepTime) %>% 
  summarise(N = n())




# GROUP_BY() -------------------------------------------------------------------------

flights %>% 
  group_by(UniqueCarrier) %>% 
  summarise(n_flights = n(), 
            n_canc = sum(Cancelled), 
            p_canc = 100*n_canc/n_flights, 
            avg_delay = mean(ArrDelay, na.rm = TRUE)) %>% 
  arrange(avg_delay)


flights %>% 
  group_by(DayOfWeek) %>% 
  summarize(avg_taxi = mean(TaxiIn + TaxiOut, na.rm = TRUE)) %>% 
  arrange(desc(avg_taxi))


# Combine group_by with mutate
rank(c(21, 22, 24, 23))

#Tanto por uno de vuelos retrasados por compañía. 
flights %>% 
  filter(!is.na(ArrDelay)) %>% 
  group_by(UniqueCarrier) %>% 
  summarise(p_delay = sum(ArrDelay >0)/n()) %>% 
  mutate(rank = rank(p_delay)) %>% # rank es para mostrar un 1, 2, 3 etc para mostrar el rankings de compañías por delay
  arrange(rank) 


##########################################################################
# Exercises: 
# 1) In a similar fashion, keep flights that are delayed (ArrDelay > 0 and not NA). 
# Next, create a by-carrier summary with a single variable: avg, the average delay 
# of the delayed flights. Again add a new variable rank to the summary according to 
# avg. Finally, arrange by this rank variable.

flights %>% 
  select(UniqueCarrier, ArrDelay) %>% 
  filter(!is.na(ArrDelay)) %>% 
  filter(ArrDelay > 0) %>% 
  group_by(UniqueCarrier) %>% 
  summarize(AverageDelay = mean(ArrDelay)) %>% 
  mutate(Ranking = rank(AverageDelay)) %>% 
  arrange(AverageDelay)

# 2) How many airplanes only flew to one destination from JFK? 
# The result contains only a single column named nplanes and a single row.

flights %>% 
  filter(Origin == 'JFK') %>% 
  group_by(TailNum) %>% 
  summarize(Destinations = n_distinct(Dest)) %>% 
  filter(Destinations == 1) %>% 
  summarize(Total = n()) 

  
# 3) Find the most visited destination for each carrier
# Your solution should contain four columns:
# UniqueCarrier and Dest, n, how often a carrier visited a particular destination,
# rank, how each destination ranks per carrier. rank should be 1 for every row, 
# as you want to find the most visited destination for each carrier.
flights %>% 
  group_by(UniqueCarrier, Dest) %>% 
  summarize(N = n()) %>% 
  mutate(Rank = rank(desc(N))) %>% 
  #arrange(UniqueCarrier, Rank)
  filter(Rank == 1)





# Other dplyr functions ---------------------------------------------------

# top_n()

flights %>% 
  group_by(UniqueCarrier) %>% 
  top_n(2, ArrDelay) %>%  #las dos conpañías con más ArrDelay
  select(UniqueCarrier,Dest, ArrDelay) %>% 
  arrange(desc(UniqueCarrier))


# mutate_if(is.character, str_to_lower)
# mutate_at
"Cogeme todas las columnas que terminen en delay y a todas ellas le apliques una función.
En el caso de abajo coger el valor (.) y divirlo entre dos. Modifa esas columnas en sí, 
no crea nuevas columnas"

foo <- flights %>% 
  head %>% 
  select(contains("Delay")) %>% 
  mutate_at(vars(ends_with("Delay")), funs(./2)) 
foo

foo %>% 
  mutate_at(vars(ends_with("Delay")), funs(round)) 

rm(foo)

"También hay mutate_if, summarize_at"
# Dealing with outliers ---------------------------------------------------
"Outliers: datos que se salen de lo estándar y me joden la media. "
# ActualElapsedTime: Elapsed Time of Flight, in Minutes
summary(flights$ActualElapsedTime)

hist(flights$ActualElapsedTime)

library(ggplot2)
ggplot(flights) + 
  geom_histogram(aes(x = ActualElapsedTime))

boxplot(flights$ActualElapsedTime,horizontal = TRUE)

#esto me dice cuales son los valores extremos
# outlier = valor extremo = valor atípico: https://es.wikipedia.org/wiki/Valor_at%C3%ADpico
# el boxplot aplica el creiterio de 3 veces la IQR
# con esos valores extremos se suele o quitarlos o asignarloes el valor justo de 3*IQR. 
outliers <- boxplot.stats(flights$ActualElapsedTime)$out
length(outliers)
outliers

no_outliers <- flights %>% 
  filter(!ActualElapsedTime %in% outliers) 

boxplot(no_outliers$ActualElapsedTime,horizontal = TRUE)

mean(no_outliers$ActualElapsedTime, na.rm = T)
hist(no_outliers$ActualElapsedTime)

rm(outliers)
rm(no_outliers)


barplot(table(flights$UniqueCarrier))



# Missing values ----------------------------------------------------------

NA

flights %>% dim

# Removing all NA's from the whole dataset

flights %>% na.omit %>% dim # el problema de esto es que me quita todas las filas que tengan algún na. pierdo todas, de hecho. 
flights %>% filter(complete.cases(.)) %>% dim #otra forma de hacerlo, pero también pierdo todas las filas
library(tidyr) # for drop_na()
flights %>% drop_na() %>% dim # esta es una función de tydyverse

# Removing all NA's from a varible

flights %>% 
  drop_na(ends_with("Delay")) %>% 
  summary()

# Better aproaches
flights %>% 
  filter(is.na(DepTime)) %>% 
  mutate(DepTime = coalesce(DepTime, 0L)) #si es nulo, ponle 0

flights %>% 
  filter(is.na(DepTime)) %>% 
  mutate(DepTime = coalesce(DepTime, CRSDepTime)) #si es 0, ponle ese valor

unique(flights$CancellationCode)
foo <- flights %>% 
  mutate(CancellationCode = na_if(CancellationCode, "")) # si es na, pone ""
unique(foo$CancellationCode)

# CancellationCode: reason for cancellation (A = carrier, B = weather, C = National Air System, D = security)
foo <- flights %>% 
  mutate(CancellationCode = recode(CancellationCode, "A"="Carrier", "B"="Weather", "C"="National Air System", 
                                   .missing="Not available", #missing para el caso de que  sea un na 
                                   .default="Others" )) # es como un decode de SQL, un swtich de programación

unique(foo$CancellationCode)

foo$CancellationCode %>% unique
rm(foo)





# Tidy Data ---------------------------------------------------------------

library(tidyr)

# Wide Vs Long 

# spread
# gather

flights %>% 
  group_by(Origin, Dest) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>%  #recuerda: es ordenar
  spread(Origin, n) %>% # me exapande la columna de origen y la n(la pivoto, wide format)
  gather("Origin", "n", 2:ncol(.)) %>%  #esto deshace lo anterior (lo despivota, a long format)
  arrange(-n) 


##########################################################################
# Run the follow statements step by step and trying to understand what they do

flights %>% 
  group_by(UniqueCarrier, Dest) %>% 
  summarise(n = n()) %>%
  ungroup() %>%  #deshace el group by y lo deja en una dataframe (tibble). Es útil, porque puede que una vez que tienes tu grupo ya no quieras
  # que las funciones se apliquen a los grupos , sino como si fuera una dataframe. 
  group_by(Dest) %>% 
  mutate(total= sum(n), pct=n/total, pct= round(pct,4)) %>% 
  ungroup() %>% 
  select(UniqueCarrier, Dest, pct) %>% 
  spread(UniqueCarrier, pct) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total = rowSums(select(., -1))) 




# unite()
# separate()

##########################################################################
# Run the follow statements step by step and trying to understand what they do

flights %>% 
  head(20) %>% 
  unite("code", UniqueCarrier, TailNum, sep = "-") %>%  #me crea una columna code con la concatenación de UniqueCarrier y TailNum usando el separador -
  select(code) %>% 
  separate(code, c("code1", "code2")) %>%  #para separar. Si no pones separaddor, por defecto usa todo lo que no sean letros o números
  separate(code2, c("code3", "code4"), -3) # separa quiotando los trs últimos caracteres. 


# Dplyr: Joins ------------------------------------------------------------

# inner_join(x, y)  SELECT * FROM x INNER JOIN y USING (z)
# left_join(x, y) SELECT * FROM x LEFT OUTER JOIN y USING (z)
# right_join(x, y, by = "z") SELECT * FROM x RIGHT OUTER JOIN y USING (z)
# full_join(x, y, by = "z") SELECT * FROM x FULL OUTER JOIN y USING (z)

# semi_join(x, y)
# anti_join(x, y)


airlines <- readr::read_csv('data/airlines.csv')
airlines

airports <- readr::read_csv('data/airports.csv')
airports

# Before joing dataframes, check for unique keys
airports %>% 
  count(iata) %>%  # count cuenta en número de casos únicos que hay de una cilumna. 
  filter(n > 1) # filtro para buscar si alguno está repetido. No lo está. 


flights2 <- flights %>% 
  select(Origin, Dest, TailNum, UniqueCarrier, DepDelay) #nos quedamos sólo con algunas columnas

airlines %>% head(10)

# Top delayed flight by airline
flights2 %>% 
  group_by(UniqueCarrier) %>%
  top_n(1, DepDelay) %>% 
  left_join(airlines, by = c("UniqueCarrier" = "Code"))



##########################################################################
# Exercises:
# Join flights2 with airports dataset

flights2 %>% head(10)

airports2 = airports %>% select(iata,city)

flights2 %>% 
  left_join(airports2 %>% rename("OriginCity" ="city"), by = c("Origin" = "iata"))  %>% 
  left_join(airports2 %>% rename("DestinationCity" ="city"), by = c("Dest" = "iata"))

airports %>% head(5)


# Dates with lubridate ----------------------------------------------------

# Base R

as.POSIXct("2013-09-06", format="%Y-%m-%d")
as.POSIXct("2013-09-06 12:30", format="%Y-%m-%d %H:%M")


flights %>% 
  head %>%
  select(Year:DayofMonth,DepTime,ArrTime) %>% 
  separate(DepTime, into = c("Hour", "Minute"), sep = -3, remove = F)

flights %>% 
  head %>%
  select(Year:DayofMonth,DepTime,ArrTime) %>% 
  separate(DepTime, into = c("Hour", "Minute"), sep = -3) %>% 
  mutate(Date = as.Date(paste(Year, Month, DayofMonth, sep = "-")),
         HourMinute = (paste(Hour, Minute, sep = ":")),
         Departure = as.POSIXct(paste(Date, HourMinute), format="%Y-%m-%d %H:%M"))

# Easier with lubridate
library(lubridate)
today()
now()


(datetime <- ymd_hms(now(), tz = "UTC"))
(datetime <- ymd_hms(now(), tz = 'Europe/Madrid'))

Sys.getlocale("LC_TIME")
Sys.getlocale(category = "LC_ALL")

# Available locales: Run this in your shell: locale -a
(datetime <- ymd_hms(now(), tz = 'Europe/Madrid', locale = Sys.getlocale("LC_TIME")))
month(datetime, label = TRUE, locale = 'fi_FI.ISO8859-15')
wday(datetime, label = TRUE, abbr = FALSE, locale = 'fi_FI.ISO8859-15')

year(datetime)
month(datetime)
mday(datetime)

ymd_hm("2013-09-06 12:3")
ymd_hm("2013-09-06 12:03")

# Esto genera un error
flights %>% 
  head %>%
  select(Year:DayofMonth,DepTime,ArrTime) %>% 
  separate(DepTime, into = c("Hour", "Minute"), sep = -3) %>% 
  mutate(dep = make_datetime(Year, Month, DayofMonth, Hour, Minute))

flights %>% 
  head %>%
  select(Year:DayofMonth,DepTime,ArrTime) %>% 
  separate(DepTime, into = c("Hour", "Minute"), sep = -3) %>% 
  mutate_if(is.character, as.integer) %>% 
  mutate(dep_date = make_datetime(Year, Month, DayofMonth) ,
         dep_datetime = make_datetime(Year, Month, DayofMonth, Hour, Minute))

# Let’s do the same thing for each of the four time columns in flights. 
# The times are represented in a slightly odd format, so we use modulus arithmetic to pull out the hour and minute components

# ?Arithmetic
# %/% := integer division
# %% := modulus

departure_times <- flights %>% 
  head(2) %>% 
  select(DepTime) %>% 
  pull()

# Supongamos la hora: 1232
departure_times %/% 100
departure_times %% 100

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights %>% select(TaxiIn, TaxiOut)

flights_dt <- flights %>%  
  filter(!is.na(DepTime), !is.na(ArrTime), !is.na(CRSDepTime), !is.na(CRSArrTime)) %>% 
  mutate(
    dep_time = make_datetime_100(Year, Month, DayofMonth, DepTime),
    arr_time = make_datetime_100(Year, Month, DayofMonth, ArrTime),
    sched_dep_time = make_datetime_100(Year, Month, DayofMonth, CRSDepTime),
    sched_arr_time = make_datetime_100(Year, Month, DayofMonth, CRSArrTime)
  ) %>% 
  select(Origin, Dest, ends_with("_time"))

# distribution of departure times across the year
flights_dt %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400)

# wday()
flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()


# Time periods functions
minutes(10)
days(7)
months(1:6)
weeks(3)

datetime
datetime + days(1)

# Datos incoherentes

flights_dt %>% 
  filter(arr_time < dep_time) %>% 
  select(Origin:arr_time)


flights_dt <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time,
    arr_time_ok = arr_time + days(overnight * 1),
    sched_arr_time_ok = sched_arr_time + days(overnight * 1)
  )

# Check
flights_dt %>% 
  filter(overnight == T)

# Time Zones
ymd_hms("2007-01-01 12:32:00")
str(flights_dt$dep_time)

pb.txt <- "2007-01-01 12:32:00"
# Greenwich Mean Time (GMT)
(pb.date <- as.POSIXct(pb.txt, tz="Europe/London"))
# Pacific Time (PT)
format(pb.date, tz="America/Los_Angeles",usetz=TRUE)
# Con lubridate
with_tz(pb.date, tz="America/Los_Angeles")
# Coordinated Universal Time (UTC)
with_tz(pb.date, tz="UTC") 
