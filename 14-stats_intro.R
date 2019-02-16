##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Estadística y Modelado
##########################################################################



# Medidas de centralidad

cars$speed

x <- cars$speed
n <- length(x)

sum(x) / n
mean(x) # Media

# La mediana es la observación numérica que divide los datos en dos partes iguales, de manera que una mitad queda bajo la mediana y la otra, por encima. Si tenemos un número impar de observaciones, la mediana es la observación central una vez ordenadas todas ellas. Si disponemos de un número par de observaciones, la mediana es el promedio de las dos observaciones centrales

# lo pngemos entre paréntesis para que se imprima directamente
(mediana <- median(x))

# Moda: La moda es el valor o categoría más frecuente

(velocidades <- unique(x)) 
velocidades[which.max(tabulate(match(x, velocidades)))]

# Medidas de dispersión

# La varianza es un estadístico que mide la dispersión de una distribución de frecuencias. 
# Específicamente, mide la dispersión de los datos respecto a su media.
sum((x-mean(x))^2)/((n-1))
var(x)

# La desviación estándar o típica es un estadístico que mide la dispersión de una distribuciónde frecuencias respecto a su media. 
# Es, concretamente, la raíz cuadrada de la varianza. 
# Supera la limitación de la varianza de venir expresada en las unidades de la variable al cuadrado. 
# Así, la desviación estándar viene medida en las unidades de la variable.

sqrt(sum((x - mean(x))^2) / (n - 1))
sqrt(var(x))
(desv_est <- sd(x))

# Rango o Recorrido: El recorrido o rango de una distribución de frecuencias es un estadístico que mide la dispersión de una distribución de frecuencias. 
# Concretamente, es la diferencia entre el valor máximo y el valor mínimo. 
# Cuanto mayor es el recorrido de una distribución de frecuencias, más dispersa es esta distribución.

(rango <- max(x) - min(x))
diff(range(x))

# El coeficiente de variación de Pearson es una medida de la dispersión relativa de una distribución de frecuencias. 
# Concretamente se define como el cociente entre la desviación estándar y la media de los datos. 
# Cuanto mayor es este coeficiente, menos representativa es la media (de la distribución) .

(pearson <- desv_est / mean(x))

## Modelos lineales
#dis es la distancia de frenado. 
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed") 

# Correlación: indicasi hay una correlación lineal entre ambas variables 
cor(cars$speed, cars$dist) 

modelo <- lm(dist ~ speed, data=cars) # dist es la variable dependiente y spped la variable(s) indpenetiente(s)

plot(x= cars$speed, y = cars$dist, main = "Cars", sub = "Gráfico de dispersión", bty="n", cex=0.5, cex.axis=0.6, pch=19, xlab="Velocidad", ylab="Distancia de frenado")
abline( modelo, col="red")

 "Hay modelos descriptivos y modelos predictvos. En Machine Learning, querremos usar predictivos. 
   Algunos llegan a unos valores con coeficientes (ej. Regresión lineal)
Otros son cajas negras (ej. Redes neuronales).
Si queremos hacerun modelo descriptivo o estadístivo NO nos interesarán modelos de caja negra.
Si tenemos que esplicarlo, siempre cuanto más descriptivo y simple el modelo,mejor. "
summary(modelo)


"
Residuals:
    Min      1Q  Median      3Q     Max 
-29.069  -9.525  -2.272   9.215  43.201 

Los errores no están centrados, lo cual nonos interesa. 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -17.5791     6.7584  -2.601   0.0123 *  
speed         3.9324     0.4155   9.464 1.49e-12 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 15.38 on 48 degrees of freedom
Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 . Porcentaje de punto s en los que el modelo está bien
F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12

"

coefficients(modelo)

# Predicción
nuevos_datos <- data.frame(speed = 17)

predict(modelo, nuevos_datos)

predict(modelo, data.frame(speed = 12:17))
# ¿Es válido el modelo para predecir?

"aquí vamos a deicir el set de datos en datos de entrenamiento y datos de validación. "
"nos explicarán estrategias como validación cruzada, aquí vamos a ver lo básico"

set.seed(100) # semilla que se pone para que la aletaoriedad sea reproducible. Es ecir, si lo ejecut0 100 veces, lo hará igual siempre. 

#recuerda sample(listaDeEntrada, numeroDeElementosQueQueremosSacar)
filas_aletorias <- sample(1:nrow(cars), 0.8*nrow(cars))  #cojo el 80% de LOS ÍNDICES de las filas de forma aleatoria

datos_entrenamiento <- cars[filas_aletorias, ]  

datos_validacion  <- cars[-filas_aletorias, ]   

modelo <- lm(dist ~ speed, data=datos_entrenamiento) 

summary (modelo)

predicciones <- predict(modelo, datos_validacion) 

#creamos un dataframe con dos columnas:una la distancia estimada y otro la destiancia real 
distancias <- data.frame(cbind(distancia_real=datos_validacion$dist, distancia_estimada=predicciones)) 

distancias


# Comparación de muestras para ver si tienen medias diferentes

# Pregunta: ¿Influye el tipo de transmisión en el consumo de un coche?

library(dplyr)
library(ggplot2)

#mpg: Millas por galón de combustible. 
#hp: Potencia del coche. 
#am: si es manual (1) o automático (0)
mtcars %>% ggplot(aes(x=hp, y=mpg)) + geom_point() +geom_smooth(method = 'lm', se = F)

mtcars %>% ggplot(aes(x=hp, y=mpg, color=factor(am))) + geom_point() +geom_smooth(method = 'lm', se = F)

mtcars$mpg
mtcars$am

ggplot(mtcars, aes(x=factor(am), y=mpg)) + 
  geom_boxplot() + 
  labs(title="Comparación de medias", 
       subtitle= "Parece que los coches automáticos consumen más que los manuales. ¿Es una casualidad?", 
       caption = "Visualización: R + ggplot2")

# Forulamos dos hipótesis 
# H0: Medias iguales
# H1: Medias no iguales

(resultados_test <- t.test(mpg ~ am, data=mtcars))
resultados_test$p.value

# p-value: probabilidad de que la diferencia aparente entre ambas medias se deba a la casualidad
# pValue < 0.05,  H0 No es cierto. Rechazamos H0. Si el p_value es < 0.05 rechazamos la hipótesis. 


# Correlación

ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

mtcars$mpg
mtcars$wt

cor.test(mtcars$mpg, mtcars$wt)

matriz_correlacion <- cor(mtcars)
matriz_correlacion

# install.packages("corrplot")
library(corrplot)
#para ver correlación de variables entre sí, modo partidosde fútbol
corrplot(matriz_correlacion, method="color")
corrplot(matriz_correlacion, method="number") 

#----------------------------------------------------------------------------
# Modelado estadístico
#----------------------------------------------------------------------------

# https://docs.google.com/presentation/d/1sQIiz9klQLdrsI7vLWa17TFU441WSu8cHCtzhXA6sEU/pub?start=false&loop=false&delayms=60000

#----------------------------------------------------------------------------
# Modelos de regresión
#----------------------------------------------------------------------------

plot(mtcars)

#wt: Weight (1000 lbs)
modelo.1 <- lm(mpg ~ wt, mtcars)
summary(modelo.1)
predict(modelo.1)

coef(summary(modelo.1))

coche_nuevo = data.frame(wt=4.5)
predict(modelo.1, coche_nuevo)
# 37.2851 + (-5.3445) * 4.5


ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_smooth(method="lm")

# Modelo de regresión múltiple
#cyl = número de cilindros
#disp cilindrada
ggplot(mtcars, aes(x=wt, y=mpg, col=cyl, size=disp)) + geom_point()

modelo.2 = lm(mpg ~ wt + disp + cyl, data=mtcars)
summary(modelo.2)
"
Residuals:
  Min      1Q  Median      3Q     Max 
-4.4035 -1.4028 -0.4955  1.3387  6.0722 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 41.107678   2.842426  14.462 1.62e-14 ***
  wt          -3.635677   1.040138  -3.495  0.00160 ** 
  disp         0.007473   0.011845   0.631  0.53322    -> Esto nos indica que el disp lo podríamos quitar del modelo.  
cyl         -1.784944   0.607110  -2.940  0.00651 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.595 on 28 degrees of freedom
Multiple R-squared:  0.8326,	Adjusted R-squared:  0.8147 -> Mjoer usar el adjusted que el munltiple, porque el adjusted 
penaliza modelos con muchas variables, que es lo correcto, un modeloes mejor cuanto menos variables tiene. 
F-statistic: 46.42 on 3 and 28 DF,  p-value: 5.399e-11"

coef(summary(modelo.2))

predict(modelo.2)

coche_nuevo = data.frame(wt=4.5, disp=300, cyl=8)
coche_nuevo

predict(modelo.2, coche_nuevo)

# Comparamos ambos modelos
anova(modelo.1, modelo.2)

"
Analysis of Variance Table

Model 1: mpg ~ wt
Model 2: mpg ~ wt + disp + cyl
  Res.Df    RSS Df Sum of Sq     F  Pr(>F)   
1     30 278.32                              
2     28 188.49  2     89.83 6.672 0.00427 **   Nos vale que es mejor el 2. 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
"

# Es válido este modelo para predecir?

set.seed(100) 

filas_aletorias <- sample(1:nrow(mtcars), 0.8*nrow(mtcars))  

datos_entrenamiento <- mtcars[filas_aletorias, ]  

datos_validacion  <- mtcars[-filas_aletorias, ]   

modelo.3 <- lm(mpg ~ wt + disp + cyl, data=datos_entrenamiento) 

summary (modelo.3)

predicciones <- predict(modelo.3, datos_validacion) 

consumos <- data.frame(cbind(consumo_real=datos_validacion$mpg, consumo_estimado=predicciones)) 

consumos

#Mediomos el RMSE: https://en.wikipedia.org/wiki/Root-mean-square_deviation
# Es la media de lo que se alejan predecidos y de reales. 
rmse <- function(valores_predecidos, valores_observados){
  sqrt(mean((valores_predecidos - valores_observados)^2))
}

rmse(consumos$consumo_estimado, consumos$consumo_real)


# Ejercicio 1: Repite el modelo incluyendo la variable "am": transmisión y comprueba si esta afecta al consumo
modelo.4 <- lm(mpg ~ wt +  cyl + am, data=datos_entrenamiento)

summary (modelo.4)

predicciones <- predict(modelo.4, datos_validacion) 

consumos <- data.frame(cbind(consumo_real=datos_validacion$mpg, consumo_estimado=predicciones)) 

consumos


rmse(consumos$consumo_estimado, consumos$consumo_real)

# Errores del modelo
fitted(modelo.3) # lo que estima mi modelo con los datos de entrenamiento 
residuals(modelo.3) # errores que ha cometido sobre las dtatos de entrenamiento. 
plot(fitted(modelo.3), resid(modelo.3)) #error cometido
abline(h = 0)


# Ejercicio 2: Encuenta el modelo de regresión lineal que mejor se ajuste a los siguientes datos:

ventas <- c(10,12,11,13,12,14,16,12,14,11,10,19,8.5,8,9,13,16,18,20,22)
tv <- c(13,14,15,17,17.5,13,14.5,9,8,9,8,10,17,18,18.5,19,20,20,13,14)
radio <-c(56,55,60,65,69,67,68,67,97,66,65,60,70,110,75,80,85,90,56,55)
online <- c(40,40,42,50,40,44,40,44,46,46,45,110,30,50,45,40,80,90,90, 110)

dataframe = tibble(ventas,tv,radio,online)

plot(dataframe)

matriz_correlacion <- cor(dataframe)
matriz_correlacion

corrplot(matriz_correlacion, method="number") 

set.seed(100) 

filas_aletorias <- sample(1:nrow(dataframe), 0.8*nrow(dataframe))  

datos_entrenamiento <- dataframe[filas_aletorias, ]  

datos_validacion  <- dataframe[-filas_aletorias, ]   

modeloVentas <- lm(ventas ~  online  , data=datos_entrenamiento) 

summary (modeloVentas)

predicciones <- predict(modeloVentas, datos_validacion) 

ventas <- data.frame(cbind(ventas_reales=datos_validacion$ventas, ventas_estimadas=predicciones)) 

ventas

rmse(ventas$ventas_estimadas, ventas$ventas_reales)

#----------------------------------------------------------------------------
# Modelos de clasificación
#----------------------------------------------------------------------------

# https://docs.google.com/presentation/d/14ac22V-8Y-69JzBW8FGwxJGqpOhg00pLmdr86cAWFF8/edit?usp=sharing

# Regresión logística
filas_aletorias <- sample(1:nrow(mtcars), 0.8*nrow(mtcars))  

datos_entrenamiento <- mtcars[filas_aletorias, ]  

datos_validacion  <- mtcars[-filas_aletorias, ]   

modelo.4 = glm(formula = am ~ hp + wt,  data=datos_entrenamiento, family=binomial)
summary (modelo.4)

coche_nuevo = data.frame(hp=120, wt=2.8)
coche_nuevo

predict(modelo.4, coche_nuevo, type="response")  # me devuelve un % de probabilidad de que sea manual

"Elegir un umbral es ya cosa de negocio. Puedes ayudarte de loa gain charst, que son unas gráficas con una serie
por cada umbral : https://www.saedsayad.com/model_evaluation_c.htm"


datos_validacion$pred <- round(predict(modelo.4, datos_validacion, type = "response") ,2)

datos_validacion[, c('am', 'pred')]

library(pROC) 3#esta librería es de curvas ROC que nor permiten saber si estamos ajustando bien. 
"ver https://es.wikipedia.org/wiki/Curva_ROC#/media/File:Curvas.png"
modelo.4.roc <- pROC::roc(datos_validacion$am, datos_validacion$pred)
plot(modelo.4.roc)
modelo.4.roc$auc


datos_validacion$pred
pred.logit <- rep(0,length(datos_validacion$pred))
pred.logit[datos_validacion$pred>=0.9] <- 1
pred.logit

modelo.4.roc.ajustado <- pROC::roc(datos_validacion$am, pred.logit)
plot(modelo.4.roc.ajustado)


# install.packages("randomForest")
library(randomForest)

table(mtcars$am) # table es una tabla de frecuencias.m
table(mtcars$am)/nrow(mtcars) # por porcentaje
table(mtcars$am)

#Evaluo que cuando divido los datos, el sampling tiene las mismas proporciones.m
# Por ejemplo, si queremos hacer un modelo de gente que no paga hipotecas,no puedo coger
# los primeros 1000 clientes, porque a lo mejor solo hay 2 queno pagna.Es necesario sacar datos.
# Ua teécnica es el oversamplig, que generamos datos de pega sobre los ya existentes. 
table(datos_entrenamiento$am)/nrow(datos_entrenamiento)
table(datos_entrenamiento$am)

set.seed(123)

datos_entrenamiento$am <- as.factor(datos_entrenamiento$am) # random forest necesita que sea categórica. 
levels(datos_entrenamiento$am)
datos_validacion$am <- as.factor(datos_validacion$am)

modelo.5 <- randomForest(am ~ .,data=datos_entrenamiento, ntree=20, importance=TRUE) # am en función de todo (.)
# son varios árboles de decisión lanzados a la vez y cuyos datos se combinana, de ahí lo de forest.
# En este caso, nuestro bosque es de 20 árboles. 
# Es potente, pero lento. 
#importance=TRUE es para que nos infoeme en el resultado de la importancia de cada variable. Despuéslomostraremos con varImpPlot
datos_validacion$pred_rf <- predict(modelo.5, datos_validacion)

datos_validacion[, c('am', 'pred', 'pred_rf')]

modelo.5.roc <- pROC::roc(as.numeric(datos_validacion$am), as.numeric(datos_validacion$pred_rf))
plot(modelo.5.roc)
modelo.5.roc$auc

 install.packages('caret') "Nos permite aplicar infinidad de modelos y algoritmos adeás cambiando entre uno y otro fácilmente. "
 # En este caso sólo lo vamos a usar para que nos dé la matriz de confuación. (verdaderos postiovs, verdaderos negativos, etc. )
 library(caret)
caret::confusionMatrix(datos_validacion$pred_rf, datos_validacion$am)

caret::confusionMatrix(data=datos_validacion$pred_rf,
                       reference=datos_validacion$am,
                       positive='1')

varImpPlot(modelo.5,
           sort = T,
           main="Feature Importance",
           n.var=5)


# Árbol de decisión
 install.packages('party')
library (party)

modelo.6 <- ctree (am ~ ., data = datos_entrenamiento)  

modelo.6

plot (modelo.6, main="Árbol de decisión") 

"Aquí vemos que la que pesa es Gear. Tamibién lo veiamos antes. "

# Ejercicio: Evaluar el resultado de este modelo en el conjunto de datos de validación
# ...


valores_estimados <- as.character (predict(modelo.6, datos_validacion)) 

valores_actuales <- as.character (datos_validacion$am) 

# % error de clasificación
mean (valores_estimados != valores_actuales) 



#----------------------------------------------------------------------------
# Ejercicio de clasificación y árboles
#----------------------------------------------------------------------------
#https://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)

table(good_bad)

# datos_entrenamiento<-subset(datos_entrenamiento, select=-default)
datos_entrenamiento <- datos_entrenamiento %>% dplyr::select(-(default))

modelo_scoring <- glm(good_bad~.,data=datos_entrenamiento,family=binomial())
summary(modelo_scoring)

datos_validacion$score <- predict(modelo_scoring,type='response',datos_validacion)

datos_validacion[, c('default', 'score')]

modelo_scoring.roc <- pROC::roc(datos_validacion$default, datos_validacion$score)
plot(modelo_scoring.roc)
modelo_scoring.roc$auc


# Árbol -------------------------------------------------------------------

library(rpart)
modelo_scoring.2 <- rpart(good_bad~.,data=datos_entrenamiento)

plot(modelo_scoring.2)
text(modelo_scoring.2)

datos_validacion$score_rpart <- predict(modelo_scoring.2,type='vector', newdata = datos_validacion)

datos_validacion[, c('default', 'score_rpart')]

modelo_scoring.2.roc <- pROC::roc(datos_validacion$default, datos_validacion$score_rpart)
plot(modelo_scoring.2.roc)
modelo_scoring.2.roc$auc


install.packages("FFTrees")
library("FFTrees")

datos_entrenamiento$default <- good_bad
datos_validacion <- credit[-filas_aletorias,]
modelo_scoring.3 <- FFTrees(default ~ .,data=datos_entrenamiento,data.test = datos_validacion)
modelo_scoring.3$auc

plot(modelo_scoring.3,
     main = "German Credit Data",decision.labels = c("Good", "Bad"))

plot(modelo_scoring.3,
     main = "German Credit Data",decision.labels = c("Good", "Bad"), tree = 7)


#----------------------------------------------------------------------------
# Agrupamiento
#----------------------------------------------------------------------------

# Agrupamiento jerárquico

# Preparación de los datos
# https://en.wikipedia.org/wiki/Standard_score

mpg <- mtcars$mpg
stripchart(mpg)

mpg_escalado <- (mtcars$mpg - mean(mtcars$mpg)) / sd(mtcars$mpg)
stripchart(mpg_escalado)

mtcars.escalado <- scale(mtcars)
head(mtcars)
head(mtcars.escalado)

distancias <- dist(mtcars.escalado)

cluster_jerarquico <- hclust(distancias)
plot(cluster_jerarquico)

rect.hclust(cluster_jerarquico, k=5, border="red")

grupos <- cutree(cluster_jerarquico, 5)

mtcars$grupo <- grupos
table(mtcars$grupo)

resumen <- mtcars %>% group_by(grupo) %>% summarise_all(mean) %>% mutate(num_coches = table(mtcars$grupo))
resumen


# K-means
# https://github.com/joseramoncajide/master_data_science_capstone
# https://es.wikipedia.org/wiki/K-means


# ¿Cuántos grupos?
wss <- (nrow(mtcars.escalado)-1)*sum(apply(mtcars.escalado,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mtcars.escalado, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Número de cluster",
     ylab="Error")

modelo_kmeans <- kmeans(mtcars.escalado, 5) 
mtcars$grupo_kmeans <- modelo_kmeans$cluster

resumen <- mtcars %>% group_by(grupo_kmeans) %>% summarise_all(mean) %>% mutate(num_coches = table(mtcars$grupo_kmeans))
resumen

library(cluster) 
clusplot(mtcars, mtcars$grupo_kmeans, color=TRUE, shade=TRUE, labels=2, lines=0, main = "Agrupación")

# Reducción de la dimensionalidad

componentes_principales <- prcomp(mtcars.escalado, center=F, scale=F,retx=T)
summary(componentes_principales)

# Loadings o ejes principales
componentes_principales$rotation

plot(componentes_principales)
biplot(componentes_principales)


