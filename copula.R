# Copula
# daniel-san

##--------------------

library(copula)
library(subcopem2D)
library(ADGofTest)
library(fitdistrplus)
## Cargar Datos

datos<-read.csv(file=file.choose())
str(datos)

## Los Datos se pueden descargar de https://www.kaggle.com/aniruddhaachar/audio-features/
## X y Y son caracteristicas de canciones que 
## forma parte de varias listas de Reproducción de Spotify. 
## X es la variable "energy" y Y la variable "loudness". Desconozco el instrumento 
## o metodo que se usa para medirlas. Vamos a usarlar como continuas, auque ya adelantado, hay valores repetidos
## Las canciones estan acomodadas por actividades: las relacionadas a dormir, a hacer ejercicio,
## cuando uno esta comiendo, en una fiesta, etc. 
## Solo tome las relacionadas con hacer ejercicio y de fiesta, que son cuando uno esta en moviento 


## Estadistica Descriptiva marginal

op<-par(mfrow=c(1,2))
hist(datos$X,prob=TRUE,col="green",main="Histograma de X")
hist(datos$Y,prob=TRUE,col="green",main="Histograma de Y")
summary(datos$X)
summary(datos$Y)

## Estadistica descriptiva conjunta

matUV<-apply(datos,2,rank)/nrow(datos)
plot(datos,main="datos observados")
plot(matUV,main="pseudo-observaciones")


cor(datos, method = "pearson")[1, 2]    # Pearson
cor(datos, method = "spearman")[1, 2]   # Spearman
cor(datos, method = "kendall")[1, 2]    # Kendall

SC <- subcopemc(datos,display = FALSE)     
SC$depMon                               # Medida de dependencia monotona 
SC$depSup                               # Medida Supremo 

# Visualmente se esperaria  que existiera una dependencia positiva entre X y Y,
# valores grandes de X reportan valores grandes de Y. Efectivamente tanto las medidas de 
# concordancia como la medida supremo y de dependencia monotona son positivas. 

## Ajuste de marginales

## Para X

# Como X se mueve entre 0 y 1, no parece mala idea usar una Beta

fw<-fitdist(datos$X, "beta")
summary(fw)$estimate    # Parametros estimados
plot(fw)                # Graficamos el histograma y le ecimamos la desidad beta con los parametros
                        
# Visualmente queda bien, vamos a realizar la prueba de bondad de ajuste Anderson-Darling 
para1<-as.numeric(summary(fw)$estimate[1])
para2<-as.numeric(summary(fw)$estimate[2])
ad.test(datos$X, pbeta, para1, para2)$p.value   

## Para Y

## Se me ocurre hacer un transformacion creciente para que la copula sea la misma.
## tambien deseo que esta transformacion la lleve al rango (0,1), para usar una beta,
## ya que esta es muy "maleable".

## Sin embargo hay algunos inconvenientes que debemos mencionar.
## En la pagina de donde saque los datos no meciona un itervalo que diga que valores puede
## tomar la variable Y.
## Pero al parecer de 0 no pasa, lo que no me queda claro es si puede tomar valor "muy negativos"
## el minimo de la muestra es: -13.816
## Para "remediar" esta situacion, o mas bien para poder aplicar la idea de la transformacion
## pensemos que el rango de Y es un intervalo de la forma (-n,0)
## con n natural. Tal que n nos de el p-values mas grande de la prueba A-D

### con n = 24 y usando la transformacionn f(y) = (y + n )/n , creo que una beta le "queda bien"

ytras<-(datos$Y+24)/24
fw2<-fitdist(ytras, "beta")
summary(fw2)$estimate    # Parametros estimados
plot(fw2)                # Graficamos el histograma y le ecimamos la desidad beta con los parametros

para11<-as.numeric(summary(fw2)$estimate[1])
para22<-as.numeric(summary(fw2)$estimate[2])
ad.test(ytras, pbeta, para11, para22)$p.value   

## Nota: Trate de ajustarle un distrubucion a los datos originales, pero no le di al clavo muy bien.

     