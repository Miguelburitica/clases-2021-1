#Parcial 1. Integrantes: Soreth Lopez, Miguel Buritica, Janira Melgarejo, Pablo Segura.

library(moments)
library(RColorBrewer)
library(ggplot2)
library(fdth)
library(stats)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(plotrix)

attach(universitarios)

#Punto 1
table(estrato,carrera)
w=table(estrato,carrera)
data.frame(table(estrato,carrera))
barplot(w,ylim=c(0,50),
        xlab="Carrera",ylab="N. Estudiantes",col = palette(), main = "Gráfico de Barras Estrato y Carrera", legend.text=c("2","3","4","5","6"))

#Diagrama de Tortas Carrera
table(carrera)
proporcionesCarrera = table(carrera) # creamos un vector con proporciones
etiquetas = c("ADMINISTRACION", "ARQUITECTURA", "BIOQUIMICA", "CIVIL", "ELECTRICA" , "ELECTRONICA" , "INDUSTRIAL" , "INFORMATICA" , "MECANICA" ,"MECATRONICA" , "QUIMICA" , "SISTEMAS" , "TIC ") # vector con etiquetas
  pie3D(proporcionesCarrera,labels=etiquetas,
        explode=0.1,
        main="Distribución Carrera")

  #Diagrama de Tortas Estrato
table(estrato)
ProporcionesEstrato = table(estrato)
  etiquetas = c("2" , "3" , "4" , "5" , "6")
  pie3D(ProporcionesEstrato,labels=etiquetas,
        explode=0.1,
        main="Distribución Estrato")

#Punto 2

tablaCarreraGenero = table(carrera,genero)


#Punto 3

tablaCarreraEstrato = table(carrera, estrato)

#Punto 4

#Diagrama de Barras Edad
table(edad)
barplot(table(edad),ylim=c(0,200),
        xlab="Edades",ylab="Número de estudiantes",col = palette(), main = "Tabla de frecuencias de Edad")

#Diagrama de torta Edad
ProporcionesEdad = table(edad)
etiquetas = c("18", "19", "20", "21", "22" , "23" , "24" , "25")
pie3D(ProporcionesEdad,labels=etiquetas,
      explode=0.1,
      main="Distribución de Edades")


#Punto 5

##Altura

###Histograma

datosAltura = data.frame(value = altura)

histogramaAltura = datosAltura %>%
  filter( altura < 200) %>%
  ggplot( aes( x = altura)) +
  geom_histogram( binwidth = 1.25, fill = "#DDFFDD", color = "#772F96", alpha = 0.9) +
  ggtitle("Rango del Intervalo = 2") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size = 10)
  );histogramaAltura

###BoxPlot

boxPlotAltura = ggplot(universitarios, aes(x = "Estudiantes", y = altura)) +
  geom_boxplot(
    color = "#787FE6" ,
    fill = "#10134A" ,
    alpha = 0.3,
    
    notch = TRUE,
    notchwidth = 0.8,
    
    outlier.color = "#A83E38",
    outlier.fill = "#A83E38",
    outlier.size = 3
  );boxPlotAltura

##Peso

###Histograma

datosPeso = data.frame(value = peso)

histogramaPeso = datosPeso %>%
  filter( peso < 200) %>%
  ggplot( aes( x = peso)) +
  geom_histogram( binwidth = 1.25, fill = "#FFDDDD", color = "#772F96", alpha = 0.9) +
  ggtitle("Rango del Intervalo = 2") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size = 10)
  );histogramaPeso

###BoxPlot

paletaColores = c("#CCA85D", "#FFED69", "#94813D", "#E3AB5D", "#D58F57")

boxPlotPeso = ggplot(universitarios, aes(x = "Estudiantes", y = peso)) +
  geom_boxplot(
    color = "#77E6A1" ,
    fill = "#2F9656" ,
    alpha = 0.3,
    
    notch = TRUE,
    notchwidth = 0.8,
    
    outlier.color = "#4A0B08",
    outlier.fill = "#A83E38",
    outlier.size = 3
  );boxPlotPeso

##Promedio

###Histograma

datosPromedio = data.frame(value = promedio)

histogramaPromedio = datosPromedio %>%
  filter( promedio < 200) %>%
  ggplot( aes( x = promedio)) +
  geom_histogram( binwidth = 1.25, fill = "#DDDDFF", color = "#772F96", alpha = 0.9) +
  ggtitle("Rango del Intervalo = 2") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size = 10)
  );histogramaPromedio

###BoxPlot

boxPlotPromedio = ggplot(universitarios, aes(x = "Estudiantes", y = promedio)) +
  geom_boxplot(
    color = "#A83E38" ,
    fill = "#4A0B08" ,
    alpha = 0.3,
    
    notch = TRUE,
    notchwidth = 0.8,
    
    outlier.color = "#2F9656",
    outlier.fill = "#2F9656",
    outlier.size = 3
  );boxPlotPromedio

#Punto 6

#Medidas de Tendencia Central 
#Media
PromedioP=mean(promedio);PromedioP
paste("Promedio del Promedio",round(PromedioP,1))
#Mediana
MedianaP=median(promedio);MedianaP
paste("Mediana del Promedio",round(MedianaP,1))
#Moda
ModaP=mfv(promedio);ModaP
paste("Moda del Promedio",round(ModaP,1))

#Punto 7

#Rango
max(promedio)
min(promedio)
Rp=((max(promedio))-min(promedio));Rp
paste("Rango del Promedio",round(Rp,1))
#Varianza
Vp=var(promedio);Vp
paste("Varianza del Promedio",round(Vp,1))
#desviacion estandar 
Dep= sd(promedio);Dep
paste("Desviacion Estandar del Promedio",round(Dep,1))
#Coeficiente de Variacion 
Cvp= (Dep)/(Rp)*100
paste("Coeficiente de Variacion del Promedio",round(Cvp,1))

#Punto 8

##Medidas de posición relativa

###Peso

summary(peso)

nDatos = length(peso)

quantile(peso, type = 6)

pesoEnlistada = sort(peso);pesoEnlistada

quantilPeso25 = quantile(pesoEnlistada, 0.25, type = 6);quantilPeso25
quantilPeso75 = quantile(pesoEnlistada, 0.75, type = 6);quantilPeso75
quantilPeso85 = quantile(pesoEnlistada, 0.85, type = 6);quantilPeso85
quantilPeso92 = quantile(pesoEnlistada, 0.92, type = 6);quantilPeso92

posicionEnLista25 = (25*(nDatos+1))/100;posicionEnLista25
posicionEnLista75 = (75*(nDatos+1))/100;posicionEnLista75
posicionEnLista85 = (85*(nDatos+1))/100;posicionEnLista85
posicionEnLista92 = (92*(nDatos+1))/100;posicionEnLista92


###Altura

summary(altura)

quantile(altura, type = 6)

alturaEnlistada = sort(altura);alturaEnlistada

quantilAltura25 = quantile(alturaEnlistada, 0.25, type = 6);quantilAltura25
quantilAltura75 = quantile(alturaEnlistada, 0.75, type = 6);quantilAltura75
quantilAltura85 = quantile(alturaEnlistada, 0.85, type = 6);quantilAltura85
quantilAltura92 = quantile(alturaEnlistada, 0.92, type = 6);quantilAltura92

posicionEnLista25 = (25*(nDatos+1))/100;posicionEnLista25
posicionEnLista75 = (75*(nDatos+1))/100;posicionEnLista75
posicionEnLista85 = (85*(nDatos+1))/100;posicionEnLista85
posicionEnLista92 = (92*(nDatos+1))/100;posicionEnLista92


###Promedio

summary(promedio)

quantile(promedio, type = 6)

promedioEnlistada = sort(promedio);promedioEnlistada

quantilPromedio25 = quantile(promedioEnlistada, 0.25, type = 6);quantilPromedio25
quantilPromedio75 = quantile(promedioEnlistada, 0.75, type = 6);quantilPromedio75
quantilPromedio85 = quantile(promedioEnlistada, 0.85, type = 6);quantilPromedio85
quantilPromedio92 = quantile(promedioEnlistada, 0.92, type = 6);quantilPromedio92

posicionEnLista25 = (25*(nDatos+1))/100;posicionEnLista25
posicionEnLista75 = (75*(nDatos+1))/100;posicionEnLista75
posicionEnLista85 = (85*(nDatos+1))/100;posicionEnLista85
posicionEnLista92 = (92*(nDatos+1))/100;posicionEnLista92


#Punto 9

#Asimetria
Ap=skewness(promedio);Ap
paste("Asimetria de la variable Promedio",round(Ap,2))

#Punto 10

#Apuntamiento 
App=kurtosis(promedio);App
paste("Apuntamiento de la variable Promedio ",round(App,2))
