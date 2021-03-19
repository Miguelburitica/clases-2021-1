library(moments)
library(RColorBrewer)
library(ggplot2)
library(fdth)
library(stats)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(viridi)

attach(universitarios)

#PUNTO #3
tablaCarreraEstrato = table(carrera, estrato)

#PUNTO #5

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

view(mpg)

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

#PUNTO 9

##Medidas de posición relativa

###Pesp

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













