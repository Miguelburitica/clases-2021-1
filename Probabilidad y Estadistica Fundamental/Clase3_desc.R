###########################
#library(stats)
#library(moments)
#library(dplyr)
#Medidas de variabilidad

#* Rango
#* Varianza
#* Desviacioón estándar
#* Coeficiente de variación
#* 
####################
#*Rango

library(stats)
library(moments)
library(dplyr)

attach(Libro1)

names(Libro1)

xbarra_gastmen = mean(gastmen) #mean() es para media

paste("Promedio de gastos mensuales es: $", round(xbarra_gastmen, 1))

mediana_gastmen = median(gastmen) #median() es para mediana

paste("Mediana de gastos mensuales es: $", round(mediana_gastmen, 1))

moda_gastmen = mfv(gastmen)

paste("Moda de gastos mensuales es: $", round(moda_gastmen, 1))

max(gastmen)# dato mayor

min(gastmen)# dato menor

rang_gastmen = max(gastmen)-min(gastmen); rang_gastmen

xbarra_gastmen = mean(gastmen);xbarra_gastmen

paste("el promedio es", round(xbarra_gastmen,3))


#* Varianza

varian_gastmen = var(gastmen);varian_gastmen

paste("la varianza es", round(varian_gastmen,2))


ds_gastmen = sqrt(varian_gastmen)

sd(gastmen)

paste("la desviacion estandar es", round(ds_gastmen,2))

#*Coeficiente de variación

CV_gastmen = (ds_gastmen)/(xbarra_gastmen)*100

apply(gastmen, sd)#tarea compita IIIIIIIIIIIIIIIMPORTANTEEEEEEEEEEEEEEEEEEEEE

paste("el coeficiente de variacion es", round(CV_gastmen,2),"%")

#Tarea.