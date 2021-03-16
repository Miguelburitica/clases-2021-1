####################################
# Prye 2021-1
# Unal
# Oscar Pacheco
# Estadística Descriptiva Primera clase 25/02/2021
##################################3

library(moments)
library(stats)
library(ggplot2)
library(dplyr)
attach(Libro1)#guarda las variables de la base de datos

?dplyr

log(20,10)
exp(1)
exp(32)
l = sin(pi/2)

b = c("María", "Carlos");b

a = c(seq(from =20, to = 59, by =2));a

sample(a,5)

Libro1# carga el archivo

View(Libro1) # permite verlo en un mejor formato


# donde Edad es el número de años de los...


dim(Libro1) # Número de filas y de columnas del conjunto de datos.

names(Libro1) # Nombre de las columnas

head(Libro1) # Para visualizar las primeras 6 observaciones de cada columna.

tail(Libro1) # permite ver las últimas 6 filas

str(Libro1) # Otro comando útil que nos permite ver los primeros valores de cada variable y sus atributos



#tabla de frecuencias absolutas para datos cualitativos

table(gen)

data.frame(table(gen,Estciv))

table(Estciv,gen)

as.data.frame(table(gen,Estr))# presenta mejor la información 

prop.table(table(Estr,gen))


par(oma=c(1,1,1,1), col = "red", new=T, font=4, cex=1)

K = rgb(maxColorValue=255, 255, 0, 153)
L = rgb(maxColorValue=255, 0, 153, 255)
M = rgb(maxColorValue=255, 153, 255, 0)
H <- c(K, L, M)

pie(col= H,
    table(gen),
    main = "Diagrama de torta",
    sub = "Tendencia de género") # Gráfico de torta, Elaborarlo mejor



mtext(outer=T,'GRAFICO DE GENEROS EN UNA MUESTRA',side=3)


#otro ejemplo

Datos = data.frame (
  Cultivo = c("Trigo", "Melocoton", "Sandia", "Tomate", "Cebada","Ciruelo","Melon", 
              "Almendro", "Calabacin", "Maiz"),
  Grupo = c("Cereal", "Frutal", "Horticola", "Horticola", "Cereal", "Frutal", "Horticola", 
            "Frutal", "Horticola", "Cereal"),
  Agua = c("Secano", "Regadio", "Regadio", "Regadio", "Secano", "Regadio", "Regadio", 
           "Secano", "Regadio", "Regadio"),
  Terreno = c("Abierto", "Abierto", "Invernadero", "Invernadero", "Abierto", "Abierto", 
              "Abierto", "Abierto", "Invernadero", "Abierto"))
View(Datos)

table(Datos$Grupo)

table (Datos$Grupo, Datos$Agua)

Tabla = table (Datos$Grupo, Datos$Agua)

prop.table (Tabla) # Proporción con respecto al total de los datos de la tabla

####################################################################

###########Para crear un diagrama de totrtas y barras.
#Se necesita crear una tabla de frecuencias (con la función table)
#y luego aplicar la función pie o barplot a esta tabla.

#Gráficos datos cualitativos

# Torta

# Diagrama barras

par(mfrow=c(1,1))

pie(prop.table(table(Estciv)),col = c("purple", "violetred1", "green3",
                                  "cornsilk"),
                                  main = "Diagrama de torta Est civil")

?pie

barplot((table(Estciv)), ylim = c(0, 15) ,main="Diagrama de barras para Estciv",col = c("purple", "violetred1", "green3",
                                                                      "cornsilk"))

##############################################33

#Gráfico para variables cuantitativas discretas



# Torta 

# Barras







barplot(table(Estr),main = "Diagrama de barra")## Gráfico de barra, Elaborarlo mejor

t= table(Estciv)
barplot(t,col = 8)

Barras = barplot(t, axes = T,axisname = T, ylim=c(0,20),
                 col=c(2,3,4,5),main="Diagrama de barras Est Civil",
                 xlab="Estado civil", ylab="frec abs")
axis(1,labels=c("control", "Casado","Separado","Soltero","Union libre"), at=Barras)
axis(2,at=seq(0,20,by = 5))

#elborar data frame

fabs=table(Estr)

Freq = as.data.frame(table(gen))

barplot(fabs,col="pink",xlab="Estatura",ylab="Frecuencia absoluta",main = "Diagrama de barras del estrato")

pie(fabs)

fabs_gen=table(gen)

pie(fabs_gen)

#################################




##Gráficas para datos cuantitativos continuos

#Histogramas

#diagrama de cajas

#>distribución de frecuencias

library(fdth)# Cargar este paquete para elaborar distribuciones de frecuencia

?fdt

dist_f = fdt(Edad,breaks="Sturges") # calcula la distribución de frecuencias utilizando la regla Sturge

#Donde
#f= frecuencia absoluta
#rf= frecuencia relativa
#rf(%) frecuencia relativa porcentual
#cf= frecuencia acumulada
#cf(%)=frecuencia acumulada porcentual

par(mfrow=c(3,2)) # particiona mi ventana grafica en 3x2.

hist(Edad, breaks = "Sturges") #histograma utilizando el numero de clases según Sturge

plot(dist_f, type="cfh")   #histograma de frecuencias acumulada

plot(dist_f, type="cfp")   #poligono de frecuencias acumulado

plot(dist_f, type="fh")# histograma frecuencias clases

plot(dist_f, type="fp")#Polígono de frecuencias

plot(dist_f, type="d")# densidad

plot(dist_f, type="cfpp",main = "Ojiva", col=4)

plot(dist_f, type="rfph")

#explore los otros argumentos graficos,
#"fh", "fp", "rfh", "rfp", "rfph", "rfpp", "d", "cdh", 
#"cdp", "cfp", "cfph", 

Loblolly

View(Loblolly)

nuevdat = as.data.frame(Loblolly)

nuevdat = rename(altura = height, edad = age,
               semilla= Seed,.data = nuevdat)

View(nuevdat)
############################################
#### Distribución de frecuencias####



#mean(Encuesta$gastmen)

###  Medidas Descriptivas  ###

#mean(gastmen)

#<- tambien asigna variables

#tabla_1=table(Estr)

#table(gen)

#tort_gen=pie(table(gen),main = "Gráfica de torta de la var gen")

#barplot(tabla_1,col="gray",xlab = "Estratos" ,ylab = "Frecuencia absoluta",main = "Gráfica de barras")# diagrama de barras

#pie(tabla_1)# diagrama de torta 

summary(Encuesta)

prom_edad=mean(Edad)

round((prom_edad),2)

median(Edad)


median(frec)

frecuencia=data.frame(Edad)

moda_Edad=frecuencia(which.max(frecuencia$Freq),1)
moda=function(arreglo)
  
{q=table(arreglo)
q=sort(q,TRUE)
return(q[1])
}
moda=moda(Encuesta$Estr)
print(moda)
max(Edad)
min(Edad)
rang_edad=max(Edad)-min(Edad)
varian_edad=var(Edad)
ds_edad=sqrt(varian_edad)
sd(Edad)
#Tarea
#Moda para datos cuantitativos
#Cuatiles 
