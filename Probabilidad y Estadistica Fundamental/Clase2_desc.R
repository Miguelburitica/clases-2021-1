#Gráfico para variables cuantitativas discretas

# Torta 

# Barras

attach(Libro1)

View(Libro1)

x = c("Pers milr", "Ope y mant", "Adquisiones", "Invest y des", 
      "Cons_mil", "Otra")

y = c(127.5, 188.1, 82.3, 65.7, 5.3, 5.5)

?barplot

View(Libro1)

Edad

#Construir diagrama de barras


par(mfrow=c(1,1),
    mar = c(6,4,4,4)
    )# dividir pantalla de graficos

pai = barplot(y,
              names.arg = x,
              ylim=c(0,200),
              xlab="Categoría",
              ylab="Gasto militar",
              col = c(rgb(0.3,0.1,0.4,0.6), rgb(0.3,0.5,0.4,0.6), rgb(0.3,0.9,0.4,0.6)),
              main = "Gráfico para gasto militar",
              las=3)## Gráfico de barra, Elaborarlo mejor

text(plot, y+0.4, paste("$", y, sep = ""), cex = 1, pos = 3)

legend("topright", 
       legend = c("Soy lo maximo", "De veras", "Mateme alguien por favor"),
       col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6)),
       bty = "n" , pch = 20, pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05,0.05)
       )

?barplot

?text

# Diagrama de torta

?pie

library(moments)

library(RColorBrewer)

library(ggplot2)

Departamento <- x

dineroGastado <- y

caja = data.frame(
  Departamento,
  dineroGastado
)

ggplot(caja,
  aes(x = "",
      y = dineroGastado, 
      fill=Departamento )) +
  geom_bar(stat = "identity", width = 1, color="white") +
  coord_polar("y", start = 0) +
  theme_void()

?theme



myColors = brewer.pal(6, "Set2")

pie(y,
    labels = x,
    col = myColors,
    border = "white"
    )# mejorar

?pie



#################################

##Gráficas para datos cuantitativos continuos

#distribución de frecuencias

#Histogramas

#diagrama de cajas

#ejemplo
#Fórmula sw sturges

dim(Libro1)

1+log(39,base=2)

library(fdth)# Cargar este paquete para elaborar distribuciones de frecuencia

?fdt

dist_f = fdt(Edad,breaks="Sturges") # calcula la distribución de frecuencias utilizando la regla Sturge

Histograma = hist(Edad,
                  breaks = 6,
                  freq = TRUE,
                  ylim = c(0, 20),
                  xlim = c(20,45),
                  col = rgb(1,0,0,0.5),
                  xlab = "Edad",
                  ylab = "Frecuencia",
                  main = "Histograma de edad")

?hist

legend("topright",
       legend = c("Edad"),
       col = c(rgb(1, 0, 0, 0.5)),
       pt.cex = 2,
       pch = 15
)

text(Histograma,
     y = dist_f,
     paste("f", fdt(Edad), sep = ""),
     cex = 1,
     pos = 3
)

text(plot, y+0.4, paste("$", y, sep = ""), cex = 1, pos = 3)
?hist

min(Edad)

max(Edad)

(max(Edad)-min(Edad))/7

length(Edad)
8/39

#Donde
#f= frecuencia absoluta
#rf= frecuencia relativa: f/n = 8/39
#rf(%) frecuencia relativa porcentual
#cf= frecuencia acumulada
#cf(%)=frecuencia acumulada porcentual

par(mfrow=c(1,1)) # particiona mi ventana grafica en 3x2.

hist(Edad, 
     breaks = 8,
     ylim = c(0, 20),
     main="histograma de frtecuencias") #histograma utilizando el numero de clases según Sturge

plot(dist_f, type="cfh", col = c(rgb(1, 0, 0, 0.5)))   #histograma de frecuencias acumulada

plot(dist_f, type="cfp", col = c(rgb(1, 0, 0, 0.5)))   #ojiva

plot(dist_f, type="d")    #Densidad

x <- seq(-pi, pi, 0.1)

plot(x, sin(x),
     main="Overlaying Graphs",
     ylab="",
     type="l",
     col="blue")
lines(x,cos(x), col="red")
legend("topleft",
       c("sin(x)","cos(x)"),
       fill=c("blue","red")
)



Encuesta


plot(dist_f, type="fp")#Polígono de frecuencias

plot(dist_f, type="d")# densidad

plot(dist_f, type="cfpp",main = "Ojiva", col=4)

plot(dist_f, type="fh") # histograma de frecuencias absolutas

plot(dist_f, type="cfh")

with(dist_f, hist(Edad))

#explore los otros argumentos graficos,
#"fh", "fp", "rfh", "rfp", "rfph", "rfpp", "d", "cdh", 
#"cdp", "cfp", "cfph", 


#Loblolly

#View(Loblolly)

#nuevdat = as.data.frame(Loblolly)

#nuevdat = rename(altura = height, edad = age,
 #                semilla= Seed,.data = nuevdat)

######################################
#Diagrama de cajas
###########

boxplot(Edad)
