############################

## Medidas de posición  en la librería moments ########

#########################

#Cuantiles

attach(Libro1)

?quantile

quantile(gastbeb, type = 6)

cantDat = length(gastbeb)

q1 = (25*(cantDat+1))/100

q3 = (75*(cantDat+1))/100

gastbeb

sort(gastbeb)[30]

median(gastbeb)

quantile(gastmen,0.75)

quantile(gastbeb)

sort(gastbeb,F)

median(gastbeb)

gastbeb

mediana_gastmen

quantile(gastbeb,0.75, type = 6)

quantile(gastbeb,0.25,type = 6)

quantile(gastbeb,0.84, type = 6) 

(39+1)*0.75

median(gastbeb)

sort(gastbeb,F)[30]

quantile(gastbeb)

quantile(gastbeb, type = 6)

0.84*40

sort(gastbeb,F)[33]

sort(gastbeb,F)[34]

157820 + 0.6*(170769-157820)

#Curtosis

?kurtosis

k_gastbeb = kurtosis(gastbeb); k_gastbeb

hist(gastbeb, ylim = c(0,25))

plot(gastbeb, type = "cfp")

###Asimetría

?skewness

hist(gastbeb)

library(moments)

As_gastbeb = skewness(gastbeb);As_gastbeb

moda = mfv(gastbeb)

k = (mean(gastbeb) - moda)/sd(gastbeb)

k_e = 3*(mean(gastbeb)-median(gastbeb))/sd(gastbeb)

#Por último la función summary muestra un resumen descriptivo de los datos 

summary(gastbeb)

summary(Libro1)

########################33
