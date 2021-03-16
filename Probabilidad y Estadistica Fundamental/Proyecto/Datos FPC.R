attach(Liga_de_futbol_Colombiano)

MediaDeGoles = round(mean(Goles))

max(Goles)

GraficoGolesEquipos = hist(Goles,
                             `Nacionalidad Goleador` = x,
                             ylim=c(0,50),
                             xlab="Nacionalidad Goleador",
                             ylab="Goles",
                             col = c(rgb(0.3,0.1,0.4,0.6), rgb(0.3,0.5,0.4,0.6), rgb(0.3,0.9,0.4,0.6)),
                             main =  "Relacion Goles-NacionalidadGoleador",
                             las=3)

GolesGoleadoresNacionalidad = data.frame(`Nacionalidad Goleador`, Goles)

GolesTotalesNacionalidad = function(x, y){
  Acumulado = c()
  for (i in x) {
    for (j in y) {
      if(i = j){
        
      }
    }
  }
}

hist(GolesGoleadoresNacionalidad)








