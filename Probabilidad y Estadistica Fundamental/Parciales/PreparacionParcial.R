############################################
#Parcial N?1
#Hacer un an?lisis descriptivo
#
#Miguel Antonio Buritic? Vargas
#

attach(Liga_de_futbol_Colombiano)

library(moments)
library(RColorBrewer)
library(ggplot2)
library(fdth)
library(stats)
library(dplyr)

x = 34

#Primer punto
#Gr?fica de barras

respectoAlPromedio = c("Bajo el promedio", "En el promedio", "Sobre el promedio")

GolesLiga = c(1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 
              1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963,
              1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970,
              1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978,
              1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 
              1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
              1995, 1996, 1997, 1998, 1999, 2000, 2001)

desempe?oGoles = c(1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 
            1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963,
            1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970,
            1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978,
            1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 
            1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
            1995, 1996, 1997, 1998, 1999, 2000, 2001)

length(desempe?oGoles)
length(GolesLiga)

j = 0
k = 1

Goles

for (i in Goles) {
        if (j != 23 && j != 25 && j != 57) {
                GolesLiga[k] = i
                k = k + 1
        }j = j + 1
}

j = 1

funcionParaPromediar = f(){
        for (i in GolesLiga) {
        if (i < round(mean(Goles))){
                desempe?oGoles[j] = respectoAlPromedio[1]
        }else if(i == round(mean(Goles))){
                desempe?oGoles[j] = respectoAlPromedio[2]
        }else{
                desempe?oGoles[j] = respectoAlPromedio[3]
        }
        j = j + 1
        }
}

desempe?oGoles = c("")

length(desempe?oGoles)
tableDesempe?oGoles = table(desempe?oGoles)
length(A?oLiga)

RespectoPromedioEdici?n = table(A?oLiga, desempe?oGoles)

myColors = c(rgb(0.0, 0.7, 0.1, 1), rgb(0.9,0.0,0.0, 1), rgb(0.9,0.9,0.0,0.8), rgb(0.0, 1,0.0, 1), rgb(0.01,0.0,0.9,0.9), rgb(0.0,0.0,0.0,0.8), rgb(0.0,0.0,0.9,0.4))
vecesCampeonDepartamento = table(`Departamento del Campe?n`)
DepartamentosCampeones = c("Antioquia", "Atl?ntico", "Bogot?", "Caldas", "Magdalena", "none", "Valle del Cauca")
CiudadesCameponas = table(`Departamento del Campe?n`)

GraficoBarras = barplot(CiudadesCameponas,
                        ylim=c(0,35),
                        xlab="Departamento del Campe?n",
                        ylab="Veces campe?n",
                        col = myColors,
                        main = "Gr?fico departamentos campeones",
                        las=1)## Gr?fico de barra, Elaborarlo mejor

text(GraficoBarras,
     CiudadesCameponas+0.4, 
     paste(CiudadesCameponas, sep = ""), 
     cex = 1, 
     pos = 3)

legend("topleft", 
        legend = ,
        col = myColors,
        bty = "n" , pch = 20, pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05,0.05)
        )

#Gr?fica pastel

pieChartDepartamentosCampeones = data.frame(
        DepartamentosCampeones,
        vecesCampeonDepartamento
)

ggplot(pieChartDepartamentosCampeones,
        aes(    x = "-",
                y = vecesCampeonDepartamento, 
                init.angle = 0,
                fill = DepartamentosCampeones ))              + 
        geom_bar(stat = "identity", width = 1, color="white") +
        coord_polar("y", start = 0)                           +
        theme_void()

?aes

pie(vecesCampeonDepartamento,
        labels = DepartamentosCampeones,
        col = myColors,
        radius = 1.2,
        main = "Gr?fico departamentos campeones",
        init.angle = 0,
        border = "white"
)

#Segundo Punto

View(Liga_de_futbol_Colombiano)

A?oLiga = c(1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 
            1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963,
            1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970,
            1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978,
            1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 
            1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
            1995, 1996, 1997, 1998, 1999, 2000, 2001)

Campe?nSinRepetir = c(1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 
                      1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963,
                      1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970,
                      1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978,
                      1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 
                      1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
                      1995, 9596, 9697, 1998, 1999, 2000, 2001)


length(A?oLiga)
length(Campe?nSinRepetir)

j = 0
k = 1

for (i in Campe?n) {
        if (j != 23 && j != 25 && j != 57) {
                paste("Campe?n = ", Campe?n[k])
                Campe?nSinRepetir[k] = i
                k = k + 1
        }
        j = j + 1
}

tablaDeCampeones = data.frame(A?oLiga, Campe?nSinRepetir)


GolesTotalesA?o = c(1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 
                      1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963,
                      1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970,
                      1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978,
                      1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 
                      1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
                      1995, 1996, 1997, 1998, 1999, 2000, 2001)

length(A?oLiga)
length(GolesTotalesA?o)

j = 0
k = 1

for (i in Goles) {
        if (j != 23 && j != 25 && j != 57) {
                GolesTotalesA?o[k] = i
                k = k + 1
        }
        j = j + 1
}

tablaLigaGoles = data.frame(A?oLiga, GolesTotalesA?o)

#Tercer Punto

barplot()
















































