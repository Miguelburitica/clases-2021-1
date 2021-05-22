
#library(gtools)

# Permutaciones

N = 5  # Número de elementos

n = 3 # grupos de 3 en 3

alumnos = c(1:N) # Son los alumnos con id un número consecutivo

permut = permutations(N, n, alumnos)

head(permut) # Las primeras permutaciones

View(permut)

nrow(permut)  # ¿cuántas permutaciones?

factorial(N) / factorial(N-n) # debe salir lo mismo que nrow(permutaciones)

#Con funcion creada

permutac = function(n,k){
  y = factorial(n)/factorial(n-k)
  return(y)
}
permutac(10,2)

# Combinaciones

combin = combinations(N, n, alumnos)

head(combin) # Las primeras combinaciones

View(combin)

nrow(combin) # ¿cuántas combinaciones?

# De acuerdo a la Fórmula. Debe salir el mismo número que  nrow(combinaciones)

factorial(N) / (factorial(n) * (factorial(N-n)))

#Con función creada

combinat = function(n,k){
  z = factorial(n)/(factorial(k)*factorial(n-k))
  return(z)
}
combinat(5,3)


suma <- function(x, y){
  resultado <- x + y
  return(resultado)
}
suma(5,2)

Pit = function(x,y) {
  z = x^2 + y^2
  return(z)
}
Pit(2,3)

CV = function(x,y){
  z = (x/y)*100
  return(z)
}
mean(gastmen)
sd(gastmen)

y/x
CV( 495256.6,754390.7)
sd(gastmen)/mean(gastmen)

calcular.cuota.hipoteca <- function(capital, anyos, interes){
  interes.mensual <- interes / 12 / 100 
  meses <- 1:(anyos*12)
  return(capital / sum(1 / (1+interes.mensual)^meses))
}
calcular.cuota.hipoteca(100000, 20, 3)

factorial(5)
combn(5,3)

combinat = function(n,k){
  z = factorial(n)/(factorial(k)*factorial(n-k))
  return(z)
}
combinat(10,2)

permutac = function(n,k){
  y = factorial(n)/factorial(n-k)
  return(y)
}
permutac(10,2)



