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


