
factoriaL = function(n){
  n_f = 1
  i = 1
  for(i in 1:n) {
    print(i)
    n_f = n_f * i
    }
  return(n_f)
}

combinacion = function(n, k){
  n_f = factoriaL(n)
  k_f = factoriaL(k)
  nk_f = factoriaL(n-k)
  nCk = n_f/(k_f*nk_f)
  return(nCk)
}

j = combinacion(45,22)






























