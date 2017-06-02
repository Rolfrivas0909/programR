#EJERCICIO 3
#Graficar 1000 pares de numeros aleatorios uniformemente distribuidos 
#entre -1 y 1, de manera talque se visualicen en azul cuando tengan 
#el mismo signo y en rojo cuando tengan signos diferentes

#x = c(-0.1, +0.1)

plot.new()
plot.window(xlim=c(-1,1), ylim=c(-1,1))
axis(1)
axis(2)
box()

for(i in 1:1000){
  x = runif(2, min=-1, max=1)
if(sign(x[1]*x[2])==-1){
  #signo positivo
  points(x[1], x[2], col="blue", pch=19)
} else {
  #signo negativo
  points(x[1], x[2], col="red", pch=19)
}
}

#EJERCICIO 7
#Calcular las raices del polinomio p(x) = ax2 + bx + c.

#sqrt(3)*1i

coef=c(-1,101,-100) #(a,b,c)

a= coef[1]
b= coef[2]
c= coef[3]

deter=b^2 - 4*a*c

if(deter<0) {
  #determinante negativo
  deter = -1*deter
  raices = (-b + c(+1,-1)*sqrt(deter)*1i)/(2*a)
} else{
  #determinante positivo
  raices = (-b + c(+1,-1)*sqrt(deter))/(2*a)
}

print(raices)


#nombreFuncion = function(args){
#  return()
#}

#creando una función para realizar el ejercicio anterior
#generando un registro de todas las llamadas/calculos que se han realizado

calcularRaices = function(coef){
  a= coef[1]
  b= coef[2]
  c= coef[3]
  
  deter=b^2 - 4*a*c
  
  if(deter<0) {
    #determinante negativo
    deter = -1*deter
    raices = (-b + c(+1,-1)*sqrt(deter)*1i)/(2*a)
  } else{
    #determinante positivo
    raices = (-b + c(+1,-1)*sqrt(deter))/(2*a)
  }
  return(raices)
}

calcularRaices(coef=c(16,-5,6))
calcularRaices(coef=c(-9,1,-12))
calcularRaices(coef=c(-1,101,-100))
#otorga el mismo resultado pero con el uso de una función creada
#cuando se ejecuta un código más de dos veces es mejor implementarlo en una función
#mayor practicidad para resolver problemas


#Crear función para graficar números aleatorios

graficarNumeros = function(n, min, max, col, pch){
  plot.new()
  plot.window(xlim=c(-1,1), ylim=c(-1,1))
  axis(1)
  axis(2)
  box()
  
  for(i in 1:n){
    x = runif(2, min=min, max=max)
    if(sign(x[1]*x[2])==+1){
      #signo positivo
      points(x[1], x[2], col=col[1], pch=pch)
    } else {
      #signo negativo
      points(x[1], x[2], col=col[2], pch=pch)
    }
  }
  return(invisible())
}

graficarNumeros(n=100, min=-1, max=1, col=c("blue", "red"), pch=19)

graficarNumeros(n=34, min=-1, max=1, col=c("purple", "yellow"), pch=1)

graficarNumeros(n=34, min=-10, max=10, col=c("green", "purple"), pch=13)

#####mi solucion

graficarNumeros2 = function(n, min, max, col, pch){
  plot.new()
  plot.window(xlim=c(min, max), ylim=c(min,max))
  axis(1)
  axis(2)
  box()
  
  for(i in 1:n){
    x = runif(2, min=min, max=max)
    if(sign(x[1]*x[2])==+1){
      #signo positivo
      points(x[1], x[2], col=col[1], pch=pch)
    } else {
      #signo negativo
      points(x[1], x[2], col=col[2], pch=pch)
    }
  }
  return(invisible())
}

graficarNumeros2(n=14, min=-50, max=50, col=c("blue", "purple"), pch=1)

graficarNumeros2(n=10, min=-15, max=50, col=c("blue", "purple"), pch=1)

graficarNumeros2(n=100, min=-10, max=50, col=c("blue", "purple"), pch=10)


##simplificando la función otorgándole valores por defecto a los argumentos innecesarios
graficarNumeros3 = function(n, min=-1, max=+1, col=c("blue", "red"), pch=19){
  plot.new()
  plot.window(xlim=c(min, max), ylim=c(min,max))
  axis(1)
  axis(2)
  box()
  
  for(i in 1:n){
    x = runif(2, min=min, max=max)
    if(sign(x[1]*x[2])==+1){
      #signo positivo
      points(x[1], x[2], col=col[1], pch=pch)
    } else {
      #signo negativo
      points(x[1], x[2], col=col[2], pch=pch)
    }
  }
  return(invisible())
}

graficarNumeros3(n=99)
graficarNumeros3(n=9)
graficarNumeros3(n=25)

graficarNumeros4 = function(n, min=-1, max=+1, col=c("blue", "red"),...){
  plot.new()
  plot.window(xlim=c(min, max), ylim=c(min,max))
  axis(1)
  axis(2)
  box()
  
  for(i in 1:n){
    x = runif(2, min=min, max=max)
    if(sign(x[1]*x[2])==+1){
      #signo positivo
      points(x[1], x[2], col=col[1], ...)
    } else {
      #signo negativo
      points(x[1], x[2], col=col[2], ...)
    }
  }
  return(invisible())
}

graficarNumeros4(n=1000, cex=1.5)
graficarNumeros4(n=1000, pch=14)
##función wrappers

graficarNumeros4(n=1000, col=c("green", "orange"))
graficarNumeros4(n=100, col=c("green", "orange"))
graficarNumeros4(n=10, col=c("green", "orange"))
graficarNumeros4(n=10000, col=c("green", "orange"))

##acá n es lo que podemos cambiar
nuevaGraficanumeros=function(n,col=c("green", "orange"),...){
  graficarNumeros4(n=n,col=col,...)
  
  return(invisible)##devolver para evitar efectos secundarios
}

nuevaGraficanumeros(n=1000) ##este es un wrapper

?read.csv
##read.csv es un wrapper de la función read.table y tiene específicos valores
##por defecto

#read.table(file, header = FALSE, sep = "", quote = "\"'",
           #dec = ".", numerals = c("allow.loss", "warn.loss", "no.loss"),
           #row.names, col.names, as.is = !stringsAsFactors,
           #na.strings = "NA", colClasses = NA, nrows = -1,
           #skip = 0, check.names = TRUE, fill = !blank.lines.skip,
           #strip.white = FALSE, blank.lines.skip = TRUE,
           #comment.char = "#",
           #allowEscapes = FALSE, flush = FALSE,
           #stringsAsFactors = default.stringsAsFactors(),
           #fileEncoding = "", encoding = "unknown", text, skipNul = FALSE)

#read.csv(file, header = TRUE, sep = ",", quote = "\"",
         #dec = ".", fill = TRUE, comment.char = "", ...)

##ejercicio 12 hoja 4
##calcular n factorial para n que pertenece a N

n=0
factorial=1
i=1
for (i in 1:n) 
  factorial=factorial*i
print(factorial)

n=-2.3
if (n<0) stop("Factorial no definido para números negativos")
if(n%%1!=0) warning("Factorial no definido para números no enteros")

if (n=0){
   factorial=1
  } else {
   factorial=1
   for(i in 1:n){
     factorial=factorial*i
  }
}
return(factorial)

factorial=function(n){
  if (n<0) stop("Factorial no definido para números negativos")
  if (n=0){
    factorial=1
  } else {
    factorial=1
    for(i in 1:n){
      factorial=nfactorial*i
    }
  }  
  return(factorial)
}

##items evaluados en presentación
##



