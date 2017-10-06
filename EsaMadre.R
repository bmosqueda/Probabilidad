N = c( 7000, 5000, 3000, 2000, 100, 300 )    #Población total

Z = 1.96    #El nivel de confianza es igual para todos 95%

d = 0.003     #Margen de er ror

p = seq( 0.0000, 0.9999, 0.0001 )      #Vector de probabilidades de 0.0000 a 0.9999 con incrementos de 0.0001

#Función que calcula el valor de n con los parámetros enviados
muestra = function( N, Z, p, d )
{
    n = ( N * Z^2 * p * (1-p) ) / ( N * d^2 + Z^2 * p * (1-p) )
    n
}

nvec = matrix( NA, nrow = 10000, ncol = 6 )       #Declaración de la matriz para hacer las gráficas

par( mfrow = c( 1, 1 ) )

for( i in 1:6 )
{
    for( j in 1:10000 )
    {
        nvec[ j, i ] = muestra( N[i], Z, p[j], d )          #Cada columna representa uno de los diferentes valores de N
    }
    nvec
}

par( mfrow = c(1,1) )

plot( p, nvec[,1], type = "l", main = "Tarea 5", xlab = "Proporción (p)", ylab = "Tamaño de muestra (n)")
lines( p, nvec[,2], type="l", col="red" )
lines( p, nvec[,3], type="l", col="blue")
lines( p, nvec[,4], type="l", col="yellow")
lines( p, nvec[,5], type="l", col="green")
lines( p, nvec[,6], type="l", col="purple")
 
