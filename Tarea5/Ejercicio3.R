source("Formulas.R")

xAxis <- function(lim)
{
	x <- c(0)
	i = 0.0001
	while(i <= lim)
	{
		x <- union(x, i)
		i <- i + 0.0001
	}
	return(x)
}


Ns <- c(7000, 5000, 3000, 2000, 100, 300)
Z = ZValue(0.95)
d = 0.03
ps = xAxis(0.99)
n <- c()
ns <- list()

for (i in 1:length(Ns))
{
	n <- c()
	for(j in 1:length(ps))
	{
	  p = ps[j]
		n <- union(n, SampleSize(Ns[i], Z, p, d))
	}
	ns[[i]] <- n
}

# par(mfrow=c(1,2))
# barplot(width = ps, height =  ns[[1]], xlab = "p values", ylab = "n values", col=rainbow(20,s=.4,v=.9))
plot(x = ps, y = ns[[2]], type = "l", col = "blue", xlab = "Valores de p", ylab = "Tamaño de la muestra")
lines(x = ps, y = ns[[2]], type="l", col="red")
lines(x = ps, y = ns[[4]], type="l", col="green")
