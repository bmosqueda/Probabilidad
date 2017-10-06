source("Formulas.R")

args = commandArgs(trailingOnly = TRUE)
if(length(args) != 3)
{
	stop("Not enough argumets")
}

n = 80
x = 20

p = Proportion(x, n)
Z = ZValue(0.95)
d = 0.03

for(i in 1:3)
{
	sprintf("n%s = ", SampleSize(as.double(args[i]), Z, p, d))
	print(SampleSize(as.double(args[i]), Z, p, d))
}