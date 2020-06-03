
# Importing related data
oysters <- read.delim("data/oysters.txt", header = FALSE, sep=" ")
oysters <- stack(oysters)$values # converting block into column

# a.i - Creating a density histograph with curve line
png(filename="out/2.a.i.png") # outputs a png
hist(oysters, prob=T)
curve(dnorm(x, mean=mean(oysters), sd=sd(oysters)), col="darkblue", lwd=2, add=TRUE, yaxt="n")
dev.off() # resets display

# b.i - Creating a normal Q-Q plot of the data
png(filename="out/2.b.i.png") # outputs a png
qqnorm(oysters, main = "Normal Q-Q Plot")
qqline(oysters, datax = FALSE, distribution = qnorm)
dev.off() # resets display

# d.i - Calculating the 99% confidence interval.
n <- length(oysters)
m <- mean(oysters)
s <- var(oysters)
error <- qnorm(0.995)*s/sqrt(n)
lowerBound <- m-error
upperBound <- m+error
cat("99% confidence interval is (" , lowerBound , "," , upperBound, ")\n")
#[1] 99% confidence interval is ( 32.89357 , 80.95406 )