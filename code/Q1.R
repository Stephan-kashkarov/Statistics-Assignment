# Useful functions
# https://stackoverflow.com/questions/4787332/
# how-to-remove-outliers-from-a-dataset
# This function removes all values outside of the the range between Q1 and Q3
remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}


# Importing related data
values <- read.delim("data/values.txt", header = FALSE, sep="")
# formats the data to be more useable
values <- as.numeric(as.character(unlist(values)))
# iqr range data
IQR_values <- na.omit(remove_outliers(values))

# a.i - Creating a density histograph with curve line
png(filename="out/1.a.i.png") # outputs a png
hist(values, prob=T)
curve(dnorm(x, mean=mean(values), sd=sd(values)), col="darkblue", lwd=2, add=TRUE, yaxt="n")
dev.off() # resets display

# a.ii - Creating an outlierless density histograph with curve line
png(filename="out/1.a.ii.png") # outputs a png
hist(IQR_values, prob=T)
curve(dnorm(x, mean=mean(IQR_values), sd=sd(IQR_values)), col="darkblue", lwd=2, add=TRUE, yaxt="n")
dev.off() # resets display


# a.iii - Creating a normal Q-Q plot of the data
png(filename="out/1.a.iii.png") # outputs a png
qqnorm(values, main = "Normal Q-Q Plot")
qqline(values, datax = FALSE, distribution = qnorm)
dev.off() # resets display

# a.iv - Creating a normal Q-Q plot of the data
png(filename="out/1.a.iv.png") # outputs a png
qqnorm(IQR_values, main = "Normal Q-Q Plot without Outliers")
qqline(IQR_values, datax = FALSE, distribution = qnorm)
dev.off() # resets display