n = 10
# 0 = red | 1 = blue
bag = c(0, 1)
while (length(which(0 == bag)) == 1) {
    ball <- sample(bag, 1)
    append(bag, ball)
    append(bag, ball)
}
print(bag)