b <- rep("blah", 1000)
print(b)
counts <- table(b)
counts
rel <- prop.table(counts)
rel
innen <- rel * log(rel)
innen
k <- length(b)
k
kehrwert <- -1 / log(k)
kehrwert * sum(innen)
anscombe
mean(anscombe$x1)
summary(anscombe)
