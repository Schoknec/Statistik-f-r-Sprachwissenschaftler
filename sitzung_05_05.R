rtdata <- read.table("Schoknec/priming_rt.tab",header = T)
summary(rtdata)
print(rtdata)
rtdata$subj <- as.factor(rtdata$subj)
rtdata[1,]
#Zentrieren
rt.zentriert <- rtdata$RT - mean (rtdata$RT)
head(rt.zentriert, n=4)
#z-Transformation
rt.z <- (rtdata$RT - mean (rtdata$RT)) / sd (rtdata$RT)
head(rt.z, n=4)

