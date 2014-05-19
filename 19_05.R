library(ggplot2)
library(car)
aphasiker <- read.csv2("Data/aphasiker.csv",header = T)
rt <- read.table("Data/punkt_rt.tab",header = T)
rt$subj <- as.factor(rt$subj)
broca.lex.dec <- aphasiker[aphasiker$Aphasie == "B","Lex_Dec"]
wernicke.lex.dec <- aphasiker[aphasiker$Aphasie == "W","Lex_Dec"]
rt1 <- rt$RT[rt$subj == 1]
rt2 <- rt[rt$subj == 2,]$RT
qqnorm(broca.lex.dec) #quantile-quantile im vergleich zur normalverteilung, Bei normalverteilung gäbe es eine diagonale von o nach rechts oben (y=x), x achse=ideal, y achse= richtige daten
qqline(wernicke.lex.dec) # fügt eine annäherungslinie ein
