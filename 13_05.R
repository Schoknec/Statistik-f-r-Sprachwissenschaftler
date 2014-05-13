noten.dist
library(reshape2)
melt(noten.dist,id.vars="Notenpunkte",value.name="P",variable.name="Standardabweichung")
noten.dist <- melt(noten.dist,id.vars="Notenpunkte",value.name="P",variable.name="Standardabweichung")
library(ggplot2)
ggplot(data=noten.dist,aes(x=Notenpunkte,y=P,color=Standardabweichung)) + geom_line() + scale_x_continuous(limits=c(0,16))
pnorm(13,mean=mu,sd=3,lower.tail=FALSE)
pnorm(5,mean=mu,sd=3)
for(s in c(3,4,5) ){
durchfall <- pnorm(5,mean=mu,sd=s)
output <- paste("Bei einer Standabweichung von",s, "fallen",durchfall*100,"% durch.")
print(output)
}
for(s in c(3,4,5) ){
sehrgut <- pnorm(13,mean=mu,sd=s,lower.tail=FALSE)
output <- paste("Bei einer Standabweichung von",s, "sind"
,sehrgut*100,"% sehr gut.")
print(output)
}
suppressPackageStartupMessages(library(sn))
qplot(x=1:15,y=dsn(1:15, xi=c(12), omega=1, alpha=-3, log=FALSE),geom="line",xlab="Notenpunkte",ylab="P")
n <- 50
noten.dist$Anzahl <- noten.dist$P * n
noten.dist$Anzahl
noten.dist
ggplot(data=noten.dist,aes(x=Notenpunkte,y=Anzahl,color=Standardabweichung)) + geom_line() + scale_x_continuous(limits=c(0,16))
mittel <- (pnorm(9,mean=mu,sd=3) - pnorm(7,mean=mu,sd=3) ) * n
output <-paste(mittel,"Studenten bekommen zwischen 7 und 9 Notenpunkten.")
print(output)
gut <- (pnorm(15,mean=mu,sd=3) - pnorm(10,mean=mu,sd=3) ) * n
output <-paste(gut,"Studenten bekommen zumindest 10 Notenpunkte.")
print(output)
weniger <- pnorm(10,mean=mu,sd=3) * n
output <-paste(weniger,"Studenten bekommen weniger als 10 Notenpunkte.")
print(output)
oh <- pnorm(8,mean=mu,sd=3) * n
output <-paste(oh,"Studenten bekommen weniger als 8 Notenpunkte.")
print(output)
paste("Um überdurchschnittlich zu sein, muss man mehr als",qnorm(0.5,mean=mu,sd=3),"Notenpunkte bekommen.")
paste("Um in dem besten 1% abzuschließen, muss man zumindest",qnorm(0.99,mean=mu,sd=3),"Notenpunkte bekommen.")
Der kritische Wert für einen einseitigen $z$-Test ist paste(qnorm(0.94)).
paste("Der kritische Wert für einen einseitigen $z$-Test ist",qnorm(0.94),".")
paste("Der kritische Wert für einen einseitigen $z$-Test ist",qnorm(0.94),".")
paste("Die kritischen Werte für einen zweiseitigen $z$-Test sind $\pm$`,qnorm(0.97),".")
paste("Die kritischen Werte für einen zweiseitigen $z$-Test sind $\pm$`",qnorm(0.97),".")
paste("Die kritischen Werte für einen zweiseitigen $z$-Test sind",qnorm(0.97),".")
kl <- 7
noten.dist$Anzahl <- noten.dist$P * kl
noten.dist$Anzahl
noten.dist
znoten <- (noten.dist$Anzahl - mean (noten.dist$Anzahl)) / sd(noten.dist$Anzahl)
znoten
head(znoten, n=4)
pnorm(10,mean=mu,sd=3) * 7
pnorm(mean=10,sd=3) * 7
paste("Der kritische Wert für einen einseitigen $z$-Test ist",qnorm(0.94),".")
paste("Die kritischen Werte für einen zweiseitigen $z$-Test sind",qnorm(0.97),".")
noten.dist$Anzahl
noten.dist$Anzahl <- noten.dist$P * kl
noten.dist
noten.dist.kl <- data.frame(Notenpunkte=noten,drei)
noten.dist.kl
noten.dist.kl$Anzahl <- noten.dist$P * kl
noten.dist.kl$Anzahl <- noten.dist$drei * kl
noten.dist <- melt(noten.dist.kl,id.vars="Notenpunkte",value.name="P",variable.name="Standardabweichung")
noten.dist.kl
kl <- 7
noten.dist.kl <- data.frame(Notenpunkte=noten,drei)
noten.dist <- melt(noten.dist.kl,id.vars="Notenpunkte",value.name="P",variable.name="Standardabweichung")
noten.dist.kl
noten <- 1:15
mu <- 8
drei <- dnorm(noten,mean=mu,sd=3)
vier <- dnorm(noten,mean=mu,sd=4)
fuenf <- dnorm(noten,mean=mu,sd=5)
noten.dist <- data.frame(Notenpunkte=noten,drei,vier,fuenf)
noten.dist
noten.dist <- melt(noten.dist,id.vars="Notenpunkte",value.name="P",variable.name="Standardabweichung")
noten.dist
drei <- dnorm(noten,mean=10,sd=3)
noten.dist <- data.frame(Notenpunkte=noten,drei)
noten.dist
drei <- dnorm(noten,mean=10,sd=3)
noten.dist.m10 <- data.frame(Notenpunkte=noten,drei)
noten.dist.m10
noten.dist.m10 <- melt(noten.dist.m10,id.vars="Notenpunkte",value.name="P",variable.name="Standardabweichung")
noten.dist.m10
kl <- 7
noten.dist.m10$Anzahl <- noten.dist.m10$P * kl
noten.dist.m10$Anzahl
noten.dist.m10
znoten <- (noten.dist.m10$Anzahl - mean (noten.dist.m10$Anzahl)) / sd(noten.dist.m10$Anzahl)
head(znoten, n=4)
znoten
head(znoten, n=4)
gr <- 50
noten.dist.m10$Anzahl <- noten.dist.m10$P * gr
noten.dist.m10$Anzahl
noten.dist.m10
znoten <- (noten.dist.m10$Anzahl - mean (noten.dist.m10$Anzahl)) / sd(noten.dist.m10$Anzahl)
znoten
head(znoten, n=4)
drei <- dnorm(noten,mean=7,sd=3)
noten.dist.m7<- data.frame(Notenpunkte=noten,drei)
noten.dist.m7
noten.dist.m7 <- melt(noten.dist.m7,id.vars="Notenpunkte",value.name="P",variable.name="Standardabweichung")
noten.dist.m7
noten.dist.m7$Anzahl <- noten.dist.m7$P * 20
noten.dist.m7$Anzahl
noten.dist.m7
znoten <- (noten.dist.m7$Anzahl - mean (noten.dist.m7$Anzahl)) / sd(noten.dist.m7$Anzahl)
znoten
head(znoten, n=4)
drei <- dnorm(noten,mean=7,sd=3)
noten.dist.m7 <- data.frame(Notenpunkte=noten,drei)
noten.dist.m7
noten.dist.m7 <- melt(noten.dist.m7,id.vars="Notenpunkte",value.name="P",variable.name="Standardabweichung")
noten.dist.m7
noten.dist.m7$Anzahl <- noten.dist.m7$P * 25
noten.dist.m7$Anzahl
noten.dist.m7
znoten <- (noten.dist.m7$Anzahl - mean (noten.dist.m7$Anzahl)) / sd(noten.dist.m7$Anzahl)
znoten
head(znoten, n=4)
znoten <- sqrt(7) * ((10-8)/3)
print(znoten)
znoten <- (noten.dist.m10$Anzahl - mean (noten.dist.m10$Anzahl)) / sd(noten.dist.m10$Anzahl)
znoten
```{r, echo=FALSE}
# citations with R -- kinda like BibTeX!
suppressPackageStartupMessages(library(knitcitations))
```
# Die nächsten Punkte sollten langsam automatisch sein...
1. Kopieren Sie diese Datei in Ihren Ordner (das können Sie innerhalb RStudio machen oder mit Explorer/Finder/usw.) und öffnen Sie die Kopie. Ab diesem Punkt arbeiten Sie mit der Kopie. Die Kopie bitte `hausaufgabe07.Rmd` nennen und nicht `Kopie...`
2. Sie sehen jetzt im Git-Tab, dass der neue Ordner als unbekannt (mit gelbem Fragezeichen) da steht. Geben Sie Git Bescheid, dass Sie die Änderungen im Ordner verfolgen möchten (auf Stage klicken). Die neue Datei steht automatisch da.
3. Machen Sie ein Commit mit den bisherigen Änderungen (schreiben Sie eine sinnvolle Message dazu -- sinnvoll bedeutet nicht unbedingt lang) und danach einen Push.
4. Ersetzen Sie meinen Namen oben mit Ihrem. Klicken auf Stage, um die Änderung zu merken.
5. Ändern Sie das Datum auf heute. (Seien Sie ehrlich! Ich kann das sowieso am Commit sehen.)
6. Sie sehen jetzt, dass es zwei Symbole in der Status-Spalte gibt, eins für den Zustand im *Staging Area* (auch als *Index* bekannt), eins für den Zustand im Vergleich zum Staging Area. Sie haben die Datei modifiziert, eine Änderung in das Staging Area aufgenommen, und danach weitere Änderungen gemacht. Nur Änderungen im Staging Area werden in den Commit aufgenommen.
7. Stellen Sie die letzten Änderungen auch ins Staging Area und machen Sie einen Commit (immer mit sinnvoller Message!).
8. Vergessen Sie nicht am Ende, die Lizenz ggf. zu ändern!
# Verteilung von Noten
An der Uni Marburg nutzen wir 15 Punkte als Benotungskala (*Notenpunkte*). Wir nehmen an, dass der Mittelwert 8 NP (=3 im üblichen 1-5 System, was eigentlich einem durchschnittlichen Verständnis des Stoffes entsprechen soll) ist. Wie sieht dann die Verteilung der Noten aus? Wir müssen uns noch überlegen, was eine sinnvolle Standardabweichung für die Noten wäre. Vielleicht ist am leichtesten, wenn wir einfach ein paar ausprobieren und plotten. Wir fangen mit $\sigma = 3,4,5$ an. Das entspricht 1, 1.5, 2 Noten auf der 1-5 Skala.
```{r}
noten <- 1:15
mu <- 8
drei <- dnorm(noten,mean=mu,sd=3)
vier <- dnorm(noten,mean=mu,sd=4)
fuenf <- dnorm(noten,mean=mu,sd=5)
noten.dist <- data.frame(Notenpunkte=noten,drei,vier,fuenf)
noten.dist
```
Die Daten sind im sog. **wide format** (*breiten Format*), weil die verschiedenen Stufen einer Variable (hier: simulierte Standardabweichung) "breit", d.h. über mehrere Spalten hinweg, dargestellt werden. Obwohl viele es als "natürlich" betrachten, ist dieses Format in R nicht bevorzugt. Unter anderem haben wir hier mehrere Beobachtungen pro Zeile, was aus der Perspektive der Statistik ein bisschen durcheinander ist. R (und die Mathematik, die R Ihnen abnimmt) bevorzugt sog. **long format** (*langes Format*), wo es eine Beobachtung pro Zeile gibt. In diesem Format gibt es dann bei unserem Beispiel eine weitere Spalte "Standardabweichung" und die drei verschiedenen beobachteten Messwerte werden zusammen in eine Spalte gepackt. Das Paket `reshape2` bietet ein paar Hilfsfunktionen an, die das Umformatieren viel leichter machen. (Es gibt auch das Paket `reshape` vom selben Autor, das auch ähnliches macht. `reshape2` hat ein paar Verbesserungen eingeführt, die nicht ganz rückwärts kompatibel sind.)
Die Funktion heißt `melt()` (*schmelzen*) aus der Analogie zu Schmieden, wo die Daten (der Rohstoff) in eine schmiedbare bzw. flüssige Form gebracht werden. Aus dem Long-Format kann man ggf. die Daten in andere Formate mit `cast()` (*gießen*) konvertieren.
```{r}
library(reshape2)
# value.name is the name of the new column with the values that were previously spread out over several columns
# variable.name is the name of the new column with the old column names
melt(noten.dist,id.vars="Notenpunkte",value.name="P",variable.name="Standardabweichung")
```
Wir müssen den Output von `melt()` natürlich einer Variable zuweisen. Wir können die Ausgangsvariable "überschreiben":
```{r}
noten.dist <- melt(noten.dist,id.vars="Notenpunkte",value.name="P",variable.name="Standardabweichung")
```
Das funktioniert, weil alles rechts von `<-` zuerst gemacht wird. *Die Zuweisung findet erst nach der Evaluation der rechten Seite statt!* Jetzt können wir alle drei Verteilungen mit einem `ggplot`-Befehl grafisch darstellen.
```{r}
library(ggplot2)
# we use geom_line() because dnorm() already gave us the densities!
# we only use geom_density() when ggplot should calculate the density for us
ggplot(data=noten.dist,aes(x=Notenpunkte,y=P,color=Standardabweichung)) + geom_line() + scale_x_continuous(limits=c(0,16))
```
Ich habe die Grenzen der Grafik ein bisschen breiter gestellt, sodass man die Endpunkte klar sieht und Sie auch einen weiteren `ggplot`-Befehl kennen lernen.
Welche Verteilung sieht am fairsten aus? Warum?
Wir können das konkreter machen: welcher Anteil der Studenten bekommt bei den jeweiligen Verteilungen eine 1 (zumindest 13 NP)? Für die Verteilung mit $\sigma = 3$ sieht die Berechnung mit R so aus:
```{r}
pnorm(13,mean=mu,sd=3,lower.tail=FALSE)
```
Oder vielleicht interessiert uns der Anteil der Durchgefallenen (< 5NP):
```{r}
pnorm(5,mean=mu,sd=3)
```
Wenn wir das für alle drei Gruppen wiederholen möchten, ist es ziemlich ärgerlich, wenn wir jede Gruppe einzeln eingeben müssen. Dafür können wir eine **`for`-Schleife** nutzen:
```{r}
for(s in c(3,4,5) ){
durchfall <- pnorm(5,mean=mu,sd=s)
output <- paste("Bei einer Standabweichung von",s, "fallen",durchfall*100,"% durch.")
print(output)
}
```
Aber wir hoffen alle, dass wir doch eine gute Note bekommen. Fügen Sie einen Code-Block hier ein, der das gleiche aber mit "ausgezeichneten" Noten (=1 bzw. >= 13) macht. (Bei evtl. Copy-Paste nicht vergessen, "fallen...durch" durch etwas Passendes zu ersetzen!)
for(s in c(3,4,5) ){
sehrgut <- pnorm(13,mean=mu,sd=s,lower.tail=FALSE)
output <- paste("Bei einer Standabweichung von",s, "sind"
,sehrgut*100,"% sehr gut.")
print(output)
}
Wie steht die Anzahl guter Noten in Beziehung zur Anzahl schlechter Noten?
Es fallen mehr Studenten durch.
Warum?
Gute Noten sind 13,14,15. Durchgefallen ist man mit 5,4,3,2,1,0. Es gibt also mehr "Chancen" auf schlechte Noten.
## Kurtosis und Schiefe
Kurtosis (im Deutschen auch *Wölbung*) ist ein Maß dafür, wie spitz eine Verteilung ist. Die Normalverteilung wird nie zu extrem spitz -- der Gipfel bleibt immer schön rund, obwohl er manchmal eng wird. Andere Verteilungen (z.B. die Laplace-Verteilung ) haben Gipfel, die nicht rund sind.
Schiefe (*skewness*) beschriebt die (A)Symmetrie einer Verteilung. Eine Verteilung ist *linksschief*, wenn die linke Seite "breiter" ist, d.h., wenn sich der "Gipfel" auf der rechten Seite befindet.  Eine Verteilung ist *rechtsschief*, wenn die rechte Seite "breiter" ist, d.h., wenn sich der "Gipfel" auf der linken Seite befindet
Die Verteilung von Noten ist oft schief mit mehr guten Noten. Ist die Verteilung rechts- oder linksschief?
linksschief
Vielleicht hilft folgende Grafik mit der Visualisierung:
```{r echo=FALSE}
suppressPackageStartupMessages(library(sn))
qplot(x=1:15,y=dsn(1:15, xi=c(12), omega=1, alpha=-3, log=FALSE),geom="line",xlab="Notenpunkte",ylab="P")
```
## Von Perzentilen auf Häufikgeiten
Wir können die Perzentile in (absolute) Häufigkeiten übersetzen. Nehmen wir an, dass es 50 Studenten in einem Kurs gibt und dass die Noten wie oben normal verteilt sind. Dann können wir unserem Data.Frame eine weitere Spalte hinzufügen:
```{r}
n <- 50
noten.dist$Anzahl <- noten.dist$P * n
noten.dist$Anzahl
noten.dist
```
Jetzt können wir die absoluten Häufigkeiten auch plotten:
ggplot(data=noten.dist,aes(x=Notenpunkte,y=Anzahl,color=Standardabweichung)) + geom_line() + scale_x_continuous(limits=c(0,16))
Beantworten Sie ein paar Fragen über die Verteilung, indem Sie den passenden R-Code einsetzen:
1. Wie viele Studenten bekommen zwischen 7 und 9 NP bei einer Standardabweichung von 3?
mittel <- (pnorm(9,mean=mu,sd=3) - pnorm(7,mean=mu,sd=3) ) * n
output <-paste(mittel,"Studenten bekommen zwischen 7 und 9 Notenpunkten.")
print(output)
Wichtig: bei Wahrscheinlichkeits- und Häufigkeitsverteilung ist der linke Rand inklusiv aber der rechte Rand exklusiv! Das heißt, wir zählen hiermit die Leute, die bis zu 9 NP bekommen haben aber nicht die, die tatsächlich 9 NP bekommen haben!
2. Wie viele Studenten bekommen zumindest 10 NP?
gut <- (pnorm(15,mean=mu,sd=3) - pnorm(10,mean=mu,sd=3) ) * n
output <-paste(gut,"Studenten bekommen zumindest 10 Notenpunkte.")
print(output)
3. Wie viele Studenten bekommen weniger als 10 NP?
weniger <- pnorm(10,mean=mu,sd=3) * n
output <-paste(weniger,"Studenten bekommen weniger als 10 Notenpunkte.")
print(output)
4. Wie viele Studenten bekommen weniger als 8 NP?
oh <- pnorm(8,mean=mu,sd=3) * n
output <-paste(oh,"Studenten bekommen weniger als 8 Notenpunkte.")
print(output)
(Die Einrückung mit 4 Leerschlägen ist die Syntax für mehrere Absätze pro Punkt auf der Liste.)
## Von Noten zu Perzentilen -- ich möchte mich den anderen überlegen fühlen!
Manchmal will man in die andere Richtung gehen -- z.B. um die Frage beantworten zu können, welche Note man erreichen muss, um überdurchschnittlich zu sein. Dafür haben wir `qnorm()`. Überdurchschnittlich heißt "besser als die Hälfte abscheiden" (duh!) und wir nehmen wieder an, dass die Standardabweichung gleich 3 ist. Dann haben wir die Aussage:
paste("Um überdurchschnittlich zu sein, muss man mehr als",qnorm(0.5,mean=mu,sd=3),"Notenpunkte bekommen.")
Nicht so überraschend, dass "überdurchschnittlich" auch "mehr Punkte als den Durchschnitt bekommen" heißt! Wie sieht es aus, wenn wir besser als 99% der anderen abschließen möchten?
paste("Um in dem besten 1% abzuschließen, muss man zumindest",qnorm(0.99,mean=mu,sd=3),"Notenpunkte bekommen.")
## z-Transformation
Bei der Überprüfung der Lehrqualität scheint es der Verwaltung, dass ein gewisser Dozent andere Noten als andere Dozenten vergibt. Es wird entschieden, dass der Notenspiegel bei den Teilnehmern in einem von seinen Kursen getestet wird, um zu schauen, ob er sich von signifikant von der idealisierten Notenverteilung ($\mu=8,\sigma=3$) unterscheidet. Um zu zeigen, dass Gott $\alpha=0.06$ so viel liebt wie $\alpha=0.05$ `r citep("10.1037/0003-066X.44.10.1276")`, setzt die Verwaltung das Signikanzniveau auf 0.06.
paste("Der kritische Wert für einen einseitigen $z$-Test ist",qnorm(0.94),".")
paste("Die kritischen Werte für einen zweiseitigen $z$-Test sind",qnorm(0.97),".")
### Gibt es einen Unterschied?
Bei diesem Dozenten ist die Verwaltung wirklich unsicher, ob und was für einen Unterschied es geben könnte. (Welche Testart sollte man hier nutzen?)
In einem kleinen Seminar mit 7 Studenten beträgt der Durchschnittswert 10. Unterscheidet sich der Notenspiegel von dem idealen? Berechnen Sie den $z$-Test:
#z-Test Formel auf Folie 21 von Sitzung 8
#Notenspiegel für das kleine Seminar mit mu=10
drei <- dnorm(noten,mean=10,sd=3)
noten.dist.m10 <- data.frame(Notenpunkte=noten,drei)
noten.dist.m10
noten.dist.m10 <- melt(noten.dist.m10,id.vars="Notenpunkte",value.name="P",variable.name="Standardabweichung")
noten.dist.m10
kl <- 7
noten.dist.m10$Anzahl <- noten.dist.m10$P * kl
noten.dist.m10$Anzahl
noten.dist.m10
znoten <- (noten.dist.m10$Anzahl - mean (noten.dist.m10$Anzahl)) / sd(noten.dist.m10$Anzahl)
znoten
head(znoten, n=4)
znoten <- sqrt(7) * ((10-8)/3)
print(znoten)
paste("Die kritischen Werte für einen zweiseitigen $z$-Test sind",qnorm(0.97),".")
znoten <- sqrt(50) * ((10-8)/3)
print(znoten)
znoten <- sqrt(20) * ((7-8)/3)
print(znoten)
paste("Der kritische Wert für einen einseitigen $z$-Test ist",qnorm(0.94),".")
znoten <- sqrt(25) * ((7-8)/3)
print(znoten)
eegdata <- read.table("gra1_bp03_20_500r_P3.tab",header=T)
library(ez)
eegdata <- read.table("gra1_bp03_20_500r_P3.tab",header=T)
eegdata <- read.table("horton/gra1_bp03_20_500r_P3.tab",header=T)
eegdata <- read.table("horton/gra1_bp03_20_500r_P3.tab",header=T)
eegdata <- read.table("horton\gra1_bp03_20_500r_P3.tab",header=T)
eegdata <- read.table("gra1_bp03_20_500r_P3.tab",header=T)
setwd("C:/Users/Pia/Documents/horton")
eegdata <- read.table("gra1_bp03_20_500r_P3.tab",header=T)
roi <- rep("drop",length(eegdata$subj))
roi[eegdata$chan == "F7"] <- "Left Anterior"
roi[eegdata$chan == "F3"] <- "Left Anterior"
roi[eegdata$chan == "FC5"] <- "Left Anterior"
roi[eegdata$chan == "FC1"] <- "Left Anterior"
roi[eegdata$chan == "F8"] <- "Right Anterior"
roi[eegdata$chan == "F4"] <- "Right Anterior"
roi[eegdata$chan == "FC6"] <- "Right Anterior"
roi[eegdata$chan == "FC2"] <- "Right Anterior"
roi[eegdata$chan == "P7"] <- "Left Posterior"
roi[eegdata$chan == "P3"] <- "Left Posterior"
roi[eegdata$chan == "CP5"] <- "Left Posterior"
roi[eegdata$chan == "CP1"] <- "Left Posterior"
roi[eegdata$chan == "P8"] <- "Right Posterior"
roi[eegdata$chan == "P4"] <- "Right Posterior"
roi[eegdata$chan == "CP6"] <- "Right Posterior"
roi[eegdata$chan == "CP2"] <- "Right Posterior"
roi[eegdata$chan == "FZ"] <- "Midline Anterior"
roi[eegdata$chan == "FCZ"] <- "Midline Anterior"
roi[eegdata$chan == "CZ"] <- "Midline Posterior"
roi[eegdata$chan == "PZ"] <- "Midline Posterior"
eegdata <- cbind(eegdata,roi)
eegdata
eegdata <- cbind(eegdata,roi)
eegdata <- subset(eegdata,roi != "drop")
eegdata
eegdata$verbart <- rep("acc",length(eegdata$subj))
eegdata
eegdata[substr(eegdata$cond,1,1)== "d",]$verbart <- "dat"
eegdata
eegdata$pronomen <- rep("acc",length(eegdata$subj))
eegdata$pronomen
eegdata$pronomen <- rep("acc",length(eegdata$subj))
eegdata[substr(eegdata$cond,2,2)== "d",]$pronomen <- "dat"
eegdata$grammatisch <- (eegdata$verbart == eegdata$pronomen)
eegdata$grammatisch
eegdata$npart <- rep("pro",length(eegdata$subj))
eegdata[substr(eegdata$cond,3,3)== "n",]$npart <- "np"
eegdata$npart
eegdata$mismatch <- rep(FALSE,length(eegdata$subj))
eegdata[substr(eegdata$cond,4,4)== "m",]$mismatch <- TRUE
eegdata$mismatch
eegdata$np <- with(eegdata
,paste0(npart
,ifelse(npart=="np"
,ifelse(mismatch,"-mismatch","-match")
,"")
)
)
eegdata$np
eegdata$subj <- factor(eegdata$subj)
eegdata$subj
eegdata$cond <- factor(eegdata$cond)
eegdata$cond
eegdata$chan <- factor(eegdata$chan)
eegdata$chan
eegdata$roi <- factor(eegdata$roi)
eegdata$roi
eegdata$win <- factor(eegdata$win)
eegdata$win
eegdata$verbart
eegdata$pronomen <- factor(eegdata$pronomen)
eegdata$pronomen
eegdata$grammatisch <- factor(eegdata$grammatisch)
eegdata$grammatisch
eegdata$npart <- factor(eegdata$npart)
eegdata$npart
eegdata$mismatch <- factor(eegdata$mismatch)
eegdata$mismatch
eegdata$np <- factor(eegdata$np)
eegdata$np
for(w in levels(eegdata$win)){
eegdata.anova <- ezANOVA(subset(eegdata,win==w)
,dv = .(mean)
,wid = .(subj)
,within =.(roi,verbart,np,grammatisch)
)
print(paste0("Time Window: ",w,"ms"),quote=F)
print(eegdata.anova)
}
library(ez)
setwd("C:/Users/Pia/Documents/horton")
eegdata <- read.table("gra1_bp03_20_500r_P3.tab",header=T)
# ROIs Region of Interest -- we divide the scalp up into regions
roi <- rep("drop",length(eegdata$subj))
roi[eegdata$chan == "F7"] <- "Left Anterior"
roi[eegdata$chan == "F3"] <- "Left Anterior"
roi[eegdata$chan == "FC5"] <- "Left Anterior"
roi[eegdata$chan == "FC1"] <- "Left Anterior"
roi[eegdata$chan == "F8"] <- "Right Anterior"
roi[eegdata$chan == "F4"] <- "Right Anterior"
roi[eegdata$chan == "FC6"] <- "Right Anterior"
roi[eegdata$chan == "FC2"] <- "Right Anterior"
roi[eegdata$chan == "P7"] <- "Left Posterior"
roi[eegdata$chan == "P3"] <- "Left Posterior"
roi[eegdata$chan == "CP5"] <- "Left Posterior"
roi[eegdata$chan == "CP1"] <- "Left Posterior"
roi[eegdata$chan == "P8"] <- "Right Posterior"
roi[eegdata$chan == "P4"] <- "Right Posterior"
roi[eegdata$chan == "CP6"] <- "Right Posterior"
roi[eegdata$chan == "CP2"] <- "Right Posterior"
roi[eegdata$chan == "FZ"] <- "Midline Anterior"
roi[eegdata$chan == "FCZ"] <- "Midline Anterior"
roi[eegdata$chan == "CZ"] <- "Midline Posterior"
roi[eegdata$chan == "PZ"] <- "Midline Posterior"
eegdata <- cbind(eegdata,roi)
eegdata <- subset(eegdata,roi != "drop")
eegdata$verbart <- rep("acc",length(eegdata$subj))
eegdata[substr(eegdata$cond,1,1)== "d",]$verbart <- "dat"
eegdata$pronomen <- rep("acc",length(eegdata$subj))
eegdata[substr(eegdata$cond,2,2)== "d",]$pronomen <- "dat"
eegdata$grammatisch <- (eegdata$verbart == eegdata$pronomen)
eegdata$npart <- rep("pro",length(eegdata$subj))
eegdata[substr(eegdata$cond,3,3)== "n",]$npart <- "np"
eegdata$mismatch <- rep(FALSE,length(eegdata$subj))
eegdata[substr(eegdata$cond,4,4)== "m",]$mismatch <- TRUE
eegdata$np <- with(eegdata
,paste0(npart
,ifelse(npart, "-np", "-mismatch")
,ifelse(mismatch=="match")
,"")
)
)
eegdata$np <- with(eegdata
,paste0(npart
,ifelse(mismatch=="match")
,ifelse(npart, "-np", "-mismatch")
,"")
)
)
aphasiker <- read.csv2("Data/aphasiker.csv",header = T)
head(aphasiker)
qplot(x=Lex_Dec,
data=na.omit(aphasiker),
geom="density",
fill=Aphasie, alpha=I(0.3))
?qplot
??qplot
ggplot
libary(ggplot)
broca.lex.dec <- aphasiker[aphasiker$Aphasie == "B","Lex_Dec"]
wernicke.lex.dec <- aphasiker[aphasiker$Aphasie == "W","Lex_Dec"]
opts_chunk$set(cache=TRUE,prompt=TRUE)
library(knitcitations)
library(ggplot2)
library(reshape2)
cite_options(tooltip = TRUE
, linked = TRUE
, numerical = TRUE
, bibtex_data = FALSE)
aphasiker <- read.csv2("Data/aphasiker.csv",header = T)
head(aphasiker)
qplot(x=Lex_Dec,
data=na.omit(aphasiker),
geom="density",
fill=Aphasie, alpha=I(0.3))
broca.lex.dec <- aphasiker[aphasiker$Aphasie == "B","Lex_Dec"]
wernicke.lex.dec <- aphasiker[aphasiker$Aphasie == "W","Lex_Dec"]
mean(broca.lex.dec)
mean(wernicke.lex.dec)
zaehler = mean(broca.lex.dec) - mean(wernicke.lex.dec)
zaehler
pooled = (var(broca.lex.dec) + var(wernicke.lex.dec)) / 2
pooled
nenner = sqrt((pooled/length(broca.lex.dec)) +  (pooled/length(wernicke.lex.dec))  )
nenner
t_wert = zaehler / nenner
t_wert
dfs = length(broca.lex.dec) - 1 + length(wernicke.lex.dec) - 1
dfs
abs(qt(0.025,df=16)) # symmetrische Verteilung, zweiseitiger Test
var.equal=TRUE)
# wenn Varianzhomogenität vorliegt
t.test(broca.lex.dec,wernicke.lex.dec,
var.equal=TRUE)
var.equal=TRUE,alternative="less")
t.test(broca.lex.dec,wernicke.lex.dec,
var.equal=TRUE,alternative="less")
t.test(broca.lex.dec,wernicke.lex.dec,
var.equal=TRUE,alternative="greater")
aphasie.bw <- aphasiker[aphasiker$Aphasie == "W" | aphasiker$Aphasie == "B", c("Aphasie","Lex_Dec")]
head(aphasie.bw)
t.test(aphasie.bw$Lex_Dec ~ aphasie.bw$Aphasie, var.equal = TRUE)
var.test(aphasie.bw$Lex_Dec ~ aphasie.bw$Aphasie)
library(car)
leveneTest(aphasie.bw$Lex_Dec ~ aphasie.bw$Aphasie)
t.test(broca.lex.dec,wernicke.lex.dec,
var.equal=TRUE)
t.test(broca.lex.dec,wernicke.lex.dec)
kurs <- read.table("Data/body_dim_long.tab",header = T)
klinisch <- subset(kurs, major=="M.A..Klinische.Linguistik")
speech <- subset(kurs, major=="M.A..Speech.Science")
var.test(klinisch$age,speech$age)
var.test(klinisch$age ~ speech$age)
var.test(klinisch$weight, speech$weight)
var.test(klinisch$height,speech$height)
shapiro.test(broca.lex.dec)
shapiro.test(wernicke.lex.dec)
plot(density(broca.lex.dec)
plot(density(broca.lex.dec, geom="density")
qplot(density(broca.lex.dec, geom="density")
qplot(wernicke.lex.dec,geom="density")
plot(density(wernicke.lex.dec))
plot(density(broca.lex.dec))
vorher  <- c(65.4, 77.6, 66.9, 61.3, 66.5, 67.4, 73.9, 72.6, 70.7, 68.9)
nachher <- c(47.8, 50.1, 58.9, 67.2, 75.5, 58.1, 69.8, 59.4, 44.3, 63.8)
nachher - vorher
diff <- nachher - vorher
diff.sd <- sd(diff)
diff.sd
diff.se <- diff.sd / sqrt(length(diff))
diff.se
t <- mean(diff) / diff.se
t
pt(t,df=length(diff)-1) * 2
qt(0.025,df=length(diff)-1)
qt(1 - 0.025,df=length(diff)-1)
t.test(nachher,vorher,paired = TRUE)
t.test(nachher,vorher,paired = TRUE, alternative = "less")
library(ggplot2)
library(car)
rt <- read.table("punkt_rt.tab",header=TRUE)
rt <- read.table("Schoknec/punkt_rt.tab",header=TRUE)
print(summary(rt))
rt$subj <- as.factor(rt$subj)
rt.plot <- qplot(x=RT,color=subj,fill=subj,data=rt, geom="density",alpha=I(0.3))
print(rt.plot)
savehistory("~/Statistik-f-r-Sprachwissenschaftler/13_05.R")
