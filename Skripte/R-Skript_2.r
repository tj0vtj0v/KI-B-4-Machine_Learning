
#################################################################
# Deskriptive Statistik
#################################################################
# von Prof. Dr. Robert Hable, Technische Hochschule Deggendorf
#################################################################


# Einlesen eines Datensatzes wie gehabt:

  setwd("C:/Users/rhable.AIKIW10NB01/Nextcloud2/Lehre/Daten/BA-KI_Maschinelles-Lernen")
  Daten <- read.csv("Koerpergewicht.csv",header=TRUE,sep=";",fill=TRUE,stringsAsFactors=TRUE)

# Im folgenden werden verschiedene statistische Kennzahlen für
# die Spalte "Groesse" berechnet.

# Hierzu speichern wir die Spalte "Groesse" des Datensatzes erstmal 
# extra als "x" ab:

  x <- Daten[,"Groesse"] 

# Mittelwert

  mean(x)

# Median

  median(x)

# Eine Zusammenfassung der Daten

  summary(x)

# Maximum und Minimum

  max(x)
  min(x)

# Quantile

  quantile(x,0.3)  # 30%-Quantil


# Standardabweichung 

  sd(x)

# Robuste Alternativen zur Standardabweichung

  IQR(x)  # Interquartilsabstand
  quantile(x,0.75)-quantile(x,0.25)  # Interquartilsabstand


# Der summary-Befehl kann auch für den kompletten Datensatz verwendet werden
# und gibt dann für jede Spalte ein 5-Number-Summary bzw. die Häufigkeiten aus

  summary(Daten)



# Korrelationen

  # Hierzu speichern wir zunächst die Spalte "Gewicht" noch als "y" ab:
  
    y <- Daten[,"Gewicht"]

  # gewöhnliche Korrelation (Pearson)

    cor(x,y)

  # Es ginge auch direkt ohne x und y, aber das ist dann etwas unübersichtlicher:
  
    cor(Daten[,"Groesse"],Daten[,"Gewicht"])
     
 
  
#########################################################
# Punktewolken und Grafikoptionen
######################################

  # Die Spalten "Groesse" und "Gewicht" als "x" und "y" abgespeichert:

    x <- Daten[,"Groesse"]
    y <- Daten[,"Gewicht"]


  # Zeichnen einer Punktewolke

    plot(x,y)


  # Setzen von Grafikoptionen (vgl. Kapitel 1)

    plot(x,y,main="Groesse und Gewicht",xlim=c(1.5,2),ylim=c(50,100),xlab="Größe",ylab="Gewicht")


  # Ändern, wie die Punkte aussehen mit Grafikoption 'pch'

    plot(x,y,main="Groesse und Gewicht",xlim=c(1.5,2),ylim=c(50,100),xlab="Größe",ylab="Gewicht",pch=19)
    plot(x,y,main="Groesse und Gewicht",xlim=c(1.5,2),ylim=c(50,100),xlab="Größe",ylab="Gewicht",pch=17)


  # Ändern der Farbe mit Grafikoption 'col'

    plot(x,y,main="Groesse und Gewicht",xlim=c(1.5,2),ylim=c(50,100),xlab="Größe",ylab="Gewicht",pch=19,col="blue")


  # Hinzufügen einer Linie in eine bestehende Grafik

    plot(x,y,main="Groesse und Gewicht",xlim=c(1.5,2),ylim=c(50,100),xlab="Größe",ylab="Gewicht",pch=19,col="blue")
    abline(a=95,b=-10,col="red")    # Hierbei ist a der Intercept und b die Steigung (slope)


  # Dasselbe nochmal, jetzt aber mit anderer Liniendicke (vgl. Kapitel 1)

    plot(x,y,main="Groesse und Gewicht",xlim=c(1.5,2),ylim=c(50,100),xlab="Größe",ylab="Gewicht",pch=19,col="blue")
    abline(a=95,b=-10,col="red",lwd=2)    # Hierbei ist a der Intercept und b die Steigung (slope)


  # Nochmal die Punktewolke, wobei nun aber das Geschlecht farblich markiert ist:
  
    # Trenne zunächst den Datensatz in Männer und Frauen
      Frauen <- subset(Daten,Geschlecht=="weiblich")
      Maenner <- subset(Daten,Geschlecht=="maennlich")
    
    # Zeichne nun mit 'plot' nur die Frauen in rot
      x <- Frauen[,"Groesse"]
      y <- Frauen[,"Gewicht"]
      plot(x,y,main="Groesse und Gewicht",xlim=c(1.5,2),ylim=c(50,100),xlab="Größe",ylab="Gewicht",pch=19,col="red")
      
    # Füge mit 'points' nun die Männer in blau ein
      x <- Maenner[,"Groesse"]
      y <- Maenner[,"Gewicht"]
      points(x,y,pch=17,col="blue")




  # Und zum Abschluss noch mehrere Bilder auf einmal

    par(mfrow=c(2,3))  # Das heißt: Jetzt kommen gleich 6 Bilder,
                       # die in jeweils 2 Zeilen mit 3 Bildern
                       # gemalt werden sollen

    # 1. Bild:
      x <- Daten[,"Groesse"]
      y <- Daten[,"Gewicht"]
      plot(x,y,main="Groesse und Gewicht",xlim=c(1.5,2),ylim=c(50,100))
      
    # 2. Bild
      plot(x,y,main="Groesse und Gewicht",xlim=c(1.5,2),ylim=c(50,100),xlab="Größe",ylab="Gewicht")

    # 3. Bild
      plot(x,y,main="Groesse und Gewicht",xlim=c(1.5,2),ylim=c(50,100),xlab="Größe",ylab="Gewicht",pch=19)

    # 4. Bild:
      x <- Frauen[,"Groesse"]
      y <- Frauen[,"Gewicht"]
      plot(x,y,main="Frauen",xlim=c(1.5,2),ylim=c(50,100),xlab="Größe",ylab="Gewicht",pch=19,col="red")

    # 5. Bild 
      x <- Maenner[,"Groesse"]
      y <- Maenner[,"Gewicht"]
      plot(x,y,main="Männer",xlim=c(1.5,2),ylim=c(50,100),xlab="Größe",ylab="Gewicht",pch=17,col="blue")
    
    # 6. Bild
      x <- Frauen[,"Groesse"]
      y <- Frauen[,"Gewicht"]
      plot(x,y,main="Groesse und Gewicht",xlim=c(1.5,2),ylim=c(50,100),xlab="Größe",ylab="Gewicht",pch=19,col="red")      
      x <- Maenner[,"Groesse"]
      y <- Maenner[,"Gewicht"]
      points(x,y,pch=17,col="blue")




#########################################################
# Boxplots
######################################

  # Körpergroßen von Frauen, Männern und allen zusammen

    x <- Frauen[,"Groesse"]
    y <- Maenner[,"Groesse"]
    z <- Daten[,"Groesse"]


  # ein Boxplot

    boxplot(x)


  # Zwei Boxplots nebeneinander und mit Überschriften

    par(mfrow=c(1,2))
    boxplot(x,main="Frauen")
    boxplot(y,main="Männer")


  # Besser, man macht die Achsen einheitlich

    par(mfrow=c(1,2))
    boxplot(x,main="Frauen",ylim=c(1.5,2))
    boxplot(y,main="Männer",ylim=c(1.5,2))

  # Noch besser man macht die beiden Boxplots in ein Bild:

    boxplot(x,y)

  # Noch besser man macht die beiden Boxplots in ein Bild
  # und vergibt auch Namen

    boxplot(x,y,main="Körpergröße",names=c("Frauen","Männer"))

  # Dasselbe geht auch mit noch mehr Boxplots:

    boxplot(x,y,z,main="Körpergröße",names=c("Frauen","Männer","Gesamt"))



#########################################################
# Histogramme
######################################

  # Körpergewicht

    x <- Daten[,"Gewicht"]


  # Ein Histogramm

    hist(x,freq=FALSE)


  # Das ganze läßt sich wieder beliebig verändern

    hist(x,freq=FALSE,main="Das ist jetzt meine Überschrift",ylim=c(0,0.2),xlab="Gewicht",ylab="Dichte")


  # Mann kann auch angeben, wo man die Balkengrenzen haben will:

    grenzen <- c(55,70,75,85,100)
    grenzen

    hist(x,freq=FALSE,breaks=grenzen)




#########################################################
# Balkendiagramme
######################################


  # Zunächst wird ein Vektor mit den Höhen der Balken definiert

    a <- c(1,5.2,-2,3.4,2,0,-1,4)

  # Dann der Vektor mit den Namen

    b <- c("Partei 1","Partei 2","Partei 3","Partei 4","Partei 5","Partei 6","Partei 7","Partei 8")

  # Jetzt das Balkendiagramm

    barplot(height=a,names.arg=b, main="Gewinne und Verluste")


  # Das Balkendiagramm mit ein paar Grafikoptionen

    barplot(height=a,names.arg=b, main="Gewinne und Verluste",ylim=c(-3,6),col="blue")




#########################################################
# Kontingenztafel und Mosaikplot
######################################

# Laden der Patientendaten

    setwd("C:/Users/rhable.AIKIW10NB01/Nextcloud2/Lehre/Daten/BA-KI_Maschinelles-Lernen")
  Daten <- read.csv("Patientendaten.csv",header=TRUE,sep=";",fill=TRUE,stringsAsFactors=TRUE)


# Ausgabe der ersten 5 Datenpunkte

  Daten[1:5,]
  
  
# Kontrolle der Datentypen mit dem summary-Befehl

  summary(Daten)
  
# Korrektur des Datentyps

  Daten[,"Krankheit"] <- as.factor(Daten[,"Krankheit"])

# Ausgabe der Zusammenfassung

  summary(Daten)

# Ausgabe von Kontingenztafeln für die nominalen Variablen:

  Daten.kategoriell <- Daten[,c("Krankheit","Geschlecht","Raucher")]
  table(Daten.kategoriell)

# der Mosaikplot

  Kontingenztafeln <- table(Daten.kategoriell)
  mosaicplot(Kontingenztafeln)


#########################################################
# Tortendiagramme
######################################


  # Ein Tortendiagramm zu machen ist immer falsch.
  # Deswegen besprechen wir das erst gar nicht.
  
  # Vgl.: Bild Pie Chart










