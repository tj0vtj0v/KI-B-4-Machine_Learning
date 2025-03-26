

######################################
# for-Schleife
#####################################


# in folgender for-Schleife wird sin(2*k+1) für k=1,...,100 berechnet

  # anlegen eines Vektors
    n <- 100
    z <- rep(0,n)

  # die for-Schleife
    for( k in 1:n ){

         z[k] <- sin(2*k+1)

    }

  # schneller geht es, wenn man die for-Schleife vermeidet und
  # Vektor-wertig rechnet:

    K <- seq(1,n,1)
    z <- sin(2*K+1)


# noch ein Beispiel für eine for-Schleife: Berechnung der Summe 1+2+...+100

    n <- 100
    Summe <- 0
    for( i in 1:n ){

         Summe <- Summe + i

    }
    Summe



##########################################
# if-Befehl und if-else
##########################################

# Ein Beispiel für einen if-Befehl in einer for-Schleife:
# alle negativen Werte sollen auf 0 gesetzt werden

  # anlegen eines Beispielvektors (mit pos. und neg. Zahlen)

    z <- rnorm(20,0,1)  # hier werden 20 Zufallszahlen aus einer Normalverteilung gezogen
    z
    length(z)

  # die for-Schleife mit dem if-Befehl

    for( i in 1:length(z) ){

         if( z[i] < 0 ){

             z[i] <- 0

         }

     }

     z

# obiges Beispiel geht in R eleganter und schneller mit dem
# ifelse-Befehl, der auch Vektor-wertig funktioniert

    z <- rnorm(20,0,1)
    z

    z <- ifelse( z < 0, 0, z )

    z



##########################################
# Aufteilen des Datensatzes in Training und Test mit Zufallsgenerator
##########################################

# Einlesen von Daten

    setwd("C:/Users/rhable.AIKIW10NB01/Nextcloud2/Lehre/Daten/BA-KI_Maschinelles-Lernen")
  Daten <- read.csv("Mietspiegel.csv",header=TRUE,sep=";",fill=TRUE,stringsAsFactors=TRUE)

# Ausgabe der ersten 20 Datenpunkte und der Summary

  Daten[1:20,]
  summary(Daten)

# Aufteilen der Daten in Training und Test
# hierzu wird die Reihenfolge der Daten zufaellig vertauscht.
# Es sind 1904 Datenpunkte, also werden bei 70/30-Aufteilung als Trainingsdaten
# 1333 Punkte und als Testdaten die restlichen 571 Punkte verwendet.

  # Vertauschung der Reihenfolge der Daten
    n <- length(Daten[,1])
    n
    index <- sample(1:n,n,replace=FALSE)
    Daten <- Daten[index,]
  # Aufteilung in Training und Test
    Daten.train <- Daten[1:1333,]
    Daten.test <- Daten[1334:1904,]




