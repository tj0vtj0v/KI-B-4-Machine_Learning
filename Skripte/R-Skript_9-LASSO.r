

#################################################################
# Variablenselektion mit LASSO
#################################################################
# von Prof. Dr. Robert Hable, Technische Hochschule Deggendorf
#################################################################


# F�r LASSO muss erst ein Zusatzpaket in R installiert werden
# Wir verwenden im folgenden das Paket "glmnet"
# Pakete muss man (einmalig) mit folgendem Befehl installieren
# (Internetverbindung n�tig!):

# install.packages("glmnet")

# Damit wird das Paket auf dem eigenen Computer installiert. Immer wenn
# man das Paket verwenden will, muss man es aber in der aktuellen
# R-Sitzung aus der Bibliothek laden. Das geht folgenderma�en:

  library(glmnet)


###
# Wir verwenden einen Datensatz zur Qualit�t verschiedener Weine als Beispieldaten:

# Setzen des Pfades und Einlesen der Beispiel-Daten

    setwd("C:/Users/rhable.AIKIW10NB01/Nextcloud2/Lehre/Daten/BA-KI_Maschinelles-Lernen")
  Daten <- read.csv("winequality-white.csv",header=TRUE,sep=";",fill=TRUE,stringsAsFactors=TRUE)
  Daten[1:10,]

# Kontrolle der Daten, insbesondere auch der Datentypen durch Ausgabe der Summary
  
  summary(Daten)

# Jetzt werden die Daten noch zuf�llig durcheinandergew�rfelt
   n <- length(Daten[,1])
   Index <- sample(seq(1,n,1), replace=FALSE)
   Daten <- Daten[Index,]
   rownames(Daten) <- 1:n




###################################################
# Variablenselektion mit LASSO
# hier ohne Aufteilung in Training- und Testdaten
####

# Laden des R-Pakets

  library(glmnet)

# erste Modellberechnung mit automatischer Kreuzvaidierung 
# des Tuningparameters lambda
# im Befehl 'coef' gibt es dabei 2 m�gliche Einstellungen, die entscheiden
# wie restriktiv das lambda beim Tuning eingestellt wird:
#     - s="lambda.min": Standardeinstellung (normale Kreuzvalidierung) 
#     - s="lambda.1se": restriktivere Einstellung (mehr Variablen werden entfernt)


  # Erstellen eines Datensatzes mit Dummy-Codierung der kategoriellen Variablen
  # alle Variablen au�er die Zielvariable werden als Einflussvariablen verwendet
    X <- model.matrix(quality ~. , Daten)
    X <- X[,-1]   # entferne den Intercept
    summary(X)
  # die Zielvariable wird als y gespeichert  
    y <- Daten[,"quality"]

  # Berechnung von LASSO mit der Standardeinstellung s="lambda.min" 
    model.lasso <- cv.glmnet(X,y)
    coef(model.lasso,s="lambda.min")

  # Wiederholung der Berechnung von LASSO mit der restriktiveren Einstellung s="lambda.1se" 
    model.lasso <- cv.glmnet(X,y)
    coef(model.lasso,s="lambda.1se")


# nun wird die Berechnung 100 mal wiederholt und dann werden die Variablen
# genommen, die nicht nur zuf�llig ein paar mal ausgew�hlt wurden, sondern
# die h�ufig ausgew�hlt wurden
# Wir verwenden hier bei diesem Datensatz die restriktivere Einstellung 

  m <- length(X[1,])
  total.numbers <- rep(0,m)

  RUNS <- 100

  for( run in 1:RUNS ){

    model.lasso <- cv.glmnet(X,y)
    beta <- coef(model.lasso,s="lambda.1se")[-1,1]  # Vektor der Koeffizienten (ohne Intercept)
    total.numbers <- total.numbers + ifelse( beta != 0, 1, 0)  # Auswahl der Koeffizienten die ungleich Null sind

  }

  total.numbers <- as.matrix(total.numbers)  # wie oft wurde welche Variable gew�hlt
  rownames(total.numbers) <- names(beta)     # die Zeilen sollen die Namen der Variablen haben
  total.numbers


# Nun speichere ich einen neuen Datensatz, bei dem die nicht-ausgew�hlten Einflussvariablen
# entfernt wurden
# In diesem Beispiel entferne ich alle Variablen, die weniger als 10 mal im LASSO gew�hlt wurden

  # Zun�chst speichere ich die Namen aller ausgew�hlten Variablen in einen Vektor
  # das geschieht hier automatisch, aber man kann es nat�rlich auch h�ndisch abtippen
    Schwelle <- 10
    temp <- total.numbers[total.numbers[,1] >= Schwelle, 1]
    auswahl <- names(temp)

  # der neue Datensatz, hierbei darf ich die Zielvariable nat�rlich nicht vergessen
    Daten.neu <- Daten[,c("quality",auswahl)]

#########################################
# Lineare Regression auf dem neuen Datensatz mit den ausgew�hlten Variablen

  model <- lm(quality ~ fixed.acidity + volatile.acidity + residual.sugar + chlorides 
                        + free.sulfur.dioxide + pH + sulphates + alcohol,data=Daten)

  model
  
# Berechnung der Prognoseg�te

  y <- Daten.neu[,"quality"] 
  Prognosen <- model$fitted.values 
  Prognosefehler <- mean( abs( y - Prognosen ) )
  Prognosefehler

# Zum Vergleich: die Prognoseg�te des simplen Medians 
# der Median der Qualit�tsbewertungen quality ist hier 6
  median(y)
  mean( abs( y - 6 ) )






