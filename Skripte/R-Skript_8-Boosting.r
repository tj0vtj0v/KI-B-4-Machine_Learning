

#################################################################
# Maschinelles Lernen mit L2-Boosting
#################################################################
# von Prof. Dr. Robert Hable, Technische Hochschule Deggendorf
#################################################################


# Für maschinelles Lernen muss erst ein Zusatzpaket in R installiert werden
# Wir verwenden im folgenden das Paket "mboost"
# Pakete muss man (einmalig) mit folgendem Befehl installieren
# (Internetverbindung nötig!):

# install.packages("mboost")

# Damit wird das Paket auf dem eigenen Computer installiert. Immer wenn
# man das Paket verwenden will, muss man es aber in der aktuellen
# R-Sitzung aus der Bibliothek laden. Das geht folgendermaßen:

  library(mboost)



######################################
# Regression mit L2-Boosting
#############


###
# Wir verwenden wieder unsere Autodaten als Beispieldaten:

  # Setzen des Pfades und Einlesen der Beispiel-Daten

      setwd("C:/Users/rhable.AIKIW10NB01/Nextcloud2/Lehre/Daten/BA-KI_Maschinelles-Lernen")
    Daten <- read.csv("autos.csv",header=TRUE,sep=",",fill=TRUE,stringsAsFactors=TRUE)
    Daten[1:10,]

  # Kontrolle der Datentypen durch Ausgabe der Summary
  
    summary(Daten)
    Daten[,"Herkunft"] <- as.factor(Daten[,"Herkunft"])
    summary(Daten)

  # Vertauschung der Reihenfolge der Daten
    n <- length(Daten[,1])
    n
    index <- sample(1:n,n,replace=FALSE)
    Daten <- Daten[index,]
  # Aufteilung in Training/Validierung und Test
    Daten.train <- Daten[1:274,]
    Daten.test <- Daten[275:391,]


  # Berechnung des Modells (noch nicht kreuzvalidiert)

    model <- gamboost(Verbrauch ~ Zylinder + Hubraum + Leistung + Gewicht + Beschleunigung + Baujahr + Herkunft, 
                      data=Daten.train, dfbase = 4, control = boost_control(mstop = 1000))
    model
    par(mfrow=c(1,7)) # Weil es 7 Einflussvariablen sind
    plot(model)
    
    
  # Kreuzvalidierung:
  
    cv10f <- cv(model.weights(model), type = "kfold")
    cvm <- cvrisk(model, folds = cv10f, papply = lapply)
    print(cvm)
    mstop(cvm)
    plot(cvm)

  # Berechnung des Modells (nun kreuzvalidiert), 
  # daher steht in den Optionen: 'mstop = mstop(cvm)'

    model <- gamboost(Verbrauch ~ Zylinder + Hubraum + Leistung + Gewicht + Beschleunigung + Baujahr + Herkunft, 
                      data=Daten.train, dfbase = 4, control = boost_control(mstop = mstop(cvm)))
    model
    par(mfrow=c(1,7))  # Weil es 7 Einflussvariablen sind
    plot(model)


  # Berechnung der Prognoseergebnisse auf den Testdaten:

    X.test <- Daten.test[,c("Zylinder","Hubraum","Leistung","Gewicht","Beschleunigung","Baujahr","Herkunft")]
    prognosen <- predict(model,X.test)

# Berechnung des mittleren Prognosefehlers (MAE)

    y.test <- Daten.test[,"Verbrauch"]
    mean(abs(y.test-prognosen))











