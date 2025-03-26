

#################################################################
# Maschinelles Lernen mit SVMs
#################################################################
# von Prof. Dr. Robert Hable, Technische Hochschule Deggendorf
#################################################################


# Für maschinelles Lernen muss erst ein Zusatzpaket in R installiert werden
# Wir verwenden im folgenden das Paket "e1071"
# Pakete muss man (einmalig) mit folgendem Befehl installieren
# (Internetverbindung nötig!):

# install.packages("e1071")

# Damit wird das Paket auf dem eigenen Computer installiert. Immer wenn
# man das Paket verwenden will, muss man es aber in der aktuellen
# R-Sitzung aus der Bibliothek laden. Das geht folgendermaßen:

  library(e1071)



######################################
# Regression mit SVR (Support Vector Regression)
#############



###
# Wir verwenden wieder unsere Körpergewichtsdaten als Beispieldaten:

  # Setzen des Pfades und Einlesen der Beispiel-Daten

      setwd("C:/Users/rhable.AIKIW10NB01/Nextcloud2/Lehre/Daten/BA-KI_Maschinelles-Lernen")
    Daten <- read.csv2("Koerpergewicht_2.csv",header=TRUE,sep=";",fill=TRUE,stringsAsFactors=TRUE)
    Daten[1:10,]

  # Kontrolle der Datentypen durch Ausgabe der Summary
  
    summary(Daten)

  # Definition der Tuning-Parameter

    cc <- seq(-5,10,1)    # für mögliche Werte von "Cost" (Tuningparameter)
    cg <- seq(-4,1,0.5)   # für mögliche werte von "gamma" (Tuningparameter)

  # Berechnung des Modells

    tuning <- tune.svm(Gewicht ~ Geschlecht + Alter + Groesse, data=Daten, scale = TRUE, type = "eps-regression", kernel = "radial",
                     gamma = 10^cg, cost = 2^cc, epsilon = 0.1,      tunecontrol = tune.control(sampling = "cross",cross=5))

    print(tuning)

  # Speichern des Modells mit den besten Parametern:

    model <- tuning$best.model    # das Model mit optimalen Tuningparametern

  # Berechnen von Prognosen

    # Berechnung der Prognosen für die Personen aus dem Datensatz:
    
      # Auswahl aller Input-Variablen und Speicherung unter 'X'
        X <- Daten[,c("Geschlecht","Alter","Groesse")]

      # Berechnung der Prognoseergebnisse:
        predict(model,X)
      
    # Berechnung einer Prognose für einen neuen Datenpunkt
    
      # Hinzufügen eines neuen Datenpunkts (Mann mit 37 Jahren und 1,75 m)
        x.neu <- data.frame("maennlich",37,1.75)
        names(x.neu) <- names(X)
        X <- rbind(X,x.neu)
        
      # Berechnung der Prognosen
        predict(model,X)


################
# Nun wiederholen wir die Berechnung nochmal, wobei wir den Datensatz
# vorher in Test- und Trainingsdaten aufteilen:

# Aufteilen der Daten:
  # Anzahl der Daten
    length(Daten[,1])
  # Da es 42 Datenpunkte sind, werden bei einer (70% zu 30% Aufteilung)
  # 29 Datenpunkte als Trainingsdaten gewählt und die restlichen datenpunkte als
  # testdaten
    Daten.train <- Daten[1:29,]
    Daten.test <- Daten[30:42,]

  # Berechnung des Modells auf den Trainingsdaten:
  # Achtung: im Befehl 'tune.svm' steht bei 'data' nun Daten.train !!!

    cc <- seq(-5,10,1)    # für mögliche Werte von "Cost" (Tuningparameter)
    cg <- seq(-3,1,0.5)   # für mögliche werte von "gamma" (Tuningparameter)

    tuning <- tune.svm(Gewicht ~ Geschlecht + Alter + Groesse, data=Daten.train, scale = TRUE, type = "eps-regression", kernel = "radial",
                     gamma = 10^cg, cost = 2^cc, epsilon = 0.1,      tunecontrol = tune.control(sampling = "cross",cross=5))

    print(tuning)
    model <- tuning$best.model    # das Model mit optimalen Tuningparametern

  # Berechnung der Prognoseergebnisse auf den Testdaten:

    X.test <- Daten.test[,c("Geschlecht","Alter","Groesse")]
    prognosen <- predict(model,X.test)

  # Berechnung des mittleren Prognosefehlers (MAE)

    y.test <- Daten.test[,"Gewicht"]
    mean(abs(y.test-prognosen))





######################################
# Klassifikation mit SVM (Support Vector Machines)
#############

      setwd("C:/Users/rhable.AIKIW10NB01/Nextcloud2/Lehre/Daten/BA-KI_Maschinelles-Lernen")
    Daten <- read.csv("Patientendaten.csv",header=TRUE,sep=";",fill=TRUE,stringsAsFactors=TRUE)
    Daten[1:10,]

  # Kontrolle der Datentypen durch Ausgabe der Summary
    
    summary(Daten)

  # Umwandlung des Datentyps
  
    Daten[,"Krankheit"] <- as.factor(Daten[,"Krankheit"])
    summary(Daten)

  # Definition der Tuning-Parameter

    cc <- seq(-5,10,1)    # für mögliche Werte von "Cost" (Tuningparameter)
    cg <- seq(-5,1,0.5)   # für mögliche werte von "gamma" (Tuningparameter)

  # Berechnung des Modells

    tuning <- tune.svm(Krankheit ~ Geschlecht + Raucher + Body_Mass_Index, data=Daten, scale = TRUE, type = "C-classification", kernel = "radial",
                     gamma = 10^cg, cost = 2^cc, epsilon = 0.1,      tunecontrol = tune.control(sampling = "cross",cross=5))

    print(tuning)

  # Speichern des Modells mit den besten Parametern:

    model <- tuning$best.model    # das Model mit optimalen Tuningparametern

  # Berechnen von Prognosen

    # Berechnung der Prognosen für die Personen aus dem Datensatz:
    
      # Auswahl aller Input-Variablen und Speicherung unter 'X'
        X <- Daten[,c("Geschlecht","Raucher","Body_Mass_Index")]

      # Berechnung der Prognoseergebnisse:
        predict(model,X)


  # Berechnung der Prognosegüte:
  
    A <- matrix(0,ncol=2,nrow=2)
    colnames(A) <- c("Real: gesund", "Real: krank") 
    rownames(A) <- c("Prognose: gesund", "Prognose: krank") 

    prognosen <- predict(model,X)
    y <- Daten[,"Krankheit"]
    
    A[1,1] <- sum(ifelse(y == 0 & prognosen ==0, 1,0))
    A[1,2] <- sum(ifelse(y == 1 & prognosen ==0, 1,0))
    A[2,1] <- sum(ifelse(y == 0 & prognosen ==1, 1,0))
    A[2,2] <- sum(ifelse(y == 1 & prognosen ==1, 1,0))













