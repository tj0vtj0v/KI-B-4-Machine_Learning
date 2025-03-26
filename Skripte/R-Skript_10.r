
#################################################################
# Zeitreihen
#################################################################
# von Prof. Dr. Robert Hable, Technische Hochschule Deggendorf
#################################################################


# Laden des Beispieldatensatzes

    setwd("C:/Users/rhable.AIKIW10NB01/Nextcloud2/Lehre/Daten/BA-KI_Maschinelles-Lernen")
  Daten <- read.csv("Zeitreihe-Nachfrage.csv",header=TRUE,sep=",",fill=TRUE,stringsAsFactors=TRUE)
  Daten[1:10,]

# Kontrolle der Datentypen durch Ausgabe der Summary
  
  summary(Daten)
  

##############
# Berechnung der linearen Regression mit naiver linearer Regression
# ohne Beachtung der Autokorrelation 

  model <- lm(Nachfrage ~ Preis, data=Daten)
  model

# Berechnung des mittleren Prognosefehlers (MAE)

  y <- Daten[,"Nachfrage"] 
  Prognosen <- model$fitted.values 
  Prognosefehler <- mean( abs( y - Prognosen ) )
  Prognosefehler

# Speichern des Prognoseergebnisses
  Prognoseergebnis <- round(Prognosefehler,digits=2)
  names(Prognoseergebnis) <- "naive lineare Regression"
  Prognoseergebnis

###############
# In den nachfolgenden Berechnungen werden die Daten immer wieder
# auf verschiedene Art neu aufbereitet. Die ursprünglichen Rohdaten werden
# daher unter den Namen 'Rohdaten' abgespeichert

  Rohdaten <- Daten
    

#####################################################
# Trend und Saisonkomponenten

# Datenaufbereitung

  # Bilden eines Datensatzes mit einer fortlaufenden Durchnumerierung 
  # aller Wochen (Spalte 'Zeit'), der Spalte 'Preis' und der Spalte 'Nachfrage'
  
    n <- length(Rohdaten[,1])
    Zeit <- seq(1,n,1)
    Daten <- cbind(Zeit,Rohdaten[,c("Preis","Nachfrage")])  # Zusammenfügen der Spalten
    
    Daten[1:10,]
    summary(Daten)

  # Hinzufügen einer kategoriellen Variable 'Saison' (Fruehling, Sommer, Herbst, Winter)
  
    # Definition einer Variablen 'Saison' 
    # Hierbei ist der Befehl '%%' das mathematische 'modulo' (der Rest beim 
    # ganzzahligen Teilen mit Rest - wie in der Grundschule gelernt)
    # Beispiel: 14 geteilt durch 3 ist 4 mit Rest 2
    # also ergibt 14 %% 3 den Wert 2 
      Saison <- "Winter"
      Saison <- ifelse(Zeit %% 52 < 20.5 & Zeit %% 52 > 7.5 , "Fruehling",Saison )
      Saison <- ifelse(Zeit %% 52 < 33.5 & Zeit %% 52 > 20.5 , "Sommer",Saison )
      Saison <- ifelse(Zeit %% 52 > 33.5 & Zeit %% 52 < 46.5 , "Herbst",Saison )
  
      summary(Saison)
    
    # Umwandeln in Datentyp factor
      Saison <- as.factor(Saison)   
    
    # Hinzufügen der Saison zum Datensatz      
      Daten <- cbind(Zeit,Saison,Daten[,c("Preis","Nachfrage")])
      Daten[1:10,]
      
    # Korrektur der Spaltenueberschrift von 'Zeit'
      names(Daten)[1] <- "Zeit"
      Daten[1:10,]
    
    # Kontrolle  
      Daten
      summary(Daten)


# Berechnung der linearen Regression mit Trend und Saisonkomponenten

  model <- lm(Nachfrage ~ Zeit + Preis + Saison, data=Daten)
  model

# Berechnung des mittleren Prognosefehlers (MAE)

  y <- Daten[,"Nachfrage"] 
  Prognosen <- model$fitted.values 
  Prognosefehler <- mean( abs( y - Prognosen ) )
  Prognosefehler

# Speichern des Prognoseergebnisses
  Prognoseergebnis <- c(Prognoseergebnis,round(Prognosefehler,digits=2))
  names(Prognoseergebnis)[length(Prognoseergebnis)] <- "Trend und Saison (lin. Reg.)"
  Prognoseergebnis


# linearer Trend und Saisonkomponenten im addidven Modell

  # Laden des R-Pakets fuer L2-Boosting
    library(mboost)
    
  # Aufteilung in Training und Test mit Vertauschen der Reihenfolge
    index <- sample(1:n,n,replace=FALSE)
    Daten.train <- Daten[index[1:146],]
    Daten.test <- Daten[index[147:208],]
    
  # Berechnung des Modells (noch nicht kreuzvalidiert)
  # Einflussvariablen mit 'bols' gehen als lineare Komponenten ein,
  # Einflussvariablen mit 'bbs' gehen als nicht-lineare Komponenten ein

    model <- gamboost(Nachfrage ~ bols(Zeit) + bbs(Preis) + bols(Saison), 
                      data=Daten.train, dfbase = 4, control = boost_control(mstop = 1000))
    
    
  # Kreuzvalidierung:
  
    cv10f <- cv(model.weights(model), type = "kfold")
    cvm <- cvrisk(model, folds = cv10f, papply = lapply)
    mstop(cvm)
    plot(cvm)

  # Berechnung des Modells (nun kreuzvalidiert), 
  # daher steht in den Optionen: 'mstop = mstop(cvm)'

    model <- gamboost(Nachfrage ~ bols(Zeit) + bbs(Preis) + bols(Saison), 
                      data=Daten.train, dfbase = 4, control = boost_control(mstop = mstop(cvm)))
    model

  # Berechnung der Prognoseergebnisse auf den Testdaten:

    X.test <- Daten.test[,c("Zeit","Preis","Saison")]
    prognosen <- predict(model,X.test)

  # Berechnung des mittleren Prognosefehlers (MAE)

    y.test <- Daten.test[,"Nachfrage"]
    Prognosefehler <- mean(abs(y.test-prognosen))

  # Speichern des Prognoseergebnisses
    Prognoseergebnis <- c(Prognoseergebnis,round(Prognosefehler,digits=2))
    names(Prognoseergebnis)[length(Prognoseergebnis)] <- "Trend und Saison (L2-Boosting)"
    Prognoseergebnis



#####################################################
# Zuwachs-Modell

# Datenaufbereitung

  Preisaenderung <- Rohdaten[-1,"Preis"]-Rohdaten[-n,"Preis"]
  Nachfrageaenderung <- Rohdaten[-1,"Nachfrage"]-Rohdaten[-n,"Nachfrage"]
  Daten <- data.frame(cbind(Preisaenderung,Nachfrageaenderung))
  
  Daten[1:15,]

# Fuer die Prognosen brauchen wir auch die Absolutwerte der Nachfrage
  Nachfrage <- Rohdaten[,"Nachfrage"]

# Lineare Regression für die Zuwaechse

  model <- lm(Nachfrageaenderung ~ Preisaenderung,data=Daten)
  model
  
# Berechnung des mittleren Prognosefehlers (MAE)

  y <- Nachfrage[-1]   # fuer die 1. Woche gibt es noch keine Prognose (deshalb: -1)
  Nachfrage.Vorwoche <- Nachfrage[-n] 
  Prognosen.z <- model$fitted.values
  Prognosen <- Nachfrage.Vorwoche + Prognosen.z 
  Prognosefehler <- mean( abs( y - Prognosen ) )
  Prognosefehler

# Speichern des Prognoseergebnisses
  Prognoseergebnis <- c(Prognoseergebnis,round(Prognosefehler,digits=2))
  names(Prognoseergebnis)[length(Prognoseergebnis)] <- "Zuwachs-Modell (lin. Reg.)"
  Prognoseergebnis


#####################################################
# Autoregressions-Modell

# Datenaufbereitung
  Nachfrage <- Rohdaten[,"Nachfrage"]
  Nachfrage.Vorwoche <- Nachfrage[-n]
  Daten <- cbind(Nachfrage.Vorwoche,Rohdaten[-1,c("Preis","Nachfrage")])  # bei Preis und Nachfrage muss der 1. datenpunkt entfernt werden
                                                        # deshalb: -1
  Daten[1:15,] 
    
# Lineare Regression für die Zuwaechse

  model <- lm(Nachfrage ~ Nachfrage.Vorwoche + Preis,data=Daten)
  model

# Berechnung des mittleren Prognosefehlers (MAE)

  y <- Daten[,"Nachfrage"] 
  Prognosen <- model$fitted.values 
  Prognosefehler <- mean( abs( y - Prognosen ) )
  Prognosefehler

# Speichern des Prognoseergebnisses
  Prognoseergebnis <- c(Prognoseergebnis,round(Prognosefehler,digits=2))
  names(Prognoseergebnis)[length(Prognoseergebnis)] <- "Autoregressions-Modell (lin. Reg.)"
  Prognoseergebnis






