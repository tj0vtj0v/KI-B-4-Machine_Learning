

#################################################################
# Klassifikation 
#################################################################
# von Dr. habil. Robert Hable, Technische Hochschule Deggendorf
#################################################################



######################################
# Klassifikation: Logistische Regression
#############

  # Setzen des Pfades und Einlesen der Beispiel-Daten

    setwd("C:/Users/rHable/Nextcloud/THD-Robert/Lehre/Daten/Einfuehrung-KI")
    Daten <- read.csv("Patientendaten.csv",header=TRUE,sep=";",fill=TRUE,stringsAsFactors=TRUE)
    Daten[1:10,]

  # Kontrolle der Datentypen durch Ausgabe der Summary
    
    summary(Daten)

  # Umwandlung und Kontrolle der Datentypen sowie Ausgabe der Summary

    Daten[,"Krankheit"] <- as.factor(Daten[,"Krankheit"])
    summary(Daten)
      
  # Berechnung der Klassifikation (Logistische Regression)   
  # Achtung: die Zievariable muss 0/1-Werte haben
                     
    model <- glm(Krankheit ~ Geschlecht + Raucher + Body_Mass_Index, data=Daten,binomial(link = "logit"))
    model
    
  # Berechnung des Klassifikationsfehler (Misclassification Error Rate)

    y <- Daten[,"Krankheit"] 
    Prognosen <- round(model$fitted.values) 
    Prognosefehler <- mean( ifelse( y != Prognosen, 1,0 ) )
    Prognosefehler
    
  # Berechnung von Prognosen:
  # Wahrscheinlichkeit einer Erkrankung bei Patienten: männlich, starker Raucher, BMI von 22:
  
    z <- -3.1092 - 0.1453 + 0.2016*22 
    exp(z)/(1+exp(z))
  
  