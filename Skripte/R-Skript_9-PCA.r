

#################################################################
# Hauptkomponentennalyse (PCA))
#################################################################
# von Prof. Dr. Robert Hable, Technische Hochschule Deggendorf
#################################################################

#########
# Beispiel eines bereits in R gespeicherten Datensatzes

  Daten <- USArrests
  Ergebnis <- prcomp(Daten, scale = TRUE)
  Ergebnis

  Ergebnis$sdev

  # Wieviel der Varianz erkl"aren die verschiedenen Variablen (PC1,PC2,PC3,PC4)?
     a <- Ergebnis$sdev
     a^2/sum(a^2)

  # Also: die erste Variable erklärt ca. 60%, die zweite ca. 25%, die dritte
  # ca. 9 Prozent, die vierte ca. 4 Prozent.

  # Die Varianzen als Säulendiagramm:
     plot(Ergebnis)

  # Die Zahlen auch in der Summary:
     summary(Ergebnis)

  # Der Biplot: grafisch die beiden ersten Variablen:
     biplot(Ergebnis)


########
# Unser Gewichtsbeispiel:


   setwd("C:/Users/rhable.AIKIW10NB01/Nextcloud2/Lehre/Daten/BA-KI_Maschinelles-Lernen")
   Daten <- read.csv2("Koerpergewicht_2.csv",header=TRUE,sep=";",fill=TRUE,stringsAsFactors=TRUE)


# Die Spalte "Geschlecht" muss entfernt werden:

  Daten[1:5,]
  Daten <- Daten[,-1]
  Daten[1:5,]

  Ergebnis <- prcomp(Daten, scale = TRUE)
  Ergebnis

  Ergebnis$sdev

  # Wieviel der Varianz erkl"aren die verschiedenen Variablen (PC1,PC2,PC3,PC4)?
     a <- Ergebnis$sdev
     a^2/sum(a^2)

  # Also: die erste Variable erklärt ca. 60%, die zweite ca. 25%, die dritte
  # ca. 9 Prozent, die vierte ca. 4 Prozent.

  # Die Varianzen als Säulendiagramm:
     plot(Ergebnis)

  # Die Zahlen auch in der Summary:
     summary(Ergebnis)

  # Der Biplot: grafisch die beiden ersten Variablen:
     biplot(Ergebnis)


