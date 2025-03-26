

#################################################################
# Clustering
#################################################################
# von Prof. Dr. Robert Hable, Technische Hochschule Deggendorf
#################################################################



######################################
# Clustering mit dem k-means-Verfahren
#############


# Als Beispieldatensatz verwenden wir einen Datensatz, der bereits in R
# gespeichert ist

  Daten <- USArrests

  Daten

  summary(Daten)

# Der Datensatz gibt für die amerikanischen Bundesstaaten Kriminalitätsraten
# für Mord (murder), Körperverletzung (assault) und Vergewaltigung (rape)
# zusammen mit dem Anteil der städtischen Bevölkerung an


# Nun die Clusteranalyse für diesen Datensatz
# Zunächst wählen wir 4 Cluster.
# Die Daten sollten immer skaliert werden beim clustern, daher schreiben
# wir 'scale(Daten)' statt einfach nur Daten

  anzahl.cluster <- 4
  ergebnis <- kmeans(scale(Daten), anzahl.cluster,nstart = 100)

# Das Ergebnis des Clusterverfahrens sind 4 Cluster-Zentren und jeder Datenpunkt
# (in diesem Beispiel also jeder Bundesstaat) wird einem Cluster zugeordnet.
# Man sieht also, welche Bundesstaat im Bezug auf die hier erhobenen Daten
# ähnlich sind

  ergebnis


# Betrachtet man nur zwei Variablen, dann kann man das Ergebnis der Clusteranalyse
# auch sehr anschaulich grafisch darstellen:

  # Auswahl der beiden Variablen "Murder" und "Assault":

    Daten <- Daten[,c("Murder","Assault")]

  # Berechnung der Clusteranalyse für 4 Cluster

    anzahl.cluster <- 4
    ergebnis <- kmeans(scale(Daten), anzahl.cluster,nstart = 100)

    ergebnis

  # Visualisieren des Ergebnisses in einer Grafik:

    x <- Daten[,"Murder"]
    y <- Daten[,"Assault"]
    plot(x,y, col = ergebnis$cluster, pch=19)


  # Aus dem Bild kann man den Eindruck gewinnen, dass 4 Cluster wohl etwas zu
  # viel sind. Im folgenden wiederholen wir die Berechnung nun für 2 Cluster:

    anzahl.cluster <- 2
    ergebnis <- kmeans(scale(Daten), anzahl.cluster,nstart = 100)

    x <- Daten[,"Murder"]
    y <- Daten[,"Assault"]
    plot(x,y, col = ergebnis$cluster, pch=19)


# Ellbogen-Methode zur Ermittlung einer geeigneten Anzahl an Cluster:
# Hierzu betrachtet man grafisch die Total Within Sum of Squares (TWSS)
#  - also die Summe der quadrierten Abstände zwischen den Datenpunkten und den Clusterzentren

  Daten <- USArrests
  max.Cluster <- 15
  TWSS <- rep(-1,max.Cluster)   # Anlegen eines Vektors für die Total Within Sum of Square (TWSS)
  
  for(anzahl.cluster in 1:max.Cluster){
    ergebnis <- kmeans(scale(Daten), anzahl.cluster,nstart = 100)
	TWSS[anzahl.cluster] <- ergebnis$tot.withinss	
  }
  
  plot(1:max.Cluster,TWSS,type="b",lwd=1.5,pch=19,col="blue",xlab="Anzahl der Cluster",ylab="Total Within Sum of Squares")


# Darstellung des Ergebnisses für mehrdimensionale Daten (mehr als 2 Dimensionen)
# in einem Biplot nach Durchführung einer Hauptkomponentenanalyse (PCA)

  # Durchführung des k-Means-Clusterings  
    anzahl.cluster <- 4
    ergebnis <- kmeans(scale(Daten), anzahl.cluster,nstart = 100)
	
  # Berechnung der Hauptkomponentenanalyse (PCA)
    pca <- prcomp(Daten, scale = TRUE)
	
  # Berechnung der neuen Koordinaten der Datenpunkte im Koordinatensystem gegeben durch die ersten beiden Hauptkomponenten aus der PCA
    Daten.PCA <- predict(pca,Daten)   # Transformation der Daten in die neuen Koordinaten aus der Hauptkomponentenanalyse
    x <- Daten.PCA[,"PC1"]   # die Werte der 1. Hauptkomponente
    y <- Daten.PCA[,"PC2"]   # die Werte der 2. Hauptkomponente
	
  # Zeichnung des Biplots, wobei die Datenpunkte in der jeweiligen Farbe ihres Clusters dargestellt sind.
    plot(x,y,pch=19,col=ergebnis$cluster,xlab="PC1",ylab="PC2")
  
  


