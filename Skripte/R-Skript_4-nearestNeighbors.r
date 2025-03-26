
#################################################################
# k-Nearest Neighbors: Regression und Klassifikation
#################################################################
# von Prof. Dr. Robert Hable, Technische Hochschule Deggendorf
#################################################################


########################
# Regression mit k-Nearest Neighbors 
#########

# Als Datensatz verwenden wir Baumdaten, die schon in R gespeichert sind.
# Bei den Daten handelt es sich um gefällte Bäume der Art Amerikanische Traubenkirsche
# (Black Cherry Trees).
# Gemessen wurden Durchmesser, Höhe und Volumen

  Daten <- trees
  Daten
  
  summary(Daten)
  
  plot(Daten)
  
# Wir betrachten zunächst nur den Zusammenhang zwischen Height und Volume

  x <- Daten[,"Height"]
  y <- Daten[,"Volume"]
  
  plot(x,y,pch=19) 

# k-Nearest Neighbors mit nur einer Einflussvariablen 

  # Laden des Packetes FNN
    library(FNN)
    
  # Trainingsdaten
    x.train <- Daten[,"Height"]
    y.train <- Daten[,"Volume"]
    
  # Testdaten
  # Als Testdaten legen wir für Height ein feines Gitter an
  # zwischen 60 bis 90 mit Zwischenschritten von 0.01
    x.test <- seq(60,90,0.01)
    
  # Weil x.train und x.test im Moment Vectoren sind, müssen diese zunächst in
  # Matrizen umgewandelt werden (das ist hier nötig, weil wir nur 1 Einflussvariable haben) 
    x.train <- matrix(x.train)
    x.test <- matrix(x.test)
    
  # Berechnung der Prognosen mit k-Nearest Neighbors für k=12
  # und malen eines Bildes 
    
    model <- knn.reg(train=x.train, test = x.test, y=y.train, k = 12)
    prognosen <- model$pred
    
    plot(x.train[,1],y.train,pch=19)
    points(x.test[,1],prognosen,type="l",col="blue")
  
 
  # Setzt man das k nach oben, z.B. k=25, dann wird die Prognose glatter  
  
    model <- knn.reg(train=x.train, test = x.test, y=y.train, k = 25)
    prognosen <- model$pred
    
    plot(x.train[,1],y.train,pch=19)
    points(x.test[,1],prognosen,type="l",col="blue")
  
  
  # Setzt man das k nach oben, z.B. k=4, dann wird die Prognose wackliger  
  
    model <- knn.reg(train=x.train, test = x.test, y=y.train, k = 4)
    prognosen <- model$pred
    
    plot(x.train[,1],y.train,pch=19)
    points(x.test[,1],prognosen,type="l",col="blue")
  
  
  

# k-Nearest Neighbors mit mehreren Einflussvariablen 

  # Laden des Packetes FNN
    library(FNN)
    
  # Trainingsdaten
    X.train <- Daten[,c("Girth","Height")]
    y.train <- Daten[,"Volume"]
    
  # Testdaten
  # Als Testdaten verwenden wir hier auch die Trainingsdaten
    X.test <- Daten[,c("Girth","Height")]
    
    
  # Berechnung der Prognosen mit k-Nearest Neighbors für k=12 
    
    model <- knn.reg(train=X.train, test = X.test, y=y.train, k = 12)
    prognosen <- model$pred
    prognosen
    

  # Berechnung der Prognosen mit k-Nearest Neighbors für k=12 mit Skalierung
  # Weil die Ergebnisse von (willkürlich gewählten) Einheiten abhängen,
  # sollten die Daten der Einflussvariablen skaliert werden.
    
    model <- knn.reg(train=scale(X.train), test = scale(X.test), y=y.train, k = 12)
    prognosen <- model$pred
    prognosen
  
 
 



########################
# Klassifikation mit k-Nearest Neighbors 
#########

# Als Datensatz verwenden wir Daten über Schwertlilien (Englisch: iris)

  Daten <- iris
  Daten
  
  summary(Daten)
  
  plot(Daten)
 
# Wir betrachten - um Bilder malen zu können - nur zwei Einflussvariablen

  plot(Daten[,"Sepal.Length"],Daten[,"Sepal.Width"],col=as.numeric(Daten[,"Species"]),pch=19)


# Berechnung der Klassifikation mit k-Nearest neighbors
  
   # Trainingsdaten
     X.train <- Daten[,c("Sepal.Length","Sepal.Width")]
     y.train <- Daten[,"Species"]
    
  # Testdaten
  # Als Testdaten verwenden wir hier auch die Trainingsdaten
    X.test <- Daten[,c("Sepal.Length","Sepal.Width")]
       
  # Berechnung der Prognosen mit k-Nearest Neighbors für k=12 
    
    model <- knn(train=X.train, test = X.test, cl=y.train, k = 12)
    n.test <- length(X.test[,1])
    prognosen <- model[1:n.test]
    prognosen
 
# Malen eines Bildes

    x.test.1 <- seq(4,8,0.01)  
    x.test.2 <- seq(1.8,5,0.01)
    X.test <- expand.grid(x.test.1,x.test.2)  
  
    model <- knn(train=X.train, test = X.test, cl=y.train, k = 12)
    n.test <- length(X.test[,1])
    prognosen <- model[1:n.test]
  
    temp <- as.numeric(prognosen)
    Farben <- "grey55"
    Farben <- ifelse(temp == 2,"coral",Farben)
    Farben <- ifelse(temp == 3,"greenyellow",Farben)
    plot(X.test[,1],X.test[,2],col=Farben,pch=19)
    points(Daten[,"Sepal.Length"],Daten[,"Sepal.Width"],col=as.numeric(Daten[,"Species"]),pch=19)
  
  
  
  
  
  
  
  
  

