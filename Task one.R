setwd("/home/tjorven/Git/KI-B-4/KI-B-4-Machine_Learning")
data <- read.csv("Daten/autos.csv", header = TRUE, sep = ",", fill = TRUE, stringsAsFactors = TRUE)

data$Herkunft <- as.factor(data$Herkunft)

# summary(data)

print("Verbrauch")
verbrauch <- data$Verbrauch
mean(verbrauch)
median(verbrauch)
sd(verbrauch)

print("Zylinder")
zylinder <- data$Zylinder
mean(zylinder)
median(zylinder)
sd(zylinder)

print("Hubraum")
hubraum <- data$Hubraum
mean(hubraum)
median(hubraum)
sd(hubraum)

plot(data$Leistung, data$Verbrauch,
     pch = 16,
     col = "black",
     main = "Verbrauch auf Leistung",
     xlab = "Leistung",
     ylab = "Verbrauch",
     cex = 1.2)

merica <- subset(data, Herkunft == 1)
europa <- subset(data, Herkunft == 2)
asia <- subset(data, Herkunft == 3)

print("Verbrauch Mean Merica")
mean(merica$Verbrauch)
print("Verbrauch Mean Europa")
mean(europa$Verbrauch)
print("Verbrauch Mean Asia")
mean(asia$Verbrauch)

plot(merica$Leistung, merica$Verbrauch,
     col = "red", pch = 16,
     xlab = "Leistung",
     ylab = "Verbrauch",
     main = "Verbrauch auf Leistung nach Region",
     xlim = range(c(merica$Leistung, europa$Leistung, asia$Leistung)),
     ylim = range(c(merica$Verbrauch, europa$Verbrauch, asia$Verbrauch)),
     cex = 1.2)
points(europa$Leistung, europa$Verbrauch, col = "blue", pch = 16, cex = 1.2)
points(asia$Leistung, asia$Verbrauch, col = "green", pch = 16, cex = 1.2)

legend("topleft",
       title = "Herkunft",
       c("Merica", "Europa", "Asien"),
       horiz = TRUE,
       inset = .02,
       fill = c("red", "blue", "green"),
       bty = "o")


boxplot(merica$Verbrauch, europa$Verbrauch, asia$Verbrauch,
        data = data,
        names = c("Merica", "Europa", "Asia"),
        main = "Verbrauch nach Herkunft",
        xlab = "Herkunft", ylab = "Verbrauch")

print("Korrelation Verbrauch und Leistung")
cor(data$Verbrauch, data$Leistung)

print("Korrelation Verbrauch und Gewicht")
cor(data$Verbrauch, data$Gewicht)
