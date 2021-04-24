
"chargement des bibliothèques"
library(MASS)
library(ggplot2)
library(plyr)
library(reshape)
library(gam)
library(lubridate)
library(randomForest)
library(forestmodel)
library(ggeffects)
library(leaps)
library(rpart)
library(rpart.plot)

set.seed(1)

"lecture du dataset"
setwd("/Users/abdelilahmoulida/Downloads/bike-sahring/data")
bike = read.csv("train.csv")

"informations sur le dataset"
str(bike)
summary(bike)

"extraction du mois, jours et heures de la variable datetime"
month = as.integer(format(as.POSIXlt(bike$datetime), format = "%m"))
weekday = as.integer(format(as.POSIXlt(bike$datetime), format = "%u"))
hour = as.integer(format(as.POSIXlt(bike$datetime), format = "%H"))

"creation du data frame contenant les variables a exploiter"
bike = data.frame(bike$season, month, weekday, hour, as.factor(bike$workingday), as.factor(bike$holiday), 
  as.factor(bike$weather), bike$temp, bike$humidity, bike$windspeed, bike$count)
names(bike) = c("season", "month", "weekday", "hour", "isweekday", "isholiday", "weathertype", "temperature", 
  "humidity", "windspeed", "count")

"suppression des lignes avec un windspeed = 0.0000"
modifbike = bike[which(bike$windspeed != 0.0000),]
head(modifbike, 5)

"(*) correlation entre variables"
bikeselect = data.frame(modifbike$season, modifbike$month, modifbike$weekday, modifbike$hour, 
  as.integer(modifbike$isweekday), as.integer(modifbike$isholiday), as.integer(modifbike$weather), modifbike$temperature, 
  modifbike$humidity, modifbike$windspeed, modifbike$count)
names(bikeselect) = c("season", "month", "weekday", "hour", "isweekday", "isholiday", "weathertype", "temperature",
  "humidity", "windspeed", "count")

"palette et couleur pour affichage"
red=rgb(1,0,0); green=rgb(0,1,0); blue=rgb(0,0,1); white=rgb(1,1,1)
RtoWrange = colorRampPalette(c(white, red ))
WtoGrange = colorRampPalette(c(green, white)) 

"correlation plot"
ggsave("/Users/abdelilahmoulida/Downloads/bike-sahring/plots2/correlation.png", 
ggplot(melt(cor(bikeselect)), aes(x = X1, y = X2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2("", low = WtoGrange(100), mid = RtoWrange(100), high = "gray") +
  geom_text(aes(label = round(value, 2))) +
  coord_flip() + 
  ggtitle("\n Correlation Matrix \n") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 0.6),
        axis.ticks.y = element_blank()) +
  xlab("") + ylab(""))

"(**) analyse descriptive"
countsummary = ddply(bike,.(season, month, weekday, hour, isweekday, isholiday, weathertype), summarise, 
  temperature = mean(temperature), humidity = mean(humidity), windspeed = mean(windspeed), count = mean(count))
head(countsummary)

"boxplot, rental v.s. season"
ggsave("/Users/abdelilahmoulida/Downloads/bike-sahring/plots2/rental_season.png", 
ggplot(countsummary, aes(x = season, y = count, fill = factor(season))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_fill_manual(values = c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
    name="Season:",
    breaks=c(1, 2, 3, 4),
    labels=c("Winter", "Spring", "Summer", "Fall")))

"line plot, rentals v.s. hour of day"
ggsave("/Users/abdelilahmoulida/Downloads/bike-sahring/plots2/rental_hour_of_day.png", 
ggplot(countsummary, aes(x = hour, y = count, color = as.factor(weekday))) +
  geom_smooth(method = "loess", fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("Hour of the Day") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_color_discrete("") +
  theme(plot.title = element_text(size = 11, face="bold")))

"boxplot, rental v.s. holiday"
ggsave("/Users/abdelilahmoulida/Downloads/bike-sahring/plots2/rental_holiday.png", 
ggplot(countsummary, aes(x = isholiday, y = count, fill = factor(season))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Is it Holiday?") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
    name="Season:",
    breaks=c(1, 2, 3, 4),
    labels=c("Winter", "Spring", "Summer", "Fall")) +
  theme(plot.title = element_text(size = 11, face="bold")))

"boxplot, rentals v.s. weather"
ggsave("/Users/abdelilahmoulida/Downloads/bike-sahring/plots2/rental_weather.png", 
ggplot(countsummary, aes(x = weathertype, y = count, fill = weathertype)) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Type of Weather") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_fill_manual(values = c("#E74C3C", "#58D68D", "#5DADE2", "#F4D03F"), 
    name = "Type of Weather:",
    breaks = c(1, 2, 3, 4),
    labels = c("\n Clear or Cloudy \n", 
      "\n Mist \n", 
      "\n Light Rain \n or Light Snow \n", 
      "\n BAD WEATHER \n")) +
  theme(plot.title = element_text(size = 11, face="bold")))

"line plot, rental v.s. temperature"
ggsave("/Users/abdelilahmoulida/Downloads/bike-sahring/plots2/rental_temperature.png", 
ggplot(countsummary, aes(x = temperature, y = count, color = weathertype)) +
  geom_smooth(fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("Temperature") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_color_discrete(name = "Type of Weather:",
    breaks = c(1, 2, 3, 4),
    labels = c("Clear or Cloudy", 
      "Mist", 
      "Light Rain or Snow", 
      "")) +
  theme(plot.title = element_text(size = 11, face="bold")))

"line plot, rental v.s. humidity"
ggsave("/Users/abdelilahmoulida/Downloads/bike-sahring/plots2/rental_humidity.png", 
ggplot(countsummary, aes(x = humidity, y = count, color = weathertype)) +
    geom_smooth(method = 'loess', fill = NA, size = 1) +
    theme_light(base_size = 11) +
    xlab("Humidity") +
    ylab("Number of Bike Rentals") +
    ggtitle("\n") +
    scale_color_discrete(name = "Type of Weather:",
      breaks = c(1, 2, 3, 4),
      labels = c("Clear or Cloudy", 
        "Mist", 
        "Light Rain or Snow", 
        "")) +
    theme(plot.title = element_text(size = 11, face="bold")))

"line plot, rental v.s. wind speed"
ggsave("/Users/abdelilahmoulida/Downloads/bike-sahring/plots2/rental_windspeed.png", 
ggplot(countsummary, aes(x = windspeed, y = count, color = weathertype)) +
    geom_smooth(fill = NA, size = 1) +
    theme_light(base_size = 11) +
    xlab("Wind Speed") +
    ylab("Number of Bike Rentals") +
    ggtitle("\n") +
    scale_color_discrete(name = "Type of Weather:",
      breaks = c(1, 2, 3, 4),
      labels = c("Clear or Cloudy", 
        "Mist", 
        "Light Rain or Snow", 
        "")) +
    theme(plot.title = element_text(size = 11, face="bold")))

"(***) construire des modèles, predictions"

"regression linéaire"
"on effectue une régression par les moindres carrés en utilisant toutes les variables
on observe globalement 6 variables significatives qui sont month, hour, weathertype, temperature, humidity (***). 
R2 faible et l’erreur quadratique moyenne est assez important : 315.5 milliers de dollars (écart moyen entre le salaire 
réel et le salaire prédit), p-value de la F-statistic est très faible"
regbike = lm(count~., data = bike)
summary(regbike)

"modèle en se basant sur le critère AIC" 
regressor = step(regbike)
summary(regressor)

"selection variable"
"recherche exhaustive"
regcount = regsubsets(count~., data = bike)
selexhau = summary(regcount)
selexhau

selexhau$rsq
selexhau$adjr2
selexhau$cp
selexhau$bic

"representation graphique"
par(mfrow = c(1, 1))
plot(selexhau$rss, xlab = "Nombre de variables", ylab = "Somme des carrees des residus", type = "l", main = "SCres")
u = which.min(selexhau$rss)
points(u, selexhau$rss[u], pch = 20, col = "red")

plot(selexhau$adjr2, xlab = "Nombre de variables", ylab = "R2 ajuste", type = "l", main = "R2 Ajuste")
u = which.max(selexhau$adjr2)
points(u, selexhau$adjr2[u], pch = 20, col = "red")

plot(selexhau$cp, xlab = "Nombre de variables", ylab = "Cp", type = "l", main = "Cp de Mallows")
u = which.min(selexhau$cp)
points(u, selexhau$cp[u], pch = 20, col = "red")

plot(selexhau$bic, xlab = "Nombre de variables", ylab = "Bic", type = "l", main = "BIC")
u = which.min(selexhau$bic)
points(u, selexhau$bic[u], pch = 20, col = "red")

par(mfrow = c(1, 1))
plot(regcount, scale = "r2")
plot(regcount, scale = "adjr2")
plot(regcount, scale = "Cp")
plot(regcount, scale = "bic")

coef(regcount, 6)
summary(lm(count ~ month + hour + weathertype + temperature + humidity, data = bike))

"random forest"
train = read.csv("train.csv")
test = read.csv("test.csv")

extractFeatures = function(data) {
  features = c("season", "holiday", "workingday", "weather", "temp", "humidity", "windspeed", "hour", "month")
  data$hour = hour(ymd_hms(data$datetime))
  data$month = as.integer(format(as.POSIXlt(data$datetime), format = "%m"))
  return(data[,features])
}

trainFea = extractFeatures(train)
testFea = extractFeatures(test)

"application du random forest"
rf = randomForest(extractFeatures(train), train$count, ntree=100, importance=TRUE)
imp = importance(rf, type=1)
featureImportance = data.frame(Feature=row.names(imp), Importance=imp[,1])

ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
     geom_bar(stat="identity", fill="#53cfff") +
     coord_flip() + 
     theme_light(base_size=20) +
     xlab("Importance") +
     ylab("") + 
     ggtitle("Random Forest Feature Importance\n") +
     theme(plot.title=element_text(size=18))

bikeTree = rpart(count~.,data=bike)
bikeTree = rpart(count~.,data=bike,control=rpart.control(minsplit=5,cp=0))
plotcp(bikeTree)

bikeSimple = prune(bikeTree, cp = 0.0002714573)
bikeOptimal = prune(bikeTree, cp = bikeTree$cptable[which.min(bikeTree$cptable[,4]), 1])
prp(bikeOptimal,extra = 1)
