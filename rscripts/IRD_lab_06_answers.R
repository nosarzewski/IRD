################################################################################
# Zadanie 1
################################################################################

# Uzupelnij skrypt z cwiczen 5 (przewidywanie jakosci bialego wina na podstawie jego
# parametrow chemicznych) o model lasu losowego. Porownaj rozne metryki jakosci
# tego modelu do analogicznych metryk jakosci modeli uzywanych na cwiczeniach 5.

library(randomForest)

################################################################################
# Regresja - liczbowa zmienna przewidywana
################################################################################

dane <- read.csv2('data/winequality-white.csv',  stringsAsFactors = FALSE, dec = '.')

set.seed(1)
train_proportion <- 0.7
train_index <- runif(nrow(dane)) < train_proportion
train <- dane[train_index,]
test <- dane[!train_index,]

rf_reg <- randomForest(quality ~., data = train)

# Ocena lasu losowego regresji
varImpPlot(rf_reg)

OcenaModeli <- function(modele, dane, predicted_col_name) {
  
  print("Suma kwadatow reszt RSS")
  print(sapply(modele, function(x) sum((dane[[predicted_col_name]] - predict(x, dane))^2) ))
  
  print("Średni błąd absolutny MAE")
  print(sapply(modele, function(x) sum(abs((dane[[predicted_col_name]] - predict(x, dane))))/nrow(dane) ))
  
  print("Pierwiastek błędu średniokwadratowego RMSE")
  print(sapply(modele, function(x) sqrt(sum((dane[[predicted_col_name]] - predict(x, dane))^2)/nrow(dane)) ))
  
  print("Względny błąd absolutny RAE")
  print(sapply(modele, function(x) sum(abs((dane[[predicted_col_name]] - predict(x, dane))))/sum(abs(dane[[predicted_col_name]] - mean(dane[[predicted_col_name]]))) ))
  
  print("Pierwiastek Względnego błędu średniokw RRSE")
  print(sapply(modele, function(x) sqrt(sum((dane[[predicted_col_name]] - predict(x, dane))^2)/sum((dane[[predicted_col_name]] - mean(dane[[predicted_col_name]]))^2)) ))
  
}

OcenaModeli(list(rf_reg), train, 'quality')
OcenaModeli(list(rf_reg), test, 'quality')

################################################################################
# Klasyfikacja - czynnikowa, dwuwartościowa zmienna przewidywana
################################################################################

dane <- read.csv2('data/winequality-white.csv',  stringsAsFactors = FALSE, dec = '.')

# Zamiana jakosci na zmienna o 2 wartosciach
dane$quality <- ifelse(dane$quality >= 6, 'high', 'low')

# Zamiana jakosci na zmienna typu czynnikowego
dane$quality <- as.factor(dane$quality)

set.seed(1)
train_proportion <- 0.7
train_index <- runif(nrow(dane)) < train_proportion
train <- dane[train_index,]
test <- dane[!train_index,]

rf_class <- randomForest(quality ~., data = train)
cm <- table(predict(rf_class, new = test, type = "class"), test$quality)

EvaluateModel <- function(classif_mx)
{
  # Sciagawka: https://en.wikipedia.org/wiki/Sensitivity_and_specificity#Confusion_matrix
  true_positive <- classif_mx[1,1]
  true_negative <- classif_mx[2,2]
  condition_positive <- sum(classif_mx[ ,1])
  condition_negative <- sum(classif_mx[ ,2])
  # Uzywanie zmiennych pomocniczych o sensownych nazwach
  # ulatwia zrozumienie, co sie dzieje w funkcji
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  sensitivity <- true_positive / condition_positive
  specificity <- true_negative / condition_negative
  return(list(accuracy = accuracy, 
              sensitivity = sensitivity,
              specificity = specificity))
  # Notacja "accuracy = accuracy" itd. jest potrzebna,
  # zeby elementy listy mialy nazwy.
}

EvaluateModel(cm)

library(ROCR) # do krzywej ROC

prognoza_ciagla <- predict(rf_class, newdata = test, type = "prob")
# trzeba podac type = "prob", bo dla lasu losowego domyslnie zwracal przewidywana klase
prognoza_ciagla <- as.vector(prognoza_ciagla[,2])

# krzywa ROC - potrzebuje "ciaglej" prognozy
plot(performance(prediction(prognoza_ciagla,test$quality),"tpr","fpr"),lwd=2, colorize=T) 

# AUC (Area Under Curve) - pole pod krzywa ROC
performance(prediction(prognoza_ciagla, test$quality),"auc")

# Sensitivity/specificity plots ~ trade-off
plot(performance(prediction(prognoza_ciagla,test$quality),"sens","spec"),lwd=2) 

# Lift chart
plot(performance(prediction(prognoza_ciagla,test$quality),"lift","rpp"),lwd=2, col = "darkblue") 
#Lift is a measure of the effectiveness of a predictive model calculated 
#as the ratio between the results obtained with and without the predictive model. 

###############################################################################
# Zadanie 2
################################################################################

# Wybierz ostatnie 30 000 transakcji z pliku 75000-out2.csv.
# Znajdz dla tych transakcji reguly asocjacyjne o minimalnym wsparciu 0.015
# i minimalnej pewnosci 0.6.
# Zwizualizuj:
# - zaleznosci pomiedzy lift, support i confidence dla znalezionych regul
# - macierz lewych i prawych stron regul z pokazana wartoscia lift
# 
# Wybierz sposrod znalezionych 4 reguly o najwyzszej pewnosci.
# Zwizualizuj je w postaci grafow.
# Wypisz w postaci tekstowej regule o najwyzszej pewnosci.

library("arules") # do znajdowania regul
library("arulesViz") # do wizualizacji regul

# Wczytanie danych o nazwach produktow i ich obrobka z uzyciem wyrazen regularnych
# (wyrazenia regularne nie obowiazuja na kolokwium)

goods_names <- readLines("data/EB-build-goods.sql")
goods_names <- gsub("^[a-zA-Z0-9 \\(]*,'", "", goods_names, fixed = FALSE)
goods_names <- gsub("','", " ", goods_names, fixed = FALSE)
goods_names <- gsub("'.*$", "", goods_names, fixed = FALSE)

# Wczytanie i eksploracja danych o transakcjach
df_all <- read.csv("data/75000-out2.csv", header = FALSE,
                   row.names = 1)

# Wybierz ostatnie 30 000 transakcji z pliku 75000-out2.csv.
df <- tail(df_all, 30000)

# Zamiana ramki danych na macierz i nadanie nazw kolumnom 
mx <- as.matrix(df)
colnames(mx) <- goods_names
ts <- as(mx, "transactions")

# Znajdz dla tych transakcji reguly asocjacyjne o minimalnym wsparciu 0.015
# i minimalnej pewnosci 0.6.
min_support <- 0.015
min_confidence <- 0.6

# Znajdowanie regul asocjacyjnych algorytmem apriori
rules = apriori(ts, parameter=list(support=min_support, confidence=min_confidence))

# - zaleznosci pomiedzy lift, support i confidence dla znalezionych regul
plot(rules, measure=c("support","lift"), shading="confidence") # mozna tez inaczej rozmiescic te miary na wykresie

# - macierz lewych i prawych stron regul z pokazana wartoscia lift
plot(rules, method="matrix", shading="lift")

# Wybierz sposrod znalezionych 4 reguly o najwyzszej pewnosci.
subrules = head(sort(rules, by="confidence", decreasing = TRUE), 4)

# Zwizualizuj je w postaci grafow.
plot(subrules2, method="graph")

# Wypisz w postaci tekstowej regule o najwyzszej pewnosci.
inspect(subrules[1])
