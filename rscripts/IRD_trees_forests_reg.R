################################################################################
### IRD
### Drzewa decyzyjne i lasy losowe (regresyjne)
################################################################################

################################################################################
# Wczytanie bibliotek
################################################################################

# Lista pakietów
list_of_packages <- c("readr" # wczytywanie danych
                      ,"dplyr" # manipulacja danymi
                      ,"rpart" # drzewa decyzyjne
                      ,"rpart.plot" # ładne wykresy dla drzew
                      ,"randomForest" # lasy losowe
                      ,"ROCR" # ocena jakości modelu - krzywa ROC, AUC, itd.
                      ,"MASS" # dobór zmiennych do modelu
)

# Instalacja brakujących bibliotek
not_installed <- list_of_packages[!(list_of_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)

# Załadowanie bibliotek
lapply(list_of_packages, library, character = TRUE)

################################################################################
# Wczytanie i przygotowanie danych
################################################################################

# Wczytanie danych
fpath <- "https://raw.githubusercontent.com/nosarzewski/IRD/master/data/winequality-white.csv"
dane <- read.csv2(fpath, stringsAsFactors = FALSE, dec = '.')
# read.csv2 zamiast read.csv ze wzgledu na separator kolumn
# dec = '.' - wskazujemy, że separatorem dziesiętnym jest kropka

################################################################################
# Eksploracja danych
################################################################################

head(dane) # pierwsze 6 obserwacji
str(dane) # typy zmiennych
summary(dane) # podstawowe statystyki

hist(dane$quality) # rozklad zmiennej objasnianej

################################################################################
# Modele regresyjne
################################################################################

# Inicjalizacja ziarna do zmiennych pseudolosowych
set.seed(1)

# dzielimy na zbior treningowy i testowy
train_proportion <- 0.7
train_index <- runif(nrow(dane)) < train_proportion
train <- dane[train_index,]
test <- dane[!train_index,]

# Regresja liniowa - dobry punkt odniesienia
lin_m <- lm(quality ~ ., data = train)
summary(lin_m)

# Drzewo regresji
d.regr <- rpart(quality ~ ., data = train, cp = 0.01)
rpart.plot(d.regr, under = FALSE, fallen.leaves = FALSE, cex = 0.9)

# Drzewo regresji - wieksze
d.regr.duze <- rpart(quality ~ ., data = train, cp = 0.003)
rpart.plot(d.regr.duze, under = FALSE, fallen.leaves = FALSE, cex = 0.5)

# Las losowy
rf <- randomForest(quality ~ ., data = train)
rf
plot(rf)
rf$importance

################################################################################
# Metody oceny regresji: RSS, MAE, RMSE, RAE, RRSE, R^2
################################################################################

# odchylenia reszt - rozne miary

# funkcja residuals liczy reszty = wartosci rzeczywiste - prognoza:
all(as.vector(residuals(d.regr)) == train$quality - predict(d.regr, train))

modele <- list("d.regr" = d.regr, "d.regr.duze" = d.regr.duze, "lin_m" = lin_m, "rf" = rf)

OcenaModeli <- function(modele, dane, predicted_col_name) {
  
  print("Suma kwadatow reszt RSS")
  print(sapply(modele, function(x) sum((dane[[predicted_col_name]] - predict(x, dane))^2) ))
  
  print("Średni błąd absolutny MAE")
  print(sapply(modele, function(x) sum(abs((dane[[predicted_col_name]] - predict(x, dane))))/nrow(dane) ))
  
  print("Pierwiastek błędu średniokwadratowego RMSE")
  print(sapply(modele, function(x) sqrt(sum((dane[[predicted_col_name]] - predict(x, dane))^2)/nrow(dane)) ))
  
  print("Względny błąd absolutny RAE")
  print(sapply(modele, function(x) sum(abs((dane[[predicted_col_name]] - predict(x, dane))))/sum(abs(dane[[predicted_col_name]] - mean(dane[[predicted_col_name]]))) ))
  
  print("Pierwiastek względnego błędu średniokwadratowego RRSE")
  print(sapply(modele, function(x) sqrt(sum((dane[[predicted_col_name]] - predict(x, dane))^2)/sum((dane[[predicted_col_name]] - mean(dane[[predicted_col_name]]))^2)) ))
  
  print("R-kwadrat")
  print(sapply(modele, function(x) 1 - sum((dane[[predicted_col_name]] - predict(x, dane))^2)/sum((dane[[predicted_col_name]] - mean(dane[[predicted_col_name]]))^2) ))
  
}

OcenaModeli(modele, train, 'quality')
OcenaModeli(modele, test, 'quality')
