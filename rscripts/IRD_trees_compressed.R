################################################################################
### IRD
### Lab 4/5
### Drzewa klasyfikacyjne i regresyjne + Ocena jakości modeli
################################################################################

################################################################################
# Drzewa klasyfikacyjne
################################################################################

# Źródła:
# https://www.kaggle.com/c/titanic
# http://trevorstephens.com/kaggle-titanic-tutorial/getting-started-with-r/

# Biblioteki
library(rpart) # do drzewa
library(rpart.plot) # do rysowania drzewa

# Wczytanie danych
titanic_fpath <- "https://raw.githubusercontent.com/nosarzewski/IRD/master/data/titanic_full.csv"
tdf <- read.csv(titanic_fpath)
tdf$survived <- as.factor(tdf$survived)

# Podział na zbiór treningowy i testowy
test_prop <- 0.25
test.set.index <- (runif(nrow(tdf)) < test_prop)
tdf.test <- tdf[test.set.index, ]
tdf.train <- tdf[!test.set.index, ]

# Budowa drzewa decyzyjnego
tree <- rpart(survived ~ pclass + sex + age + sibsp + parch + fare + embarked,
              data = tdf.train,
              method = "class",
              control = list(maxdepth = 2))

# Wizualizacja drzewa
tree

plot(tree)
text(tree, pretty = TRUE)

rpart.plot(tree, under = FALSE, tweak = 1.3, fallen.leaves = TRUE)
# Wartości w węzłach drzewa:
# - przewidywana kategoria
# - prawdopodobieństwo przynależności do kategorii 1
# - procentowy udział obserwacji w danym węźle

# Alternatywne drzewo decyzyjne - głębsze
tree_deeper <- rpart(survived ~ pclass + sex + age + sibsp + parch + fare + embarked,
                     data = tdf.train,
                     method = "class",
                     control = list(maxdepth = 10))
# Inne opcje drzewa
# minsplit - minimalna liczba obserwacji w liściu wymagana do dokonania dalszego podziału
# minbucket - minimalna liczba obserwacji w liściu końcowym
# maxdepth - maksymalna głębokość drzewa
# cp (complexity paremeter) - minimalna wartość o jaką musi poprawić się jakość modelu aby dokonać podziału w drzewie

rpart.plot(tree_deeper, under = FALSE, tweak = 1.3, fallen.leaves = TRUE)

# Wpływ zmiennych
tree$variable.importance
tree_deeper$variable.importance

# Weryfikacja jakości klasyfikacji

## Macierz pomyłek
## https://en.wikipedia.org/wiki/Confusion_matrix
table(predict(tree, new = tdf.test, type = "class"), tdf.test$survived)

CM <- list()
CM[["tree"]] <- table(predict(tree, new = tdf.test, type = "class"), tdf.test$survived)
CM[["tree_deeper"]] <- table(predict(tree_deeper, new = tdf.test, type = "class"), tdf.test$survived)

EvaluateModel <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  # Uzywanie zmiennych pomocniczych o sensownych nazwach
  # ulatwia zrozumienie, co sie dzieje w funkcji
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- 1 - accuracy # Misclassification Error Rate
  # inaczej: MER < - (false_positive + false_positive) / sum(classif_mx)
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive # inaczej - Recall / True Positive Rate (TPR)
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              MER = MER,
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
  # Notacja "accuracy = accuracy" itd. jest potrzebna,
  # zeby elementy listy mialy nazwy.
}

lapply(CM, EvaluateModel)
sapply(CM, EvaluateModel)

## Miary niezależne od punktu odcięcia prawdopodobieństwa

library(ROCR)
## Z dokumentacji pakietu ROCR:
# Here is how to call 'performance' to create some standard evaluation plots:
# ROC curves: measure="tpr", x.measure="fpr".
# Precision/recall graphs: measure="prec", x.measure="rec".
# Sensitivity/specificity plots: measure="sens", x.measure="spec".
# Lift charts: measure="lift", x.measure="rpp".

## Prognoza w postaci prawdopodobieństwa, zamiast kategorii
prognoza_ciagla_tree <- as.vector(predict(tree, newdata = tdf.test)[, 2])
prognoza_ciagla_tree_deeper <- as.vector(predict(tree_deeper, newdata = tdf.test)[, 2])

# krzywa ROC - potrzebuje "ciaglej" prognozy
plot(performance(prediction(prognoza_ciagla_tree, tdf.test$survived), "tpr", "fpr"), lwd = 2, colorize = T) 

plot(performance(prediction(prognoza_ciagla_tree, tdf.test$survived), "tpr", "fpr"), lwd = 2, colorize = T, lty = 1) 
plot(performance(prediction(prognoza_ciagla_tree_deeper, tdf.test$survived), "tpr", "fpr"), lwd = 2, colorize = T, lty = 5, add = T) 

## Jeżeli wartości na osi colorize przyjmują wartości powyżej 1:
perf_tree <- performance(prediction(prognoza_ciagla_tree, tdf.test$survived), "tpr", "fpr")
if(attributes(perf_tree)$alpha.values[[1]][1] == Inf) attributes(perf_tree)$alpha.values[[1]][1] <- 1
perf_tree_deeper <- performance(prediction(prognoza_ciagla_tree_deeper, tdf.test$survived), "tpr", "fpr")
if(attributes(perf_tree_deeper)$alpha.values[[1]][1] == Inf) attributes(perf_tree)$alpha.values[[1]][1] <- 1

plot(perf_tree, lwd = 2, colorize = T, lty = 1) 
plot(perf_tree_deeper, lwd = 2, colorize = T, lty = 5, add = T) 

# AUC (Area Under Curve) - pole pod krzywa ROC
(auc_tree <- performance(prediction(prognoza_ciagla_tree, tdf.test$survived), "auc")@y.values[[1]])
(auc_tree_deeper <- performance(prediction(prognoza_ciagla_tree_deeper, tdf.test$survived), "auc")@y.values[[1]])

# Sensitivity/specificity plots ~ trade-off
plot(performance(prediction(prognoza_ciagla_tree, tdf.test$survived), "sens", "spec"), lwd = 2)

# Lift chart
plot(performance(prediction(prognoza_ciagla_tree_deeper, tdf.test$survived), "lift", "rpp"), lwd = 2, col = "darkblue") 
#Lift is a measure of the effectiveness of a predictive model calculated 
#as the ratio between the results obtained with and without the predictive model. 

################################################################################
# Drzewa regresyjne
################################################################################

# Wczytanie danych
wine_data <- read.csv2('https://raw.githubusercontent.com/nosarzewski/IRD_19_20_Z/master/data/winequality-white.csv',  
                  stringsAsFactors = FALSE, dec = '.')
#wine_data <- read.csv2('data/winequality-white.csv',  stringsAsFactors = FALSE, dec = '.')

# Inicjalizacja ziarna do zmiennych pseudolosowych
set.seed(1)

# dzielimy na zbior treningowy i testowy
train_proportion <- 0.7
train_index <- runif(nrow(wine_data)) < train_proportion
train <- wine_data[train_index,]
test <- wine_data[!train_index,]

# Regresja liniowa - dobry punkt odniesienia
lin_m <- lm(quality ~ ., data = train)

# Drzewo regresji
tree_regr <- rpart(quality ~., data = train, cp = 0.01)
rpart.plot(tree_regr, under = FALSE, fallen.leaves = FALSE, cex = 0.9)

# Drzewo regresji - wieksze
tree_regr_deeper <- rpart(quality ~., data = train, cp = 0.003)
rpart.plot(tree_regr_deeper, under = FALSE, fallen.leaves = FALSE, cex = 0.5)

# Wpływ zmiennych
summary(lin_m)
tree_regr$variable.importance
tree_regr_deeper$variable.importance

# Metody oceny drzewa regresji: RSS, MAE, RMSE, RAE, RRSE, R^2

# odchylenia reszt - rozne miary

# funkcja residuals liczy reszty = wartosci rzeczywiste - prognoza:
all(as.vector(residuals(tree_regr)) == train$quality - predict(tree_regr, train))

modele <- list("tree_regr" = tree_regr, "tree_regr_deeper" = tree_regr_deeper, "lin_m" = lin_m)

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

OcenaModeli(modele, train, 'quality')
OcenaModeli(modele, test, 'quality')
