################################################################################
### IRD
### Drzewa decyzyjne i lasy losowe (klasyfikacyjne)
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

# Źródła:
# https://www.kaggle.com/c/titanic
# http://trevorstephens.com/kaggle-titanic-tutorial/getting-started-with-r/

# Wczytanie danych
titanic_fpath <- "https://raw.githubusercontent.com/nosarzewski/pRojekt_klasyfikacja/main/titanic_full.csv"
tdf <- read.csv(titanic_fpath)

# Krótka eksploracja
head(tdf)
summary(tdf)
str(tdf)

# Oczyszczenie danych
tdf <- tdf %>%
  dplyr::select(-c(name, ticket, cabin, boat, body, home.dest)) %>%
  mutate(survived = as.factor(survived))

str(tdf)

# Sprawdzenie braków danych (NA)
any(is.na(tdf))
apply(tdf, 2, anyNA)
sum(is.na(tdf$age))
sum(is.na(tdf$fare))

# Usunięcie braków danych
tdf <- na.omit(tdf)

# Podział na zbiór treningowy i testowy
## 1. sposób
set.seed(42)
test_prop <- 0.25
test.set.index <- (runif(nrow(tdf)) < test_prop)
tdf.test <- tdf[test.set.index, ]
tdf.train <- tdf[!test.set.index, ]

## 2. sposób
set.seed(42)
test_bound <- floor(nrow(tdf) * test_prop)
tdf <- tdf[sample(nrow(tdf)), ]
tdf.test <- tdf[1:test_bound, ]
tdf.train <- tdf[(test_bound + 1):nrow(tdf), ]

################################################################################
# Modelowanie
################################################################################

# REGRESJA
## Bazowa regresja logistyczna
reg_log_full <- glm(survived ~ ., 
                    data = tdf.train, 
                    family = binomial
)

summary(reg_log_full)

## Dobór zmiennych
reg_log_step <- reg_log_full %>%
  stepAIC(trace = FALSE)

coef(reg_log_full)
coef(reg_log_step)

summary(reg_log_step)

# Do interpretacji (odds)
exp(coef(reg_log_full))
exp(coef(reg_log_step))

# DRZEWA DECYZYJNE
## Budowa drzewa decyzyjnego
tree <- rpart(survived ~ .,
              data = tdf.train,
              method = "class")

## Wizualizacja drzewa
tree

plot(tree)
text(tree, pretty = TRUE)

rpart.plot(tree, under = FALSE, tweak = 1.3, fallen.leaves = TRUE)

# Wartości w węzłach drzewa:
# - przewidywana kategoria
# - prawdopodobieństwo przynależności do kategorii 1
# - procentowy udział obserwacji w danym węźle

## Alternatywne drzewo decyzyjne i opcje
tree_deeper <- rpart(survived ~ .,
                     data = tdf.train,
                     method = "class",
                     control = list(cp = 0.005))
# Inne opcje drzewa
?rpart.control
# minsplit - minimalna liczba obserwacji w liściu wymagana do dokonania dalszego podziału
# minbucket - minimalna liczba obserwacji w liściu końcowym
# maxdepth - maksymalna głębokość drzewa
# cp (complexity paremeter) - minimalna wartość o jaką musi poprawić się jakość modelu aby dokonać podziału w drzewie

rpart.plot(tree_deeper, under = FALSE, tweak = 1.3, fallen.leaves = TRUE)

## Wpływ zmiennych
tree$variable.importance
tree_deeper$variable.importance

# LASY LOSOWE
## Budowa (hodowla?) lasu losowego
rf <- randomForest(survived ~., 
                   data = tdf.train)
# Użyteczne parametry:
# ntree - liczba drzew
# mtry - liczba zmiennych do losowego próbkowania jako kandydaci przy każdym podziale

rf

## Wpływ zmiennych
varImpPlot(rf)
rf$importance

################################################################################
# Weryfikacja jakości klasyfikacji - macierz pomyłek
################################################################################

## Macierz pomyłek
## https://en.wikipedia.org/wiki/Confusion_matrix
table(predict(tree, new = tdf.test, type = "class"), tdf.test$survived)

CM <- list()
### Regresje
CM[["reg_log_full"]] <- table(ifelse(predict(reg_log_full, new = tdf.test, type = "response") > 0.5, 1, 0), tdf.test$survived)
CM[["reg_log_step"]] <- table(ifelse(predict(reg_log_step, new = tdf.test, type = "response") > 0.5, 1, 0), tdf.test$survived)
### Drzewa
CM[["tree"]] <- table(predict(tree, new = tdf.test, type = "class"), tdf.test$survived)
CM[["tree_deeper"]] <- table(predict(tree_deeper, new = tdf.test, type = "class"), tdf.test$survived)
### Las
CM[["rf"]] <- table(predict(rf, new = tdf.test, type = "class"), tdf.test$survived)

EvaluateModel <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  
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
}

lapply(CM, EvaluateModel)
sapply(CM, EvaluateModel)

################################################################################
# Weryfikacja jakości klasyfikacji - ROC
################################################################################

## Prognoza w postaci prawdopodobieństwa, zamiast kategorii
preds <- list()

### Regresje
preds[["reg_log_full"]] <- as.vector(predict(reg_log_full, newdata = tdf.test, type = "response"))
preds[["reg_log_step"]] <- as.vector(predict(reg_log_step, newdata = tdf.test, type = "response"))
### Drzewa
preds[["tree"]] <- as.vector(predict(tree, newdata = tdf.test)[, 2])
preds[["tree_deeper"]] <- as.vector(predict(tree_deeper, newdata = tdf.test)[, 2])
### Las
preds[["rf"]] <- as.vector(predict(rf, newdata = tdf.test, type = "prob")[, 2])

## krzywa ROC (Receiver Operating Characteristic) - potrzebuje "ciaglej" prognozy
plot(performance(prediction(preds[["reg_log_full"]], tdf.test$survived), "tpr", "fpr"), lwd = 2, colorize = T) 

for (i in 1:length(preds)){
  plot(performance(prediction(preds[[i]], tdf.test$survived), "tpr", "fpr"), lwd = 2, colorize = F, col = i, add = ifelse(i == 1, FALSE, TRUE)) 
}

abline(coef = c(0, 1), lty = 2, lwd = 0.5)

legend(0.6, 0.4, 
       legend = names(preds),
       col = 1:length(preds), 
       lty = rep(1, length(preds))
)

# AUC (Area Under Curve) - pole pod krzywa ROC
(performance(prediction(preds[["reg_log_full"]], tdf.test$survived), "auc")@y.values[[1]])

for (i in 1:length(preds)){
  cat(names(preds)[i], ": ", performance(prediction(preds[[i]], tdf.test$survived), "auc")@y.values[[1]], "\n")
}

# Lift chart
plot(performance(prediction(preds[["reg_log_full"]], tdf.test$survived), "lift", "rpp"), lwd = 2, col = "darkblue") 

for (i in 1:length(preds)){
  plot(performance(prediction(preds[[i]], tdf.test$survived), "lift", "rpp"), lwd = 2, colorize = F, col = i, lty = i, add = ifelse(i == 1, FALSE, TRUE)) 
}

legend(0.6, 2.4, 
       legend = names(preds),
       col = 1:length(preds), 
       lty = 1:length(preds)
)

