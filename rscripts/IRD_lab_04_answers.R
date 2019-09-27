################################################################################
# IRD zajecia nr 4
# Drzewa decyzyjne
################################################################################

# Zadanie 1

# W oparciu o przyklad omowiony na wykladzie
# (skrypt IRD_2017_lecture_2017-11-07_decision_trees_in_R.R)
# zbuduj drzewo decyzyjne przewidujace klase samochodu 
# w zaleznosci od jego pozostalych parametrow.
# Uzyj 80% rekordow jako zbioru uczacego i 20% jako zbioru testowego.

# Inicjalizacja ziarna do zmiennych pseudolosowych
set.seed(1)

# Dane
data_fpath <- 'data/car.data.txt' # Wpisac poprawna sciezke dostepu w zaleznosci od lokalizacji pliku
DATA_SET <- read.csv(data_fpath, header = FALSE)
names(DATA_SET) <- c("buying", "maint", "doors", "persons",
                     "lug_boot", "safety", "class")

# Eksploracja danych
str(DATA_SET)
summary(DATA_SET)

# Laczymy klasy acc, good, vgood w jedna

DATA_SET$class <- factor(ifelse(DATA_SET$class == "unacc", 0, 1))

# Biblioteki
library(rpart) # do drzewa
library(rpart.plot) # do rysowania drzewa
library(caret) # do oceny wyników

test_prop <- 0.20
test_bound <- floor(nrow(DATA_SET)* test_prop)
tdf <- DATA_SET[sample(nrow(DATA_SET)), ] # mieszamy losowo kolejnosc wierszy
tdf.test <- tdf[1:test_bound, ]
tdf.train <- tdf[(test_bound+1):nrow(tdf), ]

# Budowa drzew decyzyjnych

tree3 <- rpart(class ~ buying + maint + doors + persons + lug_boot + safety,
               data=tdf.train,
               method="class",
               control = list(maxdepth = 3))

rpart.plot(tree3, under=FALSE, tweak=1.3, fallen.leaves = TRUE)

tree5 <- rpart(class ~ buying + maint + doors + persons + lug_boot + safety,
               data=tdf.train,
               method="class",
               control = list(maxdepth = 5))

tree5 <- rpart(class ~ .,
               data=tdf.train,
               method="class",
               control = list(maxdepth = 5))

rpart.plot(tree5, under=FALSE, tweak=1.3, fallen.leaves = TRUE)

# Ewaluacja wynikow

EvaluateClassifier <- function(response_colname, prediction_colname, df,  positive="1")
{
  y <- factor(df[response_colname][[1]]) # factor of positive / negative cases
  predictions <- factor(df[prediction_colname][[1]]) # factor of predictions
  precision <- posPredValue(predictions, y, positive)
  recall <- sensitivity(predictions, y, positive)
  F1 <- (2 * precision * recall) / (precision + recall)
  
  return(list(precision=precision, recall=recall, F1=F1))
}

# Weryfikacja jakości klasyfikacji

## https://en.wikipedia.org/wiki/Precision_and_recall

# tree3

tdf.test$prediction3 <- predict(tree3, tdf.test, type = "class")
EvaluateClassifier('class', 'prediction3', tdf.test)

# tree5

tdf.test$prediction5 <- predict(tree5, tdf.test, type = "class")
EvaluateClassifier('class', 'prediction5', tdf.test)



################################################################################
# Zadanie 2
################################################################################

# Uzywajac pakietu caret, zbuduj model drzewa klasyfikacyjnego 
# do przewidywania jakosci czerwonego wina. 
# Uzyj 3-krotnej walidacji krzyzowej. Pokaz jego metryki
# dla zbioru uczacego i testowego.
# Zbuduj kolejne drzewo, tym razem uzywajac 10-krotnej walidacji krzyzowej.
# Porownaj jego metryki (dla zbioru uczacego i testowego) z metrykami poprzedniego drzewa.

dane <- read.csv2('data/winequality-red.csv',  stringsAsFactors = FALSE, dec = '.')
if (typeof(dane$quality) == "integer") dane$quality <- ifelse(dane$quality >= 6, 'high', 'low')
## Uzycie warunku na typ zmiennej zabezpiecza nas przed zepsuciem danych
## w przypadku przypadkowego wywolania tej linijki wiecej niz 1 raz.
## Nie jest konieczne, ale czyni kod bezpieczniejszym.

library(caret)
set.seed(1)

## podzial na zbior testowy i uczacy
inTraining <- createDataPartition(dane$quality, p = .8, list = FALSE)
training <- dane[ inTraining,]
training  <- na.omit(training)
testing  <- dane[-inTraining,]

## 3-krotna walidacja krzyzowa
fitControl_3 <- trainControl(
  method = "cv",
  number = 3)

treeCaret_3 <- train(quality ~ ., data = training, 
                     method = "rpart", 
                     trControl = fitControl_3)

plot(treeCaret_3)
rpart.plot(treeCaret_3$finalModel)

# Ewaluacja
confusionMatrix(data = predict(treeCaret_3, training), reference = training$quality, mode = "everything")
confusionMatrix(data = predict(treeCaret_3, testing), reference = testing$quality, mode = "everything")

## 10-krotna walidacja krzyzowa
fitControl_10 <- trainControl(
  method = "cv",
  number = 10)

treeCaret_10 <- train(quality ~ ., data = training, 
                      method = "rpart", 
                      trControl = fitControl_10)

plot(treeCaret_10)
rpart.plot(treeCaret_10$finalModel)

# Ewaluacja
confusionMatrix(data = predict(treeCaret_10, training), reference = training$quality, mode = "everything")
confusionMatrix(data = predict(treeCaret_10, testing), reference = testing$quality, mode = "everything")

