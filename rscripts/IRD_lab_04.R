################################################################################
### IRD
### Lab 04
### Drzewa decyzyjne
################################################################################

################################################################################
### Przykład 1: Titanic i biblioteka rpart
################################################################################

# Źródła:
# https://www.kaggle.com/c/titanic
# http://trevorstephens.com/kaggle-titanic-tutorial/getting-started-with-r/

# Biblioteki
library(rpart) # do drzewa
library(rpart.plot) # do rysowania drzewa
library(caret) # do oceny wyników

# Wczytanie danych

titanic_fpath <- "data/titanic_full.csv"
tdf <- read.csv(titanic_fpath)

# Ułamek liczby rekordów przeznaczony do zbioru testowego
test_prop <- 0.25

test_bound <- floor(nrow(tdf)* test_prop)

tdf <- tdf[sample(nrow(tdf)), ]
tdf.test <- tdf[1:test_bound, ]
tdf.train <- tdf[(test_bound+1):nrow(tdf), ]

# Alternatywny sposob podzialu zbioru na uczacy i testowy
test_prop <- 0.25
test.set.index <- (runif(nrow(tdf)) < test_prop)
tdf.test <- tdf[test.set.index, ]
tdf.train <- tdf[!test.set.index, ]


# Eksploracja danych

names(tdf)
str(tdf)
table(tdf$survived)
prop.table(table(tdf$survived))

# Budowa drzewa decyzyjnego

tree <- rpart(survived ~ pclass + sex + age + sibsp + parch + fare + embarked,
              data=tdf.train,
              method="class",
              control = list(maxdepth = 3))

# Wizualizacja drzewa

tree

plot(tree)
text(tree, pretty = TRUE)

rpart.plot(tree, under=FALSE, tweak=1.3, fallen.leaves = TRUE)

# Wartości w węzłach drzewa:
# - przewidywana kategoria
# - prawdopodobieństwo przynależności do kategorii 1
# - procentowy udział obserwacji w danym węźle

# Interpretacja wyników

prop.table(table(tdf$sex, tdf$survived), 1)
prop.table(table(tdf$sex, tdf$survived), 2)

# Weryfikacja jakości klasyfikacji

EvaluateClassifier <- function(response_colname, prediction_colname, df,  positive = "1")
{
  y <- factor(df[response_colname][[1]]) # factor of positive / negative cases
  predictions <- factor(df[prediction_colname][[1]]) # factor of predictions
  precision <- posPredValue(predictions, y, positive)
  recall <- sensitivity(predictions, y, positive)
  F1 <- (2 * precision * recall) / (precision + recall)
  
  return(list(precision=precision, recall=recall, F1=F1))
}

# Weryfikacja jakości klasyfikacji - zbiór uczący i testowy

## https://en.wikipedia.org/wiki/Precision_and_recall

tdf.train$survival_predicted <- predict(tree, tdf.train, type = "class")
tdf.test$survival_predicted <- predict(tree, tdf.test, type = "class")

EvaluateClassifier('survived', 'survival_predicted', tdf.train)
EvaluateClassifier('survived', 'survival_predicted', tdf.test)

# Alternatywne drzewo decyzyjne - głębsze

tree_deeper <- rpart(survived ~ pclass + sex + age + sibsp + parch + fare + embarked,
                     data=tdf.train,
                     method="class",
                     control = list(maxdepth = 10))

rpart.plot(tree_deeper, under=FALSE, tweak=1.5, fallen.leaves = TRUE)

tdf.train$survival_predicted_deeper <- predict(tree_deeper, tdf.train, type = "class")
tdf.test$survival_predicted_deeper <- predict(tree_deeper, tdf.test, type = "class")

EvaluateClassifier('survived', 'survival_predicted_deeper', tdf.train)
EvaluateClassifier('survived', 'survival_predicted_deeper', tdf.test)

################################################################################
### Przyklad 2: Pakiet caret - na przykladzie drzewa klasyfikacyjnego 
################################################################################

# http://topepo.github.io/caret/index.html

library(caret)

set.seed(1)

## wczytanie danych

data <- read.csv2('data/income.csv', na.strings = '')
data <- na.omit(data)

## podzial na zbior testowy i uczacy
inTraining <- createDataPartition(data$income, p = .8, list = FALSE)
training <- data[ inTraining,]
#training  <- na.omit(training) # gdybysmy wczesniej nie usuneli z calego zbioru
testing  <- data[-inTraining,]

## budowa modelu na zbiorze uczacym - na poczatek okreslamy sposob uczenia

## 5-krotna walidacja krzyzowa
fitControl <- trainControl(
  method = "cv",
  number = 5)

# Najpierw proste drzewo z CV
treeCaret_simple <- train(income ~ ., data = training, 
                          method = "rpart", 
                          trControl = fitControl)

plot(treeCaret_simple)
rpart.plot(treeCaret_simple$finalModel)

# Ewaluacja
confusionMatrix(data = predict(treeCaret_simple, testing), reference = testing$income, mode = "everything")

# Teraz recznie zadajemy zbior wartosci parametru zlozonosci do przeszukania
rpartGrid <- expand.grid(cp = seq(0.001, 0.1, by = 0.005))

treeCaret <- train(income ~ ., data = training, 
                   method = "rpart", 
                   trControl = fitControl,
                   tuneGrid = rpartGrid)
treeCaret
# https://en.wikipedia.org/wiki/Cohen%27s_kappa
plot(treeCaret)
rpart.plot(treeCaret$finalModel)

# Ewaluacja
confusionMatrix(data = predict(treeCaret, testing), reference = testing$income, mode = "everything")

################################################################################
# Zadanie 1
################################################################################

# W oparciu o przyklad Titanica
# zbuduj 2 drzewa decyzyjne przewidujace klase samochodu 
# w zaleznosci od jego pozostalych parametrow.

# Niech pierwsze z drzew ma maksymalna glebokosc rowna 3, drugie - rowna 5.

# Zwizualizuj drzewa. Czym sie roznia miedzy soba?
# Policz precyzje i czulosc (precision and recall) klasyfikacji
# z uzyciem obu drzew na zbiorze testowym. Porownaj wyniki. Co jest przyczyna roznicy?

# Uzyj 80% rekordow jako zbioru uczacego i 20% jako zbioru testowego.

# Poczatek:

# Inicjalizacja ziarna do zmiennych pseudolosowych
set.seed(1)

# Dane

# Informacje o zbiorze danych: https://rpubs.com/chitrav/118220

data_fpath <- 'data/car.data.txt' # Wpisac poprawna sciezke dostepu w zaleznosci od lokalizacji pliku
DATA_SET <- read.csv(data_fpath, header = FALSE)
names(DATA_SET) <- c("buying", "maint", "doors", "persons",
                     "lug_boot", "safety", "class")

# Eksploracja danych
str(DATA_SET)
summary(DATA_SET)

# Laczymy klasy acc, good, vgood w jedna

DATA_SET$class <- factor(ifelse(DATA_SET$class == "unacc", 0, 1))

################################################################################
# Zadanie 2
################################################################################

# Uzywajac pakietu caret, zbuduj model drzewa klasyfikacyjnego 
# do przewidywania jakosci czerwonego wina. 
# Uzyj 3-krotnej walidacji krzyzowej. Pokaz jego metryki
# dla zbioru uczacego i testowego.
# Zbuduj kolejne drzewo, tym razem uzywajac 10-krotnej walidacji krzyzowej.
# Porownaj jego metryki (dla zbioru uczacego i testowego) z metrykami poprzedniego drzewa.
