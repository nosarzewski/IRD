################################################################################
### IRD
### Lab 06
### Lasy losowe i reguly asocjacyjne 
################################################################################

################################################################################
### Czesc 1: Lasy losowe
################################################################################

# Biblioteki
library(readr) # do wczytywania danych
library(tidyverse) # do przeksztalcania danych
library(ROCR) # do krzywej ROC
library(rpart) # do drzewa decyzyjnego
library(rpart.plot) # do wizualizacji drzewa decyzyjnego
library(randomForest) # do budowy (zasadzenia?) lasu losowego
library(caret) # do modeli i ich ewaluacji

################################################################################

# Wczytanie i eksploracja danych
data <- read_csv2("data/income.csv", na = c("?", ""))
str(data)
summary(data)

apply(data, 2, unique)

data <- na.omit(data)
data <- select(data, age, workclass,education, income, maritalStatus, occupation, capitalGain,
               capitalLoss, hourPerWeek, nativeCountry, race,sex, relationship)

if (any(data$income == "<=50K")) data$income <- ifelse(data$income == "<=50K", "low", "high")
# Uzycie instrukcji warunkowej nie jest niezbedne,
# ale chroni nas przed zepsuciem danych w sytuacji przypadkowego wywolania tej linijki
# wiecej, niz jeden raz.

table(data$income)

data <- mutate(data, 
               income = factor(income),
               workclass = factor(workclass),
               education = factor(education), 
               maritalStatus = factor(maritalStatus),
               occupation = factor(occupation),
               nativeCountry = factor(nativeCountry), 
               race = factor(race),
               sex = factor(sex), 
               relationship = factor(relationship)
               )

# Podzial zbioru na uczacy i testowy
train_dataset_proportion <- 0.8
train_index <- (runif(nrow(data), 0, 1) <= train_dataset_proportion)

train <- data[train_index, ]
test <- data[!train_index, ]

# Budowa (hodowla?) lasu losowego
rf <- randomForest(income ~., data = train)

# Uzyteczne parametry:
# ntree - liczba drzew
# mtry - liczba zmiennych do losowego próbkowania jako kandydaci przy każdym podziale

varImpPlot(rf)

# Ocena modelu 

# Funkcja ponizej to rozwiazanie zadania 1) z zajec numer 5:
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

rf_classif_mx <- table(predict(rf, new = test, type = "class"), test$income)
EvaluateModel(rf_classif_mx)

# Dla porownania: drzewo klasyfikacyjne (powtorzenie z poprzednich zajec)

dtree <- rpart(income ~., data = train,  method = "class")
dtree_classif_mx <- table(predict(dtree, new = test, type = "class"), test$income)
EvaluateModel(dtree_classif_mx)

# Ale jakie AUC dla drzewa?
prognoza_ciagla <- predict(dtree, newdata = test)
prognoza_ciagla <- as.vector(prognoza_ciagla[,2])
performance(prediction(prognoza_ciagla, test[["income"]]),"auc")

# A jakie bedzie dla lasu losowego?

## wykresy diagnostyczne - znow powtorka

forecast <- predict(rf, newdata = test, type = "prob")[,2]
plottingData <- prediction(forecast, test$income)

# krzywa ROC - potrzebuje "ciaglej" prognozy
plot(performance(plottingData,"tpr","fpr"),lwd=2, colorize=T) 

#AUC (Area Under Curve) - pole pod krzywa ROC
performance(plottingData,"auc")@y.values[[1]]
# skladnia obiektowa: @ zamiast $, [[1]] do wyciagniecia elementu z listy

# Sensitivity/specificity plots ~ trade-off
plot(performance(plottingData ,"sens","spec"),lwd=2) 

# Lift chart
plot(performance(plottingData ,"lift","rpp"),lwd=2, col = "darkblue") 

################################################################################
### Czesc 2: Reguly asocjacyjne
################################################################################

# Skrypt wykorzystuje fragmenty kodu z artykulu 
# https://statistical-research.com/index.php/2012/09/26/association-rule-learning-and-the-apriori-algorithm/
# za wiedza i zgoda autora.

# Biblioteki
#install.packages("arules")
library("arules") # do znajdowania regul
#install.packages("arulesViz")
library("arulesViz") # do wizualizacji regul

################################################################################

# Zrodlo danych: https://wiki.csc.calpoly.edu/datasets/wiki/ExtendedBakery

# Wczytanie danych o nazwach produktow i ich obrobka z uzyciem wyrazen regularnych
# (wyrazenia regularne nie obowiazuja na kolokwium)

goods_names <- readLines("data/EB-build-goods.sql")
goods_names <- gsub("^[a-zA-Z0-9 \\(]*,'", "", goods_names, fixed = FALSE)
goods_names <- gsub("','", " ", goods_names, fixed = FALSE)
goods_names <- gsub("'.*$", "", goods_names, fixed = FALSE)

# Wczytanie i eksploracja danych o transakcjach
df_all <- read.csv("data/75000-out2.csv", header = FALSE,
                   row.names = 1)

df <- df_all[1:40000, ] # wybieramy pierwsze 40000 transakcji

# Zamiana ramki danych na macierz i nadanie nazw kolumnom 
mx <- as.matrix(df)
colnames(mx) <- goods_names
ts <- as(mx, "transactions")

# Znajdowanie regul asocjacyjnych algorytmem apriori
rules = apriori(ts, parameter=list(support=0.01, confidence=0.5))

# Podglad regul
rules
inspect(head(sort(rules, by="lift"),3))

# Wizualizacja regul asocjacyjnych - rozne sposoby

plot(rules)
head(quality(rules));
plot(rules, measure=c("support","lift"), shading="confidence")
plot(rules, shading="order", control=list(main ="Two-key plot")) # order - liczba dobr w regule

# Wybor regul asocjacyjnych z najwieksza pewnoscia 

subrules = rules[quality(rules)$confidence > 0.9]
subrules

# Rozne sposoby wizualizacji
plot(subrules, measure=c("support","lift"), shading="confidence")
plot(subrules, shading="order", control=list(main ="Two-key plot"))
plot(subrules, method="matrix", shading="lift")
plot(subrules, method="matrix", shading="confidence")
plot(subrules, method="grouped")

# Wybor regul asocjacyjnych z najwiekszym liftem 
# i kolejne sposoby wizualizacji
subrules2 = head(sort(rules, by="lift"), 3)
plot(subrules2, method="graph")
plot(subrules2, method="paracoord")

oneRule = sample(rules, 1)
inspect(oneRule)

################################################################################
# Dodatkowe materialy dla zainteresowanych:
# https://rdatamining.wordpress.com/2012/07/13/examples-and-resources-on-association-rule-mining-with-r/

################################################################################
# Zadanie 1
################################################################################

# Uzupelnij skrypt z cwiczen 5 (przewidywanie jakosci bialego wina na podstawie jego
# parametrow chemicznych) o model lasu losowego. Porownaj rozne metryki jakosci
# tego modelu do analogicznych metryk jakosci modeli uzywanych na cwiczeniach 5.

################################################################################
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

