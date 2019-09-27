################################################################################
### IRD
### Lab 07
### Analiza skupien (clustering, grupowanie) i analiza skladowych glownych
### Powtorzenie przed kolokwium
################################################################################

################################################################################
# Czesc 1: Clustering - grupowanie
################################################################################

library(datasets)
library(ggplot2)
iris

# Wizualizacja zbioru danych - wymiary kwiatow (petal)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

# Wizualizacja zbioru danych - wymiary lisci (sepal)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()

set.seed(1)

# Grupowanie metoda k-srednich - z uzyciem 2 kolumn danych

irisCluster <- kmeans(iris[, 3:4], centers = 3, nstart = 10)
irisCluster

# Porownanie odkrytych grup z podzialem na gatunki
table(irisCluster$cluster, iris$Species)

# Wizualizacja wynikow grupowania
iris$Cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species, shape = Cluster)) + geom_point()

# Alternatywnie - 2 klastry?

irisCluster2 <- kmeans(iris[, 3:4], centers = 2, nstart = 10)
irisCluster2

# Porownanie odkrytych grup z podzialem na gatunki
table(irisCluster2$cluster, iris$Species)

# Wizualizacja wynikow grupowania
iris$Cluster2 <- as.factor(irisCluster2$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species, shape = Cluster2)) + geom_point()

# Grupowanie hierarchiczne - z uzyciem 2 kolumn danych

hclusters <- hclust(dist(iris[, 3:4]))
plot(hclusters)

clusterCut <- cutree(hclusters, 3)
table(clusterCut, iris$Species)

# Grupowanie hierarchiczne - z uzyciem innych danych

hclusters <- hclust(dist(iris[, 1:2]))
plot(hclusters)

hclusterCut <- cutree(hclusters, 3)
table(hclusterCut, iris$Species)

# Wizualizacja wynikow grupowania
iris$ClusterH <- as.factor(hclusterCut)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species, shape = ClusterH)) + geom_point()

####################################################################
# Analiza skladowych glownych (Principal Component Analysis)
####################################################################

iris.pca <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)

summary(iris.pca)
str(iris.pca)

library(ggfortify)
autoplot(iris.pca, data = iris, colour = 'Species')

################################################################################
# Zadanie 1
################################################################################

# Wykonaj clustering roslin ze zbioru danych iris
# uzywajac wymiarow lisci (sepal) zamiast kwiatow (petal).
# Zwizualizuj wyniki. Jak dobrze odkryte grupy pokrywaja sie z naturalnym
# podzialem na gatunki?

# Wykonaj i zwizualizuj analogiczne grupowanie, tym razem uzywajac wszystkich
# dostepnych informacji o wymiarach roslin. Czy poprawilo to wyniki?

################################################################################
# Czesc 2: Powtorzenie przed kolokwium
################################################################################

# Rozwiaz zadania 1, 3, 4 z przykladowego kolokwium: 
# https://github.com/kaftanowicz/ird/blob/master/exam/kolokwium_przyklad.pdf
