################################################################################
# Zadanie 1
################################################################################


# Wykonaj clustering roslin ze zbioru danych iris
# uzywajac wymiarow lisci (sepal) zamiast kwiatow (petal).
# Zwizualizuj wyniki. Jak dobrze odkryte grupy pokrywaja sie z naturalnym
# podzialem na gatunki?

# Wykonaj i zwizualizuj analogiczne grupowanie, tym razem uzywajac wszystkich
# dostepnych informacji o wymiarach roslin. Czy poprawilo to wyniki?

head(iris)

# uzywajac wymiarow lisci (sepal) zamiast kwiatow (petal).

irisCluster_sepal <- kmeans(iris[, 1:2], centers = 3, nstart = 10)
iris$Cluster_sepal <- as.factor(irisCluster_sepal$cluster)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species, shape = Cluster_sepal)) + geom_point()

#uzywajac wszystkich dostepnych informacji o wymiarach roslin.

irisCluster_all <- kmeans(iris[, 1:4], centers = 3, nstart = 10)
iris$Cluster_all <- as.factor(irisCluster_all$cluster)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species, shape = Cluster_all)) + geom_point()
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species, shape = Cluster_all)) + geom_point()

table(iris$Cluster_sepal, iris$Species)
table(iris$Cluster_all,  iris$Species)

# Czy poprawilo to wyniki?
# Wzgledem Cluster_sepal - tak.