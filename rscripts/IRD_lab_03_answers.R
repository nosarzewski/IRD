################################################################################
# IRD zajecia nr 3
# Wizualizacja danych
################################################################################

### Zadania

# 1)
# Dla danych o samochodach (cars) narysuj wykres punktowy dystansu hamowania 
# w zaleznosci od predkosci, uzywajac:
# a) funkcji bazowych,
# b) ggplot.
 
# W podpunkcie b) dopasuj linie trendu (na rozne sposoby). Wybierz najlepsza, wybor uzasadnij.
# Nadaj wykresom tytuly i opisz osie (po polsku).
 
data(cars)
cars <- cars

str(cars)
summary(cars)

plot(x = cars$speed, y = cars$dist,
     xlab = "Predkosc", ylab = "Dystans hamowania", 
     main = "Wykres punktowy dystansu hamowania\n w zaleznosci od predkosci")

library(ggplot2)

plot_gg <- ggplot(cars, aes( x = speed, y = dist) )   +
  geom_point( ) +
  xlab("Predkosc") +
  ylab("Dystans hamowania") +
  ggtitle("Wykres punktowy dystansu hamowania w zaleznosci od predkosci")

plot_gg

# Rozne metody dopasowywania krzywych:
# http://ggplot2.tidyverse.org/reference/geom_smooth.html

plot_gg + geom_smooth(method = "lm")
plot_gg + geom_smooth(method = "glm")
plot_gg + geom_smooth(method = "gam")
plot_gg + geom_smooth(method = "loess")
plot_gg + geom_smooth(method = "loess", span = 2)

# Wiemy z fizyki, ze energia kinetyczna jest proporcjonalna do kwadratu predkosci,
# wiec spodziewamy sie krzywej drugiego stopnia.

## 2)
# Dla danych iris zwizualizuj zaleznosci pomiedzy wymiarami elementow 
# kwiatow w zaleznosci od gatunku, uzywajac pakietu ggplot.
# Nadaj wykresom tytuly i opisz osie (po polsku).

# Sprobuj roznych (co najmniej 2) podejsc do wizualizacji danych. 
# Wskaz sposob, ktory Twoim zdaniem bedzie najbardziej czytelny 
# dla osoby niezaznajomionej ze statystyką i programowaniem.

# Zwiazki do pokazania (w zaleznosci od gatunku):
# a) Dlugosc kwiatu a szerokosc kwiatu
# b) Dlugosc liscia a szerokosc liscia
# c) Dlugosc liscia a dlugosc kwiatu

# (Sepal oznacza lisc, petal - kwiat)

data(iris)
iris <- iris

str(iris)
summary(iris)

# a) Dlugosc kwiatu a szerokosc kwiatu

ggplot(iris)   +  
  geom_point( 
    aes( x = Petal.Width, y = Petal.Length)) +
  facet_grid(. ~ Species) +
  xlab("Szerokosc kwiatu") +
  ylab("Dlugosc kwiatu ") +
  ggtitle("Dlugosc kwiatu a szerokosc kwiatu")

ggplot(iris)   +  
  geom_point( 
    aes( x = Petal.Width, y = Petal.Length,
         col = Species)
  ) +
  xlab("Szerokosc kwiatu") +
  ylab("Dlugosc kwiatu") +
  ggtitle("Dlugosc kwiatu a szerokosc kwiatu") +
  labs(color='Gatunek') 

# b) Dlugosc liscia a szerokosc liscia

ggplot(iris)   +  
  geom_point( 
    aes( x = Sepal.Width, y = Sepal.Length)) +
  facet_grid(. ~ Species) +
  xlab("Szerokosc liscia") +
  ylab("Dlugosc liscia") +
  ggtitle("Dlugosc liscia a szerokosc liscia")

ggplot(iris)   +  
  geom_point( 
    aes( x = Sepal.Width, y = Sepal.Length,
         col = Species, shape = Species)
  ) +
  xlab("Szerokosc liscia") +
  ylab("Dlugosc liscia") +
  ggtitle("Dlugosc liscia a szerokosc liscia") +
  labs(color='Gatunek', shape = "Gatunek") 

# c) Dlugosc liscia a dlugosc kwiatu

ggplot(iris)   +  
  geom_point( 
    aes( x = Petal.Length, y = Sepal.Length)) +
  facet_grid(. ~ Species) +
  xlab("Dlugosc kwiatu") +
  ylab("Dlugosc liscia") +
  ggtitle("Dlugosc liscia a dlugosc kwiatu")

ggplot(iris)   +  
  geom_point( 
    aes( x = Petal.Length, y = Sepal.Length,
         col = Species, shape = Species)
  ) +
  xlab("Dlugosc kwiatu") +
  ylab("Dlugosc liscia") +
  ggtitle("Dlugosc liscia a dlugosc kwiatu") +
  labs(color='Gatunek', shape = "Gatunek") 

# Dodatkowa informacja o podziale na gatunki jest zwykle czytelniejsza,
# gdy przedstawia sie ja za pomoca koloru, nie na osobnych wykresach,
# ulatwia to porowanywanie zaleznosci pomiedzy gatunkami.
# Gdy punkty nakladaja sie na siebie, oprocz zmiennego koloru
# warto rozwazyc dodanie zmiennego ksztaltu punktow.