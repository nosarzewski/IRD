################################################################################
### IRD
### Lab 03
### Wizualizacja danych
################################################################################

# Biblioteki
#install.packages("ggplot2")
library(ggplot2)
library(tidyverse) 

# Dane
data(diamonds)
diamonds <- sample_frac(diamonds, 0.2)

# Eksploracja danych
str(diamonds)
summary(diamonds)

# price
# price in US dollars (\$326--\$18,823)
# 
# carat
# weight of the diamond (0.2--5.01); 1 carat == 200 milligrams
# 
# cut
# quality of the cut (Fair, Good, Very Good, Premium, Ideal)
# 
# color
# diamond colour, from J (worst) to D (best)
# 
# clarity
# a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))
# 
# x
# length in mm (0--10.74)
# 
# y
# width in mm (0--58.9)
# 
# z
# depth in mm (0--31.8)
# 
# depth
# total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43--79)
# 
# table
# width of top of diamond relative to widest point (43--95)

################################################################################

# Histogram
hist(diamonds$carat)
hist(diamonds$carat, col = "red")
hist(diamonds$carat, col = "red", breaks = 20)
hist(diamonds$carat, col = "red", breaks = 50)
hist(diamonds$carat, col = "red", breaks = 100)
hist(diamonds$carat, col = "red", breaks = 500)

# Wykresy bazowe

plot(diamonds$carat, diamonds$price)

plot(diamonds$carat, diamonds$price, col = "blue", pch = 20,
     xlim = c(0, 8), ylim = c(0, 20000), 
     xlab = "masa [karaty]", ylab = "cena [USD]",
     main = "Diamenty: zaleznosc ceny od masy")

################################################################################
# ggplot
################################################################################

 ##kazdy wykres sklada sie:
ggplot(diamonds)   +  # z odwolania do zbioru danych 
geom_point( # warstw ~ 'ksztaltow'
  aes( x = carat, y = price) # przyporzadkowania zmiennych do osi wewnatrz warstwy 
)

## ale 'rzutowania' mozna podac od razu: 
ggplot(diamonds,  aes( x = carat, y = price) ) +
  geom_point( ) ## wtedy wszystkie warstwy maja domyslne przyporzadkowane zmienne

### jakie sa inne warstwy? 

## linia (lamana)
ggplot(diamonds)   +
  geom_line( 
    aes( x = carat, y = price, color = "red") 
  )

## punkty + linia trendu
ggplot(diamonds, aes( x = carat, y = price) )   +
  geom_point( ) +
  geom_smooth( 
    aes( x = carat, y = price) 
  )
  
ggplot(diamonds, aes( x = carat, y = price) )   +
  geom_point( ) +
  geom_smooth( 
    aes( x = carat, y = price),
    method = "lm"
  )  

ggplot(diamonds,  aes( x = carat, y = price) ) +
  geom_point( ) +
  geom_smooth(method = "loess") 

## Wykres pudelkowy (boxplot) 

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

## wykres slupkowy 

diamondsMean <- diamonds %>% 
  group_by(., clarity) %>% 
  summarise(., price = mean(price))

# Srednia cena w podziale na szlif
ggplot(diamondsMean) + 
  geom_bar( aes(x = clarity, y = price ), 
            stat = "identity") 

diamondsMeanCut <- diamonds %>% 
  group_by(., clarity, cut) %>% 
  summarise(., price = mean(price))

# Srednia cena w podziale na szlif i czystosc

ggplot(diamondsMeanCut) + 
  geom_bar( aes(x = clarity, y = price, fill = cut ),
            stat = "identity") 

ggplot(diamondsMeanCut) + 
  geom_bar( aes(x = clarity, y = price, fill = cut ),
            stat = "identity",
            position = "dodge") 

## Histogram

ggplot(diamonds) + 
  geom_histogram( aes(x = carat )) 

ggplot(diamonds) + 
  geom_histogram( aes(x = carat), binwidth = 0.02) 

## Wykres gestosci 

ggplot(diamonds) + 
  geom_density( aes(x = price )) 

# W zaleznosci od czystosci
ggplot(diamonds) + 
  geom_density( aes(x = price, fill = clarity)) 

# alpha - parametr przejrzystosci
ggplot(diamonds) + 
  geom_density( aes(x = price, fill = clarity), alpha = 1) 

ggplot(diamonds) + 
  geom_density( aes(x = price, fill = clarity), alpha = 0.2) 

### jakie moga byc parametry kazdej warstwy?

## color
ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price,
         col = clarity)
  )

## ale
ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price), 
    col = "blue"
    )

ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price, col = "blue")
  )

## fill

ggplot(diamonds)   +  
  geom_smooth( 
    aes( x = carat, y = price,
         fill = clarity)
  )

### roznica miedzy fill a color 

ggplot(filter(diamonds, clarity == "I1",carat <=1))   +  
  geom_bar( 
    aes( x = carat , fill = clarity)
  )

ggplot(filter(diamonds, clarity == "I1", carat <=1))   +  
  geom_bar( 
    aes( x = carat , col = clarity)
  )

## shape 

ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price,
         col = clarity,
         shape = cut)
  )

## size

ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price,
         size = z),
    alpha = 0.4
  )


## typ linii 
ggplot(diamondsMeanCut)   +  
  geom_line( 
    aes( x = clarity , y = price , linetype = cut, group = cut)
  )

### grupowanie wykresow 

ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price)) 

ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price)) +
    facet_grid(cut ~. )

ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price)) +
  facet_grid(. ~ cut )

ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price)) +
  facet_grid(clarity ~ cut )

### kwestie estetyczne 

ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price,
         col = clarity)
  )

## sterowanei wygladem poprzez theme i jego predefiniowane warianty
ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price,
         col = clarity)
  ) + 
  theme_bw(base_size = 16)


ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price,
         col = clarity)
  ) + 
  theme_bw(base_size = 16) +
  xlab("Karaty") +
  ylab("Cena") +
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                                   size=14, angle=45))
  
## kolejnosc ma znaczenie: 

ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price,
         col = clarity)
  ) + 
  xlab("Karaty") +
  ylab("Cena") +
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                                   size=14, angle=45)) +
  theme_bw(base_size = 16) 


## zmiany na osiach 
ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price,
         col = clarity)
  ) + 
  xlab("Karaty") +
  ylab("Cena") +
  theme_bw(base_size = 16)  +
  scale_x_continuous(breaks = c(0:5), 
                     labels = c("0 karatow", "1 karat", "2 karaty", "3 karaty", "4 karaty", "5 karatow")) 

## zmiany legendy 
ggplot(diamonds)   +  
  geom_point( 
    aes( x = carat, y = price,
         col = clarity)
  ) + 
  xlab("Karaty") +
  ylab("Cena") +
  theme_bw(base_size = 16)  +
  scale_color_discrete(name = "Czystosc")


### eksportowanie wykresow 

pdf("diamonds.pdf", width = 10, height = 8)

print(
  ggplot(diamonds)   +  
        geom_point( 
          aes( x = carat, y = price))
      )

dev.off()

## wiele wykresow
clarityVal  <- unique(diamonds$clarity)

for(i in 1:length(clarityVal)){
pdf(
  paste0("diamonds",i,".pdf"), 
  width = 10, height = 8)

print(
  ggplot(
    filter(diamonds, clarity == clarityVal[i])  
    ) +  
    geom_point( 
      aes( x = carat, y = price)) +
      ggtitle(clarityVal[i])
)

dev.off()
}

  pdf(
    "diamondsClarity.pdf", 
    width = 10, height = 8)
  for(i in 1:length(clarityVal)){
  print(
    ggplot(
      filter(diamonds, clarity == clarityVal[i])  
    ) +  
      geom_point( 
        aes( x = carat, y = price)) +
      ggtitle(clarityVal[i])
  )
  
  }
  
 dev.off()
 
 
### dodatek - zmiana struktury danych funkcjami melt i dcast 

 #install.packages("datasets")
 #install.packages("reshape2")
 library(reshape2)
 
 # Indeksy giełd europejskich
 EuStockMarkets <- as_tibble(
   data.frame(time = time(EuStockMarkets), EuStockMarkets)
 )
 
 meltedStock <- as_tibble(melt(EuStockMarkets, id = "time" ))
 
 ggplot(meltedStock) +
   geom_line(aes(x = time, y = value, col = variable))
 
 ##i powrot do poprzedniej postaci
 dcast(meltedStock, time ~ variable )
 
 
### Zadania

# 1)
# Dla danych o samochodach (cars) narysuj wykres punktowy dystansu hamowania 
# w zaleznosci od predkosci, uzywajac:
# a) funkcji bazowych,
# b) ggplot.
 
# W podpunkcie b) dopasuj linie trendu (na rozne sposoby). Wybierz najlepsza, wybor uzasadnij.
# Nadaj wykresom tytuly i opisz osie (po polsku).
 
data(cars)

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

#(Sepal oznacza lisc, petal - kwiat)

data(iris)
