################################################################################
### IRD
### Lab 01
### Wprowadzenie do języka R
################################################################################

# Plan
# 1) Organizacja zajęć
# 2) Ogólnie o R
# 3) Wprowadzenie do R
# 4) Operacje matematyczne, podstawowe funkcje matematyczne/statystyczne
# 5) Typy zmiennych
# 6) Obiekty w R

### 1) Organizacja zajęć

# zaliczenie zajęć za minimum 50%, składowe:
## ćwiczenia: kolokwium za 50 pkt., 2 terminy sesji
## można mieć materiały (dowolne) i korzystać z Internetu, nie można się komunikować
## wykład: test za 50 pkt.
## projekt: 20 pkt.
# adres repozytorium z materiałami: https://github.com/nosarzewski/IRD

### 2) Ogólnie o R

## Strona projektu
# http://www.r-project.org/

## Literatura 

# Kamiński B., Zawisza M. 'Receptury w R', http://bogumilkaminski.pl/projekty/)
# Gareth James, Daniela Witten, Trevor Hastie and Robert Tibshirani
# An Introduction to Statistical Learning with Applications in R (dostępna za darmo wraz z kodami: http://www-bcf.usc.edu/~gareth/ISL/)
# http://cran.r-project.org/manuals.html

## Kursy

# http://swirlstats.com/
# https://www.datacamp.com/courses/free-introduction-to-r

## Instalacja 
# http://r.meteo.uni.wroc.pl/
# https://www.rstudio.com/

### 3) RStudio - praca z programem

# Tworzymy projekt (File -> New Project...), np. "IRD". W katalogu projektowym tworzomy katalogi rscripts oraz data.
# pobieramy z Internetu wskazane skrypty i pliki z danymi i umieszczamy w odpowiednich katalogach.

## wybrane skróty klawiszowe w R Studio
# CTRL+ENTER / CTRL+R: wykonywanie instrukcji ze skryptu (w R GUI - F5): 
#  aktualna linijka bądź zaznacznie
# CTRL+L: czyszczenie konsoli
# CTRL+ strzałka w górę: lista ostatnich komend
# TAB: podpowiedzi do funkcji/obiektów
# TAB po nawiasie otwierającym funkcji: argumenty funkcji
# F1 na funkcji: otwiera pomoc
# CTRL+1: przełączanie do edytora
# CTRL+2: przełączanie do konsoli

### 4) Wprowadzenie do R

## pomoc
help(plot)
?plot # nazwa identyczna jak podana
??regression # szukamy po słowie

## katalog roboczy
getwd() # w jakim katalogu pracujemy?

#setwd('..') # zmiana katalogu roboczego
# session -> set working directory..

dir() # zawartość katalogu roboczego
list.files()

## pakiety

# Pakiety (biblioteki) sa to zbiory funkcji i danych, które zostaly stworzone 
# przez innych uzytkowników R i udostępnione za darmo.
# Pakiety nie są automatycznie dostępne, trzeba je najpierw załadować
search() # jakie pakiety są w pamięci
install.packages("data.table") # instalowanie pakietu
library(data.table) # ładowanie pakietu
require(plyr)
# gdy biblioteka nie jest zainstalowana require zwraca FALSE,
# a library generuje błąd
detach("package:dplyr") # wyłączanie pakietu

### 5) Operacje matematyczne, podstawowe funkcje matematyczne/statystyczne

## operacje przypisania, operacje matematyczne
a <- 5
a

a * 3
a - 1

# Różne rodzaje dzielenia:
a / 2

10 %% 3   # reszta z dzielenia
10 %/% 3  # część całkowita z dzielenia
938749324789203 %% 10 # ostatnia cyfra

# Przypisywanie wartości zmiennym i porównywanie zmiennych
a <- 2
b <- 3
a == b
a > b
a < b
a != b

2 == 3
2 + 2 == 4
3 < 5
3 <= 5
3 <= 5 | 10 > 100 # lub
3 <= 5 & 10 > 100 # oraz

# Rodzaje cudzysłowów zwykle nie mają znaczenia
var_t <- "SGH"
var_t2 <- 'SGH'

var_t == var_t2

## wbudowane funkcje (wybrane)
sqrt(16) 
abs(-99)
sum(5, 6)
cos(pi)
sin(pi)
factorial(4) # silnia
log(4)
log(10)
log10(10)
logb(56, base = 5)
logb(base = 5, x = 56)
logb(5, 56)
exp(0)
round(pi, 2)
round(6.592, digits = 2)

ceiling(3.3)
floor(3.6)
trunc(5.99)
trunc(-1.5)
floor(-1.5)
ceiling(-1.5)

## przypisywanie
x <- 5 # najpopularniejszy, deklarowanie 

##############################################################################################
# Dygresja o różnicy między "=" a "<-":
# to szczegóły mało istotne na codzień, lepiej po prostu konsekwentnie używać "<-"
x = 5 # trzeba uważać, np:

# przykład 1:
sum(x = 1:10)
x # x nie zostało zadeklarowane w workspace
sum(y <- 1:10) # y deklarowane w workspace
y

# przykład 2: argumenty funkcji
x <- 1 # deklarujemy zmienną
x
cos(x = 0) # podajemy argument funkcji "cos" o nazwie 'x'
# zwraca cos(0) = 1
x # x nadal równy 1 --> "=" nie przypisało wartości 0 do x
cos(x <- 0) # ponownie zwraca cos(0) = 1
x # "<-": do x przypisano wartość 0

# Koniec dygresji
##############################################################################################

## workspace
ls() # sprawdzenie, co znajduje się w Workspace
rm("x") # usuwanie elementu z Workspace
rm(list = ls()) # usuwa wszystkie obiekty z workspace

### 6) Typy zmiennych

calkowita <- 4L
calkowita
typeof(calkowita)
class(calkowita)

przecinkowa <- 1.5
przecinkowa
typeof(przecinkowa)

tekstowa <- "tekst"
tekstowa
typeof(tekstowa)

logiczna <- TRUE
logiczna

typeof(logiczna)
T == TRUE    # obie formy są równoważne, ale czytelniej jest używać pełnej
F == FALSE  

## przydatne stałe
Inf
Inf + 2
24 / 0
-Inf
-24 / 0

NaN # Not a Number
0 / 0
Inf - Inf

NULL # brak danych - w sensie technicznym
v <- c(1, NA, NULL)
v
NA # brak danych - w sensie analitycznym
NA + 3 # operacje na NA dają w wyniku NA
NA == 5
is.na(NA) # używane głównie do usuwania brakujących wartości
is.na(3)

# Klasa to co innego niż typ. Typ który określa wewnętrzny sposób 
# przechowywania obiektu. Klasa to atrybut obiektu w sensie programowania
# obiektowego.
typeof(1)
class(1)

### 7) Obiekty w R

## skalary:
n <- 100
n

## wektory: wszystkie elementy muszą być tego samego typu:
v1 <- 1:10
v1
v2 <- c(1, 4, 6, 3, 11, 3)
v2
typeof(v2)
sort(v2)
length(v2)
unique(v2)
sum(v1)

v3 <- c("ala", "ma", "kota") # wektor tekstowy
typeof(v3)
v4 <- c()
v5 <- 11:20
v1 + v5

# Rzutowanie typów
c(10, 20, TRUE)
c(10, 20, TRUE, 'ala ma kota')
# cały wektor zostaje skonwertowany do najogólniejszego typu

# indeksowanie:
v3
v3[3] # 3ci element
v1[2:4] # element od 2giego do 4tego
v1[c(2, 3, 10)] # element 2, 3 i 10ty
v1[v1 < 4 | v1 > 6]
v1[v1 > 4 & v1 < 8]

# a co, gdy wychodzimy poza rzeczywistą długość wektora?
v3
length(v3)
v3[100]

# indeks z minusem oznacza "wszystko oprócz tego"
v3[-1]

# Wektory tekstowe
vec_txt <- c('SGH', 'PW', 'UW', 'SGGW', 'SWPS', 'SGH')
length(vec_txt)
vec_txt_u <- unique(vec_txt) # unikatowe wartości
length(vec_txt_u)
vec_txt_u <- sort(vec_txt_u) # sort działa i na liczbach, i na tekście
vec_txt_u

vec_txt_u[1]
vec_txt_u[2:3]
vec_txt_u[c(1, 3, 4)]

nchar('SGH')
nchar(vec_txt_u) # ta funkcja też jest naturalnie zwektoryzowana

w <- c(4, 36, 6, 2, 6, 5, 6, 2, 4.6)
w
k <- sqrt(w) # ta funkcja też jest naturalnie zwektoryzowana
k

# Działania na wektorach, zawijanie wektorów
w1 <- c(1, 2, 3, 4)
w2 <- c(3, 12, 5.2, 7.8)
w3 <- c(10, 100)
w4 <- c(1000, 2000, 3000)

2 * w1
w1 / 2
w1^2

# Zawijanie wektorów
w1 + w2
w1 + w3
w1 + w4

#Kilka dodatkowych operacji
x <- 1:100
3 %in% x # czy 3 jest w zbiorze x?

c(3, 100, 2000) %in% x # które elementy tego wektora są w zbiorze x?

# ciagi
licznik <- 1:10
licznik

licznik2 <- 10:0
licznik2

seq(-1, 1, 0.1)
seq(from = -1, to = 1, length = 21)
seq(1, 1,length = 21)
seq(length = 21, from = -1, by = 0.1)

rep(1, 5)
rep('TORA', 3)

rep(1:2, times = 3)
rep(1:2, each = 3)
# ^ w praktyce lepiej pisać wprost, co chcemy osiągnąć, zamiast opierać kod na
# znajomości takich pamięciowych sztuczek.


## macierze
m1 <- matrix (1:100, 10, 10, byrow = T)
m1
m2 <- matrix (1:100, 10, 10)
m2

m3 <- matrix(seq(from = -1, to = 1, length = 20), 4, 5)
m3

rep(1, 5)
rep(1:2, times = 4)
m4 <- matrix(rep(1:3, times = 3), 3, 3)
m4

t(m1) #transpozycja
m1 == t(t(m1)) #transpozycja
all(m1 == t(t(m1))) #?

dim(m1) #wymiary
m1 %*% m2 #mnozenie macierzy

nrow(m1)
ncol(m1)
colSums(m1)

cbind(m1, m2)
rbind(m1, m2)

m1 == m2
any(m1 == m2)
all(m1 == m2)

# indeksowanie macierzy:
m1[2, 3]
m1[1, ]
m1[, 3]
m1[m1 > mean(m1)]

## lista-  "wektory", ale mogą przechowywać elementy dowolnych typów,
# także bardzo rozbudowanych (np. inne listy), elementy mogą być różnej długości
lista <- list(a = 1, b = "a", c = 1:4, d = list(), 6)
lista
names(lista) # nazwy elementów listy
class(lista)

# listę indeksuje się podobnie jak wektory, ale:
lista[3] # zwraca listę jednoelementową
class(lista[3]) # mamy dalej listę
lista[[3]]         # zwraca element listy
class(lista[[3]])
lista[[3]][1]      # pierwszy element trzeciego elementu listy

# można się odwoływać po nazwach
lista[["c"]]
# lub użyć operatora '$'
lista$c

# podwójny && porównuje pierwsze elementy, a pojedynczy poszczególne wartości
# & - i ,| - lub
c(TRUE, TRUE) &  c(FALSE, TRUE)
c(TRUE, TRUE) &&  c(TRUE, FALSE)
c(TRUE, TRUE) &&  c(FALSE, FALSE)


# Lepiej nie używać tego typu sztuczek w poważnym kodzie - tak jest bezpieczniej:
all(c(TRUE, FALSE) & c(TRUE, FALSE))
all(c(TRUE, FALSE) | c(TRUE, TRUE))

## typ czynnikowy
kolor.oczu <- c('n', 'n', 'z', 'b', 'b', 'b', 'n', 'z', 'b', 'z')
kolor.oczu
kolor.oczu <- factor(kolor.oczu)
kolor.oczu
levels(kolor.oczu)
levels(kolor.oczu) <- c("brazowe", "niebieskie", "zielone")
levels(kolor.oczu)
kolor.oczu

plec <- c('f', 'm', 'f', 'f', 'm', 'm', 'f',
          'm', 'f', 'f')
plec

plec <- as.factor(plec)
plec

plec2 <- factor(c('f', 'f', 'f', 'f', 'f'),
                levels=c('f', 'm'))
plec2


# Tabele częstości
table(plec)

table(plec2)[2]

length(plec)

wiek<-c('>18', '<18', '<18', '<18', '<18', '>18', '>18', '>18', '>18', '>18')
wiek
wiek <- factor(c('>18', '<18', '<18', '<18', '<18', '>18', '>18', '>18', '>18', '>18'))
wiek

t <- table(plec, wiek)
t

# Warianty tabel częstości
prop.table(t)

margin.table(t, 1)

margin.table(t, 2)
prop.table(t, 1)
prop.table(t, 2)

## ramka danych (data frame) - wektory różnych typów o RÓWNEJ liczbie elementów:
df1 <- data.frame(v1 = c(10, 20, 30), 
                  v2 = c("ala", "ma", "kota"), 
                  v3 = c(NA, 13, NaN), 
                  v4 = c(TRUE, FALSE, TRUE))
df1
names(df1) <- c('wiek', 'haslo', 'wiek_brata', 'ma_siostre')
df1

class(df1)

## ZADANIE 1
# Stwórz wektor o nazwie indeks, który będzie się składał z elementów 
# będącymi poszczególnymi cyframi z numeru Twojego indeksu. 
# Sprawdź, jakiego jest typu. 
# Zamień trzeci element na liczbę 100
# Podnieś 1 i 2 element do kwadratu
# Sprawdź, które elementy są większe od 3 i mniejsze lub równe 8
# Posortuj malejąco 
# Stwórz wektor indeks2, który będzie dwukrotnością wektora indeks
# Dodaj elementy wektora indeks2, podziel przez 7.3 i zaokrąglij wynik do 2 miejsc po przecinku


## ZADANIE 2
# Stwórz ramkę danych "dane" z następującymi rekordami:
# • imię
# • drugie imię, jeżeli nie ma, to NA
# • wiek
# • płeć - jako czynnik (factor)
# • informację o tym, czy osoba ma status studenta (tak lub nie)
# zawierającymi dane Twoje i trzech innych osób (mogą być zmyślone)

########################################################################################
# Odpowiedzi są w osobnym pliku
########################################################################################
