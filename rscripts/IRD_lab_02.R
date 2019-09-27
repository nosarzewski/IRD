################################################################################
### IRD
### Lab 02
### Składnia języka R i przetwarzanie danych w R
################################################################################

### 1) Instrukcje warunkowe, petle, funkcje

## Instrukcje warunkowe

# Instrukcja przypisania w nawiasie oznacza: wykonaj przypisanie, ale tez i wydrukuj wynik do konsoli

# jeżeli spełniony warunek to wykonaj komendę
(zmienna <- rnorm(1))
if (zmienna < 0){
  cat("mniejsza ")
}

# jeżeli spełniony warunek to wykonaj komendę, w przeciwnym wypadku zrób co innego
(zmienna <- rnorm(1))
if (zmienna < 0){
  cat("mniejsza \n")
}else cat("wieksza\n")

# warunek musi być pojedynczą wartością
if (c(-1,0,1) > 0){
  cat("wieksze\n")
} #error

# ale można podawać rozbudowane warunki
(zmienna <- rnorm(1))
if (zmienna < 0 || zmienna^2 > 0.5){
  cat("OK\n") 
}else{
  cat("Nie OK\n")
}

# ifelse - wersja wektorowa funkcji IF
(zmienna <- rnorm(5))
ifelse(zmienna < 0, "mniejsza", "wieksza")
d <- ifelse(zmienna < 0, "mniejsza", "wieksza")
d

x <- 1:5
y <- -(1:5)
ifelse(zmienna < 0, y, x)

## Pętle

for(i in 1:5) {
  cat("aktualna wartosc i to", i, "\n")
}

(macierz <- matrix(1:20, 5, 4))
for(i in 1:nrow(macierz))
{
  print(mean(macierz[i,]))
}


# Petla while
licznik <- 1
while(licznik < 5)
{
  licznik <- licznik + runif(1, min = 0, max = 1)
  cat(licznik, '\n')
}


## Funkcje

# składnia:
# NazwaFunkcji <- function(argument1, argument 2) {
#   instrukcje
# (opcjonalnie) return(wynik)
# }

# PRZYKLADY:
MojaFunkcja <- function(a,b) {
  a^2 + b^2
}
#wywołanie:
MojaFunkcja(1,2)
MojaFunkcja(124,445)

MojaFunkcja2 <- function(x, y) {
  a <- sin(x)
  b <- cos(y)
  (a+b)^2
}

MojaFunkcja2(1, pi)

# funkcja domyślnie zwraca ostatnie obliczane wyrażenie,
# ale można zadeklarować, co ma zwrócić - instrukcja return
MojaFunkcja3 <- function(x, y) {
  a <- sin(x)
  b <- cos(y)
  wynik <- a*b*100
  print("ala ma kota")
  return(c(a, b*100, wynik))
}

MojaFunkcja3(1, 2)

# po instrukcji RETURN nic nie jest już wykonywane:
MojaFunkcja4 <- function(x, y) {
  a <- sin(x)
  b <- cos(y)
  wynik <- a*b*100
  return(c(a, b*100, wynik))
  print("ala ma kota")
}

MojaFunkcja4(1, 2)

MojaFunkcja5 <- function(x,y)
{
  paste(x,y)
}

MojaFunkcja5("ala", "ma")
MojaFunkcja5(MojaFunkcja5("ala", "ma"), "kota")

# ... - arbitralna liczba argumentow. Uzywane glownie, gdy funkcja bierze jako jeden z argumentow
# inna funkcje i musi jej te argumenty przekazac.

measure_time <- function(f, ...)
{
  start_time <- Sys.time()
  r <- f(...)
  end_time <- Sys.time()
  run_time <- end_time - start_time
  return(list(result = r, run_time = run_time))
}

(mt <- measure_time(MojaFunkcja5, 'ala', 'ma'))

mt$result
mt$run_time

### Stosowanie funkcji na wektorach, listach i macierzach - rodzina funkcji apply
# Tutorial (po angielsku): https://www.datacamp.com/community/tutorials/r-tutorial-apply-family

# apply
n <- 5
m <- 10
mx <- matrix(data = round(100*runif(n*m, 0, 1), 0), nrow = n, ncol = m)

apply(mx, 1, sum)
apply(mx, 2, sum)

# lapply

example_list <- list(c(0, 1, 2, 3), rep('p', 5), seq(2, 20, 3), runif(20))
length(example_list)

lapply(example_list, length)
lapply(example_list, function(element){
  if (length(element) > 5) 'Wiecej niz 5 elementow' else '5 elementow lub mniej'
})

### 2) Wczytywanie i przetwarzanie danych

##wczytanie i wstepne przetwarzanie danych
#pakiet readr i dplyr

###readr - pakiet sluzacy wygodnemu wczytywaniu danych z formatu csv, tsv lub fwf
#jest nawet do 10 razy szybszy od standardowej funkcji read.csv
#w przeciwienstwie do standardowej funkcji, nie zmienia wektorow o typie character na factor
#tworza obiekty typu tibble czyli proste ramki danych

#instalacja pakietu
#install.packages("tidyverse", dependencies=TRUE)

#zaladowanie pakietu
library("tidyverse")

# Budujemy pomocniczą funkcję do mierzenia czasu wykonywania różnych operacji
measure_time <- function(f, ...)
{
  start_time <- Sys.time()
  r <- f(...)
  end_time <- Sys.time()
  print(end_time - start_time)
  return(r)
}

getwd()

#ustalenie katalogu roboczego (tam, gdzie znajduja sie pliki z danymi do przykladow)
setwd("github/data/")
path <- "https://raw.githubusercontent.com/nosarzewski/IRD_18_19_L/master/data/"

##wczytywanie roznego rodzaju danych z pliku

#wartosci rozdzielone przecinkami
titanic <- measure_time(read.csv, paste0(path, "Titanic.csv"))
titanic <- measure_time(read_csv, paste0(path, "Titanic.csv"))

#jesli nie dziala wariant z podawaniem sciezki nalezy sciagnac dane, zapisac w working dir i wczytac recznie jak ponizej
#titanic <- measure_time(read.csv, "Titanic.csv")
#titanic <- measure_time(read_csv, "Titanic.csv")

# Dygresja: data.table potrafi być jeszcze szybszy
library(data.table)
titanic <- measure_time(fread, paste0(path, "Titanic.csv"))
#titanic <- measure_time(fread, "Titanic.csv")

przecinkowe <- read_csv(paste0(path, "Titanic.csv"))
#przecinkowe <- read_csv("Titanic.csv")
head(przecinkowe)

#wartosci rozdzielone srednikami
srednikowe <- read_csv2(paste0(path, "cars.csv"))
#srednikowe <- read_csv2("cars.csv")
head(srednikowe)

#wartosci rozdzielone stala szerokoscia
#fwf_widths sluzy podaniu szerokosci kolumn i ich nazw
stale <- read_fwf(file = paste0(path, "iris.txt"), 
                  fwf_widths(c(3, 5, 3, 5, 6), 
                             c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")))
#stale <- read_fwf(file="iris.txt", fwf_widths(c(3, 5, 3, 5, 6), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")))
head(stale)

#wartosci rozdzielone spacjami
spacjowe <- read_table(paste0(path, "heights.txt"), 
                       col_names=c("height", "cubit"))
#spacjowe <- read_table("heights.txt", col_names=c("height", "cubit"))
head(spacjowe)

##wczytanie danych z ciagu liczb - pierwsza linijka zawsze sluzy jako nazwy kolumn
ciag <- read_csv("a,b,c
                 1,2,3
                 A,B,C")
ciag

#pozbywanie sie metadanych za pomoca skip lub "#"
read_csv("Pierwsza linia metadanych
         Druga linia metadanych
         x,y,z
         1,2,3", skip=2)

read_csv("#Komentarz
         x,y,z
         1,2,3", comment="#")

#wczytanie danych bez nazw kolumn
esoph <- read_csv(paste0(path, "esoph.csv"), col_names=FALSE)
#esoph <- read_csv("esoph.csv", col_names=FALSE)
head(esoph)

#nadanie wlasnych nazw dla kolumn
esoph <- read_csv(paste0(path, "esoph.csv"), col_names=c("jeden", "2", "trzy", "4", "piec"))
esoph <- read_csv("esoph.csv", col_names=c("jeden", "2", "trzy", "4", "piec"))
head(esoph)

#konwersja pustych lub konkretnych wartosci na NA (w tym przypadku kropki)
#\n sluzy jako przejscie do nastepnej linijki
read_csv("a,b,c\n1,2,.", na=".")


###pakiet dplyr - umozliwia manipulacje danych zarowno zapisanych w formie ramek danych, 
# jak i przechowywanych w bazach danych. 
# Jest w stanie przetlumaczyc kod R na zapytanie SQL (ale to poza zakresem naszych zajęć)

# http://seananderson.ca/2014/09/13/dplyr-intro.html

# dplyr is built around 5 verbs. 
# These verbs make up the majority of the data manipulation you tend to do. 
# You might need to:

# Select certain columns of data.
# Filter your data to select specific rows.
# Arrange the rows of your data into an order.
# Mutate your data frame to contain new columns.
# Summarise chunks of you data in some way.

#instalacja pakietu
#install.packages("dplyr")
library("dplyr")

#wczytanie przykladowych danych
#install.packages("nycflights13")
library("nycflights13")
dim(flights)
head(flights)

##operacje na danych

#filtrowanie
filter(flights, month==2, day==22)
#rownoznaczne z: 
flights[flights$month == 2 & flights$day == 22, ]

filter(flights, month == 1 | month == 2)
filter(flights, month %in% c(1,2))
#wycinek danych
slice(flights, 1:5)

#sortowanie danych rosnaco wg kolejnych kolumn
arrange(flights, year, month, day)

#sortowanie danych malejaco
arrange(flights, desc(dep_delay))

#wybor konkretnych kolumn 
select(flights, year, month, day)
select(flights, year:day) #przedzial kolumn
select(flights, -(year:day)) #wszystkie poza przedzialem kolumn

#zmiana nazwy kolumny
rename(flights, tail_num = tailnum)
select(flights, tailnum)

#wybor niepowtarzalnych wartosci
distinct(flights, origin, dest)

#dodawanie nowej kolumny
f2 <- mutate(flights,
             gain = arr_delay - dep_delay,
             speed = distance / air_time * 60)

View(f2)

#mozna odniesc sie do wlasnie tworzonej kolumny
mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60)
)

#zachowanie tylko nowych kolumn
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
)

#podsumowanie zbioru
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE))

#losowe obserwacje
sample_n(flights, 10) #10 wierszy
sample_frac(flights, 0.01) #10% wierszy

##zgrupowane operacje na danych

#funkcja group_by() pozwala na grupowanie obserwacji w zbiorze
#wykorzystane po niej inne funkcje beda wykonywane dla kazdej grupy osobno

#select() dziala identycznie, ale grupujace zmienne sa zachowywane
a1 <- group_by(flights, year, month, day)
select(a1, day)

distinct(select(a1, day))

#arrange() porzadkuje po zgrupowanych wartosciach
arrange(a1)

#mutate() i filter() dzialaja podobnie
filter(a1, day > 30 & dep_time < 10)

#sample_n() i sample_frac() losuja liczbe lub czesc wierszy dla kazdej grupy
sample_n(a1, 5)
sample_frac(a1, 0.01)

#slice() wybiera wiersze w kazdej z grup
slice(a1,1:2)

#summarise() jest latwiejszy do zrozumienia i uzycia
#przyklad - dane pogrupowane wg nr samolotow, podsumowane poprzez liczbe lotow, srednia odleglosc
#i opoznienie lotu kazdego z samolotow
by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),  #liczba lotow
                   dist = mean(distance, na.rm = TRUE),  #sredni dystans
                   delay = mean(arr_delay, na.rm = TRUE))  #srednie opoznienie
delay <- filter(delay, count > 20, dist < 2000)
delay


#funkcje agregacyjne
#liczba obserwacji w danej grupie
destinations <- group_by(flights, dest)
summarise(destinations,
          flights = n()
)

#liczba unikatowych wartosci w zbiorze
summarise(destinations,
          planes = n_distinct(tailnum)
)


#kazde kolejne summarise zabiera jedna "warstwe" zgrupowanego zbioru
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))

(per_month <- summarise(per_day, flights = sum(flights)))

(per_year  <- summarise(per_month, flights = sum(flights)))


#lancuchy
#wykonywanie kazdej operacji po kolei z zapisywaniem krokow w kolejnych obiektach jest dosyc uciazliwe
#przyklad:
a1 <- group_by(flights, year, month, day)
a2 <- select(a1, arr_delay, dep_delay)
a3 <- summarise(a2,
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)
a4


#bez zapisywania kolejnych krokow mozna tworzyc funkcje zagniezdzone
a4_nested <- filter(
  summarise(
    select(
      group_by(flights, year, month, day),
      arr_delay, dep_delay
    ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)

all(a4 == a4_nested)

# jednak jest to trudne do czytania, dlatego w dplyr istnieje operator %>%,
# ktory wynik poprzedniej operacji przerzuca jako argument do następnej.
# Mozna dzieki temu zapisac wiele operacji, ktore nalezy czytac od lewej do prawej, od gory do dolu
a4_pipeline <- flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)

all(a4_pipeline == a4)



# Cwiczenie 1 - Dokonaj operacji na danych o rybkach zaladowanych do pamieci
install.packages("FSAdata")
library(FSAdata)

# Wczytujemy do pamieci zbior danych zawarty w pakiecie
data(RuffeSLRH92)

# Eksplorujemy strukture zbioru danych
str(RuffeSLRH92)
summary(RuffeSLRH92)

# Podpunkt a)
# Wykonaj ponizsze transformacje, za kazdym razem zapisujac posredni wynik do nowej zmiennej

# ze zbioru danych wybierz kolumny dotyczace miesiecy, plci, dlugosci i wagi rybek, zapisz wynik jako lw
# posortuj dane wedlug miesiecy i plci rybek, zapisz wynik jako ms
# do zbioru LW dodaj nowa kolumne bedaca logarytmem dlugosci rybek, nazwij ja log_l, wynikowa tabele zapisz jako log_lw
# wskaz liczbe rybek dla kazdego miesiaca i plci, zapisz wynik jako sum_mon_sex
# pogrupuj rybki wg plci i wybierz te grupy, ktorych srednia waga jest wieksza niz 15

# Podpunkt b)
# Wykonaj ponizsze transformacje, laczac je za pomoca operatora pipeline %>% z pakietu dplyr

# ze zbioru danych wybierz kolumny dotyczace miesiecy, plci, dlugosci i wagi rybek
# posortuj dane wedlug miesiecy i plci rybek
# dodaj nowa kolumne bedaca logarytmem dlugosci rybek, nazwij ja log_l
# wskaz liczbe rybek dla kazdego miesiaca i plci, zapisz wynik jako sum_mon_sex
