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

indeks <- c(2, 5, 3, 4, 9, 2, 7)
typeof(indeks)
indeks[3] <- 100
indeks[1:2] <- indeks[1:2]^2
indeks[indeks > 3 & indeks <= 8]
indeks <- sort(indeks, decreasing = TRUE)
indeks2 <- 2 * indeks
round(sum(indeks2) / 7.3, 2)

## ZADANIE 2
# Stwórz ramkę danych "dane" z następującymi rekordami:
# • imię
# • drugie imię, jeżeli nie ma, to NA
# • wiek
# • płeć - jako czynnik (factor)
# • informację o tym, czy osoba ma status studenta (tak lub nie)
# zawierającymi dane Twoje i trzech innych osób (mogą być zmyślone)

dane <- data.frame(imie = c('Jan', 'Adam', 'Ewa', 'Justyna'),
                   drugie_imie = c('Chryzostom', 'Jan', NA, 'Maria'),
                   wiek = c(24, 25, 23, 31),
                   plec = factor(c('m', 'm', 'f', 'f')),
                   status_studenta = c(TRUE, FALSE, TRUE, FALSE))

# Check:
class(dane$plec) == 'factor' # TRUE