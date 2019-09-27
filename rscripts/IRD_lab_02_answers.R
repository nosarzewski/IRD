# Cwiczenie 1 - Dokonaj operacji na danych o rybkach zaladowanych do pamieci
#install.packages("FSAdata")
library(FSAdata)

# Wczytujemy do pamieci zbior danych zawarty w pakiecie
data(RuffeSLRH92)

# Eksplorujemy strukture zbioru danych
str(RuffeSLRH92)
summary(RuffeSLRH92)

# Podpunkt a)
# Wykonaj ponizsze transformacje, za kazdym razem zapisujac posredni wynik do nowej zmiennej

library("tidyverse")

# ze zbioru danych wybierz kolumny dotyczace miesiecy, plci, dlugosci i wagi rybek, zapisz wynik jako lw

lw <- select(RuffeSLRH92, month, sex, length, weight)

# posortuj dane wedlug miesiecy i plci rybek, zapisz wynik jako ms

ms <- arrange(lw, month, sex)

# do zbioru LW dodaj nowa kolumne bedaca logarytmem dlugosci rybek, nazwij ja log_l, wynikowa tabele zapisz jako log_lw

log_lw <- mutate(lw,
                 log_l = log(length))

# wskaz liczbe rybek dla kazdego miesiaca i plci, zapisz wynik jako sum_mon_sex

by_mon_sex <- group_by(lw, month, sex)
sum_mon_sex <- summarise(by_mon_sex,
                   count = n())

# pogrupuj rybki wg plci i wybierz te grupy, ktorych srednia waga jest wieksza niz 15

by_sex <- group_by(lw, sex)
mean_weight_by_sex <- summarise(by_sex,
                                mean_weight = mean(weight, na.rm = TRUE))
mean_weight_by_sex_above_15 <- filter(mean_weight_by_sex, mean_weight > 15)

# Podpunkt b)
# Wykonaj ponizsze transformacje, laczac je za pomoca operatora pipeline %>% z pakietu dplyr

# ze zbioru danych wybierz kolumny dotyczace miesiecy, plci, dlugosci i wagi rybek
# posortuj dane wedlug miesiecy i plci rybek
# dodaj nowa kolumne bedaca logarytmem dlugosci rybek, nazwij ja log_l
# wskaz liczbe rybek dla kazdego miesiaca i plci, zapisz wynik jako sum_mon_sex

sum_mon_sex <- RuffeSLRH92 %>%
  select(., month, sex, length, weight) %>%
  arrange(., month, sex) %>%
  mutate(., log_l = log(length)) %>%
  group_by(., month, sex) %>%
  summarise(., count = n())
