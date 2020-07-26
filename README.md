---
output:
  html_document: default
  pdf_document: default
---
![KL+RP+IBE+EFS](inst/Belka-Losy-absolwentow-Kolor-PL.png)

[![Travis build status](https://travis-ci.org/tzoltak/MLASZraporty.svg?branch=master)](https://travis-ci.org/tzoltak/MLASZraporty)
[![Coverage status](https://codecov.io/gh/tzoltak/MLASZraporty/branch/master/graph/badge.svg)](https://codecov.io/github/tzoltak/MLASZraporty?branch=master)

# MLASZraporty

Pakiet został opracowany w ramach projektu *Monitorowanie losów edukacyjno-zawodowych absolwentów i młodych dorosłych* (POWR.02.15.00-IP.02-00-004/16) prowadzonego w Instytucie Badań Edukacyjnych w ramach działania 2.15. Kształcenie i szkolenie zawodowe dostosowane do potrzeb zmieniającej się gospodarki II osi priorytetowej Efektywne polityki publiczne dla rynku pracy, gospodarki i edukacji Programu Operacyjnego Wiedza, Edukacja, Rozwój

Pakiet służy do przygotowywania raportów szkół, opisujących dalsze losy zawodowe i edukacyjne ich absolwentów na podstawie zbiorów danych przygotowanych przy pomocy pakietu *MLASZdane*.

# Instalacja / aktualizacja

Pakiet nie jest wypchnięty na CRAN, więc trzeba instalować go ze źródeł.

Ponieważ jednak zawiera jedynie kod w R, nie ma potrzeby zaopatrywać się w kompilatory, itp.

Instalację najprościej przeprowadzić wykorzystując pakiet *devtools*:

```r
install.packages('devtools') # potrzebne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('tzoltak/MLASZraporty')
```

Dokładnie w ten sam sposób można przeprowadzić aktualizację pakietu do najnowszej wersji.

## Zależności

Do poprawnego działania pakiet MLASZraporty potrzebuje, aby w systmie zainstalowany był program [Pandoc](http://pandoc.org). Jeśli na komputerze zainstalowane zostało [RStudio](http://www.rstudio.com/products/rstudio/download), Pandoc najprawdopodobniej został zainstalowany razem z nim i jest gotowy do użycia (w innym przypadku p. [wiki pakietu *MLAK*](https://github.com/zozlak/MLAK/wiki/1.1-Instalacja)).

Aby możliwe było generowanie raportów w formacie PDF niezbędna jest również obecność w systemie dystrybucji LaTeX-a (p. [wiki pakietu *MLAK*](https://github.com/zozlak/MLAK/wiki/1.1-Instalacja)).

# Użycie

Do generowania raportów służy funkcja `generuj_raporty()`. Jej typowe wywołanie wygląda następująco:

```r
library(MLASZraporty)
generuj_raporty(szablon = 'raport_szkoly_1rm.Rmd',
                wskazniki = wskaznikiSzk,
                wskaznikiGrPor = wskaznikiSzkGrPor,
                kolumnaNazwaPliku = SZK_kod,
                parametry = list(typDokumentu = "pdf",
                                 progLiczebnosci = 10,
                                 rocznik = 2017,
                                 wyrownanieTabWykr = "center"))
```

Poszczególne argumenty opisują:

  - `szablon` - plik szablonu raportu, który ma zostać wykorzystany (p. sekcja *Dostępne szablony raportów* poniżej),
  - `wskazniki` - obiekt (ramka danych) zawierająca wartości wskaźników: wiersze reprezentują grupy, dla których mają zostać wygenerowane raporty (np. szkoły), kolumny zawierają poszczególne wskaźniki;
    - w wywołaniu powyżej wykorzystywany jest obiekt `wskaznikiSzk`, stanowiący część pakietu *MLASZraporty*, który zawiera przykładowe dane kompatybilne z szablonem 'raport_szkoly_1rm.Rmd';
    - w praktycznych zastosowaniach zwykle wykorzystywany będzie obiekt wskaźników na poziomie zagregowanym (np. szkół) przygotowany przy pomocy pakietu [*MLASZdane*](https://github.com/tzoltak/MLASZdane);
  - `wskaznikiGrPor` - obiekt (ramka danych) zawierająca wartości wskaźników w grupach porównawczych: wiersze reprezentują grupy porównawcze, kolumny zawierają poszczególne wskaźniki; zwykle struktura tego obiektu jest niemal identyczna (z dokładnością do tego, co reprezentują wiersze) do obiektu przekazywanego argumentem `wskazniki`;
    - sposób wyboru odpowiedniej grupy porównawczej do wykorzystania w konkretnym raporcie jest zakodowany w pliku z szablonem raportu;
    - w wywołaniu powyżej wykorzystywany jest obiekt `wskaznikiSzkGrPor`, stanowiący część pakietu *MLASZraporty*, który zawiera przykładowe dane kompatybilne z szablonem 'raport_szkoly_1rm.Rmd';
    - w praktycznych zastosowaniach zwykle wykorzystywany będzie obiekt wskaźników na poziomie zagregowanym (np. typów szkół) przygotowany przy pomocy pakietu [*MLASZdane*](https://github.com/tzoltak/MLASZdane);
  - `kolumnaNazwaPliku` - nazwa kolumny w obiekcie zawierającym wartości wskaźników, która zostanie wykorzystana do nadania nazw plikom raportów;
    - tego argumentu można nie podawać - pliki raportów będę wtedy mieć nazwy *raportNR*, gdzie *NR* to numer wiersza w ramce danych przekazanej argumentem `wskazniki`;
  - `parametry` - lista dodatkowych parametrów, niezbędnych do wygenerowania raportów na podstawie szablonu; może być specyficzna dla szablonu; najczęściej występujące parametry, które trzeba podać to:
    - `typDokumentu` - "pdf" lub "html";
    - `progLiczebnosci` - próg liczby badanych, poniżej której wartości wskaźnika nie zostaną pokazane w raporcie (zamiast tego wygenerowana zostanie informacja o zbyt małej liczbie absolwentów);
    - `rocznik` - rok ukończenia szkoły przez absolwentów;
    - `wyrownanieTabWykr` - wyrównanie (w poziomie) tabel i wykresów: "left", "right" lub "center";

Opisane powyżej dodatkowe parametry - z wyjątkiem `typDokumentu` - mogą być też przekazane jako kolumny ramki danych ze wskaźnikami grup (tj. ramki danych przekazywanej argumentem `wskazniki`) - wtedy nie muszą być już wpisywane jako elementy argumentu `parametry`.

Raporty zostaną utworzone w aktywnym folderze. Jeśli nie jesteś pewien, jaki to folder, użyj funkcji `getwd()` i ew. funkcji `setwd()`, aby go zmienić.

## Dostępne szablony raportów

W obecnej wersji pakietu dostępny jest tylko jeden szablon raportu, przeznaczony do generowania raportów na poziomie szkół. Z odpowiednio przygotowanymi danymi wejściowymi możliwe jest też wykorzystanie go do generowania raportów na niższym poziomie agregacji: zestawu zawodów w ramach szkoły.

Aby sprawdzenić, jakie szablony są dostępne w ramach pakietu można też wywołać funkcję:

```r
wypisz_dostepne_szablony()
```

# Możliwości dalszego rozwoju

 - Automatyczne dokumentowanie wymagań szablonów raportu w odniesieniu do struktury danych (zestawu wskaźników).
 - Weryfikacja struktury przekazanych danych względem wymagań szablonu raportu.
 - Winietka nt. tworzenia nowych szablonów raportów.
