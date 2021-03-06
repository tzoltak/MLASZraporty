% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wykresy_slupkowe.R
\name{wykres_slupkowy_binarny}
\alias{wykres_slupkowy_binarny}
\title{Wykres slupkowy wskaznika bedacego srednia zmiennej binarnej.}
\usage{
wykres_slupkowy_binarny(tabela, zmienna, facet = NULL, maxY = 100,
  etykietaY = "", usunZEtykietX = "")
}
\arguments{
\item{tabela}{ramka danych zawierająca zestawienie wartości wskaźników}

\item{zmienna}{wyrażenie lub ciąg znaków - nazwa kolumny w przekazywanej
ramce danych, w której podane są etykiety wartości wskaźników, których
rozkłady mają zostać ze sobą porównane}

\item{facet}{opcjonalnie wyrażenie lub ciąg znaków - nazwa kolumny
w przekazywanej ramce danych, na podstawie wartości której dane zostaną
podzielone pomiędzy \emph{segmenty} wykresu}

\item{maxY}{opcjonalnie liczba - wartość maksymalna osi Y wykresu}

\item{etykietaY}{opcjonalnie ciąg znaków - etykieta osi Y wykresu}

\item{usunZEtykietX}{opcjonalnie ciąg znaków - wyrażenie regularne opisujące
ciąg znaków, który ma być usunięty z etykiet wartości osi X wykresu (zwykle
usuwanym cięgiem znaków jest ten, który zostaje podany jako wartość parametru
\code{etykietaY})}
}
\value{
wykres ggplot2
}
\description{
Funkcja zwraca wykres słupkowy ilustrujący wartości wskaźników
będących średnimi zmiennych (wskaźników z poziomu indywidualnego) binarnych.
}
