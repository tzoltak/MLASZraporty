% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generuj_raporty.R
\name{generuj_raporty}
\alias{generuj_raporty}
\title{Drukowanie raportow z zestawieniami wskaznikow.}
\usage{
generuj_raporty(szablon, wskazniki, wskaznikiGrPor = NULL,
  kolumnaNazwaPliku = NULL, parametry = list())
}
\arguments{
\item{szablon}{ciąg znaków - nazwa szablonu raportu (por.
\code{\link{wypisz_dostepne_szablony}})}

\item{wskazniki}{ramka danych ze wskaznikami grup, dla których mają zostać
utworzone raporty}

\item{wskaznikiGrPor}{opcjonalnie ramka danych ze wskaźnikami grup
porównawczych, do wykorzystania przy tworzeniu raportów}

\item{kolumnaNazwaPliku}{opcjonalnie wyrażenie lub ciąg znaków - kolumna
ramki danych przekazanej argumentem \code{wskazniki}, zawierająca nazwy
plików, do których mają zostać wydrukowane raporty}

\item{parametry}{lista parametrów przekazywanych do szablonu}
}
\value{
wektor tekstowy z nazwami utworzonych raportów (niewidocznie)
}
\description{
Funkcja generuje raporty na podstawie zestawienia zawierającego
wartości wskaźników dla poszczególnych grup (każda grupa, dla której ma
zostać wygenerowany raport opisywana jest w innym wierszu ramki danych
przekazywanej argumentem \code{wskazniki}).
}
