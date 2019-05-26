#' @title Drukowanie informacji o liczbie absolwentow, na podsatwie ktorych
#' obliczono wartosci wskaznika.
#' @description Funkcja przygotowuje komunikat o tym, jaka liczba absolwentów
#' została uwzględniona przy obliczaniu wartości wskaźnika lub komunikat o tym,
#' że wskaźnik nie może zostać zaprezentowany ze względu na zbyt małą liczbę
#' badanych.
#' @param wskaznik obiekt (lista) z wartościami wskaźnika w danej grupie
#' @param progLiczebnosci liczba - próg liczebności, poniżej którego powinien
#' zostać zwrócony komunikat o niemożliwości zaprezentowania wskaźnika
#' @param opis opcjonalnie ciąg znaków opisujący grupę absolwentów, dla której
#' obliczony został wskaźnik (np. ", którzy mieli pracę w rok po ukończeniu
#' szkoły")
#' @return ciąg znaków
#' @export
komunikat_o_liczebnosci = function(wskaznik, progLiczebnosci, opis = "") {
  stopifnot(is.list(wskaznik),
            is.numeric(progLiczebnosci), length(progLiczebnosci) == 1,
            is.character(opis), length(opis) == 1)
  stopifnot("n" %in% names(wskaznik))
  stopifnot(is.numeric(wskaznik$n), length(wskaznik$n) == 1)
  n = wskaznik$n
  if (n >= progLiczebnosci) {
    paste0("W analizie uwzględniono ", n, " absolwentów", opis, ".") %>%
      return()
  } else {
    paste0("Zbyt mała liczba obserwacji (", n, " z wymaganych ",
           progLiczebnosci, "), aby móc zaprezentować wskaźniki.") %>%
      return()
  }
}
