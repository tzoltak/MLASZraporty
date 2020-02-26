#' @title Funkcje nieeksportowane.
#' @description Funkcja przekształca ramkę danych zawierającą zestawienie
#' wartości wskaźników, które mają być zilustrowane wykresem pudełkowym do
#' postaci, w której może być ona użyta jako argument funkcji
#' \code{\link[ggplot2]{ggplot}}.
#' @param x ramka danych w postaci \emph{szerokiej}
#' @return ramka danych w postaci \emph{długiej}
#' @details Funkcja przekształca ramkę danych na formę \emph{długą} i stara się
#' zamienić opisowe etykiety parametrów rozkładu (kwartyli, mediany, śreniej) na
#' nazwy samych parametrów.
#' @importFrom tibble is_tibble
#' @importFrom tidyr gather spread
#' @importFrom dplyr %>% .data case_when mutate one_of
do_wykresu_pudelkowego = function(x) {
  stopifnot(is_tibble(x) | is.data.frame(x))
  stopifnot("parametr" %in% names(x))
  stopifnot(any(grepl("25% osób", x$parametr)) | "1.kwartyl" %in% x$parametr,
            any(grepl("(połowa|50%) osób", x$parametr)) | "mediana" %in% x$parametr,
            any(grepl("75% osób", x$parametr)) | "3.kwartyl" %in% x$parametr,
            any(grepl("średni", x$parametr)) | "średnia" %in% x$parametr)

  domyslneKlucze = c("w tym powiecie",
                     "w grupie\nporównawczej")
  x %>%
    gather("klucz", "wartosc", -one_of("parametr")) %>%
    mutate(klucz = sub("(w grupie porównawczej|w gr[.] por[.])",
                       "w grupie\nporównawczej", .data$klucz),
           klucz = factor(.data$klucz, levels = c(domyslneKlucze,
                                                  setdiff(unique(.data$klucz),
                                                          domyslneKlucze))),
           parametr = case_when(grepl("25% osób", .data$parametr) ~ "1.kwartyl",
                                grepl("(połowa|50%) osób", .data$parametr) ~ "mediana",
                                grepl("75% osób", .data$parametr) ~ "3.kwartyl",
                                grepl("średni", .data$parametr) ~ "średnia",
                                TRUE ~ .data$parametr)) %>%
    spread("parametr", "wartosc") %>%
    return()
}
