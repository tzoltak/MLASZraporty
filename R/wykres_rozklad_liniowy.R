#' @title Wykres liniowy wskaznika.
#' @description Funkcja zwraca wykres liniowy ilustrujący rozkład wartości
#' wskaźników.
#' @param tabela ramka danych zawierająca zestawienie wartości wskaźników
#' @param facet opcjonalnie wyrażenie lub ciąg znaków - nazwa kolumny
#' w przekazywanej ramce danych, na podstawie wartości której dane zostaną
#' podzielone pomiędzy \emph{segmenty} wykresu
#' @param etykietaX opcjonalnie ciąg znaków - etykieta osi X wykresu
#' @param etykietaY opcjonalnie ciąg znaków - etykieta osi Y wykresu
#' @return wykres ggplot2
#' @details Funkcja zakłada, że przekazywana tabela zawiera kolumnę o nazwie
#' \code{etykieta} lub \code{wskaznik}, która opisuje etykiety wartosci
#' ilustrowanych rozkładów (tj. wartości, które wystąpią na osi X wykresu).
#' @export
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>% ensym sym rename vars
#' @importFrom ggplot2 ggplot aes geom_line scale_y_continuous scale_x_discrete scale_color_viridis_d theme element_text facet_wrap
wykres_rozklad_liniowy = function(tabela, facet = NULL, etykietaX = NULL,
                                  etykietaY = "częstość") {
  stopifnot(is_tibble(tabela) | is.data.frame(tabela),
            is.null(etykietaX) | is.character(etykietaX),
            is.character(etykietaY), length(etykietaY) == 1)
  stopifnot("etykieta" %in% names(tabela) | "wskaznik" %in% names(tabela),
            length(etykietaX) == 1)
  names(tabela) = sub("^etykieta$", "wskaznik", names(tabela))
  czyFacet = tryCatch(!is.null(facet), error = function(e) {return(TRUE)})
  if (czyFacet) {
    facet = ensym(facet)
    stopifnot(as.character(facet) %in% names(tabela),
              !(as.character(facet) %in% c("etykieta", "wskaznik",
                                           "wartosc", "klucz")))
  }

  wykres = ggplot(do_wykresu_procenty(tabela)) +
    geom_line(aes(!!sym("wskaznik"), !!sym("wartosc"),
                  group = !!sym("klucz"), color = !!sym("klucz")),
              size = 2) +
    scale_y_continuous(etykietaY, labels = jako_procent) +
    scale_x_discrete(etykietaX) +
    scale_color_viridis_d(NULL, option = "E", end = 0.7) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  if (czyFacet) {
    wykres = wykres + facet_wrap(vars(!!facet), scales = "free_x")
  }
  wykres
}
