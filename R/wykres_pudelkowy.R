#' @title Wykres pudelkowy wskaznika.
#' @description Funkcja zwraca wykres pudełkowy ilustrujący wartości wskaźników,
#' opisywanych przez 1. i 3. kwartyl, medianę oraz średnią.
#' @param tabela ramka danych zawierająca zestawienie wartości wskaźników
#' @param zmienna wyrażenie lub ciąg znaków - nazwa kolumny w przekazywanej
#' ramce danych, w której podane są opisy parametrów, których wartości zawiera
#' zestawienie (tj. tej kolumny, na podstawie której można się zorientować,
#' w którym wierszu opisane są wartości 1. kwartyle, w którym mediany itd.)
#' @param etykietaY ciąg znaków - etykieta osi Y wykresu
#' @param funkcjaFormatujacaY opcjonalnie funkcja, która ma zostać użyta do
#' formatowania etykiet wartości osi Y wykresu
#' @return wykres ggplot2
#' @export
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>% ensym sym rename
#' @importFrom ggplot2 ggplot aes geom_boxplot scale_y_continuous scale_x_discrete scale_fill_viridis_d scale_color_grey theme
wykres_pudelkowy = function(tabela, zmienna, etykietaY,
                            funkcjaFormatujacaY = MLASZraporty::dec_przecinek) {
  stopifnot(is_tibble(tabela) | is.data.frame(tabela),
            is.character(etykietaY), length(etykietaY) == 1,
            is.function(funkcjaFormatujacaY))
  zmienna = ensym(zmienna)
  stopifnot(as.character(zmienna) %in% names(tabela),
            !(as.character(zmienna) %in% c("klucz", "1.kwartyl", "mediana",
                                           "3.kwartyl")))

  tabela = rename(tabela, parametr = !!zmienna)
  ggplot(do_wykresu_pudelkowego(tabela)) +
    geom_boxplot(aes(!!sym("klucz"),
                     ymin = !!sym("1.kwartyl"), lower = !!sym("1.kwartyl"),
                     middle = !!sym("mediana"),
                     upper = !!sym("3.kwartyl"), ymax = !!sym("3.kwartyl"),
                     fill = !!sym("klucz"), color = !!sym("klucz")),
                 stat = "identity") +
    scale_y_continuous(etykietaY,
                       labels = funkcjaFormatujacaY, limits = c(0, NA)) +
    scale_x_discrete(NULL,
                     labels = function(x) {return(sub("[(]liczba miesięcy[)]",
                                                      "", x))}) +
    scale_fill_viridis_d(NULL, guide = FALSE, option = "E") +
    scale_color_grey(NULL, guide = FALSE, start = 0.9, end = 0.1) +
    theme(legend.position = "bottom")
}
