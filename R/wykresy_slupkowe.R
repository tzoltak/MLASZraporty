#' @title Wykres slupkowy skumulowany wskaznika.
#' @description Funkcja zwraca wykres słupkowy skumulowany ilustrujący
#' wartości wskaźników.
#' @param tabela ramka danych zawierająca zestawienie wartości wskaźników
#' @param zmienna wyrażenie lub ciąg znaków - nazwa kolumny w przekazywanej
#' ramce danych, w której podane są etykiety wartości wskaźników (tj. kolumna
#' wyznaczająca podział w ramach słupków)
#' @param x  wyrażenie lub ciąg znaków - nazwa kolumny w przekazywanej ramce
#' danych, której wartości będą ilustrowane na osi X wykresu (tj. kolumna
#' wyznaczająca podział na słupki)
#' @param facet opcjonalnie wyrażenie lub ciąg znaków - nazwa kolumny
#' w przekazywanej ramce danych, na podstawie wartości której dane zostaną
#' podzielone pomiędzy \emph{segmenty} wykresu
#' @return wykres ggplot2
#' @export
#' @importFrom tibble is_tibble
#' @importFrom dplyr ensym sym
#' @importFrom ggplot2 ggplot aes geom_col position_stack scale_y_continuous scale_x_discrete scale_fill_viridis_d facet_wrap
wykres_slupkowy_skumulowany_procenty = function(tabela, zmienna, x = "klucz",
                                                facet = NULL) {
  stopifnot(is_tibble(tabela) | is.data.frame(tabela))
  zmienna = ensym(zmienna)
  stopifnot(as.character(zmienna) %in% names(tabela),
            !(as.character(zmienna) %in% "wartosc"))
  x = ensym(x)
  stopifnot(as.character(x) %in% c(names(tabela), "grupa", "klucz"))
  czyFacet = tryCatch(!is.null(facet), error = function(e) {return(TRUE)})
  if (czyFacet) {
    facet = ensym(facet)
    stopifnot(as.character(facet) %in% c(names(tabela), "grupa", "klucz", "wskaznik"),
              !as.character(facet) %in% c("wartosc"))
  }

  wykres = ggplot(do_wykresu_procenty(tabela),
         aes(!!x, !!sym("wartosc"), fill = !!zmienna)) +
    geom_col(position = position_stack(), col = "black") +
    scale_y_continuous(NULL, labels = jako_procent) +
    scale_x_discrete(NULL) +
    scale_fill_viridis_d(as.character(zmienna), option = "C")
  if (czyFacet) {
    wykres = wykres + facet_wrap(vars(!!facet))
  }
  wykres
}
#' @title Wykres slupkowy wskaznika bedacego srednia zmiennej binarnej.
#' @description Funkcja zwraca wykres słupkowy ilustrujący wartości wskaźników
#' będących średnimi zmiennych (wskaźników z poziomu indywidualnego) binarnych.
#' @param tabela ramka danych zawierająca zestawienie wartości wskaźników
#' @param zmienna wyrażenie lub ciąg znaków - nazwa kolumny w przekazywanej
#' ramce danych, w której podane są etykiety wartości wskaźników, których
#' rozkłady mają zostać ze sobą porównane
#' @param facet opcjonalnie wyrażenie lub ciąg znaków - nazwa kolumny
#' w przekazywanej ramce danych, na podstawie wartości której dane zostaną
#' podzielone pomiędzy \emph{segmenty} wykresu
#' @param maxY opcjonalnie liczba - wartość maksymalna osi Y wykresu
#' @param etykietaY opcjonalnie ciąg znaków - etykieta osi Y wykresu
#' @param usunZEtykietX opcjonalnie ciąg znaków - wyrażenie regularne opisujące
#' ciąg znaków, który ma być usunięty z etykiet wartości osi X wykresu (zwykle
#' usuwanym cięgiem znaków jest ten, który zostaje podany jako wartość parametru
#' \code{etykietaY})
#' @return wykres ggplot2
#' @export
#' @importFrom rlang :=
#' @importFrom tibble is_tibble
#' @importFrom dplyr ensym sym mutate
#' @importFrom ggplot2 ggplot aes geom_col scale_y_continuous scale_x_discrete scale_fill_viridis_d facet_wrap
wykres_slupkowy_binarny = function(tabela, zmienna, facet = NULL, maxY = 100,
                                   etykietaY = "", usunZEtykietX = "") {
  stopifnot(is_tibble(tabela) | is.data.frame(tabela),
            is.numeric(maxY) | is.na(maxY), length(maxY) == 1,
            is.character(etykietaY), length(etykietaY) == 1,
            is.character(usunZEtykietX), length(usunZEtykietX) == 1)
  zmienna = ensym(zmienna)
  stopifnot(as.character(zmienna) %in% names(tabela),
            !(as.character(zmienna) %in% c("wartosc", "klucz")))
  if (!is.na(maxY)) {
    stopifnot(maxY > 0, maxY <= 100)
  }
  czyFacet = tryCatch(!is.null(facet), error = function(e) {return(TRUE)})
  if (czyFacet) {
    facet = ensym(facet)
    stopifnot(as.character(facet) %in% c(names(tabela), "grupa", "wskaznik"),
              !(as.character(facet) %in% c("wartosc", "klucz")))
  }

  tabela = mutate(tabela, !!zmienna := sub(usunZEtykietX, "", !!zmienna))
  if (!czyFacet & length(unique(tabela[, 1])) == 1) {
    if (etykietaY == "") {
      etykietaY = tabela[1, 1]
    }
    tabela[, 1] = names(tabela)[1]
  }
  wykres = ggplot(do_wykresu_procenty(tabela),
                  aes(!!zmienna, !!sym("wartosc"), fill = !!sym("klucz"))) +
    geom_col(position = "dodge", col = "black") +
    scale_y_continuous(etykietaY, labels = jako_procent, limits = c(0, maxY)) +
    scale_x_discrete(NULL) +
    scale_fill_viridis_d(NULL, option = "E")
  if (czyFacet) {
    wykres = wykres + facet_wrap(vars(!!facet))
  }
  wykres
}
#' @title Wykres slupkowy wskaznika mogacego sumowac sie do wiecej niz 100\%.
#' @description Funkcja zwraca wykres słupkowy ilustrujący wartości wskaźników,
#' w którym słupki nie są \emph{układane jeden na drugim}, tylko obok siebie.
#' Jest ona odpowiednia do ilustrowania wartości wskaźników procentowych,
#' których suma wartości może przekraczać 100\%.
#' @param tabela ramka danych zawierająca zestawienie wartości wskaźników
#' @param zmienna wyrażenie lub ciąg znaków - nazwa kolumny w przekazywanej
#' ramce danych, w której podane są etykiety wartości wskaźników, których
#' rozkłady mają zostać ze sobą porównane
#' @return wykres ggplot2
#' @export
#' @importFrom tibble is_tibble
#' @importFrom dplyr ensym sym
#' @importFrom ggplot2 ggplot aes geom_col scale_y_continuous scale_x_discrete scale_fill_viridis_d
wykres_slupkowy_procenty = function(tabela, zmienna) {
  stopifnot(is_tibble(tabela) | is.data.frame(tabela))
  zmienna = ensym(zmienna)
  stopifnot(as.character(zmienna) %in% names(tabela),
            !(as.character(zmienna) %in% c("wartosc", "klucz")))

  ggplot(do_wykresu_procenty(tabela),
         aes(!!sym("klucz"), !!sym("wartosc"), fill = !!zmienna)) +
    geom_col(position = "dodge", col = "black") +
    scale_y_continuous(NULL, labels = jako_procent) +
    scale_x_discrete(NULL) +
    scale_fill_viridis_d(as.character(zmienna), option = "E")
}
