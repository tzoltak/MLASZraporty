#' @title Formatowanie etykiet osi wykresu.
#' @param x liczba
#' @description Funkcje służa formatowaniu etykiet osi Y na wykresach:
#' odpowiednio zmieniają znak dziesiętny z '.' na ',' oraz dopisują znak '\%'
#' za liczbą (również zmieniając znak dziesiętny na ',').
#' @export
dec_przecinek = function(x) {
  format(x, decimal.mark = ",") %>%
    return()
}
#' @rdname dec_przecinek
#' @export
jako_procent = function(x) {
  format(x, decimal.mark = ",") %>%
    paste0("%") %>%
    return()
}
