#' @title Lista szablonow raportow dostepnych w ramach pakietu.
#' @description Funkcja pokazuje nazwy szablonów raportów, które są dostępne
#' w ramach pakietu.
#' @return wektor tekstowy (niewidocznie)
#' @export
wypisz_dostepne_szablony = function() {
  szablony = list.files(okresl_lokalizacje_szablonow(), "[.]Rmd$")
  message("Dostępne szablony:\n", paste0("- '", szablony, "'", collapse = ",\n"))
  invisible(szablony)
}
#' @title Funkcje nieeksportowane.
#' @description Funkcja określa, gdzie znajdują się szablony raportów
#' (w zależności od tego, czy pakiet jest testowany, czy już zainstalowany
#' znajdują się one albo w folderze 'inst/szablony_arportow' albo w folderze
#' 'szablony_arportow/' głównego folderu pakietu) i zwraca ścieżkę do
#' odpowiedniego folderu.
#' @return ścieżka do folderu
okresl_lokalizacje_szablonow = function() {
  sciezkaDoPakietu = find.package("MLASZraporty")
  if (file.exists(paste0(sciezkaDoPakietu, "/inst/szablony_raportow"))) {
    return(paste0(sciezkaDoPakietu, "/inst/szablony_raportow")) # nocov
  } else {
    return(paste0(sciezkaDoPakietu, "/szablony_raportow"))
  }
}
