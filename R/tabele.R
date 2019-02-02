#' @title Drukowanie tabeli z wartosciami wskaznikow.
#' @description Funkcja przygotowuję tabelę z wartościami wskaźników do
#' wydrukowania i zwraca opisujący ją kod w formacie HTML lub LaTeX (PDF).
#' @param tabela ramka danych zawierająca zestawienie wartości wskaźników
#' @param typ "html" (domyślnie) lub "pdf" - format, w jakim ma zostać
#' zwrócony kod tabeli
#' @param dodajWierszSumy opcjonalnie wartość logiczna - czy dodawać do tabeli
#' wiersz sumy? (jeśli tak, wszystkie kolumny poza pierwszą powinny zawierać
#' liczby)
#' @param zamienZnakNowejLinii opcjonalnie ciąg znaków, który ma zostać
#' podstawiony w treści tabeli, ale \strong{nie w wierszu nagłówka}, zamiast
#' oznaczenia nowej linii (tj. zamiast ciągu '\\n')
#' @param align "center" (domyślnie) "left" lub "right" - wyrównanie tabeli
#' @return ciąg znaków zawierający kod w wybranym formacie opisujący tabelę
#' @export
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>% mutate_if
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling linebreak
drukuj_tabele = function(tabela, typ = "html", dodajWierszSumy = TRUE,
                         zamienZnakNowejLinii = as.character(NA),
                         align = "center") {
  stopifnot(is_tibble(tabela) | is.data.frame(tabela),
            is.character(typ), length(typ) == 1,
            is.logical(dodajWierszSumy), length(dodajWierszSumy) == 1,
            is.character(zamienZnakNowejLinii), length(zamienZnakNowejLinii) == 1,
            is.character(align), length(align) == 1)
  stopifnot(typ %in% c("html", "pdf"),
            dodajWierszSumy %in% c(TRUE, FALSE),
            align %in% c("center", "left", "right"))

  if (dodajWierszSumy) {
    tabela = dodaj_wiersz_sumy(tabela)
  }
  if (!is.na(zamienZnakNowejLinii)) {
    tabela = tabela %>%
      mutate_if(is.character, list(~gsub("\n", zamienZnakNowejLinii, .)))
    # names(tabela) = gsub("\n", zamienZnakNowejLinii, names(tabela))
  }
  if (typ %in% "pdf") {
    nazwyKolumn = names(tabela)
    for (i in 1:ncol(tabela)) {
      nazwyKolumn[i] = nazwyKolumn[i] %>%
        linebreak(align = ifelse(is.character(tabela[, i]), "l", "r"))
      nazwyKolumn[i] = gsub("[%]", "\\\\%", nazwyKolumn[i])
    }
    tabela %>%
      mutate_if(is.character, list(~gsub("[%]", "\\\\%",
                                         linebreak(., align = ifelse(is.character(.), "l", "r"))))) %>%
      kable("latex", booktabs = TRUE, escape = FALSE,
            col.names = nazwyKolumn,
            format.args = list(decimal.mark = ",", big.mark = " ")) %>%
      kable_styling(position = align) %>%
      return()
  } else if (typ %in% "html") {
    names(tabela) = gsub("\n", "<br/>", names(tabela))
    tabela %>%
      mutate_if(is.character, list(~gsub("\n", "<br/>", .))) %>%
      kable("html", booktabs = TRUE, escape = FALSE,
            format.args = list(decimal.mark = ",", big.mark = " ")) %>%
      kable_styling(bootstrap_options = c("striped", "hover"),
                    full_width = FALSE, position = align) %>%
      return()
  }
}
#' @title Funkcje nieeksportowane.
#' @description Funkcja zamienia pierwszą kolumnę ramki danych na nazwy jej
#' wierszy, aby przy jej drukowaniu móc uzyskać efekt braku nagłówka pierwszej
#' (drukowanej) kolumny.
#' @param x ramka danych
#' @return ramka danych
#' @importFrom tibble is_tibble
bez_naglowka_1kolumny = function(x) {
  stopifnot(is_tibble(x) | is.data.frame(x))
  if (is_tibble(x)) {
    x = as.data.frame(x)
  }
  rownames(x) = x[, 1]
  return(x[, -1, drop = FALSE])
}
#' @title Funkcje nieeksportowane.
#' @description Funkcja dodaje do ramki danych wiersz zawierający sumy kolumn.
#' @param x ramka danych
#' @param skoryguj_pr_do_100 opcjonalnie wartość logiczna - czy korygować sumy
#' kolumn, \strong{w nazwach których występuje znak '\%'}, aby wynosiły one 100?
#' (p. sekcja \emph{Details})
#' @param tekstSuma opcjonalnie ciąg znaków, który ma zostać użyty jako
#' etykieta wiersza sumy
#' @details Pierwsza kolumna traktowana jest jako zawierająca etykiety wartości.
#'
#' Korekta sumowania się kolumn, których nazwy zawierają znak '\%' do
#' 100 - wykonywana domyślnie, ale możliwa do wyłączenia przy pomocy argumentu
#' \code{skoryguj_pr_do_100} - ma na celu obejście problemu zaokrągleń przy
#' obliczaniu rozkładów częstości i w związku z tym wykonywana jest tylko
#' w zakresie niewielkich odstępstw sumy wartości w kolumnie od 100. Jeśli
#' różnica jest większa od 0,3 (tj. wartość sumy mniejsza niż 97 lub większ niż
#' 103), wartość sumy nie zostanie skorygowana. Nigdy nie są też korygowane
#' wartości w innych wierszach (niż wiersz sumy).
#' @return ramka danych
#' @importFrom stats setNames
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>% bind_rows mutate_if
dodaj_wiersz_sumy = function(x, skoryguj_pr_do_100 = TRUE,
                             tekstSuma = "suma") {
  stopifnot(is_tibble(x) | is.data.frame(x),
            is.logical(skoryguj_pr_do_100), length(skoryguj_pr_do_100) == 1,
            is.character(tekstSuma), length(tekstSuma) == 1)
  stopifnot(skoryguj_pr_do_100 %in% c(TRUE, FALSE),
            all(sapply(x[, -1], is.numeric)))
  if (is.factor(x[, 1])) {
    x[, 1] = levels(x[, 1])[x[, 1]]
  }
  x = bind_rows(x,
                data.frame(etykieta = tekstSuma,
                           matrix(colSums(x[, -1, drop = FALSE]), nrow = 1),
                           stringsAsFactors = FALSE) %>%
                  setNames(names(x)))
  if (skoryguj_pr_do_100) {
    x = mutate_if(x, grepl("%", names(x)),
                  list(~ifelse(c(rep(FALSE, nrow(x) - 1), TRUE) & abs(. - 100) <= 0.3,
                              100, .)))
  }
  return(x)
}
