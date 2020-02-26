#' @title Funkcje nieeksportowane.
#' @description Funkcja przekształca ramkę danych zawierającą zestawienie
#' wartości wskaźników, które mają być zilustrowane wykresem porównującym
#' rozkłady wyrażone w procentach, do postaci, w której może być ona użyta jako
#' argument funkcji \code{\link[ggplot2]{ggplot}}.
#' @param x ramka danych w postaci \emph{szerokiej}
#' @return ramka danych w postaci \emph{długiej}
#' @details Funkcja zakłada, że pierwsza kolumna przekazywanej ramki zawiera
#' etykiety wartości. Oprócz tej kolumny w specjalny sposób traktuje też kolumny
#' o nazwach \emph{grupa} i \emph{wskaźniki} (o ile występują w przekazanej
#' ramce danych). Spośród pozostałych kolumn usuwa te, które nie wyglądają jej
#' na opisujące rozkłady procentowe. Na podstawie nazw pozostałych kolumn stara
#' się określić, które z nich opisują daną szkołę, a które grupę porównawczą
#' (dopuszcza tu nieco oboczności zapisu, niemniej ten dychotomiczny podział
#' jest jedynym w tej chwili obsługiwanym).
#' @importFrom tibble is_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr %>% .data matches mutate one_of select
do_wykresu_procenty = function(x) {
  stopifnot(is_tibble(x) | is.data.frame(x))

  # zamiana kolumn na czynniki, aby zachować porządaną kolejność grup
  # i wskaźników (tj. taką, w jakiej pojawiają się w przekazanej tabeli)
  x[, 1] = factor(x[, 1], levels = unique(x[, 1]))
  if ("grupa" %in% names(x)) {
    x = x %>%
      mutate(grupa = factor(.data$grupa, unique(.data$grupa)))
  }
  if ("wskaznik" %in% names(x)) {
    x = x %>%
      mutate(wskaznik = factor(.data$wskaznik, unique(.data$wskaznik)))
  }
  # przekształcanie
  names(x) = sub("w grupie( |\\n)porównawczej", "w gr. por.", names(x))
  x = select(x, -matches("^(n|liczba)$|( |\\n)(n|liczba)$"))
  x = suppressWarnings(gather(x, "klucz", "wartosc",
                              -one_of(names(x)[1], "wskaznik", "grupa"))) %>%
    mutate(klucz = factor(.data$klucz,
                          levels = c(grep("([%]|w tym powiecie|w tej szkole)$",
                                          .data$klucz, value = TRUE),
                                     grep("w gr[.] por[.]$",
                                          .data$klucz, value = TRUE)) %>%
                            unique(),
                          labels = c(paste0(sub("([%]|w tym powiecie)$", "",
                                                grep("([%]|w tym powiecie)$",
                                                     .data$klucz, value = TRUE)),
                                            "w tym powiecie"),
                                     sub("[%] ", "",
                                         grep("w gr[.] por[.]$",
                                              .data$klucz, value = TRUE))) %>%
                            unique()))
  levels(x$klucz) = sub("w gr[.] por[.]", "w grupie\nporównawczej",
                        levels(x$klucz))
  if (!("grupa" %in% names(x))) {
    x = x %>%
      mutate(grupa = factor(ifelse(grepl("w tym powiecie", .data$klucz),
                                   "w tym powiecie", "w grupie\nporównawczej"),
                            c("w tym powiecie", "w grupie\nporównawczej")))
  }
  if (!("wskaznik" %in% names(x))) {
    x = x %>%
      mutate(wskaznik = sub("( |\n|)(w tym powiecie|w grupie\nporównawczej)$",
                            "",
                            .data$klucz),
             wskaznik = sub("([[:digit:]]+[.]|ym|im)( |\n)(mies[.]|miesiącu)",
                            "\\1 miesiącu\npo ukończeniu szkoły",
                            .data$wskaznik))
    liczebniki = c("pierwszym", "drugim", "trzecim", "czwartym", "piątym",
                   "szóstym", "siódmym", "ósmym", "dziewiątym", "dziesiątym",
                   "jedenastym")
    for (l in 1:length(liczebniki)) {
      x$wskaznik = sub(paste0("w ", l, "[.] miesiącu"),
                       paste0("w ", liczebniki[l], " miesiącu"), x$wskaznik)
    }
    x$wskaznik = factor(x$wskaznik, unique(x$wskaznik))
  }
  return(x)
}
