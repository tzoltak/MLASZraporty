#' @title Drukowanie raportow z zestawieniami wskaznikow.
#' @description Funkcja generuje raporty na podstawie zestawienia zawierającego
#' wartości wskaźników dla poszczególnych grup (każda grupa, dla której ma
#' zostać wygenerowany raport opisywana jest w innym wierszu ramki danych
#' przekazywanej argumentem \code{wskazniki}).
#' @param szablon ciąg znaków - nazwa szablonu raportu (por.
#' \code{\link{wypisz_dostepne_szablony}})
#' @param wskazniki ramka danych ze wskaznikami grup, dla których mają zostać
#' utworzone raporty
#' @param wskaznikiGrPor opcjonalnie ramka danych ze wskaźnikami grup
#' porównawczych, do wykorzystania przy tworzeniu raportów
#' @param kolumnaNazwaPliku opcjonalnie wyrażenie lub ciąg znaków - kolumna
#' ramki danych przekazanej argumentem \code{wskazniki}, zawierająca nazwy
#' plików, do których mają zostać wydrukowane raporty
#' @param parametry lista parametrów przekazywanych do szablonu
#' @return wektor tekstowy z nazwami utworzonych raportów (niewidocznie)
#' @export
#' @importFrom tibble is_tibble
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom rlang ensym
#' @importFrom rmarkdown yaml_front_matter
generuj_raporty = function(szablon, wskazniki, wskaznikiGrPor = NULL,
                           kolumnaNazwaPliku = NULL, parametry = list()) {
  stopifnot(is.character(szablon), length(szablon) == 1,
            is_tibble(wskazniki) | is.data.frame(wskazniki),
            is_tibble(wskaznikiGrPor) | is.data.frame(wskaznikiGrPor) |
              is.null(wskaznikiGrPor),
            is.list(parametry))
  szablon = paste0(sub("[.]Rmd$", "", szablon), ".Rmd")
  if (!(szablon %in% suppressMessages(wypisz_dostepne_szablony()))) {
    stop("Szablon o podanej nazwie nie jest dostępny.\nSprawdź dostępne szablony korzystając z funkcji wypisz_dostepne_szablony().")
  }
  szablonZeSciezka = paste0(okresl_lokalizacje_szablonow(), "/", szablon)
  czyKolumnaNazwaPliku = tryCatch(!is.null(kolumnaNazwaPliku),
                                  error = function(e) {return(TRUE)})
  if (czyKolumnaNazwaPliku) {
    kolumnaNazwaPliku = ensym(kolumnaNazwaPliku)
    stopifnot(as.character(kolumnaNazwaPliku) %in% names(wskazniki))
    stopifnot(is.character(wskazniki[[kolumnaNazwaPliku]]))
    if (any(is.na(iconv(wskazniki[[kolumnaNazwaPliku]], to = "ASCII"))) |
        any(grepl('[/\\:*?<>|"]', wskazniki[[kolumnaNazwaPliku]]))) {
      stop("Wartości w kolumnie, na którą wskazuje argument 'kolumnaNazwaPliku' nie mogą zawierać polskich znaków ani znaków / \\ : ? < > | \".")
    }
    if (any(duplicated(wskazniki[[kolumnaNazwaPliku]]))) {
      stop("Wartości w kolumnie, na którą wskazuje argument 'kolumnaNazwaPliku' muszą być unikalne (inaczej jakiś raport nadpisał by inny).")
    }
  }

  yaml = yaml_front_matter(szablonZeSciezka, encoding = "UTF-8")
  niezbedneParametry = names(yaml$params)
  # Te parametry istnieją (i mogą mieć inne niż podane niżej wartości)
  # w szablonie raportu tylko po to, aby zapewnić możliwość jego łatwego "prototypowania".
  # Jednocześnie nie chcemy, aby użytkownik miał możliwość podać je samodzielnie.
  parametry$plikZObiektami = ""
  parametry$obiektWskazniki = "wskaznikiGrupa"
  parametry$obiektWskaznikiPorownanie = "wskaznikiGrPor"
  # Koniec nadpisywania wartości parametrów.
  brakujaceParametry = setdiff(unique(c(niezbedneParametry, "typDokumentu")),
                               c(names(wskazniki), names(parametry)))
  if (length(brakujaceParametry) > 0) {
    stop(paste0("Szablon '", szablon, "' wymaga następujących parametrów:\n",
                paste0("- '", brakujaceParametry, "'", collapse = ",\n"), "\n",
                "Muszą one albo wystąpić jako kolumny w ramce danych przekazanej argumentem 'wskazniki',\n",
                "albo zostać przekazane bezpośrednio jako elementy list przekazywanej argumentem 'parametry'."))
  }
  if ("obiektWskaznikiPorownanie" %in% niezbedneParametry & is.null(wskaznikiGrPor)) {
    stop(paste0("Szablon '", szablon, "' wymaga przekazania ramki danych ",
                "z wartościami wskaźników w grupach porównawczych\n ",
                "(argumentem 'wskaznikiGrPor')."))
  }
  if (!("obiektWskaznikiPorownanie" %in% niezbedneParametry) & !is.null(wskaznikiGrPor)) {
    message(paste0("Szablon '", szablon, "' nie korzysta z wartości wskaźników  w grupach porównawczych.\n",
                   "Ramka danych przekazana argumentem 'wskaznikiGrPor' nie zostanie użyta."))
  }
  stopifnot(length(parametry$typDokumentu) == 1)
  if (!(as.character(parametry$typDokumentu) %in% c("html", "pdf"))) {
    stop("Element 'typDokumentu' listy przekazywanej argumentem 'parametry' musi przyjmować wartość 'html' lub 'pdf'.")
  }

  nazwyRaportow = vector(mode = "character", length = 0)
  pb = txtProgressBar(0, nrow(wskazniki), style = 3)
  on.exit(close(pb))
  for (i in 1:nrow(wskazniki)) {
    if (!czyKolumnaNazwaPliku) {
      nazwaPliku = paste0("raport", i, ".", parametry$typDokumentu)
    } else {
      nazwaPliku = paste0(sub("[.](htm|html|pdf)", "",
                              wskazniki[i, as.character(kolumnaNazwaPliku)]),
                          ".", parametry$typDokumentu)
    }
    generuj_raport(szablonZeSciezka, nazwaPliku,
                    parametry = c(parametry,
                                  wskazniki[i, ] %>%
                                    select(one_of(setdiff(niezbedneParametry,
                                                          names(parametry)))) %>%
                                    as.list()),
                    wskazniki[i, ], wskaznikiGrPor)
    nazwyRaportow = c(nazwyRaportow, nazwaPliku)
    setTxtProgressBar(pb, i)
  }
  invisible(nazwyRaportow)
}
#' @title Funkcje nieeksportowane.
#' @description Funkcja pozwala oddzielić środowisko, w którym odbywa się
#' generowanie pojedynczego raportu od środowiska funkcji
#' \code{\link{generuj_raporty}}.
#' @param szablonZeSciezka ciąg znaków - ścieżka do pliku z szablonem raportu
#' @param nazwaPliku ciąg znaków - nazwa pliku raportu, któy ma zostać utworzony
#' @param parametry lista parametrów przekazywanych do szablonu
#' @param wskaznikiGrupa ramka danych zawierająca tylko jeden wiersz, ze
#' wskaznikami grupy, dla której ma zostać utworzony raport
#' @param wskaznikiGrPor opcjonalnie ramka danych ze wskaźnikami grup
#' porównawczych, do wykorzystania przy tworzeniu raportu
#' @return obiekt zwracany przez funkcję \code{\link[knitr]{knit_meta}}
#' zawierający metdane dotyczące utworzonego raportu
#' @importFrom tibble is_tibble
#' @importFrom knitr knit_meta
#' @importFrom rmarkdown render
generuj_raport = function(szablonZeSciezka, nazwaPliku, parametry,
                           wskaznikiGrupa, wskaznikiGrPor) {
  stopifnot(is.character(szablonZeSciezka), length(szablonZeSciezka) == 1,
            is.character(nazwaPliku), length(nazwaPliku) == 1,
            is.list(parametry),
            is_tibble(wskaznikiGrupa) | is.data.frame(wskaznikiGrupa),
            is_tibble(wskaznikiGrPor) | is.data.frame(wskaznikiGrPor) |
              is.null(wskaznikiGrPor))
  stopifnot(nrow(wskaznikiGrupa) == 1,
            file.access(szablonZeSciezka, 4) == 0)
  parametry = lapply(parametry, function(x) {
    if (is.character(x)) {
      return(gsub('"', '\\\\"', x))
    } else {
      return(x)
    }
  })
  # render() wywołując knit(), z kolei knit() ma tę własność, że zapisuje
  # "historię" metadanych nt. tworzonych dokumentów dopisując do niej nowe dane
  # po każdym swoim wywołaniu - aż w którymś momencie może być tego za dużo
  # i sypie się brakiem pamięci; wywołanie knit_meta() pozwala sczyścić te
  # zapisane metadane (i na wszelki wypadek robię to zarówno po, jak i przed)
  knit_meta(clean = TRUE)
  render(input = szablonZeSciezka,
         output_format = paste0(parametry$typDokumentu, "_document"),
         output_file = nazwaPliku, output_dir = "./",
         params = parametry, envir = new.env(),
         encoding = "UTF-8", quiet = TRUE, clean = TRUE)
  knit_meta(clean = TRUE)
}
