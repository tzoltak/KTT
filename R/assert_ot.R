#' @title Sprawdzanie argumentow funkcji
#' @description Funkcja sprawdza, czy podane dane są zgodne podanym opisem
#' struktury testu (oraz czy sam ten opis jest poprawny).
#' @param x argument do sprawdzenia - dane
#' @param y argument do sprawdzenia - opis struktury testu
#' @param nazwaArgumentuX nazwa argumentu, która zostanie wstawiona do
#' komunikatów o błędzie
#' @param nazwaArgumentuY nazwa argumentu, która zostanie wstawiona do
#' komunikatów o błędzie
#' @return
#' Funkcja zwraca \code{TRUE} jeśli argument \code{y} jest poprawnym opisem
#' struktury testu oraz dane podane argumentem \code{x} są z nim zgodne.
#' W przeciwnym wypadku wywołuje błąd.
assert_ot = function(x, y, nazwaArgumentuX = "x",
                     nazwaArgumentuY = "opisTestu") {
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop(paste0("Argument '", nazwaArgumentuX,
                "' musi być macierzą (matrix) lub ramką danych (data frame)."))
  }
  #|-> Sprawdzanie poprawności opisu struktury testu
  if (!is.data.frame(y)) {
    stop(paste0("Argument '", nazwaArgumentuY,
                "' musi być ramką danych (data frame)."))
  }
  wymaganeKolumny =  c("zmienna", "typ", "odpowiedzi", "poprawna_odpowiedz")
  if (!all(names(wymaganeKolumny %in% y))) {
    stop(paste0("Ramka danych podana argumentem '", nazwaArgumentuY,
                "' musi zawierać kolumny: '",
                paste0(wymaganeKolumny, collapse = "', '"), "'."))
  }
  opcjonalneKolumny = c("etykieta", "uwagi")
  if (!all(names(y %in% c(wymaganeKolumny, opcjonalneKolumny)))) {
    warning(paste0("Ramka danych podana argumentem '", nazwaArgumentuY,
                   "' zawiera dodatkowe kolumny (inne niż: '",
                   paste0(c(wymaganeKolumny, opcjonalneKolumny),
                          collapse = "', '"),
                   "'). Zawarte w nich informacje nie zostaną w żaden sposób ",
                   "wykorzystane"))
    y = y[, names(y) %in% c(wymaganeKolumny, opcjonalneKolumny)]
  }
  for (i in opcjonalneKolumny) {
    if (!(i %in% names(y))) {
      y = cbind(y, setNames(data.frame(x = rep("", nrow(y)),
                                       stringsAsFactors = FALSE), i))
    }
  }
  for (i in 1:ncol(y)) {
    y[, i] = as.character(y[, i])
    y[is.na(y[, i]), i] = ""
  }
  ## zmienna powinna przyjmować tylko unikalne wartości
  if (any(duplicated(y$zmienna))) {
    stop(paste0("Nazwy zmiennych podane w kolumnie 'zmienna' ramki danych ",
                "podanej argumentem '", nazwaArgumentuY, "' nie są unikalne.\n\n",
                "Problem występuje dla zmiennych: '",
                paste0(y$zmienna[duplicated(y$zmienna)], collapse = "', '"), "'."))
  }
  ## typ może zawierać tylko 'o', 'z' lub ''
  y$typ = gsub(" ", "", y$typ)
  if (!all(y$typ %in% c("", "o", "z"))) {
    print(y[!(y$typ %in% c("", "o", "z")), c("zmienna", "typ")],
          row.names = FALSE)
    stop(paste0("Kolumna 'typ' w argumencie '", nazwaArgumentuY, "' może ",
                "zawierać tylko wartości: 'o', 'z' lub '' (pusty ciąg znaków)."))
  }
  ## zamknięte muszą mieć podaną poprawną odpowiedź
  if (!all(y$poprawna_odpowiedz[y$typ == "z"] != "")) {
    stop(paste0("Dla wszystkich zadań zamkniętych (tj. opisanych wartością 'z' ",
                "w kolumnie 'typ' ramki danych podanej argumentem '",
                nazwaArgumentuY, "') musi zostać podane, jaka jest prawidłowa ",
                "odpowiedź (tj. wartość kolumny 'poprawna_odpowiedz' nie może ",
                "być brakiem danych ani pustym ciągiem znaków).\n\n",
                "Problem występuje dla zmiennych: '",
                paste0(y$zmienna[y$typ == "z" & y$poprawna_odpowiedz == ""],
                       collapse = "', '"), "'."))
  }
  ## w zamkniętych, jeśli podano odpowiedzi, to musi wśród nich być to,
  ##   co podano jako poprawną odpowiedź
  odpowiedzi = gsub("^[ ]+|[ ]+$", "", y$odpowiedzi)
  odpowiedzi = sub("^([[:digit:]]+)$", "0-\\1", odpowiedzi)
  odpowiedzi = sapply(odpowiedzi, function(x) {
    if (grepl("^[[:digit:]]+-[[:digit:]]+$", x)) {
      x = as.numeric(strsplit(x, "-")[[1]])
      return(paste0(as.character(seq(x[1], x[2])), collapse = ","))
    } else {
      return(x)
    }
  })
  odpowiedzi = strsplit(odpowiedzi, "[ ]*[,;][ ]*", fixed = FALSE)
  maska = rep(FALSE, nrow(y))
  for (i in 1:nrow(y)) {
    if (y$typ[i] == "z" & length(odpowiedzi[[i]]) > 0) {
      maska[i] = !(y$poprawna_odpowiedz[i] %in% odpowiedzi[[i]])
    }
  }
  if (any(maska)) {
    print(y[maska, c("zmienna", "typ", "odpowiedzi", "poprawna_odpowiedz")],
          row.names = FALSE)
    stop(paste0("Dla wyświetlonych powyżej zadań w opisie struktury testu ",
                "(argument '", nazwaArgumentuY, "') podana wartość ",
                "prawidłowej odpowiedzi (kolumna 'poprawna_odpowiedz') nie",
                "występuje wśród podanych możliwych wartości odpowiedzi ",
                "(kolumna 'odpowiedzi')."))
  }
  ## otwarte jako odpowiedzi muszą mieć podane tylko liczby
  maska = rep(FALSE, nrow(y))
  for (i in 1:nrow(y)) {
    if (y$typ[i] == "o") {
      maska[i] = any(is.na(suppressWarnings(as.numeric(odpowiedzi[[i]]))))
    }
  }
  if (any(maska)) {
    print(y[maska, c("zmienna", "typ", "odpowiedzi")],
          row.names = FALSE)
    stop(paste0("Dla wyświetlonych powyżej zadań w opisie struktury testu ",
                "(argument '", nazwaArgumentuY, "') wśród podanych możliwych ",
                "wartości odpowiedzi (kolumna 'odpowiedzi') występują wartości, ",
                "które nie są liczbami. Dla zadań typu 'o' możliwymi ",
                "wartościami odpowiedzi muszą być (tylko) liczby (punktów ",
                "możliwych do uzyskania w danym zadaniu)."))
  }
  #|<- Sprawdzanie poprawności opisu struktury testu
  #|-> Sprawdzanie zgodności danych z opisem struktury
  ## nazwy zmiennych
  if (!all(y$zmienna %in% names(x))) {
    stop(paste0("W zbiorze danych podanym argumentem '", nazwaArgumentuX, "' ",
                "nie występują niektóre kolumny, podane w opisie struktury ",
                "testu (argument '", nazwaArgumentuY, "').\n\n",
                "Problem występuje dla zmiennych: '",
                paste0(setdiff(y$zmienna, names(x)), collapse = "', '"), "'."))
  }
  if (!all(names(x) %in% y$zmienna)) {
    warning(paste0("W zbiorze danych podanym argumentem '", nazwaArgumentuX, "' ",
                   "występują dodatkowe kolumny, które nie pojawiają się ",
                   "w opisie struktury testu (argument '", nazwaArgumentuY,
                   "'): '", paste0(setdiff(names(x), y$zmienna),
                                   collapse = "', '"), "'."))
  }
  ## poprawność wartości
  # warning o ew. zmiennych, dla których nie podano dozwolonych wartości
  if (!all(y$typ == "" | y$odpowiedzi != "")) {
    warning(paste0("Dla niektórych zadań w opisie struktury testu nie podano ",
                   "dozwolonych wartości (tj. kolumna 'odpowiedzi' w ramce ",
                   "danych przekazanej argumentem '", nazwaArgumentuY,
                   "' jest pusta). W odniesieniu do tych zadań poprawność ",
                   "wartości występujących w danych nie może zostać sprawdzona.\n\n",
                   "Problem występuje dla zmiennych: '",
                   paste0(y$zmienna[y$typ != "" & y$odpowiedzi == ""],
                          collapse = "', '"), "'."))
  }
  # samo sprawdzanie
  bledneWartosci = as.character(rep(NA, nrow(y)))
  for (i in which(y$odpowiedzi != "")) {
    temp = unique(x[, y$zmienna[i]])
    temp = temp[!(temp %in% c(NA, ""))]
    if (!all(temp %in% odpowiedzi[[i]])) {
      temp = sort(setdiff(temp, odpowiedzi[[i]]))
      bledneWartosci[i] =
        paste0("'", paste0(temp[1:min(c(3, length(temp)))],
                           collapse = "', '"), "'",
               ifelse(length(temp) > 3,
                      paste0(", ... i ", length(temp) - 3, " innych wartości"),
                      ""))
    }
  }
  if (!all(is.na(bledneWartosci))) {
    print(data.frame(zmienna = y$zmienna[!is.na(bledneWartosci)],
                     "niedozwolone wartości" =
                       bledneWartosci[!is.na(bledneWartosci)],
                     check.names = FALSE), row.names = FALSE)
    stop(paste0("W danych (argument '", nazwaArgumentuX, "') występują ",
                "wartości odpowiedzi spoza zakresu dozwolonych wartości, ",
                "opisanego w kolumnie 'odpowiedzi' ramki danych przekazanej ",
                "argumentem '", nazwaArgumentuY, "' (p. zestawienie powyżej)."))
  }
  #|<- Sprawdzanie zgodności danych z opisem struktury
  y$odpowiedzi = odpowiedzi
  return(y)
}
