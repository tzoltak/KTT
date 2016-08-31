#' @title Sprawdzanie argumentów funkcji
#' @description Funkcja sprawdza, czy argument jest macierz liczbową lub ramką
#' danych zawierającą tylko zmienne liczbowe.
#' @param x argument do sprawdzenia
#' @param nazwaArgumentu nazwa argumentu, która zostanie wstawiona do
#' komunikatów o błędzie
#' @return
#' Funkcja zwraca \code{TRUE} jeśli argument jest macierzą liczbową lub ramką
#' daych zawierającą tylko zmienne liczbowe. W przeciwnym wypadku wywołuje błąd.
assert_mdfn = function(x, nazwaArgumentu = "x") {
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop(paste0("Argument '", nazwaArgumentu,
                "' musi być macierz (matrix) lub ramką danych (data frame)."))
  }
  if (is.matrix(x)) {
    if (!is.numeric(x)) {
      stop(paste0("Macierz podana w argumencie '", nazwaArgumentu,
                  "' musi zawierać tylko liczby (być typu 'numeric').\n",
                  "Podana macierz jest typu '", mode(x), "'."))
    }
    if (is.null(colnames(x))) {
      warning(paste0("Kolumny macierzy podanej w argumencie '", nazwaArgumentu,
                     "' nie mają nadanych nazw."))
    }
  } else {
    kolMode = sapply(x, function(x) {
      if (is.factor(x)) {
        return("factor")
      } else {
        return(mode(x))
      }
    })
    if (!all(kolMode %in% "numeric")){
      stop(paste0("Wszystkie kolumny ramki danych podanej w argumencie '",
                  nazwaArgumentu,
                  "' muszą zawierać tylko zmienne liczbowe (typu 'numeric').\n",
                  paste0("  * Kolumna '", names(x)[!(kolMode %in% "numeric")],
                         "' jest typu '", kolMode[!(kolMode %in% "numeric")],
                         "'.\n", collapse = "")))
    }
  }
  if (ncol(x) == 1) {
    warning(paste0("Podane dane zawierają tylko jedną kolumnę, co czyni",
                   "analizy psychometrczne bezsensownymi."))
  } else if (ncol(x) == 2) {
    warning(paste0("Podane dane zawierają tylko dwie kolumny - część analiz",
                   "może nie mieć sensu."))
  }
  return(TRUE)
}
#' @title Sprawdzanie argumentów funkcji
#' @description Funkcja sprawdza, czy argument jest wektorem liczb pasującym
#' do odpowiedniej macierzy/ramki danych.
#' @param x argument do sprawdzenia - wektor
#' @param y argument, względem którego \code{x} ma być zweryfikowany - macierz
#' lub ramka danych
#' @param nazwaArgumentu nazwa argumentu, która zostanie wstawiona do
#' komunikatów o błędzie
#' @return
#' Funkcja zwraca \code{TRUE} jeśli argument \code{x} jest poprawny.
#' W przeciwnym wypadku wywołuje błąd.
assert_mm = function(x, y, nazwaArgumentu = "x") {
  if (!is.vector(x)) {
    stop("Argument '", nazwaArgumentu, "' musi być wektorem liczb.")
  }
  if (!is.numeric(x)) {
    stop("Argument '", nazwaArgumentu, "' musi być wektorem liczb.")
  }
  if (length(x) != ncol(y)) {
    stop(paste0("Wektor podany w argumencie '", nazwaArgumentu,
                "' musi mieć tyle samo  elementów, ile jest kolumn w macierzy ",
                "danych"))
  }
  if (!is.null(names(x))) {
    if (any(names(x) != colnames(y))) {
      warning("Nazwy kolumn macierzy danych i nazwy elemenentów wektora ",
              "podanego w argumencie '", nazwaArgumentu, "' nie pasują do ",
              "siebie.")
    }
  }
  if (all(is.na(x))) {
    warning("Wektor podany w argumencie '", nazwaArgumentu, "' zawiera same ",
            "braki danych.")
  }
  return(TRUE)
}
#' @title Sprawdzanie argumentów funkcji
#' @description Funkcja sprawdza, czy argument jest pasującym do odpowiedniej
#' macierzy/ramki danych wektorem opisującym minimalne możliwe do przyjęcia
#' wartości.
#' @param maks argument do sprawdzenia - wektor minimalnych wartości lub NULL
#' @param x argument, względem którego \code{x} ma być zweryfikowany - macierz
#' lub ramka danych
#' @return
#' Funkcja zwraca wektor o liczbie elementóW odpowiadającej liczie kolumn
#' \code{x}, który zawiera minimalnych możliwych do przyjęcia wartości.
assert_maks = function(maks, x) {
  if (!is.null(maks)) {
    assert_mm(maks)
    maksEmp = apply(x, 2, max, na.rm = TRUE)
    if (any(maks < maksEmp & !is.na(maks))) {
      stop(paste0("W kolumnach '",
                  paste0(colnames(x)[maks < maksEmp & !is.na(maks)],
                         collapse = "', '"),
                  "' niektóre obserwacje mają przypisaną liczbę punktów ",
                  "większą, niż maksymalna możliwa (podana w argumencie ",
                  "'maks')."))
    }
  } else {
    maks = apply(x, 2, max, na.rm = TRUE)
  }
  return(maks)
}
#' @title Sprawdzanie argumentów funkcji
#' @description Funkcja sprawdza, czy argument jest pasującym do odpowiedniej
#' macierzy/ramki danych wektorem opisującym minimalne możliwe do przyjęcia
#' wartości.
#' @param min argument do sprawdzenia - wektor minimalnych wartości lub NULL
#' @param x argument, względem którego \code{x} ma być zweryfikowany - macierz
#' lub ramka danych
#' @return
#' Funkcja zwraca wektor o liczbie elementóW odpowiadającej liczie kolumn
#' \code{x}, który zawiera minimalnych możliwych do przyjęcia wartości.
assert_min = function(min, x) {
  if (!is.null(min)) {
    assert_mm(min, x, "min")
    minEmp = apply(x, 2, min, na.rm = TRUE)
    if (any(min > minEmp & !is.na(min))) {
      stop(paste0("W kolumnach '",
                  paste0(colnames(x)[min < minEmp & !is.na(min)],
                         collapse = "', '"),
                  "' niektóre obserwacje mają przypisaną liczbę punktów ",
                  "mniejszą, niż minimalna możliwa (podana w argumencie ",
                  "'min')."))
    }
  } else {
    min = rep(0, ncol(x))
    maskaMin = apply(x, 2, min, na.rm = TRUE) < 0
    if (any(maskaMin & !is.na(maskaMin))) {
      stop(paste0("Kolumny: '",
                  paste0(colnames(x)[maskaMin & !is.na(maskaMin)],
                         collapse = "', '"),
                  ' zawierają wartości mniejsze od 0.'))
    }
  }
  return(min)
}
#' @title Sprawdzanie argumentów funkcji
#' @description Funkcja sprawdza, czy argument jest wektorem liczb.
#' @param x argument do sprawdzenia
#' @param nazwaArgumentu nazwa argumentu, która zostanie wstawiona do
#' komunikatów o błędzie
#' @return
#' Funkcja zwraca \code{TRUE} jeśli argument jest wektorem liczb. W przeciwnym
#' wypadku wywołuje błąd.
assert_w = function(x, nazwaArgumentu = "x") {
  if (!is.vector(x)) {
    stop(paste0("Argument '", nazwaArgumentu, "' musi być wektorem liczb."))
  }
  if (!is.numeric(x)) {
    stop(paste0("Argument '", nazwaArgumentu, "' musi być wektorem liczb."))
  }
  return(TRUE)
}
