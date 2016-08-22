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
  return(TRUE)
}
