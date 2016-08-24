#' @title Trudność zadań
#' @description Funkcja służy do oszacowania trudności zadań.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param maks opcjonalnie wektor liczb całkowitych opisujący maksymalną
#' liczbę puntków możliwych do uzyskania za poszczególne zadania
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @param verbose wartość logiczna - czy wydrukować wyniki analizy
#' @return
#' Funkcja zwraca milcząco wektor z oszacowaniami trudności zadań.
#' @examples
#' trudnosc(symTest)
#' @export
trudnosc = function(x, maks = NULL, na.rm = TRUE, verbose = TRUE) {
  assert_mdfn(x)
  maskaMin = apply(x, 2, min, na.rm = TRUE) < 0
  if (any(maskaMin & !is.na(maskaMin))) {
    stop(paste0("Kolumny: '",
                paste0(colnames(x)[maskaMin & !is.na(maskaMin)],
                       collapse = "', '"),
                ' zawierają wartości mniejsze od 0.'))
  }
  stopifnot(na.rm %in% c(FALSE, TRUE), verbose %in% c(FALSE, TRUE))
  if (!is.null(maks)) {
    maksEmp = apply(x, 2, max, na.rm = na.rm)
    if (!is.vector(maks)) {
      stop("Argument 'maks' musi być wektorem liczb dodatnich.")
    }
    if (!is.numeric(maks)) {
      stop("Argument 'maks' musi być wektorem liczb dodatnich.")
    }
    if (any(maks <= 0 & !is.na(maks))) {
      stop("Argument 'maks' musi być wektorem liczb dodatnich.")
    }
    if (length(maks) != ncol(x)) {
      stop(paste0("Wektor podany w argumencie 'maks' musi mieć tyle samo ",
                  "elementów, ile jest kolumn w macierzy (lub ramce danych) ",
                  "podanych w argumencie 'x'."))
    }
    if (!is.null(names(maks))) {
      if (any(names(maks) != colnames(x))) {
        warning("Nazwy kolumn macierzy (lub ramki danych) podanych ",
                "w argumencie 'x' i nazwy elemenentów wektora podanego ",
                "w argumencie 'maks' nie pasują do siebie.")
      }
    }
    if (any(maks < maksEmp & !is.na(maks))) {
      stop(paste0("W kolumnach '",
                  paste0(colnames(x)[maks < maksEmp & !is.na(maks)],
                         collapse = "', '"),
                  "' niektóre obserwacje mają przypisaną liczbę punktów ",
                  "większą, niż maksymalna możliwa (podana w argumencie ",
                  "'maks')."))
    }
  } else {
    maks = apply(x, 2, max, na.rm = na.rm)
  }

  trudnosci = setNames(as.numeric(rep(NA, ncol(x))), colnames(x))
  for (i in 1:ncol(x)) {
    trudnosci[i] = 1 - mean(x[, i], na.rm = na.rm) / maks[i]
  }
  if (verbose) {
    cat("Oszacowanie trudności zadań:\n\n",
        infoMacierzDanych(x), "\n\n", sep = "")
    print(data.frame(zadanie = colnames(x), "maks.pkt." = maks,
                     "trudność" = trudnosci, "łatwość" = 1 - trudnosci,
                     check.names = FALSE), row.names = FALSE, digits = 2)
    cat("\n")
  }

  invisible(trudnosci)
}
#' @title Łatwość zadań
#' @description Funkcja służy do oszacowania łatwości zadań.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param maks opcjonalnie wektor liczb całkowitych opisujący maksymalną
#' liczbę puntków możliwych do uzyskania za poszczególne zadania
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @param verbose wartość logiczna - czy wydrukować wyniki analizy
#' @return
#' Funkcja zwraca milcząco wektor z oszacowaniami łatwości zadań.
#' @examples
#' latwosc(symTest)
#' @export
latwosc = function(x, maks = NULL, na.rm = TRUE, verbose = TRUE) {
  latwosci = 1 - trudnosc(x, maks = maks, na.rm = na.rm, verbose = verbose)
  invisible(latwosci)
}
