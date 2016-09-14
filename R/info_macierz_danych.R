#' @title Informacje o danych
#' @description Funkcja zwraca ciąg znaków zawierający podstawowe informacje
#' o macierzy danych.
#' @param x macierz ramka danych (data frame)
#' @return
#' Funkcja zwraca milcząco ciąg znaków.
info_macierz_danych = function(x) {
  stopifnot(is.matrix(x) | is.data.frame(x))

  lBD = sum(is.na(x))
  lO = nrow(x) - 10 * floor(nrow(x) / 10)
  return(paste0("test złożony z ", ncol(x), " zada",
                ifelse(ncol(x) > 1, "ń", "nia"), "\n",
                nrow(x), " obserwacj", ifelse(nrow(x) == 1, "a",
                                              ifelse(lO < 5, "e", "i")),
                " (zdający", ifelse(nrow(x) == 1, "", "ch"), ")\n",
                ifelse(lBD == 0, "w macierzy danych nie ma braków danych",
                       paste0("w macierzy danych ",
                              ifelse(lBD < 5 & lBD > 1, "są ", "jest "), lBD,
                              ifelse(lBD > 1,
                                     ifelse(lBD < 5, " braki ", " braków "),
                                     " brak "), "danych (",
                              format(100 * lBD / ncol(x) / nrow(x), digits = 3),
                              "% obserwacji)"))))
}
