#' @title Informacje o danych
#' @description Funkcja zwraca ciąg znaków zawierający podstawowe informacje
#' o macierzy danych.
#' @param x macierz ramka danych (data frame)
#' @return
#' Funkcja zwraca milcząco ciąg znaków.
infoMacierzDanych = function(x) {
  stopifnot(is.matrix(x) | is.data.frame(x))

  lBD = sum(is.na(x))
  return(paste0("test złożony z ", ncol(x), " zadań\n",
                nrow(x), " obserwacji (zdających)\n",
                ifelse(lBD == 0, "w macierzy danych nie ma braków danych",
                       paste0("w macierzy danych ",
                              ifelse(lBD < 5 & lBD > 1, "są ", "jest "), lBD,
                              ifelse(lBD > 1,
                                     ifelse(lBD < 5, " braki ", " braków "),
                                     " brak "), "danych (",
                              format(100 * lBD / ncol(x) / nrow(x), digits = 3),
                              "% obserwacji)"))))
}
