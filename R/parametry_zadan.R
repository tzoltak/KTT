#' @title Parametry zadań
#' @description Funkcja szacuje parametry zadań: łatwości/trudności, moc
#' różnicującą, rzetelność testu z wyłączeniem zadania.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param maks opcjonalnie wektor liczb całkowitych opisujący maksymalną
#' liczbę puntków możliwych do uzyskania za poszczególne zadania
#' @param min opcjonalnie wektor liczb całkowitych opisujący minimalną
#' wartość, jaką może przyjąć wynik poszczególnych zadań
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @param verbose wartość logiczna - czy wydrukować wyniki analizy
#' @seealso \code{\link{latwosc}}, \code{\link{moc_roznicujaca}},
#' \code{\link{alfa}}, \code{\link{wykres_lmr}}
#' @return
#' Funkcja zwraca milcząco listę z parametrami zadań.
#' @examples
#' parametry_zadan(wynikiSymTest)
#' @export
parametry_zadan = function(x, maks = NULL, min = NULL, na.rm = TRUE, verbose = TRUE) {
  stopifnot(na.rm %in% c(FALSE, TRUE), verbose %in% c(FALSE, TRUE))

  trudnosci = trudnosc(x, na.rm = na.rm, verbose = FALSE)
  mocRoznicujaca = moc_roznicujaca(x, na.rm = na.rm, verbose = FALSE)
  alfyC = alfaC(x, na.rm = na.rm, verbose = FALSE)
  alfyFR = alfaFR(x, na.rm = na.rm, verbose = FALSE)

  cat("Parametry zadań:\n\n",
      info_macierz_danych(x), "\n\n",
      "rzetelność testu:\n",
      "alfa Cronbacha = ", format(alfyC$alfa, digits = 3, nsmall = 3), "\n",
      "alfa Feldt-Raju = ", format(alfyFR$alfa, digits = 3, nsmall = 3),
      "\n\n", sep = "")
  if (verbose) {
    print(data.frame("zadanie" = colnames(x),
                     "maks.pkt." = attributes(trudnosci)$maks,
                     "trudność" = format(trudnosci, digits = 2, nsmall = 2),
                     "łatwość" = format(1 - trudnosci, digits = 2, nsmall = 2),
                     "MR-P" = format(mocRoznicujaca$korelacjePearsona,
                                     digits = 3, nsmall = 3),
                     "MR-PBZ" = format(mocRoznicujaca$korelacjeBezZadania,
                                       digits = 3, nsmall = 3),
                     "MR-DS" = format(mocRoznicujaca$korelacjeDwuseryjne,
                                      digits = 3, nsmall = 3),
                     "rzetBZ-aC" = paste0(format(alfyC$alfaBZ,
                                                 digits = 3, nsmall = 3),
                                          ifelse(alfyC$alfaBZ > alfyC$alfa &
                                                   !is.na(alfyC$alfaBZ),
                                                 " (!)", "    ")),
                     "rzetBZ-aFR" = paste0(format(alfyFR$alfaBZ,
                                                  digits = 3, nsmall = 3),
                                           ifelse(alfyFR$alfaBZ > alfyFR$alfa &
                                                    !is.na(alfyFR$alfaBZ),
                                                  " (!)", "    ")),
                     check.names = FALSE), row.names = FALSE)

    cat("\n",
        "MR-P       - moc różnicująca: korelacja liniowa Pearsona (punktowo-dwuseryjna)\n",
        "MR-PBZ     - moc różnicująca: korelacja Pearsona z sumą punktów z wyłączeniem danego zadania\n",
        "MR-DS      - moc różnicująca: korelacja dwuseryjna\n",
        "rzetBZ-aC  - rzetelność bez zadania: alfa Cronbacha\n",
        "rzetBZ-aFR - rzetelność bez zadania: alfa Feldt-Raju\n",
        "Zadania, po usunięciu których rzetelność rośnie oznaczono '(!)'\n\n",
        sep = "")
  }

  invisible(list(trudnosc = trudnosci,
                 latwosc = 1 - trudnosci,
                 mocRoznicujaca = mocRoznicujaca,
                 rzetelnoscBZ = list(alfaC = alfyC$alfaBZ,
                                     alfaFR = alfyFR$alfaBZ)))
}
