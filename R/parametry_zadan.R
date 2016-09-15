#' @title Parametry zadan
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
#' \code{\link{alfa_c}}, \code{\link{wykres_tmr}}
#' @return
#' Funkcja zwraca milcząco listę z parametrami zadań.
#' @examples
#' parametry_zadan(wynikiSymTest)
#' @export
parametry_zadan = function(x, maks = NULL, min = NULL, na.rm = TRUE, verbose = TRUE) {
  stopifnot(na.rm %in% c(FALSE, TRUE), verbose %in% c(FALSE, TRUE))
  stopifnot(length(na.rm) == 1, length(verbose) == 1)

  if (is.null(maks) & "maks" %in% names(attributes(x))) {
    maks = attributes(x)$maks
  }
  if (is.null(min) & "min" %in% names(attributes(x))) {
    min = attributes(x)$min
  }
  trudnosci = trudnosc(x, na.rm = na.rm, verbose = FALSE)
  mocRoznicujaca = moc_roznicujaca(x, na.rm = na.rm, verbose = FALSE)
  alfyC = alfa_c(x, na.rm = na.rm, verbose = FALSE)
  alfyFR = alfa_fr(x, na.rm = na.rm, verbose = FALSE)

  cat("Parametry zadań:\n\n",
      info_macierz_danych(x), "\n\n",
      "rzetelność testu:\n",
      "alfa Cronbacha  = ", format(round(alfyC$alfa, 3), nsmall = 3), "\n",
      "alfa Feldt-Raju = ", format(round(alfyFR$alfa, 3), nsmall = 3),
      "\n\n", sep = "")
  if (verbose) {
    print(data.frame("zadanie" = colnames(x),
                     "min.pkt." = attributes(trudnosci)$min,
                     "maks.pkt." = attributes(trudnosci)$maks,
                     "trudność" = format(round(trudnosci, 2), nsmall = 2),
                     "łatwość" = format(round(1 - trudnosci, 2), nsmall = 2),
                     "MR-P" = format(round(mocRoznicujaca$korelacjePearsona, 3),
                                     nsmall = 3),
                     "MR-PBZ" = format(round(mocRoznicujaca$korelacjeBezZadania,
                                             3), nsmall = 3),
                     "MR-DS" = format(round(mocRoznicujaca$korelacjeDwuseryjne,
                                            3), nsmall = 3),
                     "rzetBZ-aC" = paste0(format(round(alfyC$alfaBZ, 3),
                                                 nsmall = 3),
                                          ifelse(alfyC$alfaBZ > alfyC$alfa &
                                                   !is.na(alfyC$alfaBZ),
                                                 " (!)", "    ")),
                     "rzetBZ-aFR" = paste0(format(round(alfyFR$alfaBZ, 3),
                                                  nsmall = 3),
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
                 rzetelnoscBZ = list(alfa_c = alfyC$alfaBZ,
                                     alfa_fr = alfyFR$alfaBZ)))
}
