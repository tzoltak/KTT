#' @title Generowanie raportu
#' @description Funkcja generuje raport zawierający wyniki analizy
#' psyhometrycznej testu.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param tytul tytuł raportu (ciąg znaków)
#' @param data opcjonalnie data (ciąg znaków)
#' @param maks opcjonalnie wektor liczb całkowitych opisujący maksymalną
#' liczbę puntków możliwych do uzyskania za poszczególne zadania
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @seealso \code{\link{parametry_zadan}}, \code{\link{wykres_lmr}}
#' @return
#' Funkcja nic nie zwraca.
#' @examples
#' raport(symTest)
#' @export
#' @importFrom rmarkdown render html_document
#' @importFrom knitr kable
raport = function(x, tytul = "Tytul raportu", data = NULL, maks = NULL,
                  na.rm = TRUE) {
  if (is.null(data)) {
    data = format(Sys.time(), "%d.%m.%Y")
  }
  trudnosci = trudnosc(x, na.rm = na.rm, verbose = FALSE)
  mocRoznicujaca = moc_roznicujaca(x, na.rm = na.rm, verbose = FALSE)
  alfyC = alfaC(x, na.rm = na.rm, verbose = FALSE)
  alfyFR = alfaFR(x, na.rm = na.rm, verbose = FALSE)

  lBD = sum(is.na(x))
  parametryZadan = data.frame(
    "zadanie" = colnames(x),
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
    check.names = FALSE
  )

  temp = c(
    '---',
    paste0('title: "', tytul, '"'),
    paste0('date: "', data, '"'),
    'output: html_document',
    '---',
    '# Charakterystyka testu',
    "",
    "## Dane",
    paste0("Test złożony z ", ncol(x), " zadań.  "),
    paste0(nrow(x), " obserwacji (zdających).  "),
    ifelse(lBD == 0, "W macierzy danych nie ma braków danych.",
           paste0("W macierzy danych ",
                  ifelse(lBD < 5 & lBD > 1, "są ", "jest "), lBD,
                  ifelse(lBD > 1,
                         ifelse(lBD < 5, " braki ", " braków "),
                         " brak "), "danych (",
                  format(100 * lBD / ncol(x) / nrow(x), digits = 3),
                  "% obserwacji).")),
    "",
    "## Rzetelność",
    paste0("alfa Cronbacha = ", format(alfyC$alfa, digits = 3, nsmall = 3), "  "),
    paste0("alfa Feldt-Raju = ", format(alfyFR$alfa, digits = 3, nsmall = 3)),
    "",
    "# Parametry zadań",
    "",
    kable(parametryZadan, row.names = FALSE, align = c("l", rep("r", 8))),
    "",
    "MR-P       - moc różnicująca: korelacja liniowa Pearsona (punktowo-dwuseryjna)  ",
    "MR-PBZ     - moc różnicująca: korelacja Pearsona z sumą punktów z wyłączeniem danego zadania  ",
    "MR-DS      - moc różnicująca: korelacja dwuseryjna  ",
    "rzetBZ-aC  - rzetelność bez zadania: alfa Cronbacha  ",
    "rzetBZ-aFR - rzetelność bez zadania: alfa Feldt-Raju  ",
    "Zadania, po usunięciu których rzetelność rośnie oznaczono '(!)'  ",
    "",
    "## Wykres trudność x moc różnicująca",
    "```{r, echo=FALSE}",
    "wykres_lmr(x)",
    "```",
    "",
    "Wskaźnik mocy różnicującej: korelacja Pearsona."
  )
  plik = file("raport.Rmd")
  writeLines(temp, plik)
  close(plik)
  render("raport.Rmd", html_document(), quiet = TRUE, clean = TRUE)
  unlink("raport.Rmd")

  invisible(NULL)
}
