#' @title Generowanie raportu
#' @description Funkcja generuje raport zawierający wyniki analizy
#' psyhometrycznej testu. Zostanie on zapisany w aktywnym katalogu
#' (p. \code{\link[base]{setwd}}).
#' @param x ramka danych (data frame) zawierająca wyniki testu
#' @param opisTestu opcjonalnie ramka danych zawierająca opis struktury testu
#' (p. \code{\link{przypisz_punktacje}})
#' @param format definiuje format pliku, w którym wygenerowany zostanie raport:
#' jest to wektor składający się z jednej lub kilku spośród trzech wartości:
#' 'html' (plik HTML), 'docx' (plik programu MS Word), 'odt' (plik programu
#' Open Office/Libre Office)
#' @param tytul tytuł raportu (ciąg znaków)
#' @param data opcjonalnie data (ciąg znaków)
#' @param maks opcjonalnie wektor liczb całkowitych opisujący maksymalną
#' liczbę puntków możliwych do uzyskania za poszczególne zadania
#' @param min opcjonalnie wektor liczb całkowitych opisujący minimalną
#' wartość, jaką może przyjąć wynik poszczególnych zadań
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @seealso \code{\link{przypisz_punktacje}}, \code{\link{parametry_zadan}},
#' \code{\link{wykres_tmr}}, \code{\link{normy_staninowe}},
#' \code{\link{wykres_rs}}, \code{\link{wykres_rd}}, \code{\link{wykres_rwz}},
#' \code{\link{wykres_twg}}#'
#' @return
#' Funkcja nic nie zwraca.
#'
#' Efektem działania funkcji jest wygenerowanie pliku/plików z raportem
#' w aktywnym katalogu.
#' @examples
#'\dontrun{
#' raport(wynikiSymTest)
#' raport(daneEWD, opisTestuEWD)
#' }
#' @export
#' @importFrom rmarkdown render html_document word_document odt_document
#' @importFrom knitr kable
raport = function(x, opisTestu = NULL, format = "html",
                  tytul = "Analiza psychometryczna testu", data = NULL,
                  maks = NULL, min = NULL, na.rm = TRUE) {
  dec = options()$OutDec
  options(OutDec = ",")
  on.exit(options(OutDec = dec))
  if (is.null(opisTestu)) {
    message(paste0("Informacja:\n\n",
                   "Nie podano argumentu 'opisTestu'. Oznacza to, że:\n",
                   "- Argument 'x' musi zawierać tylko zmienne opisujące ",
                   "punktację uzyskaną za poszczególne zadania.\n",
                   "- Niemożliwa jest analiza częstości wybierania dystraktorów."))
    if (is.null(maks) & is.null(min)) {
      message(paste0("- Zestaw możliwych do przyjęcia wartości zostanie dla ",
                     "każdego zadania określony na podstawie danych ",
                     "(aby dowiedzieć się więcej przeczytaj dokumentację funkcji ",
                     "trudnosc(), wpisując w konsolę: '?trudnosc').\n"))
    }
    assert_mdfn(x)
    maks = assert_maks(maks, x)
    min = assert_min(min, x)
    odpowiedzi =
      lapply(x, function(x) {return(setdiff(as.character(sort(unique(x))),
                                            c(NA, "")))})
    opisTestu = data.frame(zmienna = colnames(x), typ = rep("", ncol(x)),
                           etykieta = rep("", ncol(x)), uwagi = rep("", ncol(x)),
                           odpowiedzi = rep("", ncol(x)),
                           poprawna_odpowiedz = rep("", ncol(x)))
  } else {
    y = x
    x = przypisz_punktacje(x, opisTestu)
    y = y[, names(x)]
    opisTestu = opisTestu[opisTestu$zmienna %in% names(x), ]
    if (!is.null(maks) | !is.null(min)) {
      warning(paste0("Podano argument 'opisTestu' - minimalne i maksymalne ",
                     "możliwe do uzyskania wartości punktów w poszczególnych ",
                     "zadaniach zostaną określone na jego podstawie, ",
                     "z pominięciem argumentów 'maks' i/lub 'min'."))
    }
    maks = attributes(x)$maks
    min = attributes(x)$min
    odpowiedzi = attributes(x)$odpowiedzi
  }
  if (is.null(data)) {
    data = format(Sys.time(), "%d.%m.%Y")
  }
  if (length(format) == 0) {
    stop("Argument 'format' ma zerową długość.")
  }
  if (!all(format %in% c("html", "docx", "odt"))) {
    stop(paste0("Argument 'format' może zawierać tylko następujące wartości: ",
                "'html', 'docx' lub 'odt'."))
  }
  assert_t(tytul)
  assert_t(data)
  stopifnot(na.rm %in% c(FALSE, TRUE))
  stopifnot(length(na.rm) == 1)
  if (file.access("./", c(2,4)) != 0) {
    stop(paste0("Brak praw do zapisu w aktywnym folderze:\n'",
                getwd(), "'\nZmień aktywny folder (funkcją setwd()) na inny ",
                "lub zmień prawa dostępu do obecnego folderu."))
  }

  cat("Trwa obliczanie statystyk...")
  trudnosci = trudnosc(x, na.rm = na.rm, verbose = FALSE)
  mocRoznicujaca = moc_roznicujaca(x, na.rm = na.rm, verbose = FALSE)
  alfyC = alfa_c(x, na.rm = na.rm, verbose = FALSE)
  alfyFR = alfa_fr(x, na.rm = na.rm, verbose = FALSE)

  suma = rowSums(x, na.rm = na.rm)
  sr = mean(suma)
  oS = sd(suma)
  kwantyle = quantile(suma, seq(0, 1, by = 0.25), na.rm = na.rm)
  parRS = data.frame(
    "par. poziomu wartości" = c("minimum", "1. kwartyl", "mediana", "3. kwartyl",
                                "maksimum", "średnia"),
    "wartość" = c(format(round(kwantyle, 1), nsmall = 1),
                  format(round(sr, 1), nsmall = 1)),
    "par. rozproszenia" = c("rozstęp", "odch. ćwiartkowe", "odch. standardowe",
                            rep("", 3)),
    "wartość" = c(format(round(kwantyle[5] - kwantyle[1], 1), nsmall = 1),
                  format(round(c((kwantyle[4] - kwantyle[2]) / 2, oS), 1),
                         nsmall = 1),
                  "", "", ""),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  normyStaninowe = normy_staninowe(x, verbose = FALSE)
  grupyStanin = grupuj_staniny(normyStaninowe$wynikiStaninowe)
  rozkladStaninow = as.vector(table(factor(normyStaninowe$wynikiStaninowe, 1:9)))
  rozkladTeoretyczny = c(pnorm(seq(1.5, 8.5, by = 1), 5, 2), 1) -
    c(0, pnorm(seq(1.5, 8.5, by = 1), 5, 2))
  normyStaninowe = with(normyStaninowe$normyStaninowe, data.frame(
    stanin = get("stanin"),
    "l. punktów" = paste0(get("min_pkt"), " - ", get("maks_pkt")),
    "l. zdających" = rozkladStaninow,
    "ods. zdających" = paste0(format(round(
      100 * rozkladStaninow / sum(rozkladStaninow), 1),
      nsmall = 1), " %"),
    "ods. teoretyczny" = paste0(format(round(
      100 * rozkladTeoretyczny, 1),
      nsmall = 1), " %"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  normyStaninowe[, 2] = sub("NA - NA", "ndt.", normyStaninowe[, 2])

  lBD = sum(is.na(x))
  parametryZadan = data.frame(
    "zadanie" = colnames(x),
    "min." = attributes(trudnosci)$min,
    "maks." = attributes(trudnosci)$maks,
    "trudność" = format(round(trudnosci, 2), nsmall = 2),
    "łatwość" = format(round(1 - trudnosci, 2), nsmall = 2),
    "MR-P" = format(round(mocRoznicujaca$korelacjePearsona, 3), nsmall = 3),
    "MR-PBZ" = format(round(mocRoznicujaca$korelacjeBezZadania, 3), nsmall = 3),
    "MR-DS" = format(round(mocRoznicujaca$korelacjeDwuseryjne, 3), nsmall = 3),
    "rzet. BZ-aC" = paste0(format(round(alfyC$alfaBZ, 3), nsmall = 3),
                           ifelse(alfyC$alfaBZ > alfyC$alfa &
                                    !is.na(alfyC$alfaBZ),
                                  " (!)", "    ")),
    "rzet. BZ-aFR" = paste0(format(round(alfyFR$alfaBZ, 3), nsmall = 3),
                            ifelse(alfyFR$alfaBZ > alfyFR$alfa &
                                     !is.na(alfyFR$alfaBZ),
                                   " (!)", "    ")),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  temp = c(
    '---',
    paste0('title: "', tytul, '"'),
    paste0('date: "', data, '"'),
    'output: html_document',
    '---',
    '# 1. Charakterystyka testu',
    "",
    "## 1.1. Dane",
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
    "")
  if (lBD > 0) {
    zadBD = 100 * colMeans(is.na(x))
    zadBD = zadBD[zadBD > 0]
    zadBD = zadBD[order(-zadBD)]
    temp = c(
      temp,
      "#### Zmienne z brakami danych w kolejności wg częstości ich występowania:",
      kable(data.frame(
      zmienna = names(zadBD),
      "ods. braków danych" = paste0(format(round(zadBD, 1), nsmall = 1)), " %"),
      check.names = FALSE),
      ""
    )
  }
  temp = c(
    temp,
    "## 1.2. Rzetelność",
    paste0("alfa Cronbacha = ", format(alfyC$alfa, digits = 3, nsmall = 3), "  "),
    paste0("alfa Feldt-Raju = ", format(alfyFR$alfa, digits = 3, nsmall = 3)),
    "",
    "## 1.3. Rozkład sumy punktów",
    "",
    kable(parRS, row.names = FALSE, align = c("l", "r", "l", "r")),
    "",
    "```{r, echo=FALSE, fig.width=6.7}",
    "wykres_rs(x, verbose = FALSE)",
    "```",
    "",
    "Pionowa niebieska linia wyznacza średnią.  ",
    "Czarna przerywana obwiednia obrazuje rozkład normalny o takiej samej średniej i dochyleniu standardowym.  ",
    "Pionowe linie w dolnej części wykresy (czerwone, czarne i zielone) obrazują podział na staniny.",
    "",
    "### Normy staninowe",
    "",
    kable(normyStaninowe, row.names = FALSE, align = c("c", "c", rep("r", 3))),
    "",
    "## 1.4. Parametry zadań",
    "",
    kable(parametryZadan, row.names = FALSE, align = c("l", rep("r", 9))),
    "",
    "min.       - minimalna możliwa do uzyskania liczba punktów  ",
    "maks.      - maksymalna możliwa do uzyskania liczba puktów  ",
    "MR-P       - moc różnicująca: korelacja liniowa Pearsona (punktowo-dwuseryjna)  ",
    "MR-PBZ     - moc różnicująca: korelacja Pearsona z sumą punktów z wyłączeniem danego zadania  ",
    "MR-DS      - moc różnicująca: korelacja dwuseryjna  ",
    "rzetBZ-aC  - rzetelność bez zadania: alfa Cronbacha  ",
    "rzetBZ-aFR - rzetelność bez zadania: alfa Feldt-Raju  ",
    "Zadania, po usunięciu których rzetelność rośnie oznaczono '(!)'  ",
    "",
    "### Wykres trudność x moc różnicująca",
    "```{r, echo=FALSE, fig.width=6.7}",
    "wykres_tmr(x)",
    "```",
    "",
    "Wskaźnik mocy różnicującej: korelacja Pearsona.  ",
    "Ew. niebieska obwiednia wyznacza maksymalne możliwe do uzyskania wartości korelacji dla zadań ocenianych binarnie (i przy normalnym rozkładzie sumy punktów).",
    "",
    "***",
    "",
    "# 2. Charakterystyka poszczególnych zadań",
    ""
  )
  for (i in 1:ncol(x)) {
    assign(paste0("pkt", colnames(x)[i]), x[, i])
    assign(paste0("maks", colnames(x)[i]), maks[i])
    assign(paste0("min", colnames(x)[i]), min[i])
    temp = c(
      temp,
      "***",
      "",
      paste0("## Zadanie ", colnames(x)[i]),
      "",
      paste0("**", opisTestu$etykieta[i], "**  "),
      "",
      ifelse(opisTestu$poprawna_odpowiedz[i] != "",
             paste0("**Możliwe odpowiedzi:** ",
                    paste0(odpowiedzi[[i]], collapse = ", "), "  "),
             paste0("**Punktacja:** ",
                    paste0(odpowiedzi[[i]], collapse = ", "), "  ")),
      ifelse(opisTestu$poprawna_odpowiedz[i] != "",
             paste0("**Poprawna odpowiedź:** ", opisTestu$poprawna_odpowiedz[i], "  "),
             ""),
      ifelse(!(opisTestu$uwagi[i] %in% c(NA, "")),
             paste0(opisTestu$uwagi[i], "  "),
             ""),
      "",
      ifelse(any(grepl("[!]", parametryZadan[i, -1])),
             "**Po usunięciu zadania rośnie rzetelność testu!**", ""),
      "",
      kable(parametryZadan[i, -1], row.names = FALSE, align = c("l", rep("r", 9))),
      "",
      "#### Trudność w grupach wyróżnionych na podstawie wyników całego testu",
      "",
      "```{r, echo=FALSE, fig.width=4, fig.height=3}",
      paste0("wykres_twg(", paste0("pkt", colnames(x)[i]),
             ", grupyStanin, 'grupa staninowa', TRUE, ",
             paste0("maks", colnames(x)[i]), ", ",
             paste0("min", colnames(x)[i]), ", verbose = FALSE)"),
      "```",
      ""
    )
    if (opisTestu$typ[i] == "z" & length(odpowiedzi[[i]]) > 2) {
      assign(paste0("odp", colnames(x)[i]), y[, i])
      assign(paste0("poprOdp", colnames(x)[i]), opisTestu$poprawna_odpowiedz[i])
      assign(paste0("mozlOdp", colnames(x)[i]), odpowiedzi[[i]])
      temp = c(
        temp,
        "#### Rozkład wyboru dystraktorów",
        "",
        "```{r, echo=FALSE, fig.width=4, fig.height=3}",
        paste0("wykres_rd(", paste0("odp", colnames(x)[i]), ", ",
               paste0("poprOdp", colnames(x)[i]), ", ",
               paste0("mozlOdp", colnames(x)[i]), ", verbose = FALSE)"),
        "```",
        ""
      )
    } else if (length(odpowiedzi[[i]]) > 2) {
      assign(paste0("odp", colnames(x)[i]), x[, i])
      assign(paste0("mozlOdp", colnames(x)[i]), odpowiedzi[[i]])
      temp = c(
        temp,
        "#### Rozkład punktacji za zadanie",
        "",
        "```{r, echo=FALSE, fig.width=5, fig.height=3}",
        paste0("wykres_rwz(", paste0("odp", colnames(x)[i]), ", ",
               paste0("mozlOdp", colnames(x)[i]), ", verbose = FALSE)"),
        "```",
        ""
      )
    }
  }

  plik = file("raport.Rmd")
  writeLines(temp, plik)
  close(plik)
  cat(" zakończone.\nTrwa generowanie raportów...\n")
  if ("html" %in% format) {
    render("raport.Rmd", html_document(), quiet = TRUE, clean = TRUE)
    cat("  raport został zapisany w pliku: '", getwd(), "/raport.html'\n", sep = "")
  }
  if ("docx" %in% format) {
    render("raport.Rmd", word_document(), quiet = TRUE, clean = TRUE)
    cat("  raport został zapisany w pliku: '", getwd(), "/raport.docx'\n", sep = "")
  }
  if ("odt" %in% format) {
    render("raport.Rmd", odt_document(), quiet = TRUE, clean = TRUE)
    cat("  raport został zapisany w pliku: '", getwd(), "/raport.odt'\n", sep = "")
  }
  unlink("raport.Rmd")

  invisible(NULL)
}
