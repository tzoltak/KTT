#' @title Rozklad trudnosci zadania w podziale na grupy
#' @description Funkcja rysuje wykres trudności zadania w ramach podanych grup.
#' @param x wektor liczb zawierający wyniki zadania
#' @param grupy wektor opisujący przynależność obserwacji do grup
#' @param nazwaGrupowania ciąg znaków - nazwa, która zostanie wyświetlona jako
#' etykieta osi poziomej na wykresie
#' @param oznaczMonotonicznosc wartość logiczna - czy ma być analizowana
#' monotoniczność trudności ze względu na podane grupowanie
#' @param maks opcjonalnie wektor liczb całkowitych opisujący maksymalną
#' liczbę puntków możliwych do uzyskania za poszczególne zadania
#' @param min opcjonalnie wektor liczb całkowitych opisujący minimalną
#' wartość, jaką może przyjąć wynik poszczególnych zadań
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @param verbose wartość logiczna - czy wydrukować wyniki analizy
#' @details
#' \bold{Grupowanie}
#'
#' Na potrzeby rysowania wykresu grupy zostaną posorotowane. Jeśli chce się
#' uzyskać kolejność prezentacji inną, niż wynikającą z posortowania unikalnych
#' wartości wektora podanego jako argument \code{grupy}, należy wcześniej
#' przerobić go na \link[base]{factor} zadając pożądaną kolejność jego poziomów.
#'
#' Jeśli argument \code{oznaczMonotonicznosc} przyjmuje wartość \code{TRUE},
#' przeanalizowana zostanie monotoniczność trudności ze względu na podział na
#' grupy. Dokładniej, oznaczone zostaną te grupy, w których trudność jest
#' większa, niż w poprzedniej. Dla pewnych pogrupowań, np. ze względu na wyniki
#' całego testu (w szczególności wyrażone jako stanin) ma to sens - zaburzenia
#' monotoniczności świadczą wtedy o problemie z zadaniem. Dla innych podziałów,
#' np. ze względu na płeć, typowo nie ma to sensu i wtedy należy wywołać funkcję
#' z argumentem \code{oznaczMonotonicznosc} ustawionym na \code{FALSE}.
#'
#' \bold{Maksymalne i minimalne wartości}
#'
#' Domyślnie, zgodnie z konwencją typową dla testów wiedzy/umiejętności,
#' jako minimalna możliwa do przyjęcia wartość wyniku zadania przyjmowane jest 0.
#' Maksymalna możliwa do przyjęcia wartość określana jest z kolei empirycznie,
#' jako największa wartość, która występuje w danych.
#'
#' Wykorzystanie argumentu \code{maks} jest konieczne, gdy ze względu na
#' niewielką liczbę obserwacji w zbiorze i/lub dużą trudność zadania dla
#' pewnych zadań w danych nie występuje maksymalna możliwa do osiągnięca liczba
#' punktów. Wartość przekazywana argumentem \code{maks} może być brakiem
#' danych - maksymalna możliwa do uzyskania wartość zostanie wtedy określona na
#' podstawie danych (j.w.).
#'
#' Wykorzystanie argumentu \code{min} jest konieczne w przypadku niektórych
#' schematów kodowania odpowiedzi na pytania np. w testach psychologicznych,
#' używających skal Likerta, czy dyferencjałów semantycznych. Często w takich
#' przypadkach odpowiedzi kodowane są kolejnymi liczbami naturalnymi, począwszy
#' od 1. Ogólnie rzecz biorąc, jeśli minimalną wartość, jaką może przyjąć wynik
#' danego zadania jest różna od 0, należy podać ją przy pomocy argumentu
#' \code{min}. Wartość przekazywana argumentem \code{min} może być brakiem
#' danych - jako minimalna możliwa do uzyskania wartość zostanie wtedy przyjęte 0.
#'
#' \bold{Obliczania łatwości/trudności dla zadań o minimalnej możliwej wartości różnej od 0}
#'
#' Jeśli minimalna możliwa do uzyskania wartość wyniku zadania jest różna od 0,
#' łatwość obliczona zostanie jako \code{E(X - min) / (max - min)}, gdzie
#' \code{X} oznacza wynik zadania, a \code{min} i \code{max} odpowiednio
#' najmniejszą i największą możliwą do uzyskania wartość wyniku zadania.
#' @seealso \code{\link{trudnosc}}
#' @return Funkcja zwraca milcząco wektor z oszacowaniami trudności w ramach grup.
#' @examples
#' grupy = normy_staninowe(wynikiSymTest)$wynikiStaninowe
#' wykres_twg(wynikiSymTest[, 1], grupy)
#' wykres_twg(wynikiSymTest[, 1], grupuj_staniny(grupy))
#' @export
#' @importFrom graphics barplot grid abline
#' @importFrom stats na.omit
wykres_twg = function(x, grupy, nazwaGrupowania = "grupa",
                      oznaczMonotonicznosc = TRUE,
                      maks = NA, min = NA, na.rm = TRUE, verbose = TRUE) {
  assert_w(x)
  if (!is.vector(grupy)) {
    stop("Argument 'grupy' musi być wektorem.")
  }
  if (length(x) != length(grupy)) {
    stop(paste0("Wektory podane argumentami 'x' i 'grupy' muszą być takiej ",
                "samej długości (składać się z takiej samej liczby elementów)."))
  }
  if (any(is.na(grupy))) {
    warning(paste0("Wektor opisujacy przydział do grup, podany argumentem ",
                   "'grupy' zawiera braki danych. Odpowiednie jednostki ",
                   "obserwacji nie zostaną uwzględnione w analizie."))
  }
  assert_t(nazwaGrupowania)
  stopifnot(oznaczMonotonicznosc %in% c(FALSE, TRUE), na.rm %in% c(FALSE, TRUE),
            verbose %in% c(FALSE, TRUE))
  stopifnot(length(oznaczMonotonicznosc) == 1, length(na.rm) == 1,
            length(verbose) == 1)
  if (is.na(maks)) {
    maks = as.numeric(maks)
  }
  maks = assert_maks(maks, matrix(x, ncol = 1))
  if (is.na(min)) {
    min = as.numeric(min)
  }
  min = assert_min(min, matrix(x, ncol = 1))

  gr = sort(unique(grupy))
  trudnosci = setNames(as.numeric(rep(NA, length(gr))), gr)
  for (i in 1:length(gr)) {
    trudnosci[i] =
      1 - (mean(x[grupy %in% gr[i]], na.rm = na.rm) - min) / (maks - min)
  }
  malejace = c(TRUE, trudnosci[-1] <= trudnosci[-length(trudnosci)])
  if (verbose) {
    cat("Oszacowanie trudności zadań w podziale na grupy:\n\n")
    temp = data.frame(grupa = names(trudnosci),
                      "trudność" = format(round(trudnosci, 2), nsmall = 2),
                      "łatwość" = format(round(1 - trudnosci, 2), nsmall = 2),
                      check.names = FALSE)
    if (oznaczMonotonicznosc) {
      temp$"zab.monot." = ifelse(malejace, "", "(!)       ")
    }
    print(temp, row.names = FALSE)
    cat("\n")
  }

  oldPar = par(no.readonly = TRUE)
  on.exit({par(oldPar)})
  par(mar = c(4, 5, 1, 1) + 0.1)
  if (oznaczMonotonicznosc) {
    kolory = ifelse(malejace, 3, 2)
  } else {
    kolory = 4
  }
  barplot(trudnosci, ylim = c(0, 1), col = kolory,
          xlab = nazwaGrupowania, ylab = "trudność")
  grid(nx = NA, ny = NULL, col = grey(0.5))

  attributes(trudnosci)$maks = maks
  attributes(trudnosci)$min = min
  invisible(trudnosci)
}
