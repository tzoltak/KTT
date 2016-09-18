#' @title Moc roznicujaca
#' @description Funkcja służy do oszacowania mocy różnicującej zadań.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @param verbose wartość logiczna - czy wydrukować wyniki analizy
#' @details
#' \itemize{
#'   \item{\code{korelacja liniowa Pearsona (punktowo-duseryjna)}:
#'         dla zadań z wąskimi skalami oceny (w szczególności ocenianych
#'         binarnie: 0 lub 1) w praktyce niemożliwe jest osiągnięcie korelacji
#'         bliskich jedności; maksymalna możliwa do osiągnięcia korelacja jest
#'         tym mniejsza, im bardziej trudność/łatwość zadania odbiega od 0.5.}
#'   \item{\code{korelacja dwuseryjna}: adekwatna tylko do zadań ocenianych
#'         binarnie (0 lub 1) - dla innych nie jest tu obliczana; jest zawsze
#'         większa (co do wartości bezwzględnej) od korelacji Pearsona, ale
#'         jeśli rozkład sumy punktów wyraźnie różni się od rozkładu normalnego,
#'         może przeszacowywać natężenie związku (w tym przyjmować bezsensowne
#'         wartości większe niż 1);}
#'   \item{\code{korelacja Pearsona bez zadania} - korelacja liniowa Pearsona
#'         (j.w.), ale z sumą punktów pomniejszoną o liczbę punktów uzyskanych
#'         za dane zadanie; bardziej adekwatna od zwykłej korelacji Pearsona dla
#'         krótkich testów lub w sytuacji, gdy dane zadanie ma dużo szerszą
#'         skalę oceny niż pozostałe;}
#' }
#' @seealso \code{\link{parametry_zadan}}, \code{\link{wykres_tmr}}
#' @return
#' Funkcja zwraca milcząco dwuelementową listę, której elementy zawierają:
#' \itemize{
#'    \item{\code{korelacjePearsona} wartości współczynników korelacji Pearsona
#'          (punktowo-dwuseryjnych) pomiędzy wynikiem całego testu, a wynikami
#'          poszczególnych zadań,}
#'    \item{\code{korelacjeDwuseryjne} wartości współczynników korelacji
#'          dwuseryjnych pomiędzy wynikiem całego testu, a wynikami
#'          poszczególnych zadań.}
#' }
#' @examples
#' moc_roznicujaca(wynikiSymTest)
#' @export
#' @importFrom stats var cor dnorm qnorm setNames
moc_roznicujaca = function(x, na.rm = TRUE, verbose = TRUE) {
  assert_mdfn(x)
  if ("tbl_df" %in% class(x)) {
    x = as.data.frame(x)
  }
  stopifnot(na.rm %in% c(FALSE, TRUE), verbose %in% c(FALSE, TRUE))
  stopifnot(length(na.rm) == 1, length(verbose) == 1)

  suma = rowSums(x, na.rm = na.rm)
  korP = apply(x, 2, function(x, y, na.rm) {
    if (var(x, na.rm = TRUE) == 0 | var(y, na.rm = TRUE) == 0) {
      return(NaN)
    }
    else {
      return(cor(x, y, use = ifelse(na.rm, "complete.obs", "everything")))
    }
  }, y = suma, na.rm = na.rm)
  korP = setNames(korP, colnames(x))
  if (any(is.nan(korP))) {
    warning(paste0("Obliczenie mocy różnicującej dla zadań:\n  '",
                   paste0(names(korP)[is.nan(korP)], collapse = "',\n  '"),
                   "'\nbyło niemożliwe ze względu na brak zróżnicowania punktacji."))
    korP[is.nan(korP)] = NA
  }
  maska = apply(x, 2, function(x) {
    return(all(x %in% c(0, 1, NA)) & all(c(0, 1) %in% x))})
  p = ifelse(maska,
             colMeans(x, na.rm = TRUE),
             NA)
  korDS = ifelse(maska,
                 korP * sqrt(p * (1 - p)) / dnorm(qnorm(ifelse(p > 0.5, p, 1 - p))),
                 NA)
  korDS = setNames(korDS, colnames(x))
  if (any(korDS > 1 & !is.na(korDS))) {
    warning(paste0("Uzyskano korelacje dwuseryjne większe od jedności.\n  ",
                   "Analiza wartości korelacji dwuseryjnych zapewne nie ma sensu.\n  ",
                   "Być może przyczyną jest to, że rozkład sumy punktów ",
                   "znacznie odbiega od rozkładu normalnego."))
    korDS[korDS > 1 & !is.na(korDS)] = 1
  }
  korBZ = setNames(as.numeric(rep(NA, ncol(x))), colnames(x))
  if (ncol(x) > 1) {
    for (i in 1:ncol(x)) {
      if (var(x[, i]) != 0 & var(suma - x[, i]) != 0) {
        korBZ[i] = cor(x[, i], suma - x[, i],
                       use = ifelse(na.rm, "complete.obs", "everything"))
      }
    }
  }

  if (verbose) {
    cat("Oszacowanie mocy różnicujące zadań:\n\n",
        info_macierz_danych(x), "\n\n", sep = "")
    print(data.frame(zadanie = colnames(x),
                     "Pearson" = format(round(korP, 3), nsmall = 3),
                     "bez zadania" = format(round(korBZ, 3), nsmall = 3),
                     "dwuseryjna" = format(round(korDS, 3), nsmall = 3),
                     check.names = FALSE), row.names = FALSE)
    cat("\nPearson   - korelacja liniowa Pearsona (punktowo-dwuseryjna)\n",
        "bez zadania - korelacja Pearsona z sumą punktów z wyłączeniem danego zadania\n",
        "dwuseryjna  - korelacja dwuseryjna\n\n", sep = "")
  }

  invisible(list(korelacjePearsona = korP,
              korelacjeDwuseryjne = korDS,
              korelacjeBezZadania = korBZ))
}
