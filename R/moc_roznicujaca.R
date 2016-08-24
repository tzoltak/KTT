#' @title Moc różnicująca
#' @description Funkcja służy do oszacowania mocy różnicującej zadania.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @param verbose wartość logiczna - czy wydrukować wyniki analizy
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
#' moc_roznicujaca(symTest)
#' @export
#' @importFrom stats var cor dnorm qnorm setNames
moc_roznicujaca = function(x, na.rm = TRUE, verbose = TRUE) {
  assert_mdfn(x)
  stopifnot(na.rm %in% c(FALSE, TRUE), verbose %in% c(FALSE, TRUE))

  suma = rowSums(x, na.rm = na.rm)
  korP = apply(x, 2, cor, y = suma,
               use = ifelse(na.rm, "complete.obs", "everything"))
  korP = setNames(korP, colnames(x))
  maska = apply(x, 2, function(x) {
    return(all(x %in% c(0, 1, NA)) & all(c(0, 1) %in% x))})
  p = ifelse(maska,
             colMeans(x, na.rm = TRUE),
             NA)
  korDS = ifelse(maska,
                 korP * sqrt(p * (1 - p)) / dnorm(qnorm(max(c(p, 1 - p)))),
                 NA)
  korDS = setNames(korDS, colnames(x))
  if (any(korDS > 1)) {
    warning(paste0("Uzyskano korelacje dwuseryjne większe od jedności.\n  ",
                   "Analiza wartości korelacji dwuseryjnych zapewne nie ma sensu.\n  ",
                   "Być może przyczyną jest to, że rozkład sumy punktów ",
                   "znacznie odbiega od rozkładu normalnego."))
    korDS[korDS > 1 & !is.na(korDS)] = 1
  }
  korBZ = setNames(as.numeric(rep(NA, ncol(x))), colnames(x))
  if (ncol(x) > 1) {
    for (i in 1:ncol(x)) {
      korBZ[i] = cor(x[, i], suma - x[, i],
                     use = ifelse(na.rm, "complete.obs", "everything"))
    }
  }

  if (verbose) {
    cat("Oszacowanie mocy różnicujące zadań:\n\n",
        "test złożony z ", ncol(x), " zadań\n\n", sep = "")
    print(data.frame(zadanie = colnames(x), "Pearson" = korP,
                     "bez zadania" = korBZ, "dwuseryjna" = korDS,
                     check.names = FALSE), row.names = FALSE, digits = 3)
    cat("\nPearson   - korelacja liniowa Pearsona (punktowo-dwuseryjna)\n",
        "bez zadania - korelacja Pearsona z sumą punktów z wyłączeniem danego zadania\n",
        "dwuseryjna  - korelacja dwuseryjna\n\n", sep = "")
  }

  invisible(list(korelacjePearsona = korP,
              korelacjeDwuseryjne = korDS,
              korelacjeBezZadania = korBZ))
}
