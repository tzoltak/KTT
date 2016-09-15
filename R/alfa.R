#' @name alfa_c
#' @title Szacowanie rzetelnosci
#' @description Funkcje służące do oszacowania rzeteleności testu przy pomocy
#' współczynnika alfa Cronbacha lub alfa Feldt-Rahu.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @param verbose wartość logiczna - czy wydrukować wyniki analizy
#' @details
#' Funkcja \code{alfa_c} wykorzystuje współczynnik alfa Cronbacha - szerzej
#' rozpoznawalny i stosowany, ale oparty na modelu nakładającym na test bardziej
#' restrykcyjne założenia (taka sama siła związku pomiędzy mierzoną cechą,
#' a wynikiem każdego zadania). W efekcie alfa Cronbacha może nieco zaniżać
#' oszacowanie rzetelności testu.
#'
#' Funkcja  \code{alfa_fr} wykorzystuje współczynnik alfa Feldt-Raju,
#' o analogicznej interpretacji i podobnej konstrukcji, ale dopuszczający różną
#' siłę związku pomiędzy mierzoną cechą a wynikami każdego zadania. Jego użycie
#' jest bardziej adekwatne szczególnie w sytuacji, gdy w teście występuje duża
#' rozpiętość trudności zadań i/lub różne zadania mają różne skale oceny. Jego
#' wartość jest zawsze nie mniejsza niż wartość współczynnika alfa Cronbacha.
#' @seealso \code{\link{parametry_zadan}}, \code{\link{wykres_tmr}}
#' @return
#' Funkcje zwracają milcząco trzyelementową listę, której elementy zawierają:
#' \itemize{
#'    \item{\code{alfa} wartość współczynnika alfa Cronbacha/Feldt-Raju dla
#'          całego testu,}
#'    \item{\code{alfaBZ} wektor liczbowy zawierający współczynniki alfa
#'          Cronbacha/Feldt-Raju obliczone dla testu z wyłączeniem danego
#'          zadania,}
#'    \item{\code{wsp = ("alfa Cronbacha" | "alfa Feldt-Raju").}}
#' }
#' @examples
#' alfa_c(wynikiSymTest)
#' alfa_fr(wynikiSymTest)
NULL
#' @rdname alfa_c
#' @export
#' @importFrom stats var setNames
alfa_c = function(x, na.rm = TRUE, verbose = TRUE) {
  assert_mdfn(x)
  stopifnot(na.rm %in% c(FALSE, TRUE), verbose %in% c(FALSE, TRUE))
  stopifnot(length(na.rm) == 1, length(verbose) == 1)

  suma = rowSums(x, na.rm = na.rm)
  war = apply(x, 2, var, na.rm = na.rm)
  alfa = (ncol(x) / (ncol(x) - 1)) *
    (1 - sum(war, na.rm = na.rm) / var(suma, na.rm = na.rm))
  if (verbose) {
    cat("Oszacowanie rzetelności współczynnikiem alfa Cronbacha:\n\n",
        info_macierz_danych(x), "\n\n",
        "alfa Cronbacha = ", format(round(alfa, 3), nsmall = 3), "\n\n",
        sep = "")
  }

  if (ncol(x) > 2) {
    alfaBZ = setNames(as.numeric(rep(NA, ncol(x))), colnames(x))
    for (i in 1:ncol(x)) {
      alfaBZ[i] = ((ncol(x) - 1) / (ncol(x) - 2)) *
        (1 - sum(war[-i], na.rm = na.rm) / var(suma - x[, i], na.rm = na.rm))
    }
    if (verbose) {
      lZnMax = max(nchar(colnames(x)))
      cat("Oszacowania rzetelności bez poszczególnych zadań:\n",
          paste0("  ", format(colnames(x), width = lZnMax, justify = "right"),
                 "  ", format(round(alfaBZ, 3), nsmall = 3),
                 ifelse(alfaBZ > alfa & !is.na(alfaBZ), " (!)", ""), "\n",
                 collapse = ""),
          "Zadania, po usunięciu których rzetelność rośnie oznaczono '(!)'\n\n",
          sep = "")
    }
  } else {
    alfaBZ = NULL
    if (verbose) {
      cat("Oszacowania rzetelności bez poszczególnych zadań niemożliwe -",
          "test zawiera tylko dwa zadania.\n\n")
    }
  }


  wynik = list(alfa = alfa, alfaBZ = alfaBZ, wsp = "alfa Cronbacha")
  class(wynik) = c(class(wynik), "oszacowanieRzetelnosci")
  invisible(wynik)
}
#' @rdname alfa_c
#' @export
#' @importFrom stats var cov setNames
alfa_fr = function(x, na.rm = TRUE, verbose = TRUE) {
  assert_mdfn(x)
  stopifnot(na.rm %in% c(FALSE, TRUE), verbose %in% c(FALSE, TRUE))

  suma = rowSums(x, na.rm = na.rm)
  warSuma = var(suma, na.rm = na.rm)
  war = apply(x, 2, var, na.rm = na.rm)
  lambda = apply(x, 2, cov, y = suma,
                 use = ifelse(na.rm, "complete.obs", "everything"))
  lambda = lambda / warSuma
  alfa = (1 / (1 - sum(lambda^2, na.rm = na.rm))) * (1 - sum(war) / warSuma)
  if (verbose) {
    cat("Oszacowanie rzetelności współczynnikiem alfa Feldt-Raju:\n\n",
        info_macierz_danych(x), "\n\n",
        "alfa Feldt-Raju = ", format(round(alfa, 3), nsmall = 3), "\n\n",
        sep = "")
  }

  if (ncol(x) > 2) {
    alfaBZ = setNames(as.numeric(rep(NA, ncol(x))), colnames(x))
    for (i in 1:ncol(x)) {
      warSuma = var(suma - x[, i], na.rm = na.rm)
      lambda = apply(x[, -i], 2, cov, y = suma - x[, i],
                         use = ifelse(na.rm, "complete.obs", "everything"))
      lambda = lambda / warSuma
      alfaBZ[i] = (1 / (1 - sum(lambda^2, na.rm = na.rm))) *
        (1 - sum(war[-i]) / warSuma)
    }
    if (verbose) {
      lZnMax = max(nchar(colnames(x)))
      cat("Oszacowania rzetelności bez poszczególnych zadań:\n",
          paste0("  ", format(colnames(x), width = lZnMax, justify = "right"),
                 "  ", format(round(alfaBZ, 3), nsmall = 3),
                 ifelse(alfaBZ > alfa & !is.na(alfaBZ), " (!)", ""), "\n",
                 collapse = ""),
          "Zadania, po usunięciu których rzetelność rośnie oznaczono '(!)'\n\n",
          sep = "")
    }
  } else {
    alfaBZ = NULL
    if (verbose) {
      cat("Oszacowania rzetelności bez poszczególnych zadań niemożliwe -",
          "test zawiera tylko dwa zadania.\n\n")
    }
  }


  wynik = list(alfa = alfa, alfaBZ = alfaBZ, wsp = "alfa Feldt-Raju")
  class(wynik) = c(class(wynik), "oszacowanieRzetelnosci")
  invisible(wynik)
}
