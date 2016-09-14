#' @title Punktowanie odpowiedzi udzielonych w zadaniach zamkniętych
#' @description Funkcja służy do przypisania punktacji uzyskanej za zadania
#' zamknięte na podstawie podanego klucza odpowiedzi.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param opisTestu ramka danych zawierająca opis struktury testu
#' @param verbose wartość logiczna - czy wydrukować wyniki analizy
#' @details
#' \bold{Reguły punktowania}
#'
#' Dla danego zadania \emph{zamkniętego} (p. sekcja \emph{Opis struktury
#' testu}) odpowiedziom równym wartości podanej w opisie struktury testu
#' odpowiedzi poprawnej przypisana zostanie wartość 1. Wszystkim pozostałym
#' odpowiedziom, \bold{w tym brakom danych}, przypisana zostanie wartość 0.
#'
#' \bold{Opis struktury testu}
#' Opis struktury testu podawany jest funkcji w postaci ramki danych, która
#' musi zawierać następujące kolumny
#' \itemize{
#'   \item{\code{zmienna}: nazwy zmiennych w zbiorze danych;}
#'   \item{\code{typ}: typ zadania (zmiennej w zbiorze):
#'         \itemize{
#'           \item{brak danych lub pusty ciąg znaków - zmienna, która nie
#'                 opisuje odpowiedzi na zadanie testowe (np. identyfikator
#'                 zdającego, zmienne opisujące jego cechy, które znajdują się
#'                 w tym samym zbiorze co odpowiedzi na zadania itp.);}
#'           \item{'o' - zadanie \emph{otwarte}, rozumiane tutaj jako takie
#'                 zadanie, które nie musi zostać zapunktowane na podstawie
#'                 klucza (\bold{w szczególności zadania zamknięte, które
#'                 zostały zapunktowane już wcześniej, należy opisać właśnie
#'                 w ten sposób});}
#'           \item{'z' - zadanie \emph{zamknięte}, rozumiane tutaj jako takie
#'                 zadanie, które dopiero musi zostać zapunktowane na podstawie
#'                 klucza;}
#'         }}
#'   \item{\code{odpowiedzi} - zestaw możliwych odpowiedzi (tj. wartości, które
#'         może przyjąć zmienna zawierająca odpowiedzi na dane zadanie); może
#'         zostać opisany w następujący sposób:
#'         \itemize{
#'           \item{brak danych lub pusty ciąg znaków - należy przypisać, jeśli
#'                 dla danej zmiennej ma nie być sprawdzana poprawność wartości
#'                 w zbiorze (np. nie ma to sensu, bo jest to zmienna ciągła,
#'                 albo nie jesteśmy w stania - lub nie chce nam się wypisywać -
#'                 zestawu wszystkich możliwych do przyjęcia wartości);}
#'           \item{ciąg znaków zawierający poszczególne wartości, oddzielone od
#'                 siebie przecinkami, np. \code{'0,1'}, lub \code{'tak,nie'};}
#'           \item{ciąg znaków postaci \code{'liczba1-liczba2'}, co oznacza, że
#'                 zmienna może przyjmować jako wartości wszystkie liczby
#'                 całkowite z przedziału \code{[liczba1; liczba2]};}
#'           \item{pojedyncza liczba (podana albo jako liczba, albo w formie
#'                 ciągu znaków), co jest skróconą (równoważną) formą zapisu:
#'                 \code{'0-liczba'};}
#'         }
#'         W przypadku zadań typu 'o' jako dopouszczalne wartości akceptowane są
#'         tylko liczby (z tym że mogą to być liczby niecałkowite).}
#'   \item{\code{poprawna_odpowiedz} - ciąg znaków (lub ew. liczba), który
#'         koduje poprawną odpowiedź na zadanie.}
#' }
#' Dodatkowo opis struktury testu może (ale nie musi) zawierać kolumny:
#' \itemize{
#'   \item{\code{etykieta} - ciąg znaków z etykietą (tytułem/krótkim opisem)
#'         zadania;}
#'   \item{\code{uwagi} - ciąg znaków zawierający ew. uwagi (lub np. opis
#'         treści zadania).}
#' }
#' @seealso \code{\link{raport}}
#' @return
#' Funkcja zwraca ramkę danych (data frame) z wynikami rozwiązania zadań
#' (zadania 'o' w takiej formie, w jakiej były w wejściowych danych, zadania 'z'
#' zapunktowane).
#'
#' Zwracana ramka danych nie zawiera żadnych innych kolumn, niż te z punktacją
#' za zadania, ale zawiera te same wiersze, ułożone w tej samej kolejności, co
#' ramka danych z danymi wejściowymi przekazana do funkcji argumentem \code{x}.
#' Dane z obu ramek można więc łatwo łączyć, używając funkcji
#' \code{\link[base]{cbind}}.
#' @examples
#' summary(daneWzrost)
#' wynikiWzrost = przypisz_punktacje(daneWzrost, opisTestuWzrost)
#' summary(wynikiWzrost)
#' @export
#' @importFrom stats var cor dnorm qnorm setNames
przypisz_punktacje = function(x, opisTestu, verbose = TRUE) {
  opisTestu = assert_ot(x, opisTestu)
  stopifnot(verbose %in% c(TRUE, FALSE))
  stopifnot(length(verbose) == 1)

  opisTestu = opisTestu[opisTestu$typ != "", ]
  x = x[, opisTestu$zmienna]
  if (verbose) {
    cat("Zbiór zadań przed przypisaniem punktacji za zadania zamknięte:\n\n",
        info_macierz_danych(x), "\n\n", sep = "")
  }
  maskaP = opisTestu$poprawna_odpowiedz != "" & opisTestu$typ == "z"
  for (i in which(maskaP)) {
    x[, opisTestu$zmienna[i]] =
      ifelse(x[, opisTestu$zmienna[i]] %in% opisTestu$poprawna_odpowiedz[i],
             1, 0)
  }

  odpowiedzi = setNames(opisTestu$odpowiedzi, opisTestu$zmienna)
  min = sapply(odpowiedzi,
               function(x) {return(min(suppressWarnings(as.numeric(x))))})
  maks = sapply(odpowiedzi,
               function(x) {return(max(suppressWarnings(as.numeric(x))))})
  min = ifelse(maskaP, 0, min)
  maks = ifelse(maskaP, 1, maks)

  if (verbose) {
    cat("Przypisano punktację w ", sum(maskaP), " zadaniach zamkniętych.\n\n",
        "Zapunktowany zbiór zadań:\n\n", sep = "")
    trudnosc(x, maks, min)
  }

  attributes(x)$odpowiedzi = odpowiedzi
  attributes(x)$min = min
  attributes(x)$maks = maks
  return(x)
}
