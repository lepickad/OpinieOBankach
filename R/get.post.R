#' Pobieranie danych z Facebooka
#'
#' Pobiera dane dotyczące postów o numerach posts_id. Dla każdego posta pobierane
#' są informacje o poście, polubieniach i komentarzach. Funkcja używa wielu wątków.
#'
#' @param posts_id Wektor zawierający numery id postów do pobrania.
#' @param token Token, umożliwiający pobieranie danych z Facebooka.
#' @param il.watkow Liczba watków, na których będzie uruchamiona funkcja.
#'
#' @return Lista o długości równej długości posts_id. Każdy element jest listą
#' o 3 elementach:
#' * informacje o poście (autor, data utworzenia, id, liczba lików,
#'  liczba komentarzy, liczba udostępnień),
#' * ramka danych z nazwami i id osób, które polubiły post,
#' * ramka danych dotycząca komentarzy (autor, wiadomość, czas utworzenia, id).
#' page_name - nazwa strony
#'
#' @import Rfacebook parallel httr
#' @export

get.post <- function(posts_id, token, il.watkow) {

    klaster <- makeCluster(il.watkow)

    clusterExport(klaster, c("posts_id", "token"), envir = environment())

    parLapply(klaster, posts_id, function(post){
        require(Rfacebook)
        require(httr)
        set_config( config( ssl_verifypeer = 0L, ssl_verifyhost = 0L))
        getPost(post, token, n = 1000)

    }) -> raw_data

    stopCluster(klaster)


    raw_data
}
