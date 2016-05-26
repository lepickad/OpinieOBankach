#' Lączy post i komentarze w jedną ramkę
#'
#' Służy do przetworzenia wyniku działania funkcji getPost z pakietu Rfacebook
#' do ramki danych.
#'
#' @param element Lista o długości 3 o elementach:
#' * informacje o poście (autor, data utworzenia, id, liczba lików,
#'  liczba komentarzy, liczba udostępnień),
#' * ramka danych z nazwami i id osób, które polubiły post,
#' * ramka danych dotycząca komentarzy (autor, wiadomość, czas utworzenia, id).
#' @param page_name Nazwa strony
#'
#' @return Ramka danych
#'
#' @import stringi
#' @export

merge.comments <- function(element, page_name){

    # jesli nie bylo komentarzy pod postem
    if(nrow(element$comments) == 0){

        merged <- cbind(element$post[, c(7,1,2,3,8,9,10)],
                        date = strftime(element$post$created_time, "%Y-%m-%d"),
                        time = stri_sub(element$post$created_time, from=12, to=-6),
                        weekday = strftime(element$post$created_time, "%w"),
                        parent_id = NA,
                        page_name = page_name)
    }else{ # jesli byly komentarze pod postem

        merged <- rbind(
            cbind(

                element$post[, c(7,1,2,3,8,9,10)],
                date = strftime(element$post$created_time, "%Y-%m-%d"),
                time = stri_sub(element$post$created_time, from=12, to=-6),
                weekday = strftime(element$post$created_time, "%w"),
                parent_id = NA,
                page_name = page_name

            ),

            cbind(

                element$comments[, c(6,1,2,3, 5)],
                comments_count = "",
                shares_count = "",
                date = strftime(element$comments$created_time, "%Y-%m-%d"),
                time = stri_sub(element$post$created_time, from=12, to=-6),
                weekday = strftime(element$comments$created_time, "%w"),
                parent_id = element$post$id,
                page_name = page_name

            )
        )
    }


    colnames(merged)[4]<-"body"


    merged


}


