#' @export
cb_chat_ui <- function(chat_id) {
  shinychat::chat_ui(chat_id)
}

#' @export
cb_chat_server <- function(chat_id, chat, input, output, session) {
  chat_input_id <- glue::glue("{chat_id}_user_input")
  shiny::observeEvent(input[[chat_input_id]], {
    stream <- chat$stream_async(input[[chat_input_id]])
    shinychat::chat_append(chat_id, stream)
  })
}
