#' @export
cb_chat_ui <- function(id) {
  shinychat::chat_ui(id)
}

#' @export
cb_chat_server <- function(id, chat, input, output, session) {
  chat_input_id <- glue::glue("{id}_user_input")
  shiny::observeEvent(input[[chat_input_id]], {
    stream <- chat$stream_async(input[[chat_input_id]])
    shinychat::chat_append(id, stream, session = session)
  })
}
