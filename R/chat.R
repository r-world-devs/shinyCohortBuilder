#' Cohort assistant chat module
#'
#' A thin wrapper around \pkg{shinychat} that embeds an LLM assistant alongside
#' the filtering panel. \code{cb_chat_ui()} renders the chat widget and
#' \code{cb_chat_server()} wires user messages to a chat client, streaming the
#' assistant's reply back into the widget.
#'
#' The chat client (\code{chat}) is created by the caller (for example with
#' \code{ellmer::chat_*()}) and passed in, so the host application controls the
#' model, system prompt and tools. \code{cb_server()} mounts these helpers
#' automatically when its \code{chat} argument is supplied.
#'
#' @param id Module id shared by \code{cb_chat_ui()} and \code{cb_chat_server()}.
#' @param chat A chat client object exposing \code{stream_async()} (e.g. an
#'   \pkg{ellmer} chat), used to generate streamed responses.
#' @param input,output,session The hosting Shiny module's reactive context
#'   objects.
#'
#' @return \code{cb_chat_ui()} returns the chat UI (a \code{shiny.tag}).
#'   \code{cb_chat_server()} is called for its side effects (registering the
#'   message observer) and returns the observer invisibly.
#'
#' @name cb_chat
#' @export
cb_chat_ui <- function(id) {
  shinychat::chat_ui(id)
}

#' @rdname cb_chat
#' @export
cb_chat_server <- function(id, chat, input, output, session) {
  chat_input_id <- glue::glue("{id}_user_input")
  shiny::observeEvent(input[[chat_input_id]], {
    stream <- chat$stream_async(input[[chat_input_id]])
    shinychat::chat_append(id, stream, session = session)
  })
}
