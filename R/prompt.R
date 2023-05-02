#' Start a chatgpt session in the R terminal
#'
#' @param prompt the beginning prompt for your session
#'
#' @export
#'
start_chat = function(prompt) {
  cat("Type STOP to end session. Type RESET to reset the session\n")
  newchat = Chat$new(prompt)
  i = 1
  while (TRUE) {
    if (i %% 10 == 0) mewchat$usage()
    x = readline("Response: ")
    if (x == "STOP") break
    if (x == "RESET")
      newchat$reset()
    else
      newchat$chat(x)
  }
}
