#' Perform a streaming GPT API request
#'
#' This function sends a streaming request to the GPT API and handles the response
#' using a callback function. It is designed to keep the connection open and
#' process the data as it streams in.
#'
#' @param base_url The endpoint URL for the GPT API request.
#' @param callback A callback function that defines how to process the data
#'        as it streams in. This function should take a single argument, which
#'        will be the raw vector of bytes received from the stream.
#' @param payload A list containing the data to be sent as the body of the POST
#'        request. This will be converted to JSON format.
#' @param token Your OpenAI API token used for authentication.
#'        Default is to retrieve from your config settings.
#' @return An object of class `c("curl_response", "list")` that includes
#'         information about the request and the status of the streaming data.
#' @export
#' @examples
#' # Define a simple callback function to print incoming data
#' callback <- function(data) {
#'   message <- rawToChar(data)
#'   cat(message)
#' }
#'
#' # Prepare payload with your parameters (replace with actual values)
#' my_payload <- list(
#'   model = "model = "gpt-3.5-turbo",
#'   messages = list(list(role = "system", content = "Hi, nice to meet you")),
#'   stream = TRUE
#' )
#'
#' # Call the function (replace with your actual base URL and token)
#' gpt_request_stream(
#'   base_url = "https://api.openai.com/v1/engines/text-davinci-002/completions",
#'   callback = callback,
#'   payload = my_payload
#' )
gpt_request_stream <- function(base_url, callback, payload, token = config::get()$openai) {
  # Convert the payload to JSON and specify that this is a streaming request.
  payload = jsonlite::toJSON(c(payload, list(stream = TRUE)), auto_unbox = TRUE)

  # Set up a new curl handle with appropriate headers and the postfields set to the payload.
  h <- curl::new_handle() |>
    curl::handle_setheaders(
      `Content-Type` = "application/json",
      Authorization = paste("Bearer", token)
    ) |>
    curl::handle_setopt(postfields = payload)

  # Make the streaming request with the specified callback function to handle the stream.
  x = curl::curl_fetch_stream(base_url, fun = callback, h)

  # If the status code of the response is not 200 (OK), emit a warning and return nothing.
  if (x$status_code != 200) {
    warning(x$status_code)
    return()
  }

  # Assign the classes from the response to the object to facilitate downstream handling.
  class(x) <- c(class(x), x$object)

  # Return the response object.
  x
}

callback_ <- function(x) {
  y = rawToChar(x)                       # 1. Converts the raw vector of bytes into characters
  y2 = stringr::str_split_1(y, "\\n\\n") # 2. Splits the character string on double newlines
  y2 = y2[y2 != ""]                      # 3. Removes any empty strings from the split results

  for (i in seq_along(y2)) {             # 4. Iterates over each element of the split results
    y2[i] = gsub("data: ", "", y2[i])    # 5. Removes the "data: " prefix from each element
  }

  y2 = sapply(y2, \(x) {                 # 6. Applies a function to each element of y2
    if (x == "[DONE]")                   # 7. If the element is "[DONE]", it returns an empty string
      return("")
    else
      jsonlite::fromJSON(x)$choices$delta$content  # 8. Parses the JSON and extracts the content field
  }) |> paste(collapse = "")             # 9. Collapses the list into a single string

  cat(y2)                                # 10. Outputs the result to the console
}

completions_stream <- function(prompt
                               , callback = callback_
                               , payload = list(model = "gpt-3.5-turbo")
                               , token = config::get()$openai) {

  payload = c(payload, messages = list(list(list(role = "system", content = prompt))))
  x = gpt_request_stream("https://api.openai.com/v1/chat/completions", callback, payload, token)
  x
}


callback_ <- function(raw_data) {
  # Decode the raw data stream into a character vector
  decoded_data <- rawToChar(raw_data)

  # Split the decoded data by double newlines and remove the 'data: ' prefix
  data_entries <- stringr::str_split(decoded_data, "\\n\\n", simplify = TRUE)
  data_entries <- gsub("data: ", "", data_entries)
  data_entries <- data_entries[data_entries != ""]

  # Parse the JSON data entries and extract the message content
  message_content <- vapply(data_entries, function(entry) {
  browser()
    if (entry == "[DONE]") {
      ""
    } else {
      parsed_entry <- jsonlite::fromJSON(entry, simplifyVector = FALSE)
      parsed_entry$choices[[1]]$delta$content
    }
  }, character(1))

  # Output the concatenated message contents
  output_content <- paste(message_content, collapse = "")
  cat(output_content)
}

# completions_stream("hey there")
