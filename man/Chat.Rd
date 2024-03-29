% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Chat_r6.R
\name{Chat}
\alias{Chat}
\title{Create a a running chat dialogue}
\description{
This R6 class stores chat messages to be used in the future  calls, so gpt can retain 'memory' of the conversation. Additionally there are some helpers to display token usage and to reset the chat.
}
\examples{
chat = Chat$new('List ingredients in spaghetti')


## ------------------------------------------------
## Method `Chat$add_function`
## ------------------------------------------------

chat <- Chat$new("How can I assist you?")
chat$add_function(
  name = "analyze_sentiment",
  description = "Analyzes the sentiment of the provided text and returns a score between 0 and 1.",
  parameters = list(
    score = list(
      type = "number",
      description = "Sentiment score where 0 indicates negative and 1 indicates positive sentiment."
    )
  )
)

}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Chat-reset}{\code{Chat$reset()}}
\item \href{#method-Chat-new}{\code{Chat$new()}}
\item \href{#method-Chat-add_function}{\code{Chat$add_function()}}
\item \href{#method-Chat-call_function}{\code{Chat$call_function()}}
\item \href{#method-Chat-chat}{\code{Chat$chat()}}
\item \href{#method-Chat-usage}{\code{Chat$usage()}}
\item \href{#method-Chat-clone}{\code{Chat$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Chat-reset"></a>}}
\if{latex}{\out{\hypertarget{method-Chat-reset}{}}}
\subsection{Method \code{reset()}}{
reset the chat, this removes all "memory" of the previous conversation.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Chat$reset()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Chat-new"></a>}}
\if{latex}{\out{\hypertarget{method-Chat-new}{}}}
\subsection{Method \code{new()}}{
create a new chat session
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Chat$new(
  message,
  payload = list(model = "gpt-3.5-turbo"),
  token = config::get()$openai,
  stream = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{message}}{the message you'd like to send}

\item{\code{payload}}{additional post body options, usually the model}

\item{\code{token}}{your openai auth token
Add a custom function for the ChatGPT model to call

This method registers a new custom function that the ChatGPT model can call.
The function is defined by its name, a description of its behavior, and the
structure of the parameters that will be included in the JSON response from ChatGPT.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Chat-add_function"></a>}}
\if{latex}{\out{\hypertarget{method-Chat-add_function}{}}}
\subsection{Method \code{add_function()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Chat$add_function(name, description = NULL, parameters)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the custom function to be added.}

\item{\code{description}}{A description of what the function does and what it is used for.}

\item{\code{parameters}}{A list describing the expected parameters in the JSON
response from ChatGPT when this function is called. Each parameter should include
its name, type, and a description.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The method invisibly returns the updated Chat object with the new function
added to its list of available functions.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{chat <- Chat$new("How can I assist you?")
chat$add_function(
  name = "analyze_sentiment",
  description = "Analyzes the sentiment of the provided text and returns a score between 0 and 1.",
  parameters = list(
    score = list(
      type = "number",
      description = "Sentiment score where 0 indicates negative and 1 indicates positive sentiment."
    )
  )
)

}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Chat-call_function"></a>}}
\if{latex}{\out{\hypertarget{method-Chat-call_function}{}}}
\subsection{Method \code{call_function()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Chat$call_function(name, msg)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the function to call.}

\item{\code{arguments}}{A list of arguments for the function call (default is an empty list).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The updated Chat object, reflecting the latest state after the function call.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Chat-chat"></a>}}
\if{latex}{\out{\hypertarget{method-Chat-chat}{}}}
\subsection{Method \code{chat()}}{
continue the chat conversation
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Chat$chat(message)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{message}}{the message you'd like to send}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Chat-usage"></a>}}
\if{latex}{\out{\hypertarget{method-Chat-usage}{}}}
\subsection{Method \code{usage()}}{
view the current token usage for this chat session
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Chat$usage()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Chat-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Chat-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Chat$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
