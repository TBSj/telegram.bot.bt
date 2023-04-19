
#### CLASS ####
#' Inline queries
#'
#' \code{\link{Handler}} class to handle Telegram callback queries. Optionally
#' based on a regex.
#'
#' @docType class
#' @format An \code{\link{R6Class}} object.
#' @param callback The callback function for this handler.
#'   See \code{\link{Handler}} for information about this function.
#'
#' @export


InlineQueryHandler <- function(callback) {
  InlineQueryHandlerClass$new(callback)
}


InlineQueryHandlerClass <- R6::R6Class("InlineQueryHandler",
                                            inherit = HandlerClass,
                                            public = list(
                                              initialize = function(callback) {
                                                self$callback = callback
                                              },

                                              check_update = function(update) {

                                                if( !is.null(update$inline_query)){
                                                  return(TRUE)
                                                }
                                                else{
                                                  return(NULL)
                                                }


                                              },

                                              handle_update = function(update, dispatcher) {
                                                self$callback(dispatcher$bot, update)
                                              },

                                              # Params
                                              callback = NULL
                                            )
)
