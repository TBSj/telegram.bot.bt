
#### CLASS ####
#' Shipping queries
#'
#' \code{\link{Handler}} class to handle Telegram callback queries. Optionally
#' based on a regex.
#'
#' @docType class
#' @format An \code{\link{R6Class}} object.
#' @param callback The callback function for this handler.
#'   See \code{\link{Handler}} for information about this function.
#'
#'
ShippingQueryHandler <- function(callback) {
  ShippingQueryHandlerClass$new(callback)
}


ShippingQueryHandlerClass <- R6::R6Class("ShippingQueryHandler",
                                            inherit = HandlerClass,
                                            public = list(
                                              initialize = function(callback) {
                                                self$callback = callback
                                              },

                                              # Methods
                                              is_allowed_update = function(update) {
                                                !is.null(update$shipping_query)
                                              },

                                              check_update = function(update) {

                                                if( !is.null(update$shipping_query)){
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
