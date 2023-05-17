
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
#' @param pattern (Optional). Regex pattern to test.
#'
#' @export


InlineQueryHandler <- function(query,
                               pattern = NULL) {
  InlineQueryHandlerClass$new(query, pattern)
}


InlineQueryHandlerClass <- R6::R6Class(
  "InlineQueryHandler",
  inherit = HandlerClass,
  public = list(
    initialize = function(query, pattern) {
      self$query <- query

      if (!missing(pattern)) {
        self$pattern <- pattern
      }
    },
    is_allowed_update = function(update) {
      !is.null(update$inline_query)
    },

    # This method is called to determine if an update should be handled by
    # this handler instance.
    check_update = function(update) {
      if (is.Update(update) && self$is_allowed_update(update)) {
        if (!is.null(self$pattern) && !is.null(update$inline_query$query)) {
          return(grepl(self$pattern, update$inline_query$query))
        } else {
          return(TRUE) # nocov
        }
      } else {
        return(FALSE) # nocov
      }
    },

    # check_update = function(update) {
    #   if (!is.null(update$inline_query)) {
    #     return(TRUE)
    #   }
    #   else{
    #     return(NULL)
    #   }
    # },

    handle_update = function(update, dispatcher) {
      self$query(dispatcher$bot, update)
    },

    # Params
    query = NULL,
    pattern = NULL
  )
)
