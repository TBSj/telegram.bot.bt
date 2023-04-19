
#### INTERNAL METHODS ####

# Print function
.print <- function() {
  obj <- objects(self)
  api_methods <- obj[grepl("[A-Z]", obj)]
  snake <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", api_methods, perl = TRUE)
  dont_show <- c("clone", "initialize", "print")
  avail_methods <- sort(api_methods)
  remaining_methods <- sort(setdiff(obj, c(avail_methods, snake, dont_show)))
  api_string <- method_summaries(avail_methods, indent = 4L)
  remaining_string <- method_summaries(remaining_methods, indent = 4L)

  ret <- paste0("<", class(self)[1L], ">")

  classes <- setdiff(class(self), "R6")
  if (length(classes) >= 2L) {
    ret <- c(ret, paste0("  Inherits from: <", classes[2L], ">")) # nocov
  }

  ret <- c(
    ret,
    "  API Methods:", api_string,
    "  Other Methods:", remaining_string
  )

  cat(paste(ret, collapse = "\n"), sep = "\n")
}

# A very basic validation on token
.validate_token <- function(token) {
  if (grepl(" ", token)) {
    stop("Invalid `token`.")
  }

  split <- strsplit(token, ":")[[1L]]
  if (length(split) < 2L ||
    identical(split[2L], "") ||
    grepl("\\D", split[1L]) ||
    nchar(split[1L]) < 3L) {
    stop("Invalid `token`.")
  }

  token
}

# Request an URL
.request <- function(url,
                     data, encode = 'multipart') {
  result <- httr::POST(
    url = url,
    body = data,
    config = private$request_config,
    encode = encode
  )
  httr::stop_for_status(result)

  if (result$status_code >= 200L && result$status_code < 300L) {
    # 200-299 range are HTTP success statuses
    return(private$parse(result))
  } else {
    stop("HTTPError") # nocov
  }
}

# Parse result
.parse <- function(result) {
  data <- tryCatch({
    httr::content(result, as = "parsed", encoding = "UTF-8")
  })

  if (is.list(data) && data$ok) {
    return(data$result)
  } else {
    stop("Invalid server response.") # nocov
  }
}


#### API METHODS ####

#' Check your bot's information
#'
#' A simple method for testing your bot's auth token. Requires no parameters.
#'
#' You can also use it's snake_case equivalent \code{get_me}.
getMe <- function() {
  url <- sprintf("%s/getMe", private$base_url)

  data <- list()

  result <- private$request(url, data)

  invisible(result)
}


#' Send text messages
#'
#' Use this method to send text messages.
#'
#' You can also use it's snake_case equivalent \code{send_message}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param text Text of the message to be sent.
#' @param parse_mode (Optional). Send 'Markdown' or 'HTML', if you want
#'     Telegram apps to show bold, italic, fixed-width text or inline URLs in
#'     your bot's message.
#' @param disable_web_page_preview (Optional). Disables link previews for links
#'     in this message.
#' @param disable_notification (Optional). Sends the message silently. Users
#'     will receive a notification with no sound.
#' @param reply_to_message_id (Optional). If the message is a reply, ID of the
#'     original message.
#' @param reply_markup (Optional). A Reply Markup parameter object, it can be
#'     either:
#'     \itemize{
#'      \item{\code{\link{ReplyKeyboardMarkup}}}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'      \item{\code{\link{ReplyKeyboardRemove}}}
#'      \item{\code{\link{ForceReply}}}
#'     }
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#'
#' bot$sendMessage(
#'   chat_id = chat_id,
#'   text = "foo *bold* _italic_",
#'   parse_mode = "Markdown"
#' )
#' }
sendMessage <- function(chat_id,
                        text,
                        parse_mode = NULL,
                        disable_web_page_preview = NULL,
                        disable_notification = FALSE,
                        reply_to_message_id = NULL,
                        reply_markup = NULL) {
  url <- sprintf("%s/sendMessage", private$base_url)

  data <- list(
    chat_id = chat_id,
    text = text
  )

  if (!missing(parse_mode)) {
    data[["parse_mode"]] <- parse_mode
  }
  if (!missing(disable_web_page_preview)) {
    data[["disable_web_page_preview"]] <- disable_web_page_preview
  }
  if (!missing(disable_notification)) {
    data[["disable_notification"]] <- disable_notification
  }
  if (!missing(reply_to_message_id)) {
    data[["reply_to_message_id"]] <- reply_to_message_id
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }

  result <- private$request(url, data)

  invisible(result)
}


#' Delete a message
#'
#' Use this method to delete a message. A message can only be deleted if it was
#' sent less than 48 hours ago. Any such recently sent outgoing message may be
#' deleted. Additionally, if the bot is an administrator in a group chat, it
#' can delete any message. If the bot is an administrator in a supergroup, it
#' can delete messages from any other user and service messages about people
#' joining or leaving the group (other types of service messages may only be
#' removed by the group creator). In channels, bots can only remove their own
#' messages.
#'
#' You can also use it's snake_case equivalent \code{delete_message}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param message_id Identifier of the message to delete.
deleteMessage <- function(chat_id,
                          message_id) { # nocov start
  url <- sprintf("%s/deleteMessage", private$base_url)

  data <- list(
    chat_id = chat_id,
    message_id = message_id
  )

  result <- private$request(url, data)

  invisible(result)
} # nocov end


#' Forward messages of any kind
#'
#' Use this method to forward messages of any kind.
#'
#' You can also use it's snake_case equivalent \code{forward_message}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param from_chat_id Unique identifier for the chat where the
#'     original message was sent.
#' @param message_id Message identifier in the chat specified in from_chat_id.
#' @param disable_notification (Optional). Sends the message silently. Users
#'     will receive a notification with no sound.
forwardMessage <- function(chat_id,
                           from_chat_id,
                           message_id,
                           disable_notification = FALSE) { # nocov start
  url <- sprintf("%s/forwardMessage", private$base_url)

  data <- list(
    chat_id = chat_id,
    from_chat_id = from_chat_id,
    message_id = message_id
  )

  if (!missing(disable_notification)) {
    data[["disable_notification"]] <- disable_notification
  }

  result <- private$request(url, data)

  invisible(result)
} # nocov end


#' Send image files
#'
#' Use this method to send photos.
#'
#' You can also use it's snake_case equivalent \code{send_photo}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param photo Photo to send. Pass a file_id as String to send a photo that
#'     exists on the Telegram servers (recommended), pass an HTTP URL as a
#'     String for Telegram to get a photo from the Internet, or upload a local
#'     photo by passing a file path.
#' @param caption (Optional). Photo caption (may also be used when re-sending
#'     photos by file_id), 0-1024 characters.
#' @param disable_notification (Optional). Sends the message silently. Users
#'     will receive a notification with no sound.
#' @param reply_to_message_id (Optional). If the message is a reply, ID of the
#'     original message.
#' @param reply_markup (Optional). A Reply Markup parameter object, it can be
#'     either:
#'     \itemize{
#'      \item{\code{\link{ReplyKeyboardMarkup}}}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'      \item{\code{\link{ReplyKeyboardRemove}}}
#'      \item{\code{\link{ForceReply}}}}
#' @param parse_mode (Optional). Send 'Markdown' or 'HTML', if you want
#'     Telegram apps to show bold, italic, fixed-width text or inline URLs in
#'     your bot's message.
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#' photo_url <- "https://telegram.org/img/t_logo.png"
#'
#' bot$sendPhoto(
#'   chat_id = chat_id,
#'   photo = photo_url,
#'   caption = "Telegram Logo"
#' )
#' }
sendPhoto <- function(chat_id,
                      photo,
                      caption = NULL,
                      disable_notification = FALSE,
                      reply_to_message_id = NULL,
                      reply_markup = NULL,
                      parse_mode = NULL) {
  url <- sprintf("%s/sendPhoto", private$base_url)

  if (file.exists(photo)) {
    photo <- httr::upload_file(photo) # nocov
  }

  data <- list(chat_id = chat_id, photo = photo)

  if (!missing(caption)) {
    data[["caption"]] <- caption
  }
  if (!missing(disable_notification)) {
    data[["disable_notification"]] <- disable_notification
  }
  if (!missing(reply_to_message_id)) {
    data[["reply_to_message_id"]] <- reply_to_message_id
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }
  if (!missing(parse_mode)) {
    data[["parse_mode"]] <- parse_mode
  }

  result <- private$request(url, data)

  invisible(result)
}


#' Send audio files
#'
#' Use this method to send audio files, if you want Telegram clients to display
#' them in the music player. Your audio must be in the .mp3 format. On success,
#' the sent Message is returned. Bots can currently send audio files of up to
#' 50 MB in size, this limit may be changed in the future.
#' For sending voice messages, use the \code{\link{sendVoice}} method instead.
#'
#' You can also use it's snake_case equivalent \code{send_audio}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param audio Audio file to send. Pass a file_id as String to send an audio
#'     that exists on the Telegram servers (recommended), pass an HTTP URL as a
#'     String for Telegram to get an audio from the Internet, or upload a local
#'     audio file by passing a file path.
#' @param duration (Optional). Duration of sent audio in seconds.
#' @param performer (Optional). Performer.
#' @param title (Optional). Track name.
#' @param caption (Optional). Audio caption, 0-1024 characters.
#' @param disable_notification (Optional). Sends the message silently. Users
#'     will receive a notification with no sound.
#' @param reply_to_message_id (Optional). If the message is a reply, ID of the
#'     original message.
#' @param reply_markup (Optional). A Reply Markup parameter object, it can be
#'     either:
#'     \itemize{
#'      \item{\code{\link{ReplyKeyboardMarkup}}}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'      \item{\code{\link{ReplyKeyboardRemove}}}
#'      \item{\code{\link{ForceReply}}}}
#' @param parse_mode (Optional). Send 'Markdown' or 'HTML', if you want
#'     Telegram apps to show bold, italic, fixed-width text or inline URLs in
#'     your bot's message.
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#' audio_url <- "http://www.largesound.com/ashborytour/sound/brobob.mp3"
#'
#' bot$sendAudio(
#'   chat_id = chat_id,
#'   audio = audio_url
#' )
#' }
sendAudio <- function(chat_id,
                      audio,
                      duration = NULL,
                      performer = NULL,
                      title = NULL,
                      caption = NULL,
                      disable_notification = FALSE,
                      reply_to_message_id = NULL,
                      reply_markup = NULL,
                      parse_mode = NULL) {
  url <- sprintf("%s/sendAudio", private$base_url)

  if (file.exists(audio)) {
    audio <- httr::upload_file(audio) # nocov
  }

  data <- list(chat_id = chat_id, audio = audio)

  if (!missing(duration)) {
    data[["duration"]] <- duration
  }
  if (!missing(performer)) {
    data[["performer"]] <- performer
  }
  if (!missing(title)) {
    data[["title"]] <- title
  }
  if (!missing(caption)) {
    data[["caption"]] <- caption
  }
  if (!missing(disable_notification)) {
    data[["disable_notification"]] <- disable_notification
  }
  if (!missing(reply_to_message_id)) {
    data[["reply_to_message_id"]] <- reply_to_message_id
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }
  if (!missing(parse_mode)) {
    data[["parse_mode"]] <- parse_mode
  }

  result <- private$request(url, data)

  invisible(result)
}


#' Send general files
#'
#' Use this method to send general files.
#'
#' You can also use it's snake_case equivalent \code{send_document}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param document File to send. Pass a file_id as String to send a file that
#'     exists on the Telegram servers (recommended), pass an HTTP URL as a
#'     String for Telegram to get a file from the Internet, or upload a local
#'     file by passing a file path
#' @param filename (Optional). File name that shows in telegram message.
#' @param caption (Optional). Document caption, 0-1024 characters.
#' @param disable_notification (Optional). Sends the message silently. Users
#'     will receive a notification with no sound.
#' @param reply_to_message_id (Optional). If the message is a reply, ID of the
#'     original message.
#' @param reply_markup (Optional). A Reply Markup parameter object, it can be
#'     either:
#'     \itemize{
#'      \item{\code{\link{ReplyKeyboardMarkup}}}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'      \item{\code{\link{ReplyKeyboardRemove}}}
#'      \item{\code{\link{ForceReply}}}}
#' @param parse_mode (Optional). Send 'Markdown' or 'HTML', if you want
#'     Telegram apps to show bold, italic, fixed-width text or inline URLs in
#'     your bot's message.
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#' document_url <- paste0(
#'   "https://github.com/ebeneditos/telegram.bot/raw/gh-pages/docs/",
#'   "telegram.bot.pdf"
#' )
#'
#' bot$sendDocument(
#'   chat_id = chat_id,
#'   document = document_url
#' )
#' }
sendDocument <- function(chat_id,
                         document,
                         filename = NULL,
                         caption = NULL,
                         disable_notification = FALSE,
                         reply_to_message_id = NULL,
                         reply_markup = NULL,
                         parse_mode = NULL) {
  url <- sprintf("%s/sendDocument", private$base_url)

  if (file.exists(document)) {
    document <- httr::upload_file(document) # nocov
  }

  data <- list(chat_id = chat_id, document = document)

  if (!missing(filename)) {
    data[["filename"]] <- filename
  }
  if (!missing(caption)) {
    data[["caption"]] <- caption
  }
  if (!missing(disable_notification)) {
    data[["disable_notification"]] <- disable_notification
  }
  if (!missing(reply_to_message_id)) {
    data[["reply_to_message_id"]] <- reply_to_message_id
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }
  if (!missing(parse_mode)) {
    data[["parse_mode"]] <- parse_mode
  }

  result <- private$request(url, data)

  invisible(result)
}


#' Send a sticker
#'
#' Use this method to send \code{.webp} stickers.
#'
#' You can also use it's snake_case equivalent \code{send_sticker}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param sticker Sticker to send. Pass a file_id as String to send a file that
#'     exists on the Telegram servers (recommended), pass an HTTP URL as a
#'     String for Telegram to get a \code{.webp} file from the Internet, or
#'     upload a local one by passing a file path.
#' @param disable_notification (Optional). Sends the message silently. Users
#'     will receive a notification with no sound.
#' @param reply_to_message_id (Optional). If the message is a reply, ID of the
#'     original message.
#' @param reply_markup (Optional). A Reply Markup parameter object, it can be
#'     either:
#'     \itemize{
#'      \item{\code{\link{ReplyKeyboardMarkup}}}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'      \item{\code{\link{ReplyKeyboardRemove}}}
#'      \item{\code{\link{ForceReply}}}}
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#' sticker_url <- "https://www.gstatic.com/webp/gallery/1.webp"
#'
#' bot$sendSticker(
#'   chat_id = chat_id,
#'   sticker = sticker_url
#' )
#' }
sendSticker <- function(chat_id,
                        sticker,
                        disable_notification = FALSE,
                        reply_to_message_id = NULL,
                        reply_markup = NULL) {
  url <- sprintf("%s/sendSticker", private$base_url)

  if (file.exists(sticker)) {
    sticker <- httr::upload_file(sticker) # nocov
  }

  data <- list(chat_id = chat_id, sticker = sticker)

  if (!missing(disable_notification)) {
    data[["disable_notification"]] <- disable_notification
  }
  if (!missing(reply_to_message_id)) {
    data[["reply_to_message_id"]] <- reply_to_message_id
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }

  result <- private$request(url, data)

  invisible(result)
}


#' Send a video
#'
#' Use this method to send video files, Telegram clients support mp4 videos
#' (other formats may be sent as Document).
#'
#' You can also use it's snake_case equivalent \code{send_video}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param video Video file to send. Pass a file_id as String to send a video
#'     that exists on the Telegram servers (recommended), pass an HTTP URL as a
#'     String for Telegram to get a video from the Internet, or upload a local
#'     video file by passing a file path.
#' @param duration (Optional). Duration of sent audio in seconds.
#' @param caption (Optional). Video caption, 0-1024 characters.
#' @param disable_notification (Optional). Sends the message silently. Users
#'     will receive a notification with no sound.
#' @param reply_to_message_id (Optional). If the message is a reply, ID of the
#'     original message.
#' @param reply_markup (Optional). A Reply Markup parameter object, it can be
#'     either:
#'     \itemize{
#'      \item{\code{\link{ReplyKeyboardMarkup}}}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'      \item{\code{\link{ReplyKeyboardRemove}}}
#'      \item{\code{\link{ForceReply}}}}
#' @param width (Optional). Video width.
#' @param height (Optional). Video height.
#' @param parse_mode (Optional). Send 'Markdown' or 'HTML', if you want
#'     Telegram apps to show bold, italic, fixed-width text or inline URLs in
#'     your bot's message.
#' @param supports_streaming (Optional). Pass \code{TRUE}, if the uploaded
#'     video is suitable for streaming.
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#' video_url <- "http://techslides.com/demos/sample-videos/small.mp4"
#'
#' bot$sendVideo(
#'   chat_id = chat_id,
#'   video = video_url
#' )
#' }
sendVideo <- function(chat_id,
                      video,
                      duration = NULL,
                      caption = NULL,
                      disable_notification = FALSE,
                      reply_to_message_id = NULL,
                      reply_markup = NULL,
                      width = NULL,
                      height = NULL,
                      parse_mode = NULL,
                      supports_streaming = NULL) {
  url <- sprintf("%s/sendVideo", private$base_url)

  if (file.exists(video)) {
    video <- httr::upload_file(video) # nocov
  }

  data <- list(chat_id = chat_id, video = video)

  if (!missing(duration)) {
    data[["duration"]] <- duration
  }
  if (!missing(width)) {
    data[["width"]] <- width
  }
  if (!missing(height)) {
    data[["height"]] <- height
  }
  if (!missing(caption)) {
    data[["caption"]] <- caption
  }
  if (!missing(disable_notification)) {
    data[["disable_notification"]] <- disable_notification
  }
  if (!missing(reply_to_message_id)) {
    data[["reply_to_message_id"]] <- reply_to_message_id
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }
  if (!missing(parse_mode)) {
    data[["parse_mode"]] <- parse_mode
  }
  if (!missing(supports_streaming)) {
    data[["supports_streaming"]] <- supports_streaming
  }

  result <- private$request(url, data)

  invisible(result)
}


#' Send video messages
#'
#' Use this method to send video messages.
#'
#' You can also use it's snake_case equivalent \code{send_video_note}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param video_note Video note file to send. Pass a file_id as String to send
#'     a video note that exists on the Telegram servers (recommended), pass an
#'     HTTP URL as a String for Telegram to get a video note from the Internet,
#'     or upload a local video note file by passing a file path.
#' @param duration (Optional). Duration of sent audio in seconds.
#' @param length (Optional). Video width and height.
#' @param disable_notification (Optional). Sends the message silently. Users
#'     will receive a notification with no sound.
#' @param reply_to_message_id (Optional). If the message is a reply, ID of the
#'     original message.
#' @param reply_markup (Optional). A Reply Markup parameter object, it can be
#'     either:
#'     \itemize{
#'      \item{\code{\link{ReplyKeyboardMarkup}}}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'      \item{\code{\link{ReplyKeyboardRemove}}}
#'      \item{\code{\link{ForceReply}}}}
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#' video_note_url <- "http://techslides.com/demos/sample-videos/small.mp4"
#'
#' bot$sendVideoNote(
#'   chat_id = chat_id,
#'   video_note = video_note_url
#' )
#' }
sendVideoNote <- function(chat_id,
                          video_note,
                          duration = NULL,
                          length = NULL,
                          disable_notification = FALSE,
                          reply_to_message_id = NULL,
                          reply_markup = NULL) {
  url <- sprintf("%s/sendVideoNote", private$base_url)

  if (file.exists(video_note)) {
    video_note <- httr::upload_file(video_note) # nocov
  }

  data <- list(chat_id = chat_id, video_note = video_note)

  if (!missing(duration)) {
    data[["duration"]] <- duration
  }
  if (!missing(length)) {
    data[["length"]] <- length
  }
  if (!missing(disable_notification)) {
    data[["disable_notification"]] <- disable_notification
  }
  if (!missing(reply_to_message_id)) {
    data[["reply_to_message_id"]] <- reply_to_message_id
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }

  result <- private$request(url, data)

  invisible(result)
}


#' Send animation files
#'
#' Use this method to send animation files (GIF or H.264/MPEG-4 AVC video
#' without sound).
#'
#' You can also use it's snake_case equivalent \code{send_animation}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param animation Animation to send. Pass a file_id as String to send an
#'     animation that exists on the Telegram servers (recommended), pass an
#'     HTTP URL as a String for Telegram to get an animation from the Internet,
#'     or upload a local file by passing a file path.
#' @param duration (Optional). Duration of sent audio in seconds.
#' @param width (Optional). Video width.
#' @param height (Optional). Video height.
#' @param caption (Optional). Animation caption, 0-1024 characters.
#' @param parse_mode (Optional). Send 'Markdown' or 'HTML', if you want
#'     Telegram apps to show bold, italic, fixed-width text or inline URLs in
#'     your bot's message.
#' @param disable_notification (Optional). Sends the message silently. Users
#'     will receive a notification with no sound.
#' @param reply_to_message_id (Optional). If the message is a reply, ID of the
#'     original message.
#' @param reply_markup (Optional). A Reply Markup parameter object, it can be
#'     either:
#'     \itemize{
#'      \item{\code{\link{ReplyKeyboardMarkup}}}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'      \item{\code{\link{ReplyKeyboardRemove}}}
#'      \item{\code{\link{ForceReply}}}}
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#' animation_url <- "http://techslides.com/demos/sample-videos/small.mp4"
#'
#' bot$sendAnimation(
#'   chat_id = chat_id,
#'   animation = animation_url
#' )
#' }
sendAnimation <- function(chat_id,
                          animation,
                          duration = NULL,
                          width = NULL,
                          height = NULL,
                          caption = NULL,
                          parse_mode = NULL,
                          disable_notification = FALSE,
                          reply_to_message_id = NULL,
                          reply_markup = NULL) {
  url <- sprintf("%s/sendAnimation", private$base_url)

  if (file.exists(animation)) {
    animation <- httr::upload_file(animation) # nocov
  }

  data <- list(chat_id = chat_id, animation = animation)

  if (!missing(duration)) {
    data[["duration"]] <- duration
  }
  if (!missing(width)) {
    data[["width"]] <- width
  }
  if (!missing(height)) {
    data[["height"]] <- height
  }
  if (!missing(caption)) {
    data[["caption"]] <- caption
  }
  if (!missing(disable_notification)) {
    data[["disable_notification"]] <- disable_notification
  }
  if (!missing(reply_to_message_id)) {
    data[["reply_to_message_id"]] <- reply_to_message_id
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }
  if (!missing(parse_mode)) {
    data[["parse_mode"]] <- parse_mode
  }

  result <- private$request(url, data)

  invisible(result)
}


#' Send voice files
#'
#' Use this method to send audio files, if you want Telegram clients to display
#' the file as a playable voice message. For this to work, your audio must be
#' in an \code{.ogg} file encoded with OPUS (other formats may be sent with
#' \code{\link{sendAudio}} or \code{\link{sendDocument}}).
#'
#' You can also use it's snake_case equivalent \code{send_voice}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param voice Voice file to send. Pass a file_id as String to send a voice
#'     file that exists on the Telegram servers (recommended), pass an HTTP URL
#'     as a String for Telegram to get a voice file from the Internet, or
#'     upload a local voice file file by passing a file path.
#' @param duration (Optional). Duration of sent audio in seconds.
#' @param caption (Optional). Voice message caption, 0-1024 characters.
#' @param disable_notification (Optional). Sends the message silently. Users
#'     will receive a notification with no sound.
#' @param reply_to_message_id (Optional). If the message is a reply, ID of the
#'     original message.
#' @param reply_markup (Optional). A Reply Markup parameter object, it can be
#'     either:
#'     \itemize{
#'      \item{\code{\link{ReplyKeyboardMarkup}}}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'      \item{\code{\link{ReplyKeyboardRemove}}}
#'      \item{\code{\link{ForceReply}}}}
#' @param parse_mode (Optional). Send 'Markdown' or 'HTML', if you want
#'     Telegram apps to show bold, italic, fixed-width text or inline URLs in
#'     your bot's message.
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#' ogg_url <- "https://upload.wikimedia.org/wikipedia/commons/c/c8/Example.ogg"
#'
#' bot$sendVoice(
#'   chat_id = chat_id,
#'   voice = ogg_url
#' )
#' }
sendVoice <- function(chat_id,
                      voice,
                      duration = NULL,
                      caption = NULL,
                      disable_notification = FALSE,
                      reply_to_message_id = NULL,
                      reply_markup = NULL,
                      parse_mode = NULL) {
  url <- sprintf("%s/sendVoice", private$base_url)

  if (file.exists(voice)) {
    voice <- httr::upload_file(voice) # nocov
  }

  data <- list(chat_id = chat_id, voice = voice)

  if (!missing(duration)) {
    data[["duration"]] <- duration
  }
  if (!missing(caption)) {
    data[["caption"]] <- caption
  }
  if (!missing(disable_notification)) {
    data[["disable_notification"]] <- disable_notification
  }
  if (!missing(reply_to_message_id)) {
    data[["reply_to_message_id"]] <- reply_to_message_id
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }
  if (!missing(parse_mode)) {
    data[["parse_mode"]] <- parse_mode
  }

  result <- private$request(url, data)

  invisible(result)
}


#' Send point on the map
#'
#' Use this method to send point on the map.
#'
#' You can also use it's snake_case equivalent \code{send_location}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param latitude Latitude of location.
#' @param longitude Longitude of location.
#' @param disable_notification (Optional). Sends the message silently. Users
#'     will receive a notification with no sound.
#' @param reply_to_message_id (Optional). If the message is a reply, ID of the
#'     original message.
#' @param reply_markup (Optional). A Reply Markup parameter object, it can be
#'     either:
#'     \itemize{
#'      \item{\code{\link{ReplyKeyboardMarkup}}}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'      \item{\code{\link{ReplyKeyboardRemove}}}
#'      \item{\code{\link{ForceReply}}}}
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#'
#' bot$sendLocation(
#'   chat_id = chat_id,
#'   latitude = 51.521727,
#'   longitude = -0.117255
#' )
#' }
sendLocation <- function(chat_id,
                         latitude,
                         longitude,
                         disable_notification = FALSE,
                         reply_to_message_id = NULL,
                         reply_markup = NULL) {
  url <- sprintf("%s/sendLocation", private$base_url)

  data <- list(chat_id = chat_id, latitude = latitude, longitude = longitude)

  if (!missing(disable_notification)) {
    data[["disable_notification"]] <- disable_notification
  }
  if (!missing(reply_to_message_id)) {
    data[["reply_to_message_id"]] <- reply_to_message_id
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }

  result <- private$request(url, data)

  invisible(result)
}


#' Send a chat action
#'
#' Use this method when you need to tell the user that something is happening
#' on the bot's side. The status is set for 5 seconds or less (when a message
#' arrives from your bot, Telegram clients clear its typing status).
#'
#' You can also use it's snake_case equivalent \code{send_chat_action}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param action Type of action to broadcast. Choose one, depending on
#' what the user is about to receive:
#' \itemize{
#'  \item{\code{typing}}{ for text messages}
#'  \item{\code{upload_photo}}{ for photos}
#'  \item{\code{upload_video}}{ for videos}
#'  \item{\code{record_video}}{ for video recording}
#'  \item{\code{upload_audio}}{ for audio files}
#'  \item{\code{record_audio}}{ for audio file recording}
#'  \item{\code{upload_document}}{ for general files}
#'  \item{\code{find_location}}{ for location data}
#'  \item{\code{upload_video_note}}{ for video notes}
#'  \item{\code{record_video_note}}{ for video note recording}
#' }
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#'
#' bot$sendChatAction(
#'   chat_id = chat_id,
#'   action = "typing"
#' )
#' }
sendChatAction <- function(chat_id,
                           action) {
  url <- sprintf("%s/sendChatAction", private$base_url)

  data <- list(chat_id = chat_id, action = action)

  result <- private$request(url, data)

  invisible(result)
}

#' Send invoice
#'
#' Use this method to send invoice.
#'
#' You can also use it's snake_case equivalent \code{send_invoice}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
#' @param title Product name.
#' @param description Product description.
#' @param payload Bot-defined invoice payload. This will not be displayed to
#' the user, use for your internal processes..
#' @param provider_token Payment provider token, obtained via BotFather
#' \href{(https://t.me/botfather)}.
#' @param currency Three-letter ISO 4217 currency code, see
#' \href{ https://core.telegram.org/bots/payments#supported-currencies}.
#' @param label Portion label
#' @param amount Price of the product in the smallest units of the currency
#' (integer, not float/double). For example, for a price of US$ 1.45 pass
#' amount = 145. See the exp parameter
#' in \href{https://core.telegram.org/bots/payments#supported-currencies}, it shows
#' the number of digits past the decimal point for each currency
#' (2 for the majority of currencies).
#' @param max_tip_amount (Optional). The maximum accepted amount for tips
#' in the smallest units of the currency (integer, not float/double).
#' For example, for a maximum tip of US$ 1.45 pass max_tip_amount = 145.
#' See the exp parameter
#' in \href{https://core.telegram.org/bots/payments#supported-currencies}, it shows
#' the number of digits past the decimal point for each currency
#' (2 for the majority of currencies). Defaults to 0.
#' @param suggested_tip_amounts (Optional). List of suggested amounts of tips
#' in the smallest units of the currency (integer, not float/double).
#' At most 4 suggested tip amounts can be specified. The suggested tip amounts
#' must be positive, passed in a strictly increased order and must not exceed
#' max_tip_amount.
#' @param start_parameter (Optional). Unique deep-linking parameter.
#' If left empty, forwarded copies of the sent message will have a Pay button,
#' allowing multiple users to pay directly from the forwarded message, using
#' the same invoice. If non-empty, forwarded copies of the sent message
#' will have a URL button with a deep link to the bot (instead of a Pay button),
#'  with the value used as the start parameter.
#' @param provider_data (Optional). Data about the invoice, which will be
#' shared with the payment provider. A detailed description of required fields
#' should be provided by the payment provider.
#' @param photo_url (Optional). URL of the product photo for the invoice.
#' Can be a photo of the goods or a marketing image for a service.
#' People like it better when they see what they are paying for.
#' @param photo_size (Optional). Photo size in bytes
#' @param photo_width (Optional). Photo width
#' @param photo_height (Optional). Photo height
#' @param need_name (Optional). Pass \code{TRUE}, if you require the user's full
#' name to complete the order
#' @param need_phone_number (Optional). Pass \code{TRUE}, if you require the user's
#' phone number to complete the order
#' @param need_email (Optional). Pass \code{TRUE}, if you require the user's email
#' address to complete the order
#' @param need_shipping_address (Optional). Pass \code{TRUE}, if you require
#' the user's shipping address to complete the order
#' @param send_phone_number_to_provider (Optional). Pass \code{TRUE},
#' if the user's phone number should be sent to provider
#' @param send_email_to_provider (Optional). Pass \code{TRUE},
#' if the user's email address should be sent to provider
#' @param is_flexible (Optional). Pass \code{TRUE},
#' if the final price depends on the shipping method
#' @param disable_notification (Optional). Sends the message silently.
#' Users will receive a notification with no sound.
#' @param protect_content (Optional). Protects the contents of the sent message
#' from forwarding and saving
#' @param reply_to_message_id (Optional). If the message is a reply, ID of the
#' original message.
#' @param allow_sending_without_reply (Optional). Pass \code{TRUE}, if the message
#' should be sent even if the specified replied-to message is not found
#' @param reply_markup (Optional). Object for an inline keyboard. If empty,
#' one 'Pay total price' button will be shown. If not empty,
#' the first button must be a Pay button.
#'     \itemize{}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'     }
#' @examples
#' \dontrun{
#' # Just send
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#'
#' bot$sendInvoice(
#'   chat_id = chat_id,
#'   title = 'title',
#'   description = 'description',
#'   payload = 'payload',
#'   provider_token = 'Your provider_token',
#'   currency = 'USD',
#'   label = 'label',
#'   amount = 145 # 1.45$
#' )
#'
#' # Bot
#'
#' token <- "RTelegramBot"
#' bot <- Bot(token = token)
#' updater <- Updater(token)
#'
#' Pay <- function(bot, update){
#'   bot$sendInvoice(
#'     chat_id = update$message$chat_id,
#'     title = 'title',
#'     description = 'description',
#'     payload = 'payload',
#'     provider_token = 'Your provider_token',
#'     currency = 'USD',
#'     label = 'label',
#'     amount = 145 # 1.45$
#'   )
#' }
#'
#' pay_cmd     <- CommandHandler('pay', Pay)
#'
#'
#' answerQuery <- function(bot, update) {
#'   chat_id <- update$pre_checkout_query$id
#'   bot$answerPreCheckoutQuery(chat_id)
#' }
#'
#' pay_cmd_handler     <- CommandHandler('pay', Pay)
#' checkout_handler <- PreCheckoutQueryHandler(answerQuery)
#'
#' updater <- updater +
#'   pay_cmd_handler + checkout_handler
#'
#' updater$start_polling()
#'
#' }
sendInvoice <- function(chat_id,
                        title,
                        description,
                        payload,
                        provider_token,
                        currency,
                        label,
                        amount,
                        max_tip_amount                = NULL,
                        suggested_tip_amounts         = NULL,
                        start_parameter               = NULL,
                        provider_data                 = NULL,
                        photo_url                     = NULL,
                        photo_size                    = NULL,
                        photo_width                   = NULL,
                        photo_height                  = NULL,
                        need_name                     = FALSE,
                        need_phone_number             = FALSE,
                        need_email                    = FALSE,
                        need_shipping_address         = FALSE,
                        send_phone_number_to_provider = FALSE,
                        send_email_to_provider        = FALSE,
                        is_flexible                   = FALSE,
                        disable_notification          = FALSE,
                        protect_content               = FALSE,
                        reply_to_message_id           = NULL,
                        allow_sending_without_reply   = FALSE,
                        reply_markup                  = NULL)
{
  url <- sprintf("%s/sendInvoice", private$base_url)

  data <- list(
    chat_id = chat_id,
    title = title,
    description = description,
    payload = payload,
    provider_token = provider_token,
    currency = currency,
    prices = data.frame(label = label, amount = amount)
  )

  if (!missing(max_tip_amount)) {
    data[["max_tip_amount"]] <- max_tip_amount
  }
  if (!missing(suggested_tip_amounts)) {
    data[["suggested_tip_amounts"]] <- suggested_tip_amounts
  }
  if (!missing(start_parameter)) {
    data[["start_parameter"]] <- start_parameter
  }
  if (!missing(provider_data)) {
    data[["provider_data"]] <- provider_data
  }
  if (!missing(photo_url)) {
    data[["photo_url"]] <- photo_url
  }
  if (!missing(photo_size)) {
    data[["photo_size"]] <- photo_size
  }
  if (!missing(photo_width)) {
    data[["photo_width"]] <- photo_width
  }
  if (!missing(photo_height)) {
    data[["photo_height"]] <- photo_height
  }
  if (!missing(need_name)) {
    data[["need_name"]] <- need_name
  }
  if (!missing(need_phone_number)) {
    data[["need_phone_number"]] <- need_phone_number
  }
  if (!missing(need_email)) {
    data[["need_email"]] <- need_email
  }
  if (!missing(need_shipping_address)) {
    data[["need_shipping_address"]] <- need_shipping_address
  }
  if (!missing(send_phone_number_to_provider)) {
    data[["send_phone_number_to_provider"]] <- send_phone_number_to_provider
  }
  if (!missing(send_email_to_provider)) {
    data[["send_email_to_provider"]] <- send_email_to_provider
  }
  if (!missing(is_flexible)) {
    data[["is_flexible"]] <- to_json(is_flexible)
  }
  if (!missing(disable_notification)) {
    data[["disable_notification"]] <- disable_notification
  }
  if (!missing(protect_content)) {
    data[["protect_content"]] <- protect_content
  }
  if (!missing(reply_to_message_id)) {
    data[["reply_to_message_id"]] <- reply_to_message_id
  }
  if (!missing(allow_sending_without_reply)) {
    data[["allow_sending_without_reply"]] <- allow_sending_without_reply
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }

  result <- private$request(url, data, encode = 'json')

  invisible(result)
}

#' Create Invoice Link
#'
#' Use this method to create a link for an invoice. Returns the created invoice
#' link as String on success.
#'
#' You can also use it's snake_case equivalent \code{create_invoice_link}.
#' @param title Product name.
#' @param description Product description.
#' @param payload Bot-defined invoice payload. This will not be displayed to
#' the user, use for your internal processes..
#' @param provider_token Payment provider token, obtained via BotFather
#' \href{(https://t.me/botfather)}.
#' @param currency Three-letter ISO 4217 currency code, see
#' \href{https://core.telegram.org/bots/payments#supported-currencies}.
#' @param label Portion label.
#' @param amount Price of the product in the smallest units of the currency
#' (integer, not float/double). For example, for a price of US$ 1.45 pass
#' amount = 145. See the exp parameter
#' in \href{https://core.telegram.org/bots/payments#supported-currencies}, it shows
#' the number of digits past the decimal point for each currency
#' (2 for the majority of currencies).
#' @param max_tip_amount (Optional). The maximum accepted amount for tips
#' in the smallest units of the currency (integer, not float/double).
#' For example, for a maximum tip of US$ 1.45 pass max_tip_amount = 145.
#' See the exp parameter.
#' in \href{https://core.telegram.org/bots/payments#supported-currencies}, it shows
#' the number of digits past the decimal point for each currency
#' (2 for the majority of currencies). Defaults to 0.
#' @param suggested_tip_amounts (Optional). List of suggested amounts of tips
#' in the smallest units of the currency (integer, not float/double).
#' At most 4 suggested tip amounts can be specified. The suggested tip amounts
#' must be positive, passed in a strictly increased order and must not exceed
#' max_tip_amount.
#' @param provider_data (Optional). Data about the invoice, which will be
#' shared with the payment provider. A detailed description of required fields
#' should be provided by the payment provider.
#' @param photo_url (Optional). URL of the product photo for the invoice.
#' Can be a photo of the goods or a marketing image for a service.
#' People like it better when they see what they are paying for.
#' @param photo_size (Optional). Photo size in bytes.
#' @param photo_width (Optional). Photo width.
#' @param photo_height (Optional). Photo height.
#' @param need_name (Optional). Pass \code{TRUE}, if you require the user's full
#' name to complete the order.
#' @param need_phone_number (Optional). Pass \code{TRUE}, if you require the user's
#' phone number to complete the order.
#' @param need_email (Optional). Pass \code{TRUE}, if you require the user's email
#' address to complete the order.
#' @param need_shipping_address (Optional). Pass \code{TRUE}, if you require
#' the user's shipping address to complete the order.
#' @param send_phone_number_to_provider (Optional). Pass \code{TRUE},
#' if the user's phone number should be sent to provider.
#' @param send_email_to_provider (Optional). Pass \code{TRUE},
#' if the user's email address should be sent to provider.
#' @param is_flexible (Optional). Pass \code{TRUE},
#' if the final price depends on the shipping method.
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#'
#' bot$createInvoiceLink(
#'   title = 'title',
#'   description = 'description',
#'   payload = 'payload',
#'   provider_token = 'Your provider_token',
#'   currency = 'USD',
#'   label = 'label',
#'   amount = 145 # 1.45$
#' )
#' }
createInvoiceLink <- function(title,
                              description,
                              payload,
                              provider_token,
                              currency,
                              label,
                              amount,
                              max_tip_amount                = NULL,
                              suggested_tip_amounts         = NULL,
                              provider_data                 = NULL,
                              photo_url                     = NULL,
                              photo_size                    = NULL,
                              photo_width                   = NULL,
                              photo_height                  = NULL,
                              need_name                     = FALSE,
                              need_phone_number             = FALSE,
                              need_email                    = FALSE,
                              need_shipping_address         = FALSE,
                              send_phone_number_to_provider = FALSE,
                              send_email_to_provider        = FALSE,
                              is_flexible                   = FALSE
                        )
{
  url <- sprintf("%s/createInvoiceLink", private$base_url)

  data <- list(
    title = title,
    description = description,
    payload = payload,
    provider_token = provider_token,
    currency = currency,
    prices = data.frame(label = label, amount = amount)
  )

  if (!missing(max_tip_amount)) {
    data[["max_tip_amount"]] <- max_tip_amount
  }
  if (!missing(suggested_tip_amounts)) {
    data[["suggested_tip_amounts"]] <- suggested_tip_amounts
  }
  if (!missing(provider_data)) {
    data[["provider_data"]] <- provider_data
  }
  if (!missing(photo_url)) {
    data[["photo_url"]] <- photo_url
  }
  if (!missing(photo_size)) {
    data[["photo_size"]] <- photo_size
  }
  if (!missing(photo_width)) {
    data[["photo_width"]] <- photo_width
  }
  if (!missing(photo_height)) {
    data[["photo_height"]] <- photo_height
  }
  if (!missing(need_name)) {
    data[["need_name"]] <- need_name
  }
  if (!missing(need_phone_number)) {
    data[["need_phone_number"]] <- need_phone_number
  }
  if (!missing(need_email)) {
    data[["need_email"]] <- need_email
  }
  if (!missing(need_shipping_address)) {
    data[["need_shipping_address"]] <- need_shipping_address
  }
  if (!missing(send_phone_number_to_provider)) {
    data[["send_phone_number_to_provider"]] <- send_phone_number_to_provider
  }
  if (!missing(send_email_to_provider)) {
    data[["send_email_to_provider"]] <- send_email_to_provider
  }
  if (!missing(is_flexible)) {
    data[["is_flexible"]] <- to_json(is_flexible)
  }

  result <- private$request(url, data, encode = 'json')

  invisible(result)
}

#' Info about Invoice
#'
#' This object contains basic information about an invoice.
#'
#' You can also use it's snake_case equivalent \code{invoice}.
#' @param title Product name.
#' @param description Product description.
#' @param start_parameter Unique bot deep-linking parameter that can be used to generate this invoice.
#' @param currency Three-letter ISO 4217 currency code, see
#' \href{https://core.telegram.org/bots/payments#supported-currencies}.
#' @param total_amount Price of the product in the smallest units of the currency
#' (integer, not float/double). For example, for a price of US$ 1.45 pass
#' amount = 145. See the exp parameter.
#' in \href{https://core.telegram.org/bots/payments#supported-currencies}, it shows
#' the number of digits past the decimal point for each currency
#' (2 for the majority of currencies).
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#'
#' bot$Invoice(
#'   title = 'title',
#'   description = 'description',
#'   start_parameter = 'start_parameter',
#'   currency = 'USD',
#'   total_amount = 145 # 1.45$
#' )
#' }
Invoice <- function(title,
                    description,
                    start_parameter,
                    currency,
                    total_amount
)
{
  url <- sprintf("%s/Invoice", private$base_url)

  data <- list(
    title = title,
    description = description,
    start_parameter = start_parameter,
    currency = currency,
    total_amount = total_amount
  )
  result <- private$request(url, data, encode = 'json')
  invisible(result)
}

#' Successful payment
#'
#' This object contains basic information about a successful payment.
#'
#' You can also use it's snake_case equivalent \code{successful_payment}.
#' @param currency Three-letter ISO 4217 currency code, see
#' \href{https://core.telegram.org/bots/payments#supported-currencies}.
#' @param total_amount Price of the product in the smallest units of the currency
#' (integer, not float/double). For example, for a price of US$ 1.45 pass
#' amount = 145. See the exp parameter.
#' in \href{https://core.telegram.org/bots/payments#supported-currencies}, it shows
#' the number of digits past the decimal point for each currency
#' (2 for the majority of currencies).
#' @param invoice_payload Bot specified invoice payload.
#' @param telegram_payment_charge_id Telegram payment identifier.
#' @param provider_payment_charge_id Provider payment identifier.
#' @param shipping_option_id (Optional). Identifier of the shipping option chosen by the user.
#' @param name (Optional). User name. It needs to OrderInfo.
#' @param phone_number (Optional). User's phone number. It needs to OrderInfo.
#' @param email (Optional). User email. It needs to OrderInfo.
#' @param country_code (Optional). Two-letter ISO 3166-1 alpha-2 country code. It needs to ShippingAddress.
#' @param state (Optional). State, if applicable. It needs to ShippingAddress.
#' @param city (Optional). City. It needs to ShippingAddress.
#' @param street_line1 (Optional). First line for the address. It needs to ShippingAddress.
#' @param street_line2 (Optional). Second line for the address. It needs to ShippingAddress.
#' @param post_code (Optional). Address post code. It needs to ShippingAddress.
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#'
#' bot$SuccessfulPayment(
#'                       currency = 'USD',
#'                       total_amount = 145, # 1.45$
#'                       invoice_payload = 'invoice_payload',
#'                       telegram_payment_charge_id = 'telegram_payment_charge_id',
#'                       provider_payment_charge_id = 'provider_payment_charge_id'
#'                       )
#' }
SuccessfulPayment <- function(currency,
                              total_amount,
                              invoice_payload,
                              telegram_payment_charge_id,
                              provider_payment_charge_id,
                              shipping_option_id = NULL,
                              name = NULL,
                              phone_number = NULL,
                              email = NULL,
                              country_code = NULL,
                              state = NULL,
                              city = NULL,
                              street_line1 = NULL,
                              street_line2 = NULL,
                              post_code = NULL
)
{
  url <- sprintf("%s/SuccessfulPayment", private$base_url)

  data <- list(
    currency = currency,
    total_amount = total_amount,
    invoice_payload = invoice_payload,
    telegram_payment_charge_id = telegram_payment_charge_id,
    provider_payment_charge_id = provider_payment_charge_id
  )
  if (!missing(shipping_option_id)) {
    data[["shipping_option_id"]] <- shipping_option_id
  }
  if ((!missing(name)) & (!missing(phone_number)) & (!missing(email)) & (!missing(country_code)) & (!missing(state)) & (!missing(city)) & (!missing(street_line1)) & (!missing(street_line2)) & (!missing(post_code))) {
    data[["order_info"]] <- list(
      name = name,
      phone_number = phone_number,
      email = email,
      shipping_address = list(
        country_code = country_code,
        state = state,
        city = city,
        street_line1 = street_line1,
        street_line2 = street_line2,
        post_code = post_code
      )
    )
  }
  result <- private$request(url, data, encode = 'json')
  invisible(result)
}


#' Get a user's profile photos
#'
#' Use this method to get a list of profile pictures for a user.
#'
#' You can also use it's snake_case equivalent \code{get_user_profile_photos}.
#'
#' See \code{\link{getFile}} to know how to download files.
#' @param user_id Unique identifier of the target user.
#' @param offset (Optional). Sequential number of the first photo to be
#'     returned. By default, all photos are returned.
#' @param limit (Optional). Limits the number of photos to be retrieved. Values
#'     between 1-100 are accepted. Defaults to 100.
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#'
#' photos <- bot$getUserProfilePhotos(chat_id = chat_id)
#' }
getUserProfilePhotos <- function(user_id,
                                 offset = NULL,
                                 limit = 100L) {
  url <- sprintf("%s/getUserProfilePhotos", private$base_url)

  data <- list(user_id = user_id)

  if (!missing(offset)) {
    data[["offset"]] <- offset
  }
  if (!missing(limit)) {
    data[["limit"]] <- limit
  }

  result <- private$request(url, data)

  invisible(result)
}


#' Prepare a file for downloading
#'
#' Use this method to get basic info about a file and prepare it for
#' downloading. For the moment, bots can download files of up to 20MB in size.
#' It is guaranteed that the link will be valid for at least 1 hour. When the
#' link expires, a new one can be requested by calling \code{getFile} again.
#'
#' You can also use it's snake_case equivalent \code{get_file}.
#' @param file_id The file identifier.
#' @param destfile (Optional). If you want to save the file, pass by a
#'     character string with the name where the downloaded file is saved.
#'     See the \code{destfile} parameter from \code{?curl::curl_download} for
#'     further details.
#' @param ... (Optional). Additional parameters to be passed to
#'     \code{\link[curl]{curl_download}}. It is not used if \code{destfile} is
#'     \code{NULL}.
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#' chat_id <- user_id("Me")
#'
#' photos <- bot$getUserProfilePhotos(chat_id = chat_id)
#'
#' # Download user profile photo
#' file_id <- photos$photos[[1L]][[1L]]$file_id
#' bot$getFile(file_id, destfile = "photo.jpg")
#' }
getFile <- function(file_id,
                    destfile = NULL, ...) { # nocov start
  url <- sprintf("%s/getFile", private$base_url)

  data <- list(file_id = file_id)

  result <- private$request(url, data)

  if (!is.null(destfile)) {
    file_url <- sprintf("%s/%s", private$base_file_url, result$file_path)
    curl::curl_download(file_url, destfile, ...)
  }

  invisible(result)
} # nocov end


#' Send answers to callback queries
#'
#' Use this method to send answers to callback queries sent from inline
#' keyboards. The answer will be displayed to the user as a notification at the
#' top of the chat screen or as an alert. On success, \code{TRUE} is returned.
#'
#' You can also use it's snake_case equivalent \code{answer_callback_query}.
#' @param callback_query_id Unique identifier for the query to be answered.
#' @param text (Optional). Text of the notification. If not specified, nothing
#'     will be shown to the user, 0-200 characters.
#' @param show_alert (Optional). If \code{TRUE}, an alert will be shown by the
#'     client instead of a notification at the top of the chat screen. Defaults
#'     to \code{FALSE}.
#' @param url (Optional). URL that will be opened by the user's client.
#' @param cache_time (Optional). The maximum amount of time in seconds that the
#'     result of the callback query may be cached client-side. Telegram apps
#'     will support caching starting in version 3.14. Defaults to 0.
#' @examples
#' \dontrun{
#'     answerQuery <- function(bot, update) {
#'   chat_id <- update$pre_checkout_query$id
#'   bot$answerPreCheckoutQuery(chat_id)
#' }
#'
#' checkout_handler <- PreCheckoutQueryHandler(answerQuery)
#' }
answerCallbackQuery <- function(callback_query_id,
                                text = NULL,
                                show_alert = FALSE,
                                url = NULL,
                                cache_time = NULL) { # nocov start
  url_ <- sprintf("%s/answerCallbackQuery", private$base_url)

  data <- list(callback_query_id = callback_query_id)

  if (!missing(text)) {
    data[["text"]] <- text
  }
  if (!missing(show_alert)) {
    data[["show_alert"]] <- show_alert
  }
  if (!missing(url)) {
    data[["url"]] <- url
  }
  if (!missing(cache_time)) {
    data[["cache_time"]] <- cache_time
  }

  result <- private$request(url_, data)

  invisible(result)
} # nocov end



#' Send answers to precheckout queries
#'
#' Once the user has confirmed their payment and shipping details, the Bot API sends the final confirmation in the form of an Update with the field pre_checkout_query. Use this method to respond to such pre-checkout queries. On success, True is returned. Note: The Bot API must receive an answer within 10 seconds after the pre-checkout query was sent.
#'
#' You can also use it's snake_case equivalent \code{answer_precheckout_query}.
#' @param pre_checkout_query_id Unique identifier for the query to be answered.
#' @param ok Specify \code{TRUE} if everything is alright (goods are available, etc.) and the bot is ready to proceed with the order. Use \code{False} if there are any problems.
#' @param error_message (Optional). Required if \code{ok} is \code{FALSE}. Error message in human readable form that explains the reason for failure to proceed with the checkout (e.g. "Sorry, somebody just bought the last of our amazing black T-shirts while you were busy filling out your payment details. Please choose a different color or garment!"). Telegram will display this message to the user.
#' @examples
#' \dontrun{
#'  answerPreCheckQuery <- function(bot, update) {
#'   id <- update$pre_checkout_query$id
#'   bot$answerPreCheckoutQuery(pre_checkout_query_id = id, ok = TRUE)
#' }
#'
#' checkout_handler <- PreCheckoutQueryHandler(answerPreCheckQuery)
#' }
answerPreCheckoutQuery <- function(pre_checkout_query_id,
                                   ok = TRUE,
                                   error_message = NULL) { # nocov start

  url_ <- sprintf("%s/answerPreCheckoutQuery", private$base_url)

  data <- list(pre_checkout_query_id = pre_checkout_query_id,
               ok = ok)

    if (!missing(error_message)) {
    data[["error_message"]] <- error_message
  }

  result <- private$request(url_, data, encode = 'json')

  invisible(result)
}

#' Send answers to shipping queries (Unfortunaly, doesn't work)
#'
#' If you sent an invoice requesting a shipping address and the parameter is_flexible was specified, the Bot API will send an Update with a shipping_query field to the bot. Use this method to reply to shipping queries. On success, True is returned.
#'
#' You can also use it's snake_case equivalent \code{answer_shipping_query}.
#' @param shipping_query_id Unique identifier for the query to be answered.
#' @param ok Specify \code{TRUE} if delivery to the specified address is possible and \code{False} if there are any problems (for example, if delivery to the specified address is not possible).
#' @param title (Optional). Option title. It is need to shipping_options. Required if \code{ok} is \code{True}. A JSON-serialized array of available shipping options. Need activate @param title, @param label, @param amount .
#' @param label (Optional). Portion label. It is need to shipping_options. Required if \code{ok} is \code{True}. A JSON-serialized array of available shipping options. Need activate @param title, @param label, @param amount .
#' @param amount (Optional). Price of the product in the smallest units of the currency. It is need to shipping_options. Required if \code{ok} is \code{True}. A JSON-serialized array of available shipping options. Need activate @param title, @param label, @param amount .
#' @param error_message (Optional). Required if \code{ok} is \code{FALSE}. Error message in human readable form that explains why it is impossible to complete the order (e.g. "Sorry, delivery to your desired address is unavailable'). Telegram will display this message to the user.
#' @examples
#' \dontrun{
#' answerShipQuery <- function(bot, update) {
#'   id <- update$pre_checkout_query$id
#'   bot$answerShippingQuery(shipping_query_id = id, ok = FALSE, error_message = "Sorry, we can't send to this country")
#' }
#'
#' shipping_handler <- ShippingQueryHandler(answerShipQuery)
#' }
answerShippingQuery <- function(shipping_query_id,
                                ok = TRUE,
                                title = NULL,
                                label = NULL,
                                amount = NULL,
                                error_message = NULL) { # nocov start

  url_ <- sprintf("%s/answerShippingQuery", private$base_url)



  data <- list(shipping_query_id = shipping_query_id,
               ok = ok)

  if ((!missing(title)) & (!missing(label)) & (!missing(amount))) {
    data[["shipping_options"]] <- list(
      id     = shipping_query_id,
      title  = title,
      prices = data.frame(label = label, amount = amount)
    )
  }

  if (!missing(error_message)) {
    data[["error_message"]] <- error_message
  }

  result <- private$request(url_, data, encode = 'json')

  invisible(result)
}




#' Send answers to an inline query
#'
#' Use this method to send answers to an inline query. No more than 50 results
#' per query are allowed.
#'
#' To enable this option, send the \code{/setinline} command to
#' \href{https://t.me/botfather}{@BotFather} and provide the placeholder text
#' that the user will see in the input field after typing your bot's name.
#'
#' You can also use it's snake_case equivalent \code{answer_inline_query}.
#' @param inline_query_id Unique identifier for the answered query.
#' @param results A list of \code{\link{InlineQueryResult}} for the inline
#'     query.
#' @param cache_time (Optional). The maximum amount of time in seconds that the
#'     result of the inline query may be cached on the server.
#' @param is_personal (Optional). Pass \code{TRUE}, if results may be cached on
#'     the server side only for the user that sent the query. By default,
#'     results may be returned to any user who sends the same query.
#' @param next_offset (Optional). Pass the offset that a client should send in
#'     the next query with the same text to receive more results. Pass an empty
#'     string if there are no more results or if you don't support pagination.
#'     Offset length can't exceed 64 bytes.
#' @param switch_pm_text (Optional). If passed, clients will display a button
#'     with specified text that switches the user to a private chat with the
#'     bot and sends the bot a start message with the parameter
#'     \code{switch_pm_parameter}.
#' @param switch_pm_parameter (Optional). Deep-linking parameter for the
#'     \code{/start} message sent to the bot when user presses the switch
#'     button. 1-64 characters, only \code{A-Z}, \code{a-z}, \code{0-9},
#'     \code{_} and \code{-} are allowed.
#'
#'     \emph{Example:} An inline bot that sends YouTube videos can ask the user
#'     to connect the bot to their YouTube account to adapt search results
#'     accordingly. To do this, it displays a 'Connect your YouTube account'
#'     button above the results, or even before showing any. The user presses
#'     the button, switches to a private chat with the bot and, in doing so,
#'     passes a start parameter that instructs the bot to return an auth link.
#'     Once done, the bot can offer a switch_inline button so that the user can
#'     easily return to the chat where they wanted to use the bot's inline
#'     capabilities.
answerInlineQuery <- function(inline_query_id,
                              results,
                              cache_time = 300L,
                              is_personal = NULL,
                              next_offset = NULL,
                              switch_pm_text = NULL,
                              switch_pm_parameter = NULL) { # nocov start
  url <- sprintf("%s/answerInlineQuery", private$base_url)

  # results <- to_json(results)

  data <- list(inline_query_id = inline_query_id, results = results)

  if (!missing(cache_time)) {
    data[["cache_time"]] <- cache_time
  }
  if (!missing(is_personal)) {
    data[["is_personal"]] <- is_personal
  }
  if (!is.null(next_offset)) {
    data[["next_offset"]] <- next_offset
  }
  if (!missing(switch_pm_text)) {
    data[["switch_pm_text"]] <- switch_pm_text
  }
  if (!missing(switch_pm_parameter)) {
    data[["switch_pm_parameter"]] <- switch_pm_parameter
  }

  result <- private$request(url, data, encode = 'json')

  invisible(result)
} # nocov end


#' Edit a text message
#'
#' Use this method to edit text messages.
#'
#' You can also use it's snake_case equivalent
#' \code{edit_message_text}.
#' @param chat_id (Optional). Unique identifier for the target chat or username
#'     of the target channel.
#' @param message_id (Optional). Required if inline_message_id is not
#'     specified. Identifier of the sent message.
#' @param inline_message_id (Optional). Required if chat_id and message_id are
#'     not specified. Identifier of the inline message.
#' @param text New text of the message.
#' @param parse_mode (Optional). Send 'Markdown' or 'HTML', if you want
#'     Telegram apps to show bold, italic, fixed-width text or inline URLs in
#'     your bot's message.
#' @param disable_web_page_preview (Optional). Disables link previews for links
#'     in this message.
#' @param reply_markup (Optional). A Reply Markup parameter object, it can be
#'     either:
#'     \itemize{
#'      \item{\code{\link{ReplyKeyboardMarkup}}}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'      \item{\code{\link{ReplyKeyboardRemove}}}
#'      \item{\code{\link{ForceReply}}}
#'    }
editMessageText <- function(chat_id = NULL,
                            message_id = NULL,
                            inline_message_id = NULL,
                            text,
                            parse_mode = NULL,
                            disable_web_page_preview = NULL,
                            reply_markup = NULL) { # nocov start
  if (is.null(inline_message_id) & (is.null(chat_id) | is.null(message_id))) {
    stop(
      "Both `chat_id` and `message_id` are required ",
      "when `inline_message_id` is not specified."
    )
  }

  url <- sprintf("%s/editMessageText", private$base_url)

  data <- list(
    text = text
  )

  if (!missing(chat_id)) {
    data[["chat_id"]] <- chat_id
  }
  if (!missing(message_id)) {
    data[["message_id"]] <- message_id
  }
  if (!missing(inline_message_id)) {
    data[["inline_message_id"]] <- inline_message_id
  }
  if (!missing(parse_mode)) {
    data[["parse_mode"]] <- parse_mode
  }
  if (!missing(disable_web_page_preview)) {
    data[["disable_web_page_preview"]] <- disable_web_page_preview
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }

  result <- private$request(url, data)

  invisible(result)
} # nocov end


#' Edit a caption
#'
#' Use this method to edit captions of messages.
#'
#' You can also use it's snake_case equivalent
#' \code{edit_message_caption}.
#' @param chat_id (Optional). Unique identifier for the target chat or username
#'     of the target channel.
#' @param message_id (Optional). Required if inline_message_id is not
#'     specified. Identifier of the sent message.
#' @param inline_message_id (Optional). Required if chat_id and message_id are
#'     not specified. Identifier of the inline message.
#' @param caption (Optional). New caption of the message.
#' @param parse_mode (Optional). Send 'Markdown' or 'HTML', if you want
#'     Telegram apps to show bold, italic, fixed-width text or inline URLs in
#'     your bot's message.
#' @param reply_markup (Optional). A Reply Markup parameter object, it can be
#'     either:
#'     \itemize{
#'      \item{\code{\link{ReplyKeyboardMarkup}}}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'      \item{\code{\link{ReplyKeyboardRemove}}}
#'      \item{\code{\link{ForceReply}}}
#'    }
editMessageCaption <- function(chat_id = NULL,
                               message_id = NULL,
                               inline_message_id = NULL,
                               caption = NULL,
                               parse_mode = NULL,
                               reply_markup = NULL) { # nocov start
  if (is.null(inline_message_id) & (is.null(chat_id) | is.null(message_id))) {
    stop(
      "Both `chat_id` and `message_id` are required ",
      "when `inline_message_id` is not specified."
    )
  }

  url <- sprintf("%s/editMessageCaption", private$base_url)

  data <- list()

  if (!missing(chat_id)) {
    data[["chat_id"]] <- chat_id
  }
  if (!missing(message_id)) {
    data[["message_id"]] <- message_id
  }
  if (!missing(inline_message_id)) {
    data[["inline_message_id"]] <- inline_message_id
  }
  if (!missing(caption)) {
    data[["caption"]] <- caption
  }
  if (!missing(parse_mode)) {
    data[["parse_mode"]] <- parse_mode
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }

  result <- private$request(url, data)

  invisible(result)
} # nocov end


#' Edit a reply markup
#'
#' Use this method to edit only the reply markup of messages sent by the bot or
#' via the bot (for inline bots).
#'
#' You can also use it's snake_case equivalent
#' \code{edit_message_reply_markup}.
#' @param chat_id (Optional). Unique identifier for the target chat or username
#'     of the target channel.
#' @param message_id (Optional). Required if inline_message_id is not
#'     specified. Identifier of the sent message.
#' @param inline_message_id (Optional). Required if chat_id and message_id are
#'     not specified. Identifier of the inline message.
#' @param reply_markup (Optional). A Reply Markup parameter object, it can be
#'     either:
#'     \itemize{
#'      \item{\code{\link{ReplyKeyboardMarkup}}}
#'      \item{\code{\link{InlineKeyboardMarkup}}}
#'      \item{\code{\link{ReplyKeyboardRemove}}}
#'      \item{\code{\link{ForceReply}}}}
editMessageReplyMarkup <- function(chat_id = NULL,
                                   message_id = NULL,
                                   inline_message_id = NULL,
                                   reply_markup = NULL) { # nocov start
  if (is.null(inline_message_id) & (is.null(chat_id) | is.null(message_id))) {
    stop(
      "Both `chat_id` and `message_id` are required ",
      "when `inline_message_id` is not specified."
    )
  }

  url <- sprintf("%s/editMessageReplyMarkup", private$base_url)

  data <- list()

  if (!missing(chat_id)) {
    data[["chat_id"]] <- chat_id
  }
  if (!missing(message_id)) {
    data[["message_id"]] <- message_id
  }
  if (!missing(inline_message_id)) {
    data[["inline_message_id"]] <- inline_message_id
  }
  if (!missing(reply_markup)) {
    data[["reply_markup"]] <- to_json(reply_markup)
  }

  result <- private$request(url, data)

  invisible(result)
} # nocov end


#' Receive incoming updates
#'
#' Use this method to receive incoming updates. It returns a
#' list of \code{\link{Update}} objects.
#'
#' 1. This method will not work if an outgoing webhook is set up.
#'
#' 2. In order to avoid getting duplicate updates, recalculate offset after
#' each server response or use \code{Bot} method \code{\link{clean_updates}}.
#'
#' 3. To take full advantage of this library take a look at
#' \code{\link{Updater}}.
#'
#' You can also use it's snake_case equivalent \code{get_updates}.
#' @param offset (Optional). Identifier of the first update to be returned.
#' @param limit (Optional). Limits the number of updates to be retrieved.
#'     Values between 1-100 are accepted. Defaults to 100.
#' @param timeout (Optional). Timeout in seconds for long polling. Defaults to
#'     0, i.e. usual short polling. Should be positive, short polling should
#'     be used for testing purposes only.
#' @param allowed_updates (Optional). String or vector of strings with the
#'     types of updates you want your bot to receive. For example, specify
#'     \code{c("message", "edited_channel_post", "callback_query")} to only
#'     receive updates of these types. See
#'     \href{https://core.telegram.org/bots/api#update}{Update}
#'     for a complete list of available update types. Specify an empty string
#'     to receive all updates regardless of type (default). If not specified,
#'     the previous setting will be used.
#'
#'     Please note that this parameter doesn't affect updates created before
#'     the call to the getUpdates, so unwanted updates may be received for a
#'     short period of time.
#' @examples
#' \dontrun{
#' bot <- Bot(token = bot_token("RTelegramBot"))
#'
#' updates <- bot$getUpdates()
#' }
getUpdates <- function(offset = NULL,
                       limit = 100L,
                       timeout = 0L,
                       allowed_updates = NULL) {
  url <- sprintf("%s/getUpdates", private$base_url)

  data <- list(timeout = timeout)

  if (!missing(offset)) {
    data[["offset"]] <- offset
  }
  if (!missing(limit)) {
    data[["limit"]] <- limit
  }
  if (!missing(allowed_updates) && !is.null(allowed_updates)) {
    data[["allowed_updates"]] <- I(allowed_updates)
  }

  result <- private$request(url, data)

  invisible(lapply(result, function(u) Update(u)))
}


#' Set a webhook
#'
#' Use this method to specify a url and receive incoming updates via an
#' outgoing webhook. Whenever there is an update for the bot, we will send an
#' HTTPS POST request to the specified url, containing a JSON-serialized
#' \href{https://core.telegram.org/bots/api#update}{Update}.
#'
#' If you'd like to make sure that the webhook request comes from Telegram, we
#' recommend using a secret path in the URL, e.g.
#' \code{https://www.example.com/<token>}.
#'
#' You can also use it's snake_case equivalent \code{set_webhook}.
#' @param url HTTPS url to send updates to. Use an empty string to remove
#'     webhook integration.
#' @param certificate (Optional). Upload your public key certificate so that
#'     the root certificate in use can be checked. See Telegram's
#'     \href{https://core.telegram.org/bots/self-signed}{self-signed guide} for
#'     details.
#' @param max_connections (Optional). Maximum allowed number of simultaneous
#'     HTTPS connections to the webhook for update delivery, 1-100. Defaults to
#'     40. Use lower values to limit the load on your bot's server, and higher
#'     values to increase your bot's throughput.
#' @param allowed_updates (Optional). String or vector of strings with the
#'     types of updates you want your bot to receive. For example, specify
#'     \code{c("message", "edited_channel_post", "callback_query")} to only
#'     receive updates of these types. See
#'     \href{https://core.telegram.org/bots/api#update}{Update}
#'     for a complete list of available update types. Specify an empty string
#'     to receive all updates regardless of type (default). If not specified,
#'     the previous setting will be used.
#'
#'     Please note that this parameter doesn't affect updates created before
#'     the call to the get_updates, so unwanted updates may be received for a
#'     short period of time.
setWebhook <- function(url = NULL,
                       certificate = NULL,
                       max_connections = 40L,
                       allowed_updates = NULL) {
  url_ <- sprintf("%s/setWebhook", private$base_url)

  data <- list()

  if (!missing(url)) {
    data[["url"]] <- url
  }
  if (!missing(certificate)) {
    if (file.exists(certificate)) { # nocov
      data[["certificate"]] <- httr::upload_file(certificate) # nocov
    } else {
      data[["certificate"]] <- certificate # nocov
    }
  }
  if (!missing(max_connections)) {
    data[["max_connections"]] <- max_connections
  }
  if (!missing(allowed_updates) && !is.null(allowed_updates)) {
    data[["allowed_updates"]] <- I(allowed_updates)
  }

  result <- private$request(url_, data)

  invisible(result)
}


#' Remove webhook integration
#'
#' Use this method to remove webhook integration if you decide to switch back
#' to \code{getUpdates}. Requires no parameters.
#'
#' You can also use it's snake_case equivalent \code{delete_webhook}.
deleteWebhook <- function() {
  url <- sprintf("%s/deleteWebhook", private$base_url)

  data <- list()

  result <- private$request(url, data)

  invisible(result)
}


#' Get current webhook status
#'
#' Use this method to get current webhook status. Requires no parameters.
#'
#' If the bot is using \code{getUpdates}, will return an object with the url
#' field empty.
#'
#' You can also use it's snake_case equivalent \code{get_webhook_info}.
getWebhookInfo <- function() {
  url <- sprintf("%s/getWebhookInfo", private$base_url)

  data <- list()

  result <- private$request(url, data)

  invisible(result)
}

#' Leave a chat
#'
#' Use this method for your bot to leave a group, supergroup or channel.
#'
#' You can also use it's snake_case equivalent \code{leave_chat}.
#' @param chat_id Unique identifier for the target chat or username of
#'     the target channel.
leaveChat <- function(chat_id) { # nocov start
  url <- sprintf("%s/leaveChat", private$base_url)

  data <- list(chat_id = chat_id)

  result <- private$request(url, data)

  invisible(result)
} # nocov end


#### OTHER METHODS ####

#' Clean any pending updates
#'
#' Use this method to clean any pending updates on Telegram servers.
#' Requires no parameters.
clean_updates <- function() {
  updates <- self$get_updates()

  if (length(updates)) {
    self$get_updates(updates[[length(updates)]]$update_id + 1L) # nocov
  }

  invisible(NULL)
}


#' Change your bot's auth token
#'
#' Use this method to change your bot's auth token.
#' @param token The bot's token given by the \emph{BotFather}.
set_token <- function(token) {
  if (!missing(token)) {
    private$token <- token
  }

  invisible(NULL)
}


### CLASS ####

#' Creating a Bot
#'
#' This object represents a Telegram Bot.
#'
#' To take full advantage of this library take a look at \code{\link{Updater}}.
#'
#' You can also use its methods \code{snake_case} equivalent.
#' @section API Methods: \describe{
#'     \item{\code{\link{answerCallbackQuery}}}{Send
#'     answers to callback queries}
#'     \item{\code{\link{answerInlineQuery}}}{Send answers to an inline query}
#'     \item{\code{\link{answerShippingQuery}}}{Send answers to an shipping query}
#'     \item{\code{\link{answerPreCheckoutQuery}}}{Send answers to an pre checkout query}
#'     \item{\code{\link{deleteMessage}}}{Delete a message}
#'     \item{\code{\link{deleteWebhook}}}{Remove webhook integration}
#'     \item{\code{\link{editMessageText}}}{Edit a text message}
#'     \item{\code{\link{editMessageCaption}}}{Edit a caption}
#'     \item{\code{\link{editMessageReplyMarkup}}}{Edit the reply
#'     markup of a message}
#'     \item{\code{\link{forwardMessage}}}{Forward messages of any
#'     kind}
#'     \item{\code{\link{getFile}}}{Prepare a file for downloading}
#'     \item{\code{\link{getMe}}}{Check your bot's information}
#'     \item{\code{\link{getUpdates}}}{Receive incoming
#'     updates}
#'     \item{\code{\link{getUserProfilePhotos}}}{Get a user's profile photos}
#'     \item{\code{\link{getWebhookInfo}}}{Get current webhook status}
#'     \item{\code{\link{leaveChat}}}{Leave a chat}
#'     \item{\code{\link{sendAnimation}}}{Send animation files}
#'     \item{\code{\link{sendAudio}}}{Send audio files}
#'     \item{\code{\link{sendChatAction}}}{Send a chat action}
#'     \item{\code{\link{sendDocument}}}{Send general files}
#'     \item{\code{\link{sendLocation}}}{Send point on the map}
#'     \item{\code{\link{sendMessage}}}{Send text messages}
#'     \item{\code{\link{sendPhoto}}}{Send image files}
#'     \item{\code{\link{sendSticker}}}{Send a sticker}
#'     \item{\code{\link{sendVideo}}}{Send a video}
#'     \item{\code{\link{sendVideoNote}}}{Send video messages}
#'     \item{\code{\link{sendVoice}}}{Send voice files}
#'     \item{\code{\link{sendInvoice}}}{Send invoice}
#'     \item{\code{\link{setWebhook}}}{Set a webhook}
#'     \item{\code{\link{createInvoiceLink}}}{Create a Invoice Link}
#' }
#' @section Other Methods: \describe{
#'     \item{\code{\link{clean_updates}}}{Clean any pending updates}
#'     \item{\code{\link{set_token}}}{Change your bot's auth token}
#' }
#' @docType class
#' @format An \code{\link{R6Class}} object.
#' @param token The bot's token given by the \emph{BotFather}.
#' @param base_url (Optional). Telegram Bot API service URL.
#' @param base_file_url (Optional). Telegram Bot API file URL.
#' @param request_config (Optional). Additional configuration settings
#'     to be passed to the bot's POST requests. See the \code{config}
#'     parameter from \code{?httr::POST} for further details.
#'
#'     The \code{request_config} settings are very
#'     useful for the advanced users who would like to control the
#'     default timeouts and/or control the proxy used for HTTP communication.
#' @examples
#' \dontrun{
#' bot <- Bot(token = "TOKEN")
#'
#' # In case you want to set a proxy (see ?httr:use_proxy)
#' bot <- Bot(
#'   token = "TOKEN",
#'   request_config = httr::use_proxy(...)
#' )
#' }
#' @export
Bot <- function(token,
                base_url = NULL,
                base_file_url = NULL,
                request_config = NULL) {
  BotClass$new(token, base_url, base_file_url, request_config)
}


BotClass <- R6::R6Class("Bot",
  inherit = TelegramObject,
  public = list(
    initialize = function(token, base_url, base_file_url, request_config) {
      private$token <- private$validate_token(token)

      if (is.null(base_url)) {
        base_url <- "https://api.telegram.org/bot"
      }
      if (is.null(base_file_url)) {
        base_file_url <- "https://api.telegram.org/file/bot"
      }
      if (is.null(request_config)) {
        request_config <- list()
      }

      private$base_url <- paste0(
        as.character(base_url),
        as.character(private$token)
      )
      private$base_file_url <- paste0(
        as.character(base_file_url),
        as.character(private$token)
      )
      private$request_config <- request_config
    },
    print = .print,

    # API Methods
    getMe = getMe,
    get_me = getMe,
    sendMessage = sendMessage,
    send_message = sendMessage,
    deleteMessage = deleteMessage,
    delete_message = deleteMessage,
    forwardMessage = forwardMessage,
    forward_message = forwardMessage,
    sendPhoto = sendPhoto,
    send_photo = sendPhoto,
    sendAudio = sendAudio,
    send_audio = sendAudio,
    sendDocument = sendDocument,
    send_document = sendDocument,
    sendSticker = sendSticker,
    send_sticker = sendSticker,
    sendVideo = sendVideo,
    send_video = sendVideo,
    sendVideoNote = sendVideoNote,
    send_video_note = sendVideoNote,
    sendAnimation = sendAnimation,
    send_animation = sendAnimation,
    sendVoice = sendVoice,
    send_voice = sendVoice,
    sendLocation = sendLocation,
    send_location = sendLocation,
    sendChatAction = sendChatAction,
    send_chat_action = sendChatAction,
    getUserProfilePhotos = getUserProfilePhotos,
    get_user_profile_photos = getUserProfilePhotos,
    getFile = getFile,
    get_file = getFile,
    sendInvoice = sendInvoice,
    send_invoice = sendInvoice,
    Invoice = Invoice,
    invoice = Invoice,
    SuccessfulPayment = SuccessfulPayment,
    successful_payment = SuccessfulPayment,
    createInvoiceLink = createInvoiceLink,
    create_invoice_link = createInvoiceLink,
    answerCallbackQuery = answerCallbackQuery,
    answer_callback_query = answerCallbackQuery,
    answerPreCheckoutQuery = answerPreCheckoutQuery,
    answer_pre_checkout_query = answerPreCheckoutQuery,
    answerShippingQuery = answerShippingQuery,
    answer_shipping_query = answerShippingQuery,
    answerInlineQuery = answerInlineQuery,
    answer_inline_query = answerInlineQuery,
    editMessageText = editMessageText,
    edit_message_text = editMessageText,
    editMessageCaption = editMessageCaption,
    edit_message_caption = editMessageCaption,
    editMessageReplyMarkup = editMessageReplyMarkup,
    edit_message_reply_markup = editMessageReplyMarkup,
    getUpdates = getUpdates,
    get_updates = getUpdates,
    setWebhook = setWebhook,
    set_webhook = setWebhook,
    deleteWebhook = deleteWebhook,
    delete_webhook = deleteWebhook,
    getWebhookInfo = getWebhookInfo,
    get_webhook_info = getWebhookInfo,
    leaveChat = leaveChat,
    leave_chat = leaveChat,

    # Other Methods
    clean_updates = clean_updates,
    set_token = set_token
  ),
  private = list(
    # Params
    token = NULL,
    base_url = NULL,
    base_file_url = NULL,
    request_config = NULL,

    # Internal Methods
    validate_token = .validate_token,
    request = .request,
    parse = .parse
  )
)

#' @rdname Bot
#' @param x Object to be tested.
#' @export
is.Bot <- function(x) {
  inherits(x, "Bot")
}
