# telegram.bot.dt
 For Telegram Bot API in R

The goal of telegram.bot.dt is to use Telegram Bot API in R. This package inherits "telegram.bot" library. In additional, it contains "Payments" module.

## Installation

You can install the development version of telegram.bot.dt from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TSjB/telegram.bot.dt")

```
## Example

This is a basic example which shows you how to solve a common problem:

```{r example}
library(telegram.bot.dt)

 token <- "your bot's token"
 bot <- Bot(token = token)
 updater <- Updater(token)

say_hello <- function(bot, update) {

  user_name <- update$message$from$first_name

  bot$sendMessage(update$message$chat_id, 
                  text = paste0("Hello, ", user_name, "!"), 
                  parse_mode = "Markdown")

}

hi_hendler <- CommandHandler('hi', say_hello)

updater <- updater + hi_hendler

updater$start_polling()
```
