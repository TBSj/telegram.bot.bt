# telegram.bot.bt
 For Telegram Bot API in R

The goal of telegram.bot.bt is to use Telegram Bot API in R. This package inherits "telegram.bot" library. In additional, it also contains "Payment" and "Inline query" modules.

## Installation

You can install the development version of telegram.bot.dt from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TBSj/telegram.bot.bt")

```
## Example

This is a basic example which shows you how to solve a common problem:

```{r example}
library(telegram.bot.bt)

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


# License
Shield: [![CC BY-NC-SA 4.0][cc-by-nc-sa-shield]][cc-by-nc-sa]

This work is licensed under a
[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License][cc-by-nc-sa].

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg
