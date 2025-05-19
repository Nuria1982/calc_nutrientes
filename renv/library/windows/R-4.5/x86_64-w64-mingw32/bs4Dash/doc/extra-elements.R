## ----setup, include=FALSE-----------------------------------------------------
library(bslib)
knitr::opts_chunk$set(echo = TRUE)

## ----accordions-code, eval=TRUE, echo=FALSE-----------------------------------
card(
  bs4Dash:::create_link_iframe(bs4Dash:::shinylive_links["inst/examples/vignettes-demos/accordions"]),
  full_screen = TRUE,
  style = "margin: 0 auto; float: none;"
)

## ----message-code, eval=FALSE-------------------------------------------------
#  list(
#    author = "David",
#    date = "Now",
#    image = "https://i.pinimg.com/originals/f1/15/df/f115dfc9cab063597b1221d015996b39.jpg",
#    type = "received",
#    text = tagList(
#      sliderInput(
#        "obs",
#        "Number of observations:",
#        min = 0,
#        max = 1000,
#        value = 500
#      ),
#      plotOutput("distPlot")
#    )

## ----chat-code, eval=TRUE, echo=FALSE-----------------------------------------
card(
  bs4Dash:::create_link_iframe(bs4Dash:::shinylive_links["inst/examples/vignettes-demos/userMessages"]),
  full_screen = TRUE,
  style = "margin: 0 auto; float: none;"
)

