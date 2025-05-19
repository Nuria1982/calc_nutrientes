## ----setup, include=FALSE-----------------------------------------------------
library(shiny)
library(bslib)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----skinSelector-code, eval=TRUE, echo=FALSE---------------------------------
card(
  bs4Dash:::create_link_iframe(bs4Dash:::shinylive_links["inst/examples/vignettes-demos/skinSelector"]),
  full_screen = TRUE,
  style = "margin: 0 auto; float: none;"
)

## ----fresh-code, eval=TRUE, echo=FALSE----------------------------------------
card(
  bs4Dash:::create_link_iframe(bs4Dash:::shinylive_links["inst/examples/vignettes-demos/fresh-theming"]),
  full_screen = TRUE,
  style = "margin: 0 auto; float: none;"
)

