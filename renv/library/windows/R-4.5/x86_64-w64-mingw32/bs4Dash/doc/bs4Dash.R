## ----include = FALSE----------------------------------------------------------
library(bslib)
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----basic-demo-code, eval=TRUE, echo=FALSE-----------------------------------
card(
  bs4Dash:::create_link_iframe(bs4Dash:::shinylive_links["inst/examples/vignettes-demos/scrollToTop"]),
  full_screen = TRUE,
  style = "margin: 0 auto; float: none;"
)

