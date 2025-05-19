## ----setup, include=FALSE-----------------------------------------------------
library(bslib)

## ----boxAPI-code, eval=TRUE, echo=FALSE---------------------------------------
card(
  bs4Dash:::create_link_iframe(bs4Dash:::shinylive_links["inst/examples/vignettes-demos/box-api"]),
  full_screen = TRUE,
  style = "margin: 0 auto; float: none;"
)

## ----boxTools, echo=FALSE, fig.cap='Box Tools. From left to right: boxLabel, boxDropdown, collapsible and closable buttons, boxSidebar trigger.', fig.align = 'center', out.width='50%'----
knitr::include_graphics("figures/boxTools.png")

## ----boxSidebar-code, eval=TRUE, echo=FALSE-----------------------------------
card(
  bs4Dash:::create_link_iframe(bs4Dash:::shinylive_links["inst/examples/vignettes-demos/box-sidebar"]),
  full_screen = TRUE,
  style = "margin: 0 auto; float: none;"
)

## ----boxDropdown-code, eval=TRUE, echo=FALSE----------------------------------
card(
  bs4Dash:::create_link_iframe(bs4Dash:::shinylive_links["inst/examples/vignettes-demos/box-dropdown"]),
  full_screen = TRUE,
  style = "margin: 0 auto; float: none;"
)

## ----user-description, eval=FALSE---------------------------------------------
#  userDescription(
#    title = "Nadia Carmichael",
#    subtitle = "lead Developer",
#    type = 2,
#    image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
#  )

## ----userBox-code, eval=TRUE, echo=FALSE--------------------------------------
card(
  bs4Dash:::create_link_iframe(bs4Dash:::shinylive_links["inst/examples/vignettes-demos/userBox"]),
  full_screen = TRUE,
  style = "margin: 0 auto; float: none;"
)

## ----user-block, eval=FALSE---------------------------------------------------
#  userBlock(
#    image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
#    title = "Social Box",
#    subtitle = "example-01.05.2018"
#  )

## ----socialBox-code, eval=TRUE, echo=FALSE------------------------------------
card(
  bs4Dash:::create_link_iframe(bs4Dash:::shinylive_links["inst/examples/vignettes-demos/socialBox"]),
  full_screen = TRUE,
  style = "margin: 0 auto; float: none;"
)

