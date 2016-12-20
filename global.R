# title: "global.R"
# author: "Jason Taylor"

# todos:
# - Should axis variable be in global?
# - Change symbol.list to read from file list in data folder

# Global setup
library(shiny) # main library to load shiny application
library(shinythemes) # themes for shiny
library(ggvis) # visualization used in shiny app
library(options.data) # custom local package with options data
library(options.studies) # custom local package with options strategy functions
library(audio) # used by beepr package
library(beepr) # sends alert to speaker when script is complete
library(dtplyr) # update for data.table

# Variables that can be put on the axis
axis_vars <- c(
  "Days Held" = "days.held",
  "IV Rank" = "open.ivrank",
  "Open ROC" = "open.roc",
  "Profit" = "profit",
  "RSI" = "open.rsi",
  "Year" = "year"
  )

symbol.list <- c("AMZN", "EEM", "EWZ", "FXI", "GDX", "GS", "IBM", "SLV", "SPY", "XLE")
