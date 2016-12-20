# title: "Shiny UI"
# author: "Jason Taylor"
# Version: 1.11.16

library(shiny)
library(shinydashboard)
library(shinyjs)
library(rbokeh)

# Todos ----
# Add menu items to show why trades were not opened
# Add buy and hold to the profit plot (Short Put, Strangle complete)
# Change buy and hold plot to return on margin instead of profit
# Test all stocks, studies, and dates
# Move to root folder
# If expiration is reached, make full profit or specific loss instead of just end of day price
# Add confidence intervals to the plots
# Change all long variable names to underscores
# Function names to verbs
# Argument names in functions (df, x, y, z, p, n)
# detail arguments should always be given a default value

# Need to test ----

# JavaScript ----
actionLink <- function(inputId, ...) {
  tags$a(href = 'javascript:void',
         id = inputId,
         class = 'action-button',
         ...)
}

warnings.table <- data.frame()

# Dashboard ----
dashboardPage(
  skin = "green",
  dashboardHeader(
    dropdownMenuOutput("messageMenu"),
    dropdownMenuOutput("notificationMenu"),
    dropdownMenuOutput("taskMenu")
    ),
# Sidebar Section ----
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Study", tabName = "Study", icon = icon("cogs"),
               selectInput("stock", "Stock", symbol.list),
               selectInput("study", "Study", c("Short Put", "Strangle")),
               selectInput("openOption", "Open on", c("First of Month", "First of Week", "Daily"))),
      menuItem("Entry Criteria", tabName = "Entry Criteria", icon = icon("cogs"),
               sliderInput("open.dte", "DTE", 0, 90, 45, step = 5),
               conditionalPanel(
                 condition = ("input.study == 'Strangle'"),
                 sliderInput("call.delta", "Call delta", 0, 1, .16, step = .01)),
               conditionalPanel(
                 condition = ("input.study == 'Short Put' || input.study == 'Strangle'"),
                 sliderInput("put.delta", "Put delta", -1, 0, -.16, step = .01)),
               # conditionalPanel(
               #   condition = ("input.study == 'Call Calendar' || input.study == 'Poor Mans Cov Call'"),
               #   sliderInput("second.dte", "Min short DTE", 0, 90, 30, step = 5)),
               sliderInput("open.ivrank", "IV Rank", 0, 100, c(0, 100), step = 1)),
               # sliderInput("min.roc", "Min ROC", 0, 50, 0, step = 1)),
      menuItem("Exit Criteria", tabName = "Exit Criteria", icon = icon("cogs"),
               sliderInput("proftarg", "Profit target %", 0, 100, 50, step = 5),
               sliderInput("loss.lim", "Max loss x times credit received", 0, 10, 2, step = .25),
               # conditionalPanel(
               #   condition = ("input.study == 'Poor Mans Cov Call'"),
               #   sliderInput("l.loss.lim", "Long max loss % debit paid", 10, 100, 50, step = 5)),
               sliderInput("gamma.days", "Days prior to expiration", 0, 45, 0, step = 1)),
               # conditionalPanel(
               #   condition = ("(input.study == 'Short Put' || input.study == 'Strangle' || input.study == 'Straddle') &&
               #                  (input.stock == 'AMZN' || input.stock == 'GS' || input.stock == 'IBM')  &&
               #                  (input.openOption == 'First of Week' || input.openOption == 'First of Month' ||
               #                  input.openOption == 'Previous Close' || input.openOption == 'Daily')"),
               #   selectInput("earn.close", "Close day prior to earnings?", c("No", "Yes")))),
      actionButton('goPlot', 'Run Study', icon = icon("play-circle")))
  ),
# Body Section ----  
  body <- dashboardBody(
    fluidRow(
      column(width = 4,
             tags$head(HTML("<script type='text/javascript' src='google-analytics.js'></script>")),
             useShinyjs(),
             htmlOutput("total_profit"),
             htmlOutput("avg_prof_trade"),
             htmlOutput("avg_prof_day"),
             htmlOutput("avg_days"),
             h4("")),
      column(width = 4,
             htmlOutput("n_trades"),
             htmlOutput("percent_winners"),
             htmlOutput("max_loss"),
             htmlOutput("max_win")),
      column(width = 4,
             htmlOutput("exit.profit.target"),
             htmlOutput("exit.loss.limit"),
             htmlOutput("exit.expiration"),
             htmlOutput("exit.gamma.risk"))),
    fluidRow(
      tabBox(
        id = "tabset1", height = "800px", width = "1000px",
        tabPanel("Trade Results",
                 #imageOutput("loading.image"),
                 htmlOutput("welcome.message"),
                 rbokehOutput("rbokeh.trades"),
                 fluidRow(
                   column(width = 1,
                          ""),
                   column(width = 5,
                          selectInput("xvar", "X-axis variable", axis_vars, selected = "open.ivrank")),
                   column(width = 6,
                          selectInput("yvar", "Y-axis variable", axis_vars, selected = "profit"))
                 )),
        tabPanel("Portfolio",
                 rbokehOutput("rbokeh.profits")),
        tabPanel("Table",
                 downloadButton('downloadData', 'Download'),
                 h4(" "),
                 dataTableOutput('table'))
        # tabPanel("Warnings",
        #          dataTableOutput("table2"))
      )
    )
  )
)
