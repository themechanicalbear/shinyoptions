# title: "Shiny server"
# author: "Jason Taylor"
# Date: "Nov-16-2016"
# Version: 1.11.16

# todos:
# - change envir .Global to current shiny envir
# - ensure earnings date is correct due to AM or PM reporting

shinyServer(function(input, output, session) {
  shinyOptions(progress.style = "old")
  main.color <- "#00a65a" # Green
  
  # Uncomment next line for debugging to console
  # options(shiny.trace = TRUE)
  # The following two lines can be inserted to box in code section for profiling
  # Rprof("boot.out")
  # Rprof(NULL)
  # options(shiny.error = recover)
  
  output$notificationMenu <- renderMenu({
      notificationItem(text = "Need help?", icon = shiny::icon("user"), status = "info",
                       href = "mailto:taylorizing@gmail.com")
    })

  # Hide/UnHide Axis variable inputs
  observe({
    toggle(id = "xvar", condition = input$goPlot)
    toggle(id = "yvar", condition = input$goPlot)
  })
  
  output$welcome.message <- renderUI({
    HTML("<p style=\"text-align: center; font-size: 60px; color: #FFFFFF;\">.</p> 
         <p style=\"text-align: center; font-size: 20px;\"><strong>Welcome:</strong> Please fill out the study inputs in the left sidebar</p>
         <p style=\"text-align: center; font-size: 20px;\">When ready click&nbsp;<strong>Run Study</strong>&nbsp;to see the results</p>")
  })
  
  # Insert loading image in the message output
  observeEvent(input$goPlot, {
    output$loading.image <- renderImage({
      list(
        src = "www/images/ajax-loader.gif",
        filetype = "image/gif",
        alt = "Loading"
      )
    }, deleteFile = FALSE)
  })
  
  # Reactive section for building executed trade list
  observeEvent(input$goPlot, {
    # We reset the results data.frame when inputs are changed
    if (exists("results", envir = .GlobalEnv) && is.data.frame(get("results"))) {
      rm(results, envir = .GlobalEnv)
    }
    assign("openOption", input$openOption, envir = .GlobalEnv)
    if (openOption == "First of Month") {
      data(list = "open.first.day.month")
      assign("first.day", open.first.day.month, envir = .GlobalEnv)
      assign("inc.amount", .004)}
    else if (openOption == "First of Week") {
      data(list = "open.first.day.week")
      assign("first.day", open.first.day.week, envir = .GlobalEnv)
      assign("inc.amount", .001)}
    else if (openOption == "Daily") {
      data(list = "open.daily")
      assign("first.day", open.daily, envir = .GlobalEnv)
      assign("inc.amount", .0002)}
    
    withProgress(message = "Progress", detail = "Setting up study", value = .05, {
      t <- 0 # Set inital trade number to zero
      progress.int <- inc.amount # Set progress bar increment amount
      
      # Values defined by the customer in shiny ui
      assign("study", input$study, envir = .GlobalEnv)
      assign("stock", input$stock, envir = .GlobalEnv)
      assign("low.iv", input$open.ivrank[1], envir = .GlobalEnv)
      assign("high.iv", input$open.ivrank[2], envir = .GlobalEnv)
      assign("o.dte", input$open.dte, envir = .GlobalEnv)
      assign("s.dte", input$second.dte, envir = .GlobalEnv)
      assign("c.delta", input$call.delta, envir = .GlobalEnv)
      assign("p.delta", input$put.delta, envir = .GlobalEnv)
      assign("prof.targ", input$proftarg / 100, envir = .GlobalEnv)
      assign("loss.lim", input$loss.lim + 1, envir = .GlobalEnv)
      assign("l.loss.lim", input$l.loss.lim / 100, envir = .GlobalEnv)
      assign("g", input$gamma.days, envir = .GlobalEnv)
      assign("earn.close", input$earn.close, envir = .GlobalEnv)
      assign("min.roc", input$min.roc, envir = .GlobalEnv)
      assign("p.delta.lim", p.delta + .1, envir = .GlobalEnv)
      assign("c.delta.lim", c.delta - .1, envir = .GlobalEnv)
      assign("stock.list", as.data.frame(symbol.list[-length(symbol.list)],
                                         stringsAsFactors = FALSE), envir = .GlobalEnv)
      
      # Load option chain data for stock chosen by customer
      if (!stock == "ALL") {
        data(list = paste0(stock, ".options"))
      }
      
      # # Opening frequency
      # if (openOption == "First of Month") {
      #   data(list = "open.first.day.month")
      #   assign("first.day", open.first.day.month, envir = .GlobalEnv)}
      # else if (openOption == "First of Week") {
      #   data(list = "open.first.day.week")
      #   assign("first.day", open.first.day.week, envir = .GlobalEnv)}
      # else if (openOption == "Daily") {
      #   data(list = "open.daily")
      #   assign("first.day", open.daily, envir = .GlobalEnv)}
      # else if (openOption == "Earnings") {
      #   data(list = paste0("earnings.dates.", stock))
      #   assign("first.day", earnings.dates, envir = .GlobalEnv)}
      # else if (openOption == "Previous Close") {
      # #Fill in the custom dates .csv for this
      # source("Shared/customopen.R")}
      # 
      # # Close prior to earnings?
      # if (earn.close == "Yes")  {
      #   data(list = paste0("earnings.dates.", stock))
      #   assign("earnings.close", earnings.dates, envir = .GlobalEnv)}
      
    })
    
# Run function for study selected
    # if (study == "Call Calendar") {
    #   call.calendar(progress.int, t)}
    # if (study == "Poor Mans Cov Call") {
    #   pmcc(progress.int, t)}
    if (study == "Short Put") {
      short.put(progress.int, t)}
    # if (study == "Long Stock") {
    #   LongStock(progress.int, t)}
    if (study == "Strangle") {
      if (stock == "ALL") {
        for (i in 1:nrow(stock.list)) {
          data(list = paste0(stock.list[i, ], ".options"))
          strangle(progress.int, t)}}
      else {
        strangle(progress.int, t)}}
    if (study == "Straddle") {
      straddle(progress.int, t)}
    # if (study == "Strangle Daily Close") {
    #   strangle.daily.close(progress.int, t)}
    results
  })
  
  # Function for generating tooltip (hover over) text
  trade_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$trade.num)) return(NULL)
    
    # Pick out the trade with this trade.num
    trades <- isolate(trades())
    trade <- trades[trades$trade.num == x$trade.num, ]
    
    paste0("Open: ", trade$open.date, "<br>",
           "Close: ", trade$close.date, "<br>",
           "Call strike: ", trade$call.strike, "<br>",
           "Put strike: ", trade$put.strike, "<br>",
           "DTE: ", trade$dte, "<br>",
           "IVRank: ", round(trade$open.ivrank, digits = 0), "<br>",
           "rsi: ", round(trade$open.rsi, digits = 0), "<br>",
           "Exit: ", trade$exit.reason, "<br>",
           "Profit: $", format(trade$profit, big.mark = ",", scientific = FALSE)
    )
  }
  
  # HTML output
  observeEvent(input$goPlot, {
    output$welcome.message <- renderUI({
      HTML("")
    })
    output$n_trades <- renderUI({
      str.num.trades <- paste0("Number of trades: ", nrow(results))
      HTML(str.num.trades)
    })
    environment(outputHTML) <- environment()
    outputHTML()
  })
  
  # Render datatable
  observeEvent(input$goPlot, {
    # Output of a table to show the trade details
    output$table <- renderDataTable({results.table},options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 15),
      scrollX = TRUE
    ))
    # Download table
    output$downloadData <- downloadHandler(
      filename = function() { paste(stock, 'pdelta', as.character(p.delta),
                                    'cdelta', as.character(c.delta), 'results.csv') },
      content = function(file) {
        write.csv(results.table, file)
      }
    )
  })
  
  # rbokeh trades plot
  observeEvent(input$goPlot,{
    output$rbokeh.trades <- renderRbokeh({
      # Lables for axes
      xvar_name <- names(axis_vars)[axis_vars == input$xvar]
      yvar_name <- names(axis_vars)[axis_vars == input$yvar]
      
      plot_data <- results
      h <- figure(xlab = xvar_name, ylab = yvar_name, title = paste0(openOption, " ", stock, " ", study),
                  width = 1200, legend_location = "top_left", padding_factor = .2) %>%
        ly_points(input$xvar, input$yvar, hover = list(open.date, close.date, call.strike, put.strike, dte,
                                                       open.ivrank, open.rsi, exit.reason, profit),
                  data = plot_data, alpha = 0.5, color = Profitable, size = 5)
      return(h)
    })
  })
  
  # rbokeh profits plot
  observeEvent(input$goPlot,{
    output$rbokeh.profits <- renderRbokeh({
      plot_data <- results
      plot_data <- mutate(plot_data, cum.sum = cumsum(profit), stock.sum = cumsum(hold.profit))
      g <- figure(xlab = "Open Date", ylab = "Cumulative Profit", width = 1200,
                  title = paste0(openOption, " ", stock, " ", study, " (allows for overlapping positions)"), legend_location = "top_left") %>%
        #ly_points(open.date, cum.sum, hover = list(open.date, close.date, call.strike, put.strike, dte,
        #                                           open.ivrank, open.rsi, exit.reason, profit, cum.sum), data = plot_data,
        #          alpha = 0.5, color = main.color, size = 5) %>%
        ly_lines(open.date, cum.sum, data = plot_data, color = main.color, alpha = 0.3, legend = "Study return", width = 4) %>%
        #ly_lines(open.date, hold.profit, data = plot_data, legend = "Buy & Hold", type = 2, width = 4) %>%
        ly_lines(open.date, stock.sum, data = plot_data, legend = "Stock hold return", type = 2, width = 2) %>%
        #ly_text(open.date, hold.profit, text = hold.profit, data = tail(plot_data, n = 1),
        #        font_style = "normal", font_size = "8pt",
        #        align = "left", baseline = "top") %>%
        ly_text(open.date, cum.sum, text = cum.sum, data = tail(plot_data, n = 1),
                font_style = "normal", font_size = "8pt", color = main.color,
                align = "left", baseline = "top") %>%
        ly_text(open.date, stock.sum, text = stock.sum, data = tail(plot_data, n = 1),
                font_style = "normal", font_size = "8pt", color = "black",
                align = "left", baseline = "top")
      return(g)
    })
  })
  
  # Render second datatable
  # observeEvent(input$goPlot,{
  #   # Output of a table to show the trade details
  #   output$table2 <- renderDataTable({warnings.table}, options = list(pageLength = 15, lengthMenu = c(5, 15, 30)
  #   ))
  #   # Download table
  #   output$downloadData2 <- downloadHandler(
  #     filename = function() { paste('st.results.table.csv') },
  #     content = function(file) {
  #       write.csv(st.results.table, file)
  #     }
  #   )
  # })
  
  # Reset default values when inputs change
  observe({
    if (input$stock == "EEM" || input$stock == "EWZ" || input$stock == "FXI" ||
        input$stock == "GDX" || input$stock == "SLV" || input$stock == "SPY" ||
        input$stock == "XLE")  {
      updateSelectInput(session, "earn.close", selected = "No")
    }
  })
})  # End shiny server content
