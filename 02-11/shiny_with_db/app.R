library(shiny)
library(tidyverse)
library(lubridate)
library(DBI)

con <- dbConnect(
  RPostgreSQL::PostgreSQL(),
  dbname = "aegacorr",
  host = "otto.db.elephantsql.com",
  user = "aegacorr",
  password = Sys.getenv("ELEPHANTPWD"))

onStop(function() dbDisconnect(con))

if (!("table1" %in% dbListTables(con))) {
  # create table if it doesn't exist
  con %>% dbCreateTable(
    "table1",
    tibble(time = as_datetime(double(0)), action = integer(0))
  )
}


ui <- fluidPage(
  actionButton("plus", "+1"),
  actionButton("minus", "-1"),
  textOutput("count_message")
)


get_count <- function() {
  con %>% tbl("table1") %>% summarize(sum(action, na.rm = TRUE)) %>% pull()
}

server <- function(input, output) {
  count <- get_count()

  if (is.na(count)) {
    count <- 0
  }
  counter <- reactiveVal(count)

  observeEvent(input$plus, {
    sql <- con %>% sqlAppendTable(
      "table1", tibble(time = as.character(now()), action = 1), row.names = FALSE)
    con %>% dbExecute(sql)
    # query the database to check if there is any update from other sessions
    counter(get_count())
  })

  observeEvent(input$minus, {
    sql <- con %>% sqlAppendTable(
      "table1", tibble(time = as.character(now()), action = -1), row.names = FALSE)
    con %>% dbExecute(sql)
    # query the database to check if there is any update from other sessions
    counter(get_count())
  })

  output$count_message <- renderText({
    value <- counter()
    str_c("The counter is ", value)
  })
}

shinyApp(ui = ui, server = server)
