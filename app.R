#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rjson)
library(tidyverse)
library(rvest)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("NHL BettoR "),
    
    sidebarLayout(
        sidebarPanel(
            helpText(
                "Enter the current size of your bankroll, then click the Refresh button"
            ),
            numericInput(inputId = "portfolio",
                         label = "Bankroll size ($)",
                         value = 100,
                         min = 0),
            br(),
            actionButton(inputId = "update",
                         label = "Refresh odds"),
            br(),
            br(),
            helpText(
                "This tool compares live odds from Bovada",
                "with pre-game and in-game win probabilities",
                "from moneypuck.com.  It may take a few seconds",
                "to update after clicking the button."
            ),
            br(),
            helpText(
                "Click the Explanation tab to read more about these predictions and gambling strategy."
            ),
            br(),
            helpText(
                "Created by Trevor Greissinger using open-source packages in R.  
                I take no responsibility for any damages (financial,
                emotional, or otherwise) caused by use of this tool.  Please gamble responsibly."
            )
        ),
        
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel("Today's NHL odds", dataTableOutput("odds")),
                tabPanel("Explanation", withMathJax(includeMarkdown("writeup.Rmd")))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    timer <- reactiveVal(Sys.time())
    i_counter <- reactiveVal(0)
    
    observeEvent(input$update, {
        while (timer() > Sys.time()) {
            Sys.sleep(0.5)
        }
        timer(Sys.time() + 5)
        
        i_counter(i_counter() + 1)
    })
    
    curr.date <- Sys.Date()
    moneypuck.link <- paste0("http://moneypuck.com/moneypuck/dates/",
                             format(Sys.Date(), 
                                    format = "%Y%m%d"),
                             ".htm")
    
    bovada.live <- eventReactive(input$update, {
        fromJSON(file = "https://www.bovada.lv/services/sports/event/v2/events/A/description/hockey/nhl")
    })
    value <- tibble()
    bovada.probs <- eventReactive(input$update, {
        for (i in 1:length(bovada.live()[[1]][[2]])) {
            date <- bovada.live()[[1]][[2]][[i]]$link %>%
                str_sub(-12, -5) %>%
                as.Date(format = "%Y%m%d")
            if (date != as.Date(Sys.time() - 3600*5)) break
            
            away.name <- bovada.live()[[1]][[2]][[i]][[17]][[1]][[5]][[1]][[9]][[1]]$description
            home.name <- bovada.live()[[1]][[2]][[i]][[17]][[1]][[5]][[1]][[9]][[2]]$description
            away.prob <- bovada.live()[[1]][[2]][[i]][[17]][[1]][[5]][[1]][[9]][[1]]$price$decimal %>%
                as.numeric() %>%
                "^"(-1) %>%
                round(4)
            home.prob <- bovada.live()[[1]][[2]][[i]][[17]][[1]][[5]][[1]][[9]][[2]]$price$decimal %>%
                as.numeric() %>%
                "^"(-1) %>%
                round(4)
            away.american <- bovada.live()[[1]][[2]][[i]][[17]][[1]][[5]][[1]][[9]][[1]]$price$american
            home.american <- bovada.live()[[1]][[2]][[i]][[17]][[1]][[5]][[1]][[9]][[2]]$price$american
            teams <- c(away.name, home.name) %>%
                enframe()
            probs <- c(away.prob, home.prob) %>%
                enframe()
            american <- c(away.american, home.american) %>%
                enframe()
            game.probs <- left_join(teams, probs, by='name') %>%
                left_join(american, by = 'name')
            value <- bind_rows(value, game.probs)
        }
        value
    })
    bovada.clean <- eventReactive(input$update, {
        bovada.probs() %>%
            select(team = value.x,
                   bovada.prob = value.y,
                   odds = value) %>%
            mutate(team = toupper(team))
    })
    moneypuck.probs <- eventReactive(input$update, {
        read_html(moneypuck.link) %>% 
            html_nodes("h2") %>% 
            html_text() %>% 
            enframe() %>%
            transmute(moneypuck.prob = str_extract(string = value,
                                                   pattern = "[0-9.]+%") %>%
                          str_sub(end = -2) %>%
                          as.numeric() / 100) %>%
            drop_na()
    })
    moneypuck.names <- eventReactive(input$update, {
        read_html(moneypuck.link) %>% 
            html_nodes("img") %>% 
            as.character() %>% 
            str_sub(65, -3)
    })
    moneypuck.clean <- eventReactive(input$update, {
        moneypuck.probs() %>%
            add_column(team = moneypuck.names())
    })
    odds.table <- eventReactive(input$update, {
        left_join(bovada.clean(), moneypuck.clean()) %>%
            mutate(net.odds = 1 / bovada.prob - 1,
                   kelly = (net.odds * moneypuck.prob - (1 - moneypuck.prob)) / net.odds %>%
                       round(4),
                   units = if_else(kelly > 0, kelly * 100 / 4, 0),
                   bet.size = input$portfolio * units / 100) %>%
            select(Team = team,
                   `Bovada odds` = odds,
                   `Implied win probability` = bovada.prob,
                   `Expected win probability` = moneypuck.prob,
                   `Kelly criterion` = kelly,
                   `Units to wager` = units,
                   `Bet size ($)` = bet.size) %>%
            datatable() %>%
            formatRound(columns = c("Kelly criterion", "Units to wager"), 
                        digits = 3) %>%
            formatRound(columns = 7, 
                        digits = 2)
    })
    output$odds <- renderDataTable({
        odds.table()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
