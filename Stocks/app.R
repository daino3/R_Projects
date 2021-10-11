#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

install.packages("devtools") # if not installed
install.packages("FinancialInstrument") #if not installed
install.packages("PerformanceAnalytics") #if not installed

# next install blotter from GitHub
devtools::install_github("braverock/blotter")
# next install quantstrat from GitHub
devtools::install_github("braverock/quantstrat")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# https://github.com/braverock/quantstrat
# https://www.r-bloggers.com/2017/05/the-end-of-the-honeymoon-falling-out-of-love-with-quantstrat/
# https://github.com/mementum/backtrader
# https://github.com/jasonyip184/StockSentimentTrading
# https://towardsdatascience.com/https-towardsdatascience-com-algorithmic-trading-using-sentiment-analysis-on-news-articles-83db77966704
# Data: https://www.kaggle.com/borismarjanovic/price-volume-data-for-all-us-stocks-etfs
# ML & News: https://towardsdatascience.com/making-a-continual-ml-pipeline-to-predict-apple-stock-with-global-news-python-90e5d6610b21

## TODO:
# Rank stocks based on:
# 1. "hype": sentiment analysis: bloomberg, wsj, etc. 
# 2. "momentum": recent trading (last 3-5 days) vs. S&P/benchmarks
# 3. "financials": solid historical and projected earnings
# 4. "industry": tech v materials v services
# 5. "technicals": pure stats-based/technical trading indicators (will need advice)
# 6. "events": earnings, etc.
# 7. "market": what's the overall market doing?
# 8. "stock attributes": liquidity, exchange, market cap
# multiply "rank attributes" x "weight" to get letter grade
# e.g. 
# hype x hypeWeight + momentum x momWeight + financials x finWeight + industry x indWeight = TradeRating
#   90 x 20%        +       75 x 20%       +        95  x 20%       +       85 x 20%       = 86.5 % -> "B"
# derivative or big (+/-) change in one ranking attribute will flag the stock
# use machine learning / regression to find weights based on back testing


stock.str='AAPL' # what are we trying it on
currency('USD')
stock(stock.str,currency='USD',multiplier=1)
startDate="1999-12-31"
initEq=1000000
portfolio.st='macross'
account.st='macross'
initPortf(portfolio.st,symbols=stock.str)
#> [1] "macross"
initAcct(account.st,portfolios=portfolio.st, initEq=initEq)
#> [1] "macross"
initOrders(portfolio=portfolio.st)
stratMACROSS<- strategy(portfolio.st)
stratMACROSS

    