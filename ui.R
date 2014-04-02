library(shiny)

# read some variables from file
source("variables.R")
names(tatort_days) <- tatort_labels

################################################################################
# user interface
################################################################################
shinyUI(pageWithSidebar(
  headerPanel("Interactive #tatort and #polizeiruf tweet statistics"),
  
  # Sidebar controls for filtering what tweets to display
  sidebarPanel(
    conditionalPanel(
      condition="input.TabPanel != 'overview'",
      # days
      checkboxGroupInput("selected_days", "Days:",
                         tatort_days, selected=tatort_days[1:6]),
      # time
      sliderInput("selected_time", "Time:", 
                  min=1900, max=2400, value=c(1900,2400), step=500/60, format="##,#0"),

      actionButton(inputId="goButton", "apply filter"),

      p("Tweets selected: "),
      textOutput('dimension')
    ),
   conditionalPanel(
     condition="input.TabPanel == 'overview'",
     p("All tweets selected."),
     p("Please use 'tweet stats' to view selected tweets.")
   )
  ),
  
  mainPanel(
    tabsetPanel(
      id = "TabPanel",
      tabPanel("tweets",dataTableOutput('mytable')),
      tabPanel("overview",
               h3("tweets per day"),
               plotOutput('tweetsByDay'),
               h3("tweets and retweets"),
               plotOutput('reTweets'),
               h3("number of characters per tweet"),
               plotOutput('nCharPlot'),
               verbatimTextOutput('nCharSummary'),
               h3("number of words per tweet"),
               plotOutput('nWordPlot'),
               verbatimTextOutput('nWordSummary')),
      tabPanel("word freq",dataTableOutput('freqterms')),
      tabPanel("n-grams",
               h3("bigrams"),
               tableOutput('bigrams'),
               h3("trigrams"),
               tableOutput('trigrams'),
               h3("fourgrams"),
               tableOutput('fourgrams'),
               h3("fivegrams"),
               tableOutput('fivegrams')
               ),
      tabPanel("tweet stats",
               h3("number of characters per tweet"),
               plotOutput('nCharPlotS'),
               verbatimTextOutput('nCharSummaryS'),
               h3("number of words per tweet"),
               plotOutput('nWordPlotS'),
               verbatimTextOutput('nWordSummaryS')
               ),
      tabPanel("user stats",
               h3("user statistics"),
               tableOutput('userStats'),
               h3("30 most active users"),
               tableOutput('freqUsers'),
               ),
      tabPanel("whodunit?",
               h3("wer war's?"),
               tableOutput('whodunit'))
    )
  )
))