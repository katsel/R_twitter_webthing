library(data.table)
library(ggplot2)
library(rjson)
library(RMongo)
library(scales)
library(shiny)
library(slam)
#library(SnowballC)
library(stringr)
library(tau)
library(tm)

################################################################################
# include neccessary self-written functions and variables
################################################################################

source("functions.R")
source("variables.R")

################################################################################
# retrieve and preprocess data
################################################################################

mongo <- mongoDbConnect('twitter_test_db','127.0.0.1',27017)
# retrieve tweets from DB
output1 <- dbAggregate(mongo, "tatort", c(
  ' { "$match" : {retweeted_status: {$exists: false}, time: {$gte : 18, $lt: 23}, created_at : {$lte : { $date: "2014-03-01T00:00:00Z" }}} } ',
  ' { "$project" : { "username" : "$user.screen_name",created_at:"$created_at", text:"$text"} } '
))
output2 <- dbAggregate(mongo, "tatort", c(
  ' { "$match" : {retweeted_status: {$exists: false}, time: {$gte : 18, $lt: 23}, created_at : {$gt : { $date: "2014-03-01T00:00:00Z" }}} } ',
  ' { "$project" : { "username" : "$user.screen_name",created_at:"$created_at", text:"$text"} } '
))
output <- c(output1, output2) #merge 2 parts
rm(output1,output2)

# retrieve retweets from DB
# retweets
out_RT <- dbAggregate(mongo, "tatort", c(
  ' { "$match" : {retweeted_status: {$exists: true}, time: {$gte : 18, $lt : 23} } }',
  ' { "$project" : {created_at:"$created_at"} } '
))

#process output: JSON --> dataframe
#tweets
output <- as.list(output)
output <- lapply(output,fromJSON)
output <- lapply(output,unlist)
output <- do.call(rbind,output)
output <- as.data.frame(output,stringsAsFactors = FALSE)
names(output) <- c("X_id","text","created_at","username")
alltweets <- output
rm(output)
# retweets
out_RT <- as.list(out_RT)
out_RT <- lapply(out_RT,fromJSON)
out_RT <- lapply(out_RT,unlist)
out_RT <- do.call(rbind,out_RT)
out_RT <- as.data.frame(out_RT,stringsAsFactors = FALSE)
names(out_RT) <- c("X_id","created_at")

dbDisconnect(mongo)
rm(mongo)

# DateTime stuff
Sys.setlocale("LC_TIME", "en_GB.UTF-8")

# format time as time object that can be handled by R
alltweets$created_at <- sub("T"," ",alltweets$created_at)
alltweets$created_at <- sub("Z","",alltweets$created_at)
# change timezone GMT to CET
alltweets$created_at <- as.POSIXct(alltweets$created_at, format="%Y-%m-%d %X", tz="GMT")
alltweets$created_at <- format(alltweets$created_at, tz="CET")
alltweets$created_at <- as.POSIXct(alltweets$created_at, format="%Y-%m-%d %X", tz="CET")

# time + day component
alltweets$created_day <- format(alltweets$created_at, format = "%F")
alltweets$created_time <- format(alltweets$created_at, format = "%H:%M")
alltweets$created_time <- as.POSIXct(alltweets$created_time, format = "%H:%M")

# tatort-days only
alltweets <- alltweets[alltweets$created_day %in% tatort_days,]
alltweets$created_day <- as.factor(alltweets$created_day)

# calculate length
alltweets$length <- charcount(alltweets$text)
alltweets$n_words <- sapply(alltweets$text, wordcount)

# RETWEETS
out_RT$created_at <- sub("T"," ",out_RT$created_at)
out_RT$created_at <- sub("Z","",out_RT$created_at)
out_RT$created_at <- as.POSIXct(out_RT$created_at, format="%Y-%m-%d %X", tz="GMT")
out_RT$created_at <- format(out_RT$created_at, tz="CET")
out_RT$created_at <- as.POSIXct(out_RT$created_at, format="%Y-%m-%d %X", tz="CET")
# time + day component
out_RT$created_day <- format(out_RT$created_at, format = "%F")
out_RT$created_time <- format(out_RT$created_at, format = "%H:%M")
out_RT$created_time <- as.POSIXct(out_RT$created_time, format = "%H:%M")
# merge tweets and retweets into one dataframe
tmp1 <- data.frame(out_RT[2:4], RT=TRUE)
tmp2 <- data.frame(alltweets[c(3,5,6)], RT=FALSE)
rt_time <- rbind(tmp1,tmp2)
rm(tmp1,tmp2,out_RT) #delete the rest
rt_time <- rt_time[rt_time$created_day %in% tatort_days,]
rt_time$created_day <- as.factor(rt_time$created_day)
#done

################################################################################
# server logic
################################################################################
shinyServer(function(input,output) {

  # reactive function returning the tweets currently selected
  tweetsSelected <- reactive({
    input$goButton
    # select by days
    seldays <- isolate(input$selected_days)
    selected <- alltweets[alltweets$created_day %in% seldays,]
    # ugly but it works: convert "decimal" time to time
    toTime <- function(x) {
      hour <- floor (x / 100)
      min <- ((x - 100*hour) / 100) * 60
      time <- paste0(hour,":",min)
      return (as.POSIXct(time,format="%H:%M"))
    }
    seltime <- isolate(input$selected_time)
    # select by time
    selected <- selected[selected$created_time > toTime(seltime[1]),]
    selected <- selected[selected$created_time <= toTime(seltime[2]),]

    # handle empty dataset
    if (dim(selected)[1] == 0) {
      selected <- 0
    }
    else
        selected
  })

  # reactive function returning a text corpus made of currently selected tweets
  corpusSelected <- reactive({
    selected <- tweetsSelected()
    corpus <- createCorpus(selected$text)
  })

  # shows number of tweets
  output$dimension = renderText({
    selected <- tweetsSelected()
    dim(selected)[1]
  })

  # tab: table displaying tweets
  output$mytable = renderDataTable({
    selected <- tweetsSelected()
    selected[c("created_at","username","text")]
  })

  #tab: overview -- tweet count by day and time
  output$tweetsByDay = renderPlot({
    q <- qplot(created_time, data=alltweets, stat="bin", binwidth=300) # 5min
    q <- q + facet_wrap(~ created_day) + scale_x_datetime(minor_breaks=date_breaks("15 min"), 
                                                          labels = date_format("%H:%M"), 
                                                          breaks=date_breaks("1 hour"))
    print(q)
  })
  #tab: overview -- retweets
  output$reTweets = renderPlot({
    q <- qplot(created_time, data=rt_time, geom="bar", stat="bin", binwidth=3*300, #15min 
               color=RT, fill=RT, position=position_dodge())
    q <- q + facet_wrap(~ created_day) + scale_y_continuous(minor_breaks=seq(0,3000,250)) + 
      scale_x_datetime(minor_breaks=date_breaks("15 min"), labels = date_format("%H:%M"), 
                       breaks=date_breaks("1 hour"))
    print(q)
  })

  #tab: overview -- number of chars
  output$nCharPlot = renderPlot({
    print(qplot(alltweets$length, binwidth=2, xlab="number of chars"))
  })
  output$nCharSummary = renderPrint({
    summary(alltweets$length)
  })
  
  #tab: overview -- number of words
  output$nWordPlot = renderPlot({
    print(qplot(alltweets$n_words,binwidth=1, xlab="number of words", ))
  })
  output$nWordSummary = renderPrint({
    summary(alltweets$n_words)
  })
  
  # tab: word frequency table
  output$freqterms = renderDataTable({
    # NLP stuff
    corpus <- corpusSelected()
    # document-term-matrix
    dtm <- DocumentTermMatrix(corpus)
    # frequency list
    cs <- col_sums(dtm)
    freqlist <- sort(cs, decr=TRUE)
    rm(cs)
    # rule out common terms
    freqlist <- freqlist[-which(names(freqlist) %in% frequentwords)]
    # to dataframe
    freqlist <- data.frame(word=names(freqlist),freq=freqlist)
  })

  # tab: ngrams
  output$bigrams = renderTable({
    corpus <- corpusSelected()
    tw2 <- unlist(corpus[1:length(corpus)])
    tw2 <- removeWords(tw2,c("tatort","polizeiruf","rt"))
    bigrams = textcnt(tw2, n=2, method="string", lower=10L)
    bigrams = bigrams[order(bigrams, decreasing=TRUE)]
    bigrams <- data.frame(bigrams)
  })
  output$trigrams = renderTable({
    corpus <- corpusSelected()
    tw2 <- unlist(corpus[1:length(corpus)])
    tw2 <- removeWords(tw2,c("tatort","polizeiruf","rt"))
    trigrams = textcnt(tw2, n=3, method="string", lower=5L)
    trigrams = trigrams[order(trigrams, decreasing=TRUE)]
    trigrams <- data.frame(trigrams)
  })
  output$fourgrams = renderTable({
    corpus <- corpusSelected()
    tw2 <- unlist(corpus[1:length(corpus)])
    tw2 <- removeWords(tw2,c("tatort","polizeiruf","rt"))
    fourgrams = textcnt(tw2, n=4, method="string", lower=3L)
    fourgrams = fourgrams[order(fourgrams, decreasing=TRUE)]
    fourgrams <- data.frame(fourgrams)
  })
  output$fivegrams = renderTable({
    corpus <- corpusSelected()
    tw2 <- unlist(corpus[1:length(corpus)])
    tw2 <- removeWords(tw2,c("tatort","polizeiruf","rt"))
    fivegrams = textcnt(tw2, n=5, method="string", lower=2L)
    fivegrams = fivegrams[order(fivegrams, decreasing=TRUE)]
    fivegrams <- data.frame(fivegrams)
  })
  # tab: tweet stats -- number of chars
  output$nCharPlotS = renderPlot({
    selected <- tweetsSelected()
    print(qplot(selected$length, binwidth=2, xlab="number of chars"))
  })
  output$nCharSummaryS = renderPrint({
    selected <- tweetsSelected()
    summary(selected$length)
  })
  # tab: tweet stats -- number of words
  output$nWordPlotS = renderPlot({
    selected <- tweetsSelected()
    print(qplot(selected$n_words,binwidth=1, xlab="number of words", ))
  })
  output$nWordSummaryS = renderPrint({
    selected <- tweetsSelected()
    summary(selected$n_words)
  })
  #tab: user stats
  output$userStats = renderTable({
    selected <- tweetsSelected()
    n_tweets <- dim(selected)[1]
    selected <- data.table(selected, key="username")
    selected <- selected[,list(tweets=.N), by="username"]
    n_users <- dim(selected)[1]
    tpu <- n_tweets / n_users
    table <- data.frame(n_tweets, n_users, tpu)
    names(table) <- c("tweets", "active users", "tweets per user")
    table
  })
  output$freqUsers = renderTable({
    selected <- tweetsSelected()
    selected <- data.table(selected, key="username")
    selected <- selected[,list(tweets=.N), by="username"]
    selected <- selected[selected$tweets > 1,]
    selected <- selected[order(selected$tweets,decreasing=TRUE),]
    head(selected, 30)
  })
  output$whodunit = renderTable({
    corpus <- corpusSelected()
    tw2 <- unlist(corpus[1:length(corpus)])
    tw2 <- removeWords(tw2,c("tatort","polizeiruf","rt"))
    bigrams <- textcnt(tw2, n=2, method="string", lower=1L)
    bigrams <- bigrams[grepl("wars$",names(bigrams))]
    bigrams <- bigrams[order(bigrams, decreasing=TRUE)]
    bigrams <- data.frame(bigrams)
  })
})