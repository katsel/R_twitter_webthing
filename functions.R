removeURLs <- function(x) {
  x <- gsub("https?:.+", "", x)
}

removeHTML <- function(x) {
  x <- gsub("&.+;", " ", x)
}

clearPunctuation <- function(x) {
  # Assume there are no ASCII 1, 2 or 3 characters.
  # preserve intra word dashes
  #  x <- gsub("(\\w)-(\\w)", "\\1\1\\2", x)
  # preserve twitter usernames (= entities starting with @)
  if (grepl("@(\\w)",x)) {
    x <- gsub("@(\\w)", "\2\\1", x)
    # preserve underscores within usernames
    x <- gsub("_","\4",x)
  }
  # preserve hashtags (= entities starting with #)
  x <- gsub("#(\\w)", " \3\\1", x)
  # replace remaining punctuation
  x <- gsub("[[:punct:]]+", "", x)
  # restore intra word dashes, usernames, hashtags and underscores
  #  x <- gsub("\1", "-", x, fixed = TRUE)
  x <- gsub("\2", "@", x, fixed = TRUE)
  x <- gsub("\3", "#", x, fixed = TRUE)
  x <- gsub("\4", "_", x, fixed = TRUE)
  return(x)
}

clearNumbers <- function(x) UseMethod("clearNumbers", x)
clearNumbers.PlainTextDocument <- function(x) {
  x[1] <- clearNumbers.character(x[1])
  return(x)
}
clearNumbers.character <- function (x) {
  x_vec <- unlist(strsplit(x," ",fixed=TRUE))
  x_vec[!grepl("[#@](\\w)",x_vec)] <- gsub("[[:digit:]]+", "",x_vec[!grepl("[#@](\\w)",x_vec)])
  x <- paste(x_vec,collapse=" ")
  return(x)
}

charcount <- function(x) {
  x <- iconv(x, to="utf-8", sub="") # unicode
  x <- gsub("&amp;","&",fixed=TRUE,x) # html entities
  x <- gsub("&gt;",">",fixed=TRUE,x)
  x <- gsub("&lt;","<",fixed=TRUE,x)
  str_length(x)
}

wordcount <- function(x) {
  x <- iconv(x, to="utf-8", sub="") # unicode
  x <- gsub("&amp;","",fixed=TRUE,x) # html entities
  x <- gsub("&gt;","",fixed=TRUE,x)
  x <- gsub("&lt;","",fixed=TRUE,x)
  x <- gsub("[[:punct:]]+", "", x) # punctuation
  x <- gsub("[[:digit:]]+", "", x) # digits
  vec <- sapply(
    strsplit(x, " ", fixed=TRUE), # split into chunks
    function(x) grepl("\\w",x)    # tell if word or not
  )
  length(vec[vec == TRUE])        # count only words
}

createCorpus <- function(x) {
  x <- iconv(x, to="utf-8", sub="")
  corpus <- Corpus(VectorSource(x,encoding="UTF-8"))
  # all lowercase
  corpus <- tm_map(corpus, tolower)
  # remove URLs
  corpus <- tm_map(corpus, removeURLs)
  # remove HTML entities
  corpus <- tm_map(corpus, removeHTML)
  # remove punctuation
  corpus <- tm_map(corpus, clearPunctuation)
  # remove numbers
  #corpus <- tm_map(corpus, clearNumbers)
  # remove stopwords
  corpus <- tm_map(corpus, removeWords, c(stopwords('german'),mystopwords))
}
