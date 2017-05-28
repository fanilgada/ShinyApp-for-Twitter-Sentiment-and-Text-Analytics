## Allocate memory
options(java.parameters = "-Xmx10g")

## clear console
cat("\014")

## clear global variables
rm(list=ls())

## list of packages required
list.of.packages = c("git2r","digest","devtools",
                     "RCurl","RJSONIO","stringr","syuzhet","httr",
                     "rjson","tm","NLP","RCurl","wordcloud","wordcloud2",
                     "tidytext","dplyr","zipcode","bit", "shiny", "shinythemes")


## for devtools
library(git2r);library(digest)
#require(devtools)
#install_github("hadley/devtools")
library(devtools)

## data manipultion
library(dplyr);library(stringr)

# loading the libraries
## Linked to importing tweets
library(rjson);library(httr);library(twitteR);library(zipcode)

## Linked to generating a wordcloud
library(tm);library(NLP);library(RCurl);library(RJSONIO)
library(stringr);library(wordcloud);library(wordcloud2); 

#To create Shiny environment
library(shinythemes)
library(shiny)

## Linked to sentiment analysis
library(syuzhet)

# Twitter authentication key
oauth = setup_twitter_oauth(consumer_key = "3oIox1ayCJn6V4QaK1pBvpwwN",
                            consumer_secret = "JxSI5XMAdLyZYjDRudjDU9dg1clJIRk1JoPpL5oPj89GF3ZvNN",
                            access_token = "2892962053-Ssj5QqnjQkioN1BcsL02hsdhRJ9WwhgCP9EJKFq",
                            access_secret = "2xSQuPOoNIwui2oL69BooGcp2Yfo8UB59HEvJtBQ6lCku")

cat("\014")

cleanTweets = function(object.with.tweets){
  # list to dataframe
  df.tweets <- twListToDF(object.with.tweets)
  
  # Removes RT
  df.tweets$text_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df.tweets$text)
  
  #Remove non-ASCII characters
  Encoding(df.tweets$text_clean) = "latin1"
  iconv(df.tweets$text_clean, "latin1", "ASCII", sub = "")
  
  # Removes @<twitter handle>
  df.tweets$text_clean = gsub("@\\w+", "", df.tweets$text_clean)
  # Removes punctuations
  df.tweets$text_clean = gsub("[[:punct:]]", "", df.tweets$text_clean)
  # Removes numbers
  df.tweets$text_clean = gsub("[[:digit:]]", "", df.tweets$text_clean)
  # Removes html links
  df.tweets$text_clean = gsub("http\\w+", "", df.tweets$text_clean)
  # Removes unnecessary spaces
  df.tweets$text_clean = gsub("[ \t]{2,}", "", df.tweets$text_clean)
  df.tweets$text_clean = gsub("^\\s+|\\s+$", "", df.tweets$text_clean)
  # Fix for error related to formatting 'utf8towcs'"
  df.tweets$text_clean <- str_replace_all(df.tweets$text_clean,"[^[:graph:]]", " ")
  return(df.tweets)
}

searchThis = function(search_string,geocode_string = "42.375,-71.1061111,1000mi",number.of.tweets = 100)
{
  searchTwitter(search_string, geocode=geocode_string,n = number.of.tweets, lang = "en")
}

userTL = function(user.name,number.of.tweets = 100)
{
  userTimeline(user.name,n = number.of.tweets)
}

# Generate Term Document Matrix using stopword list from tm pacakge
tdm.tmStopWord = function(clean.tweets.dataframe){
  # Creates a text corpus from the plain text document for every tweet
  text_corpus = Corpus(VectorSource(clean.tweets.dataframe$text_clean))
  # Text_corpus is a collection of tweets where every tweet is a document
  
  # creating a Term Document Matrix 
  tdm = TermDocumentMatrix(
    # the text corpus created from the text_clean object
    text_corpus,
    # defining the stopwords to be removed before creating a term document matrix
    control = list(
      removePunctuation = TRUE,
      stopwords("en"),
      removeNumbers = TRUE,
      tolower = TRUE)
  )
  
  return(tdm)
}

# Generate Term Document Matrix using TF-IDF
tdm.TFIDF = function(clean.tweets.dataframe){
  
  text_corpus = Corpus(VectorSource(clean.tweets.dataframe$text_clean))
  
  # Text_corpus is a collection of tweets where every tweet is a document
  tdm <- DocumentTermMatrix(text_corpus, control = list(weighting = weightTfIdf))
  
  return(tdm)
}

# Generate Term Document Matrix without removing stopwords
tdm.tm = function(clean.tweets.dataframe){
  
  text_corpus = Corpus(VectorSource(clean.tweets.dataframe$text_clean))
  tdm = TermDocumentMatrix(text_corpus,control = list(removePunctuation = TRUE,
                                                      removeNumbers = TRUE,
                                                      tolower = TRUE))
  
  return(tdm)
}

getSentiments.TF_IDF.nrc = function(tdm.tfidf){
  
  m <- as.matrix(tdm.tfidf)
  
  word_tfidf <- sort(colSums(m), decreasing = TRUE)
  dm <- data.frame(word = names(word_tfidf), tfidf = word_tfidf)
  dm.subset <- dm[dm$tfidf>=quantile(dm$tfidf,0.25),]
  nrc.lex <- get_nrc_sentiment(as.character(dm.subset$word))
  
}

generateWordCloud.positive.tmStopWords = function(tdm.tm.stopword){
  
  
  # converting term document matrix to matrix
  m = as.matrix(tdm.tm.stopword)
  
  # get word counts in decreasing order
  word_freqs = sort(rowSums(m), decreasing = TRUE)
  
  # create a data frame with words and their frequencies
  dm = data.frame(word = names(word_freqs), freq = word_freqs)
  
  nrc.lexicons = get_nrc_sentiment(as.character(dm$word))
  tweets.positive = dm[nrc.lexicons$positive>0,]
  
}

generateWordCloud.negative.tmStopWords = function(tdm.tm.stopword){
  
  # converting term document matrix to matrix
  m = as.matrix(tdm.tm.stopword)
  
  # get word counts in decreasing order
  word_freqs = sort(rowSums(m), decreasing = TRUE)
  
  # create a data frame with words and their frequencies
  dm = data.frame(word = names(word_freqs), freq = word_freqs)
  
  nrc.lexicons = get_nrc_sentiment(as.character(dm$word))
  
  tweets.negative = dm[nrc.lexicons$negative>0,]
}

generateWordCloud.positive.TF_IDF = function(tdm.tfidf, tdm.tm.nostop){
  
  # converting term document matrix to matrix
  m <- as.matrix(tdm.tfidf)
  
  word_tfidf <- sort(colSums(m), decreasing = TRUE)
  # create a data frame with words and their frequencies
  dm <- data.frame(word = names(word_tfidf), tfidf = word_tfidf)
  #plot(dm$freq,type = "l")
  dm.subset <- dm[dm$tfidf>=quantile(dm$tfidf,0.25),]
  
  ## creating term frequency dataframe
  m.word.freq <- as.matrix(tdm.tm.nostop)
  word_freqs.word.freq <- sort(colSums(m), decreasing = TRUE)
  dm.word.freq <- data.frame(word = names(word_freqs.word.freq), freq = word_freqs.word.freq)
  
  ## subsetting the tdm 
  dm.word.freq.new <- dm.word.freq[dm.word.freq$word %in% dm.subset$word,]
  
  nrc.lexicons <- get_nrc_sentiment(as.character(dm.word.freq.new$word))
  tweets.positive <- dm.word.freq.new[nrc.lexicons$positive>0,]
}

generateWordCloud.negative.TF_IDF = function(tdm.tfidf, tdm.tm.nostop){
  # converting term document matrix to matrix
  m <- as.matrix(tdm.tfidf)
  
  word_tfidf <- sort(colSums(m), decreasing = TRUE)
  # create a data frame with words and their frequencies
  dm <- data.frame(word = names(word_tfidf), tfidf = word_tfidf)
  #plot(dm$freq,type = "l")
  dm.subset <- dm[dm$tfidf>=quantile(dm$tfidf,0.25),]
  
  ## creating term frequency dataframe
  m.word.freq <- as.matrix(tdm.tm.nostop)
  word_freqs.word.freq <- sort(colSums(m), decreasing = TRUE)
  dm.word.freq <- data.frame(word = names(word_freqs.word.freq), freq = word_freqs.word.freq)
  
  ## subsetting the tdm 
  dm.word.freq.new <- dm.word.freq[dm.word.freq$word %in% dm.subset$word,]
  
  nrc.lexicons <- get_nrc_sentiment(as.character(dm.word.freq.new$word))
  tweets.negative <- dm.word.freq.new[nrc.lexicons$negative>0,]
}

data("zipcode")
attach(zipcode)

getLatLong.zip = function(enter.zipcode,radius.mi)
{
  enter.zipcode = as.character(enter.zipcode)
  radius.mi = as.character(radius.mi)
  lat.long = zipcode[zip == enter.zipcode,c("latitude","longitude")]
  lat.long.mi = paste0(lat.long$latitude,",",lat.long$longitude,",",radius.mi,"mi")
  return(lat.long.mi)
}

# Functions for creating fluid page layouts in shiny Application.
# A fluid page layout consists of rows which in turn include columns
################################################################################
ui = fluidPage(
  
  #Setting a theme for the Shiny app| The theme that is used here is called 'darkly'
  #package used here is shinytheme
  theme = shinytheme("darkly"),
  # Defining the header Panel on the shiny application 
  #h3- argument is used to obtain a specific size for the header/ title.
  #windowTitle - The title that should be displayed by the browser window. 
  headerPanel(h3("Twitter Sentiment Analysis"), windowTitle = "Fanil"),
  #Sidebar Layout - used to create a layout with a sidebar and main area in the Shiny Aplication.
  sidebarLayout(
    #Create a sidebar panel containing input controls that can in turn be passed to sidebarLayout.
    #img argument is used to load an image into the sodeba panel of the shiny Application
    sidebarPanel(
      # radioButtons -Create a set of radio buttons used to select an item from a list.          
      radioButtons("typeInput", "Extract tweets by: ",
                   list("Hashtag & Location" = "hashtag", "Twitter Username"= "username")),
      #sliderInput -Constructs a slider widget to select a numeric value from a range.
      sliderInput("numberInput", "Select number of tweets",
                  min = 0, max = 3000, value = 100),
      #Creates a panel that is visible or not, depending on the value of the input.
      #Condition 1 - Only show this panel if input type is "Hashtag & Location"    
      conditionalPanel(
        condition = "input.typeInput == 'hashtag'",
        #Create an input control for entry of unstructured text values
        textInput("hashtagInput", "Enter search string","", placeholder = "input search string"),
        textInput("zipInput", "Enter zipcode (from 00210 to 99950)", placeholder = "input ZIP code"),
        textInput("radiusInput", "Enter radius (miles)", placeholder = "input miles")),
      
      #Condition 2 - Only show this panel if Input type is "Twitter Username"
      conditionalPanel(
        condition = "input.typeInput == 'username'",
        textInput("usernameInput", "Username", placeholder = "input username")),
      #actionButton - Used to create a go button, that allows the shiny Application the execute the input
      actionButton("goButton", "Search", icon("twitter"),
                   style="color: #fff; background-color: #337ab7") ,width = 3),
    
    #Panel to display output
    # mainPanel - Create a main panel containing output elements that can in turn be passed to sidebarLayout.
    mainPanel(
      #Tabsets - used for dividing output into multiple independently viewable sections.
      #Dividing the main panel into multiple tabs
      tabsetPanel(
        #tabPanel - Create a tab panel that can be included within a tabsetPanel.
        #Argument plotOutput - used to create a plot as an output element based on the inputid that is passed to it
        tabPanel("Sentiment Plots TM", plotOutput("plot1")),
        tabPanel("Sentiments Plots TFIDF", plotOutput("plot3")),
        tabPanel("+/- Plots TM", plotOutput("plot2")),
        tabPanel("+/- Plots TFIDF", plotOutput("plot4")),
        #navbarMenu - Creates a page that contains a top level navigation bar that can be used to toggle a set of tabPanel elements.
        navbarMenu("Word Clouds TM",
                   #tabPanel - Create a tab panel that can be included within a tabsetPanel
                   tabPanel("Positive", wordcloud2Output("wordCloud1", width = "100%", height = "400px")),
                   #wordcloud2Output -used to render a wordcloud object| uses library - wordcloud2
                   tabPanel("Negative", wordcloud2Output("wordCloud2", width = "100%", height = "400px"))),
        navbarMenu("Word Clouds TFIDF",
                   tabPanel("Positive", wordcloud2Output("wordCloud3", width = "100%", height = "400px")),
                   tabPanel("Negative", wordcloud2Output("wordCloud4", width = "100%", height = "400px"))),
        #dataTableOutput -used to render a table as an output
        tabPanel("Tweets", dataTableOutput("tweetTable"))
        ,type = "pills"), width = 9)
  )
)


