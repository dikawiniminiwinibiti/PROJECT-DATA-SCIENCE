library(tm)
library(rsconnect)   
library(base64enc)
library(shiny)
library(rtweet)
library(reactable)
library(scales)
library(reshape2)
library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(shinydashboard)
library(twitteR)
library(ROAuth)
library(plotly)
library(twitteR)
library(rvest)
library(wordcloud2)
library(textdata)

ui <- dashboardPage(
    dashboardHeader(title = h4(HTML("Sentiment Analysis <br/>Twitter Covid-19 ")), titleWidth = 230,
                    disable = FALSE),
    dashboardSidebar(
        sidebarPanel(
            h5(style="color:#ff6699", sliderInput("Tweets_to_Download",
                                                  "Jumlah Tweet :",
                                                  min = 500,
                                                  max = 2000,
                                                  value = 500,
                                                  step = 500)),
            h6(style = "color:#006d2c", selectInput("Input_Hashtag", "Hashtag to search:", c("#corona",
                                                                                             "#covid",
                                                                                             "#covid19",
                                                                                             "#coronavirus",
                                                                                             "#Covid-19")))
            , width = 0.3)
    ),
    
    
    dashboardBody(
        tags$head(tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
        .main-sidebar {
            background-color: skinblue !important;
          }
  
          
        "))),
        tabsetPanel(
            tabPanel(title = "Sentiment Analysis",
                     fluidRow(
                         valueBoxOutput("value1"),
                         valueBoxOutput("value2"),
                         valueBoxOutput("value3")),            
                     fluidRow(  
                         box(
                             title = "WordCloud"
                             ,status = "primary"
                             ,solidHeader = TRUE 
                             ,collapsible = TRUE
                             ,wordcloud2Output("wordcloud", height = "300px")
                         ),
                         box(
                             title = "Top 10 words"
                             ,status = "primary"
                             ,solidHeader = TRUE 
                             ,collapsible = TRUE 
                             ,plotOutput("top10", height = "300px")
                         )),
                     fluidRow(
                         box(
                             title = "Top Positive and Negative Words",
                             status = "primary",
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             plotOutput("bing", height = "400px")
                         )),
            ),
            tabPanel(title = "Tweet Table",
                     fluidRow(
                         valueBoxOutput("value4"),
                         valueBoxOutput("value5"),
                         valueBoxOutput("value6")),
                     fluidRow(
                         valueBoxOutput("value7"),
                         valueBoxOutput("value8"),
                         valueBoxOutput("value9")),
                     fluidRow(reactableOutput("tweet_table")))
        )))

server <- function(input, output) {
    #Save app credentials for access to tweets
    consumerKey <- "TLwcYDEblVFgtDIqeqGXY05zH"
    
    consumerSecret <- "Wpy8VEH6eGliqisMCNonqC7q9Z3YrxpmjxkiIyMdSj8qsocdSw"
    
    accessToken <- "1097552545411678208-zIYNJm6l1aOwxUwCtLbgbpmjGciJtp"
    
    accessTokenSecret <-  "d3V7AQ2BSkcj5qCZIjuSeuOPpsfUfM3eRb1YyMmlLdoPG"
    
    #set up 
    setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
    
    dataInput <- reactive({
        data <- searchTwitter(input$Input_Hashtag, n = input$Tweets_to_Download, 
                              resultType = "recent", lang = "en")
        
        twListToDF(data)
    })
    
    #Create reactive word cloud
    output$wordcloud <- renderWordcloud2({
        ##Word clouds for all tweets
        table_1 <- dataInput() %>% 
            mutate(rowmumber = row_number()) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("rt", "", text)) %>%
            mutate(text = gsub("RT", "", text)) %>%
            mutate(text = gsub("https","", text)) %>% 
            mutate(text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)) %>% 
            mutate(text = gsub("[[:punct:]]", " ", text)) %>% 
            mutate(text = gsub("t.co", "", text)) %>% 
            mutate(text = removeNumbers(text)) %>% 
            mutate(text = gsub("coronoavirus", "coronavirus", text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            count(word, sort = T) 
        
        wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                                 fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                                 minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                                 rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                                 widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
        {
            if ("table" %in% class(data)) {
                dataOut = data.frame(name = names(data), freq = as.vector(data))
            }
            else {
                data = as.data.frame(data)
                dataOut = data[, 1:2]
                names(dataOut) = c("name", "freq")
            }
            if (!is.null(figPath)) {
                if (!file.exists(figPath)) {
                    stop("cannot find fig in the figPath")
                }
                spPath = strsplit(figPath, "\\.")[[1]]
                len = length(spPath)
                figClass = spPath[len]
                if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
                    stop("file should be a jpeg, jpg, png, bmp or gif file!")
                }
                base64 = base64enc::base64encode(figPath)
                base64 = paste0("data:image/", figClass, ";base64,", 
                                base64)
            }
            else {
                base64 = NULL
            }
            weightFactor = size * 180/max(dataOut$freq)
            settings <- list(word = dataOut$name, freq = dataOut$freq, 
                             fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                             minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                             gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                             shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                             ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
            chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                              width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                                      browser.padding = 0, browser.fill = TRUE))
            chart
        }
        
        wordcloud2a(table_1, size = 0.75, shape = "circle", ellipticity = 0.65)
    })
    
    #Build value box
    output$value1 <- renderValueBox({
        n <- dataInput() %>% 
            mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
            mutate(text = tolower(text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            inner_join(get_sentiments("bing")) %>% 
            group_by(word, sentiment) %>% 
            count(word, sentiment, sort = T) %>% 
            ungroup() %>% 
            group_by(sentiment) %>% 
            summarise(n = sum(n)) %>% 
            mutate(n = round(n/sum(n), 2)) %>% 
            filter(sentiment == "positive")
        
        n <- n[,2]
        
        
        #BOX POSITIF MULAI MENGGUNAKAN BING
        
        valueBox(paste(n, "%"), subtitle = "Positive Tweets", 
                 icon = icon("smile", lib ="font-awesome" ), color = "aqua")
    })
    
    output$value2 <- renderValueBox({
        n <- dataInput()[,1:16] %>% 
            mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
            mutate(text = tolower(text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            inner_join(get_sentiments("bing")) %>% 
            group_by(word, sentiment) %>% 
            count(word, sentiment, sort = T) %>% 
            ungroup() %>% 
            group_by(sentiment) %>% 
            summarise(n = sum(n)) %>% 
            mutate(n = round(n/sum(n), 2)) %>% 
            filter(sentiment == "negative")
        
        n <- n[,2]
        
        #BOX NEGATIF 
        
        valueBox(paste(n, "%"), subtitle = "Negative Tweets", 
                 icon = icon("angry", lib ="font-awesome" ), color = "orange")
    })
    
    output$value3 <- renderValueBox({
        
        #BOX TOTAL
        tweets_count <- dataInput() %>% 
            nrow()
        
        
        valueBox(tweets_count, subtitle = "Total Tweets", 
                 icon = icon("chart-bar", lib ="font-awesome" ), color = "green")
    })
    
    output$top10 <- renderPlot({
        topwords <-  dataInput()[,1:16] %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("rt", "", text)) %>% 
            mutate(text = gsub("RT", "", text)) %>%
            mutate(text = gsub("https","", text)) %>%
            mutate(text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)) %>% 
            mutate(text = gsub("[[:punct:]]", " ", text)) %>% 
            mutate(text = gsub("t.co", "", text)) %>% 
            mutate(text = gsub("covid", "", text)) %>% 
            mutate(text = removeNumbers(text)) %>% 
            mutate(text = gsub("19", "", text)) %>% 
            mutate(text = gsub("ppl", "people", text)) %>% 
            mutate(text = gsub("coronoavirus", "coronavirus", text)) %>% 
            mutate(text = gsub("en", "", text)) %>% 
            mutate(rowmumber = row_number()) %>%#mutate row numbers
            mutate(text = str_remove(text, "rt")) %>% 
            unnest_tokens(word, text) %>%  #unnest words
            anti_join(stop_words) %>% #removes stop words
            count(word, sort = T) %>%#count most occuring words
            top_n(10) #select top 10
        
        ggplot(topwords, aes(reorder(word, n), n, fill = word)) + #piped into ggplot
            geom_bar(stat = "identity", show.legend = F) + coord_flip() +
            labs(x = "Word", y = "count") + theme_minimal() +
            theme(axis.title.x = element_text(face ="bold", size = 15),
                  axis.title.y = element_text(face = "bold", size = 15),
                  axis.text = element_text(face = "bold"))
    })
    
    
    
    output$bing <- renderPlot({
        pos_vs_neg <- dataInput()[,1:16] %>% 
            mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("fidelity", " ", text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            inner_join(get_sentiments("bing")) %>% 
            group_by(word, sentiment) %>% 
            count(word, sentiment, sort = T) %>% 
            ungroup() %>% 
            group_by(sentiment) %>% 
            top_n(10)
        
        ggplot(pos_vs_neg, aes(reorder(word, n), n, fill = word)) +
            geom_col(show.legend = F) +
            facet_wrap(~sentiment, scales = "free_y") +
            coord_flip() + 
            labs(y = "Count", x = "Words") +
            theme_bw()
        
        
    })
    
    
    
    
    #TABEL DATA
    tweet_table_data <- reactive({
        req(dataInput())
        dataInput() %>% 
            mutate(rowmumber = row_number()) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("rt", "", text)) %>%
            mutate(text = gsub("RT", "", text)) %>%
            mutate(text = gsub("https","", text)) %>% 
            mutate(text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)) %>% 
            mutate(text = gsub("[[:punct:]]", " ", text)) %>% 
            mutate(text = gsub("t.co", "", text)) %>% 
            mutate(text = removeNumbers(text)) %>%
            select(created,screenName,text)
    })
    
    output$tweet_table <- renderReactable({
        reactable(tweet_table_data(), 
                  filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                  showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(10, 50, 75, 100, 200), 
                  columns = list(
                      
                      created = colDef(defaultSortOrder = "asc"),
                      screenName = colDef(defaultSortOrder = "asc"),
                      text = colDef(html = TRUE, minWidth = 190, resizable = TRUE)
                  )
        )
    })
    
    
}

shinyApp(ui = ui, server = server)