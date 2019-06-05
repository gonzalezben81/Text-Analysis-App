#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)
library(tm)
library(wordcloud)
library(SnowballC)
library(rvest)
library(dplyr)
library(reshape2)
# library(tidytext)
library(syuzhet)
library(pander)
library(xlsx)
library(ggplot2)
library(RWeka)
library(RWekajars)
library(partykit)
library(DT)
library(shinydashboard)
library(qdap)
library(rJava)

romeo <- readLines("romeo.txt")
othello <- readLines("othello.txt")
midsummers <- readLines("midsummers.txt")


###Dashboard Beginning for Text Analysis App ######
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Text Analysis"),
                    ###Dashboard Sidebar Menu####
                    dashboardSidebar(
                      sidebarMenu(
                        ##Tab One
                        menuItem("File Upload",tabName = "file",icon = icon("file-text-o")),
                        ##Tabe One-Half
                        menuItem("Web Scrape Text",tabName = "scrape_text",icon = icon("fab fa-internet-explorer")),
                        ##Tab Two
                        menuItem("Text Output",tabName = "text",icon = icon("file-text-o")),
                        ##Tab WordBreakdown
                        menuItem("Word Breakdown",tabName = "breakdown",icon = icon("table")),
                        ##Tab Three
                        menuItem("Wordcloud",tabName = "wordcloud",icon = icon("cloud")),
                        ##Tab Four
                        menuItem("Word Count Bar Plot",tabName = "barplot",icon = icon("bar-chart-o")),
                        ##Tab Five
                        menuItem("Emotional Sentiment",tabName = "emotionalsentiment",icon = icon("bar-chart-o")),
                        ##Tab Six
                        menuItem(paste("Positive vs. Negative Sentiment"),tabName = "pnsentiment",icon = icon("bar-chart-o")),
                        ##Tab Percentage
                        menuItem("Emotion Percentages Table",tabName = "emotionalpercentages",icon = icon("percent")),
                        ##Tab Nine
                        #menuItem("Barplot % by Word",tabName = "plotg",icon = icon("bar-chart-o")),
                        ##Tab Seven
                        menuItem("Plot Trajectory",tabName = "plottrajectory",icon = icon("line-chart")),
                        ##Lexical Dispersion Plot
                        menuItem("Lexical Dispersion Plot",tabName = "lexical_plot",icon = icon("line-chart")),
                        ##Tab Eight
                        menuItem("Word Tokenizer",tabName = "wordtokenizer",icon = icon("table")),
                        ##Tab Ten
                        #menuItem("Sentence Sentiment",tabName = "sentencefinder",icon = icon("table")),
                        ##Works Cited
                        menuItem("References:",tabName = "workscited"),
                        ##Text Analysis Report
                        menuItem("Text Analysis Report",tabName = 'analysisreport'),
                        ##Contact:
                        menuItem("Contact:",tabName = "contact"),
                        ##Digital Ocean Credit
                        menuItem("Digital Ocean Credit",tabName="digitalocean")
                        
                      )),
                    
                    ###Beginning of Dashboard Body####
                    dashboardBody(
                      tabItems(
                        ###File Upload Tab
                        tabItem(tabName = "file",
                                fileInput("selection", "Upload Text File:",multiple = TRUE),
                                helpText(paste("Please upload a plain .txt file with the text", 
                                               "you would like to analyze."),
                                         br(),
                                         br(),
                                         selectInput("datasetten", "Choose Sample Text:", 
                                                     choices = c("Romeo and Juliet", "Othello", "Midsummer Nights Dream"),selected = "Romeo and Juliet"),
                                         br(),
                                         
                                         downloadButton("downloadromeo","Downaload Sample Text"),
                                         #paste("Sample Text:"),a("Romeo and Juliet",href="https://www.rgonzo.us/shiny/textfiles/text/romeo.txt"),
                                         br(),
                                         br(),
                                         paste("Windows Users: Please use a .txt plain text file extension via NOTEPAD *."),
                                         br(),
                                         br(),
                                         paste("Linux Users: Please use a .txt plain text file extension."),
                                         br(),
                                         br(),
                                         paste("Mac Users: Please use a .txt plain text file extension via TEXTEDIT *."),
                                         br(),
                                         br(),
                                         a("Plain text tutorial: Windows and Mac",href="http://support.smqueue.com/support/solutions/articles/159273-saving-a-text-file-on-a-mac-or-pc-in-utf-8",target = "_blank"),
                                         br(),
                                         br(),
                                         tags$b(paste("* Please ensure the file uploaded utilizes UTF-8 encoding")))),
                        ###Text Output Tab####
                        tabItem(tabName = "text",
                                helpText(paste("This tab displays the uploaded text file.")),
                                actionButton("display","Display Text"),
                                br(),
                                br(),
                                box(title = "Text Ouput",textOutput("text",inline = FALSE),width = 450)),
                        ###Word Frequency Barplot Tab####
                        tabItem(tabName = "barplot",
                                helpText(paste("This tab allows you to display the frequency of words in the uploaded text "),
                                         br(),
                                         paste("via a bar chart. The bar chart by default displays the first through tenth"),
                                         br(),
                                         paste("most frequent words in the text.")),
                                actionButton(inputId = "barplot",label = "Create Barplot"),
                                downloadButton(outputId = "downloadsix",label = "Download Barplot"),
                                selectInput(inputId = "download6",label = "Choose Format",choices = list("png","pdf","bmp","jpeg")),
                                numericInput(inputId = "numeric",label =  " From:",min = 1,max = 50000,step = 1,value = 1),
                                numericInput(inputId = "numeric2",label =  "To:",min = 1,max = 50000,step = 1,value = 10), 
                                checkboxInput(inputId = "horz",label = "Horizontal Bars",value = FALSE),
                                selectInput(inputId = 'color',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                            selected = "Blue"),
                                plotOutput("plot2")),
                        ###WordCloud Tab####
                        tabItem(tabName = "wordcloud",
                                fluidRow(
                                  box(actionButton(inputId = "update", label = "Create Wordcloud"),
                                      helpText(paste("The minimum frequency refers to the minimum number of times"),
                                               br(),
                                               paste("the word needs to appear in the uploaded text to be included in the wordcloud.")),
                                      sliderInput("freq","Minimum Frequency:",min = 1,  max = 500, value = 10),
                                      helpText(paste("The maximum number of words refers to the maximum number of words"),
                                               br(),
                                               paste("you want to appear in the wordcloud that is created.")),
                                      sliderInput("max","Maximum Number of Words:",min = 1,  max = 1000,  value = 25),
                                      selectInput(inputId = "pal",label = "Cloud Color",choices = c("Dark"="Dark2","Pastel One"="Pastel1","Pastel Two"="Pastel2","Set One"="Set1",
                                                                                                    "Set Two"="Set2","Set Three"="Set3"),selected = "Dark2"),
                                      downloadButton("download1","Download Wordcloud"),
                                      selectInput(inputId = "download3",label = "Choose Wordcloud Format",choices = list("png","pdf","bmp","jpeg"))),
                                  box(plotOutput("plot")))),
                        ###Emotional Sentiment Bar Chart Tab####
                        tabItem(tabName = "emotionalsentiment",
                                helpText(paste("This tab allows you to calculate eight types of emotion present within the uploaded text."),
                                         br(),
                                         br(),
                                         paste("The following types of emotion are calculated:"),
                                         br(),
                                         br(),
                                         tags$b(paste("Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, and Trust.")),
                                         br(),
                                         paste("The emotions calculated are the 8 basic universal emotions conveyed by humans in all cultures."),
                                         br(),
                                         paste("Each bar represents the overall percentage of each emotion present within the uploaded text file.")),
                                actionButton("sentiment","Calculate Emotion"),
                                br(),
                                br(),
                                downloadButton("downloadseven","Download Emotional Sentiment Barplot"),
                                selectInput(inputId = 'colornow',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                            selected = "Blue"),
                                plotOutput("nrcplot")),
                        ###Positive & Negative sentiment Tab####
                        tabItem(tabName = "pnsentiment",
                                helpText(paste("This tab allows you to calculate the positive and negative sentiment present within the uploaded text."),
                                         br(),
                                         br(),
                                         paste("The following sentiments are calculated:"),
                                         br(),
                                         br(),
                                         tags$b(paste("Positive & Negative")),
                                         br(),
                                         paste("The bar graphs displayed are in relation to the percentage of positive and negative words present in the uploaded text.")),
                                actionButton("negative","Calculate Positive & Negative Sentiment"),
                                br(),
                                br(),
                                downloadButton(outputId = "downloadeight",label = "Download Pos vs. Neg Barplot"),
                                selectInput(inputId = 'colornow2',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                            selected = "Blue"),
                                br(),
                                plotOutput("nrcplot2")),
                        ###Emotional Percentages Table Tab####
                        tabItem(tabName = "emotionalpercentages",
                                box(helpText(paste("The data table created calculates the percentage of each emotion", 
                                                   "present within the uploaded text file and outputs it to a table."),
                                             br(),
                                             br(),
                                             paste("The following emotions are calculated:"),
                                             br(),
                                             tags$b(paste("Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, and Trust.")),
                                             br(),
                                             br(),
                                             paste("The emotions calculated are the 8 basic universal emotions conveyed by humans in all cultures."),
                                             # paste("The following sentiments are also calculated:"),
                                             # br(),
                                             # tags$b(paste("Positive & Negative")),
                                             br(),
                                             br(),
                                             a("Reference: NRC Package",href="https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html",target = "_blank")),
                                    br(),
                                    br(),
                                    actionButton("scsentiment","Calculate Emotional %"),
                                    br(),
                                    br(),
                                    downloadButton("downloadfour","Download Emotional %")),
                                box(DT::dataTableOutput("scosentiment"))),
                        ###Text Plot Trajectory Tab#####
                        tabItem(tabName = "plottrajectory",
                                helpText(paste("This tab allows you to plot the trajectory of the uploaded text."),
                                         br(),
                                         br(),
                                         paste("The plot will display the overall emotion of pieces of the text at different successive linear locations in the text. Large text files will be more condensed than small text files."),
                                         br(),
                                         
                                         paste("The plot displayed can be thought of as the story arc in a movie or book. If text items besides books are used it is highly suggested to order the text correctly. The graph will show"),
                                         br(),
                                         paste("how the emotional content of the uploaded text has changed over time e.g. beginning of a text to the end of the text.The Narrative Timeline axis refers to how the book,text, or comments"),
                                         br(),
                                         paste("have changed from the beginning of the text to the end of the same text being analyzed. The Emotional Valence axis refers to the positive/good-ness and the negative/bad-ness of the text."),
                                         br(),
                                         paste(" Positive valence or upward motion can be seen as the good linear parts of a story, while Negative Valence can be thought of as bad or negative linear parts of the story. Therefore,"),
                                         br(),
                                         paste(" as the plotted line moves up or down it is in turn visualizing the good or bad parts of the text being analyzed.")),
                                actionButton("trajectory","Create Plot Trajectory"),
                                br(),
                                br(),
                                downloadButton("downloadnine","Download Plot Trajectory"),
                                plotOutput("nrcplot3")),
                        ###Text Bar Chart Tab #####
                        tabItem(tabName = "plotg",
                                helpText(paste("This tab allows you to create a bar chart that displays both the type of emotion and  type of sentiment"),
                                         br(),
                                         paste("present within the uploaded text file. The percentage of each emotion and sentiment  is displayed at "),
                                         br(),
                                         paste("the top of each bar.")),
                                actionButton("gplottwo","Create Barplot"),
                                br(),
                                br(),
                                plotOutput("gplot")),
                        ###Word Tokenizer Tab####
                        tabItem(tabName = "wordtokenizer",
                                helpText(paste("This tab allows you to utilize a  word tokenizer to see which words in a text are displayed together."),
                                         br(),
                                         paste("You can choose to display words from 1 to 5 tokens. Therefore, words that appear next to each other"),
                                         br(),
                                         paste("in the uploaded text will be displayed. If you choose 2, then two words that appear next to each"),
                                         br(),
                                         paste("other will be displayed. You can choose up to 5 words that display next to each other, thus allowing"),
                                         br(),
                                         paste("you ,the end user, to look for patterns in any text.")),
                                actionButton("bigram","Create Tokenizer Table"),
                                numericInput(inputId ="numeric3",label="Tokenizer Min.",min=1,max=5,value=2),
                                numericInput(inputId="numeric4",label="Tokenizer Max",min=1,max=5,value=2),
                                DT::dataTableOutput("nrcplot4")
                        ),
                        ###Sentence Sentiment Finder Tab#####
                        tabItem(tabName = "sentencefinder",
                                helpText(paste("This tab allows you to display sentences by emotion. A sentence may appear more"),
                                         br(),
                                         paste("than once if an one emotion is closely related to another: e.g. anger and disgust.")),
                                actionButton("emotion","Get Sentence Sentiment"),
                                br(),
                                br(),
                                downloadButton("downloadfive", label="Download Sentence Breakdown"),
                                br(),
                                helpText(paste("Select the following number below that corresponds with the emotion you want to display:"),
                                         br(),
                                         br(),
                                         tags$b(paste("1 = Anger   2 = Anticipation   3 = disgust   4 = Fear   5 = Joy")),
                                         br(),
                                         br(),
                                         tags$b(paste("6 = Sadness   7 = Surprise   8 = Trust    9 = Negative   10 =  Positive"))),
                                numericInput(inputId = 'emselect',label =  'Emotion Selector',
                                             value = 1,min = 1,max = 10,step = 1),
                                br(),
                                br(),
                                DT::dataTableOutput("nrcplot5")),
                        ###Word Frequency Tab ######
                        tabItem(tabName = "breakdown",
                                helpText(paste("This tab allows you to display the frequency of each word present within the uploaded text file."),
                                         br(),
                                         paste("The frequency of each word will be shown and can be searched via the interactive table displayed below.")),
                                box(actionButton("wbdown","Create Word Breakdown"),
                                    br(),
                                    br(),
                                    downloadButton("downloadtwo", label="Download Word Breakdown")),
                                DT::dataTableOutput("wordbreakdown")),
                        ###Text Analysis Report Tab #####
                        tabItem(tabName = "analysisreport",
                                downloadButton(outputId = "text_report",label = "Download Text Analysis Report")),
                        ###Works Cite ####
                        tabItem(tabName = "workscited",
                                helpText(strong("                                     References :"),
                                         br(),
                                         br(),
                                         paste("Cashell, D. (2014)."),em("Social media sentiment analysis using data mining techniques"),paste(". National 	College of Ireland."),
                                         br(),
                                         br(),
                                         paste("Hennessey, A. (2014)."),em("Sentiment analysis of twitter: using knowledge based and machine learning techniques"),paste(". National College of Ireland."),
                                         br(),
                                         br(),
                                         paste("Jockers, M. (2016)."),em("Introduction to the syuzhet package"),paste(".Retrieved from:"),a("https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html",href="https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html",target="_blank"),
                                         br(),
                                         br(),
                                         paste("Mohammad, S. (2013)."),em("NRC word-emotion association lexicon (aka emolex)"),paste(".Retrieved from:"),a("http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm",href="http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm",target="_blank"),
                                         br(),
                                         br(),
                                         paste("Mullen. (2014)."),em("Introduction to sentiment analysis"),paste(".Retrieved from:"),a("https://lct-master.org/files/MullenSentimentCourseSlides.pdf",href="https://lct-master.org/files/MullenSentimentCourseSlides.pdf",target="_blank"),
                                         br(),
                                         br(),
                                         paste("Robinson, D. (2016)."),em("Text analysis of trump's tweets confirms he writes only the angrier android half"),paste(".Retrieved from:"),a("http://varianceexplained.org/r/trump-tweets/",href= "http://varianceexplained.org/r/trump-tweets/",target="_blank"),
                                         br(),
                                         br(),
                                         paste("Smith, D. (2015)."),em("Comparing subreddits, with latent semantic analysis in r"),paste(". Retrieved from:"),a("http://blog.revolutionanalytics.com/2017/03/comparing-subreddits.html",href="http://blog.revolutionanalytics.com/2017/03/comparing-subreddits.html",target="_blank"))),
                        tabItem(tabName = "contact",
                                helpText(paste("Application Author: Ben Gonzalez"),
                                         br(),
                                         br(),
                                         paste("Email: gonzalezben81@gmail.com"),
                                         br(),
                                         br(),
                                         paste("Phone: 314-472-5417"))),
                        tabItem(tabName = "digitalocean",
                                helpText(paste("Build your own Linux Server and host your own app with a $10 credit on Digital Ocean"),
                                         br(),
                                         br(),
                                         paste("Click on this link to get your Digital Ocean Credit:"),a("Digital Ocean $10 Credit",href="https://m.do.co/c/b72d3479beb8",target="_blank")
                                )),
                        ###Web Scrape Tab####
                        tabItem(tabName = "scrape_text",
                                textInput(inputId = "text",label = "Enter Website url:",value = "",placeholder = "Enter valid website here"),
                                br(),
                                helpText("Enter the HTML node such as 'p' for paragraph to scrape the relevant data from the website. You can then download the text file to save it and upload it for analysis later."),
                                textInput(inputId = "node",label = "HTML Node",value = "",placeholder = "Enter valid CSS selector here"),
                                h4("Reference Links:"),
                                br(),
                                a(img(src="~/www/CSSTWO.png",width="35",height="35"),href="http://www.w3schools.com/css/default.asp", target="_blank"),
                                a(img(src="~/www/html.png",width="35",height="35"),href="http://www.w3schools.com/html/default.asp", target="_blank"),
                                a(img(src="~/www/java2.png",width="35",height="35"),href="http://www.w3schools.com/js/default.asp", target="_blank"),
                                hr(),
                                actionButton(inputId = "do",label = "Get Data",icon = icon("gears")),
                                br(),
                                br(),
                                textInput(inputId = "name",label = "Save File As:",value = "",placeholder = "Type File Name Here"),
                                # downloadButton('download', 'Download',class = "butt")
                                br(),
                                downloadButton("download", label="Download"),
                                hr(),
                                verbatimTextOutput("printoutput")
                        ),
                        tabItem(tabName = "lexical_plot",
                                actionButton("lexical_run","Create Lexical Dispersion Plot"),
                                hr(),
                                textInput(inputId = "words",label = "Word to search for in text:"),
                                hr(),
                                plotOutput("distPlot")
                        )
                        
                        
                        
                      ))
                    ###End of Dashboard Body####
)

###Dashboard End for Text Analysis App ######


# Define server logic required to run the Text Analysis App
server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)
  memory.limit(size = 4095)
  
  ##Code for uploading Text File from User ##########
  
  ford <- reactive({ 
    req(input$selection) ## ?req #  require that the input is available
    
    inFile <- input$selection 
    
    df <- readLines(inFile$datapath)
    
    return(df)
    
  })
  
  
  ##Create DocumentTerm Matrix (DTM) ###########
  
  getTermMatrix <- function(f) {
    
    
    text <- readLines(f$datapath,encoding = "UTF-8")
    
    docs<-Corpus(VectorSource(text))
    
    docs<-tm_map(docs, content_transformer(tolower))
    docs<-tm_map(docs, removePunctuation)
    docs<-tm_map(docs, removeNumbers)
    docs<-tm_map(docs, removeWords,
                 c(stopwords("SMART"),input$words))
    
    myDTM = TermDocumentMatrix(docs,
                               control = list(minWordLength = 1,wordLengths=c(0,Inf)))
    
    m = as.matrix(myDTM)
    
    sort(rowSums(m), decreasing = TRUE)
  }
  
  terms <- reactive({
    
    getTermMatrix(input$selection)
    
    
  })
  
  
  
  
  ##Create Text Terms Object ########### 
  text_terms <-reactive({
    
    
    doc_terms<- ford()
    
    # Make a vector source: text_source
    doc_source<-VectorSource(doc_terms)
    
    ## text_source is already in your workspace
    
    # Make a volatile corpus: text_corpus
    doc_corpus <- VCorpus(doc_source)
    ## text_source is already in your workspace
    
    
    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),input$words))
      return(corpus)
    }
    
    doc_corp<-clean_corpus(doc_corpus)
    
    doc_dtm<-DocumentTermMatrix(doc_corp)
    
    # Convert text_dtm to a matrix: text_m
    doc_m<-as.matrix(doc_dtm)
    
    
    # Calculate the rowSums: term_frequency
    doc_frequencyone<-rowSums(doc_m)
    
    # Sort term_frequency in descending order
    doc_frequency<-sort(doc_frequencyone,decreasing=TRUE)
    
  })
  
  ###Renders WordCloud Plot####
  observeEvent(input$update,{output$plot <- renderPlot({
    inFile <- input$selection
    if (is.null(inFile))
      return("Please Upload File")
    withProgress(message = 'Creating WordCloud',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.75)
                   }
                 },env = parent.frame(n=1))
    ##Wordcloud code
    set.seed(1234)
    v <- terms()
    wordcloud(names(v), v, scale=c(6,0.5),
              min.freq = input$freq, max.words=input$max,
              rot.per=0.35,
              colors=brewer.pal(8, input$pal))
  })})
  
  
  ##Renders Barplot plot code ######
  
  observeEvent(input$barplot,{output$plot2<-renderPlot({
    withProgress(message = 'Creating BarPlot',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    doc_terms<- ford()
    
    # Make a vector source: text_source
    doc_source<-VectorSource(doc_terms)
    
    # Make a volatile corpus: text_corpus
    doc_corpus <- VCorpus(doc_source)
    
    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),c(input$words)))
      return(corpus)
    }
    
    doc_corp<-clean_corpus(doc_corpus)
    
    doc_dtm<-DocumentTermMatrix(doc_corp)
    
    # Convert text_dtm to a matrix: text_m
    doc_m<-as.matrix(doc_dtm)
    
    # Calculate the rowSums: term_frequency
    doc_frequencyone<-colSums(doc_m)
    
    # Sort term_frequency in descending order
    doc_frequency<-sort(doc_frequencyone,decreasing=TRUE)
    # termstwo<-text_terms()
    
    
    # Plot a barchart of the 10 most common words,  
    barplot(doc_frequency[input$numeric:input$numeric2],col=input$color,horiz = input$horz,las=2)
  })})
  
  
  ## Download code for wordcloud picture download ####
  
  output$download1 <- downloadHandler(
    filename = function() { paste("WordCloud",input$download3,sep = ".") },
    content = function(file) {
      if(input$download3=="png")
        png(file)
      else if (input$download3=="jpeg")
        jpeg(file)
      else if (input$download3=="bmp")
        bmp(file)
      else if (input$download3=="pdf")
        pdf(file)
      set.seed(1234)
      v <- terms()
      wordcloud(names(v),v, scale=c(6,0.5),
                min.freq = input$freq, max.words=input$max,
                rot.per=0.35,
                colors=brewer.pal(8, input$pal))
      dev.off()
    })
  
  
  
  ##Displays Text of Uploaded File ###############
  
  observeEvent(input$display,{output$text<-renderText({
    inFile <- input$selection
    if (is.null(inFile))
      return("Please Upload File")
    ford()})})
  
  ## Creates word breakdown matrix for csv file #####
  
  texterdf2<- reactive({
    
    withProgress(message = 'Downloading CSV File',
                 value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    doc_terms<- ford()
    
    # Make a vector source
    doc_source<-VectorSource(doc_terms)
    
    # Make a volatile corpu
    text <- VCorpus(doc_source)
    
    ##Function to Clean the Corpus
    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus<- tm_map(corpus,removeNumbers)
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),"the","you","httpstco","for","amp","today","--"))
      return(corpus)
    }
    
    # Apply your customized function to the text_corp: clean_corp
    text_corp<-clean_corpus(text)
    
    
    # Create the dtm from the corpus: text_dtm
    text_dtm<-DocumentTermMatrix(text_corp)
    
    # Convert text_dtm to a matrix: text_m
    text_m<-as.matrix(text_dtm)
    
    ## Calculate the rowSums: term_frequency ##################################################################
    term_frequency<-colSums(text_m)
    
    # Sort term_frequency in descending order
    term_frequency<-sort(term_frequency,decreasing=TRUE)
    
    ##Creates data frame of words ########
    text_freq<-data.frame(term=names(term_frequency),num=term_frequency)
    text_freq
    return(text_freq)
    
  })
  
  ##Textbreakdown Download ###########
  
  output$downloadtwo <- downloadHandler(
    filename = function() { paste("TextBreakDown",input$name, sep='',".csv") },
    content = function(file) {
      write.csv(texterdf2(), file)
      
    })
  
  ##Emotional Sentiment Analysis ###########
  observeEvent(input$sentiment,{output$nrcplot<-renderPlot({
    withProgress(message = 'Calculating Emotional Sentiment by Word',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    value<- ford()
    
    value <- get_nrc_sentiment(value)
    
    value
    
    #Barplot of Emotional Sentiment
    barplot(
      sort(colSums(prop.table(value[, 1:8]))),
      # horiz = input$horz2,
      cex.names = 0.7,
      las = 1,
      main = "Emotional Sentiment by Word"
      ,col = input$colornow
    )
    
  })})
  
  ##Positive and Negative Sentiment Analysis ##########
  
  ##Sentiment Try 
  observeEvent(input$negative,{output$nrcplot2<-renderPlot({
    withProgress(message = 'Calculating Positive & Negative Sentiment by Word',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    value<- ford()
    
    value <- get_nrc_sentiment(value)
    
    value
    
    ##Barplot of Emotional Sentiment
    barplot(
      sort(colSums(prop.table(value[, 9:10]))),
      # horiz = input$horz2,
      cex.names = 0.7,
      las = 1,
      main = "Positive vs. Negative Sentiment"
      ,col = input$colornow2
    )
    
  })})
  
  ##Get Trajectory #########
  
  ## Plot Trajectory #########
  observeEvent(input$trajectory,{output$nrcplot3<-renderPlot({
    withProgress(message = 'Creating Plot Trajectory',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    value<- ford()
    
    s_v <- get_sentences(value)
    s_v_sentiment <- get_sentiment(s_v)
    plot(
      s_v_sentiment, 
      type="l", 
      main="Plot Trajectory", 
      xlab = "Narrative Timeline", 
      ylab= "Emotional Valence"
    )
    
  })})
  
  ##Tokenizer Table ###################
  
  ##Reactive for Tokenizer Table 
  wordbreak2d<-reactive({
    
    doc_terms<- ford()
    
    # Make a vector source: doc_source
    doc_source<-VectorSource(doc_terms)
    
    
    
    # Make a volatile corpus: doc_corpus
    doc_corpus <- VCorpus(doc_source)
    
    
    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),c(input$words)))
      return(corpus)
    }
    
    
    doc_corp<-clean_corpus(doc_corpus)
    
    tokenizer<-function(x)
      NGramTokenizer(x,Weka_control(min=input$numeric3,max=input$numeric4))
    
    doc_dtm<-DocumentTermMatrix(doc_corp,control = list(tokenize = tokenizer))
    
    
    # Convert doc_dtm to a matrix: doc_m
    doc_m<-as.matrix(doc_dtm)
    
    
    # Calculate the rowSums: term_frequency
    doc_frequencyone<-colSums(doc_m)
    
    
    # Sort term_frequency in descending order
    doc_frequency<-names(doc_frequencyone)
    
    doc_frequency <- as.data.frame(doc_frequency)
    
    colnames(doc_frequency) <- c("Tokenized Words")
    
    doc_frequency
    
  })
  
  ###Renders the bigram table output to the end user
  observeEvent(input$bigram,{output$nrcplot4<-DT::renderDataTable({
    withProgress(message = 'Creating Bigram Table',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    DT::datatable(
      wordbreak2d(),extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ))
    
  })})
  
  ##Sentence Finder ##########
  
  texterdf5<- reactive({
    value<- ford()
    
    s_v <- get_sentences(value)
    
    nrc_data <- get_nrc_sentiment(s_v)
    
    emotion_conveyed <- which(nrc_data[,input$emselect] > 0)
    
    final <- as.matrix(s_v[emotion_conveyed])
    
    final
  })
  
  observeEvent(input$emotion,{output$nrcplot5<-DT::renderDataTable({
    withProgress(message = 'Getting Sentences',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    DT::datatable(
      texterdf5(),extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ))
    
  })})
  
  ##Sentiment Analysis Score ###########
  observeEvent(input$scsentiment,{output$scosentiment<-DT::renderDataTable({
    withProgress(message = 'Calculating Emotional Sentiment',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    value<- ford()
    
    value <- get_nrc_sentiment(value)
    
    prop.table(value[,1:8])
    
    sentimentscores <- round(colSums(prop.table((value[,1:8])))*100,digits = 1)
    
    sentimentscores <- as.data.frame(sentimentscores)
    colnames(sentimentscores) <- c("Percentages")
    
    
    Emotions <- c("anger","anticipation","disgust","fear","joy","sadness",
                  "surprise","trust")
    
    
    Percentages<- sentimentscores$Percentages
    emotionality<- cbind(Emotions,Percentages)
    emotionality
    
  })})
  
  
  ##Dataframe for Wordbreakdown ####
  
  texterdf3<- reactive({
    
    doc_terms<- ford()
    
    doc_source<-VectorSource(doc_terms)
    
    # Make a volatile corpus: rom_corpus
    text <- VCorpus(doc_source)
    
    ##Function to Clean the Corpus
    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus<- tm_map(corpus,removeNumbers)
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),"the","you","httpstco","for","amp","today","--"))
      return(corpus)
    }
    
    # Apply your customized function to the text_corp: clean_corp
    text_corp<-clean_corpus(text)
    
    
    # Create the dtm from the corpus: text_dtm
    text_dtm<-DocumentTermMatrix(text_corp)
    
    # Convert text_dtm to a matrix: text_m
    text_m<-as.matrix(text_dtm)
    
    ## Calculate the rowSums: term_frequency ##################################################################
    term_frequency<-colSums(text_m)
    
    # Sort term_frequency in descending order
    term_frequency<-sort(term_frequency,decreasing=TRUE)
    
    ##Creates data frame of words ########
    text_freq<-data.frame(term=names(term_frequency),num=term_frequency)
    colnames(text_freq) <- c("Term","Number of Occurences")
    text_freq
    return(text_freq)
    
  })
  
  ##Word Breakdown Table ####  
  observeEvent(input$wbdown,{output$wordbreakdown<-DT::renderDataTable({
    withProgress(message = 'Creating Word Breakdown',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    worddatabreakdown<- as.matrix.data.frame(texterdf3())  
    
    wordatabreakdown <- worddatabreakdown[,1:2]
    wordatabreakdown
    
  })})
  
  ##Download for Sentiment Percentages ####
  
  texterdf4<- reactive({
    
    withProgress(message = 'Downloading Emotional % CSV File',
                 value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    value<- ford()
    
    value <- get_nrc_sentiment(value)
    
    #colSums(as.matrix(value))
    
    prop.table(value[,1:8])
    
    sentimentscores <- round(colSums(prop.table((value[,1:8])))*100,digits = 1)
    
    sentimentscores <- as.data.frame(sentimentscores)
    
    colnames(sentimentscores) <- c("Percentages")
    
    Emotions <- c("anger","anticipation","disgust","fear","joy","sadness",
                  "surprise","trust","negative","positive")
    
    Percentages<- sentimentscores$Percentages
    
    emotionality<- cbind(Emotions,Percentages)
    
  })
  
  output$downloadfour <- downloadHandler(
    filename = function() { paste("Emotional Percentage Breakdown",input$name, sep='',".csv") },
    content = function(file) {
      write.csv(texterdf4(), file)
      
    })
  
  output$downloadfive <- downloadHandler(
    filename = function() { paste("Emotion by Sentence Breakdown",input$name, sep='',".csv") },
    content = function(file) {
      write.csv(texterdf5(), file)
      
    })
  
  barplotdw <- reactive({
    doc_terms<- ford()
    
    # Make a vector source: text_source
    doc_source<-VectorSource(doc_terms)
    
    ## text_source is already in your workspace
    
    # Make a volatile corpus: text_corpus
    doc_corpus <- VCorpus(doc_source)
    ## text_source is already in your workspace
    
    
    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),c(input$words)))
      return(corpus)
    }
    
    doc_corp<-clean_corpus(doc_corpus)
    
    doc_dtm<-DocumentTermMatrix(doc_corp)
    
    # Convert text_dtm to a matrix: text_m
    doc_m<-as.matrix(doc_dtm)
    
    
    # Calculate the rowSums: term_frequency
    doc_frequencyone<-colSums(doc_m)
    
    # Sort term_frequency in descending order
    doc_frequency<-sort(doc_frequencyone,decreasing=TRUE)
    
    # Plot a barchart of the 10 most common words,  
    barplot(doc_frequency[input$numeric:input$numeric2],col=input$color,horiz = input$horz,las=2)
    
  })
  
  ##Barplot download code ######
  output$downloadsix <- downloadHandler(
    filename = function() { paste("Barplot",input$download6,sep = ".") },
    content = function(file) {
      if(input$download6=="png")
        png(file)
      else if (input$download6=="jpeg")
        jpeg(file)
      else if (input$download6=="bmp")
        bmp(file)
      else if (input$download6=="pdf")
        pdf(file)
      withProgress(message = 'Downloading BarPlot',
                   value = 0, {
                     for (i in 1:3) {
                       incProgress(1/3)
                       Sys.sleep(0.25)
                     }
                   },env = parent.frame(n=1))
      
      doc_terms<- ford()
      
      # Make a vector source: text_source
      doc_source<-VectorSource(doc_terms)
      
      ## text_source is already in your workspace
      
      # Make a volatile corpus: text_corpus
      doc_corpus <- VCorpus(doc_source)
      ## text_source is already in your workspace
      
      clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removeWords, c(stopwords("english"),c(input$words)))
        return(corpus)
      }
      
      doc_corp<-clean_corpus(doc_corpus)
      
      doc_dtm<-DocumentTermMatrix(doc_corp)
      
      # Convert text_dtm to a matrix: text_m
      doc_m<-as.matrix(doc_dtm)
      
      # Calculate the rowSums: term_frequency
      doc_frequencyone<-colSums(doc_m)
      
      # Sort term_frequency in descending order
      doc_frequency<-sort(doc_frequencyone,decreasing=TRUE)
      # termstwo<-text_terms()
      
      # [c(input$subset1),c(input$subset2)]
      # Plot a barchart of the 10 most common words,  
      barplot(doc_frequency[input$numeric:input$numeric2],col=input$color,horiz = input$horz,las=2)
      dev.off()
    })
  
  ##Emotional Sentiment Download Code ####
  
  
  ##Emotion ggplot2 reactive download code for barplot###### 
  emotplot1 <- reactive({
    value<- ford()
    
    val_word <- get_tokens(value, pattern = "\\W")
    
    value <- get_nrc_sentiment(value)
    
    
    value <- as.data.frame(sort(colSums((prop.table(value[,1:8])))))
    
    colnames(value) <- "percentages"
    
    ggplot1<- ggplot(value, aes(x=sort(rownames(value),decreasing = FALSE), y=value$percentages)) +
      # plot the bars
      geom_bar(stat="identity", position="dodge",fill=input$colornow) +
      # create the label, "dodged" to fit the bars
      geom_text(aes(label=percent(value$percentages)), vjust=1, colour="white",
                position=position_dodge(.9), size=4)+labs(title="Emotional Sentiment",y = "Percentage",x="Emotion")+
      theme(panel.background = element_blank())
  })
  
  output$downloadseven <- downloadHandler(
    filename = function() { paste("Emotional Sentiment",'png',sep = ".") },
    content = function(file) {
      withProgress(message = 'Downloading BarPlot',
                   value = 0, {
                     for (i in 1:3) {
                       incProgress(1/3)
                       Sys.sleep(0.25)
                     }
                   },env = parent.frame(n=1))
      ggsave(file,emotplot1())})
  
  ##Positive vs Negative ggplot2 download code #######
  emotplot2 <- reactive({
    value<- ford()
    
    val_word <- get_tokens(value, pattern = "\\W")
    
    value <- get_nrc_sentiment(value)
    
    
    value <- as.data.frame(sort(colSums((prop.table(value[,9:10])))))
    
    colnames(value) <- "percentages"
    
    ggplot1<- ggplot(value, aes(x=sort(rownames(value),decreasing = FALSE), y=value$percentages))+
      # plot the bars
      geom_bar(stat="identity", position="dodge",fill=input$colornow2) +
      # create the label, "dodged" to fit the bars
      geom_text(aes(label=percent(value$percentages)), vjust=1, colour="white",
                position=position_dodge(.9), size=4)+labs(title="Positive vs. Negative Sentiment",y = "Percentage",x="Sentiment")+
      theme(panel.background = element_blank())
  })
  
  
  ##Positive vs Negative GGPLOT2 Download Code ######
  output$downloadeight <- downloadHandler(
    filename = function() { paste("Positive vs. Negative Sentiment",'png',sep = ".") },
    content = function(file) {
      # if(input$download6=="png")
      #   png(file)
      # else if (input$download6=="jpeg")
      #   jpeg(file)
      # else if (input$download6=="bmp")
      #   bmp(file)
      # else if (input$download6=="pdf")
      #   pdf(file)
      withProgress(message = 'Downloading BarPlot',
                   value = 0, {
                     for (i in 1:3) {
                       incProgress(1/3)
                       Sys.sleep(0.25)
                     }
                   },env = parent.frame(n=1))
      ggsave(file,emotplot2())})
  
  sentimentplot<- reactive({
    value<- ford()
    
    s_v <- get_sentences(value)
    s_v_sentiment <- get_sentiment(s_v)
    plot(
      s_v_sentiment, 
      type="l", 
      main="Plot Trajectory", 
      xlab = "Narrative Timeline", 
      ylab= "Emotional Valence"
    )
  })
  
  ##Plot Trajectory Download Code #############
  output$downloadnine <- downloadHandler(
    filename = function() { paste("Plot Trajectory",'png',sep = ".") },
    content = function(file) {
      # if(input$download6=="png")
      #   png(file)
      # else if (input$download6=="jpeg")
      #   jpeg(file)
      # else if (input$download6=="bmp")
      #   bmp(file)
      # else if (input$download6=="pdf")
      #   pdf(file)
      withProgress(message = 'Downloading Plot Trajectory',
                   value = 0, {
                     for (i in 1:3) {
                       incProgress(1/3)
                       Sys.sleep(0.25)
                     }
                   },env = parent.frame(n=1))
      
      ggsave(file,sentimentplot())
    })
  
  datasetromeo <- reactive({
    switch(input$datasetten,
           "Romeo and Juliet" = "romeo.txt",
           "Othello" = "othello.txt",
           "Midsummer Nights Dream" = "midsummers.txt")
  })
  
  output$downloadromeo <- downloadHandler(
    filename <- function() {
      paste(input$datasetten, "txt", sep=".")
    },
    
    content <- function(file) {
      file.copy(datasetromeo(), file)
    },
    contentType = "text"
  )
  
  
  
  output$text_report<- downloadHandler(
    filename = function() {
      paste('Text Analysis Report','pdf', sep = '.')
    },
    
    
    
    
    content = function(file) {
      src <- normalizePath('./text.Rmd')
      # src2 <- normalizePath('ChemDiVo Results COA.png') #NEW
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      # owd <- setwd(tempdir())
      # on.exit(setwd(owd))
      # file.copy(src, './report.Rmd', overwrite = TRUE)
      
      textReport <- file.path(tempdir(), "./text.Rmd")
      # tempPictures <- file.path(tempdir(), "./ChemDiVo Results COA.png")
      file.copy("./text.Rmd", textReport, overwrite = TRUE)
      # file.copy("./ChemDiVo Results COA.png",tempPictures,overwrite = TRUE) #NEW
      
      
      library(rmarkdown)
      out <- render(input = 'text.Rmd',output_format = pdf_document()
                    #               switch(
                    # input$format,
                    # PDF = pdf_document(), HTML = html_document(), Word = word_document()
      )
      file.rename(out, file)
      
      #     # Set up parameters to pass to Rmd document
      params <- list(s = texterdf3())
      rmarkdown::render(tempReporters, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
  
  ##Web Scrape Text #######################################################################
  observeEvent(input$do, {
    cat("Getting", input$text, "Data")
  })
  
  df_scrape <- eventReactive(input$do, {
    withProgress(message = 'Running',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    seven<-(input$text)
    value<-read_html(seven) %>%
      html_nodes(input$node) %>%
      html_text()
    
  })
  
  output$printoutput <- renderPrint({
    print(df_scrape())
  })
  
  tabledata<- reactive({
    
    seven<-(input$text)
    value<-read_html(seven) %>%
      html_nodes(input$node) %>%
      html_text()
    # value<-paste(value,collapse = "\\n")
    print(value)
  })
  
  output$download <- downloadHandler(
    filename = function() { paste("Text",input$name, sep='',".txt") },
    content = function(file) {
      write.table(tabledata(), file)
      
    })
  
  ##Lexical Dispersion Plot #############################################################################
  
  
  
  
  observeEvent(input$lexical_run,{output$distPlot <- renderPlot({
    
    # inFile <- input$selection 
    # 
    # df <- readLines(inFile$datapath)
    
    ##Retrieve reactive .txt file
    lexical_terms<- ford()
    ##Scan .txt file for characters
    lexical.text <- scan( what = "characters",text = lexical_terms)
    # # x <- c(input$words)
    # x<- cat(paste(shQuote(input$words, type="cmd"), collapse=", "))
    # print(x)
    # x <- as.vector(x,mode = "any")
    ##Create Lexical dispersin plot using qdap package
    dispersion_plot(lexical.text, input$words,
                    color = "black", bg.color = "grey90", horiz.color = "grey85",
                    total.color = "black", symbol = "|", title = "Lexical Dispersion Plot",
                    rev.factor = TRUE, wrap = "'", xlab = NULL, ylab = "Word Frequencies",
                    size = 3, plot = TRUE)
    
    
  })
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

