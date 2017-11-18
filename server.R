#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
# library(googlesheets)
library(dplyr)
library(tidytext)
library(tm)
library(tidyr)
library(ggplot2)

# Data <- gs_read_csv(gs_title("Data_Small"))
# Data <- gs_read_csv(gs_title("Data_Med"))
# Data <- readRDS("./data/Data_Med.rds")
Data <- readRDS("./data/Data_MedPlus.rds")
# Data <- readRDS("./data/DataAll.rds")

NextWord <- function(Input = c()) {
    # Required packages
    # suppressMessages(require(dplyr))
    # suppressMessages(require(tidytext))
    # suppressMessages(require(tm))
    # suppressMessages(require(tidyr))
    
    # Cleansing input
    Input <- tolower(Input)
    Input <- removePunctuation(Input)
    Input <- removeNumbers(Input)
    # Input <- removeWords(Input, stopwords("english"))
    Input <- gsub(pattern = "ð|ÿ|â|î|ï|ÿ", replacement = "",  Input)
    Banned <- readLines(con = "http://www.bannedwordlist.com/lists/swearWords.txt", warn = FALSE)
    Input <- gsub(pattern = paste(Banned, collapse = "|"), replacement = "", Input)
    rm(Banned)
    Input <- trimws(Input, "both")
    Input <- stripWhitespace(Input)
    Input <- data_frame(text = Input) %>%
        unnest_tokens(word, text)
    Number <- nrow(Input)
    
    if (# Data DOES NOT contain any lines with the last inut word, or input contains no words
        Number == 0 |
        sum(grepl(pattern = paste0("\\b", Input$word[Number], "\\b"), Data$text)) == 0) {
        
        # Return most common 1-gram from Data
        Output <- Data %>%
            unnest_tokens(word1, text) %>%
            count(word1, sort = TRUE) %>%
            filter(!word1 %in% stopwords("en")) %>%
            head(5) #%>%
            #select(word1) #%>%
        #as.character()
        
    } else {
        # Subset Data to lines that contain last input word: Data_sub1
        Data_sub1 <- Data %>%
            filter(grepl(pattern = paste0("\\b", Input$word[Number], "\\b"), text))
        
        if (# Data_sub1 DOES NOT contain any lines with second-to-last input word, or input contains one word
            Number == 1 |
            sum(grepl(pattern = paste0("\\b", Input$word[Number - 1], "\\b"), Data_sub1$text)) == 0) {
            
            # Return second word of most common 2-gram from Data_sub1, beginning with last input word
            Output <- Data_sub1 %>%
                mutate(text = sub(pattern = paste0(".*?\\b", Input$word[Number], "\\b"), 
                                  replacement = Input$word[Number], text)) %>%
                unnest_tokens(gram2, text, token = "ngrams", n = 2) %>%
                count(gram2, sort = TRUE) %>%
                separate(gram2, c("word2", "word1"), sep = " ") %>%
                filter(word2 == Input$word[Number]) %>%
                filter(!word1 %in% stopwords("en")) %>%
                head(5) #%>%
                #select(word1) #%>%
            #as.character()
            
            # Even though Data_sub1 has the last input word in all its lines, it's not guaranteed
            # to have any relevant 2-grams beginning with those words.  In these cases, we simply 
            # return the most common word in the corpus.
            # if (Output == "character(0)") {
            if (nrow(Output) == 0) {
                Output <- Data %>%
                    unnest_tokens(word1, text) %>%
                    count(word1, sort = TRUE) %>%
                    filter(!word1 %in% stopwords("en")) %>%
                    head(5) #%>%
                    #select(word1) #%>%
                #as.character()
            }
            
        } else {
            # Subset Data_sub1 to lines that contain second-to-last input word: Data_sub2
            Data_sub2 <- Data_sub1 %>%
                filter(grepl(pattern = paste0("\\b", Input$word[Number - 1], "\\b"), text))
            
            if (# Data_sub2 DOES NOT contain any lines with third-to-last input word, or input contains 2 words
                Number == 2 |
                sum(grepl(pattern = paste0("\\b", Input$word[Number - 2], "\\b"), Data_sub2$text)) == 0) {
                
                # Return third word of most common 3-gram from Data_sub2, beginning with second-to-last and last input word
                Output <- Data_sub2 %>%
                    filter(grepl(pattern = paste(Input$word[Number - 1], Input$word[Number]), text)) %>%
                    mutate(text = sub(pattern = paste0(".*?\\b", Input$word[Number - 1], " ", Input$word[Number], "\\b"), 
                                      replacement = paste(Input$word[Number - 1], Input$word[Number]), text)) %>%
                    rbind(data_frame(line = as.integer(NA), text = as.character(NA))) %>%
                    unnest_tokens(gram3, text, token = "ngrams", n = 3) %>%
                    count(gram3, sort = TRUE) %>%
                    separate(gram3, c("word3", "word2", "word1"), sep = " ") %>%
                    filter(word3 == Input$word[Number - 1]) %>%
                    filter(word2 == Input$word[Number]) %>%
                    filter(!word1 %in% stopwords("en")) %>%
                    head(5) #%>%
                    #select(word1) #%>%
                #as.character()
                
                # Even though Data_sub2 has second-to-last and last input words in all its lines, it's not guaranteed
                # to have any relevant 3-grams beginning with those words.  In these cases, we look for 2-grams
                # starting with the last input word, which hasn't been done yet for this input set.
                # if (Output == "character(0)") {
                if (nrow(Output) == 0) {
                    Output <- Data_sub1 %>%
                        mutate(text = sub(pattern = paste0(".*?\\b", Input$word[Number], "\\b"), 
                                          replacement = Input$word[Number], text)) %>%
                        unnest_tokens(gram2, text, token = "ngrams", n = 2) %>%
                        count(gram2, sort = TRUE) %>%
                        separate(gram2, c("word2", "word1"), sep = " ") %>%
                        filter(word2 == Input$word[Number]) %>%
                        filter(!word1 %in% stopwords("en")) %>%
                        head(5) #%>%
                        #select(word1) #%>%
                    #as.character()
                }
                
                # Similarly, there may be no 2-grams starting with the last input word, so we simply return the most
                # common word in the corpus.
                # if (Output == "character(0)") {
                if (nrow(Output) == 0) {
                    Output <- Data %>%
                        unnest_tokens(word1, text) %>%
                        count(word1, sort = TRUE) %>%
                        filter(!word1 %in% stopwords("en")) %>%
                        head(5) #%>%
                        #select(word1) #%>%
                    #as.character()
                }
                
            } else {
                # Subset Data_sub2 to lines that contain third-to-last input word: Data_sub3
                Data_sub3 <- Data_sub2 %>%
                    filter(grepl(pattern = paste0("\\b", Input$word[Number - 2], "\\b"), text))
                
                # Return fourth word of most common 4-gram from Data_sub3, beginning with third-to-last, 
                # second-to-last, and last input word.
                Output <- Data_sub3 %>%
                    filter(grepl(pattern = paste(Input$word[Number - 2], Input$word[Number - 1], Input$word[Number]), text)) %>%
                    mutate(text = sub(pattern = paste0(".*?\\b", Input$word[Number - 2], " ", Input$word[Number - 1], " ", Input$word[Number], "\\b"), 
                                      replacement = paste(Input$word[Number - 2], Input$word[Number - 1], Input$word[Number]), text)) %>%
                    rbind(data_frame(line = as.integer(NA), text = as.character(NA))) %>%
                    unnest_tokens(gram4, text, token = "ngrams", n = 4) %>%
                    count(gram4, sort = TRUE) %>%
                    separate(gram4, c("word4", "word3", "word2", "word1"), sep = " ") %>%
                    filter(word4 == Input$word[Number - 2]) %>%
                    filter(word3 == Input$word[Number - 1]) %>%
                    filter(word2 == Input$word[Number]) %>%
                    filter(!word1 %in% stopwords("en")) %>%
                    head(5) #%>%
                    #select(word1) #%>%
                #as.character()
                
                # Even though Data_sub3 has the third-to-last, second-to-last, and last input words in all its lines,
                # it's not guaranteed to have any relevant 4-grams beginning with those words.  In these cases, we
                # look for 3-grams starting with the second-to-last and last input words, which hasn't been done yet
                # for this input set.
                # if (Output == "character(0)") {
                if (nrow(Output) == 0) {
                    Output <- Data_sub2 %>%
                        filter(grepl(pattern = paste(Input$word[Number - 1], Input$word[Number]), text)) %>%
                        mutate(text = sub(pattern = paste0(".*?\\b", Input$word[Number - 1], " ", Input$word[Number], "\\b"), 
                                          replacement = paste(Input$word[Number - 1], Input$word[Number]), text)) %>%
                        rbind(data_frame(line = as.integer(NA), text = as.character(NA))) %>%
                        unnest_tokens(gram3, text, token = "ngrams", n = 3) %>%
                        count(gram3, sort = TRUE) %>%
                        separate(gram3, c("word3", "word2", "word1"), sep = " ") %>%
                        filter(word3 == Input$word[Number - 1]) %>%
                        filter(word2 == Input$word[Number ]) %>%
                        filter(!word1 %in% stopwords("en")) %>%
                        head(5) #%>%
                        #select(word1) #%>%
                    #as.character()
                }
                
                # Similarly, there may be no 3-grams starting with the second-to-last and last input words, so we look
                # for 2-grams starting with the last input word, which hasn't been done yet for this input set.
                # if (Output == "character(0)") {
                if (nrow(Output) == 0) {
                    Output <- Data_sub1 %>%
                        mutate(text = sub(pattern = paste0(".*?\\b", Input$word[Number], "\\b"), 
                                          replacement = Input$word[Number], text)) %>%
                        unnest_tokens(gram2, text, token = "ngrams", n = 2) %>%
                        count(gram2, sort = TRUE) %>%
                        separate(gram2, c("word2", "word1"), sep = " ") %>%
                        filter(word2 == Input$word[Number]) %>%
                        filter(!word1 %in% stopwords("en")) %>%
                        head(5) #%>%
                        #select(word1) #%>%
                    #as.character()
                }
                
                # Similarly, there may be no 2-grams starting with the last input word, so we simply return the most
                # common word in the corpus.
                # if (Output == "character(0)") {
                if (nrow(Output) == 0) {
                    Output <- Data %>%
                        unnest_tokens(word1, text) %>%
                        count(word1, sort = TRUE) %>%
                        filter(!word1 %in% stopwords("en")) %>%
                        head(5) #%>%
                        #select(word1) #%>%
                    #as.character()
                }
            }
        }
    }
    
    print(Output)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # # Defines function to read in data from Google Sheets
    # LoadGoogleData <- function() {
    #     Sheet <- gs_title(Table)
    #     gs_read_csv(Sheet)
    # }
    
  #   output$distPlot <- renderPlot({
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2] 
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # })
    
    # output$Head <- renderText({unlist(Data[1, 2])})

    Output <- eventReactive(input$button, {
        NextWord(input$Input)
        })
    
    # output$Word1 <- renderText({paste("Likeliest Next Word:", unlist(Output())[1])})
    # output$Word2 <- renderText({paste("Second Likeliest Next Word:", unlist(Output())[2])})
    # output$Word3 <- renderText({paste("Third Likeliest Next Word:", unlist(Output())[3])})
    
    output$Word1 <- renderText({
        # paste("Likeliest Next Word:", Output()[1, ncol(Output()) - 1])
        as.character(Output()[1, ncol(Output()) - 1])
        })
    
    # output$Table <- renderTable({Output()})
    
    # output$distPlotBase <- renderPlot({
    #     # barplot(height = Output()$n, names.arg = Output()[, ncol(Output()) - 1])
    #     barplot(height = rev(Output()$n),
    #             names.arg = rev(Output()$word1),
    #             xaxt = "n",
    #             col = "royalblue1",
    #             border = NA,
    #             main = "Relative Likelihood of Possible Next Words",
    #             horiz = TRUE,
    #             #cex.names = 1.5,
    #             las = 2)
    # })
    
    output$distPlotGG <- renderPlot({
        ggplot(data = Output(), aes(reorder(word1, n), n, fill = reorder(word1, n))) +
            geom_bar(stat = "identity") +
            ggtitle("Relative Likelihood of Possible Next Words") +
            theme_minimal() +
            theme(plot.title = element_text(size = 20, face = "italic"),
                  legend.position = "none",
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.title = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size = 15)) +
            scale_fill_manual(values = c(rep("grey", nrow(Output()) - 1), "royalblue1")) +
            coord_flip()
    })
})
