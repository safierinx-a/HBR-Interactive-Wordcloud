library(shiny)
library(wordcloud2)
wc2ClickedWord = function(cloudOutputId, inputId) {
    #ARGUMENTS
    #       - cloudOutputId: string; outputId of wordcloud2 obj being rendered (should be identical to the value passed to wordcloud2Output)
    #       -       inputId: string; inputId of word clicked on (ie you will reference in server the word by input$inputId)
    #OUPUT
    #       - referencing input in server will return a string of form word:freq (same as hover info shown in wordcloud; ie 'super:32')
    
    tags$script(HTML(
        sprintf("$(document).on('click', '#%s', function() {", cloudOutputId),
        'word = document.getElementById("wcSpan").innerHTML;',
        sprintf("Shiny.onInputChange('%s', word);", inputId),
        "});"
    ))
}

full <- read.csv("F:/HBR data.csv")
full$word<-as.character(full$word)
full$text<-as.character(full$text)
df <- subset(full, select = -c(X, text) )
shinyApp(
    ui=shinyUI(fluidPage(
        wc2ClickedWord("my_wc", "selected_word"),
        # Application title
        titlePanel("Harvard Business Review: WordCloud"),
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
            sidebarPanel(
                textOutput("print"),
                tags$style(type="text/css", "#print {white-space: pre-wrap;}")
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                wordcloud2Output("my_wc")
            )
        )
    )),
    server=shinyServer(function(input,output,session){
        output$my_wc =  renderWordcloud2({
            
            wordcloud2(df, size = 0.5, minSize = 0, gridSize =  0,fontFamily ='Segoe UI', 
                       fontWeight ='bold',color ='random-light', backgroundColor = "black",
                       minRotation = -pi/2, maxRotation = pi/2, shuffle = TRUE,rotateRatio = 0.4,
                       shape ='circle', ellipticity = 0.65,widgetsize = NULL, figPath = NULL,
                       hoverFunction = NULL) 
        })
        
        output$print = renderPrint(strsplit(full[full$word==gsub("\\:.*","", as.character(input$selected_word)),]$text, "\n"))
        
        
    })
)
