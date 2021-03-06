library(shiny)
library(lime)

feature_selection_strategy <- local({
    strategies <- c("auto", "none", "forward_selection", "highest_weights",
                    "lasso_path", "tree")
    strategies_clean <- stringi::stri_replace_all_fixed(strategies, "_", " ")
    names(strategies) <- strategies_clean
    as.list(strategies)
})

ui <- fluidPage(
    title = "Shiny Wine",
    theme = shinythemes::shinytheme("superhero"),
    titlePanel(title = "Is this wine good?"),
    hr(),
    sidebarPanel(
        textAreaInput(
            "text_to_explain", 
            label = NULL, 
            resize = "both", 
            height = "200px"
        ),
        numericInput(
            "number_permutations", 
            label = h5("Quantity of permutations to generate"), 
            value = 5000, 
            step = 1000
            ),
        selectInput(
            "feature_selection_strategy", 
            label = h5("Word selection strategies"), 
            choices = feature_selection_strategy, 
            selected = "auto"
        ),
        sliderInput(
            "number_features_to_explain", 
            label = h5("Number of words to select"), 
            min = 1, 
            max = 20, 
            value = 5, 
            ticks = FALSE
        )
    ),
    
    mainPanel(
        lime::text_explanations_output("text_explanations_plot")
    )
)

server <- function(input, output) {
    library(shiny)
    library(lime)
    
    # Custom text explainer function requires explicitly loading some packages
    # library(assertthat)
    # library(htmlwidgets)
    source("plot_text_custom.R")
    
    load(file = "stem_tokeniser.rda", .GlobalEnv)
    load(file = "vectoriser.rda", .GlobalEnv)
    load(file = "tfidf.rda", .GlobalEnv)
    load(file = "map_to_dtm.rda", .GlobalEnv)
    load(file = "xgb.rda", .GlobalEnv)
    load(file = "wine_classification_explainer.rda", .GlobalEnv)
    
    shared_states <- list()
    
    output$text_explanations_plot <- render_text_explanations({
        validate(
            need(
                stringi::stri_count_words(input$text_to_explain) >= 5, 
                message = "Text provided is too short to be explained (>= 5)."
            )
        )
        
        points <- predict(xgb, map_to_dtm(input$text_to_explain))
        
        shared_states$explanations <<- lime::explain(
            input$text_to_explain, 
            wine_classification_explainer, 
            n_labels = 1, 
            n_features = input$number_features_to_explain, 
            feature_select = input$feature_selection_strategy, 
            n_permutations = input$number_permutations
        )
        
        plot_text_explanations_custom(shared_states$explanations, points)
    })
}

shinyApp(ui, server)