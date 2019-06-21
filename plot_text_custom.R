#' Plot text explanations
#'
#' Customised version of the functions available in the LIME package
#' The differences are:
#' - If the predicted label is 0, the wine is bad, and words that support this
#' prediction should be highlighted red. No change if the predicted label is
#' green.
#' - Predicted labels are human-friendly, eg. instead of "predicted label: 1"
#' we display "predicted: good wine".
#' - Explicitly specify packages---since we're using a custom function, we're
#' no longer in LIME's namespace.

plot_text_explanations_custom <- function(explanations, ...) {
    assertthat::assert_that(is.data.frame(explanations))
    assertthat::assert_that(!is.null(explanations$data))
    original_text <- explanations$data
    
    text_highlighted_raw <- lapply(unique(explanations$case), function(id) {
        current_case_df <- explanations[explanations$case == id,]
        original_text <- unique(current_case_df[["data"]])
        predicted_label <- unique(current_case_df[["label"]])
        predicted_label_text <- ifelse(predicted_label == 1, "good wine", "bad wine")
        predicted_label_prob <- unique(current_case_df[["label_prob"]])
        assertthat::assert_that(assertthat::is.string(original_text))
        assertthat::assert_that(assertthat::is.string(predicted_label))
        assertthat::assert_that(assertthat::is.number(predicted_label_prob))
        info_prediction_text <- paste0(
            '<sub>Predicted: ',
            predicted_label_text,
            ' (',
            round(predicted_label_prob * 100, 2),
            '%)<br/>Explainer fit: ',
            format(current_case_df$model_r2[1], digits = 2),
            '</sub>'
        )
        
        current_case_df$weight_percent <- abs(current_case_df$feature_weight) / sum(abs(current_case_df$feature_weight))
        
        if (predicted_label == 1) {
            current_case_df$sign <- ifelse(current_case_df$feature_weight > 0, 1, -1)
        } else {
            current_case_df$sign <- ifelse(current_case_df$feature_weight > 0, -1, 1)            
        }
        current_case_df$code_level <- current_case_df$sign * (1 + current_case_df$weight_percent %/% 0.2)
        current_case_df$color <- sapply(current_case_df$code_level, get_color_code)
        
        paste(
            get_html_span(original_text, current_case_df),
            "</br>",
            info_prediction_text
        )
    })
    
    text_highlighted <- paste(
        '<div style="overflow-y:scroll;font-family:sans-serif;height:100%">',
        paste("<p>", text_highlighted_raw, "</p>", collapse = "<br/>"),
        "</div>"
    )
    
    htmlwidgets::createWidget(
        "plot_text_explanations",
        list(html = text_highlighted),
        sizingPolicy = htmlwidgets::sizingPolicy(
            knitr.figure = FALSE,
            defaultHeight = "auto",
            knitr.defaultWidth = "100%",
            ...
        ),
        package = "lime"
    )
}

#' @importFrom stringi stri_replace_all_regex
get_html_span <- function(text, current_case_df) {
    result <- text
    for (word in current_case_df$feature) {
        color <- as.character(current_case_df[current_case_df$feature == word, "color"])
        text_searched <- paste0("(\\b", word, "\\b)")
        replace_expression <- paste0("<span class='", color, "'>", "$1", "</span>")
        result <- stringi::stri_replace_all_regex(result, text_searched, replace_expression)
    }
    result
}

get_color_code <- function(code_level) {
    switch(as.character(code_level),
           "-6" = "negative_5", # for -100%
           "-5" = "negative_5",
           "-4" = "negative_4",
           "-3" = "negative_3",
           "-2" = "negative_2",
           "-1" = "negative_1",
           "1" = "positive_1",
           "2" = "positive_2",
           "3" = "positive_3",
           "4" = "positive_4",
           "5" = "positive_5",
           "6" = "positive_5") # for 100%
}