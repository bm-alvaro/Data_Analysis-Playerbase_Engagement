# This script and analysis is not finished and is not working properly. Further work is needed. 

library(pdftools)
library(stringr)
library(dplyr)
library(tidyverse)
library(stringi)
library(rlang)

text_df <- pdf_text("raw_data/GemBet Survey - Euro 2024 predictor.pdf") %>%
  str_split("\n") %>%
  unlist() %>%  
  .[nzchar(.)] %>% 
  .[!grepl("GemBet Survey - Euro 2024 predictor|8/6/2024, 3:06 PM|Average rating", .)] %>% 
  .[-c(1, 294)] %>%         
  trimws() %>%                
  gsub("\\s+", " ", .)  

text_df <- as.data.frame(text_df, stringsAsFactors = FALSE)
colnames(text_df) <- "text"

answered_indices <- which(grepl("of 175 answered", text_df$text))

questions <- sapply(answered_indices, function(i) {
  if (i > 1) {
    return(text_df$text[i - 1])  # The question is the line before the "of 175 answered"
  } else {
    return(NA)  # Handle the case where there is no previous line
  }
})

questions <- trimws(questions)

questions_df <- as.data.frame(questions, stringsAsFactors = FALSE)
colnames(questions_df) <- "Questions"

answers_list <- list()

for (i in seq_along(answered_indices)) {
  index <- answered_indices[i]
  
  # Define start and end indices for answers
  start_index <- index + 1
  end_index <- index - 2
  
  # Ensure indices are within bounds
  if (end_index > 0 && start_index <= nrow(text_df)) {
    # Extract the text for the answers
    answers <- text_df$text[start_index:end_index]
    # Combine answers into a single string for each question
    answers_combined <- paste(answers, collapse = " ")
    answers_list[[i]] <- answers_combined
  }
}

answers_df <- as.data.frame(answers_list, stringsAsFactors = FALSE)
answers_df <- do.call(rbind, lapply(answers_list, function(x) data.frame(Answer = paste(x, collapse = " "), stringsAsFactors = FALSE)))
rownames(answers_df) <- NULL

