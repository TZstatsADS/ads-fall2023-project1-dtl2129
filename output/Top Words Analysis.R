library(tidyverse)
library(ggplot2)
library(maps)
library(countrycode)
library(tm)
library(topicmodels)
library(flashClust)
library(tidytext)
library(shiny)
library(wordcloud2)
library(DT)

hdb <- read.csv("../output/processed_moments_w_demog.csv")

dtm <- read_rds("../output/dtm.rds")

tfidf <- read_rds("../output/tfidf.rds")

analyze_country <- function(country) {
  # Extract the ISO3C code entered by the user
  
  if(country %in% countryname_dict$country.name.en){
    iso3c <- countrycode(country, "country.name", "iso3c")
  }
  
  else if(country %in% codelist$iso3c){
    iso3c <- country
  }
  
  else {
    print("Invalid Country Name")
  }
  
  # Subset hdb
  
  ctry_df <- hdb[hdb$country == iso3c,]
  
  ctry_df <- ctry_df[complete.cases(ctry_df[,c("wid")]),] 
  
  # Subset DTM
  
  sub_dtm <- dtm[as.integer(row.names(ctry_df)),]
  
  word_freq <- sub_dtm %>% as.matrix() %>% colSums()
  
  top_words <- head(sort(word_freq, decreasing = T), 10)
  
  top_words_df <- data.frame(word = names(top_words), 
                             freq = top_words)
  
  freq_plot <- ggplot(top_words_df, aes(reorder(word, freq), freq)) +
    geom_bar(stat="identity", fill = "lightblue") + 
    coord_flip() + 
    labs(title = paste0("Ten Most Common Happy Words in ", 
                        countrycode(iso3c, "iso3c", "country.name")),
         y = "Frequency", 
         x = "Word")
  
  # Tf-Idf Subset
  
  # Subset the data for the selected country
  
  country_indices <- which(hdb$country == iso3c)
  
  tfidf_country <- tfidf[country_indices, ]
  
  # Subset the data for all other countries (excluding the selected country)
  
  other_indices <- which(hdb$country != iso3c)
  
  tfidf_others <- tfidf[other_indices, ]
  
  # Calculate distinctiveness score
  
  avg_tfidf_country <- colMeans(as.matrix(tfidf_country))
  
  avg_tfidf_others <- apply(tfidf_others, 2, mean)
  
  distinctiveness_score <- avg_tfidf_country - avg_tfidf_others
  
  top_distinctive_words <- head(sort(distinctiveness_score, decreasing = TRUE),
                                10)
  
  top_distinctive_words_df <- data.frame(word = names(top_distinctive_words), 
                                         dist = top_distinctive_words)
  
  distinctive_plot <- ggplot(top_distinctive_words_df, 
                             aes(reorder(word, dist), dist)) +
    geom_bar(stat="identity", fill = "lightblue") + 
    coord_flip() + 
    labs(title = paste0("Ten Most Distinctive Happy Words in ", 
                        countrycode(iso3c, "iso3c", "country.name")),
         y = "Distinctiveness Score", 
         x = "Word")
  
  return(list(freq_plot, 
              distinctive_plot))
  
}

# Create a list to store results for each country
results_list <- list()

#Loop through all countries
for (country in unique(hdb$country)) {
  # Calculate results for each country
  result <- analyze_country(country)
  results_list[[country]] <- result
}

# Save the results to a CSV file
saveRDS(results_list, file = "precomputed_results.rds")
