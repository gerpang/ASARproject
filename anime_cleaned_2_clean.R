#to clean anime_cleaned_2 dataset

#drop variables: (1) title_japanese, (2) title_synonyms, (3) image_url, (4) status, (5) background, 
# (6) premiered, (7) related, (8) licensor, (9) producer, (10) opening_theme, (11) ending_theme 

anime$title_japanese <- NULL
anime$title_synonyms <- NULL
anime$image_url <- NULL
anime$status <- NULL
anime$background <- NULL
anime$premiered <- NULL
anime$related <- NULL
anime$licensor <- NULL
anime$producer <- NULL
anime$opening_theme <- NULL
anime$ending_theme <- NULL

#round durations below one minute to one decimal place
duration_round <- round(anime$duration_min,digits=1)
anime$duration_min <- duration_round