
########################################################################################################
# Import the mined reddit dataset
# Courtesy of Kaggle User Nikhileswar Komati- https://www.kaggle.com/nikhileswarkomati/suicide-watch


reddit_data <- reactive({
  
  reddit_data <- readr::read_csv("data/Suicide_Detection_reduced.csv")
  
  reddit_data  
})
