clean_text <- function(input_str){
  t <- as.character(input_str) #Convert possible factor strings to characters
  
  t <- tolower(t) # Lowercase everything
  
  t <- gsub("[^A-Za-z']", " ", t) # Remove anythng that is non alphabetical
  
  # Replace general contracted texts to full forms
  t <- gsub("won\'t", "will not", t)
  t <- gsub("can\'t", "can not", t)
  t <- gsub("wont", "will not", t)
  t <- gsub("cant", "can not", t)
  t <- gsub("dont", "do not", t)
  
  # Replace general contractions to full forms 
  
  t <- gsub("n\'t", " not", t)
  t <- gsub("\'re", " are", t)
  t <- gsub("\'s", " is", t)
  t <- gsub("\'d", " would", t)
  t <- gsub("\'ll", " will", t)
  t <- gsub("\'t", " not", t)
  t <- gsub("\'ve", " have", t)
  t <- gsub("\'m", " am", t)
  
  # General text cleaning
  
  t <- gsub("\\b\\w{1,2}\\b",' ',t) # Remove words <=3 letters in length
  
  t <- gsub("[']", "", t) # remove random apostrophes present as they may clutter vocabulary later 
  
  t <- gsub("\\s+"," ",t) # remove successive white spaces and contract them to single white space
  
  t <- unlist(strsplit(t, " " )) # Split individual words for filtering
  
  t <- t[!(t %in% stopwords())] # Remove stopwords
  
  t <- str_replace_all(t,"\n\"", "") # Remove a peculiar escape character noticed while building vocabulary
  
  t <- t[t %in% GradyAugmented] # Retain only valid words present across a dictionary, simplifying computation
  
  t <- t[!(t %in% c(""))] # Remove nulls from strings to avoid unnecessary characters in vocabulary later
  
  t <- paste(t,collapse = " ") # Convert back to a sentence as we will use a standardized tokenizer on this text
  
}

manual_stopword_dict <- function(input){
  t <- gsub(" ", "", input) # remove random spaces present as they may clutter vocabulary later 
  t <- unlist(strsplit(t, "," ))
}

vocab_pruner <- function(vocab_in, num_words, term_count_min, doc_proportion_min, doc_proportion_max, add_stopword){
  
  add_stopword_list <- manual_stopword_dict(add_stopword)
  
  vocab_in <- vocab_in[!(vocab_in$term %in% add_stopword_list),]
  
  vocab_in['term_len'] <- lapply(vocab_in['term'], nchar)
  
  vocab_in <- vocab_in[vocab_in$term_len >=3,] # remove terms less than 3 characters in size. Very few <3 character terms convey any useful information
  
  # The variable portion based on inputs provided
  
  vocab_in <- vocab_in[order(vocab_in$term_count, decreasing = TRUE),] # Sort in descending based on document count
  
  vocab_in <- vocab_in[1:num_words+1,]# Pick top n words as per input
  
  vocab_in <- prune_vocabulary(vocab_in,
                               term_count_min = term_count_min, 
                               doc_proportion_min = doc_proportion_min, 
                               doc_proportion_max = doc_proportion_max)
  
  return(vocab_in)
}

convert_to_class <- function(input, cutoff){
  temp <- input>=cutoff
  
  lapply(temp, function(x){ifelse(x == 0,"Non-Suicide", "Suicide")})
}