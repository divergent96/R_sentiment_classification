function(input, output, session) {
  
  ########################################################################################################
  # General headers and stuff
  
  ##################################
  # Input box header
  
  output$word_cloud_header <- renderText({
    
    temp <- text_processing_picker()$processed_inputs # Assign appropriate vocab
    
    vocab_wc <- temp$vocab_wc
    
    paste("Word Cloud (based on all data available) -",nrow(vocab_wc), "words", sep = " ")
    })
  
  ##################################
  # Wordcloud headers nd stuff
  output$word_cloud_footer <- renderText({
    
    temp <- text_processing_picker()$processed_inputs # Assign appropriate vocab
    
    vocab <- temp$vocab
    
    paste("Words used for training of model -",nrow(vocab), "words", sep = " ")
  })
  
  ##################################
  # 2nd tab Input row headers
  
  # Output for custom prediction
  output$model_pred_header <- renderText({
    paste()
    })
  
  output$model_pred_output <- shinydashboard::renderValueBox({
    temp <- custom_prediction()
    
    cutoff <- input$prediction_proba/100
    
    temp_pred <- ifelse(temp >= cutoff, "Suicidal","Non-Suicidal")
    
    color <- ifelse(temp_pred == "Suicidal", "red","green")
    
    valueBox(value = temp_pred ,"", color = color)
  })

  output$model_pred_proba <- renderText({
    temp <- custom_prediction()
    
    paste("Probablity of text indicating suicide = ",percent(temp), sep = " ")
  })
  
  ##################################
  # 2nd tab Input row headers
  
  
  ########################################################################################################
  # Import the mined reddit dataset
  # Courtesy of Kaggle User Nikhileswar Komati- https://www.kaggle.com/nikhileswarkomati/suicide-watch
  
  
  reddit_data <- reactive({
    
    readr::read_csv("data/Suicide_Detection_reduced.csv")
    
  })
  
  ######################################################################
  # Function to split input into train and test
  
  final_data <- reactive({
    my_data <- reddit_data()
    
    data_size <- nrow(my_data)
    
    
    # Split dataset into train and test groups based on input
    
    train_split <- input$train_split
    
    split_ratio <- train_split/100 # Reading the split ratio input and convert to 
    train_size <- floor(split_ratio*data_size) # Convert ratio to actual number for use
    
    train_indices <- sample(train_size) # Pick random sample based on given ratio
    
    # Split dataset into train and test data
    
    train_data <- my_data[train_indices,]
    test_data <- my_data[-train_indices,]
    
    list(train = train_data, test = test_data)
    
  })
  # Import the stopwords list
  
  ######################################################################
  # Function to output clean data
  
  clean_data <- reactive({
    my_data <- reddit_data()
    df <- final_data()
    
    train_data <- df$train
    test_data <- df$test
    
    
    test_data['tokens'] <- apply(test_data['text'],1,clean_text) # clean text and join back to dataframe
    test_data['tokens_lem'] <- apply(test_data[,'tokens'],1,lemmatize_strings) #Apply lemmitizer to remove tense information
    
    train_data['tokens'] <- apply(train_data['text'],1,clean_text) # clean text and join back to dataframe
    train_data['tokens_lem'] <- apply(train_data[,'tokens'],1,lemmatize_strings) #Apply lemmitizer to remove tense information
    
    list(train_X = train_data[,c('tokens_lem')], test_X = test_data[,c('tokens_lem')], train_y <- train_data[,c('class')], test_y <- test_data[,c('class')])
    
  })
  
  processed_data <- reactive({
    my_data <- readr::read_csv("data/suicide_detection_processed.csv")
    
    data_size <- nrow(my_data)
    
    # Split dataset into train and test groups based on input
    
    train_split <- input$train_split
    
    split_ratio <- train_split/100 # Reading the split ratio input and convert to 
    train_size <- floor(split_ratio*data_size) # Convert ratio to actual number for use
    
    train_indices <- sample(train_size) # Pick random sample based on given ratio
    
    # Split dataset into train and test data
    
    train_data <- my_data[train_indices,]
    test_data <- my_data[-train_indices,]
    
    list(train_data = train_data, test_data = test_data, all_data = my_data)
    
  })
  
  ########################################################################################################
  # Functions for Vectorization
  
  ########################################################################################################
  # Create iterators
  
  iterators <- reactive({
    
    #temp <- clean_data()
    temp <- processed_data() # Bypass preprocessing to speed up execution
    
    train_data <- temp$train_data
    test_data <- temp$test_data
    all_data <- temp$all_data
    suicide_data <- all_data[all_data$class == "suicide",]
    
    it_train <- itoken(as.character(train_data$tokens_lem),ids = seq.int(nrow(train_data)) ,tokenizer = space_tokenizer, n_chunks = 1L, progressbar = FALSE)
    
    it_test <- itoken(as.character(test_data$tokens_lem),ids = seq.int(nrow(test_data)) ,tokenizer = space_tokenizer, n_chunks = 1L, progressbar = FALSE)
    
    it_suicide <- itoken(as.character(suicide_data$tokens_lem),ids = seq.int(nrow(suicide_data)) ,tokenizer = space_tokenizer, n_chunks = 1L, progressbar = FALSE)
    
    list(it_train = it_train, it_test = it_test, it_suicide = it_suicide)
    
  })
  
  ########################################################################################################
  # Count vectorizer - Simple total term frequency matrix
  
  tf_vectors <- reactive({
    temp <- iterators()
    
    it_train <- temp$it_train
    it_test <- temp$it_test
    it_suicide <- temp$it_suicide
    
    # We create vocabulary only based on training set
    
    vocab <- create_vocabulary(it_train)
    vocab_wc <- create_vocabulary(it_suicide)
    
    
    #############################################
    # Create pruned vocublary for use
    
    # Wordcloud vocublary based on all input. Not to be vectorised
    vocab_wc <- vocab_pruner(vocab_in = vocab_wc, 
                             num_words = input$top_words, 
                             term_count_min = input$min_freq, 
                             doc_proportion_min = input$min_doc_prop/100, 
                             doc_proportion_max = input$max_doc_prop/100, 
                             add_stopword = input$manual_stopwords)
    
    # Normal vocab for training
    vocab <- vocab_pruner(vocab_in = vocab, 
                          num_words = input$top_words, 
                          term_count_min = input$min_freq, 
                          doc_proportion_min = input$min_doc_prop/100, 
                          doc_proportion_max = input$max_doc_prop/100, 
                          add_stopword = input$manual_stopwords)
    
    
    ## Vectorise the training dataset vocublar for use
    vectorizer <- vocab_vectorizer(vocab)
    
    dtm_train = create_dtm(it_train, vectorizer)
    dtm_test = create_dtm(it_test, vectorizer)
    
    ################
    # Output
    
    list(train_X = dtm_train, test_X = dtm_test, vocab = vocab, vocab_wc = vocab_wc, vectorizer = vectorizer)
    
  })

  ########################################################################################################
  # Count vectorizer - Simple Document term frequency matrix
  
  dtm_vectors <- reactive({
    temp <- iterators()
    
    it_train <- temp$it_train
    it_test <- temp$it_test
    it_suicide <- temp$it_suicide
    
    # We create vocabulary only based on training set
    
    vocab <- create_vocabulary(it_train)
    vocab_wc <- create_vocabulary(it_suicide)
    
    
    # Wordcloud vocublary based on all input. Not to be vectorised
    vocab_wc <- vocab_pruner(vocab_in = vocab_wc, 
                             num_words = input$top_words, 
                             term_count_min = input$min_freq, 
                             doc_proportion_min = input$min_doc_prop/100, 
                             doc_proportion_max = input$max_doc_prop/100, 
                             add_stopword = input$manual_stopwords)
    
    # Normal vocab for training
    vocab <- vocab_pruner(vocab_in = vocab, 
                          num_words = input$top_words, 
                          term_count_min = input$min_freq, 
                          doc_proportion_min = input$min_doc_prop/100, 
                          doc_proportion_max = input$max_doc_prop/100, 
                          add_stopword = input$manual_stopwords)
    
    
    ## Vectorise the training dataset vocublar for use
    vectorizer <- vocab_vectorizer(vocab)
    
    dtm_train = create_dtm(it_train, vectorizer)
    dtm_test = create_dtm(it_test, vectorizer)
    
    ################
    # Output
    
    list(train_X = dtm_train, test_X = dtm_test, vocab = vocab, vocab_wc = vocab_wc, vectorizer = vectorizer)
    
  })
  
    
  ########################################################################################################
  # 2 word N gram vectorizer - Vectorize data using n-grams occuring across documents
  
  ngram_vectors <- reactive({
    
    temp <- iterators()
    
    it_train <- temp$it_train
    it_test <- temp$it_test
    it_suicide <- temp$it_suicide
    
    # We create vocabulary only based on training set
    
    vocab <- create_vocabulary(it_train, ngram = c(2L,2L))
    vocab_wc <- create_vocabulary(it_suicide, ngram = c(2L,2L))
    
    # Wordcloud vocublary based on all input. Not to be vectorised
    vocab_wc <- vocab_pruner(vocab_in = vocab_wc, 
                             num_words = input$top_words, 
                             term_count_min = input$min_freq, 
                             doc_proportion_min = input$min_doc_prop/100, 
                             doc_proportion_max = input$max_doc_prop/100, 
                             add_stopword = input$manual_stopwords)
    
    # Normal vocab for training
    vocab <- vocab_pruner(vocab_in = vocab, 
                          num_words = input$top_words, 
                          term_count_min = input$min_freq, 
                          doc_proportion_min = input$min_doc_prop/100, 
                          doc_proportion_max = input$max_doc_prop/100, 
                          add_stopword = input$manual_stopwords)
    
    
    ## Vectorise the training dataset vocublar for use
    vectorizer <- vocab_vectorizer(vocab)
    
    ngram_train = create_dtm(it_train, vectorizer)
    ngram_test = create_dtm(it_test, vectorizer)
    
    ################
    
    list(train_X = ngram_train, test_X = ngram_test, vocab = vocab, vocab_wc = vocab_wc, vectorizer = vectorizer)
    
  })
  
  ######################################################################
  # Vocublary helper function
  # Switches vocublary based on user input. Standardized as we use it in various places
  
  text_processing_picker <- reactive({
    text_processor <- input$text_processor
    
    switch(text_processor
           ,"Term Frequency"={processed_inputs <- tf_vectors()}
           
           ,"Document frequency"={processed_inputs <- dtm_vectors()}
           
           ,"N-grams: 2 words"={processed_inputs <- ngram_vectors()}
           )
    list(processed_inputs = processed_inputs)
  })
  
  ######################################################################
  # XG Boost model
  
  xgb_model <- reactive({
    
    set.seed(0723983)# Consisent results when tweaking parameters
    # Raw model input parameters
    
    model_iterations <- input$model_iterations
    model_depth <- input$model_depth
    model_rate <- input$model_rate
    model_gamma <- input$model_gamma
    model_weight <- input$model_weight
    model_subsample <- input$model_subsample
    proba_cutoff <- input$prediction_proba/100
    
    # Import processed inputs from function
    processed_inputs <- text_processing_picker()$processed_inputs
    
    temp <- processed_data()
    train_data <- temp$train_data
    test_data <- temp$test_data
    
    # Independent variables/input word vectors
    train_X <- processed_inputs$train_X
    test_X <- processed_inputs$test_X
    
    # Labels
    train_y <- as.integer(train_data$class == "suicide")
    test_y <- as.integer(test_data$class == "suicide")
    
    model <- xgboost(data = train_X # the input data
                     ,label = train_y # labels
                     ,verbose = 0 # Supress outputs on terminal
                     ,nround = 1 # max number of boosting iterations
                     ,max.depth = model_depth
                     ,eta = model_rate
                     ,gamma = model_gamma
                     ,min_child_weight = model_weight
                     ,subsample = model_subsample
                     ,objective = "binary:logistic"
                     ,eval_metric = "aucpr"
                     
                     )
    
    
    train_proba <- predict(model, train_X)
    test_proba <- predict(model, test_X)
    
    # Convert probablities to 1 or 0 predictions
    train_predictions <- as.numeric(train_proba >= proba_cutoff)
    test_predictions <- as.numeric(test_proba >= proba_cutoff)
    
    # Calculate accuracy for training set
    train_accuracy <- mean(train_predictions == train_y)
    
    
    # Calculate the precision, recall and f1 score
    temp <- model_perf_metrics(actual = test_y, pred = test_predictions)
    
    # Outputs
    list(train_accuracy = train_accuracy,
         accuracy = temp$accuracy,
         precision = temp$precision,
         recall = temp$recall,
         f1_score = temp$f1_score,
         model = model)
    })
  
  
  #################################################
  # Custom input text predictions function
  
  custom_prediction <- reactive({
    
    processed_inputs <- text_processing_picker()$processed_inputs
    # Read in vocabulary and trained vectorizer (based on selection in wordcloud page)
    
    vocab <- processed_inputs$vocab
    vectorizer <- processed_inputs$vectorizer
    
    # Read in trained model
    model <- xgb_model()$model
    
    # Red custom user input 
    input_text <- input$manual_prediction_input
    
    # Clean input and convert to one compatible with model
    custom <- data.frame(text = input_text)
    
    custom['tokens'] <- apply(custom['text'],1,clean_text) # clean text and join back to dataframe
    
    custom['tokens_lem'] <- apply(custom['tokens'],1,lemmatize_strings) # clean text and join back to dataframe
    
    custom <- custom[,c("text","tokens_lem")]
    
    if(custom$tokens_lem == ""){return(0)}
    
    it_custom <- itoken(as.character(custom$tokens_lem),ids = seq.int(length(custom$tokens_lem)) ,tokenizer = space_tokenizer, n_chunks = 1L, progressbar = TRUE)
    
    # Use trained vectorizer on custom iterator for similar input array
    custom_input <- create_dtm(it_custom, vectorizer)
    
    # Predict output
    custom_pred <- predict(model,custom_input)
    
    
  })
  
  ######################################################################
  # Make Wordcloud
  
  output$word_cloud <- renderWordcloud2({
    
    temp <- text_processing_picker()$processed_inputs # Assign appropriate vocab
    
    vocab_wc <- temp$vocab_wc 
    
    data <- data.frame(word = vocab_wc$term, freq = vocab_wc$doc_count) # Standard format dataframe for use with word cloud 2
    
    par(mar = rep(0, 4))
    wordcloud2a(data = data, size = 0.5, color = 'random-dark', shuffle = FALSE) # Using word clud 2 package for plotting
  })

  ######################################################################
  # Second tab - header row boxes
  
  output$validation_accuracy <- shinydashboard::renderValueBox({
    
    temp <- xgb_model() # Assign model outputs
    
    accuracy <- temp$accuracy
    
    valueBox(value = percent(accuracy), tags$p("Validation Accuracy", style = "font-size: 120%;"), icon = icon(name="crosshairs", lib = "font-awesome"), color = "blue")
  })
  
  output$train_accuracy <- shinydashboard::renderValueBox({
    
    temp <- xgb_model() # Assign model outputs
    
    train_accuracy <- temp$train_accuracy
    #paste(25)
    valueBox(value = percent(train_accuracy), tags$p("Training Accuracy", style = "font-size: 120%;"), icon = icon(name="bullseye", lib = "font-awesome"), color = "yellow")
  })

  output$model_f1_score <- shinydashboard::renderValueBox({
    
    temp <- xgb_model() # Assign model outputs
    
    f1_score <- temp$f1_score
    #paste(25)
    valueBox(value = round(f1_score,1), tags$p("F-score (higher is better)", style = "font-size: 120%;"), icon = icon(name="cogs", lib = "font-awesome"), color = "orange")
  })
  
  output$model_precision <- shinydashboard::renderValueBox({
    
    temp <- xgb_model() # Assign model outputs
    
    precision <- temp$precision
    #paste(25)
    valueBox(value = round(precision,1), tags$p("Precision", style = "font-size: 120%;"), icon = icon(name="check", lib = "font-awesome"), color = "yellow")
  })
  
  output$model_recall <- shinydashboard::renderValueBox({
    
    temp <- xgb_model() # Assign model outputs
    
    recall <- temp$recall
    #paste(25)
    valueBox(value = round(recall,1), tags$p("Recall", style = "font-size: 120%;"), icon = icon(name="check", lib = "font-awesome"), color = "light-blue")
  })
  
  ######################################################################
  # test output function to test various functionality and errors
  output$testoutput <- renderText({
    temp <- xgb_model() # Assign model outputs
    
    accuracy <- temp$accuracy
    paste(accuracy, sep = " ")
  })

  
  }
