####--UI--------------------------------------------------------------------------------------------
navbarPage(
  # Give the page a title
  title = "Suicidal tendency detection"
  
  ,tags$style(type = 'text/css',
              '.navbar { background-color: #6FB2D2;}',
              '.navbar-default .navbar-brand{color: white;font}',
              '.tab-panel{ background-color: red; color: white}'           
              )
             
  ,tabPanel(
    useShinydashboard()
    ,title = "Word Cloud"
    # Generate a row with a sidebar
    ,fluidRow(
      # Define the sidebar with one input
      column(width = 2,align = 'left',style = 'border-right: 1px solid; border-color: grey'
             ,h2("Parameters for corpus and word cloud:")
             
             ,selectInput(inputId = "text_processor",label = "Text Processing method:"
                          ,choices=text_processor_list)
             
             ,sliderInput(inputId = "top_words",label = "# of Top terms to retain:"
                          ,min = 2,  max = 2000, value = 1000)
             
             ,sliderInput(inputId = "min_freq",label = "Min. term occurences:"
                          ,min = 1,  max = 1000, value = 50)
             
             ,sliderInput(inputId = "min_doc_prop",label = "Min. proportion of documents containing a term:"
                          ,min = 1,  max = 100, value = 1)
             
             ,sliderInput(inputId = "max_doc_prop",label = "Max. proportion of documents containing a term:"
                          ,min = 1,  max = 100, value = 100)
             
             ,textInput(inputId = "manual_stopwords",label = "Input additional words to remove manually from corpus:"
                        ,value = "not,just,just_live"
                        ,placeholder = "Insert words seperated by commas. e.g.: why,can,just")
             
             ,hr()
             ,helpText("Data obtained from Suicide Watch subreddit on Reddit.com")
             )
      # Create a spot for the barplot
      ,column(width = 10, align = 'center'
              ,fluidRow(h2(textOutput(outputId = "word_cloud_header")))
              ,fluidRow(h6("Will not display words if only 1 word is present in corpus"))
              
              ,fluidRow(wordcloud2Output(outputId = "word_cloud", width = "1200px", height = "600px"))
              
              ,fluidRow(h3(textOutput(outputId = "word_cloud_footer")))
              ,fluidRow(h6("The number of words is lower than wordcloud as it is restricted to training set (80% of data)"))
              )
      )
    )

  ,tabPanel(
    useShinydashboard()
    ,title = "XG Boost model training and Prediction"
      # Generate a row with a sidebar
      ,fluidRow(
        column(width = 2, style = 'border-right: 1px solid; border-color: grey'
               ,h2("Model tuning for manual predictions:")
               
               # Define the sidebar with one input
               
               ,sliderInput(inputId = "train_split",label = "Training dataset split"
                            ,min = 1,  max = 100, value = 80)
               
               ,sliderInput("prediction_proba", "Probablity cutoff for prediction",
                            min = 1, max = 100, value = 50, step = 1)
               
               ,sliderInput("model_iterations", "Boosting iterations",
                            min = 1, max = 100, value = 10, step = 1)

               ,sliderInput("model_depth", "Maximum tree depth",
                            min = 1, max = 20, value = 2, step = 1)

               ,sliderInput("model_rate", "Learning rate (eta)",
                            min = 0.1, max = 0.5, value = 0.5, step = 0.05)

               ,sliderInput("model_gamma", "Minimum loss reduction for split (gamma)",
                            min = 0, max = 1, value = 0, step = 0.01)

               ,sliderInput("model_weight", "Minimum child weight",
                            min = 1, max = 10, value = 1, step = 1)

               ,sliderInput("model_subsample", "Subsample ratio of rows",
                            min = 0.1, max = 1, value = 0.8, step = 0.1)

               ,hr()

               ,helpText("Data obtained from Suicide Watch subreddit on Reddit.com")
               
               )
        # Create a spot for the barplot
        ,column(width = 10, align = 'center'
                ,fluidRow(style = 'padding:5px', h1("Trained model performance metrics"))
                ,fluidRow(
                  column(width = 2, style='padding:0px;', shinydashboard::valueBoxOutput(outputId = "validation_accuracy", width = 12))
                  ,column(width = 2, style='padding:0px;', shinydashboard::valueBoxOutput("train_accuracy", width = 12))
                  ,column(width = 4, style='padding:0px;', shinydashboard::valueBoxOutput("model_f1_score", width = 12))
                  ,column(width = 2, style='padding:0px;', shinydashboard::valueBoxOutput("model_precision", width = 12))
                  ,column(width = 2, style='padding:0px;', shinydashboard::valueBoxOutput("model_recall", width = 12))
                  ,h4("Try tweaking prediction probablity to fine tune the model's confidence in results")
                  ,h4("Higher validation accuracy and f1-score will ideally represent the best model")
                  )
                
                ,hr()
                ,fluidRow(style = 'padding:5px', h1("Predicting behaviour based on given text."))
                
                ,fluidRow(h3("Try inputting a sentence or paragraph below to evaluate it with your model!"))
                ,fluidRow(textAreaInput(inputId = "manual_prediction_input",label = "",value = "I want to die. I lost will. Death, kill."))
                ,hr()
                ,fluidRow(style = 'padding-bottom:10px', h1("Model thinks your text indicates below tendency:"))
                ,fluidRow(column(width = 4,offset = 4,align = 'center',shinydashboard::valueBoxOutput(outputId = "model_pred_output", width = 12)))
                ,fluidRow(h3(textOutput(outputId = "model_pred_proba")))
                )
        )
    )
  )
  
