shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    m <- readRDS(file = rdsfile)  
    models[[name]] <- m
    
    # try to update the preprocessing steps with the ones that were used
    inpId <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), "Preprocess")
    steps <- m$recipe$steps
    seld <- c()
    for (step in steps) {
      s <- gsub(pattern = "step_", replacement = "", x = class(step)[1])
      seld <- c(seld, s)
    }
    if (length(seld) > 0 && seld[1] == "date") { 
      seld <- seld[2:length(seld)] #discard initial date step 
      seld <- seld[seld != "rm"] #discard rm step
    }
    updateSelectizeInput(session = session, inputId = inpId, choices = ppchoices, selected = seld)
    if (length(seld) > 0) {
      showNotification(paste("Setting preprocessing for", name, "to", paste(seld, collapse = ",")), session = session, duration = 3)
    }
  }
  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Ass3Data.csv", row.names = "ID", stringsAsFactors = TRUE)
    d$TreatmentDate <- as.Date(d$TreatmentDate)
    d
  })
  
  # output DfSummary ----
  output$DfSummary <- renderUI({
    view(dfSummary(getData()), headings = FALSE, method = 'render', footnote = NA)
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()[input$VariablesA]
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  #GGpair plot ----
  output$Pairs <- renderPlot({
    GGally::ggpairs(data = getData()[input$VariablesB], mapping = aes(colour = getData()$BloodType), 
                    title = 'Paire plots of Assignment3 data', progress = FALSE) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE)
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode="list", length = n+1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 15, min = 1000, max = 5000)))
    }
    seeds[[n+1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  # observeEvent NullGo ----
  observeEvent(
    input$NullGo,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # observeEvent NullGo ----
  output$NullMetrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output NullRecipe
  output$NullRecipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------

  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$GlmnetPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent GlmnetGo ----
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output GlmnetModelSummary (text) ----
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })
  
  # output GlmnetMetrics (table) ----
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output GlmnetModelPlots (plot) ----
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  # output GlmnetRecipe (print) ----
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output GlmnetModelSummary2 (print) ----
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })


  
  
  

  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------

  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$PlsPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent PlsGo ----
  observeEvent(
    input$PlsGo,
    {
      library(pls)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output PlsModelSummary0 (text) ----
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  # output PlsMetrics (table) ----
  output$PlsMetrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output PlsModelPlots (plot) ----
  output$PlsModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output PlsRecipe (print) ----
  output$PlsRecipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output PlsModelSummary2 (print) ----
  output$PlsModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  

  
  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------

  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$RpartPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent RpartGo ----
  observeEvent(
    input$RpartGo,
    {
      library(rpart)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output RpartModelSummary0 (print) ----
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  # output RpartMetrics (table) ----
  output$RpartMetrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output RpartRecipe (print) ----
  output$RpartRecipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output RpartModelPlots (plot) ----
  output$RpartModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output RpartModelTree (plot) ----
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  

  
  
  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # add further methods here 
  
  
  # METHOD * bayesglm ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getbayesglmRecipe ----
  getbayesglmRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$bayesglmPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent bayesglmGo ----
  observeEvent(
    input$bayesglmGo,
    {
      library(arm)
      method <- "bayesglm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getbayesglmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output bayesglmModelSummary0 (text) ----
  output$bayesglmModelSummary0 <- renderText({
    description("bayesglm")
  })
  
  # output bayesglmMetrics (table) ----
  output$bayesglmMetrics <- renderTable({
    req(models$bayesglm)
    models$bayesglm$results[ which.min(models$bayesglm$results[, "RMSE"]), ]
  })
  
  # output bayesglmModelPlots (plot) ----
  output$bayesglmModelPlots <- renderPlot({
    req(models$bayesglm)
    plot(models$bayesglm)
  })     
  
  # output bayesglmRecipe (print) ----
  output$bayesglmRecipe <- renderPrint({
    req(models$bayesglm)
    models$bayesglm$recipe
  })  
  
  # output bayesglmModelSummary2 (print) ----
  output$bayesglmModelSummary2 <- renderPrint({
    req(models$bayesglm)
    summary(models$bayesglm$finalModel)
  })
  
  
  # METHOD * glmStepAIC ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getglmStepAICRecipe ----
  getglmStepAICRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$glmStepAICPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent glmStepAICGo ----
  observeEvent(
    input$glmStepAICGo,
    {
      library(MASS)
      method <- "glmStepAIC"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getglmStepAICRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output glmStepAICModelSummary0 (text) ----
  output$glmStepAICModelSummary0 <- renderText({
    description("glmStepAIC")
  })
  
  # output glmStepAICMetrics (table) ----
  output$glmStepAICMetrics <- renderTable({
    req(models$glmStepAIC)
    models$glmStepAIC$results[ which.min(models$glmStepAIC$results[, "RMSE"]), ]
  })
  
  # output glmStepAICModelPlots (plot) ----
  output$glmStepAICModelPlots <- renderPlot({
    req(models$glmStepAIC)
    plot(models$glmStepAIC)
  })     
  
  # output glmStepAICRecipe (print) ----
  output$glmStepAICRecipe <- renderPrint({
    req(models$glmStepAIC)
    models$glmStepAIC$recipe
  })  
  
  # output glmStepAICModelSummary2 (print) ----
  output$glmStepAICModelSummary2 <- renderPrint({
    req(models$glmStepAIC)
    summary(models$glmStepAIC$finalModel)
  })
  
  
  
  
  # METHOD * plsRglm ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getplsRglmRecipe ----
  getplsRglmRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$plsRglmPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent plsRglmGo ----
  observeEvent(
    input$plsRglmGo,
    {
      library(plsRglm)
      method <- "plsRglm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getplsRglmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output plsRglmModelSummary0 (text) ----
  output$plsRglmModelSummary0 <- renderText({
    description("plsRglm")
  })
  
  # output plsRglmMetrics (table) ----
  output$plsRglmMetrics <- renderTable({
    req(models$plsRglm)
    models$plsRglm$results[ which.min(models$plsRglm$results[, "RMSE"]), ]
  })
  
  # output plsRglmModelPlots (plot) ----
  output$plsRglmModelPlots <- renderPlot({
    req(models$plsRglm)
    plot(models$plsRglm)
  })     
  
  # output plsRglmRecipe (print) ----
  output$plsRglmRecipe <- renderPrint({
    req(models$plsRglm)
    models$plsRglm$recipe
  })  
  
  # output plsRglmModelSummary2 (print) ----
  output$plsRglmModelSummary2 <- renderPrint({
    req(models$plsRglm)
    summary(models$plsRglm$finalModel)
  })
  
  
  
  # METHOD * avNNet ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getavNNetRecipe ----
  getavNNetRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$avNNetPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent avNNetGo ----
  observeEvent(
    input$avNNetGo,
    {
      library(nnet)
      method <- "avNNet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getavNNetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output avNNetModelSummary0 (text) ----
  output$avNNetModelSummary0 <- renderText({
    description("avNNet")
  })
  
  # output avNNetMetrics (table) ----
  output$avNNetMetrics <- renderTable({
    req(models$avNNet)
    models$avNNet$results[ which.min(models$avNNet$results[, "RMSE"]), ]
  })
  
  # output avNNetModelPlots (plot) ----
  output$avNNetModelPlots <- renderPlot({
    req(models$avNNet)
    plot(models$avNNet)
  })     
  
  # output avNNetRecipe (print) ----
  output$avNNetRecipe <- renderPrint({
    req(models$avNNet)
    models$avNNet$recipe
  })  
  
  # output avNNetModelSummary2 (print) ----
  output$avNNetModelSummary2 <- renderPrint({
    req(models$avNNet)
    summary(models$avNNet$finalModel)
  })
  
  
  
  # METHOD * knn ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getknnRecipe ----
  getknnRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$knnPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent knnGo ----
  observeEvent(
    input$knnGo,
    {
      method <- "knn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getknnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output knnModelSummary (text) ----
  output$knnModelSummary0 <- renderText({
    description("knn")
  })
  
  # output knnMetrics (table) ----
  output$knnMetrics <- renderTable({
    req(models$knn)
    models$knn$results[ which.min(models$knn$results[, "RMSE"]), ]
  })
  
  # output knnModelPlots (plot) ----
  output$knnModelPlots <- renderPlot({
    req(models$knn)
    plot(models$knn)
  })
  
  # output knnRecipe (print) ----
  output$knnRecipe <- renderPrint({
    req(models$knn)
    models$knn$recipe
  })  
  
  # output knnModelSummary2 (print) ----
  output$knnModelSummary2 <- renderPrint({
    req(models$knn)
    print(models$knn)
  })
  
  
  
  # METHOD * rf ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getrfRecipe ----
  getrfRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$rfPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rfGo ----
  observeEvent(
    input$rfGo,
    {
      library(randomForest)
      method <- "rf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output rfModelSummary0 (text) ----
  output$rfModelSummary0 <- renderText({
    description("rf")
  })
  
  # output rfMetrics (table) ----
  output$rfMetrics <- renderTable({
    req(models$rf)
    models$rf$results[ which.min(models$rf$results[, "RMSE"]), ]
  })
  
  # output rfModelPlots (plot) ----
  output$rfModelPlots <- renderPlot({
    req(models$rf)
    plot(models$rf)
  })     
  
  # output rfRecipe (print) ----
  output$rfRecipe <- renderPrint({
    req(models$rf)
    models$rf$recipe
  })  
  
  # output rfModelSummary2 (print) ----
  output$rfModelSummary2 <- renderPrint({
    req(models$rf)
    summary(models$rf$finalModel)
  })
  
  
  

  # METHOD * kernelpls ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getkernelplsRecipe ----
  getkernelplsRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$kernelplsPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent kernelplsGo ----
  observeEvent(
    input$kernelplsGo,
    {
      library(pls)
      method <- "kernelpls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getkernelplsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output kernelplsModelSummary0 (text) ----
  output$kernelplsModelSummary0 <- renderText({
    description("kernelpls")
  })
  
  # output kernelplsMetrics (table) ----
  output$kernelplsMetrics <- renderTable({
    req(models$kernelpls)
    models$kernelpls$results[ which.min(models$kernelpls$results[, "RMSE"]), ]
  })
  
  # output kernelplsModelPlots (plot) ----
  output$kernelplsModelPlots <- renderPlot({
    req(models$kernelpls)
    plot(models$kernelpls)
  })     
  
  # output kernelplsRecipe (print) ----
  output$kernelplsRecipe <- renderPrint({
    req(models$kernelpls)
    models$kernelpls$recipe
  })  
  
  # output kernelplsModelSummary2 (print) ----
  output$kernelplsModelSummary2 <- renderPrint({
    req(models$kernelpls)
    summary(models$kernelpls$finalModel)
  })
  
  
  
  
  # METHOD * rlm ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getrlmRecipe ----
  getrlmRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$rlmPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rlmGo ----
  observeEvent(
    input$rlmGo,
    {
      library(MASS)
      method <- "rlm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getrlmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output rlmModelSummary0 (text) ----
  output$rlmModelSummary0 <- renderText({
    description("rlm")
  })
  
  # output rlmMetrics (table) ----
  output$rlmMetrics <- renderTable({
    req(models$rlm)
    models$rlm$results[ which.min(models$rlm$results[, "RMSE"]), ]
  })
  
  # output rlmModelPlots (plot) ----
  output$rlmModelPlots <- renderPlot({
    req(models$rlm)
    plot(models$rlm)
  })     
  
  # output rlmRecipe (print) ----
  output$rlmRecipe <- renderPrint({
    req(models$rlm)
    models$rlm$recipe
  })  
  
  # output rlmModelSummary2 (print) ----
  output$rlmModelSummary2 <- renderPrint({
    req(models$rlm)
    summary(models$rlm$finalModel)
  })
  
  
  
  
  # METHOD * rqlasso ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getrqlassoRecipe ----
  getrqlassoRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$rqlassoPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rqlassoGo ----
  observeEvent(
    input$rqlassoGo,
    {
      library(rqPen)
      method <- "rqlasso"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getrqlassoRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output rqlassoModelSummary0 (text) ----
  output$rqlassoModelSummary0 <- renderText({
    description("rqlasso")
  })
  
  # output rqlassoMetrics (table) ----
  output$rqlassoMetrics <- renderTable({
    req(models$rqlasso)
    models$rqlasso$results[ which.min(models$rqlasso$results[, "RMSE"]), ]
  })
  
  # output rqlassoModelPlots (plot) ----
  output$rqlassoModelPlots <- renderPlot({
    req(models$rqlasso)
    plot(models$rqlasso)
  })     
  
  # output rqlassoRecipe (print) ----
  output$rqlassoRecipe <- renderPrint({
    req(models$rqlasso)
    models$rqlasso$recipe
  })  
  
  # output rqlassoModelSummary2 (print) ----
  output$rqlassoModelSummary2 <- renderPrint({
    req(models$rqlasso)
    summary(models$rqlasso$finalModel)
  })
  
  
  
  # METHOD * cubist ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getcubistRecipe ----
  getcubistRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$cubistPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent cubistGo ----
  observeEvent(
    input$cubistGo,
    {
      library(Cubist)
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getcubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output cubistModelSummary0 (text) ----
  output$cubistModelSummary0 <- renderText({
    description("cubist")
  })
  
  # output cubistMetrics (table) ----
  output$cubistMetrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  # output cubistModelPlots (plot) ----
  output$cubistModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })     
  
  # output cubistRecipe (print) ----
  output$cubistRecipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  # output cubistModelSummary2 (print) ----
  output$cubistModelSummary2 <- renderPrint({
    req(models$cubist)
    summary(models$cubist$finalModel)
  })
  
  
  
  # METHOD * gaussprPoly ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getgaussprPolyRecipe ----
  getgaussprPolyRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$gaussprPolyPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent gaussprPolyGo ----
  observeEvent(
    input$gaussprPolyGo,
    {
      library(kernlab)
      method <- "gaussprPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getgaussprPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output gaussprPolyModelSummary0 (text) ----
  output$gaussprPolyModelSummary0 <- renderText({
    description("gaussprPoly")
  })
  
  # output gaussprPolyMetrics (table) ----
  output$gaussprPolyMetrics <- renderTable({
    req(models$gaussprPoly)
    models$gaussprPoly$results[ which.min(models$gaussprPoly$results[, "RMSE"]), ]
  })
  
  # output gaussprPolyModelPlots (plot) ----
  output$gaussprPolyModelPlots <- renderPlot({
    req(models$gaussprPoly)
    plot(models$gaussprPoly)
  })     
  
  # output gaussprPolyRecipe (print) ----
  output$gaussprPolyRecipe <- renderPrint({
    req(models$gaussprPoly)
    models$gaussprPoly$recipe
  })  
  
  # output gaussprPolyModelSummary2 (print) ----
  output$gaussprPolyModelSummary2 <- renderPrint({
    req(models$gaussprPoly)
    summary(models$gaussprPoly$finalModel)
  })
  
  
  
  
  # METHOD * gaussprRadial ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getgaussprRadialRecipe ----
  getgaussprRadialRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$gaussprRadialPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent gaussprRadialGo ----
  observeEvent(
    input$gaussprRadialGo,
    {
      library(kernlab)
      method <- "gaussprRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getgaussprRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output gaussprRadialModelSummary0 (text) ----
  output$gaussprRadialModelSummary0 <- renderText({
    description("gaussprRadial")
  })
  
  # output gaussprRadialMetrics (table) ----
  output$gaussprRadialMetrics <- renderTable({
    req(models$gaussprRadial)
    models$gaussprRadial$results[ which.min(models$gaussprRadial$results[, "RMSE"]), ]
  })
  
  # output gaussprRadialModelPlots (plot) ----
  output$gaussprRadialModelPlots <- renderPlot({
    req(models$gaussprRadial)
    plot(models$gaussprRadial)
  })     
  
  # output gaussprRadialRecipe (print) ----
  output$gaussprRadialRecipe <- renderPrint({
    req(models$gaussprRadial)
    models$gaussprRadial$recipe
  })  
  
  # output gaussprRadialModelSummary2 (print) ----
  output$gaussprRadialModelSummary2 <- renderPrint({
    req(models$gaussprRadial)
    summary(models$gaussprRadial$finalModel)
  })
  
  
  
  
  # METHOD * gaussprLinear ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getgaussprLinearRecipe ----
  getgaussprLinearRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$gaussprLinearPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent gaussprLinearGo ----
  observeEvent(
    input$gaussprLinearGo,
    {
      library(kernlab)
      method <- "gaussprLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getgaussprLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output gaussprLinearModelSummary0 (text) ----
  output$gaussprLinearModelSummary0 <- renderText({
    description("gaussprLinear")
  })
  
  # output gaussprLinearMetrics (table) ----
  output$gaussprLinearMetrics <- renderTable({
    req(models$gaussprLinear)
    models$gaussprLinear$results[ which.min(models$gaussprLinear$results[, "RMSE"]), ]
  })
  
  # output gaussprLinearModelPlots (plot) ----
  output$gaussprLinearModelPlots <- renderPlot({
    req(models$gaussprLinear)
    plot(models$gaussprLinear)
  })     
  
  # output gaussprLinearRecipe (print) ----
  output$gaussprLinearRecipe <- renderPrint({
    req(models$gaussprLinear)
    models$gaussprLinear$recipe
  })  
  
  # output gaussprLinearModelSummary2 (print) ----
  output$gaussprLinearModelSummary2 <- renderPrint({
    req(models$gaussprLinear)
    summary(models$gaussprLinear$finalModel)
  })
  
  
  
  
  
  
  # METHOD * svmPoly ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive svmPolyRecipe ----
  getsvmPolyRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$svmPolyPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmPolyGo ----
  observeEvent(
    input$svmPolyGo,
    {
      library(kernlab)
      method <- "svmPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getsvmPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output svmPolyModelSummary (text) ----
  output$svmPolyModelSummary0 <- renderText({
    description("svmPoly")
  })
  
  # output svmPolyMetrics (table) ----
  output$svmPolyMetrics <- renderTable({
    req(models$svmPoly)
    models$svmPoly$results[ which.min(models$svmPoly$results[, "RMSE"]), ]
  })
  
  # output svmPolyModelPlots (plot) ----
  output$svmPolyModelPlots <- renderPlot({
    req(models$svmPoly)
    plot(models$svmPoly)
  })
  
  # output svmPolyRecipe (print) ----
  output$svmPolyRecipe <- renderPrint({
    req(models$svmPoly)
    models$svmPoly$recipe
  })  
  
  # output svmPolyModelSummary2 (print) ----
  output$svmPolyModelSummary2 <- renderPrint({
    req(models$svmPoly)
    print(models$svmPoly)
  })
  
  
  # METHOD * krlsPoly ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive krlsPolyRecipe ----
  getkrlsPolyRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$krlsPolyPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent krlsPolyGo ----
  observeEvent(
    input$krlsPolyGo,
    {
      library(KRLS)
      method <- "krlsPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getkrlsPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output krlsPolyModelSummary (text) ----
  output$krlsPolyModelSummary0 <- renderText({
    description("krlsPoly")
  })
  
  # output krlsPolyMetrics (table) ----
  output$krlsPolyMetrics <- renderTable({
    req(models$krlsPoly)
    models$krlsPoly$results[ which.min(models$krlsPoly$results[, "RMSE"]), ]
  })
  
  # output krlsPolyModelPlots (plot) ----
  output$krlsPolyModelPlots <- renderPlot({
    req(models$krlsPoly)
    plot(models$krlsPoly)
  })
  
  # output krlsPolyRecipe (print) ----
  output$krlsPolyRecipe <- renderPrint({
    req(models$krlsPoly)
    models$krlsPoly$recipe
  })  
  
  # output krlsPolyModelSummary2 (print) ----
  output$krlsPolyModelSummary2 <- renderPrint({
    req(models$krlsPoly)
    print(models$krlsPoly)
  })
  
  
  # METHOD * rvmPoly ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive rvmPolyRecipe ----
  getrvmPolyRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$rvmPolyPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rvmPolyGo ----
  observeEvent(
    input$rvmPolyGo,
    {
      library(kernlab)
      method <- "rvmPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getrvmPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output rvmPolyModelSummary (text) ----
  output$rvmPolyModelSummary0 <- renderText({
    description("rvmPoly")
  })
  
  # output rvmPolyMetrics (table) ----
  output$rvmPolyMetrics <- renderTable({
    req(models$rvmPoly)
    models$rvmPoly$results[ which.min(models$rvmPoly$results[, "RMSE"]), ]
  })
  
  # output rvmPolyModelPlots (plot) ----
  output$rvmPolyModelPlots <- renderPlot({
    req(models$rvmPoly)
    plot(models$rvmPoly)
  })
  
  # output rvmPolyRecipe (print) ----
  output$rvmPolyRecipe <- renderPrint({
    req(models$rvmPoly)
    models$rvmPoly$recipe
  })  
  
  # output rvmPolyModelSummary2 (print) ----
  output$rvmPolyModelSummary2 <- renderPrint({
    req(models$rvmPoly)
    print(models$rvmPoly)
  })
  
  
  
  # METHOD * qrf ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive qrfRecipe ----
  getqrfRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$qrfPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent qrfGo ----
  observeEvent(
    input$qrfGo,
    {
      library(quantregForest)
      method <- "qrf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getqrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output qrfModelSummary (text) ----
  output$qrfModelSummary0 <- renderText({
    description("qrf")
  })
  
  # output qrfMetrics (table) ----
  output$qrfMetrics <- renderTable({
    req(models$qrf)
    models$qrf$results[ which.min(models$qrf$results[, "RMSE"]), ]
  })
  
  # output qrfModelPlots (plot) ----
  output$qrfModelPlots <- renderPlot({
    req(models$qrf)
    plot(models$qrf)
  })
  
  # output qrfRecipe (print) ----
  output$qrfRecipe <- renderPrint({
    req(models$qrf)
    models$qrf$recipe
  })  
  
  # output qrfModelSummary2 (print) ----
  output$qrfModelSummary2 <- renderPrint({
    req(models$qrf)
    print(models$qrf)
  })
  
  
  
  
  # METHOD * ranger ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive rangerRecipe ----
  getrangerRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$rangerPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rangerGo ----
  observeEvent(
    input$rangerGo,
    {
      library(e1071)
      library(ranger)
      library(dplyr)
      method <- "ranger"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getrangerRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output rangerModelSummary (text) ----
  output$rangerModelSummary0 <- renderText({
    description("ranger")
  })
  
  # output rangerMetrics (table) ----
  output$rangerMetrics <- renderTable({
    req(models$ranger)
    models$ranger$results[ which.min(models$ranger$results[, "RMSE"]), ]
  })
  
  # output rangerModelPlots (plot) ----
  output$rangerModelPlots <- renderPlot({
    req(models$ranger)
    plot(models$ranger)
  })
  
  # output rangerRecipe (print) ----
  output$rangerRecipe <- renderPrint({
    req(models$ranger)
    models$ranger$recipe
  })  
  
  # output rangerModelSummary2 (print) ----
  output$rangerModelSummary2 <- renderPrint({
    req(models$ranger)
    print(models$ranger)
  })
  
  
  
  # METHOD * xgbLinear ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive xgbLinearRecipe ----
  getxgbLinearRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$xgbLinearPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent xgbLinearGo ----
  observeEvent(
    input$xgbLinearGo,
    {
      library(xgboost)
      method <- "xgbLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getxgbLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output xgbLinearModelSummary (text) ----
  output$xgbLinearModelSummary0 <- renderText({
    description("xgbLinear")
  })
  
  # output xgbLinearMetrics (table) ----
  output$xgbLinearMetrics <- renderTable({
    req(models$xgbLinear)
    models$xgbLinear$results[ which.min(models$xgbLinear$results[, "RMSE"]), ]
  })
  
  # output xgbLinearModelPlots (plot) ----
  output$xgbLinearModelPlots <- renderPlot({
    req(models$xgbLinear)
    plot(models$xgbLinear)
  })
  
  # output xgbLinearRecipe (print) ----
  output$xgbLinearRecipe <- renderPrint({
    req(models$xgbLinear)
    models$xgbLinear$recipe
  })  
  
  # output xgbLinearModelSummary2 (print) ----
  output$xgbLinearModelSummary2 <- renderPrint({
    req(models$xgbLinear)
    print(models$xgbLinear)
  })
  
  
  
  
  # METHOD * blackboost ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive blackboostRecipe ----
  getblackboostRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$blackboostPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent blackboostGo ----
  observeEvent(
    input$blackboostGo,
    {
      library(party)
      library(mboost)
      library(plyr)
      library(partykit)
      method <- "blackboost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getblackboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output blackboostModelSummary (text) ----
  output$blackboostModelSummary0 <- renderText({
    description("blackboost")
  })
  
  # output blackboostMetrics (table) ----
  output$blackboostMetrics <- renderTable({
    req(models$blackboost)
    models$blackboost$results[ which.min(models$blackboost$results[, "RMSE"]), ]
  })
  
  # output blackboostModelPlots (plot) ----
  output$blackboostModelPlots <- renderPlot({
    req(models$blackboost)
    plot(models$blackboost)
  })
  
  # output blackboostRecipe (print) ----
  output$blackboostRecipe <- renderPrint({
    req(models$blackboost)
    models$blackboost$recipe
  })  
  
  # output blackboostModelSummary2 (print) ----
  output$blackboostModelSummary2 <- renderPrint({
    req(models$blackboost)
    print(models$blackboost)
  })
  
  # METHOD * bagEarthGCV ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive bagEarthGCVRecipe ----
  getbagEarthGCVRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$bagEarthGCVPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent bagEarthGCVGo ----
  observeEvent(
    input$bagEarthGCVGo,
    {
      library(earth)
      method <- "bagEarthGCV"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getbagEarthGCVRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output bagEarthGCVModelSummary (text) ----
  output$bagEarthGCVModelSummary0 <- renderText({
    description("bagEarthGCV")
  })
  
  # output bagEarthGCVMetrics (table) ----
  output$bagEarthGCVMetrics <- renderTable({
    req(models$bagEarthGCV)
    models$bagEarthGCV$results[ which.min(models$bagEarthGCV$results[, "RMSE"]), ]
  })
  
  # output bagEarthGCVModelPlots (plot) ----
  output$bagEarthGCVModelPlots <- renderPlot({
    req(models$bagEarthGCV)
    plot(models$bagEarthGCV)
  })
  
  # output bagEarthGCVRecipe (print) ----
  output$bagEarthGCVRecipe <- renderPrint({
    req(models$bagEarthGCV)
    models$bagEarthGCV$recipe
  })  
  
  # output bagEarthGCVModelSummary2 (print) ----
  output$bagEarthGCVModelSummary2 <- renderPrint({
    req(models$bagEarthGCV)
    print(models$bagEarthGCV)
  })
  
  # METHOD * GaussprPoly & Cubist ensemble ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive GaussCubistEnsembleRecipe ----
  getGaussCubistEnsembleRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$GaussCubistEnsemblePreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent GaussCubistEnsembleGo ----
  observeEvent(
    input$GaussCubistEnsembleGo,
    {
      library(caretEnsemble)
      library(Cubist)
      library(kernlab)
      method <- c("gaussprPoly", "cubist")
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caretList(getGaussCubistEnsembleRecipe(), data = getTrainData(), methodList = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output GaussCubistEnsembleModelSummary (text) ----
  output$GaussCubistEnsembleModelSummary0 <- renderText({
    description("GaussCubistEnsemble")
  })
  
  # output GaussCubistEnsembleMetrics (table) ----
  output$GaussCubistEnsembleMetrics <- renderTable({
    req(models$GaussCubistEnsemble)
    models$GaussCubistEnsemble$results[ which.min(models$GaussCubistEnsemble$results[, "RMSE"]), ]
  })
  
  # output GaussCubistEnsembleModelPlots (plot) ----
  output$GaussCubistEnsembleModelPlots <- renderPlot({
    req(models$GaussCubistEnsemble)
    plot(models$GaussCubistEnsemble)
  })
  
  # output GaussCubistEnsembleRecipe (print) ----
  output$GaussCubistEnsembleRecipe <- renderPrint({
    req(models$GaussCubistEnsemble)
    models$GaussCubistEnsemble$recipe
  })  
  
  # output GaussCubistEnsembleModelSummary2 (print) ----
  output$GaussCubistEnsembleModelSummary2 <- renderPrint({
    req(models$GaussCubistEnsemble)
    print(models$GaussCubistEnsemble)
  })
  
  
  
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  # reactive getResamples ----
  getResamples <- reactive({
    models <- reactiveValuesToList(models)
    results <- caret::resamples(models)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }

    #hide results worse than null model
    subset <- rep(TRUE, length(models))
    if (input$HideWorse & NullModel %in% names(models)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })

  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })

  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
    
})
