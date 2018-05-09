require(magrittr)

load("sample.entry")
source("systems.R")
source("history view.R")
data.kos <- cbind(numeric(nrow(data.deck)),data.deck[,2],
                  data.member[as.numeric(data.deck[,3]),2],numeric(nrow(data.deck)))
colnames(data.kos) <- c("順位","デッキ名","使用者","勝")

react.val <- shiny::reactiveValues(kos = data.kos, sttext = NULL, title = "ROUND 0",
                                save.time = "", values.history = NULL)

data.history <- NULL

shinyServer(
  function(input, output, session) {
    
    ####################
    # Ranking View
    ####################
    
    output$history <- renderTable({
      hist <- cbind(react.val$values.history[, 1], ShowHistory(hist = react.val$values.history))
      if(!is.null(hist)) colnames(hist) <- c("Round", "Deck1", "Deck2", "Result1", "Result2")
      hist
    })
    
    output$summary <- renderTable({
      if(!is.null(react.val$values.history)){
        summary <- ShowKOSummary(react.val$values.history)
        rownames(summary) <- deck.user[,1]
        colnames(summary) <- c("圧勝", "辛勝", "惜敗", "惨敗")
        summary <- summary[order(summary[,1], summary[,2],
                                 summary[,3], summary[,4], decreasing = T),]
        summary
      }
    }, include.rownames=T)
    
    
    ####################
    # Match View
    ####################
    
    #### TitlePanel ####
    output$title <- renderText({
      react.val$title
    })
    
    #### SideBar ####
    output$status <- renderText({
      react.val$sttext
    })
    
    output$kos <- renderTable({
      react.val$kos
    }, include.rownames=F)
    
    output$savetime <- renderText({
      react.val$save.time
    })
    
    #### MainPanel ####
    output$deck.left <- renderUI({
      lapply(1:(decks / 2), function(i){
        selectInput(paste0("deckl", i), label = paste0("match", i), 
                    choices = as.character(data.deck[,2]), selected = 0)
      })
    })
    output$deck.right <- renderUI({
      lapply(1:(decks / 2), function(i){
        selectInput(paste0("deckr",i ), label = "vs",
                    choices = as.character(data.deck[,2]),selected = 0)
      })
    })
    output$result.left <- renderUI({
      lapply(1:(decks / 2), function(i){
        selectInput(paste0("resultl", i), label = paste0("result", i), choices = c("--", 0:2))
      })
    })
    output$result.right <- renderUI({
      lapply(1:(decks / 2), function(i){
        selectInput(paste0("resultr", i), label = "vs", choices = c("--", 0:2))
      })
    })
    
    
    #### Ivents ####
    observeEvent(input$save, {
      
      react.val$sttext <- NULL
      
      #成績収集
      result <- cbind(
        sapply(1:(decks / 2), function(i) which(data.deck[,2] == input[[paste0("deckl",i)]])),
        sapply(1:(decks / 2), function(i) which(data.deck[,2] == input[[paste0("deckr",i)]])),
        sapply(1:(decks / 2), function(i) input[[paste0("resultl",i)]]),
        sapply(1:(decks / 2), function(i) input[[paste0("resultr",i)]])
      )
      
      #historyの記録
      for (game in 1:(decks / 2)) {
        if(identical(could_be_numeric(result[game, 3:4]), c(T, T))){
          if (IdenticalMatch(result[game, 1:2], data.history)) {
            if (result[game, 3] == result[game, 4])
              react.val$sttext <- "Error: Result that wins same times was tried to save."
            else
              data.history <<- rbind(data.history,
                                     react.val$title %>% as.character %>% substring(first = 7) %>% 
                                       c(result[game,]) %>% as.numeric
                                     )
            react.val$values.history <- data.history
          }
        }
      }
      
      #koの更新
      if (!is.null(data.history)) {
        data.kos <<- calcKos(data.kos,data.history[, 2:5], react.val)
        react.val$kos <- data.kos[order(as.numeric(data.kos[, 4]), decreasing = T),]
        react.val$save.time <- paste("last saved", Sys.time())
        
        attr(data.history, "match.table") <- result
        attr(data.history, "round") <- substring(as.character(react.val$title), 7)
        save(data.history, file = "korec.ko")
      }
      
    })
    
    #抽選ボタン
    observeEvent(input$lottery,{
      opponent <- calcOponent(data.kos, data.history[, 2:5], react.val)
      for (i in 1:(decks / 2)) {
        updateSelectInput(session,paste0("deckl", i), selected = data.deck[opponent[i, 1], 2])
        updateSelectInput(session,paste0("deckr", i), selected = data.deck[opponent[i, 2], 2])
        updateSelectInput(session,paste0("resultl", i), selected = "--")
        updateSelectInput(session,paste0("resultr", i), selected = "--")
      }
      
      react.val$title <- paste("ROUND", as.numeric(substring(react.val$title, 7))+1)
    })
    
    #再抽選ボタン（ラウンドを進めない抽選）
    observeEvent(input$relottery,{
      opponent <- calcOponent(data.kos, data.history[, 2:5], react.val)
      for (i in 1:(decks / 2)) {
        updateSelectInput(session,paste0("deckl", i), selected = data.deck[opponent[i, 1], 2])
        updateSelectInput(session,paste0("deckr", i), selected = data.deck[opponent[i, 2], 2])
        updateSelectInput(session,paste0("resultl", i), selected = "--")
        updateSelectInput(session,paste0("resultr", i), selected = "--")
      }
    })
    
    #load ko 機能
    observeEvent(input$kofile,{
      if (is.null(input$kofile) == F) {
        load(paste0("./", input$kofile[1]))
        data.history <<- data.history
        
        # for .ko file ver2.2 early
        if(ncol(data.history) == 4){
          data.history.attr <- attributes(data.history)
          data.history <- cbind(0, data.history)
          attr(data.history, "match.table") <- data.history.attr$match.table
        }
          
        react.val$values.history <- data.history
        
        
        match.table <- attr(data.history, "match.table")
        for (i in 1:(decks / 2)) {
          updateSelectInput(session, paste0("deckl", i),
                            selected = data.deck[as.numeric(match.table[i, 1]), 2])
          updateSelectInput(session, paste0("deckr", i),
                            selected = data.deck[as.numeric(match.table[i, 2]), 2])
          updateSelectInput(session, paste0("resultl", i), selected = match.table[i, 3])
          updateSelectInput(session, paste0("resultr", i), selected = match.table[i, 4])
        }
        react.val$title <- paste("ROUND", as.numeric(attr(data.history, "round"), 7))
        
        if (!is.null(data.history)) {
          data.kos <<- calcKos(data.kos, data.history[, 2:5], react.val)
          react.val$kos <- data.kos[order(as.numeric(data.kos[, 4]), decreasing = T),]
          react.val$stext <- "Load completed."
        }
      }
    })
    
  })

