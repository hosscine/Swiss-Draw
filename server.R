require(magrittr)

source("entry_class.R")
source("tournament_class.R")
source("systems.R")
source("history view.R")

ent <- loadEntryData("sample.entry")
tor <- startTournament(ent)

data.kos <- cbind(numeric(nrow(data.deck)),data.deck[,2],
                  data.member[as.numeric(data.deck[,3]),2],numeric(nrow(data.deck)))
colnames(data.kos) <- c("順位","デッキ名","使用者","勝")

react.val <- shiny::reactiveValues(tornament = tor, sttext = NULL, title = "ROUND 0",
                                   save.time = "", values.history = NULL)

data.history <- NULL

shinyServer(
  function(input, output, session) {
    
    ####################
    # Ranking View
    ####################
    
    output$history <- renderTable({
      # hist <- cbind(react.val$values.history[, 1], ShowHistory(hist = react.val$values.history))
      # if(!is.null(hist)) colnames(hist) <- c("Round", "Deck1", "Deck2", "Result1", "Result2")
      # hist
      react.val$tornamet$results
    })
    
    output$summary <- renderTable({
      if(!is.null(react.val$tornament$results)){
        summary <- ShowKOSummary(react.val$tornament$results)
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
      react.val$tornament$results
    }, include.rownames=F)
    
    output$savetime <- renderText({
      react.val$save.time
    })
    
    #### MainPanel ####
    output$deck.left <- renderUI({
      lapply(1:(decks / 2), function(i){
        selectInput(paste0("deckl", i), label = paste0("match", i), 
                    choices = ent$deck, selected = 0)
      })
    })
    output$deck.right <- renderUI({
      lapply(1:(decks / 2), function(i){
        selectInput(paste0("deckr",i ), label = "vs",
                    choices = ent$deck, selected = 0)
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
      
      react.val$sttext <- "ok"
      
      #勝敗記録をuiから収集
      result <- data.frame(
        didl = sapply(1:(decks / 2), function(i) which(ent$deck == input[[paste0("deckl",i)]])),
        didr = sapply(1:(decks / 2), function(i) which(ent$deck == input[[paste0("deckr",i)]])),
        winl = sapply(1:(decks / 2), function(i) input[[paste0("resultl",i)]]),
        winr = sapply(1:(decks / 2), function(i) input[[paste0("resultr",i)]])
      )
      print(result)
      
      #有効な勝敗記録を登録
      for (i in 1:ent$nplayer) {
        # if result[i,] include "--"
        if(NA %in% suppressWarnings(result[i,] %>% as.character %>% as.numeric)) next
        
        result.i <- result[i,]
        if(!tor$is.validFightCard(result.i$didl, result.i$didr))
          react.val$sttext <- "Error: tied to submit non valid fight card"
        
        else if(result.i$winl == result.i$winr)
          react.val$sttext <- "Error: includes invalid fight result both players win same times"
        
        else if(xor(result.i$winl == 2, result.i$winr == 2))
          react.val$sttext <- "Error: includes invalid fight result neither player win 2 times"
        
        # submit fight result here
        else react.val$tornament$addFightResult(result.i)
        
      }
      
      # 勝敗数を再計算
      react.val$tornament$updateWinLose()
      react.val$save.time <- paste("last saved", Sys.time()) 
      
      save(react.val$tornament, file = "korec.ko")
      
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

