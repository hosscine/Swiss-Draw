require(magrittr)

source("entry_class.R")
source("tournament_class.R")
source("history view.R")

ent <- loadEntryData("sample2.entry")
tor <- startTournament(ent)


RV <- shiny::reactiveValues(error = "", round = 0, save.time = "",
                            dnm = list(left = rep("NO ENTRY", tor$nfcard),
                                       right = rep("NO ENTRY", tor$nfcard)))

data.history <- NULL

shinyServer(
  function(input, output, session) {
    
    ####################
    # Ranking View
    ####################
    
    output$history <- renderTable({
      tor$result.complete
    }, digits = 0)
    
    output$summary <- renderTable({
      if(!is.null(tor$result)){
        summary <- ShowKOSummary(tor$result)
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
      paste("ROUND", RV$round)
    })
    
    #### SideBar ####
    output$status <- renderText({
      RV$error
    })
    
    output$kos <- renderTable({
      tor$result
    }, include.rownames=F)
    
    output$savetime <- renderText({
      RV$save.time
    })
    
    #### MainPanel ####
    output$fightrow <- renderUI({
      lapply(1:tor$nfcard, function(i){
        fluidRow(
          column(3, h3(RV$dnm$left[i])),
          column(1, h3("VS")),
          column(3, h3(RV$dnm$right[i])),
          column(2, offset = 1,
                 selectInput(paste0("resultl", i), label = "result", choices = c("--", 0:2))),
          column(2, selectInput(paste0("resultr", i), label = "vs", choices = c("--", 0:2)))
        )
      })
    })
    
    #### Ivents ####
    observeEvent(input$save, {
      
      # 抽選が押される前は無視
      if(nrow(tor$fight.card) > 0){        
        RV$error <- ""
        
        #勝敗記録をuiから収集
        result <- data.frame(
          didl = tor$fight.card.id$didl,
          didr = tor$fight.card.id$didr,
          winl = sapply(1:tor$nfcard, function(i) input[[paste0("resultl",i)]]),
          winr = sapply(1:tor$nfcard, function(i) input[[paste0("resultr",i)]])
        )
        
        test <<- result
        #有効な勝敗記録を登録
        for (i in 1:ent$nplayer) {
          # if result[i,] includes "--"
          if(NA %in% suppressWarnings(result[i,] %>% as.matrix %>% as.numeric)) next
          
          # i行目の対戦カードと結果をリストに格納
          result.i <- result[i,] %>% as.matrix %>% as.numeric %>% as.list
          names(result.i) <- colnames(result)
          
          if(!tor$is.validFightCard(result.i$didl, result.i$didr)) next
            # RV$error <- "Error: tried to submit non valid fight card"
          
          else if(result.i$winl == result.i$winr)
            RV$error <- "Error: includes invalid fight result both players win same times"
          
          else if(!xor(result.i$winl == 2, result.i$winr == 2))
            RV$error <- "Error: includes invalid fight result neither player win 2 times"
          
          # submit fight result here
          else tor$addFightResult(didl = result.i$didl, didr = result.i$didr,
                                  winl = result.i$winl, winr = result.i$winr)
        }
        
        # 勝敗数を再計算
        tor$updateWinLose()
        RV$save.time <- paste("last saved", Sys.time()) 
        
        # トーナメントを保存
        save(tor, file = "korec.ko")
        print(tor$result)
      }
    })
    
    #抽選ボタン
    observeEvent(input$lottery,{
      for (i in 1:tor$nfcard) {
        updateSelectInput(session, paste0("resultl", i), selected = "--")
        updateSelectInput(session, paste0("resultr", i), selected = "--")
      }
      
      tor$setNewFightCard()
      RV$dnm <- tor$fight.card.list
      tor$round <- tor$round + 1
      RV$round <- tor$round
    })
    
    #再抽選ボタン（ラウンドを進めない抽選）
    observeEvent(input$relottery,{
      for (i in 1:tor$nfcard) {
        updateSelectInput(session, paste0("resultl", i), selected = "--")
        updateSelectInput(session, paste0("resultr", i), selected = "--")
      }
      tor$setNewFightCard()
      RV$dnm <- tor$fight.card.list
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
        
        RV$values.history <- data.history
        
        
        match.table <- attr(data.history, "match.table")
        for (i in 1:(decks / 2)) {
          updateSelectInput(session, paste0("deckl", i),
                            selected = data.deck[as.numeric(match.table[i, 1]), 2])
          updateSelectInput(session, paste0("deckr", i),
                            selected = data.deck[as.numeric(match.table[i, 2]), 2])
          updateSelectInput(session, paste0("resultl", i), selected = match.table[i, 3])
          updateSelectInput(session, paste0("resultr", i), selected = match.table[i, 4])
        }
        RV$round <- tor$round
        
        if (!is.null(data.history)) {
          data.kos <<- calcKos(data.kos, data.history[, 2:5], RV)
          RV$kos <- data.kos[order(as.numeric(data.kos[, 4]), decreasing = T),]
          RV$stext <- "Load completed."
        }
      }
    })
    
  })

