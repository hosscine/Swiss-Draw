
RV <- shiny::reactiveValues(error = "", round = 0, save.time = "", update = 0,
                            dnm = list(left = rep("NO ENTRY", tor$nfcard),
                                       right = rep("NO ENTRY", tor$nfcard)))

shinyServer(
  function(input, output, session) {
    
    ####################
    # Ranking View
    ####################
    
    output$history <- renderTable({
      if (RV$update > 0) tor$result.complete
    }, digits = 0)
    
    output$summary <- renderTable({
      if (RV$update > 0){
        sm <- tor$result.summary[tor$ranking.id,]
        colnames(sm) <- c("圧勝", "辛勝", "惜敗", "惨敗")
        sm
      }
    }, include.rownames=T, digits = 0)
    
    
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
      if (RV$update > 0){
        sm <- tor$ranking.summary[tor$ranking.id,]
        colnames(sm) <- c("順位", "デッキ", "使用者", "勝")
        sm
      }
    }, include.rownames=F, digit = 0)
    
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
        
        # 勝敗記録をuiから収集
        result <- data.frame(
          didl = tor$fight.card.id$didl,
          didr = tor$fight.card.id$didr,
          winl = sapply(1:tor$nfcard, function(i) input[[paste0("resultl",i)]]),
          winr = sapply(1:tor$nfcard, function(i) input[[paste0("resultr",i)]])
        )
        
        test <<- result
        # 有効な勝敗記録を登録
        for (i in 1:tor$nplayer) {
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
        RV$save.time <- paste("Last Saved --", format(Sys.time(), "%b月%d日 %H時%M分")) 
        RV$update <- RV$update + 1
        
        # トーナメントを保存
        if (exists("datapath")) save(tor, file = datapath)
        else save(tor, file = paste0("tournament/", tor$title.en, ".tournament"))
        # print(tor$result)
        # test <<- tor$clone()
      }
    })
    
    # 抽選ボタン
    observeEvent(input$lottery,{
      for (i in 1:tor$nfcard) {
        updateSelectInput(session, paste0("resultl", i), selected = "--")
        updateSelectInput(session, paste0("resultr", i), selected = "--")
      }
      
      error <- tor$setNewFightCard()
      RV$dnm <- tor$fight.card.list
      RV$error <- ifelse(error, "Error: lottery failed", "")
      tor$round <- tor$round + 1
      RV$round <- tor$round
    })
    
    # 再抽選ボタン（ラウンドを進めない抽選）
    observeEvent(input$relottery,{
      for (i in 1:tor$nfcard) {
        updateSelectInput(session, paste0("resultl", i), selected = "--")
        updateSelectInput(session, paste0("resultr", i), selected = "--")
      }
      error <- tor$setNewFightCard()
      RV$error <- ifelse(error, "Error: lottery failed", "")
      RV$dnm <- tor$fight.card.list
    })
    
    # load tournament 機能
    observeEvent(input$tofile,{
      if (!is.null(input$tofile)) {
        # トーナメントファイルを読み込み
        datapath <<- paste0("tournaments/", input$tofile$name)
        load(datapath)
        tor <<- tor
        RV$error <- "Message: load complete"
        RV$round <- tor$round
        RV$update <- RV$update + 1
        
        # 読み込んだトーナメントファイルを対戦テーブルに反映
        RV$dnm <- tor$fight.card.list
        for (i in 1:tor$nfcard) {
          l <- tor$fight.card.id$didl[i]
          r <- tor$fight.card.id$didr[i]
          if (!tor$is.newFightCard(l, r)){
            result.i <- tor$result[tor$result$didl == l & tor$result$didr == r,]
            updateSelectInput(session, paste0("resultl", i), selected = result.i$winl)
            updateSelectInput(session, paste0("resultr", i), selected = result.i$winr)
          }
          else{
            updateSelectInput(session, paste0("resultl", i), selected = "--")
            updateSelectInput(session, paste0("resultr", i), selected = "--")
          }
        }
        
      }
    })
    
  })

