tagList(
  navbarPage(
    theme = shinythemes::shinytheme("darkly"),  # <--- To use a theme, uncomment this
    title = "Swiss Draw Lottery",

# Match View タブ ------------------------------------------------------------------
    tabPanel("Match View",
             titlePanel(textOutput("title")),
             sidebarLayout(position = "right",

# 右側の対戦テーブル -----------------------------------------------------------------
                           sidebarPanel(
                             h3("Points Summary"),
                             textOutput("status"), # エラーメッセージ表示
                             tags$head(tags$style("#status{color: red; font-size: 14px; }")),
                             tableOutput("kos"), # 現在の順位表
                             actionButton("lottery","抽選"),
                             actionButton("relottery","再抽選"),
                             hr(),
                             fileInput("kofile","koファイル読み込み")
                           ),

# 左側の対戦デッキと結果 -------------------------------------------------------------
                           mainPanel(
                             fluidRow(
                               column(3,offset = 1,uiOutput("deck.left")), # 左側の対戦者
                               column(3,uiOutput("deck.right")), # 右側の対戦者
                               column(2,offset = 1,uiOutput("result.left")), # 左側の結果入力
                               column(2,uiOutput("result.right")) # 右側の結果入力
                             ),
                             fluidRow(
                               column(6,offset = 3,textOutput("savetime")), # 最終保存時間
                               column(3,actionButton("save","成績記録")) # 保存ボタン
                             )
                           )
             )
    ),

# Ranking View タブ ------------------------------------------------------------------
  tabPanel("Ranking View",
             sidebarLayout(mainPanel(titlePanel("Ranking"),
                                     tableOutput("summary"),width = 6),
                           
                           sidebarPanel(h3("Duel History"),
                                        tableOutput("history"),width = 6),position = "left")
    )
  )
)