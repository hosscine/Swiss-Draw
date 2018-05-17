
# 0. required packages -------------------------------------------------------
# myfs
# magrittr
# R6

require(magrittr)
source("entry_class.R")
source("tournament_class.R")


# 1. edit japanese/english tournament title ----------------------------------
title.ja <- "AとBの闘い"
title.en <- "battle of A and B"


# 2. edit player names -------------------------------------------------------
member <- c("A", "B")


# 3. edit deck.name and deck.player.id ---------------------------------------
deck <- data.frame(
  deck.names = c("Kaiju", "Kozmo", "SR", "BA"),
  member.id = c(1, 1, 2, 2)
)

# save(deck, member, title.ja, title.en, file = paste0("tournaments/", title.en, ".entry"))
# ent <- loadEntryData("entry_file.entry")
ent <- loadEntryData()
tor <- startTournament(ent)


# 4. edit the file name ------------------------------------------------------
save(tor, file = paste0("tournaments/", tor$title.en, ".tournament"))


# 5. RUN SHINY APP and READ TOURNAMENT FILE! ---------------------------------
