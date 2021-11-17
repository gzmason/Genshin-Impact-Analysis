library(tidyverse)  
library(rvest)  
library(robotstxt) 
library(readxl)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  
  
  to_return = data.frame(matrix(ncol = 0, nrow = 0))

  for (df in x){
    to_return = rbind(to_return, df)
  }

  return (to_return)
}

base_url = "https://genshin.honeyhunterworld.com/db/"
character_url = "char/characters/"
weapons_urls = c("weapon/sword/", "weapon/claymore/", "weapon/polearm/", "weapon/bow/","weapon/catalyst/")
artifact_url = "db/artifact/"
cn_url = "?lang=CHS"
eng_url = "?lang=EN"

# Get character names
eng_char_page <- read_html(paste(base_url, character_url , eng_url)) 
cn_char_page <- read_html(paste(base_url, character_url , cn_url))

char_name_eng = eng_char_page %>% html_nodes(".sea_charname") %>% html_text()
char_name_cn = cn_char_page %>% html_nodes(".sea_charname") %>% html_text()

character <- data.frame(char_name_eng, char_name_cn)

# Get artifact names
eng_artifact_page <- read_html(paste(base_url, artifact_url , eng_url)) 
cn_artifact_page <- read_html(paste(base_url, artifact_url , cn_url))

artifact_name_eng = eng_artifact_page %>% html_nodes("td~ td+ td a") %>% html_text()
artifact_name_cn = cn_artifact_page %>% html_nodes("td~ td+ td a") %>% html_text()

artifact <- data.frame(artifact_name_eng, artifact_name_cn)



# Get weapon names
weapon = data.frame(matrix(ncol = 0, nrow = 0))
for (weapon_url in weapons_urls){
  eng_weapon_page <- read_html(paste(base_url, weapon_url , eng_url)) 
  cn_weapon_page <- read_html(paste(base_url, weapon_url , cn_url))
  
  weapon_name_eng = eng_weapon_page %>% html_nodes("td:nth-child(3) a") %>% html_text()
  weapon_name_cn = cn_weapon_page %>% html_nodes("td:nth-child(3) a") %>% html_text()
  
  weapon <- rbind(weapon, data.frame(weapon_name_eng, weapon_name_cn))
}
  
# Function to apply for translation
fun_char <- function(x) {
  for(i in seq(1,nrow(character))){
    if (x == character[i, 2]){
      return (character[i, 1])
    }
  }
}

fun_artifact <- function(x) {
  for(i in seq(1,nrow(artifact))){
    if (x == artifact[i, 2]){
      return (artifact[i, 1])
    }
  }
}

dirs = c("sept_late/", "oct_early/", "oct_late/", "nov_early/")
files = c("battle_stats.xlsx", "character_artifact.xlsx", "character_owning.xlsx")

for (i in 1:3){
  battle_stats <- read_excel_allsheets(paste( dirs[i], files[1], sep = ""))
  battle_stats$avatar <- lapply(battle_stats$avatar, fun_char)
  battle_stats <- apply(battle_stats,2,as.character)
  write.csv(battle_stats, paste(dirs[i], "battle_stats_translated.csv",sep = ""))
  
  
  character_artifact <- read_excel_allsheets(paste( dirs[i], files[2], sep = ""))
  character_artifact$avatar <- lapply(character_artifact$avatar, fun_char)
  character_artifact$relicSet <- lapply(character_artifact$relicSet, fun_artifact)
  character_artifact <- apply(character_artifact,2,as.character)
  write.csv(character_artifact, paste(dirs[i], "character_artifact_translated.csv",sep = ""))
  
  
  
  character_owning <- read_excel_allsheets(paste( dirs[i], files[3], sep = ""))
  character_owning$avatar <- lapply(character_owning$avatar, fun_char)
  character_owning <- apply(character_owning,2,as.character)
  write.csv(character_owning, paste(dirs[i], "character_owning_translated.csv",sep = ""))
  
}


