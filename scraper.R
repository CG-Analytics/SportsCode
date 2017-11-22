library("rvest")
library("magrittr")

lego<- html("http://www.imdb.com/title/tt1490017/")

sports <- read_html("https://football.fantasysports.yahoo.com/f1/624082")

sports %>%
  
  html_nodes("#matchup_selectlist_nav .flyout-title")%>%
  html_text()

  
frame <- sports %>% 
  html_nodes(".F-link , #matchupweek .Fz-lg")%>%
  html_text()

w = 1
ind = 0

name <- c()
score <- c()

#use this frame to assign a score to an index
comp <- c("RichHomieKwons","The Twerkinators","Moat Cailin","XO TILLY LIF3","100% Rand","Azor Ajayi","Path of Totality","2nd Siege AJ Meereen","Shitticanes","Gut Cassidies")

scores<-c("","","","","","","","","","")

for (w in 1:length(frame)){
  
  if(w%%2==0){
    
    ind = which(comp == frame[w])
    scores[ind] = frame[w-1]
  }
  
  w = w+1
  
}

scores = as.numeric(scores)
scores

w#use this frame to assign a score to an index
comp <- c("RichHomieKwons","The Twerkinators","Moat Cailin","XO TILLY LIF3","100% Rand","Azor Ajayi","Path of Totality","2nd Siege AJ Meereen","Shitticanes","Gut Cassidies")
which(comp == "Moat Cailin", arr.ind = TRUE)



name
score
