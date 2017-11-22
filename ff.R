rm(list=ls());
print(date())
library("RODBC")
library(sqldf)
library(plyr)
library(dplyr)
library(MatchIt)
library(lpSolveAPI)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(reshape2)
library(zoo)
library(gridExtra)
library(psych)
library("rvest")
library("magrittr")

#scrape up top and leave the rest of the code intact. 

#read in our home page. Use CSS Selectors for scores and team names
sports <- read_html("https://football.fantasysports.yahoo.com/f1/624082")

#create a container for scraped text
frame <- sports %>% 
  html_nodes(".F-link , #matchupweek .Fz-lg")%>%
  html_text()

#set some values for the forthcoming loop
w = 1
ind = 0

#empty data frames
name <- c()
score <- c()

#use this frame to assign a score to an index
comp <- c("RichHomieKwons","The Twerkinators","Moat Cailin","XO TILLY LIF3","100% Rand","Azor Ajayi","Path of Totality","2nd Siege AJ Meereen","Shitticanes","Gut Cassidies")

#not crazy familiar with r so unsure is this (or the above) is necessary as a place holder for the following
scores<-c("","","","","","","","","","")

#for information in the frame
for (w in 1:length(frame)){
  
  #modular operation to pull the name which follows score 
  if(w%%2==0){
    
    #find index of name which corresponds to jesse's convention and assign the score to the proper spot in the data frame
    ind = which(comp == frame[w])
    scores[ind] = frame[w-1]
  }
  
  w = w+1
  
}

#numeric for week 11 
wk_11_pts = as.numeric(scores)

#next stop make this all dynamic to avoid fat fingering

# weekly points scored
teams <- c(       'A',    'B',   'C',     'D',    'E',      'F',    'G',     'H',     'I',     'J')
team_owners <- c('Johnny','Ben','Jesse','Tilly','Mills','Brendan','Turbs','Parish','Cameron','Body')
wk_1_pts <- c(79.78,97.44,109.62,83.84,113.24,139.34,102.30,76.82,75.70,62.88)
wk_2_pts <- c(131.00,85.64,118.68,92.08,64.30,96.12,75.66,124.42,116.96,101.68)
wk_3_pts <- c(70.02,152.2,125.26,115.36,122.14,87.22,103.12,71.68,111.2,117.92)
wk_4_pts <- c(132.28,108.62,121.28,76.68,90.08,96.56,146.48,89,75.2,76.78)
wk_5_pts <- c(118.32,92.48,93.24,98.14,75.66,98.14,129.96,93.92,69.50,80.82)
wk_6_pts <- c(99.3,121.84,139.18,119.42,104.08,99.42,79.14,73.06,107.50,78.58)
wk_7_pts <- c(118.58,119.54,87.62,115.32,124.52,132.08,93.66,130.64,89.12,94.76)
wk_8_pts <- c(135.88,133.16,81.18,78.16,117.04,129.58,110.3,93.66,77.72,128.42)
wk_9_pts <- c(108.2,103.12,124.92,93.62,89.16,67.02,82.68,91.58,132.78,110.76)
wk_10_pts <- c(125.74,105.16,96.26,112.7,152.36,81.94,79.98,101.72,88.28,110.14)



wk_pts_mx <- t(matrix(wk_1_pts))
wk_pts <- data.frame( wk_pts_mx )
colnames(wk_pts) <- teams
wk_pts <- rbind (wk_pts , wk_2_pts, wk_3_pts, wk_4_pts, wk_5_pts, wk_6_pts, wk_7_pts, wk_8_pts, wk_9_pts, wk_10_pts, wk_11_pts)

#schedule
A <- c('J','H','C','F','I','G','E','D','B','J','H','C','F','I')
B <- c('I','G','E','D','F','J','H','C','A','I','G','E','D','F')
C <- c('H','F','A','I','G','E','D','B','J','H','F','A','I','G')
D <- c('G','E','F','B','J','H','C','A','I','G','E','F','B','J')
E <- c('F','D','B','J','H','C','A','I','G','F','D','B','J','H')
F <- c('E','C','D','A','B','I','J','G','H','E','C','D','A','B')
G <- c('D','B','J','H','C','A','I','F','E','D','B','J','H','C')
H <- c('C','A','I','G','E','D','B','J','F','C','A','I','G','E')
I <- c('B','J','H','C','A','F','G','E','D','B','J','H','C','A')
J <- c('A','I','G','E','D','B','F','H','C','A','I','G','E','D')

schedule <- data.frame(A,B,C,D,E,F,G,H,I,J)
# Could program a QC check that schedules are correct

# points per week loop 
wk_1_rank <- rank(wk_1_pts, ties.method = c("average"))
all_wk_rank <- t(matrix(wk_1_rank)) %>% data.frame()
z = 2
  for (z in 2:nrow(wk_pts)) {    nam <- paste("wk_",z,"_pts", sep = "") ; 
  all_wk_rank <- rbind(all_wk_rank, rank(get(nam), ties.method =  c("average")))
  }

# win/loss loop
winners <- matrix(0,  nrow = nrow(wk_pts), ncol = 10)
w = 1
y = 1 
#length(wk_pts)
for (w in 1:nrow(wk_pts)) {  
  for (y in 1:10 ) {  tm_score <- wk_pts[w,y];  
  opp <-     schedule[w,y] ;  
  opp_score <- wk_pts[w,paste(opp)] ; 
  winners[w,y] <- if (tm_score > opp_score) {1} else if (tm_score < opp_score) {0} else {0.5} 
  } }

# Standings points 
total <-  t(data.frame(colSums(winners) + colSums(all_wk_rank)/10))
colnames(total) <- team_owners
print(total)

# QC 
nrow(wk_pts)*10.5-sum(total[1,])


