# Example code for webscraping using Selector Gadget
baseballurl<-
  "https://gopoly.com/sports/baseball/roster"
baseballlink <- read_html(baseballurl)
baseballhtml <- html_nodes(baseballlink, css=".sidearm-roster-player-name a")
baseballtext <- html_text(baseballhtml)[1:36]

height <- baseballurl %>% 
  read_html() %>% 
  html_nodes(css=".sidearm-roster-player-height") %>% 
  html_text()


height <- gsub('\"', "", height, fixed = TRUE)[1:36]
baseballheights <- data.frame(player = baseballtext, heights = height)
baseballheights
