library(tabulizer)
library(tidyverse)
library(rvest)
library(RODBC)

setwd("~/f1/Practice/2018")

cal <- read_html("https://www.fia.com/events/fia-formula-one-world-championship/season-2018/formula-one") %>%
  html_nodes(".event-item.past") %>% html_nodes(".event.cell") %>% html_nodes("a") %>% html_attr("href")

cal

laptimes <- data.frame()

for(k in 1:length(cal)){
  
  dir <- str_replace(str_split(cal[k],"/")[[1]][length(str_split(cal[k],"/")[[1]])],"-0","")
  
  dir.create(dir)
  
  url <- paste0("https://www.fia.com",cal[k])
  
  page1 <- read_html(url) %>% html_nodes("a") %>% html_attr("href") %>% tibble() %>% set_names("link") %>%
    filter(str_detect(link, "eventtiming"))
  
  page <- read_html(paste0("https://www.fia.com",page1[[1]][1])) %>% html_nodes(".for-documents") %>% html_nodes("a") %>% html_attr("href")
  
  files <- page[str_detect(page,"practicesessionlaptimes")]
  
  for(l in 1:length(files)){
    
    filename <- paste0(dir, "/p", 4-l, ".pdf")
    
    download.file(paste0("https://www.fia.com",files[l]), filename, mode = "wb")
    
    
    out4 <- extract_tables(filename, guess = F, columns = list(c(0, width/3, (2*width)/3)), 
                           area = list(c(176,0,750,width)),
                           outcome = "data.frame")
    
    
    outputs1 <- data.frame()
    
    for(i in 1:length(out4)){
      outputs1 <- bind_rows(outputs1, out4[[i]] %>% data.frame())
    }
    
    outputs2 <- data.frame(x = c(outputs1[,2], outputs1[,3], outputs1[,4])) %>% 
      mutate(nd = ifelse(x == "NO TIME NO TIME", row_number(), NA))
    
    index <- unique(na.omit(outputs2$nd))
    
    for(i in 1:length(index)){
      
      rn1 <- index[i] - 1
      rn2 <- index[i] + 1
      rn3 <- ifelse(i < length(index), index[i+1] - 2, nrow(outputs2))
      
      drivername <- outputs2[rn1,"x"] %>% str_remove("\\d+") %>% trimws()
      
      a <- outputs2[rn2:rn3,] %>% filter(x != "")
      
      if(nrow(a) > 0){
        
        laps <- data.frame(t(data.frame(strsplit(a[,"x"], "\\s\\d+\\s"))))
        if(nrow(laps) > 1){
          laps <- laps %>% mutate(X2 = ifelse(X2 == X1, NA, X2))
        }
        laps2 <- data.frame(laptime = c(laps[1:nrow(laps),1], laps[1:nrow(laps),2]))
        laptimes <- bind_rows(laptimes, laps2 %>% transmute(gp = dir,
                                                            session = 4-l,
                                                            driver = drivername,
                                                            lapno = row_number(),
                                                            is_p = str_detect(laptime, "P"),
                                                            laptime = str_extract(laptime, "\\d+:\\d+.\\d+")
        ) %>% na.omit()
        )
        
      }
    }
    
    
  }
  
  
  
}

