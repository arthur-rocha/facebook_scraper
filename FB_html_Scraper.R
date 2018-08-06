#######FB html Scraper#############################################
## 1- Save page as .html
## 2- Run the function
## 3- Be happy with your wordcloud


f=choose.files()  #load the .html


FB_Scraper=function(f,min_freq=5){  ##choose the min_freq to plot(wordcloud)
  ##packages
  library(dplyr)    ##data manipulation
  library(qdapRegex)##deal with strings
  library(stringi) ##deal with strings (faster)
  library(tm.plugin.webmining) ## web mining
  library(wordcloud) #wordcloud
  library(extrafont) #fonts
  
  info=paste(readLines(f,encoding = "UTF-8"), collapse=" ") #read as character
  
  #Posts information
  
  cleaner=function(information){
    tam= stringi::stri_length(information)
    information2=information %>%
      extractHTMLStrip(encoding = "UTF-8") %>%  #remove html tags
      gsub(pattern = '[[:punct:] ]+',replacement = ' ')%>% #remove punctuation
      substr(start = 8,tam) %>%   #remove code
      gsub(pattern = "\\d",replacement =  "") %>% ##remover numbers
      tolower() #lowercase
    
    return(information2)
    
  }
  
  posts=rm_between(info, 'data-ft="{&quot;tn&quot;:&quot;K&quot;}" id="',
                    '</p></div>',
                    extract=TRUE) %>%unlist()
  pfinal=sapply(posts,FUN = cleaner)  
  
  ############################## Mining --> wordcloud##################
  ##-----------------stopwords (remove)
  stopbr=readLines("https://gist.githubusercontent.com/alopes/5358189/raw/2107d809cca6b83ce3d8e04dbd9463283025284f/stopwords.txt",encoding = "UTF-8")
  stopbr= stopbr %>% gsub(pattern = " ",replacement = "")
  stopbr= c(stopbr,"","vc","v","br",".","www")
  ##------------------------------------------------------------------
  
  `%nin%` = Negate(`%in%`)   ## negate %in%
  
  
  #remove accents and split data to vector
  mining_split=function(vec){
    
    palav=vec %>% gsub(pattern = "ç",replacement =  "c", perl = TRUE) %>%
      gsub(pattern = "é",replacement =  "e", perl = TRUE)%>%
      gsub(pattern = "ê",replacement =  "e", perl = TRUE) %>%
      gsub(pattern = "í",replacement =  "i", perl = TRUE) %>%
      gsub(pattern = "ã",replacement =  "a", perl = TRUE) %>%
      gsub(pattern = "â",replacement =  "a", perl = TRUE) %>%
      gsub(pattern = "á",replacement =  "a", perl = TRUE) %>%
      gsub(pattern = "à",replacement =  "a", perl = TRUE) %>%
      gsub(pattern = "õ",replacement =  "o", perl = TRUE) %>%
      gsub(pattern = "ó",replacement =  "o", perl = TRUE) 
    
    palav=stri_split(palav,regex = " ") %>% unlist() 
    return(palav[palav %nin% stopbr])
    
  }
  
  #apply the function
  word_data= sapply(pfinal,FUN = mining_split) %>% unlist()
  
  #custom palette
  colors_pal <- colorRampPalette(c("grey23", "#ffca00"))
  
  #wordcloud
  wordcloud(word_data,scale = c(2,.1),
            min.freq = min_freq,random.order = F,random.color = F,
            colors = colors_pal(4),use.r.layout = F,
            family = "Impact")
  
}

