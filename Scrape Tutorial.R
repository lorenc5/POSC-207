setwd("D:/Dropbox/Public/Sean's Stuff/Grad School/Research/Scrape Tutorial") #set WD
library("rvest")
library(purrr)
library(plyr)
library(dplyr) #loading packages


##### Create Page Holder
page_holder <- rep(NA, 10)
for (i in 1:length(page_holder)){
  page_holder[i]  <-(paste("https://www.politico.com/news/2020-elections",i))
}
page_holder <- gsub("s ", "s/",page_holder) #minor adjustment to get formatting working


####### Create Functions ###
URLFunction <- function(url){
  Sys.sleep(1)
  read_html(url) %>%
    html_nodes("#main h1 a") %>%
    html_attr('href')
}

HeadlineFunction <- function(url){
  Sys.sleep(1)
  as.character(html_text(html_nodes(read_html(url), '#main h1 a')))
}
AuthorFunction <- function(url){
  Sys.sleep(1)
  as.character(html_text(html_nodes(read_html(url),'.story-meta__authors .vcard')))
}
DateFunction <- function(url){
  Sys.sleep(1)
  as.character(html_text(html_nodes(read_html(url),'time')))
}

TextFunction <- function(url){
  Sys.sleep(1)
  (html_text(html_nodes(read_html(url), '.story-text__paragraph')))
}

##### Scraping #####

PoliticoURL <- sapply(page_holder, URLFunction) #Apply URL Function, will take some time
PoliticoURL <- URLFunction(page_holder[1])

# PoliticoHeadline <- sapply(page_holder, HeadlineFunction) #Apply Headline function
# PoliticoHeadline <- as.character(PoliticoHeadline) #put into one column
PoliticoHeadline <- HeadlineFunction(page_holder[1])

Database <- as.data.frame(cbind(PoliticoHeadline, PoliticoURL)) #if you want to make sure they match up

Dates  <- rep(NA,length(Database$PoliticoHeadline)) #we are going from actual articles now, so using a loop to deal with formatting errors/lag
for(i in 1:length(Dates)){
  Dates[i] <- DateFunction(PoliticoURL[i])
  print(i)
}
saveRDS(Dates, "Dates.rds")

Author  <- rep(NA,length(Database$PoliticoHeadline))
for(i in 1:length(Author)){
  Author[i] <- AuthorFunction(PoliticoURL[i])
  print(i)
}
saveRDS(Author, "Author.rds")

Database$Date <- Dates
Database$Author <- Author

###### Scraping the Text ###
setwd("D:/Dropbox/Public/Sean's Stuff/Grad School/Research/Scrape Tutorial/Text")

for (i in 1:length(Database$PoliticoURL)){
  text_holder <- NA
  text_holder <- TextFunction(Database$PoliticoURL[i])
  output <- paste0(i, ".txt") #create a name for each
  write.csv(text_holder, output) #saves file
  rm(text_holder)
  print(i)
}

#### Bringing Text Back In ###
library(readr)
setwd("D:/Dropbox/Public/Sean's Stuff/Grad School/Research/Scrape Tutorial/Text")
file_list <- rep(NA, 20)
x <- 0
for (i in 1:length(file_list)){
  x <- 0 + i
  file_list[i] <- paste(x, ".txt") #creating list of the file names that we exported
}
file_list <- gsub(" .txt", ".txt", file_list)

for (file in file_list){
  if(exists("dataset")){
    temp_dataset <- read_file(file)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
    print(file)
  }
  if(!exists("dataset")){
    dataset <- read_file(file)
  }}

Database$Text <- dataset
