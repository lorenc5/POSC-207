## Loren Collingwood ##
## Scraping Twitter  ##

# install.packages("rtweet")
library(rtweet)

# searches hash tags last 6-9 days or so...
rstats <- search_tweets(q="Clicks4Kass"); dim(rstats)

# Search Kassra's timeline #
ko_time <- get_timeline("kassrao", n =3000)

# Dimensions #
dim(ko_time)

# convert text to lower
tex <- data.frame(kass_text = tolower(ko_time$text))

# Create dummy indicator
tex$clicks <- ifelse(grepl("clicks4kass", tex$kass_text)==TRUE, 1, 0)

# Print to Console #
tex$kass_text[tex$clicks==1]

###################################################
# Running with Python to get full hashtag history #
###################################################

# User needs to:
# Install python3
# install package twint and pip
# Variation may exist for windows vs. mac

# Set Directory to where py_tweet.R is located #
setwd("~/Dropbox/collingwood_research/posc_fall_20/POSC-207/lecture"); list.files()

source("py_tweet.R")

# Execute Function #
py_tweet(  
    search = "'#Clicks4Kass'", 
    until = "'2020-09-09'",
    since = "'2007-01-01'",
    limit = 10000000, 
    output = "'Clicks.csv'",
    pfile = "twitter_hist.py"   
)

# Read back in #
clicks_all <- read.csv("Clicks.csv", header=T)

# Check Dimensions #
dim(clicks_all)

# Convert Text to Lower #
clicks_all$tweet <- tolower(clicks_all$tweet)

# Validate that the scraper mostly worked #
table(grepl("clicks4kass", clicks_all$tweet))

# Look at the 'FALSE' tweets #
clicks_all[!grepl("clicks4kass", clicks_all$tweet), "tweet"]

# Subset out the 'FALSE' tweets #
clicks_all <- clicks_all[grepl("clicks4kass", clicks_all$tweet),]
dim(clicks_all)

# Further Subset Columns

clicks_all <- dplyr::select(clicks_all, date, username, name, 
                            place, tweet, photos, replies_count, 
                            retweets_count, likes_count)

# Clean the Text #
clean_string <- function(string){
    
    # Lowercase
    temp <- tolower(string)
    
    # Remove everything that is not a number or letter (may want to keep more 
    # stuff in your actual analyses). 
    temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
    
    # Shrink down to just one white space
    temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
    
    # Clean front/back whitespace
    temp <- stringr::str_trim(temp)
    
    return(temp)
}

# Apply across the character tweet vector #
clicks_all$tweets_c <- sapply(clicks_all$tweet, 
                              FUN = clean_string, 
                              USE.NAMES = F)


# Some Basic Analysis #

summary ( lm(likes_count ~ factor(username), data = clicks_all) )

summary ( lm(retweets_count ~ factor(username), data = clicks_all) )

#######################
library(quanteda)
#######################

a <- corpus(clicks_all$tweet)

kwic(corpus(clicks_all$tweet), 
     pattern = phrase("blue hens"), 
     window=10, 
     valuetype = "fixed")

kwic(corpus(clicks_all$tweet), 
     pattern = "kass", 
     window=10, 
     valuetype = "fixed")

kwic(corpus(clicks_all$tweet), 
     pattern = "muslim", 
     window=10, 
     valuetype = "fixed")

kwic(corpus(clicks_all$tweet), 
     pattern = "discrimination", 
     window=10, 
     valuetype = "fixed")



# Who is trolling the hardest #
sort ( table(clicks_all$username) )

table(clicks_all$replies_count)
table(clicks_all$retweets_count)

# See the number of 'Blue Hen' References

grepl("blue hen", clicks_all$tweet)

# Subset to lots of likes #
clicks_all[clicks_all$likes_count>30, c("tweet", "likes_count","username") ]

# Fix Date to be date #
clicks_all$date <- as.Date(clicks_all$date)
hist(clicks_all$date, breaks=10)

###################################
# PRIEC Conference Hashtag Search #
#################

py_tweet(  
    search = "'#priec'", 
    until = "'2020-09-09'",
    since = "'2016-01-01'",
    limit = 10000000, 
    output = "'priec.csv'",
    pfile = "twitter_hist_priec.py",
    remove=T
)

# Read back in #
priec <- read.csv("priec.csv", header=T)
table(priec$username)


