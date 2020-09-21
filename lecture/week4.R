# Loren Collingwood #
# Week 4, Dictionary Methods #

options(scipen = 999, digits = 4)

########################
#       Packages       #
########################

library(quanteda)
#install.packages("descr")
library(descr)
#install.packages("ggplot2)
library(ggplot2)

#########################
# Set Working Directory #
#########################

setwd("~/Dropbox/collingwood_research/posc_fall_20/POSC-207/lecture")

# Step 1 
Use the lexicoder dictionary of negativity and positivity, first read in 
the Clicks4Kass dataset.

# Read in the #Clicks4Kass Corpus #
clicks <- read.csv("Clicks.csv", header=T)

# Reduce the number of possible retweets for now #
clicks <- clicks[clicks$retweets_count < 1,]

# Convert to Corpus #
click_corp <- corpus(clicks$tweet)

# Step 2 
Take a look at the tokens, what is getting converted. Do this to ensure that what 
you are doing has face validity, etc.

# look at tokens, nicely #
tok_look <- tokens_lookup(tokens(click_corp), 
                   dictionary = data_dictionary_LSD2015,
                   exclusive = FALSE, 
                   nested_scope = "dictionary")
tok_look[[3]]

# Step 3
Deal with compounds negative negatives and negative positives so you can then subtract 
that later (e.g., the biscuits are NOT good)

# Compound neg_negative and neg_positive tokens before creating a dfm object
toks <- tokens_compound(tokens(click_corp), 
                        data_dictionary_LSD2015)

# Step 4
Generate a document term matrix that is just based on sentiment scores 

# Create the DFM #
c_dfm <- dfm_lookup(dfm(toks), 
                    data_dictionary_LSD2015)

# Convert the Document Term Matrix to Sentiment data.frame() object #
sent_data <- convert(c_dfm, "data.frame")

# Step 5
Do some addition and subtraction, and also add on user name from corpus 

sent_data$pos_final <- with(sent_data, positive - neg_positive)
sent_data$neg_final <- with(sent_data, negative - neg_negative)
sent_data$pos_min_neg <- with(sent_data, pos_final - neg_final)
sent_data$username <- clicks$username

# Step 6
Look at sentiment by user, or any other grouping of interest

out <- as.data.frame(compmeans(sent_data$pos_min_neg, sent_data$username))

# Order it real good #
out <- out[order(out$Mean),]
out

################################################################
# Using the tidytext and textdata package to analyze sentiment #
################################################################

#install.packages("tidytext")
#install.packages("textdata")

library(tidytext); library(textdata)

nrc <- get_sentiments("nrc")

# Look at the sentiment-type words #
table(nrc$sentiment)

# Plotting it out #
tidy_kass_tweets<- clicks %>%
    select(id, date, user_id, tweet) %>%
    unnest_tokens("word", tweet)

# Negative #
kass_sentiment_plot <-
    tidy_kass_tweets %>%
    inner_join(get_sentiments("nrc")) %>% 
    filter(sentiment=="negative") %>%
    count(date, sentiment)

# Positive #
kass_sentiment_plot_pos <-
    tidy_kass_tweets %>%
    inner_join(get_sentiments("nrc")) %>% 
    filter(sentiment=="positive") %>%
    count(date, sentiment)

# Joy #
kass_sentiment_plot_joy <-
    tidy_kass_tweets %>%
    inner_join(get_sentiments("nrc")) %>% 
    filter(sentiment=="joy") %>%
    count(date, sentiment)

# Plotting with ggplot #
ggplot() +
       aes(x=kass_sentiment_plot$date, 
           y=kass_sentiment_plot$n, group = 1)+
    geom_line(color="red")+
    theme_minimal()+
    ylab("Frequency of Word Usage in #Clicks4Kass's Tweets") +
    xlab("Date") + 
    geom_line(aes(x=kass_sentiment_plot_pos$date,
                  y = kass_sentiment_plot_pos$n, group = 1),
              color='blue') +
    geom_line(aes(x=kass_sentiment_plot_joy$date,
              y = kass_sentiment_plot_joy$n, group = 1),
          color='brown')




