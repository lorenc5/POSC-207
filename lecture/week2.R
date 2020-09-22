######################
# Loren Collingwood  #
# Week 2             #
# Text Preprocessing #
######################

############
# Packages #
############

#install.packages("dplyr")
library(dplyr)

#install.packages("quanteda")
library(quanteda)

#################
# Set Directory #
#################
# Swap this out with your directory (where you stored the NYT muslim ban rdata) #
setwd("~/Dropbox/collingwood_research/posc_fall_20/POSC-207/lecture/")

# Load Data #
load("nyt_muslim_ban_2017.RData")

head(nyt_muslim_ban_2017)

# Subset to relevnt columns #

nyts <- dplyr::select(nyt_muslim_ban_2017, response.docs.pub_date, 
                      response.docs.headline.main, 
                      response.docs.web_url, 
                      response.docs.byline.original,
                      response.docs.section_name,
                      response.docs.word_count,
                      text)

##############################
# Convert Text into a corpus #
##############################

nyt_corpus <- corpus(nyts$text)

# Look at the texts in the corpus; in this case text document 3

texts(nyt_corpus)[3]

####################################################
# Create new docvar onto the data.frame summary()  #
####################################################

docvars(nyt_corpus, "headline") <- nyts$response.docs.headline.main
docvars(nyt_corpus, "date") <- as.Date(nyts$response.docs.pub_date)
docvars(nyt_corpus, "author") <- nyts$response.docs.byline.original

# Look at Number of: Types, Tokens, and Sentences #
# Look at top 6 documents                         #
head(summary(nyt_corpus))

# Subset corpus to keep only articles by Adam Liptak #
adam_corp <- subset(nyt_corpus, nyt_corpus$author=="By Adam Liptak")
summary(adam_corp)

# Tokenize words then sentences #

#tokenize_word(nyt_corpus)
tokenize_sentence(nyt_corpus[1])

##################################
# Document Frequency/Term Matrix #
##################################

# Convert text to lower, remove stopwords, stem words, and remove punctution #
# Rows are the 'Documents', columns are 'features'
nyt_dfm <- dfm(nyt_corpus,
               tolower=T,
               remove = stopwords("english"),
               stem = T,
               verbose = T,
               remove_punct = T
               )

featfreq(nyt_dfm)

############################################
# Trim the document -- 20, 5 are arbitrary #
############################################

smalldfm <- dfm_trim(nyt_dfm,minCount=20,minDoc=5)
smalldfm <- dfm_trim(nyt_dfm, sparsity = 0.8)
smalldfm

# Look at all the words that made it in #
head (featnames(smalldfm))

# Look at the top features real good #
topfeatures(smalldfm, n = 100)

# Further drop out words that don't tell us much #
smalldfm2 <- dfm_select(smalldfm, 
                       pattern = c("said", "mr", "advertis", "also", "like",
                                   "say", "call", "now", "can", "ms", "make", 
                                   "just", "may", "go", "ask", "use", "way", 
                                   "still", "wrote", "week", "want", "sinc", 
                                   "parti", "ad", "version", "argu", "made",
                                   "alreadi", "whether", "might", "came", "see",
                                   "need", "set", "tri", "accord", "show", 
                                   "yet", "well", "anoth", "must", "mean", "put",
                                   "turn", "refer", "previous", "becom", "decid",
                                   "rather", "expect", "good", "part", "effort",
                                   "consid", "often", "today", "mani"),
                       selection= "remove")

topfeatures(smalldfm2, n = 50)

#dfm_out <- convert(smalldfm2, "data.frame")
#write.csv(dfm_out, "smalldfm2.csv", row.names = F)

################################
# Feature Co-Occurrence Matrix #
################################

nyt_co <- fcm(smalldfm2,
              ordered=T)

# Convert this to  data frame #
nyt_dat <- convert(nyt_co, to = "data.frame")

muslim_vec <- nyt_dat[3,]
muslim_vec <- muslim_vec[-1]

# top 50 co-occuring words with muslim
rev(sort(muslim_vec))[1:50]

########################
### Tf-idf weighting ###
########################

# The higher the number the more unique that word is to that document #

tf.idf <- dfm_tfidf(smalldfm2,
                    scheme_tf = 'prop')

# Term Frequency: Frequency of times word shows up in document divided by 
# total number of words in the document

docfreq(smalldfm2)
doc_df <- convert(smalldfm2, "data.frame")

# Let's just look at the first textual document #
doc1 <- doc_df[1,]

# Drop the first doc_id entity
doc1 <- doc1[-1]

# Calculate tf #
tf <- doc_df$govern[1]/sum(doc1)

# Calculate idf #
n <- nrow(doc_df)
s <- table( ifelse(doc_df$govern >= 1, 1, 0) )[2]
idf <- log10(n/s)

# Calculate tf-idf #
tf*idf


