##########################
# Loren Collingwood      #
# Week 3 -- Similarities #
# Text Reuse             #
##########################

#########################
#        Packages       #
#########################

# install.packages("quanteda")
library(quanteda)

#########################
# Set Working Directory #
#########################

setwd("~/Dropbox/collingwood_research/posc_fall_20/POSC-207/lecture")

####################################
# Read in the NYTimes DFM/DTM data #
####################################

nyt <- read.csv("smalldfm2.csv", header=T, stringsAsFactors = F)

#############################
# Convert data.frame to dfm #
# Same ordering as before...#
#############################

nyt <- as.dfm(nyt)

#####################
# Lexical Diversity #
#####################
# v / n
# where v = total number of unique types
# n = total number of tokens

head(textstat_lexdiv(nyt))

# Calculate lexical diversity 'by hand' #
table( ifelse(nyt[1,] >=1, 1,0) )[2] / 
sum(nyt[1,])

#####################
# Word Collocations #
#####################

clicks <- read.csv("Clicks.csv", header=T)

# Reduce the number of possible retweets for now #
clicks <- clicks[clicks$retweets_count < 1,]

# Convert to Corpus #
click_corp <- corpus(clicks$tweet)

textstat_collocations(click_corp, 
                      size =4, 
                      min_count=4)

###########################
##  Document similarity  ##
###########################

head(tsimil <- textstat_simil(nyt,
                    method = "cosine"))


# Take a look at two documents that score above .80 cosine similarity
load("nyt_muslim_ban_2017.RData")

nyt_muslim_ban_2017$text[11]
nyt_muslim_ban_2017$text[45]

#######################
## Document Distance ##
#######################

head(td <- textstat_dist(nyt, 
                        method = "euclidean"))

# Plot out dendrogram of similarities/differences #
plot(hclust(as.dist(td)))

###########
# Copycat #
###########

# Open used in policy diffusion #

# install.packages("textreuse")
library(textreuse)

# Put all your text files here #
dir <- "~/Dropbox/collingwood_research/posc_fall_20/POSC-207/data/test"

# Set up parallelization if needed #
options("mc.cores" = 7L)

# Read in the Text Files #
corpus <- TextReuseCorpus(dir = dir, meta = list(title = "Bills"),
                          tokenizer = tokenize_ngrams, 
                          n = 7,
                          keep_tokens=T)

# Meta stuffs #
meta(corpus)

# Look at first text #
head(tokens(corpus[[1]]))

# Word count per text document #
wordcount(corpus)

# Jaccard Similarity #
comparisons <- pairwise_compare(corpus, jaccard_similarity)

# Full table of pairwise comparisons #
pwc <- pairwise_candidates(comparisons)
pwc <- as.data.frame(pwc)

# Look at ALEC specifically #
pwc[pwc$b == "model_bill-VOTER_ID_ACT Exposed",]

# Ratio of Matches #
comparisons_rm <- pairwise_compare(corpus, ratio_of_matches, directional = T)
pwc <- pairwise_candidates(comparisons_rm)
pwc <- as.data.frame(pwc)
# Look at ALEC specifically #

pwc[pwc$b == "model_bill-VOTER_ID_ACT Exposed",]

nc <- ncol(comparisons_rm)
sort(comparisons_rm[,nc])

