#########################################
# Loren Collingwood                     #
# Scaling Text: Wordscores and Wordfish #
#########################################

# Set installtion repositories if having problems #
options(repos = c(CRAN = "http://cran.rstudio.com"))
options(max.print=1000000)

#############################
# Install and Load Packages #
#############################

#install.packages("quanteda")
library(quanteda)
#install.packages("quanteda.textmodels")
library(quanteda.textmodels)
#install.packages("readxl")
library(readxl)

# Step 1

# Gather the Corpus of text I've stored it locally in RDS file #
corp_ger <-  readRDS("~/Dropbox/collingwood_research/posc_fall_20/POSC-207/data/data_corpus_germanifestos.rds")
summary(corp_ger)

# Step 2
Convert the corpus to a document term/frequency matrix

# Create a Document-Feature/Term Matrix #
dfmat_ger <- dfm(corp_ger, remove = stopwords("de"), remove_punct = TRUE)

# Step 3
Apply Wordscores algorithm to document-feature matrix

tmod_ws <- textmodel_wordscores(dfmat_ger, y = corp_ger$ref_score, smooth = 1)
summary(tmod_ws)

# Step 4
Predict the Wordscores on the virgin text, then plot it out

pred_ws <- predict(tmod_ws, se.fit = TRUE, newdata = dfmat_ger)

# Plot it out real good #
textplot_scale1d(pred_ws)

Now try it out with  toy example. This will give you sort of funky but still 
somewhat interpretable results.

# Create a corpus
feaux_corp <- corpus(
    c("this is love",
    "hate is all i've got",
    "these losers suck so much",
    "love and like the dogs they're pretty",
    "mitt romney hates to vote that way he won't",
    "trump is a hater and loser, I hate him so much",
    "biden is a loser and hater, he just loses always",
    "harris will win she's the best omg, love harris ",
    "when you're young you're idealstic but that's not wrong",
    "politics is about doing what's right so really its an effort of love")
)

# Add on the toy scores #
docvars(feaux_corp, "ref_score") <- c(10, 1, 2, 8,NA, NA, NA, NA, NA, 9)

# Take  look real nice #
summary(feaux_corp)

# Create a Document-Feature/Term Matrix #
dfmat_feaux <- dfm(feaux_corp, 
                 remove = stopwords("english"), 
                 remove_punct = TRUE)

# Apply Wordscores algorithm to document-feature matrix
tmod_ws <- textmodel_wordscores(dfmat_feaux, y = feaux_corp$ref_score, smooth = 1)
summary(tmod_ws)

# Predict the Wordscores on the virgin text #
pred_ws <- predict(tmod_ws, se.fit = TRUE, newdata = dfmat_feaux)

# Plot it out real good #
textplot_scale1d(pred_ws)

####################
# Wordfish Scaling #
####################

# Step 1
Read in the data, this comes from media stories about homicide victims in  
Chicago in 2014 during the months of August and September (or so).

# Read in Data #
nc <- read_xlsx("~/Dropbox/collingwood_research/posc_fall_20/POSC-207/data/news_coverage_WordfishReady.xlsx", 
                sheet = 1)

# Relabel column 3 #
colnames(nc)[3] <- "victim_text"

# Step 2
Turn data into corpus then document frequency/term matrix 

# Turn text into corpus #
vcorpus <- corpus(nc$victim_text)
head(summary(vcorpus))

vdfm <- dfm(vcorpus, stem=T, 
            remove_numbers=T,
            remove_punct=T, 
            remove = stopwords("english"))
# Look at top set of rows
vdfm

# Step 3

Estimate a Wordfish model but before you do you need to identify documents that 
are polar on the dimension of interest. A priori here I had identified document 
27 and 11, respectivly.

# Look at 27
head(vcorpus[[27]])

# Look at 11
vcorpus[[11]])

# Wordfish Model #
wf <- textmodel_wordfish(vdfm, 
                dir=c(27,11))# directional command -- want global identification 
                             # so that document 2 receives lower value than 
                             # document 1

# Take a look at the summary #
summary(wf)

# Store the theta document estimates and se's #
sumwf <- summary(wf)$estimated.document.positions

# Merge the scores and the text together (real good) #
text_scaling <- data.frame(sumwf, nc$victim_text)
colnames(text_scaling)[3] <- "victim_text"

# Sort the data frame (nice and good) #
text_final <- text_scaling[order(text_scaling[["theta"]]),]

# Take a look at the distribution #
hist(text_final$theta)

# Look at the distribution more formally
textplot_scale1d(wf)

# Then look at the words on either end that pop out
textplot_scale1d(wf, margin = "features")

