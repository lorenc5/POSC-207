###########################
# LOREN COLLINGWOOD       #
# Structured Topic Models #
# Unsupervised Learning   #
###########################

#################
#    Packages   #
#################
#install.packages("stm")
library(stm)

#install.packages("wordcloud")
library(wordcloud)

#################
# Set Directory #
#################

setwd("~/Dropbox/collingwood_research/posc_fall_20/POSC-207/data"); list.files()

# Read in Data #
data <- read.csv("poliblogs2008.csv")

# Check Dimensions; n=13,246
dim(data)

# Look at column names, and you can see the metadata (we will use rting and day)
colnames(data)

# Process the text real good #
processed <- textProcessor(data$documents, 
                           metadata = data)

# Prep the Documents too #
out <- prepDocuments(processed$documents, 
                     processed$vocab, 
                     processed$meta)

# Remove more Terms                           
# This plot is hard to understand, good luck! #
plotRemoved(processed$documents, 
            lower.thresh = seq(1, 200, by = 100))

# Prep the Documents some More then... #
out <- prepDocuments(processed$documents, 
                     processed$vocab,
                     processed$meta, 
                     lower.thresh = 15)

# Estimate Model -- in this case we estimate a K = 20 topic, topic model #
# The prevalence covariates act as additional document-specific covariates 
poliblogPrevFit <- stm(out$documents, 
                       out$vocab, 
                       K=20, 
                       prevalence=~rating+s(day), 
                       max.em.its=75, 
                       data=out$meta, 
                       init.type="Spectral", 
                       seed=8458159)

# Plot out a short summary of the topics and the proportions of each
plot(poliblogPrevFit, 
     type="summary", 
     xlim=c(0,.4))

# PLot more in depth with more 'top' words #
plot(poliblogPrevFit, 
     type="labels", 
     topics=c(1,6,13,18))

# Plot out Topic Distribution (i.e., Topic 2, 6, 7, 15 most prevalent)
plot(poliblogPrevFit, type="hist")

# Plot to see how two topics are discussed differently #
plot(poliblogPrevFit, type="perspectives", topics=c(2,13))

# Step XX
Selecting the Appropriate Number of Topics. Note this is the inevitable question
and is not answerable. The manyTopics() function lets you estimate K topics at 
a time then take a look to what may work best. The searchK() function, with 
argument K=0 will estimate the best fitting topic model based on a mathematical 
criterion; however, in my experience the estimated number of topics is large and 
often generates many useless topics.

# Topic K Selection; Estimate and then look at, this can take a while  #
storage <- manyTopics(out$documents, 
                      out$vocab, 
                      K=c(5,10,15,20,25), 
                      prevalence=~rating+s(day),
                      data=meta, 
                      runs=5)

storageOutput1 <- storage$out[[1]] # For example, choosing the model with 5 topics

# Then plot it out real nice #
plot(storageOutput1)

# Lee and Mimno (2014) Topic Selection strategy #
# Set to 0 and you can then get a guess as to the number of topics using 
storage2 <- searchK(out$documents, 
                   out$vocab, 
                   K = 0,
                   prevalence =~ rating + s(day), 
                   data = meta)

#########################################################
# Further Interpretation: Print out top words per topic #
#########################################################

# Look at the top words for each Topic #
labelTopics(poliblogPrevFit)

# Thoughts: Find the documents 'most' related to the topic #
thoughts1 <- findThoughts(poliblogPrevFit, 
                          texts = data$documents,
                          n = 4, 
                          topics = c(1, 2, 5))

######################################################################
# Set up a plot for like a paper to convey the key text of the topic #
######################################################################

two_sent <- c("A new set of Rasmussen polls, all conducted yesterday in the 
              middle of John McCain's post-convention bounce, suggests that this 
              race remains close on the state-by-state level",
              "Here's our final daily composite of the six major national 
              tracking polls. Many national polls have coalesced around a 
              projected 52% popular-vote share for Barack Obama, and it shows in 
              our composite")

plotQuote(two_sent, 
          width =30, 
          main = "Topic 1: 2008 Polling Horse Race")

############################################################
# Estimating Metadata Covariate Effects on Topic Selection #
############################################################

# Convert Rating into a Factor #
out$meta$rating <- as.factor(out$meta$rating)

prep <- estimateEffect(1:20 ~ rating , poliblogPrevFit,
                          meta = out$meta, uncertainty = "Global")

# Look at the effects of the metadata 'rating' variable on influence on each topic
summary(prep)

# Plot out an Effects Plot the rating variable has on probability of each topic
plot(prep, 
     covariate="rating", 
     topics = c(1, 2,5), 
     method="difference",
     model = poliblogPrevFit,
     cov.value1 = "Liberal", 
     cov.value2 = "Conservative",
     xlab="More Conservative ... More Liberal", 
     main="Effect of Liberal vs. Conservative",
     xlim = c(-.20, .20),
     labeltype ="custom", 
     custom.labels=c('Topic 1:Polling', 'Topic 2: Dumping on GOP', 'Topic 5: Senate')
     )

# Look at inter topic correlation #
mod.out.corr <- topicCorr(poliblogPrevFit)

head(mod.out.corr$cor)

# For example Topic 1 and 19 are correlated more than most correlations.
# Plot it out real nice #
# But I then also go and look at some articles from Topic 1 and 19 using the 
# findThoughts() function above to make sense of the correlation...

plot(mod.out.corr)

#########################################################
# Always gotta present a Wordcloud to impress everyone! #
#########################################################

cloud(poliblogPrevFit, topic=1)

# Content Plot -- how 'libs' and 'conservs' use words for a particular topic #
# Re-estimate STM now add in a content argument 

poliblogContent <- stm(out$documents, 
                       out$vocab, 
                       K=20, 
                       prevalence=~rating+s(day), 
                       content=~rating, 
                       max.em.its=75, 
                       data=out$meta, 
                       init.type="Spectral", 
                       seed=8458159)

# Plot out how liberal/conservative blogs speak about same topic #
plot(poliblogContent, 
     type="perspectives", 
     topics=2,
     main = "Dumping on GOP")


