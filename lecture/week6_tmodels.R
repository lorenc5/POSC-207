######################################
# Loren Collingwood                  #
# Topic Modeling                     #
# Marijuana Topics News coverage     #
######################################

rm(list=ls())

#install.packages(c("topicmodels", "tm")
library(quanteda)
library(topicmodels)
library(tm)
library(descr)

#################
# Set Directory #
#################

setwd("~/Dropbox/collingwood_research/posc_fall_20/POSC-207/data"); list.files()

############################
# Loading & Pre-processing #
############################

load("mj_nyt.Rdata") #loads "final_out" object, which is the dataframe object from above loop
objects()

dim(final_out)

########################
# Convert to Dataframe #
########################

final_out <- as.data.frame(final_out) 

##############################
# Take Random Sample for now #
# (ease of computing)        #
##############################

set.seed(492847)
samp <- sample(1:nrow(final_out), 3000)
final_out <- final_out[samp,]

######################################
# Subset to relevant items (for now) #
######################################

final_qe <- final_out[,c("uniq_id", "year", "texts")]
dim(final_qe)

#############################
# Text Mining Create Corpus #
#############################

nyt_corpus <- corpus(final_qe, text_field="texts")

############################
# Label Document Variables #
############################

docnames(nyt_corpus) <- final_qe$uniq_id
head(summary(nyt_corpus))

###############################
# Remove Non-alpha Characters #
###############################

nyt_corpus <- tokens(nyt_corpus, 
                     remove_punct=T, 
                     remove_numbers = T)

#########################################
# Create Document Term/Frequency Matrix #
#########################################

nyt_dfm <- dfm(nyt_corpus, 
               stem=T, 
               remove= stopwords("english"))

###########################
# Look at top 20 features #
###########################

topfeatures(nyt_dfm, n=20)

#################################
# Trim Matrix Based on Sparsity #
#################################

smalldfm <- dfm_trim(nyt_dfm,sparsity=.991)
smalldfm
topfeatures(smalldfm, n=20)

#########################################
# Convert to Matrix so can remove words #
#########################################

sdfm_mat <- as.matrix(smalldfm) # Turn into matrix format for easier access

#############################################################################
# Remove Words that don't tell us anything in Topic Model, but are frequent #
#############################################################################

remove <- c("play", "mr", "new", "show", "like", "first", "one",
            "will", "said", "say","two", "home", "get","go", "just","want",
            "use", "peopl", "think", "know", "time", "can", "make", "way", "thing", "now",
            "even", "place", "around", "ms", "includ", "charact", "also",
            "man", "ask", "come", "look", "back", "work", "see", "seem", "got", "day",
            "year", "call", "plan", "open", "room" ,"water", "men","last", "good",
            "never","us","talk", "much", "take", "road", "live", "s", "someth", "still",
            "lot", "tell", "s", "word", "well", "mani", "along", "told", "went", "tri",
            "live")

######################
# Remove those words #
######################

sdfm_mat <- sdfm_mat[,!colnames(sdfm_mat) %in% remove]

#############################################################
# Clear out text with none of the words, after the Trimming #
#############################################################

zeros <- apply(sdfm_mat, 1, function(x) ifelse(sum(x) == 0, FALSE, TRUE))
sdfm_mat <- sdfm_mat[zeros,] # Regular Matrix but seems to work with LDA
dim(sdfm_mat)

#####################################################
# Calculate marijuana usage in text, for subsetting #
#####################################################

smalldfm <- as.data.frame(sdfm_mat)
table(smalldfm$marijuana)

###############################################################
# Take only texts with word marijuana appearing at least once #
###############################################################

smalldfm <- smalldfm[smalldfm$marijuana > 0 ,]
dim(smalldfm)

##########################
# Convert Back to Matrix #
##########################

sdfm_mat <- as.matrix(smalldfm) 
dim(sdfm_mat)

############################################
# Set up Parameters for LDA/Gibbs Sampling #
# 15 Topic Model 						               #
############################################

burnin = 1000
iter = 1000
keep = 50
thin <- 500
k <- 15
alpha <- 1/k # This improves the probability separation; very important
seed <- 48790 # Seed for replication #

##################
# Estimate Model #
##################
# may take some time to run #

fitted <- LDA(sdfm_mat, k = k, method = "Gibbs",
              control = list(alpha=alpha, burnin = burnin,
                             iter = iter, keep = keep, seed=seed) ) 
##########
# Assess #
##########

get_terms(fitted, k=10)

##############################
# Gather Topic Probabilities #
##############################

topicProbabilities <- as.data.frame(fitted@gamma)

topProb <- apply(topicProbabilities, 1, max)
hist(topProb) # Decent

########################
# Extract Topics, etc. #
########################

ldaOut.topics <- as.data.frame(as.matrix(topics(fitted)))
ldaOut.topics$uniq_id <- row.names(ldaOut.topics)
colnames(ldaOut.topics)[1] <- "topic_15"
ldaOut.terms <- as.matrix(terms(fitted,6))

#######################################
# Merge Topic Model with Exist Datas  #
#######################################

final_out <- merge(final_out, ldaOut.topics, by.x="uniq_id", by.y="uniq_id", all.x=T)

table(final_out$topic_15)

###########################
# Get Proportions by Year #
###########################

tabs <- CrossTable(final_out$year, final_out$topic_15, prop.r=T, prop.c=F, prop.t=F, prop.chisq = F)$prop.row

# Clean #
tabs <- tabs[row.names(tabs)!="2007",]

###########################
#     Initiate Plot       #
###########################

plot(row.names(tabs), tabs[,5], type="n", ylim=c(0,.17), bty="n", lwd=3, # Legalization/medicinal
     ylab="Topic Percent of all articles",
     xlab= "Year",
     main = "Marijuana Newspaper Topic Model Across Time\n(NYT marijuana-related articles)")
lines(lowess(row.names(tabs), tabs[,7]), lty=1, lwd=3, col="blue") # Mexican Drug/Border
lines(lowess(row.names(tabs), tabs[,4]), lty=2, lwd=3, col="red") # Addiction
lines(lowess(row.names(tabs), tabs[,14]), lty=3, lwd=3, col="pink") # Courts
lines(lowess(row.names(tabs), tabs[,2]), lty=4, lwd=3, col="black") # Police/shooting/murder
lines(lowess(row.names(tabs), tabs[,9]), lty=5, lwd=3, col="green") # State Revenue/Tax
lines(lowess(row.names(tabs), tabs[,5]), lty=6, lwd=3, col="orange") # Legalization/Medicinal

legend("topright", 
       bty="n", 
       lty=1:6, 
       lwd=3,
       cex=.7,
       legend=c("Mexico/Border", "Addiction", "Law and Courts", 
                "Police/Shoot/Murder","State Revenue", 
                "Legalization/Medicinal"),
       col=c("blue", "red", "pink", "black", "green",  "orange")
)

