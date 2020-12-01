#######################################################
# Text Matching and Causal Inference with Observables #
# Following Roberts, Stewart, and Nielson (2020) AJPS #
# Notes copped from textmatching package              #
# Loren Collingwood, POSC 207                         #
#######################################################

# Clear everything nice and good #
rm(list=ls())

# install.packages("textmatching")
library(textmatching)

#We start by assuming that you have run a topic model in stm using
#your treatment variable as a content covariate. This is step 1.

#We have done this already and the following command loads the 
#topic model as well as the documents, vocab and meta data objects.
#See the stm package for more details about these model objects.
data(sim)

#Step 2 is to recalculate the topics as though they were observed
#as treated.  We do this using the refit() function.
refitted <- refit(sim_topics, sim_documents, content_level="1")

#to this we needed to specify the value of the treatment (here "1").  
#If you have forgotten content_levels() will tell you the levels
#for a given stm model. 
content_levels(sim_topics)

#Step 3 is to calculate the projection onto the treatment variable
projection <- project(sim_topics, sim_documents, interactions = FALSE)
#NB: here we have turned off interactions purely for speed during
#CRAN checks.  Consider including them if you believe topic-specific
#word choice is relevant.  See description above.

#Finally Step 4 is to match using CEM or other matching method of your
#choice
matched <- cem_match(refitted,projection=projection, sim_meta$treat,
                     projection_breaks=2)
#note here we use a much weaker match on the projections because the data
#have already been trimmed a lot.

#Now the matched data can be analyzed using standard tools from cem
cem::att(matched, simy ~ treat, data=sim_meta)
#the estimator overestimates a bit but contains the truth in the CI

#We can compare this to the unadjusted difference in means (overestimates)
summary(lm(simy ~ treat, data=sim_meta))
#and the oracle estimator (based on unobserved covariates)
summary(lm(simy ~ treat + confound1 + confound2 + confound3,data=sim_meta))

#Please, be sure to diagnose your matches!!! The key advantage of matching
#is being able to examine matched pairs.  It is always important to read 
#the documents!