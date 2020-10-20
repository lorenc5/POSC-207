##########################
# Loren Collingwood      #
# Readability Statistics # 
# Lexical Diversity      #
# Week 3 e.g's           #
##########################

library(quanteda)
library(dplyr)

txt <- c(doc1 = "Readability zero one. Ten, Eleven.",
         doc2 = "The cat in a dilapidated tophat.")
textstat_readability(txt, measure = "Flesch")

# sentence number of words
n <- length( unlist(str_split(as.vector(txt[1]), " ")) ); n
n_sent <- 2

# Number of syllables: Here this is off slighty...
nsyllable(txt[1], use.names=T)

# Calculate Flesch-Kincaid #
206.835 - (1.015*(n/n_sent)) - 84.6*(12/n)

# Type to Token Ratio #
txt <- c("Anyway, like I was sayin', shrimp is the fruit of the sea. You can
          barbecue it, boil it, broil it, bake it, saute it.",
         "There's shrimp-kabobs,
          shrimp creole, shrimp gumbo. Pan fried, deep fried, stir-fried. There's
          pineapple shrimp, lemon shrimp, coconut shrimp, pepper shrimp, shrimp soup,
          shrimp stew, shrimp salad, shrimp and potatoes, shrimp burger, shrimp
          sandwich.")

tokens(txt) %>%
    textstat_lexdiv(measure = c("TTR", "CTTR", "K"))
