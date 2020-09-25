##############################################
#      Deep Learning with Neural Networks    #
#      and tensorflow/keras                  #
##############################################
# https://tensorflow.rstudio.com/tutorials/

############
# Packages #
############

#install.packages(c("keras", "purrr", "pins", "tensorflow"))

library(keras)
library(dplyr)
library(ggplot2)
library(purrr)
library(pins)
library(tensorflow)

# Had to create a kaggle account and then download this api key #
# Otherwise you can download the data yourself and store it some place nice
board_register_kaggle(token = "~/Downloads/kaggle.json")

# Pull in the data and movie review .csv file 
path <- pins::pin_get("nltkdata/movie-review", "kaggle")[1]

# Read in the .csv file from the path
df <- readr::read_csv(path)

# take a look at first 6 rows 
head(df)

# Look at text 1 for shites and giggles 
df$text[1]

# Count of review variable #
df %>% count(tag)


# Create Unique id, 80% of dataset; used for separating train/test set
training_id <- sample.int(nrow(df), size = nrow(df)*0.8)

# Generate Train and Test sets
training <- df[training_id,]
testing <- df[-training_id,]

# Take a look at distribution of Words in Each Review
df$text %>% 
    strsplit(" ") %>% 
    sapply(length) %>% 
    summary()

# Define Text Vectorization Level #
# This will install miniconda; then install tensorflow()
# You might have some problem with installation here depending on your 
# computer setup.

# Create Text Vectorization Layer #
num_words <- 10000
max_length <- 50
text_vectorization <- layer_text_vectorization(
    max_tokens = num_words, 
    output_sequence_length = max_length, 
)

text_vectorization %>% 
    adapt(df$text)

# Look at vocabulary/words #
# TODO see https://github.com/tensorflow/tensorflow/pull/34529
head ( get_vocabulary(text_vectorization) )

#You can see how the text vectorization layer transforms it’s inputs:
text_vectorization(matrix(df$text[1], ncol = 1))

# Build Model for Input
input <- layer_input(shape = 1, dtype = "string")

# Output 
output <- input %>% 
    text_vectorization() %>% 
    layer_embedding(input_dim = num_words + 1, output_dim = 16) %>%
    layer_global_average_pooling_1d() %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dropout(0.5) %>% 
    layer_dense(units = 1, activation = "sigmoid")

model <- keras_model(input, output)

# Hidden Units and Loss Optimizer #
model %>% compile(
    optimizer = 'adam',
    loss = 'binary_crossentropy',
    metrics = list('accuracy')
)

###################
# Train the Model #
###################

history <- model %>% fit(
    training$text,
    as.numeric(training$tag == "pos"),
    epochs = 10,
    batch_size = 512,
    validation_split = 0.2,
    verbose=2
)

results <- model %>% evaluate(testing$text, as.numeric(testing$tag == "pos"), verbose = 0)
results

# Look at the within model accuracy/loss 
plot(history)

###################
# Predict Classes #
###################

# This is what you would do onto virgin text 

head(pred_out <- as.data.frame(predict(model,
                  x = testing$text)))

# Create the dummy/binary where 1 = pos, 0 = negative (rating)
pred_out$pred_bin <- ifelse(pred_out$V1 >=0.5, 1, 0)

# Just add on the original truth #
pred_out$truth <- as.numeric(testing$tag == "pos")

# Look at Confusion Matrix #
table(pred_out$pred_bin, pred_out$truth)

# Run the numbers real nice #
(4371 + 4404) / nrow(testing)

