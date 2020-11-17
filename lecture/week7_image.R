##############################################
#      Loren Collingwood                     #
#      Image Classification with Keras       #
##############################################
# https://tensorflow.rstudio.com/tutorials/
# https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_basic_classification/

rm(list=ls())
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

# Read in Image Dataset (built into keras) #
# May take some time to download real nice #
fashion_mnist <- keras::dataset_fashion_mnist()

class(fashion_mnist); length(fashion_mnist)

# Numbers are pixel values between 0 - 255
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

# Label the Categorical Labels: 0-9 #
class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat', 
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

# Look at training set dimensions
dim(train_images)
dim(train_labels)

# Look at test set dimensions
dim(test_images)
dim(test_labels)

#######################
# Preprocess the Data #
#######################

# Let's take a look at the first image #
image_1 <- as.data.frame(train_images[1, , ])

# Label the Columns #
colnames(image_1) <- seq_len(ncol(image_1))

# Ordered DV #
image_1$y <- seq_len(nrow(image_1))

# Put the data in stacked fashion where y is 1:28, then 28 x's =1, and the 
# associated values
image_1 <- gather(image_1, "x", "value", -y)

# Convert x from chracter to integer #
image_1$x <- as.integer(image_1$x)

# This is what the first image looks like #
ggplot(image_1, aes(x = x, y = y, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "black", na.value = NA) +
    scale_y_reverse() +
    theme_minimal() +
    theme(panel.grid = element_blank())   +
    theme(aspect.ratio = 1) +
    xlab("") +
    ylab("")

# Scale the 0-255 to be from 0 - 1 for neural network reasons #
train_images <- train_images / 255
test_images <- test_images / 255

# Display first 25 Images
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
    img <- train_images[i, , ]
    img <- t(apply(img, 2, rev)) 
    image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
          main = paste(class_names[train_labels[i] + 1]))
}

###################
# Build the model #
###################

# Set up the layer #
model <- keras_model_sequential()
model %>%
    layer_flatten(input_shape = c(28, 28)) %>%
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dense(units = 10, activation = 'softmax')

# Compile the Model #
model %>% compile(
    optimizer = 'adam', 
    loss = 'sparse_categorical_crossentropy',
    metrics = c('accuracy')
)

# Train the Model #

# Might take some time real nice
model %>% fit(train_images, train_labels, epochs = 5, verbose = 2)

score <- model %>% evaluate(test_images, test_labels, verbose = 0)

cat('Test loss:', score[1], "\n")
cat('Test accuracy:', score[2], "\n")

# Predictions: onto test images #
predictions <- model %>% predict(test_images)

# See the first images; you want one to be much higher than the rest #
predictions[1, ]
which.max(predictions[1, ])

# Predict Classes 
class_pred <- model %>% predict_classes(test_images)
class_pred[1:20]

# Look at first one -- ground truth #
test_labels[1]

# Clear it good #
dev.off()

# Plot out #
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
    img <- test_images[i, , ]
    img <- t(apply(img, 2, rev)) 
    # subtract 1 as labels go from 0 to 9
    predicted_label <- which.max(predictions[i, ]) - 1
    true_label <- test_labels[i]
    if (predicted_label == true_label) {
        color <- '#008800' 
    } else {
        color <- '#bb0000'
    }
    image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
          main = paste0(class_names[predicted_label + 1], " (",
                        class_names[true_label + 1], ")"),
          col.main = color)
}


# Let's look at prediction for just one #

img <- test_images[1, , , drop = FALSE]
dim(img)

predictions <- model %>% predict(img)
predictions

# Handling the labels 
prediction <- predictions[1, ] - 1
which.max(prediction)

# As before we predict class 9 #
class_pred <- model %>% predict_classes(img)
class_pred