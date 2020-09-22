########################
# Loren Collingwood    #
# Supervised Learning  #
# Using NYTIMES corpus #
########################

# Packages #
library(RTextTools)

# Load NYT Media Data #
data(NYTimes)
head(NYTimes)

# Look at Distribution #
table(NYTimes$Topic.Code)

# Randomize the sample to reduce any chance of time dependency.

data <- NYTimes[sample(1:3100,size=3100,replace=FALSE),]
n <- nrow(data)

# Create DTM train/test
matrix <- create_matrix(cbind(data["Title"],data["Subject"]), 
                        language="english", 
                        removeNumbers=TRUE, 
                        stemWords=FALSE, 
                        weighting=tm::weightTfIdf)

# Container/holder...

container <- create_container(matrix,data$Topic.Code,
                              trainSize=1:2100, 
                              testSize=2101:n, 
                              virgin=FALSE)


# look at available algorithms #
print_algorithms()

# Train your model this can take some time
models <- train_models(container, algorithms=c("GLMNET","SVM"))

#Classification

results <- classify_models(container, models)

# Create analytics out of results #
analytics <- create_analytics(container, results)
summary(analytics)

