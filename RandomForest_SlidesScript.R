require(lgrdata)
data(howell)

require(rpart)
require(rpart.plot)
set.seed(1234)
#build the initial decision tree
tree <- rpart(age ~ sex + weight + height, data = howell, control=rpart.control(cp=.0055))

prp(tree,
    faclen = 0, # use full names for factor labels
    extra = 1, # display number of observations for each terminal node
    roundint = F, # don't round to integers in output
    digits = 5, # display 5 decimal places in output
    box.col='olivedrab4') # colour of leaf nodes

require(randomForest)
set.seed(1234)
train_set_prop <- 0.9 # Choose 90% of observations for training
train_inx <- sample(c(1:nrow(howell)),
                    size = train_set_prop*nrow(howell)) # Get training set indices
test_inx <- setdiff(c(1:nrow(howell)), train_inx) # Test set indices
# Build a random forest
rf_howell <- randomForest(age ~ sex + weight + height,
                          data = howell,
                          importance = TRUE, # Compute variable importances
                          xtest = howell[test_inx, c(1, 3, 4)],
                          ytest = howell[test_inx, 2])

# Plot the OOB MSE against the number of trees
plot(x = c(1:rf_howell$ntree),
     y = rf_howell$mse,
     type = 'l',
     xlab = 'Number of trees',
     ylab = 'OOB Mean Squared Error',
     main = 'Training a Random Forest on Howell data')

# Plot variable importances
varImpPlot(rf_howell, 
           sort = TRUE, 
           main = "Variable Importance Plot (Howell Data)",
           col = 'navy',
           pch = 16)