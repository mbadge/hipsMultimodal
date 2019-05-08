# Explore how to leverage the caret train routine to generate optimal caret::train objects
# This example trains a linear regression model with the
# product caret::train object capturing extra metadata


# Pass `dots` ----
# Retaining information in the caret::train object slot `dots`
library(mlbench)
data(BostonHousing)

# base train args produce a train object with dots slot containing an empty list
data(BostonHousing, package="mlbench")
lmFit <- train(medv ~ . + rm:lstat,
               data = BostonHousing,
               method = "lm")
lmFit$call
lmFit %>% class
lmFit$dots

# If additional terms are passed to caret::train they're stored in the caret::train object slot dots (even if not the very last)
lmFit <- train(medv ~ . + rm:lstat,
               data = BostonHousing,
               predictors = "ehr", indication = "fx",
               method = "lm")
lmFit$dots



# How about with x, y based definitions instead of formula?
X <- BostonHousing %>% select(-medv)
Y <- BostonHousing$medv

lmFit <- train(x = X, y = Y,
               method = "lm")
lmFit$call
lmFit %>% class
lmFit$dots

lmFit <- train(x = X, y = Y,
               predictors = "ehr", indication = "fx",
               method = "lm")
lmFit$call
lmFit %>% class
lmFit$dots

# Yup! Either way I define the model training, the dots will be saved!



lmFit <- train(x = X, y = Y,
               extra_df = mtcars,
               method = "lm")
lmFit$call
lmFit %>% class
lmFit$dots
