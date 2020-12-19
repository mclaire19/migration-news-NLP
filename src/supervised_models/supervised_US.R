# model training for US keywords
# using DTM object from clean_preprocess_viz.R

corp_features <- docvars(dtm)

keep <- which(corp_features$US == 1 | corp_features$US == 0)
features <- corp_features[keep,]
dtm <- dtm[keep,]

# partition our data into train and test sets:
trainIndex <- createDataPartition(features$US,
                                  p = 0.8,
                                  list = FALSE,
                                  times = 1)

trainIndex <- trainIndex[,1]

train <- dtm[trainIndex, ]
test <- dtm[-trainIndex, ]

# Create separate vectors of our outcome variable for both our train and test sets
# We'll use these to train and test our model later
train.label  <- features$US[trainIndex]
test.label   <- features$US[-trainIndex]

# train our lasso
cvfit = cv.glmnet(x = train,
                  y = train.label,
                  family = "binomial",
                  type.measure = "class")

pred <- predict(
  cvfit,
  newx = test,
  s = "lambda.min",
  type = "response")

pred_vals <- ifelse(pred >= 0.5, 1, 0)
confusionMatrix(table(pred_vals, test.label),positive="1")

m <- coef(cvfit, s = "lambda.min")
M <- as.matrix(m)
M <- as.data.frame(M)
M <- cbind(feature = rownames(M), M)
rownames(M) <- 1:nrow(M)
names(M)[2] <- "weight"

M <- M %>%
  filter(weight != 0.00000000)

Marranged_US <- M%>%
  arrange(desc(weight))

Marranged2_US <- M%>%
  arrange(weight)

US_top <- as.matrix(Marranged_US)
US_top <- US_top[1:100,1]

US2_top <- as.matrix(Marranged2_US)
US2_top <- US2_top[1:100,1]

UStopfinal <- c(US_top, US2_top)
