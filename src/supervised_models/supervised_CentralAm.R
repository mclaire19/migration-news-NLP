# model training for Central Am keywords
# using DTM object from clean_preprocess_viz.R

dtm <- corp %>%
  tokens(
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE
  ) %>%
  dfm(
    remove = c(stopwords('english'), all_country_keywords)
  )

dtm <- dfm_trim(dtm, min_termfreq = 50,
                min_docfreq = 10)

corp_features <- docvars(dtm)

keep <- which(corp_features$CentralAm == 1 | corp_features$CentralAm == 0)
features <- corp_features[keep,]
dtm <- dtm[keep,]

# partition our data into train and test sets:
trainIndex <- createDataPartition(features$CentralAm,
                                  p = 0.8,
                                  list = FALSE,
                                  times = 1)

trainIndex <- trainIndex[,1]

train <- dtm[trainIndex, ]
test <- dtm[-trainIndex, ]

train.label  <- features$CentralAm[trainIndex]
test.label   <- features$CentralAm[-trainIndex]

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

Marranged_CentralAm <- M%>%
  arrange(desc(weight))

Marranged2_CentralAm <- M%>%
  arrange(weight)

CA_top <- as.matrix(Marranged_CentralAm)
CA_top <- CA_top[1:100,1]

CA2_top <- as.matrix(Marranged2_CentralAm)
CA2_top <- CA2_top[1:100,1]

CAtopfinal <- c(CA_top, CA2_top)
