################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate")
if(!require(stringr)) install.packages("stringr")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

data_filename <- "movie-lens/movie-data.rda"
force_reload <- FALSE

if(file.exists(data_filename) && !force_reload) {

    load(data_filename)

} else {
  
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                             title = as.character(title),
                                             genres = as.character(genres))
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Validation set will be 10% of MovieLens data
  set.seed(1, sample.kind="Rounding")
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  edx <- movielens[-test_index,]
  temp <- movielens[test_index,]
  
  # Make sure userId and movieId in validation set are also in edx set
  validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(temp, validation)
  edx <- rbind(edx, removed)
  
  rm(dl, ratings, movies, test_index, temp, movielens, removed)
  
  ################################
  # Data Wrangling/Cleaning
  ################################
  
  # Store edx and validation in data files to make them
  # available for the Rmd report
  t_specifications <- tibble(
    edx = summary.default(edx),
    validation = summary.default(validation)
  )
  edx_summary <- summary(edx)
  edx_head <- head(edx)
  
  wrangle_data <-function(ds) {
    ds %>%
      mutate(
        userId = factor(userId), # int to factor
        movieId = factor(movieId), # num to factor
        datetime = as_datetime(timestamp), # parse int into date
        movieYear = str_sub(title,-5,-2), # extract movieYear from title (xxxx),
        reviewYear = factor(year(datetime)),
        reviewWeek = factor(week(datetime))
      )
  }
  
  edx <- wrangle_data(edx)
  validation <- wrangle_data(validation)
  
  save(t_specifications, edx_summary, edx_head, edx, validation, file = data_filename)
}


# Skip, no longer used
if (FALSE) {
  # Separate the known observations dataframe into train_set and test_set
  # Test set will be 10% of MovieLens data
  set.seed(17, sample.kind="Rounding")
  test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
  train_set <- edx[-test_index,]
  temp <- edx[test_index,]
  
  # Ensure again that userId and movieId in the test_set are included in the train_set
  test_set <- temp %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  removed <- anti_join(temp, test_set)
  train_set <- rbind(train_set, removed)
  
  rm(test_index, temp, removed)
}

################################
# Modeling
################################

# The following code takes over 45 minutes to run
# After some analysis reported in the .Rmd I came up with this model

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

generate_sets <- function (seed) {
  set.seed(seed, sample.kind="Rounding")
  test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
  train_set <- edx[-test_index,]
  temp <- edx[test_index,]
  
  test_set <- temp %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  removed <- anti_join(temp, test_set)
  train_set <- rbind(train_set, removed)
  
  rm(test_index, temp, removed)
  list(train_set, test_set)
}

predict_model <- function(train, test, l=0) {
  mu <- mean(train$rating)
  reg_movie_avgs <- train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  reg_user_avgs <- train %>%
    left_join(reg_movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  test %>%
    left_join(reg_movie_avgs, by='movieId') %>%
    left_join(reg_user_avgs, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>% .$pred
}

cross_validate <- function(seed) {
  lambdas <- seq(0,10,.25)
  sets <- generate_sets(seed)
  
  train_set <- sets[[1]]
  test_set <- sets[[2]]
  rm(sets)
  
  y <- test_set$rating
  
  sapply(lambdas, function(l){
    preds <- predict_model(train_set, test_set, l)
    RMSE(y, preds)
  })
}

lambdas <- seq(0,10,.25)
seeds <- seq(1, 50, 7)

rmses_lambda_filename <- "movie-lens/data/rmses_matrix.rda"
if(file.exists(rmses_lambda_filename) && !force_reload) {
  load(rmses_lambda_filename)
} else {
  rmses_matrix <- sapply(seeds, cross_validate)
  colnames(rmses_matrix) <- seeds
  rownames(rmses_matrix) <- lambdas
  save(rmses_matrix, file = rmses_lambda_filename)  
}

colMeans(rmses_matrix)
rowMeans(rmses_matrix)

validation_lambda <- lambdas[which.min(rowMeans(rmses_matrix))]
validation_lambda

################################
# Results
################################

# final_model
predict_model_2 <- function(df, l=0) {
  mu <- mean(edx$rating)
  reg_movie_avgs <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  reg_user_avgs <- edx %>%
    left_join(reg_movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  df %>%
    left_join(reg_movie_avgs, by='movieId') %>%
    left_join(reg_user_avgs, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>% .$pred
}

RMSE(validation$rating, predict_model_2(validation, l=validation_lambda))
