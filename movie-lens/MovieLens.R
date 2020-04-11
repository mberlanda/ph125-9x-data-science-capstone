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
# The approach choosen of modeling, does not require anymore
# to partition the dataset at this stage
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
# Analysis
################################

# This code was run in RStudio while exploring the analysis
# When running the code in the terminal, we may just want to
# generate the .rda files required by the .Rmd
rstudio <- FALSE

if (rstudio) {
  # Check which columns can be considered as features
  setdiff(colnames(edx), c("rating", "title", "timestamp"))

  # Analyse the outcome distribution: rating

  # Overall rating distribution
  edx %>% group_by(rating) %>% count() %>%
    as_tibble() %>% mutate(half = factor((rating * 2) %% 2), rating = factor(rating)) %>%
    ggplot(aes(x=rating, y=n, color=half, fill = half)) +
    geom_bar(stat ="identity", alpha = 0.3) +
    ggtitle("Overall edx ratings distribution") +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

  ###
  # Ratings by reviewYear
  ###

  # Overall distribution
  edx %>% group_by(reviewYear, rating) %>% count() %>%
    ggplot(aes(x=reviewYear)) +
    geom_bar(aes(y=n, fill = factor(rating)), stat ="identity", alpha = 0.5) +
    geom_vline(xintercept = "2003", color = "red", linetype = "dashed") +
    ggtitle("Edx ratings distribution by reviewYear")

  # Since half-ratings have been introduced in 2003
  # visualize the rating distribution this year
  edx %>% group_by(rating, reviewYear) %>% count() %>%
    filter(as.numeric(levels(reviewYear)[reviewYear]) >= 2003) %>% as_tibble() %>%
    group_by(rating) %>% summarize(n = sum(n)) %>%
    mutate(half = factor((rating * 2) %% 2), rating = factor(rating)) %>%
    select(rating, n, half) %>%
    ggplot(aes(x=rating, y=n, color=half, fill = half)) +
    geom_bar(stat ="identity", alpha = 0.3) +
    ggtitle("Edx ratings distribution (2003-)") +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 90, vjust = .5), legend.position = "none")

  ###
  # Ratings by movieYear
  ###

  ratingByMovieYear <- edx %>% group_by(movieYear) %>% count() %>% mutate(perc = round(n/nrow(edx), digits = 3))
  # breaks levels for plotting
  movieYearStep <- ratingByMovieYear$movieYear[seq(1,nrow(ratingByMovieYear), 5)]

  ratingByMovieYear <- ratingByMovieYear %>%
    add_column(cumsum = cumsum(ratingByMovieYear$perc)) %>%
    add_column(nMovieYear = as.numeric(ratingByMovieYear$movieYear))

  # ratings distribution by movieYear
  ratingByMovieYear %>%
    ggplot(aes(x=movieYear, group=1)) +
    geom_bar(aes(y=perc, fill = factor(nMovieYear/10)), stat ="identity", alpha = 0.3) +
    geom_line(aes(y = (max(ratingByMovieYear$perc)*cumsum)), linetype = "dashed") +
    scale_y_continuous(sec.axis = sec_axis(~./max(ratingByMovieYear$perc), name = "Cumsum")) +
    scale_x_discrete(
      breaks=movieYearStep, labels=movieYearStep
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    ggtitle("Edx ratings distribution by movieYear")

  # ratings average by movieYear
  ratingAvgByMovieYear <- edx %>% group_by(movieYear) %>% summarize(avg=mean(rating))
  ratingAvgByMovieYear %>%
    ggplot(aes(x=movieYear, y=avg, group=1)) +
    geom_line() +
    scale_x_discrete(
      breaks=ratingAvgByMovieYear$movieYear[seq(1,nrow(ratingAvgByMovieYear), 5)],
      labels = ratingAvgByMovieYear$movieYear[seq(1,nrow(ratingAvgByMovieYear), 5)]
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
    ggtitle("Ratings average by movieYear")

  rm(ratingByMovieYear, ratingAvgByMovieYear, movieYearStep)

  ###
  # Ratings by timestamp: year, week, month, day of week
  # compares count and average
  ###

  t_ratings_by_year <- edx %>%
    group_by(year = year(datetime)) %>% summarise(count = n(), avg = mean(rating))
  t_ratings_by_year

  # ratings by year
  t_ratings_by_year %>%
    ggplot(aes(x = year)) +
    geom_histogram(aes(y = count), stat = "identity", color = "#eb5e55", fill = "#eb5e55", alpha = 0.3) +
    geom_line(aes(y = (max(t_ratings_by_year$count)/5)*avg), linetype = "dashed") +
    scale_y_continuous(sec.axis = sec_axis(~./(max(t_ratings_by_year$count)/5), name = "Average rating")) +
    ggtitle("Ratings by year")

  t_ratings_by_month <- edx %>%
    group_by(month = month(datetime, label=TRUE)) %>% summarise(count = n(), avg = mean(rating))
  t_ratings_by_month

  # ratings by month
  t_ratings_by_month %>%
    ggplot(aes(x = month, group=1)) +
    geom_histogram(aes(y = count), stat = "identity", color = "#540d6e", fill = "#540d6e", alpha = 0.3) +
    geom_line(aes(y = (max(t_ratings_by_month$count)/5)*avg), linetype = "dashed") +
    scale_y_continuous(sec.axis = sec_axis(~./(max(t_ratings_by_month$count)/5), name = "Average rating")) +
    ggtitle("Ratings by month")

  t_ratings_by_week <- edx %>%
    group_by(week = week(datetime)) %>% summarise(count = n(), avg = mean(rating))
  t_ratings_by_week

  # ratings by week
  t_ratings_by_week %>%
    ggplot(aes(x = week)) +
    geom_histogram(aes(y = count), stat = "identity", color = "#88a2aa", fill = "#88a2aa", alpha = 0.3) +
    geom_line(aes(y = (max(t_ratings_by_week$count)/5)*avg), linetype = "dashed") +
    scale_y_continuous(sec.axis = sec_axis(~./(max(t_ratings_by_week$count)/5), name = "Average rating")) +
    ggtitle("Ratings by week")

  t_ratings_by_wday <- edx %>%
    group_by(wday = wday(datetime, label=TRUE)) %>% summarise(count = n(), avg = mean(rating))
  t_ratings_by_wday

  # ratings by wday
  t_ratings_by_wday %>%
    ggplot(aes(x = wday, group=1)) +
    geom_histogram(aes(y = count), stat = "identity", color = "#e3d7ff", fill = "#e3d7ff", alpha = 0.5) +
    geom_line(aes(y = (max(t_ratings_by_wday$count)/5)*avg), linetype = "dashed") +
    scale_y_continuous(sec.axis = sec_axis(~./(max(t_ratings_by_wday$count)/5), name = "Average rating")) +
    ggtitle("Ratings by week day")

  rm(
    t_ratings_by_year,
    t_ratings_by_month,
    t_ratings_by_week,
    t_ratings_by_wday
  )

  ###
  # Ratings by genres
  ###

  # Proof of concept of the following code
  #
  # tibble(g = c("a", "a", "a|b", "b"), r = c(5, 3, 1, 2)) %>%
  #   group_by(g) %>% summarise(n = n(), tmp = sum(r)) %>%
  #   separate_rows(g, sep = "\\|") %>%
  #   group_by(g) %>%
  #   summarise(count = sum(n), avg = sum(tmp)/sum(n))
  #

  t_ratings_by_genre <- edx %>%
    group_by(genres) %>%
    summarize(n = n(), tmp = sum(rating)) %>%
    separate_rows(genres, sep = "\\|") %>%
    group_by(genres) %>%
    summarize(count = sum(n), avg = sum(tmp)/sum(n)) %>%
    arrange(desc(count))

  t_ratings_by_genre %>% head(10)

  t_ratings_by_genre %>%
    ggplot(aes(x = genres, group=1)) +
    # geom_line(aes(y = avg), linetype = "dashed") +
    geom_histogram(aes(y = count), stat = "identity", color = "#b2aa8e", fill = "#b2aa8e", alpha = 0.3) +
    geom_line(aes(y = (max(t_ratings_by_genre$count)/5)*avg), linetype = "dashed") +
    scale_y_continuous(sec.axis = sec_axis(~./(max(t_ratings_by_genre$count)/5), name = "Average rating")) +
    ggtitle("Ratings by Genres") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  rm(t_ratings_by_genre)

  ###
  # Ratings by userId
  ###

  n_distinct(edx$userId)
  edx %>% group_by(userId) %>%
    summarize(b_u = mean(rating)) %>%
    ggplot(aes(b_u)) +
    geom_histogram(bins = 10, color = "#EB5E55", fill = "#EB5E55", alpha = 0.3) +
    ggtitle("Edx average rating by userId")

  ###
  # Ratings by movieId
  ###

  n_distinct(edx$movieId)
  edx %>% group_by(movieId) %>%
    summarize(b_i = mean(rating)) %>%
    ggplot(aes(b_i)) +
    geom_histogram(bins = 10, color = "#540D6E", fill = "#540D6E", alpha = 0.3) +
    ggtitle("Edx average rating by movieId")

  # Top 10 rated movies
  edx %>% group_by(movieId, title) %>%
    summarize(count = n(), avg_rating = mean(rating)) %>%
    arrange(desc(count))  %>%
    head(10)
}

################################
# Modeling
################################

# The following code takes several minutes to run

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Bind the modeling code execution to a logic variable
exec_model <- FALSE

if (exec_model) {

  ###
  # Skew of ratings count by movieId and userId
  ###

  movies_reviews_count <- edx %>% group_by(movieId) %>% count() %>% as_tibble() %>%
    group_by(n) %>% count() %>% rename(n_ratings=n, n_movies=nn)
  movies_reviews_count <- movies_reviews_count %>%
    add_column(cumsum=cumsum(movies_reviews_count$n_movies/sum(movies_reviews_count$n_movies)))

  movies_reviews_count %>% head(1000) %>%
    ggplot(aes(x=n_ratings)) +
    geom_histogram(aes(y = n_movies), stat = "identity", alpha = 0.7) +
    geom_line(aes(y=cumsum * max(movies_reviews_count$n_movies)), linetype = "dashed") +
    scale_y_continuous(sec.axis = sec_axis(~./max(movies_reviews_count$n_movies), name = "cumsum (%)")) +
    ggtitle("Number of movies by number of ratings")

  users_reviews_count <- edx %>% group_by(movieId) %>% count() %>% as_tibble() %>%
    group_by(n) %>% count() %>% rename(n_ratings=n, n_users=nn)
  users_reviews_count <- users_reviews_count %>%
    add_column(cumsum=cumsum(users_reviews_count$n_users/sum(users_reviews_count$n_users)))

  users_reviews_count %>% head(1000) %>%
    ggplot(aes(x=n_ratings)) +
    geom_histogram(aes(y = n_users), stat = "identity", alpha = 0.7) +
    geom_line(aes(y=cumsum * max(users_reviews_count$n_users)), linetype = "dashed") +
    scale_y_continuous(sec.axis = sec_axis(~./max(users_reviews_count$n_users), name = "cumsum (%)")) +
    ggtitle("Number of users by number of ratings")

  rm(movies_reviews_count, users_reviews_count)

  # We will need to introduce a penalty term to moderate this effect


  ###
  # Linear model based on average
  ###

  y <- edx$rating
  mu <- mean(y)

  ###
  # 1. Simple average
  ###

  # Rudimental model execution timing for benchmarks
  start_time <- Sys.time()
  preds_0 <- predict_model_0(edx)
  model_0_rmse <- RMSE(y, preds_0)
  end_time <- Sys.time()
  model_0_time <- end_time - start_time
  rmse_results <- tibble(method = "Model 0: Average", RMSE = model_0_rmse, accuracy = mean(preds_0==y), lambda = 0, time = model_0_time)

  rm(preds_0, start_time, end_time, model_0_time)

  ###
  # 2. Movie effect
  ###

  # fit <- lm(rating ~ movieId, data = train_set)
  # Error: vector memory exhausted (limit reached?)

  l <- 0 # penalty term
  reg_movie_avgs <- edx %>%
       group_by(movieId) %>%
       summarize(b_i = sum(rating - mu)/(n()+l))

  ggplot(aes(x=b_i), data=reg_movie_avgs) + geom_histogram(bins=10, alpha=0.8, color="black")

  predict_model_1 <- function(df, l=0) {
    mu <- mean(edx$rating)
    reg_movie_avgs <- edx %>%
       group_by(movieId) %>%
       summarize(b_i = sum(rating - mu)/(n()+l))

    mu + df %>% left_join(reg_movie_avgs, by='movieId') %>% .$b_i
  }

  # Rudimental model execution timing for benchmarks
  start_time <- Sys.time()
  preds_1 <-predict_model_1(edx)
  model_1_rmse <- RMSE(y, preds_1)
  end_time <- Sys.time()
  model_1_time <- end_time - start_time

  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Model 1: Movie Effect",
                                       RMSE = model_1_rmse,
                                       accuracy = mean(preds_1==y),
                                       lambda = 0,
                                       time = model_1_time
                                       ))


  # Minimize the RMSE using a penalty factor
  l_min <- 0
  l_max <- 5
  l_step <- .25
  lambdas <- seq(l_min, l_max, l_step)

  model_1_lambdas_rmse <- sapply(lambdas, function(l) {
    RMSE(y, predict_model_1(edx, l))
  })
  l <- lambdas[which.min(model_1_lambdas_rmse)]

  l_min <- max(0, l-l_step)
  l_max <- l+l_step
  l_step <- l_step/5
  lambdas <- seq(l_min, l_max, l_step)

  model_1_lambdas_rmse <- sapply(lambdas, function(l) {
    RMSE(y, predict_model_1(edx, l))
  })
  l <- lambdas[which.min(model_1_lambdas_rmse)]
  qplot(lambdas, model_1_lambdas_rmse)

  if(l != 0) {
    preds_1l <- predict_model_1(edx, l)
    rmse_results <- bind_rows(rmse_results,
                            data_frame(
                              method="Model 1: Movie Effect (reg)",
                              RMSE = min(model_1_lambdas_rmse),
                              accuracy = mean(preds_1l==y),
                              lambda = l,
                              time = model_1_time
                            ))
  }

  rm(lambdas, l, l_min, l_max, preds_1, preds_1l, start_time, end_time, model_1_time)

  ###
  # 3. Genres effect
  ###

  reg_genres_avgs <- edx %>%
    left_join(reg_movie_avgs, by='movieId') %>%
    group_by(genres) %>%
    summarize(b_c = mean(rating - mu - b_i))

  ggplot(aes(x=b_c), data=reg_genres_avgs) + geom_histogram(bins=10, alpha=0.8, color="black")
  rm(reg_genres_avgs)

  ###
  # 4. movieYear effect
  ###

  reg_movieYear_avgs <- edx %>%
    left_join(reg_movie_avgs, by='movieId') %>%
    group_by(movieYear) %>%
    summarize(b_y = mean(rating - mu - b_i))

  ggplot(aes(x=b_y), data=reg_movieYear_avgs) + geom_histogram(bins=10, alpha=0.8, color="black")
  rm(reg_movieYear_avgs)

  ###
  # 5. userId effect
  ###

  l <- 0
  reg_user_avgs <- edx %>%
      left_join(reg_movie_avgs, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = mean(rating - mu - b_i))

  ggplot(aes(x=b_u), data=reg_user_avgs) + geom_histogram(bins=10, alpha=0.8, color="black")

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

  # Rudimental model execution timing for benchmarks
  start_time <- Sys.time()
  model_2_rmse <- RMSE(y, predict_model_2(edx, 0))
  end_time <- Sys.time()
  model_2_time <- end_time - start_time

  preds_2 <- predict_model_2(edx, 0)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Model 2: Movie + User Effect",
                                       RMSE = model_2_rmse,
                                       accuracy = mean(preds_2==y),
                                       lambda = 0,
                                       time = model_2_time
                                       ))
  # Minimize the RMSE by a penalty factor
  l_min <- 0.25
  l_max <- 10
  l_step <- .25
  lambdas <- seq(l_min, l_max, l_step)

  model_2_lambdas_rmse <- sapply(lambdas, function(l) {
    RMSE(y, predict_model_2(edx, l))
  })
  l <- lambdas[which.min(model_2_lambdas_rmse)]
  qplot(lambdas, model_2_lambdas_rmse)

  l_min <- max(0, l-l_step)
  l_max <- l+l_step
  l_step <- l_step/5
  lambdas <- seq(l_min, l_max, l_step)

  model_2_lambdas_rmse <- sapply(lambdas, function(l) {
    RMSE(y, predict_model_2(edx, l))
  })
  l <- lambdas[which.min(model_2_lambdas_rmse)]
  qplot(lambdas, model_2_lambdas_rmse)

  if(l != 0) {
    preds_2l <- predict_model_2(edx, l)
    rmse_results <- bind_rows(rmse_results,
                            data_frame(
                              method="Model 2: Movie + User Effect (reg)",
                              RMSE = min(model_2_lambdas_rmse),
                              accuracy = mean(preds_2l==y),
                              lambda = l,
                              time = model_2_time
                            ))
  }
  rm(lambdas, L, l_min, l_max, preds_2, preds_2l, start_time, end_time, model_2_time)

  ###
  # 6. reviewYear and reviewWeek effect
  ###

  l <- 0
  reg_reviewYear_avgs <- edx %>%
      left_join(reg_movie_avgs, by='movieId') %>%
      left_join(reg_user_avgs, by='userId') %>%
      group_by(reviewYear) %>%
      summarize(b_y = sum(rating - mu - b_i)/(n()+l))

  ggplot(aes(x=b_y), data=reg_reviewYear_avgs) + geom_histogram(bins=10, alpha=0.8, color="black")

  reg_reviewWeek_avgs <- edx %>%
      left_join(reg_movie_avgs, by='movieId') %>%
      left_join(reg_user_avgs, by='userId') %>%
      left_join(reg_reviewYear_avgs, by='reviewYear') %>%
      group_by(reviewWeek) %>%
      summarize(b_w = sum(rating - mu - b_i - b_u - b_y)/(n()+l))

  ggplot(aes(x=b_w), data=reg_reviewWeek_avgs) + geom_histogram(bins=10, alpha=0.8, color="black")

  predict_model_3 <- function(df, l=0) {
    mu <- mean(edx$rating)

    reg_movie_avgs <- edx %>%
     group_by(movieId) %>%
     summarize(b_i = sum(rating - mu)/(n()+l))

    reg_user_avgs <- edx %>%
      left_join(reg_movie_avgs, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - mu - b_i)/(n()+l))

    reg_reviewYear_avgs <- edx %>%
      left_join(reg_movie_avgs, by='movieId') %>%
      left_join(reg_user_avgs, by='userId') %>%
      group_by(reviewYear) %>%
      summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+l))

    reg_reviewWeek_avgs <- edx %>%
      left_join(reg_movie_avgs, by='movieId') %>%
      left_join(reg_user_avgs, by='userId') %>%
      left_join(reg_reviewYear_avgs, by='reviewYear') %>%
      group_by(reviewWeek) %>%
      summarize(b_w = sum(rating - mu - b_i - b_u - b_y)/(n()+l))

    df %>%
     left_join(reg_movie_avgs, by='movieId') %>%
     left_join(reg_user_avgs, by='userId') %>%
     left_join(reg_reviewYear_avgs, by='reviewYear') %>%
     # left_join(reg_reviewWeek_avgs, by='reviewWeek') %>%
     mutate(pred = mu + b_i + b_u + b_y) %>% .$pred
  }

  # Rudimental model execution timing for benchmarks
  start_time <- Sys.time()
  model_3_rmse <- RMSE(y, predict_model_3(edx, 0))
  end_time <- Sys.time()
  model_3_time <- end_time - start_time
  preds_3 <- predict_model_3(edx, 0)

  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Model 3: Movie + User + reviewYearWeek Effect",
                                       RMSE = model_3_rmse,
                                       accuracy = mean(preds_3==y),
                                       lambda = 0,
                                       time = model_3_time
                                       ))

  # Minimize the RMSE using a penalty factor
  l_min <- 0
  l_max <- 5
  l_step <- .25
  lambdas <- seq(l_min, l_max, l_step)

  model_3_lambdas_rmse <- sapply(lambdas, function(l) {
    RMSE(y, predict_model_3(edx, l))
  })
  l <- lambdas[which.min(model_3_lambdas_rmse)]

  l_min <- max(0, l-l_step)
  l_max <- l+l_step
  l_step <- l_step/5
  lambdas <- seq(l_min, l_max, l_step)

  model_3_lambdas_rmse <- sapply(lambdas, function(l) {
    RMSE(y, predict_model_3(edx, l))
  })
  l <- lambdas[which.min(model_3_lambdas_rmse)]
  qplot(lambdas, model_3_lambdas_rmse)

  if(l != 0) {
    preds_3l <- predict_model_3(edx, l)
    rmse_results <- bind_rows(rmse_results,
                            data_frame(
                              method="Model 3: Movie + User + reviewYearWeek Effect (reg)",
                              RMSE = min(model_3_lambdas_rmse),
                              accuracy = mean(preds_3l==y),
                              lambda = l,
                              time = model_3_time
                            ))
  }
  rm(lambdas, l, l_min, l_max, preds_3, preds_3l, start_time, end_time, model_3_time)

  ###
  # 7. Rounding
  ###

  # How to convert continous to categorical outcome?
  # seq(.5,5,.5)
  # Rounding POC
  n <- 2.7
  ceiling(n*2)/2

  ## ceiling factor for rounding
  uReviewYear <- sort(unique(edx$reviewYear))
  ceiling_reviewYear <- tibble(
    reviewYear = uReviewYear,
    c_i = sapply(uReviewYear, function(x) { ifelse(as.numeric(levels(uReviewYear)[x]) >= 2003, 2, 1) })
  )

  l <- 0
  predict_model_4 <- function(df, l=0) {
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
     left_join(ceiling_reviewYear, by='reviewYear') %>%
     mutate(pred = ceiling((mu + b_i + b_u)*c_i)/c_i) %>% .$pred
  }

  # Rudimental model execution timing for benchmarks
  start_time <- Sys.time()
  preds_4 <- predict_model_4(edx, 0)
  model_4_rmse <- RMSE(y, preds_4)
  end_time <- Sys.time()
  model_4_time <- end_time - start_time


  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Model 4: Movie + User Effect rounded",
                                       RMSE = model_4_rmse,
                                       accuracy = mean(preds_4==y),
                                       lambda = 0,
                                       time = model_4_time
                                       ))
  # Minimize the RMSE by a penalty factor
  l_min <- 0
  l_max <- 50
  l_step <- 5
  lambdas <- seq(l_min, l_max, l_step)

  model_4_lambdas_rmse <- sapply(lambdas, function(l) {
    RMSE(y, predict_model_4(edx, l))
  })
  l <- lambdas[which.min(model_4_lambdas_rmse)]
  qplot(lambdas, model_4_lambdas_rmse)

  rm(preds_4, start_time, end_time, model_4_time)

  ###
  # Result: evaluate RMSE of models
  ###

  rmse_results

  ggplot(rmse_results, aes(x=method, fill=RMSE <= 0.86490, group=1)) +
    geom_bar(aes(y=RMSE), stat ="identity", alpha = 0.5) +
    geom_hline(yintercept = 0.86490, linetype = "dotted", color = "red") +
    geom_line(aes(y = (max(rmse_results$RMSE)/as.integer(max(rmse_results$time)))*time), linetype = "dashed") +
    scale_y_continuous(sec.axis = sec_axis(~./(max(rmse_results$RMSE)/as.integer(max(rmse_results$time))), name = "Execution time (sec)")) +
    ggtitle("Models results on edx vs target (0.86490)") +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_flip()
}


###
# 8. Cross-validation
# prevent bias and overfitting on training set
###

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

if (exec_model) {
  library(reshape2)

  melt(rmses_matrix, value.name = "rmse") %>%
    rename(lambda = Var1, seed = Var2) %>%
    ggplot(aes(x = lambda, y=rmse)) +
    geom_point(alpha = .8) +
    geom_vline(xintercept = validation_lambda,color="red", linetype="dashed") +
    geom_smooth(formula = y ~ x, method = "loess") +
    ggtitle("Cross-validation of penalty term lambda")
}

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


################################
### Result: validation
################################

RMSE(validation$rating, predict_model_2(validation, l=validation_lambda))
