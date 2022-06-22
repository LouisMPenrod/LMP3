### Test select_select()
library(tibble)
library(dplyr)

data_df <- data.frame(x=1:10,y=1:10,z=1:10)

testthat::test_that("select_select allows data.frames and tibbles",{
  data_tbl <- as_tibble(data_df)

  expect_silent(data_df %>% select_select())
  expect_silent(data_tbl %>% select_select())
  })

testthat::test_that("select_select does not allow matricies",{
  data_mat <- as.matrix(data_df)
  data_list <- list(data_df)
  data_vec <- data_df[1:10,1]

  expect_error(data_mat %>% select_select(), "Object data must have a class data\\.frame \\(or tibble\\)\\.")
  expect_error(data_list %>% select_select(), "Object data must have a class data\\.frame \\(or tibble\\)\\.")
  expect_error(data_vec %>% select_select(), "Object data must have a class data\\.frame \\(or tibble\\)\\.")
})

testthat::test_that("select_select gives warning when only one column",{
  data_df_red <- data_df %>%
    select(1)

  expect_warning(data_df_red %>% select_select(), "Function unnecessary\\. Only one column exists in dataset provided\\.")
})
