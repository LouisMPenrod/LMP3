### Test factor_sort()
library(dplyr)

data <- data.frame(x=factor(rep(c("Hi","Med","Low"),each=5)))

testthat::test_that("select_select allows data.frames and tibbles",{
  data_tbl <- as_tibble(data)

  expect_silent(data %>% factor_sort(col = x))
  expect_silent(data_tbl %>% factor_sort(col = x))
})

testthat::test_that("select_select does not allow matricies",{
  data_mat <- as.matrix(data)
  data_list <- list(data)
  data_vec <- data[1:10,1]

  expect_error(data_mat %>% factor_sort(col = x), "Object data must have a class data\\.frame \\(or tibble\\)\\.")
  expect_error(data_list %>% factor_sort(col = x), "Object data must have a class data\\.frame \\(or tibble\\)\\.")
  expect_error(data_vec %>% factor_sort(col = x), "Object data must have a class data\\.frame \\(or tibble\\)\\.")
})

testthat::test_that("test check for column existence",{
  expect_silent(data %>% factor_sort(col = x))
  expect_error(data %>% factor_sort(col = y), "column provided does not exist")
})

testthat::test_that("test check for column type",{
  expect_silent(data %>% factor_sort(col = x)) # check factor column
  expect_silent(data %>%
                  mutate(x = as.character(x)) %>%
                  factor_sort(col = x)) # check character column

  # check non-character or -factor column
  expect_error(data %>%
                 mutate(x = as.numeric(x)) %>%
                 factor_sort(col = x),
               "column must be either a factor or a character")
})

testthat::test_that("test check for single unique value",{
  expect_silent(data %>% factor_sort(col = x)) # check column with multiple values

  expect_warning(data %>%
                   filter(x=="Hi") %>%
                   factor_sort(col = x),
                 "only one unique value exists in column provided") # check column with single value

})
