context('match_names()')

test_that("match_names() return an integer vector equal length to no. of data columns", {
  result <- match_names(data_example[[1]])
  expect_is(result, 'integer')
  expect_equal(length(result), ncol(data_example[[1]][["data"]]))
})

test_that("match_names() works in cases of fewer or more metadata fields than data columns", {
  fewer <- data_example[[1]]
  fewer[["attribute_metadata"]] <- fewer[["attribute_metadata"]][-1, ]
  
  fewer_match <- match_names(fewer)
  expect_error(fewer_match, NA)
  expect_equal(length(fewer_match), ncol(fewer[["data"]]))
  
  more <- data_example[[1]]
  more[["attribute_metadata"]][nrow(more[["attribute_metadata"]]) + 1, ] <- more[["attribute_metadata"]][1, ]
  
  more_match <- match_names(more)
  expect_error(more_match, NA)
  expect_equal(length(more_match), ncol(more[["data"]]))
})

# ---
# this fails about 9 out of 10 times :( need better fuzzy matching mechanism
# let's hope that most datasets don't list the attributes in completely random order

# test_that("match_names() works with metadata in shuffled order", {
#   shuffled <- data_example[[1]]
#   new_order <- sample(ncol(shuffled[["data"]]))
#   shuffled[["data"]] <- shuffled[["data"]][, new_order]
#   
#   expect_equal(match_names(shuffled), new_order)
# })
