# vector input test ----
test_that("create_palette requires a vector", {

  expect_error(create_palette(1, 2))

  expect_equal(length(create_palette(c(1, 2, 3))),
               3)

})
