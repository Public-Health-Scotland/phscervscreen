# dataframe and column tests ----
test_that("cerv_get_pct requires a data frame", {
  df1 <- "not a data frame"

  expect_error(cerv_get_pct(df1, (reported_fy == 1),
                            reported_fy, reported_14,
                            hb2019, fy_yr_start = 2023))
})

test_that("cerv_get_pct requires numerator to be a column name within data frame", {
  df1 <- data.frame(
    reported_fy = c(1),
    reported_14 = c(0),
    hb2019 = "S08000024"
  )

  expect_error(cerv_get_pct(df1, (reported_fy == 1),
                            reported_10, reported_fy,
                            hb2019, fy_yr_start = 2023))
})

test_that("cerv_get_pct requires denominator to be a column name within data frame", {
  df1 <- data.frame(
    reported_fy = c(1),
    reported_14 = c(0),
    hb2019 = "S08000024"
  )

  expect_error(cerv_get_pct(df1, (reported_fy == 1),
                            reported_14, reported,
                            hb2019, fy_yr_start = 2023))
})



# filter_expr works ----
test_that("filter_expr works correctly", {
  df1 <- data.frame(
    reported_fy = c(1, 1, 0),
    reported_14 = c(0, 1, 1),
    hb2019 = c("S08000024", "S08000031", "S08000026")
  )

  expect_equal(nrow(cerv_get_pct(df1, (reported_fy == 1),
                                 reported_14, reported_fy,
                                 hb2019, fy_yr_start = 2023,
                                 scotland = FALSE)), 2)

  expect_equal(cerv_get_pct(df1, (reported_14 == 0),
                            reported_14, reported_fy,
                            hb2019, fy_yr_start = 2023,
                            scotland = FALSE)[[1,1]], "S08000024")

})


# ... grouping works ----
test_that("... require columns for grouping", {
  df1 <- data.frame(
    reported_fy = c(1, 1, 0),
    reported_14 = c(0, 1, 1),
    hb2019 = c("S08000024", "S08000031", "S08000026"),
    simd = c(1, 4, 5)
  )

  expect_error(cerv_get_pct(df1, (reported_fy == 1),
                            reported_14, reported_fy,
                            #hb2019,
                            #fct_levels = hb_levels,
                            fy_yr_start = 2023))

  expect_equal(colnames(cerv_get_pct(df1, (reported_fy == 1),
                                     reported_14, reported_fy,
                                     hb2019,
                                     fy_yr_start = 2023))[[1]], "hb2019")

})


# Coverage calculations works ----
test_that("When calc_coverage is true pre-2025 'age' column is required", {
  df1 <- data.frame(
    scr3_5 = c(1, 1, 1),
    #age = c(25, 54, 34),
    reported_fy = c(1, 1, 0),
    reported_14 = c(0, 1, 1),
    hb2019 = c("S08000024", "S08000031", "S08000026"),
    simd = c(1, 4, 5)
  )

  expect_error(cerv_get_pct(df1, (reported_fy == 1),
                            reported_14, reported_fy,
                            hb2019,
                            #fct_levels = hb_levels,
                            fy_yr_start = 2023,
                            calc_coverage = TRUE))


})


test_that("When calc_coverage is true pre-2025 'scr3_5' column is required", {
  df1 <- data.frame(
    age = c(25, 54, 34),
    reported_fy = c(1, 1, 0),
    reported_14 = c(0, 1, 1),
    hb2019 = c("S08000024", "S08000031", "S08000026"),
    simd = c(1, 4, 5)
  )

  expect_error(cerv_get_pct(df1, (reported_fy == 1),
                            reported_14, reported_fy,
                            hb2019,
                            #fct_levels = hb_levels,
                            fy_yr_start = 2023,
                            calc_coverage = TRUE))

})


# Scotland total ----
test_that("When scotland is true hb2019 is the first grouping in ...", {
  df1 <- data.frame(
    reported_fy = c(1, 1, 0),
    reported_14 = c(0, 1, 1),
    hb2019 = c("S08000024", "S08000031", "S08000026"),
    simd = c(1, 4, 5)
  )

  hb_levels <- c("S08000015", "S08000016", "S08000017", "S08000029", "S08000019",
  "S08000020", "S08000031", "S08000022", "S08000032", "S08000024",
  "S08000025", "S08000026", "S08000030", "S08000028", "S92000003")

  expect_identical(
    as.vector(cerv_get_pct(df1, (reported_fy == 1),
                           reported_14, reported_fy,
                           hb2019,
                           fct_levels = hb_levels,
                           fy_yr_start = 2023,
                           scotland = TRUE)[[1]]),
    hb_levels
  )


  expect_true(
    tail(
      cerv_get_pct(df1, (reported_fy == 1),
                   reported_14, reported_fy,
                   hb2019,
                   fct_levels = hb_levels,
                   fy_yr_start = 2023,
                   scotland = TRUE),
      1)[1] == "S92000003"
  )

})

