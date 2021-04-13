test_that("calculate how many terms", {

  ############################################################
  # calc quarter term
  # 四半期の期間を数える（両端はいる）
  ############################################################

  input_min <- as.Date("2009-11-01")
  input_max <- as.Date("2009-12-01")

  expect_identical(calc_quarter_term(input_min,input_max), 1)


  input_min <- as.Date("2009-01-01")
  input_max <- as.Date("2010-01-01")

  expect_identical(calc_quarter_term(input_min,input_max), 5)


  ############################################################
  # calc month term
  # 月の期間を数える（両端はいる）
  ############################################################

  input_min <- as.Date("2009-11-01")
  input_max <- as.Date("2009-11-21")

  expect_identical(calc_month_term(input_min,input_max), 1)


  input_min <- as.Date("2009-01-01")
  input_max <- as.Date("2010-01-01")

  expect_identical(calc_month_term(input_min,input_max), 13)

})
