context("getData")



test_that("getData rejects errounous urls", {
  expect_error(getData("","","", url = "http://api.kolada.se/v3/"))
  expect_error(getData("123","123","", url = ""))
  expect_error(getData("","","", url = "http://api.kolada.se/v2/kpi/"))

})
