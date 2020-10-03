context("getData")



test_that("getData rejects errounous urls", {

  expect_error(getData( url = "http://api.kolada.se/v3/"))

  expect_error(getData(l_m = "123", l_kpi="123", l_year=""))

})

test_that("getData rejects urls if no data available", {

  expect_error(getData(url = "http://api.kolada.se/v2/data/kpi/N00002/municipality/0180/year/2050/"))

})

test_that("getData() establishes connection", {
  expect_output(getData(url = "http://api.kolada.se/v2/data/kpi/N00002/municipality/0180/year/2010"),"GET URL STATUS: Success")
})


test_that("getData() fetched data", {
  test_val = unname(getData(url = "http://api.kolada.se/v2/data/kpi/N00002/municipality/0180/year/2010"))
  expect_output(print(paste(test_val, collapse = ",", sep = ",")),"1,N00002,0180,2010,1,T,,38.088957")
})
