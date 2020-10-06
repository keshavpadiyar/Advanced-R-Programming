context("getKoladaAPIData")



test_that("getKoladaAPIData rejects errounous urls", {

  expect_error(getKoladaAPIData( url = "http://api.kolada.se/v3/"))

  expect_error(getKoladaAPIData(l_m = "123", l_kpi="123", l_year=""))

})

test_that("getKoladaAPIData rejects urls if no data available", {

  expect_error(getKoladaAPIData(url = "http://api.kolada.se/v2/data/kpi/N00002/municipality/0180/year/2050/"))

})

test_that("getKoladaAPIData() establishes connection", {
  expect_output(getKoladaAPIData(url = "http://api.kolada.se/v2/data/kpi/N00002/municipality/0180/year/2010"),"GET URL STATUS: Success")
})


test_that("getKoladaAPIData() fetched data", {
  test_val = unname(getKoladaAPIData(url = "http://api.kolada.se/v2/data/kpi/N00002/municipality/0180/year/2010"))
  expect_output(print(paste(test_val, collapse = ",", sep = ",")),"N00002,0180,2010,1,T,,38.088957")
})
