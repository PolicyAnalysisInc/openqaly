context("Lookup tables")

test_that("Basic Lookups", {
  
  expect_equal(
    look_up(airquality, Month = 8, Day = 1, value = 'Wind'),
    6.9
  )
  
  expect_equal(
    look_up(airquality, Month = 6, Day = c(1,2,3), value = 'Wind'),
    c(8.6, 9.7, 16.1)
  )
  
  expect_equal(
    look_up(airquality, Month = 6, Day = 1.2, value = 'Temp', bin = T),
    78
  )
  
  expect_equal(
    look_up(airquality, Month = 6, Day = 1.2, value = 'Temp', bin = 'Day'),
    78
  )
  
  expect_equal(
    look_up(CO2, Plant = "Qn1", conc = 95, value = 'uptake'),
    16
  )
  
})

test_that("Lookup Erros", {
  
  expect_error(
    look_up(as.list(CO2), Plant = "Qn1", conc = 95, value = 'uptake'),
    'must be a data.frame'
  )
  
  expect_error(
    look_up(CO2, Plant == "Qn1", conc = 95, value = 'uptake'),
    paste0(
      "Error in look_up: Use '=' instead of '==' to specify lookup variables.",
      " Found an argument like ", sQuote("Plant == \"Qn1\""), ".",
      " Did you mean ", sQuote("Plant = \"Qn1\""), "?"
    ),
    fixed = TRUE
  )
  
  expect_error(
    look_up(CO2, Plant = "Qn1", conc = 95, 40, value = 'uptake'),
    "All lookup variables passed to 'look_up()' must be named using '='.",
    fixed = TRUE
  )
  
  expect_error(
    look_up(CO2, Plant = "Qn1", conc = 95, blah = 40, value = 'uptake'),
    "not found in 'data'"
  )
  
  expect_error(
    look_up(airquality, Month = 6, Day = 1.2, value = 'Temp', bin = 'blah'),
    "Names in 'bin' not found in source data"
  )
  
  expect_error(
    look_up(CO2, Plant = "Qn1", conc = 95, value = 'uptake', bin = "Plant"),
    "Some variables in 'bin' are not numeric"
  )
  
  expect_error(
    look_up(mtcars, cyl = 6, hp = 110, value = 'qsec'),
    "Some rows in 'data' are duplicates"
  )
  
  expect_warning(
    look_up(CO2, Plant = "blah", conc = 95, value = 'uptake'),
    "Some values were not found"
  )
  
})
