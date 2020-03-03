# object_model_data output format ----
test_that("object_model_data output format", {
  expect_equal(object = class(t3:::object_model_data$new()),
               c("object_model_data", "R6"))
})

# object trip output format ----
test_that("object trip output format", {
  capture.output(expect_equal(object = class(t3::object_r6(class_name = "trips")),
                              c("trips", "list_t3", "R6")),
                 file = 'NUL')
})
