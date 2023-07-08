test_that("Check Input", {
  expect_error(
    export_pdf(1:3, "Non sense", 1:5, 1:3)
  )
})

test_that("Check function runs", {
  expect_no_error(
    withr::with_tempfile(
      "tf",
      code = {
        export_pdf(figures, file = tf, width = widths, height = heights)
        export_cairo_pdf(figures, file = tf, width = widths, height = heights)
      }
    )
  )
})
