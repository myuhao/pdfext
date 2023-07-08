test_that("Check Input", {
  expect_error(export_pdf(1:3, "Nonsense", 1:5, 1:3))
  expect_error(export_pdf(1:3, "Nonsense", 1, 1:5))
  expect_error(export_pdf(1:3, "Nonsense", 1:5, 1))
})

test_that("Check function writes the expected number of pdfs", {
  expect_equal(
    get_pdf_length(export_pdf, figures, width = widths, height = heights),
    expected = length(figures)
  )
  expect_equal(
    get_pdf_length(export_pdf, figures, width = 7, height = heights),
    expected = length(figures)
  )
  expect_equal(
    get_pdf_length(export_pdf, figures, width = widths, height = 7),
    expected = length(figures)
  )
  expect_equal(
    get_pdf_length(export_pdf, figures, width = 7, height = 7),
    expected = length(figures)
  )


  expect_equal(
    get_pdf_length(export_cairo_pdf, figures, width = widths, height = heights),
    expected = length(figures)
  )
  expect_equal(
    get_pdf_length(export_cairo_pdf, figures, width = 7, height = heights),
    expected = length(figures)
  )
  expect_equal(
    get_pdf_length(export_cairo_pdf, figures, width = widths, height = 7),
    expected = length(figures)
  )
  expect_equal(
    get_pdf_length(export_cairo_pdf, figures, width = 7, height = 7),
    expected = length(figures)
  )


})
