test_that("Check Input", {
  expect_error(export_pdf(1:3, "Nonsense", 1:5, 1:3))
  expect_error(export_pdf(1:3, "Nonsense", 1, 1:5))
  expect_error(export_pdf(1:3, "Nonsense", 1:5, 1))
})


actual_dimensions = test_dimensions %>%
  mutate(
    pdf_info = purrr::pmap(
      list(.f, x, width, height),
      get_exported_pdf_info
    ),
    pdf_len = purrr::map_int(pdf_info, nrow),
    pdf_widths = purrr::map(pdf_info, ~.x$width / 72),
    pdf_heights = purrr::map(pdf_info, ~.x$height / 72),
    expected_len = purrr::map_int(x, length),
    expected_widths = purrr::map2(
      width, expected_len,
      ~ifelse(length(.x) == 1, rep(.x, .y), .x)
    ),
    expected_heights = purrr::map2(
      height, expected_len,
      ~ifelse(length(.x) == 1, rep(.x, .y), .x)
    )
  )

test_that("Check function writes the expected number of pdfs", {
  purrr::map2(
    actual_dimensions$pdf_len,
    actual_dimensions$expected_len,
    expect_equal
  )
})

test_that("Check function produce PDF of correct widths", {
  purrr::map2(
    actual_dimensions$pdf_widths,
    actual_dimensions$expected_widths,
    expect_setequal
  )
})

test_that("Check function produce PDF of correct heights", {
  purrr::map2(
    actual_dimensions$pdf_heights,
    actual_dimensions$expected_heights,
    expect_setequal
  )
})
