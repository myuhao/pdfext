test_that("Check auto export checks user input", {
  expect_error(auto_export_ggplot())
  expect_error(auto_export_ggplot("nonsense-dir", "nonsense-file"))
})

EXPECTED_N_PDFS = 3
EXPECTED_WIDTHS = c(7, 1, 10)
EXPECTED_HEIGHTS = c(7, 10, 1)
EXPECTED_FILENAMES = c("this-is-a-named-chunk.pdf", "this-is-another-named-chunk.pdf", "unnamed-chunk-2.pdf")
DPI = 72


test_that("Check auto save individual files excluding unnamed chunks", {
  snapshot_tempdir()
  dir = withr::local_tempdir(pattern = "test-dir-ex-unnamed-chunks")
  withr::with_tempfile(
    "tf",
    knitr::knit(
      test_path("test-auto_export_rmd/to-dir-exclude-unnamed.Rmd"),
      output = tf,
      quiet = TRUE
    ),
    pattern = "test-dir-ex-unnamed-chunks"
  )


  pagesize = list.files(dir, pattern = ".pdf", full.names = TRUE) %>%
    purrr::map(pdftools::pdf_pagesize) %>%
    bind_rows()

  expect_setequal(list.files(!!dir), EXPECTED_FILENAMES[1:2])
  expect_setequal(pagesize$width / DPI, EXPECTED_WIDTHS[1:2])
  expect_setequal(pagesize$height / DPI, EXPECTED_HEIGHTS[1:2])

})

test_that("Check auto save individual files including unnamed chunks", {
  snapshot_tempdir()
  dir = withr::local_tempdir(pattern = "test-dir-in-unnamed-chunks")
  withr::with_tempfile(
    "tf",
    knitr::knit(
      test_path("test-auto_export_rmd/to-dir-include-unnamed.Rmd"),
      output = tf,
      quiet = TRUE
    ),
    pattern = "test-dir-in-unnamed-chunks"
  )
  pagesize = list.files(dir, pattern = ".pdf", full.names = TRUE) %>%
    purrr::map(pdftools::pdf_pagesize) %>%
    bind_rows()

  expect_setequal(list.files(!!dir), EXPECTED_FILENAMES)
  expect_setequal(pagesize$width / DPI, EXPECTED_WIDTHS)
  expect_setequal(pagesize$height / DPI, EXPECTED_HEIGHTS)
})



test_that("Check auto export to a single file", {
  snapshot_tempdir()
  file = withr::local_tempfile(pattern = "test-file-in-unnamed-chunks", fileext = ".pdf")
  withr::with_tempfile(
    "tf",
    knitr::knit(
      test_path("test-auto_export_rmd/to-file-include-unnamed.Rmd"),
      output = tf,
      quiet = TRUE
    )
  )
  pagesize = pdftools::pdf_pagesize(file)
  expect_equal(nrow(pagesize), EXPECTED_N_PDFS)
  expect_setequal(pagesize$width / DPI, EXPECTED_WIDTHS)
  expect_setequal(pagesize$height / DPI, EXPECTED_HEIGHTS)

})



