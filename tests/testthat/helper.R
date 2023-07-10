get_exported_pdf_info = function(.f, x, width, height, ..., dpi = 72) {
  pagesize = withr::with_tempfile(
    "tf",
    code = {
      .f(x, tf, width, height)
      pdftools::pdf_pagesize(tf)
    }
  )

  # if (length(width) == 1) {
  #   width = rep(width, length(x))
  # }
  # if (length(height) == 1) {
  #   height = rep(height, length(x))
  # }
  #
  # pdf_len = nrow(pagesize)
  # pdf_widths = pagesize$width / dpi
  # pdf_heights = pagesize$height / dpi
  #
  # expect_equal(pdf_len, length(x))
  # expect_setequal(pdf_widths, width)
  # expect_setequal(pdf_heights, height)
}
