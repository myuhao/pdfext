library(ggplot2)

figures = purrr::map(
  1:10,
  ~ggplot(midwest, aes(x = area, y = poptotal)) +
    geom_point()
)
widths = withr::with_seed(
  0L,
  sample(1:10, length(figures), replace = TRUE)
)
heights = withr::with_seed(
  100L,
  sample(1:10, length(figures), replace = TRUE)
)

get_pdf_length = function(.f, x, width, height, ...) {
  withr::with_tempfile(
    "tf",
    code = {
      .f(x, tf, width, height, ...)
      qpdf::pdf_length(tf)
    }
  )
}
