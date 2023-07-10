library(ggplot2)
library(tibble)
library(dplyr)

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

test_dimensions = tibble(
  .f = list(export_pdf, export_pdf, export_pdf, export_pdf),
  x = list(figures, figures, figures, figures),
  width = list(widths, 7, widths, 7),
  height = list(heights, heights, 7, 7)
)


test_dimensions = add_row(
  test_dimensions,
  mutate(test_dimensions, .f = list(export_cairo_pdf))
)

