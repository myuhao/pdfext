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

