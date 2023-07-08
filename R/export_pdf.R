#' Export a list of figures to a single PDF
#'
#' @importFrom withr with_pdf with_cairo_pdf local_tempfile
#' @importFrom qpdf pdf_combine
#' @importFrom purrr pmap_chr
#' @importFrom cli cli_abort
#'
#' @param x A list of object that can be understand by the pdf device.
#' @param file The output file name.
#' @param width The width of each page in the final the PDF.
#' @param height The height of each page in the final the PDF.
#' @param ... Additional argument passed to the [pdf()] or [cairo_pdf()] device.
#'
#' @return Silently return the list of input object.
#'
#' @details
#' This function can be used to save multiple objects (usually plots) into
#' a single PDF file, with different dimensions.
#'
#' @name export_pdf
#'
#' @export
export_pdf = function(x, file, width, height, ...) {
  export_with_device(with_pdf, x, file, width, height, ...)
}

#' @name export_pdf
#' @export
export_cairo_pdf = function(x, file, width, height, ...) {
  export_with_device(with_cairo_pdf, x, file, width, height, ...)
}


export_with_device = function(with_dev, x, file, width, height, ...) {
  if (length(width) != length(x) || length(height) != length(x)) {
    cli_abort(
      c(
        "{.var width} and {.var height} must be the same length as the {.var x}",
        "i" = "{.var x} has {length(x)} element{?s}.",
        "i" = "{.var height} has {length(height)} element{?s}.",
        "i" = "{.var width} has {length(width)} element{?s}."
      )
    )
  }

  temp_files = pmap_chr(
    list(x, width, height),
    function(.g, .w, .h) {
      # create a temp pdf file.
      # Delete on exit of the `save_pdf` function
      fpath = local_tempfile(
        fileext = ".pdf",
        .local_envir = parent.frame(n = 3)
        # I think should be 2: anonymous function -> export_pdf
      )
      # save the figure to the pdf file
      with_dev(
        fpath,
        print(.g),
        width = .w, height = .h,
        ...
      )
      return(fpath)
    }
  )

  pdf_combine(temp_files, output = file) # Call this function to combine the file
  invisible(x)
}
