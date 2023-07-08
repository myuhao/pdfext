#' Export a list of figures to a single PDF
#'
#' @importFrom withr with_pdf with_cairo_pdf local_tempfile
#' @importFrom qpdf pdf_combine
#' @importFrom purrr pmap_chr walk
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
#' @details
#' A scalar can be used for the width and height argument.
#' In this case, this width/height will be applied to all pages
#' of the final PDF file.
#'
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


#' @keywords internal
#' Use the native pdf device to export images.
native_export = function(with_dev, x, file, width, height, ...) {
  with_dev(
    new = file,
    code = {
      walk(x, print)
    },
    width = width, height = height,
    ...
  )
  invisible(x)
}

#' @keywords internal
#' Allow for different dimmensions.
custom_export = function(with_dev, x, file, width, height, ...) {
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


#' @keywords internal
#' Check input and fix input
export_with_device = function(with_dev, x, file, width, height, ...) {

  n_x = length(x)
  n_width = length(width)
  n_height = length(height)

  # 1. If n_width and n_height are both scalar,
  # call the native export
  if (n_height == 1 && n_width == 1) {
    invisible(native_export(with_dev, x, file, width, height, ...))
  }

  # 2. Otherwise, we will try to match the length of w/h by the length of x if
  # it is a scalar.
  if (n_height > 1 && n_height != n_x) {
    cli_abort(
      c(
        "{.var height} must be a scalar or a vector of the same length as the {.var x}",
        "i" = "{.var x} has {n_x} element{?s}.",
        "i" = "{.var height} has {n_height} element{?s}."
      )
    )
  }
  if (n_width > 1 && n_width != n_x) {
    cli_abort(
      c(
        "{.var width} must be a scalar or a vector of the same length as the {.var x}",
        "i" = "{.var x} has {n_x} element{?s}.",
        "i" = "{.var width} has {n_width} element{?s}."
      )
    )
  }

  width = ifelse(n_width == 1, rep(width, n_x), width)
  height = ifelse(n_height == 1, rep(height, n_x), height)

  invisible(custom_export(with_dev, x, file, width, height, ...))

}
