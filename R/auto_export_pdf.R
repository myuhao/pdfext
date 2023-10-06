#' Automatic save ggplot as PDF
#'
#' @param dir The directory to save the file
#' @param file The single file used to save all pdf
#' @param include_unnamed_chunk Should code chunk be exported as well?
#' @param env The environment to register the method. Usually is the caller env.
#'
#' @importFrom knitr opts_current knit_print
#' @importFrom cli cli_abort cli_inform
#' @importFrom qpdf pdf_combine
#' @importFrom withr local_tempdir defer
#'
#' @return NULL
#'
#' @details
#' This function is called for its side-effect.
#'
#' It defines a custom [knitr::knit_print()] method for object of class `gg`,
#' i.e. `ggplot` objects. This custom print function will save the all `ggplot`
#' That is printed to the designated directory.
#'
#'
#' By default, only chunk with a name will be saved, and the  pdf file is
#' named after the chunk name. One can also use `include_unnamed_chunk = TRUE`.
#' In this case, they will be named something like `unnamed-chunk-2.pdf`.
#'
#' @rdname auto_export
#'
#' @export
auto_export_ggplot = function(dir, file, include_unnamed_chunk = FALSE, env = parent.frame()) {

  if (missing(dir) && missing(file)) {
    cli_abort(
      c(
        "Please provde either {.var dir} or {.var file}"
      )
    )
  }

  if (!missing(dir) && !missing(file)) {
    cli_abort(
      c(
        "Please provde {.var dir} or {.var file}, not both"
      )
    )
  }

  if (missing(file)) {
    cli_inform("Save ggplots as individual PDFs in {dir}")
    auto_export_to_dir(dir, include_unnamed_chunk, env)
  }

  if (missing(dir)) {
    cli_inform("Save ggplots as a single PDFs in {file}")
    auto_export_to_file(file, include_unnamed_chunk, env)
  }
}


#' @keywords internal
auto_export_to_dir = function(dir, include_unnamed_chunk, env) {
  force(include_unnamed_chunk)
  force(dir)

  default_knit_print = getS3method("knit_print", "default")

  fn = function(x, ...) {
    curr_chunk_name = opts_current$get("label")
    is_named = !grepl("^unnamed-chunk-\\d*$", curr_chunk_name)

    if (is_named || include_unnamed_chunk) {
      fname = paste0(curr_chunk_name, ".pdf")
      fname = file.path(dir, fname)
      default_knit_print(x, ...) # Call for side effect
      dev.print(pdf, fname)
    } else {
      default_knit_print(x, ...) # Call for side effect
    }
    invisible(x)
  }

  registerS3method(
    genname = "knit_print",
    class = "gg",
    method = fn,
    envir = env
  )
  invisible(NULL)
}

#' @keywords internal
auto_export_to_file = function(file, include_unnamed_chunk, env) {
  dir = local_tempdir(pattern = "pdfext", .local_envir = parent.frame(n = 2)) # This should be at global level?
  auto_export_to_dir(dir, include_unnamed_chunk, env = env) # Hope the S3 methods is registered.

  # Add this onto the defer stack.
  defer(
    expr = {
      pdfs = list.files(dir, pattern = "*.pdf", full.names = TRUE)
      # https://stackoverflow.com/questions/13762224/how-to-sort-files-list-by-date
      pdfs = file.info(pdfs)
      pdfs = pdfs[order(as.POSIXct(pdfs$ctime)), ]
      pdfs = rownames(pdfs)
      pdf_combine(pdfs, output = file)
    },
    envir = parent.frame(n = 2),
    priority = "first"
  )
}
