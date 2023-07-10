get_exported_pdf_info = function(.f, x, width, height, ..., dpi = 72) {
  pagesize = withr::with_tempfile(
    "tf",
    code = {
      .f(x, tf, width, height)
      pdftools::pdf_pagesize(tf)
    }
  )
}



# Clean up temp files from knitting...
snapshot_tempdir = function(temp_dir = tempdir(), env = parent.frame()) {
  existing_temp_files = list.files(temp_dir, full.names = TRUE)
  withr::defer({
    new_temp_files = setdiff(
      list.files(temp_dir, full.names = TRUE),
      existing_temp_files
    )
    if (length(new_temp_files) > 0) {
      file.remove(new_temp_files)
    }
  },
  envir = env
  )
}
