#' @import grid
#' @importFrom bittermelon as_bm_pixmap bm_replace bm_trim
#' @importFrom dplyr filter mutate
#' @importFrom grDevices dev.off pdf
#' @importFrom pnpmisc pdf_create_jacket
#' @importFrom rlang abort check_dots_empty .data
#' @importFrom stringr str_c str_glue str_sub str_sub<-
#' @importFrom utils download.file
#' @importFrom xmpdf as_docinfo set_bookmarks set_docinfo set_xmp xmp
NULL

assert_runtime_dependencies <- function() {
	stopifnot(
		capabilities("cairo"),
		piecepackr::has_font("Carlito"),
		xmpdf::supports_set_bookmarks(),
		xmpdf::supports_set_docinfo(),
		xmpdf::supports_set_xmp()
	)
}
