#' Check  `sbgjackets` run-time system dependencies
#'
#' `check_sbgjackets_dependencies()` checks this `sbgjackets`
#' run-time system dependencies.
#'
#' `sbgjackets` has the following run-time system dependencies:
#'
#' 1. The Carlito font.
#' 2. R compiled with support for the Cairo-devices i.e. `cairo_pdf()`.
#' 3. System tools to embed various pdf metadata such as `exiftool`, `gs`, and/or `pdftk`.
#'
#' @export
check_sbgjackets_dependencies <- function() {
	stopifnot(
		capabilities("cairo"),
		piecepackr::has_font("Carlito"),
		xmpdf::supports_set_bookmarks(),
		xmpdf::supports_set_docinfo(),
		xmpdf::supports_set_xmp()
	)
}

has_runtime_dependencies <- function() {
	capabilities("cairo") &&
		piecepackr::has_font("Carlito") &&
		xmpdf::supports_set_bookmarks() &&
		xmpdf::supports_set_docinfo() &&
		xmpdf::supports_set_xmp()
}
