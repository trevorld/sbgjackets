#' Polish a jacket pdf with instructions and/or metadata
#'
#' `pdf_polish_jacket()` polishes a jacket pdf by adding instructions and/or metadata.
#'
#' @param output Filename of a pdf file created by [pnpmisc::pdf_create_jacket()] or [pnpmisc::pdf_create_poker_jacket()].
#' @inheritParams sbgj_dominoes_double6
#' @param xmp A [xmpdf::xmp()] object.
#' @return The `output` filename invisibly
#' @export
pdf_polish_jacket <- function(output, ..., instructions = FALSE, xmp = NULL) {
	check_dots_empty()
	if (isTRUE(instructions)) {
		prepend_instructions(
			output,
			paper = pdf_paper(output)[[1L]],
			orientation = pdf_orientation(output)[[1L]]
		)
	}

	if (inherits(xmp, "xmp")) {
		set_xmp(xmp, output)
		set_docinfo(as_docinfo(xmp), output)
	}
	invisible(output)
}
