n_indents <- function(x) {
	lines <- str_split(x, "\n")[[1L]]
	n_tabs <- str_count(lines, "\t")
	n_tabs <- n_tabs[n_tabs > 0L]
	if (length(n_tabs) == 0L) {
		return(0L)
	}
	min(n_tabs)
}

#' Trim common indentation from a multiline string
#'
#' `trim_multistring()` trims leading and trailing whitespace from
#' a string and then removes the minimum number of leading tabs
#' shared by all indented lines.
#'
#' @param x A single string containing newlines and tab indentation.
#' @return A single string with common leading tabs removed.
#' @noRd
trim_multistring <- function(x) {
	x <- str_trim(x)
	lines <- str_split(x, "\n")[[1L]]
	lines <- str_replace(lines, str_glue("^\\t{{{n_indents(x)}}}"), "")
	str_flatten(lines, "\n")
}
