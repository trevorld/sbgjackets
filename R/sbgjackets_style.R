#' Small Box Game Jackets `marquee` style
#'
#' `sbgjackets_style()` returns a `marquee` style set object
#' to help add text to jackets in a consistent style.
#'
#' @param size Either `"4x6"` for a 4x6 photo storage box jacket,
#'             `"poker"` for a playing card storage box, or
#'             `"wallet"` for a card wallet origami jacket.
#' @param ... Ignored
#' @param color The desired text color.
#' @param align If not `NULL` the text alignment
#'              (e.g. `"left"`, `"center"`, or `"right"`)
#'              to use for the `"base"` style,
#'              see [marquee::style()] for allowed values.
#' @param cex Multiplier applied to the base font size (and hence the
#'            line spacing which is relative to the font size).
#' @return A `marquee` style set object.
#' @export
sbgjackets_style <- function(
	size = c("4x6", "poker", "wallet"),
	...,
	color = "black",
	align = NULL,
	cex = 1
) {
	check_dots_empty()
	size <- match.arg(size)
	if (size == "4x6") {
		base_size <- 10
		lineheight <- 1.6
	} else if (size == "poker") {
		base_size <- 9
		lineheight <- 1.4
	} else {
		base_size <- 8
		lineheight <- 1.3
	}
	base_size <- cex * base_size
	# Don't manually set `bullets` to avoid #99
	style <- marquee::classic_style(
		base_size = base_size,
		body_font = "Carlito",
		color = color,
		header_font = "Carlito",
		lineheight = lineheight,
		margin = marquee::trbl(0, bottom = marquee::rem(0.7))
	)
	style <- style |>
		marquee::modify_style(
			"h1",
			border = "transparent",
			size = marquee::relative(1.4),
			border_size = marquee::trbl(NULL),
			margin = marquee::trbl(NULL),
			padding = marquee::trbl(NULL)
		) |>
		marquee::modify_style("ul", padding = marquee::trbl(right = marquee::em(1))) |>
		marquee::modify_style("a", color = color)
	if (!is.null(align)) {
		style <- style |> marquee::modify_style("base", align = align)
	}
	style
}
