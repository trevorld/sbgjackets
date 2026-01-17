#' Small Box Game Jackets `marquee` style
#'
#' `sbgjackets_style()` returns a `marquee` style set object
#' to help add text to jackets in a consistent style.
#'
#' @param size Either `"4x6"` for a 4x6 photo storage box jacket or `"poker"` for a playing card storage box.
#' @param ... Ignored
#' @param color The desired text color.
#' @return A `marquee` style set object.
#' @export
sbgjackets_style <- function(
	size = c("4x6", "poker"),
	...,
	color = "black"
) {
	check_dots_empty()
	size <- match.arg(size)
	if (size == "4x6") {
		base_size = 10
		lineheight = 1.6
	} else {
		base_size = 9
		lineheight = 1.4
	}
	if (packageVersion("marquee") >= "1.2.0") {
		# Don't manually set `bullets` to avoid #99
		style <- marquee::classic_style(
			base_size = base_size,
			body_font = "Carlito",
			color = color,
			header_font = "Carlito",
			lineheight = lineheight,
			margin = marquee::trbl(0, bottom = marquee::rem(0.7))
		)
	} else {
		# Manually set `bullets` to avoid #53
		style <- marquee::classic_style(
			base_size = base_size,
			body_font = "Carlito",
			color = color,
			header_font = "Carlito",
			lineheight = 1.6,
			margin = marquee::trbl(0, bottom = marquee::rem(0.7)),
			bullets = rep("\u2022", 6L)
		)
	}
	style |>
		marquee::modify_style(
			"h1",
			border = NA,
			size = marquee::relative(1.4),
			border_size = marquee::trbl(NULL),
			margin = marquee::trbl(NULL),
			padding = marquee::trbl(NULL)
		) |>
		marquee::modify_style("ul", padding = marquee::trbl(right = marquee::em(1)))
}
