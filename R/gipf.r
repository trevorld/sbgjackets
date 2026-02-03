#' Create SBG Jacket for YINSH
#'
#' `sbgj_yinsh()` creates a small box game jacket for [YINSH](https://www.gipf.com/yinsh/).
#'
#' Note that these print-and-play small box game jackets are for **Personal Use Only**.
#' @inheritParams sbgj_dominoes_all
#' @inheritParams pnpmisc::pdf_create_jacket
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @rdname sbgj_gipf
#' @export
sbgj_yinsh <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()
	current_dev <- grDevices::dev.cur()
	on.exit(restore_devices(current_dev), add = TRUE)

	background_col <- "white"
	text_col <- "white"

	url <- "https://boardgamegeek.com/image/3782516/yinsh"
	bm_pic <- bm_cache_url(url, "yinsh_bgg_image.jpg", download = FALSE)
	bm_pic <- bm_extract(bm_pic, 415:1308, 342:1400)
	front <- fullGrob(bm_pic, height = 1)

	back_notes <- c(
		"# Contents",
		"",
		"* 51 reversible white and black disc *markers*",
		"* 5 white and 5 black *rings*",
		"* 1 game board (may be too big for this box)",
		"* 1 rulebook",
		"",
		"# Rules Summary",
		"",
		'* Place pieces on board *intersections* including on the edges',
		'* A straight line of 5 *markers* of same color is a *row*',
		"",
		'* Setup: Place *markers* in a shared "pool" next to board and alternate placing all 10 *rings* on board with *white* going first',
		"* Players alternate moves:",
		"",
		"  1. Place *marker* from pool into one of your rings your color up",
		"  2. Move that ring in a straight line to vacant space",
		"",
		"     + May move over 1+ vacant spaces and/or *markers*",
		"     + If moving over *marker(s)* must stop at first possible vacant space in line",
		"     + May not move over another *ring*",
		"",
		'  3. Flip over any *markers* that ring "jumped" over (if any)',
		'  4. If any *row* exists then owning player removes *row* and *ring* of same color.  Repeat if necessary.',
		"",
		'     + If after a move both players have a *row* then player who just moved gets to remove *row* and *ring* first',
		"",
		"* End: whoever removes 3 rings first wins (or 1 ring in **blitz** version)",
		"",
		"  + If no one has won after all *markers* were placed then whoever removed the most rings wins else game ends in a draw"
	)
	back_notes <- paste(back_notes, collapse = "\n") |> marquee::marquee_glue(.trim = FALSE)
	back <- marquee::marquee_grob(
		back_notes,
		style = sbgjackets_style(color = "black"),
		width = unit(pnpmisc:::JACKET_4x6_FRONT_WIDTH, "in"),
		x = unit(1 / 8, "in"),
		y = unit(1, "npc") - unit(1 / 8, "in")
	)

	spine <- gList(
		fullGrob("#3C697EFF"),
		spineTextGrob("Yinsh", col = text_col),
		spineIconGrob(2, 60, 2.63, text_col)
	)

	xmp <- xmp(creator = "Trevor L. Davis", title = "Yinsh Small Box Game Jacket")
	credits <- c(
		"* YINSH was invented by Kris Burm https://www.gipf.com/yinsh/",
		"",
		"* YINSH has been published by several companies including HUCH!; DiceTree Games; Rio Grande Games; Smart Toys and Games, Inc.; and Don & Co.",
		"",
		"* This Small Box Game Jacket is not affiliated, sponsored, nor endorsed by either Kris Burm or any of the YINSH publishers",
		str_glue(
			"  * The use of a non-stylized {dQuote('Yinsh')} is intended as fair use to indicate that this 4x6 photo case is usable to store a {dQuote('Yinsh')} set."
		),
		"",
		"* *Yinsh* image by Wizzy Parkerir cropped to fit front cover",
		"",
		"  + https://boardgamegeek.com/image/3782516/yinsh",
		"  + CC0 1.0 Public Domain Dedication"
	)

	inner <- creditsGrob(xmp, credits, icons = TRUE)

	pdf_create_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper,
		bg = background_col
	) |>
		pdf_polish_jacket(xmp = xmp, instructions = instructions)
}
