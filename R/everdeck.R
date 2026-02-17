#' Create playing card box jacket for the Everdeck
#'
#' `pcbj_everdeck()` creates two playing card box jackets for the Everdeck.
#'
#' @inheritParams pcbj_english_pattern
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @export
pcbj_everdeck <- function(
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
	text_col <- "black"

	url <- "https://boardgamegeek.com/filepage/279177/packvelopes-storage-boxes"
	bm_pic <- cache_url(url, "Everdeck_Packvelopes.zip", download = FALSE) |>
		zip_extract_bm_pixmap("Red and Black.pdf") |>
		bm_extract(1076:1681, 1304:2130)
	front <- fullGrob(bm_pic, width = 1)

	back_notes <- r"(
		# Contents

		* 120 cards = 8 suits x 15 ranks

		  + 4 French suits plus Coins, Crowns, Moons, Stars
		  + Ranks: 0{en_dash}9, X, J, Q, K, A
		  + Points per suit: 5x1, 4x2, 3x3, 2x4, 1x5

		* Numbered from 1{en_dash}120

		  + 12 x 10 rightmost *colored* digits
		  + 10 x 12 leftmost *black* digits

		* Each card has unique name

		  - First letter has word game frequency
		  - Includes Tarot Major Arcana

		* 2 x 60 animal pictures

		  - Includes Hanafuda animals

		# Links

		* <https://thewrongtools.wordpress.com/2019/10/10/the-everdeck/>
	)"
	back <- backNotesGrob(back_notes, col = text_col, size = "poker")

	spine1 <- spineTextGrob("Everdeck (1/2)", col = text_col, size = "poker")
	spine2 <- spineTextGrob("Everdeck (2/2)", col = text_col, size = "poker")
	spine <- list(spine1, spine2)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-SA-3.0",
		title = "Everdeck Playing Card Box Jackets"
	)
	credits <- r"(
		* *The Everdeck* was designed by Wilhelm Su

		* These Playing Card Box Jackets are not affiliated, sponsored, nor endorsed by Wilhelm Su

		  * The use of a non-stylized {dQuote('Everdeck')} is intended as fair use to indicate that this playing card box is usable to store a {dQuote('Everdeck')} deck.
		* *Everdeck Packvelopes* by Brooks Boyd

		  + <https://boardgamegeek.com/profile/midnightlightning>
		  + <https://boardgamegeek.com/filepage/279177/packvelopes-storage-boxes>
		  + Creative Commons Attribution-ShareAlike 3.0 Unported License
		  + Cropped to fit front cover
	)"

	inner <- creditsGrob(xmp, credits, icons = FALSE, size = "poker")

	pdf_create_poker_jacket(
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
