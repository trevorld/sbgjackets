#' Create playing card wallet for *Blorg in the Midwest*
#'
#' `pcw_blorg_in_the_midwest()` creates an origami playing card wallet for the game
#' *Blorg in the Midwest*.
#'
#' @inheritParams pnpmisc::pdf_create_wallet
#' @param output Output file name.  Defaults to `tempfile(fileext = ".pdf")`.
#' @param ... Should be empty.
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @export
pcw_blorg_in_the_midwest <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter")
) {
	check_dots_empty()
	check_sbgjackets_dependencies()
	current_dev <- grDevices::dev.cur()
	on.exit(restore_devices(current_dev), add = TRUE)

	text_col <- "black"

	url <- "https://boardgamegeek.com/filepage/145694/blorg-in-the-midwest-rules"
	bm_page_1 <- bm_cache_url(url, "Blorg_in_the_Midwest_rules.pdf", download = FALSE)
	bm_cover <- bm_extract(bm_page_1, 2537:3286, 217:706)
	bm_setup <- bm_extract(bm_page_1, 445:936, 1740:2316)
	setup_grob <- rasterGrob(bm_setup, width = unit(0.9, "npc"))
	front <- fullGrob(bm_cover)

	back_notes <- r"(
		# Contents

		* 9 double-sided cards
		* rule booklet

		# Setup

		* *Your* hand always starts with *Crash Site* and *Memory Eraser*
		* Randomly flip and shuffle the rest then place them as the *draw pile*
		* Deal top 4 cards as your *time line*

		![](setup_grob)

		# Links

		* <https://boardgamegeek.com/boardgame/220380/blorg-in-the-midwest>
	)"
	back <- gList(
		backNotesGrob(back_notes, col = text_col, size = "wallet", setup_grob = setup_grob)
	)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-NC-SA-4.0",
		title = "Blorg in the Midwest Playing Card Wallet"
	)
	credits <- r"(
		* *Blorg in the Midwest rules* by John Kean

		  + <https://boardgamegeek.com/filepage/145694/blorg-in-the-midwest-rules>
		  + Creative Commons Attribution-NonCommercial-ShareAlike 4.0 License
		  + Cover and setup illustrations cropped from page 1
	)"

	spine <- gList(
		fullGrob("white"),
		spineTextGrob("Blorg in the Midwest", col = text_col, size = "wallet"),
		spineIconGrob(1L, 25L, 1.62, col = text_col, size = "wallet"),
		creditsGrob(xmp, credits, icons = TRUE, size = "wallet")
	)

	output <- pdf_create_wallet_silent(
		output = output,
		front = front,
		back = back,
		spine = spine,
		paper = paper
	)
	pdf_polish_jacket(output, xmp = xmp)
}
