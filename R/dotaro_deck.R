#' Create playing card box jackets for the Dotaro Deck
#'
#' `pcbj_dotaro_deck()` creates two playing card box jackets for the
#' Dotaro Deck.
#'
#' @inheritParams pcbj_english_pattern
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @export
pcbj_dotaro_deck <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()
	stopifnot(requireNamespace("dotaro.deck", quietly = TRUE))
	current_dev <- grDevices::dev.cur()
	on.exit(restore_devices(current_dev), add = TRUE)

	background_col <- "white"
	text_col <- "black"

	cfg <- dotaro.deck::dotaro_decks()$dotaro_full_traditional
	front <- piecepackr::pieceGrob(
		"card_back",
		suit = 1,
		rank = 1,
		cfg = cfg,
		width = unit(2.25, "in"),
		height = unit(3.5, "in")
	)

	back_notes <- r"(
		# Contents

		108 card multideck:

		* Each card has a light half and a dark half
		* Each suit x rank has a light and dark half on different cards
		  (except light/dark halves of knights on same card)
		* 112 "traditional" halves = 2 x 4 suits x 14 ranks
		* 100 "number" halves = 2 x 5 suits x 10 ranks
		* 4 unsuited "fool" halves = 2 x 2 shapes

		Can be used to play variety of card game systems:

		* one/two decks of French-suited playing cards
		* one deck of French-suited tarot cards
		* one double-nine set of dominoes
		* Pyramid decks e.g. 1 one, 2 twos, ..., 10 tens
		* Sequential deck with ordering up to 100 cards

		# Links

		* <https://github.com/trevorld/dotaro.deck>
	)"
	back <- backNotesGrob(back_notes, col = text_col, size = "poker")

	spine1 <- spineTextGrob("Dotaro Deck (1/2)", col = text_col, size = "poker")
	spine2 <- spineTextGrob("Dotaro Deck (2/2)", col = text_col, size = "poker")
	spine <- list(spine1, spine2)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "Dotaro Deck Playing Card Box Jackets"
	)
	credits <- r"(
		* The *Dotaro Deck* card back was designed by Trevor L. Davis and generated in R using the {dQuote('dotaro.deck')} package

		  + <https://github.com/trevorld/dotaro.deck>
		  + MIT license
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
