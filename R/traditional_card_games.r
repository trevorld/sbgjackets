#' Create playing card box jacket for traditional card games
#'
#' `pcbj_bridge()` creates a playing card box jacket for a Bridge deck.
#' `pcbj_pinochle()` creates a playing card box jacket for a Pinochle deck.
#'
#' @inheritParams pcbj_english_pattern
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @rdname traditional_card_games
#' @export
pcbj_bridge <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE,
	double = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	background_col <- "white"
	text_col <- "black"

	# https://www.worthpoint.com/worthopedia/1905-postcard-game-bridge-savile-276822716
	url <- "https://www.haroldschogger.com/histor11.jpg"
	bm_pic <- bm_cache_url(url, "bridge_postcards.jpg") |>
		bm_extract(22:496, 5:325)
	front <- fullGrob(bm_pic, height = 1)

	bm_pic <- bm_cache_url(url, "bridge_postcards.jpg") |>
		bm_extract(15:527, 757:1081)

	back <- fullGrob(bm_pic, width = 1)

	spine <- gList(
		spineTextGrob("Bridge", col = text_col, size = "poker"),
		spineIconGrob(4:4, 60, 3.88, text_col, size = "poker")
	)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "Bridge Playing Card Box Jacket"
	)
	credits <- c(
		"* *Reliable Series 9no 9335* by William Ritchie & Sons (c. 1908\u20131918) and *The Game of Bridge* by Savile Lumley (1905)",
		"",
		"  + https://www.haroldschogger.com/history.htm",
		"  + Public Domain in the USA"
	)

	inner <- creditsGrob(xmp, credits, icons = TRUE, size = "poker")

	if (double) {
		spine <- list(spine, spine)
	}
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

#' @rdname traditional_card_games
#' @export
pcbj_pinochle <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE,
	double = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()
	current_dev <- grDevices::dev.cur()
	on.exit(restore_devices(current_dev), add = TRUE)

	background_col <- "white"
	text_col <- "black"

	url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1d/Pinochle_meld.jpg/1280px-Pinochle_meld.jpg"
	bm_pic <- bm_cache_url(url, "pinochle.jpg") |>
		bm_trim(right = 300L)

	front <- fullGrob(bm_pic, height = 1)

	around_table <- data.frame(
		Rank = c("one per suit", "all eight"),
		J = c("40", "400"),
		Q = c("60", "600"),
		K = c("80", "800"),
		A = c("100", "1,000")
	)

	# gt::tab_options(container.height) does nothing?
	# gt::tab_options(container.padding.y) does nothing?
	around_table <- gt::gt(around_table) |>
		gt::opt_table_font(font = "Carlito", size = 10) |>
		gt::tab_options(
			table.align = "left"
		)

	back_notes <- c(
		"# Contents",
		"",
		"48 cards = 2 x 4 suits x 6 ranks (9, J, Q, K, 10, A)",
		"",
		"# Meld",
		"",
		"* Run (J, Q, K, 10, A in trump suit) = 150",
		"* Double Run (all J, Q, K, 10, A in trump) = 1,500",
		"* Royal Marriage (Q+K trump, not in run) = 40",
		"* Common Marriage (Q+K in same suit) = 20",
		"* Dix (9 in trump suit) = 10",
		"* Pinochle ({.black \u2660}Q+{.red \u2666}J) = 40",
		"* Double Pinochle ({.black \u2660}Q,{.black \u2660}Q,{.red \u2666}J,{.red \u2666}J) = 300",
		"* Aces, Kings, Queens, or Jacks Around",
		"",
		"  ![](around_table)"
	)
	back_notes <- paste(back_notes, collapse = "\n") |> marquee::marquee_glue(.trim = FALSE)
	back <- marquee::marquee_grob(
		back_notes,
		style = sbgjackets_style("poker", color = text_col) |>
			marquee::modify_style(
				"img",
				marquee::style(img_asp = 3.5)
			),
		width = unit(pnpmisc:::JACKET_POKER_FRONT_WIDTH, "in"),
		x = unit(1 / 8, "in"),
		y = unit(1, "npc") - unit(1 / 8, "in")
	)

	spine <- gList(
		spineTextGrob("Pinochle", col = text_col, size = "poker"),
		spineIconGrob(2:4, 45, 2.31, text_col, size = "poker")
	)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-SA-3.0",
		title = "Pinochle Playing Card Box Jacket"
	)
	credits <- c(
		"* *Pinochle meld* by AMK1211",
		"",
		"  + https://commons.wikimedia.org/wiki/File:Pinochle_meld.jpg",
		"  + Creative Commons Attribution-ShareAlike 3.0 Unported License",
		"  + Cropped to fit front cover"
	)

	inner <- creditsGrob(xmp, credits, icons = TRUE, size = "poker")

	if (double) {
		spine <- list(spine, spine)
	}
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

#' @rdname traditional_card_games
#' @export
pcbj_poker <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE,
	double = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	background_col <- "white"
	text_col <- "black"

	# https://commons.wikimedia.org/wiki/File:Cassius_Marcellus_Coolidge_-_Poker_Game_(1894).png
	url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7a/Cassius_Marcellus_Coolidge_-_Poker_Game_%281894%29.png/960px-Cassius_Marcellus_Coolidge_-_Poker_Game_%281894%29.png"
	bm_pic <- bm_cache_url(url, "dogs_playing_poker.png")
	front <- fullGrob(bm_pic, height = 1)

	back_notes <- c(
		"# Contents",
		"",
		"52+ cards = 4 suits x 13 ranks (plus 0+ jokers)",
		"",
		"# Standard Poker Hand Rankings",
		"",
		"1. Straight Flush",
		"1. Four of a Kind",
		"1. Full House",
		"1. Flush",
		"1. Straight",
		"1. Three of a Kind",
		"1. Two Pairs",
		"1. Pair",
		"1. High Card",
		"",
		"# Links",
		"",
		"* https://www.pagat.com/poker/"
	)
	back_notes <- paste(back_notes, collapse = "\n") |> marquee::marquee_glue(.trim = FALSE)
	back <- marquee::marquee_grob(
		back_notes,
		style = sbgjackets_style("poker", color = text_col),
		width = unit(pnpmisc:::JACKET_POKER_FRONT_WIDTH, "in"),
		x = unit(1 / 8, "in"),
		y = unit(1, "npc") - unit(1 / 8, "in")
	)

	spine <- gList(
		spineTextGrob("Poker", col = text_col, size = "poker"),
		spineIconGrob(2:10, 60, 2.43, text_col, size = "poker")
	)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "Poker Playing Card Box Jacket"
	)
	credits <- c(
		"* *Poker Game* by Cassius Marcellus Coolidge (1894)",
		"",
		"  + https://commons.wikimedia.org/wiki/File:Cassius_Marcellus_Coolidge_-_Poker_Game_(1894).png",
		"  + Public Domain in the USA"
	)

	inner <- creditsGrob(xmp, credits, icons = TRUE, size = "poker")

	if (double) {
		spine <- list(spine, spine)
	}
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
