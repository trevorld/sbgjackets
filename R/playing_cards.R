#' Create playing card box jacket for various playing card decks.
#'
#' `pcbj_bavarian_pattern()` creates a playing card box jacket for a German-suited deck in the Bavarian pattern.
#' `pcbj_castilian_pattern()` creates a playing card box jacket for a Spanish-suited deck in the Castilian pattern.
#' `pcbj_english_pattern()` creates a playing card box jacket for a French-suited deck in the English pattern.
#'
#' @inheritParams sbgj_dominoes_all
#' @inheritParams pnpmisc::pdf_create_jacket
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @param double If `TRUE` produce two jackets instead of one jacket.
#' @rdname pcbj_playing_cards
#' @export
pcbj_bavarian_pattern <- function(
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

	# Actual Bavarian Pattern cards are 2.25 inches wide and
	# the clear plastic box I received them in are a bit narrower and
	# taller than the poker boxes I have
	BAVARIAN_WIDTH <- 2.3
	BAVARIAN_DELTA <- pnpmisc:::JACKET_POKER_FRONT_WIDTH - BAVARIAN_WIDTH

	url <- "https://upload.wikimedia.org/wikipedia/commons/8/89/Eduard_B%C3%BCttner_Berlin_Bayrisches_Einfachbild.jpg"
	bm_pic <- bm_cache_url(url, "bavarian_pattern.jpg") |>
		bm_extract(10:870, 1020:1500)
	front <- rasterGrob(
		bm_pic,
		height = 1,
		x = unit(0.5, "npc") - unit(BAVARIAN_DELTA, "in"),
		vp = viewport(just = "center", width = unit(BAVARIAN_WIDTH, "in"))
	)

	back_notes <- r"(
		# Contents

		* 36 cards (4 suits x 9 ranks)

		  + Suits: Acorns, Leaves, Hearts, Bells
		  + Ranks: 6{en_dash}10, Under, Over, King, Ace
		  + Common to remove the 6's and often the 7's and 8's as well

		# Notable games for this deck

		* Bavarian Schafkopf
		* Bavarian Tarock
		* Dreeg
		* Sechsundsechzig (66)
		* Skat
		* Watten
	)"
	back_notes <- trim_multistring(back_notes)
	back <- marquee::marquee_grob(
		back_notes,
		style = sbgjackets_style("poker", color = text_col),
		width = unit(BAVARIAN_WIDTH, "in"),
		x = unit(1 / 8 + BAVARIAN_DELTA, "in"),
		y = unit(1, "npc") - unit(1 / 8, "in"),
	)

	spine <- spineTextGrob("German Suits: Bavarian Pattern", col = text_col, size = "poker")

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "German Suits: Bavarian Pattern Playing Card Box Jacket"
	)
	credits <- r"(
		* *Spielkartensatz Bayrisches Einfachbild* by Herstellers Eduard B{u_umlaut}ttner, Berlin

		  + Public domain (published between 1895 and 1907).
		  + Cropped to fit front cover.
		  + <https://commons.wikimedia.org/wiki/File:Eduard_B%C3%BCttner_Berlin_Bayrisches_Einfachbild.jpg>
		  + <https://www.froja.de/karten/karten.php?menu_id=2_4>
	)"

	inner <- creditsGrob(
		xmp,
		credits,
		icons = FALSE,
		size = "poker",
		width = unit(2 * BAVARIAN_WIDTH, "in"),
		x = unit(1 / 8 + BAVARIAN_DELTA, "in")
	)

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

#' @rdname pcbj_playing_cards
#' @export
pcbj_castilian_pattern <- function(
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

	# https://commons.wikimedia.org/wiki/Category:Castilian_pattern
	url <- "https://museotik.euskadi.eus/contenidos/cultural_asset/museotik_ca_64272/es_def/media/images/0901a0fe80239d26.jpg"
	bm_pic <- bm_cache_url(url, "fournier_1907.jpg") |>
		bm_extract(59:402, 301:514)

	front <- fullGrob(bm_pic, height = 1)

	back_notes <- r"(
		# Contents

		* 40 cards (4 suits x 10 ranks)

		  + 4 Spanish suits: Swords (espadas), Clubs (bastos), Cups (copas), Coins (oros)
		  + 10 ranks: 1{en_dash}7, jack/page (sota), knight/horse (caballo), king (rey)
		  + 48 card decks add an 8 and 9 per suit, 50 card decks also add two jokers.

		# Notable games for this deck

		* Brisca
		* Escoba
		* Mus
		* Siete y Media
		* Tresillo (modern variant of l'Hombre)
		* Tute
	)"
	back <- backNotesGrob(back_notes, col = text_col, size = "poker")

	spine <- spineTextGrob("Spanish Suits: Castilian Pattern", col = text_col, size = "poker")

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "Spanish Suits: Castilian Pattern Playing Card Box Jacket"
	)
	credits <- r"(
		* *Baraja Heraclio Fournier 1907* by Heraclio Fournier Gonz{a_acute}lez (1907).

		  + Public domain (author died on July 28th, 1916).
		  + Cropped to fit front cover.
		  + <https://museotik.euskadi.eus/coleccion/-/autoria-fournier-gonzalez-heraclio/titulo-baraja-heraclio-fournier-1907/objeto-baraja/museotik-ca-64272/>
	)"

	inner <- creditsGrob(xmp, credits, icons = FALSE, size = "poker")

	if (double) {
		spine <- list(spine, spine)
	}
	output <- pdf_create_poker_jacket(
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

#' @rdname pcbj_playing_cards
#' @export
pcbj_english_pattern <- function(
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

	url <- "https://museotik.euskadi.eus/contenidos/cultural_asset/museotik_ca_65097/es_def/media/images/0901a0fe80607e66.jpg"
	bm_pic <- bm_cache_url(url, "worshipful_1897.jpg") |>
		bm_extract(125:1137, 77:768)

	front <- fullGrob(bm_pic, height = 1)

	back_notes <- r"(
		The *English Pattern* is the most common deck of traditional playing cards.

		# Contents

		* 52+ cards (4 suits x 13 ranks plus jokers)

		  + 4 French suits: Spades, Diamonds, Clubs, Hearts
		  + 13 ranks: Ace, 2{en_dash}10, Jack, Queen, King
		  + Usually 1{en_dash}8 jokers (commonly two)

		# Links

		* <https://boardgamegeek.com/boardgamefamily/98/components-traditional-playing-cards>
		* <https://i-p-c-s.org/pattern/ps-48.html>
		* <https://www.pagat.com>
	)"
	back <- backNotesGrob(back_notes, col = text_col, size = "poker")

	spine <- spineTextGrob("French Suits: English Pattern", col = text_col, size = "poker")

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "French Suits: English Pattern Playing Card Box Jacket"
	)
	credits <- r"(
		* *Worshipful 1897* by Worshipful Company of the Makers of Playing Cards (1897).

		  + Public domain.
		  + Cropped to fit front cover.
		  + <https://museotik.euskadi.eus/coleccion/-/autoria-worshipful-company-of-the-makers-of-playing-cards/titulo-worshipful-1897/objeto-baraja/museotik-ca-65097/>
	)"

	inner <- creditsGrob(xmp, credits, icons = FALSE, size = "poker")

	if (double) {
		spine <- list(spine, spine)
	}
	output <- pdf_create_poker_jacket(
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
