#' Create playing card box jacket for various playing card decks.
#'
#' `pcbj_bavarian_pattern()` creates a playing card box jacket for a German-suited deck in the Bavarian pattern.
#' `pcbj_castilian_pattern()` creates a playing card box jacket for a Spanish-suited deck in the Castilian pattern.
#' `pcbj_english_pattern()` creates a playing card box jacket for a French-suited deck in the English pattern.
#' `pcbj_playing_cards_all()` creates all of those into a single pdf file.
#'
#' @inheritParams sbgj_dominoes_all
#' @inheritParams pnpmisc::pdf_create_jacket
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @rdname pcbj_playing_cards
#' @export
pcbj_playing_cards_all <- function(
	output = NULL,
	...,
	paper = c("letter", "a4"),
	instructions = FALSE
) {
	check_dots_empty()
	assert_runtime_dependencies()

	paper <- tolower(paper)
	paper <- match.arg(paper)
	output <- pnpmisc:::normalize_output(output)

	title <- c(
		"French Suits: English Pattern",
		"German Suits: Bavarian Pattern",
		"Spanish Suits: Castilian Pattern"
	)
	bm <- bm_from_title(title, instructions)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "Playing Cards Box Jackets"
	)

	bavarian <- pcbj_bavarian_pattern(paper = paper)
	castilian <- pcbj_castilian_pattern(paper = paper)
	english <- pcbj_english_pattern(paper = paper)
	output_c <- tempfile(fileext = ".pdf")
	qpdf::pdf_combine(c(english, bavarian, castilian), output_c)
	if (instructions) {
		prepend_instructions(output_c, paper = paper)
	}
	output_c |>
		pnpmisc::pdf_set_bookmarks(bookmarks = bm) |>
		pnpmisc::pdf_set_xmp(xmp = xmp) |>
		pnpmisc::pdf_set_docinfo(docinfo = as_docinfo(xmp)) |>
		pnpmisc::pdf_compress(output, linearize = TRUE)

	pnpmisc::rm_temp_pdfs()

	invisible(output)
}


#' @rdname pcbj_playing_cards
#' @export
pcbj_bavarian_pattern <- function(
	output = NULL,
	...,
	paper = c("letter", "a4"),
	instructions = FALSE
) {
	check_dots_empty()
	assert_runtime_dependencies()

	paper <- tolower(paper)
	paper <- match.arg(paper)
	output <- pnpmisc:::normalize_output(output)

	background_col <- "white"
	text_col <- "black"

	current_dev <- grDevices::dev.cur()
	if (current_dev > 1) {
		on.exit(grDevices::dev.set(current_dev), add = TRUE)
	} else {
		on.exit(grDevices::graphics.off(), add = TRUE)
	}

	# Actual Bavarian Pattern cards are 2.25 inches wide and
	# the clear plastic box I received them in are a bit narrower and
	# taller than the poker boxes I have
	BAVARIAN_WIDTH <- 2.3
	BAVARIAN_DELTA <- pnpmisc:::JACKET_POKER_FRONT_WIDTH - BAVARIAN_WIDTH

	dir <- get_data_dir()
	# https://commons.wikimedia.org/wiki/Category:Castilian_pattern
	pic <- normalizePath(file.path(dir, "bavarian_pattern.jpg"), mustWork = FALSE)

	if (!file.exists(pic)) {
		download.file(
			"https://upload.wikimedia.org/wikipedia/commons/8/89/Eduard_B%C3%BCttner_Berlin_Bayrisches_Einfachbild.jpg",
			pic
		)
	}
	bm_pic <- magick::image_read(pic) |>
		as_bm_pixmap()
	bm_pic <- bm_pic[10:870, 1020:1500]
	front <- rasterGrob(
		bm_pic,
		height = 1,
		x = unit(0.5, "npc") - unit(BAVARIAN_DELTA, "in"),
		vp = viewport(just = "center", width = unit(BAVARIAN_WIDTH, "in"))
	)

	back_notes <- c(
		"# Contents",
		"",
		"* 36 cards (4 suits x 9 ranks)",
		"",
		"  + Suits: Acorns, Leaves, Hearts, Bells",
		"  + Ranks: 6\u201310, Under, Over, King, Ace",
		"  + Common to remove the 6's and often the 7's and 8's as well",
		"",
		"# Notable games for this deck",
		"",
		"* Bavarian Schafkopf",
		"* Bavarian Tarock",
		"* Dreeg",
		"* Sechsundsechzig (66)",
		"* Skat",
		"* Watten"
	)
	back_notes <- paste(back_notes, collapse = "\n") |> marquee::marquee_glue(.trim = FALSE)
	mg <- marquee::marquee_grob(
		back_notes,
		style = credits_style("poker", color = text_col),
		width = unit(BAVARIAN_WIDTH, "in"),
		x = unit(1 / 8 + BAVARIAN_DELTA, "in"),
		y = unit(1, "npc") - unit(1 / 8, "in"),
	)

	back <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), mg)
	spine <- gList(
		rectGrob(gp = gpar(col = NA, fill = background_col)),
		spineTextGrob("German Suits: Bavarian Pattern", col = text_col, size = "poker")
	)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "German Suits: Bavarian Pattern Playing Card Box Jacket"
	)
	credits <- c(
		"* *Spielkartensatz Bayrisches Einfachbild* by Herstellers Eduard B\u00fcttner, Berlin",
		"",
		"  + Public domain (published between 1895 and 1907).",
		"  + Cropped to fit front cover.",
		"  + https://commons.wikimedia.org/wiki/File:Eduard_B%C3%BCttner_Berlin_Bayrisches_Einfachbild.jpg",
		"  + https://www.froja.de/karten/karten.php?menu_id=2_4"
	)

	inner <- creditsGrob(
		xmp,
		credits,
		icons = FALSE,
		size = "poker",
		width = unit(2 * BAVARIAN_WIDTH, "in"),
		x = unit(1 / 8 + BAVARIAN_DELTA, "in")
	)

	output <- pdf_create_poker_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper
	)
	if (instructions) {
		prepend_instructions(output, paper = paper)
	}

	set_xmp(xmp, output)
	set_docinfo(as_docinfo(xmp), output)
	invisible(output)
}

#' @rdname pcbj_playing_cards
#' @export
pcbj_castilian_pattern <- function(
	output = NULL,
	...,
	paper = c("letter", "a4"),
	instructions = FALSE
) {
	check_dots_empty()
	assert_runtime_dependencies()

	paper <- tolower(paper)
	paper <- match.arg(paper)
	output <- pnpmisc:::normalize_output(output)

	background_col <- "white"
	text_col <- "black"

	current_dev <- grDevices::dev.cur()
	if (current_dev > 1) {
		on.exit(grDevices::dev.set(current_dev), add = TRUE)
	} else {
		on.exit(grDevices::graphics.off(), add = TRUE)
	}

	dir <- get_data_dir()
	# https://commons.wikimedia.org/wiki/Category:Castilian_pattern
	pic <- normalizePath(file.path(dir, "fournier_1907.jpg"), mustWork = FALSE)

	if (!file.exists(pic)) {
		download.file(
			"https://museotik.euskadi.eus/contenidos/cultural_asset/museotik_ca_64272/es_def/media/images/0901a0fe80239d26.jpg",
			pic
		)
	}
	bm_pic <- magick::image_read(pic) |>
		as_bm_pixmap()
	bm_pic <- bm_pic[59:402, 301:514]
	bm_pic <- rasterGrob(bm_pic, height = 1)

	front <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_pic)))

	back_notes <- c(
		"# Contents",
		"",
		"* 40 cards (4 suits x 10 ranks)",
		"",
		"  + 4 Spanish suits: Swords (espadas), Clubs (bastos), Cups (copas), Coins (oros)",
		"  + 10 ranks: 1\u20137, jack/page (sota), knight/horse (caballo), king (rey)",
		"  + 48 card decks add an 8 and 9 per suit, 50 card decks also add two jokers.",
		"",
		"# Notable games for this deck",
		"",
		"* Brisca",
		"* Escoba",
		"* Mus",
		"* Siete y Media",
		"* Tresillo (modern variant of l'Hombre)",
		"* Tute"
	)
	back_notes <- paste(back_notes, collapse = "\n") |> marquee::marquee_glue(.trim = FALSE)
	mg <- marquee::marquee_grob(
		back_notes,
		style = credits_style("poker", color = text_col),
		width = unit(pnpmisc:::JACKET_POKER_FRONT_WIDTH, "in"),
		x = unit(1 / 8, "in"),
		y = unit(1, "npc") - unit(1 / 8, "in")
	)

	back <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), mg)
	spine <- gList(
		rectGrob(gp = gpar(col = NA, fill = background_col)),
		spineTextGrob("Spanish Suits: Castilian Pattern", col = text_col, size = "poker")
	)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "Spanish Suits: Castilian Pattern Playing Card Box Jacket"
	)
	credits <- c(
		"* *Baraja Heraclio Fournier 1907* by Heraclio Fournier Gonz\u00e1lez (1907).",
		"",
		"  + Public domain (author died on July 28th, 1916).",
		"  + Cropped to fit front cover.",
		"  + https://museotik.euskadi.eus/coleccion/-/autoria-fournier-gonzalez-heraclio/titulo-baraja-heraclio-fournier-1907/objeto-baraja/museotik-ca-64272/"
	)

	inner <- creditsGrob(xmp, credits, icons = FALSE, size = "poker")

	output <- pdf_create_poker_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper
	)
	if (instructions) {
		prepend_instructions(output, paper = paper)
	}

	set_xmp(xmp, output)
	set_docinfo(as_docinfo(xmp), output)
	invisible(output)
}

#' @rdname pcbj_playing_cards
#' @export
pcbj_english_pattern <- function(
	output = NULL,
	...,
	paper = c("letter", "a4"),
	instructions = FALSE
) {
	check_dots_empty()
	assert_runtime_dependencies()

	paper <- tolower(paper)
	paper <- match.arg(paper)
	output <- pnpmisc:::normalize_output(output)

	background_col <- "white"
	text_col <- "black"

	current_dev <- grDevices::dev.cur()
	if (current_dev > 1) {
		on.exit(grDevices::dev.set(current_dev), add = TRUE)
	} else {
		on.exit(grDevices::graphics.off(), add = TRUE)
	}

	dir <- get_data_dir()
	# https://commons.wikimedia.org/wiki/Category:Castilian_pattern
	pic <- normalizePath(file.path(dir, "worshipful_1897.jpg"), mustWork = FALSE)

	if (!file.exists(pic)) {
		download.file(
			"https://museotik.euskadi.eus/contenidos/cultural_asset/museotik_ca_65097/es_def/media/images/0901a0fe80607e66.jpg",
			pic
		)
	}
	bm_pic <- magick::image_read(pic) |>
		as_bm_pixmap()
	bm_pic <- bm_pic[125:1137, 77:768]
	bm_pic <- rasterGrob(bm_pic, height = 1)

	front <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_pic)))

	back_notes <- c(
		"The *English Pattern* is the most common deck of traditional playing cards.",
		"",
		"# Contents",
		"",
		"* 52+ cards (4 suits x 13 ranks plus jokers)",
		"",
		"  + 4 French suits: Spades, Diamonds, Clubs, Hearts",
		"  + 13 ranks: Ace, 2\u201310, Jack, Queen, King",
		"  + Usually 1\u20138 jokers (commonly two)",
		"",
		"# Links",
		"",
		"* https://boardgamegeek.com/boardgamefamily/98/components-traditional-playing-cards",
		"* https://i-p-c-s.org/pattern/ps-48.html",
		"* https://www.pagat.com"
	)
	back_notes <- paste(back_notes, collapse = "\n") |> marquee::marquee_glue(.trim = FALSE)
	mg <- marquee::marquee_grob(
		back_notes,
		style = credits_style("poker", color = text_col),
		width = unit(pnpmisc:::JACKET_POKER_FRONT_WIDTH, "in"),
		x = unit(1 / 8, "in"),
		y = unit(1, "npc") - unit(1 / 8, "in")
	)

	back <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), mg)
	spine <- gList(
		rectGrob(gp = gpar(col = NA, fill = background_col)),
		spineTextGrob("French Suits: English Pattern", col = text_col, size = "poker")
	)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "French Suits: English Pattern Playing Card Box Jacket"
	)
	credits <- c(
		"* *Worshipful 1897* by Worshipful Company of the Makers of Playing Cards (1897).",
		"",
		"  + Public domain.",
		"  + Cropped to fit front cover.",
		"  + https://museotik.euskadi.eus/coleccion/-/autoria-worshipful-company-of-the-makers-of-playing-cards/titulo-worshipful-1897/objeto-baraja/museotik-ca-65097/"
	)

	inner <- creditsGrob(xmp, credits, icons = FALSE, size = "poker")

	output <- pdf_create_poker_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper
	)
	if (instructions) {
		prepend_instructions(output, paper = paper)
	}

	set_xmp(xmp, output)
	set_docinfo(as_docinfo(xmp), output)
	invisible(output)
}

#' @rdname pcbj_playing_cards
#' @export
pcbj_english_pattern <- function(
	output = NULL,
	...,
	paper = c("letter", "a4"),
	instructions = FALSE
) {
	check_dots_empty()
	assert_runtime_dependencies()

	paper <- tolower(paper)
	paper <- match.arg(paper)
	output <- pnpmisc:::normalize_output(output)

	background_col <- "white"
	text_col <- "black"

	current_dev <- grDevices::dev.cur()
	if (current_dev > 1) {
		on.exit(grDevices::dev.set(current_dev), add = TRUE)
	} else {
		on.exit(grDevices::graphics.off(), add = TRUE)
	}

	dir <- get_data_dir()
	# https://commons.wikimedia.org/wiki/Category:Castilian_pattern
	pic <- normalizePath(file.path(dir, "worshipful_1897.jpg"), mustWork = FALSE)

	if (!file.exists(pic)) {
		download.file(
			"https://museotik.euskadi.eus/contenidos/cultural_asset/museotik_ca_65097/es_def/media/images/0901a0fe80607e66.jpg",
			pic
		)
	}
	bm_pic <- magick::image_read(pic) |>
		as_bm_pixmap()
	bm_pic <- bm_pic[125:1137, 77:768]
	bm_pic <- rasterGrob(bm_pic, height = 1)

	front <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_pic)))

	back_notes <- c(
		"The *English Pattern* is the most common deck of traditional playing cards.",
		"",
		"# Contents",
		"",
		"* 52+ cards (4 suits x 13 ranks plus jokers)",
		"",
		"  + 4 French suits: Spades, Diamonds, Clubs, Hearts",
		"  + 13 ranks: Ace, 2\u201310, Jack, Queen, King",
		"  + Usually 1\u20138 jokers (commonly two)",
		"",
		"# Links",
		"",
		"* https://boardgamegeek.com/boardgamefamily/98/components-traditional-playing-cards",
		"* https://i-p-c-s.org/pattern/ps-48.html",
		"* https://www.pagat.com"
	)
	back_notes <- paste(back_notes, collapse = "\n") |> marquee::marquee_glue(.trim = FALSE)
	mg <- marquee::marquee_grob(
		back_notes,
		style = credits_style("poker", color = text_col),
		width = unit(pnpmisc:::JACKET_POKER_FRONT_WIDTH, "in"),
		x = unit(1 / 8, "in"),
		y = unit(1, "npc") - unit(1 / 8, "in")
	)

	back <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), mg)
	spine <- gList(
		rectGrob(gp = gpar(col = NA, fill = background_col)),
		spineTextGrob("French Suits: English Pattern", col = text_col, size = "poker")
	)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "French Suits: English Pattern Playing Card Box Jacket"
	)
	credits <- c(
		"* *Worshipful 1897* by Worshipful Company of the Makers of Playing Cards (1897).",
		"",
		"  + Public domain.",
		"  + Cropped to fit front cover.",
		"  + https://museotik.euskadi.eus/coleccion/-/autoria-worshipful-company-of-the-makers-of-playing-cards/titulo-worshipful-1897/objeto-baraja/museotik-ca-65097/"
	)

	inner <- creditsGrob(xmp, credits, icons = FALSE, size = "poker")

	output <- pdf_create_poker_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper
	)
	if (instructions) {
		prepend_instructions(output, paper = paper)
	}

	set_xmp(xmp, output)
	set_docinfo(as_docinfo(xmp), output)
	invisible(output)
}
