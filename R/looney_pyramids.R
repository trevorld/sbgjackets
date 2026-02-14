#' Create SBG Jacket for Looney Pyramids
#'
#' `sbgj_looney_pyramids()` creates a small box game jacket for [Looney Pyramids](https://www.looneylabs.com/pyramids-home).
#' `sbgj_homeworlds()` creates a small box game jacket for [Homeworlds](https://www.looneylabs.com/games/homeworlds).
#' `sbgj_ice_duo()` creates a small box game jacket for [Ice Duo](https://www.looneylabs.com/games/ice-duo).
#' `sbgj_jinxx()` creates a small box game jacket for [Jinxx](https://www.looneylabs.com/games/jinxx).
#' `sbgj_martian_chess()` creates a small box game jacket for [Martian Chess](https://www.looneylabs.com/games/martian-chess).
#' `sbgj_nomids()` creates a small box game jacket for [Nomids](https://www.looneylabs.com/games/nomids).
#'
#' Note that these print-and-play small box game jackets are for **Personal Use Only**.
#' These jackets use images by Looney Labs.  These jackets are **not** for distribution.
#' For more information see the Looney Labs FAQ: <https://faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774>.
#'
#' @param output Output file name.  Defaults to `tempfile(fileext = ".pdf")`.
#' @param ... Should be empty.
#' @inheritParams sbgj_dominoes_all
#' @inheritParams pnpmisc::pdf_create_jacket
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @rdname sbgj_looney
#' @export
sbgj_looney_pyramids <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	background_col <- "#EFE8D5FF"
	text_col <- "#1C3160FF"

	url <- "https://www.looneylabs.com/sites/default/files/LP4%20Top%20Header.png"
	bm_logo <- bm_cache_url(url, "LP4TopHeader.png") |>
		bm_replace(text_col, "black") |>
		bm_replace(text_col, "#191A17FF") |>
		bm_replace(text_col, "#1C1D1AFF")

	url <- "https://www.looneylabs.com/sites/default/files/literature/Pyramids_Intro_Pamphlet7.pdf"
	pamphlet_path <- cache_url(url)

	bm_pamphlet1 <- pnpmisc::pdf_render_bm_pixmap(pamphlet_path, page = 1L)
	bm_pamphlet1 <- bm_pamphlet1[, seq.int(ncol(bm_pamphlet1) / 2 + 1, ncol(bm_pamphlet1))]
	bm_pamphlet1 <- cbind(
		bm_pamphlet1[, seq.int(ncol(bm_pamphlet1) / 2 + 1, ncol(bm_pamphlet1))],
		bm_pamphlet1[, seq.int(ncol(bm_pamphlet1) / 2)]
	)
	bm_pamphlet1[1:90, seq.int(ncol(bm_pamphlet1) / 2)] <- background_col
	bm_pamphlet1[1080:1200, 170:830] <- background_col
	bm_pamphlet1 <- bm_pamphlet1 |>
		bm_replace(background_col) |>
		bm_replace(background_col, "#EDE4D3FF") |>
		bm_replace(background_col, "#EEE5D4FF")

	bm_pamphlet2 <- pnpmisc::pdf_render_bm_pixmap(pamphlet_path, page = 2L)
	bm_pamphlet2 <- bm_pamphlet2[, seq.int(ceiling(0.75 * ncol(bm_pamphlet2)))] |>
		bm_replace(background_col) |>
		bm_replace(background_col, "#EEE5D4FF") |>
		bm_replace(background_col, "#EDE4D3FF")

	#   Alien City
	# * Apophis (Zark City deck, Uno deck, etc., d6, 5 tokens)
	# * Black Ice (3 lightning dice)
	# * BLAM! (chess board)
	# * Caldera (volcano) (4 x 6 trios plus 6 small black plus 5x5 grid)
	# * Closest Ghost (2 ludo boards, 16 pieces of candy)
	# * Freeze Tag (pyramid die, 5x5 grid)
	# * Give or Take (1 pyramid die) deprecated by Nomids
	#   Gleebs and Grues (4x4 grid)
	# * Gnostica
	# * Homeworlds (turn token, optional 3x4 grid) (BYRG)
	# * Hijinks (1 pyramid die, 3x3 board)
	# * Icehouse
	# * Ice Towers
	# * Jinxx (3x3 grid, pyramid die)
	# * Pharaoh (5x5 grid, 2 d6)
	# * Martian Chess (2 4x4 grid)
	# * Penguin Soccer (8x8 board)
	# * Pikemen (chess board)
	# * Powerhouse (3 tokens such as dice, cloth bag)
	#   Pylon (5x6 grid)
	# * Pyramid-Sham-Bo (color reminders)
	#   Quicksand (4 martian coasters)
	# * RAMbots (chessboard plus screens)
	# * Skurdir
	# * Solomids (5x10 grid plus cloth bag)
	# * StarRunners (2 d6)
	#   Synapse-Ice (5x6 grid)
	# * Tic Tac Doh! (3x3 grid)
	# * Treehouse
	# * Zarcana
	# * Zark City
	# * Zendo
	front <- gList()
	badges <- c(
		"BlackIce",
		"Blam",
		"Volcano",
		"ClosestGhost",
		"FreezeTag",
		"Gnostica",
		"Homeworlds",
		"IceHouse",
		"IceTowers",
		"Jinxx",
		"Pharaoh",
		"MartianChess",
		"PenguinSoccer",
		"Pikemen",
		"Powerhouse",
		"PyramidShamBo",
		"RAMbots",
		"Skurdir",
		"StarRunners",
		"TicTacDoh",
		"Treehouse",
		"Zarcana",
		"ZarkCity",
		"Zendo"
	)
	l_badges <- load_pyramid_badges()
	xs <- rep(1:4 / 4 - 1 / 8, 6L)
	ys <- 1 - rep(1:6 / 6 - 1 / 12, each = 4L)
	for (i in seq_along(badges)) {
		vp <- viewport(x = xs[i], y = ys[i], width = 1 / 4, height = 1 / 6)
		front[[i + 1L]] <- editGrob(l_badges[[badges[i]]], vp = vp)
	}

	back <- gList(
		rasterGrob(bm_logo, y = 0.96, just = "top", width = unit(0.94, "npc"), interpolate = FALSE),
		rasterGrob(bm_pamphlet1, y = 0.84, just = "top"),
		rasterGrob(bm_pamphlet2, y = 0.05, just = "bottom", width = 0.95)
	)

	xmp <- xmp(creator = "Trevor L. Davis", title = "Looney Pyramids Small Box Game Jacket")
	credits <- c(
		"* From https://www.looneylabs.com/",
		"",
		"  + *Looney Pyramids Intro Pamphlet* (cropped and edited)",
		# "  + *Looney Pyramids - Zoom*",
		"  + https://www.looneylabs.com/sites/default/files/LP4%20Top%20Header.png",
		"  + From https://www.looneylabs.com/pyramid-arcade-game-badges-boards",
		"",
		"    the badges of 24 Looney Pyramids games",
		"  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
		"",
		"    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law."
	)
	cr_grob <- creditsGrob(xmp, credits, icons = FALSE)
	inner <- gList(cr_grob, bank_grob())

	pdf_create_jacket(
		output = output,
		front = front,
		back = back,
		spine = spineTextGrob("Looney Pyramids", col = text_col),
		inner = inner,
		paper = paper,
		bg = background_col
	) |>
		pdf_polish_jacket(xmp = xmp, instructions = instructions)
}

bank_grob <- function() {
	xs <- unit(2 + 1.1 * seq(-1, 1, by = 1), "in")
	ys <- unit(3 + 1.1 * seq(-2, 2, by = 1), "in")
	w <- unit(9 / 16, "in")
	vp <- vp_inner_right()
	grobTree(
		rectGrob(
			xs,
			y = ys[5L],
			width = w,
			height = w,
			gp = gpar(col = "black", fill = "black", lwd = 4)
		),
		rectGrob(
			xs,
			y = ys[4L],
			width = w,
			height = w,
			gp = gpar(col = "black", fill = "#009e73", lwd = 4)
		), # bluish-green
		rectGrob(
			xs,
			y = ys[3L],
			width = w,
			height = w,
			gp = gpar(col = "black", fill = "#D55E00", lwd = 4)
		), # vermillion
		rectGrob(
			xs,
			y = ys[2L],
			width = w,
			height = w,
			gp = gpar(col = "black", fill = "#F0E442", lwd = 4)
		), # yellow
		rectGrob(
			xs,
			y = ys[1L],
			width = w,
			height = w,
			gp = gpar(col = "black", fill = "#0072B2", lwd = 4)
		), # blue
		segmentsGrob(
			x0 = rep(xs - 0.5 * w, 3L),
			x1 = rep(xs + 0.5 * w, 3L),
			y0 = rep(ys[4:2] + 0.5 * w, each = 3L),
			y1 = rep(ys[4:2] - 0.5 * w, each = 3L),
			gp = gpar(col = "black", lwd = 4)
		),
		segmentsGrob(
			x0 = rep(xs - 0.5 * w, 3L),
			x1 = rep(xs + 0.5 * w, 3L),
			y0 = rep(ys[4:2] - 0.5 * w, each = 3L),
			y1 = rep(ys[4:2] + 0.5 * w, each = 3L),
			gp = gpar(col = "black", lwd = 4)
		),
		# textGrob("X", x = rep(xs, 3L), y = rep(ys[2:4], each = 3L)),
		vp = vp
	)
}

pyramid_badges <- function() {
	badges <- c(
		"BlackIce",
		"ColorWheel",
		"GiveOrTake",
		"Hijinks",
		"Homeworlds",
		"IceDice",
		"IceTowers",
		"Launchpad23",
		"LooneyLudo",
		"LunarInvaders",
		"MartianChess",
		"PetalBattle",
		"PetriDish",
		"Pharaoh",
		"Powerhouse",
		"PyramidShamBo",
		"Treehouse",
		"TwinWin",
		"Verticality",
		"Volcano",
		"WorldWar5",
		"ZarkCity",
		"AquariusRising",
		"Blam",
		"CarrotsBroccoli",
		"FreezeTag",
		"Gnostica",
		"IceHouse",
		"PenguinSoccer",
		"RAMbots",
		"Skurdir",
		"Solomids",
		"Zendo",
		"Apophis",
		"BowlerRink",
		"ClosestGhost"
	)
	g_badges <- c("PyramidBall")
	patches <- c(
		"IceToids",
		"Jinxx",
		"Kickback",
		"LavaFlows",
		"Logger",
		"Nomids",
		"NothingBeats",
		"Pikemen",
		"Sandships",
		"StarRunners",
		"TicTacDoh",
		"Zarcana"
	)
	list(badges = badges, g_badges = g_badges, patches = patches)
}

load_pyramid_badges <- function() {
	pb <- pyramid_badges()

	l_badges <- list()

	for (badge in pb$badges) {
		Sys.sleep(1)
		url <- str_glue("https://www.looneylabs.com/sites/default/files/{badge}.png")
		l_badges[[badge]] <- bm_cache_url(url) |> rasterGrob()
	}
	for (badge in pb$g_badges) {
		Sys.sleep(1)
		f <- str_glue("{badge}.png")
		url <- str_glue("https://www.looneylabs.com/sites/default/files/{badge}.g.png")
		l_badges[[badge]] <- bm_cache_url(url, f) |> rasterGrob()
	}
	for (badge in pb$patches) {
		Sys.sleep(1)
		f <- str_glue("{badge}.png")
		url <- str_glue("https://www.looneylabs.com/sites/default/files/{badge}.patch_.png")
		l_badges[[badge]] <- bm_cache_url(url, f) |> rasterGrob()
	}
	l_badges
}

#' @rdname sbgj_looney
#' @export
sbgj_homeworlds <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	background_col <- "#EFE8D5FF"
	text_col <- "#1C3160FF"

	url <- "https://www.looneylabs.com/sites/default/files/marketing_images/HomeworldsBoxFront.jpg"
	bm_cover <- bm_cache_url(url) |>
		bm_trim(left = 10L, right = 10L)

	url <- "https://www.looneylabs.com/sites/default/files/marketing_images/HomeworldsBoxBack.jpg"
	bm_back <- bm_cache_url(url) |>
		bm_trim(left = 10L, right = 10L, bottom = 10L, top = 10L)

	front <- rasterGrob(bm_cover, height = 0.8)
	back <- rasterGrob(bm_back, height = 0.85)
	spine <- gList(
		spineTextGrob("Homeworlds", col = text_col),
		spineIconGrob(2, 60, 3.43, text_col)
	)

	xmp <- xmp(creator = "Trevor L. Davis", title = "Homeworlds Small Box Game Jacket")
	credits <- c(
		"* From https://www.looneylabs.com/resources/game/Homeworlds",
		"",
		"  + *Homeworlds Box Front*",
		"  + *Homeworlds Box Back*",
		"  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
		"",
		"    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law."
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

#' @rdname sbgj_looney
#' @export
sbgj_ice_duo <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	background_col <- "#EFE8D5FF"
	text_col <- "#1C3160FF"

	url <- "https://www.looneylabs.com/sites/default/files/marketing_images/IceDuoBoxFront.jpg"
	bm_cover <- bm_cache_url(url) |>
		bm_trim(left = 10L, right = 10L)

	url <- "https://www.looneylabs.com/sites/default/files/marketing_images/IceDuoBoxBack.jpg"
	bm_back <- bm_cache_url(url) |>
		bm_trim(left = 10L, right = 10L, bottom = 10L, top = 10L)

	front <- rasterGrob(bm_cover, height = 0.8)
	back <- rasterGrob(bm_back, height = 0.85)
	spine <- gList(
		spineTextGrob("Ice Duo", col = text_col),
		spineIconGrob(2, 30, 1.50, text_col)
	)

	xmp <- xmp(creator = "Trevor L. Davis", title = "Ice Duo Small Box Game Jacket")
	credits <- c(
		"* From https://www.looneylabs.com/resources/game/Ice%20Duo",
		"",
		"  + *Ice Duo Box Front*",
		"  + *Ice Duo Box Back*",
		"  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
		"",
		"    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law."
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

#' @rdname sbgj_looney
#' @export
sbgj_jinxx <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	background_col <- "#EFE8D5FF"
	text_col <- "#1C3160FF"

	url <- "https://www.looneylabs.com/sites/default/files/marketing_images/JinxxFlatBoxFront.png"
	bm_cover <- bm_cache_url(url) |>
		bm_trim(left = 300L, right = 300L) |>
		rasterGrob(height = 0.8)

	url <- "https://www.looneylabs.com/sites/default/files/marketing_images/JinxxFlatBoxBack.png"
	bm_back <- bm_cache_url(url) |>
		bm_trim(left = 300L, right = 300L) |>
		rasterGrob(height = 0.85)

	front <- bm_cover
	back <- bm_back
	spine <- gList(
		spineTextGrob("Jinxx", col = text_col),
		spineIconGrob(2:4, 30, 1.50, text_col)
	)

	xmp <- xmp(creator = "Trevor L. Davis", title = "Jinxx Small Box Game Jacket")
	credits <- c(
		"* From https://www.looneylabs.com/resources/game/Jinxx",
		"",
		"  + *Jinxx Box Front*",
		"  + *Jinxx Box Back*",
		"  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
		"",
		"    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law."
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

#' @rdname sbgj_looney
#' @param ... Should be left empty.
#' @param silver If `TRUE` make jacket for silver Martian Chess.
#' @export
sbgj_martian_chess <- function(
	output = NULL,
	...,
	silver = FALSE,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	background_col <- "#EFE8D5FF"
	text_col <- "#1C3160FF"

	url <- "https://www.looneylabs.com/sites/default/files/marketing_images/MartianChessBoxFront.jpg"
	bm_cover <- bm_cache_url(url)
	front <- rasterGrob(bm_cover)

	if (silver) {
		url <- "https://www.looneylabs.com/sites/default/files/marketing_images/MartianChessSilverFlatBoxBack.png"
		bm_back <- bm_cache_url(url) |>
			bm_trim(left = 300L, right = 300L)
		back <- rasterGrob(bm_back, height = 0.85)
	} else {
		url <- "https://www.looneylabs.com/sites/default/files/marketing_images/MartianChessBoxBack.jpg"
		bm_back <- bm_cache_url(url) |>
			bm_trim(bottom = 10L, top = 10L)
		back <- rasterGrob(bm_back)
	}

	spine <- gList(
		spineTextGrob("Martian Chess", col = text_col),
		spineIconGrob(2, 20, 2.42, text_col)
	)

	if (silver) {
		xmp <- xmp(
			creator = "Trevor L. Davis",
			title = "Martian Chess (Silver) Small Box Game Jacket"
		)
		credits <- c(
			"* From https://www.looneylabs.com/resources/game/Martian%20Chess",
			"",
			"  + *Martian Chess Box Front*",
			"  + *Martian Chess Silver Box Back*",
			"  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
			"",
			"    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law."
		)
	} else {
		xmp <- xmp(creator = "Trevor L. Davis", title = "Martian Chess Small Box Game Jacket")
		credits <- c(
			"* From https://www.looneylabs.com/resources/game/Martian%20Chess",
			"",
			"  + *Martian Chess Box Back*",
			"  + *Martian Chess Box Front*",
			"  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
			"",
			"    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law."
		)
	}
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

#' @rdname sbgj_looney
#' @param custom If `TRUE` use custom back imagery.
#' @export
sbgj_nomids <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE,
	custom = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	background_col <- "#EFE8D5FF"
	text_col <- "#1C3160FF"

	url <- "https://www.looneylabs.com/sites/default/files/marketing_images/NomidsBoxFront.jpg"
	bm_cover <- bm_cache_url(url)
	front <- rasterGrob(bm_cover)

	if (custom) {
		url <- "https://www.looneylabs.com/sites/default/files/marketing_images/NomidsLogo_Background.jpg"
		bm_logo <- bm_cache_url(url)

		url <- "https://www.looneylabs.com/sites/default/files/literature/Nomids_Rules6.pdf"
		bm_rules <- bm_cache_url(url)
		bm_rules <- bm_rules[, 1:(ncol(bm_rules) / 2)] |>
			bm_replace(background_col)

		url <- "https://www.looneylabs.com/sites/default/files/marketing_images/PyramidArcadeZoomBackground.jpg"
		bm_zoom <- bm_cache_url(url)

		back <- gList(
			rasterGrob(bm_logo, y = 1, just = "top", height = unit(0.15, "npc")),
			rasterGrob(bm_rules, y = 0.86, just = "top"),
			rasterGrob(bm_zoom, y = 0, just = "bottom")
		)
	} else {
		url <- "https://www.looneylabs.com/sites/default/files/marketing_images/NomidsBoxBack.jpg"
		bm_back <- bm_cache_url(url)
		back <- rasterGrob(bm_back)
	}

	spine <- gList(
		spineTextGrob("Nomids", col = text_col),
		spineIconGrob(2:10, 10, 1.0, text_col)
	)

	if (custom) {
		xmp <- xmp(
			creator = "Trevor L. Davis",
			title = "Nomids Small Box Game Jacket (Custom Back)"
		)
		credits <- c(
			"* From https://www.looneylabs.com/resources/game/Nomids",
			"",
			"  + *Nomids Logo with Background*",
			"  + *Nomids Box Front*",
			"  + *Nomids Rules* (cropped, background tweaked)",
			"  + *Looney Pyramids - Zoom*",
			"  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
			"",
			"    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law."
		)
	} else {
		xmp <- xmp(creator = "Trevor L. Davis", title = "Nomids Small Box Game Jacket")
		credits <- c(
			"* From https://www.looneylabs.com/resources/game/Nomids",
			"",
			"  + *Nomids Box Back*",
			"  + *Nomids Box Front*",
			"  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
			"",
			"    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law."
		)
	}
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
