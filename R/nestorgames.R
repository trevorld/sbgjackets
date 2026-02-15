#' Create SBG Jacket for nestorgames products
#'
#' `sbgj_nestortiles()` creates a small box game jacket for
#' [nestortiles](https://boardgamegeek.com/boardgame/74615/nestortiles).
#' `sbgj_shibumi()` creates a small box game jacket for [Shibumi](https://cambolbro.com/games/shibumi/).
#'
#' Note that these print-and-play small box game jackets are for **Personal Use Only**.
#' @inheritParams sbgj_dominoes_all
#' @inheritParams pnpmisc::pdf_create_jacket
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @rdname sbgj_nestorgames
#' @export
sbgj_nestortiles <- function(
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

	url <- "https://spielstein.com/images/games/keks/keks_photo.jpg"
	bm_pic <- bm_cache_url(url)
	front <- fullGrob(bm_pic, height = 1)

	back_notes <- c(
		"# Contents",
		"",
		"* 60 interlocking panels = 10 colors x 1\u20136 dice pips",
		"",
		"# Notable games using nestortiles",
		"",
		"* *Cuboid* by Stephen Tavener",
		"",
		"  + https://boardgamegeek.com/boardgame/88844/cuboid",
		"",
		"* *Domina 4* by N\u00e9stor Romeral Andr\u00e9s",
		"",
		"  + https://boardgamegeek.com/boardgame/39992/domina-4",
		"",
		"* *Keks* by Dieter Stein",
		"",
		"  + https://spielstein.com/games/keks",
		"",
		"* *Myrmidons* by N\u00e9stor Romeral Andr\u00e9s",
		"",
		"  + https://boardgamegeek.com/boardgame/83750/myrmidons",
		"",
		"* *Nestortiles DUEL* by N\u00e9stor Romeral Andr\u00e9s",
		"",
		"  + https://boardgamegeek.com/boardgame/76107/",
		"",
		"* *Octopus* by N\u00e9stor Romeral Andr\u00e9s",
		"",
		"  + https://boardgamegeek.com/boardgame/76202/",
		"",
		"* *Roll Your Own* by Mark R. Brown",
		"",
		"  + https://boardgamegeek.com/boardgame/76695/roll-your-own",
		"",
		"* *Temple* by Martin Windischer",
		"",
		"  + https://boardgamegeek.com/boardgame/84745/temple",
		"",
		"* *Textile* by Dieter Stein",
		"",
		"  + https://spielstein.com/games/textile",
		"",
		"* *Tilting Dice* by James Ryan",
		"",
		"  + https://boardgamegeek.com/boardgame/88605/tilting-dice",
		"",
		"* *Topologic* by Dieter Stein",
		"",
		"  + https://spielstein.com/games/topologic"
	)
	back <- backNotesGrob(back_notes, col = "black")

	spine <- gList(
		fullGrob("black"),
		spineTextGrob("nestortiles", col = text_col)
	)

	xmp <- xmp(creator = "Trevor L. Davis", title = "nestortiles Small Box Game Jacket")
	credits <- c(
		"* *One possible Keks setup* by Dieter Stein",
		"",
		"  + https://spielstein.com/games/keks",
		"  + Creative Commons Attribution-NonCommercial 4.0 License",
		"  + Cropped to fit front cover",
		"",
		"* nestortiles was published by nestorgames",
		"",
		"* This Small Box Game Jacket is not affiliated, sponsored, nor endorsed by nestorgames",
		str_glue(
			"  * The use of a non-stylized {dQuote('nestortiles')} is intended as fair use to indicate that this 4x6 photo case is usable to store a nestortiles set."
		)
	)

	inner <- creditsGrob(xmp, credits, icons = FALSE)

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

#' @rdname sbgj_nestorgames
#' @export
sbgj_shibumi <- function(
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

	game <- ppn::read_ppn(system.file("ppn/spoff.ppn", package = "ppn"))[[1L]]
	df_front <- game$dfs[["Setup...."]]

	df_back <- data.frame(
		piece_side = c("board_face", rep_len("bit_back", 16 * 3)),
		x = c(2, rep(1:4, 4L), rep(5:8, 8L)),
		y = c(2, rep(5:8, each = 4L), rep(8:1, each = 4L)),
		suit = c(7L, rep(1L, 16L), rep(2L, 16L), rep(6L, 16L)),
		rank = c(4L, rep(9L, 3 * 16)),
		scale = c(1, rep(0.95, 3 * 16)),
		cfg = "marbles"
	)

	envir <- piecepackr::game_systems(round = TRUE, shading = TRUE)

	l_front <- piecepackr::render_piece(
		df_front,
		dev = ragg::agg_capture,
		image = "nativeRaster",
		envir = envir,
		op_scale = 0.5,
		trans = piecepackr::marbles_transform
	)
	l_back <- piecepackr::render_piece(
		df_back,
		dev = ragg::agg_capture,
		image = "nativeRaster",
		envir = envir,
		op_scale = 0.0,
		trans = piecepackr::marbles_transform
	)

	front <- rasterGrob(l_front$image)
	back <- rasterGrob(l_back$image)
	spine <- gList(
		fullGrob("black"),
		spineTextGrob("Shibumi", col = text_col)
	)

	xmp <- xmp(creator = "Trevor L. Davis", title = "Shibumi Small Box Game Jacket")
	credits <- c(
		"* The shibumi game system was invented by Cameron Browne",
		"",
		"  * https://cambolbro.com/games/shibumi/",
		"",
		"* The shibumi game system was published by nestorgames",
		"",
		"  * https://nestorgames.com/shibumi",
		"",
		"* This Small Box Game Jacket is not affiliated, sponsored, nor endorsed by either Cameron Browne or nestorgames",
		str_glue(
			"  * The use of a non-stylized {dQuote('Shibumi')} is intended as fair use to indicate that this 4x6 photo case is usable to store a {dQuote('shibumi')} set."
		)
	)

	inner <- creditsGrob(xmp, credits, icons = FALSE)

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
