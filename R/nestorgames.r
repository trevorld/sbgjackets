#' Create SBG Jacket for nestorgames products
#'
#' `sbgj_shibumi()` creates a small box game jacket for [Shibumi](https://cambolbro.com/games/shibumi/).
#'
#' Note that these print-and-play small box game jackets are for **Personal Use Only**.
#' @inheritParams sbgj_dominoes_all
#' @inheritParams pnpmisc::pdf_create_jacket
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @rdname sbgj_nestorgames
#' @export
sbgj_shibumi <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	assert_runtime_dependencies()
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
