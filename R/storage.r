#' Create SBG Jackets for Generic Component Storage
#'
#' `sbgj_glass_stones()` creates a small box game jacket for glass stones.
#' `sbgj_pawns()` creates a small box game jacket for pawns.
#' `sbgj_polyhedral_dice()` creates a small box game jacket for polyhedral dice.
#' `sbgj_storage_all()` creates all of those into a single pdf file.
#'
#' @param output Output file name.  Defaults to `tempfile(fileext = ".pdf")`.
#' @param ... Should be empty.
#' @inheritParams pnpmisc::pdf_create_jacket
#' @param instructions If `TRUE` then prepend instructions on how to make the jacket to the beginning of the pdf
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @rdname sbgj_storage
#' @export
sbgj_storage_all <- function(output = NULL, ..., paper = c("letter", "a4"), instructions = TRUE) {
	check_dots_empty()
	assert_runtime_dependencies()

	paper <- tolower(paper)
	paper <- match.arg(paper)
	output <- pnpmisc:::normalize_output(output)

	title <- c("Dice", "Glass Stones", "Pawns", "Polyhedral Dice")
	bm <- bm_from_title(title, instructions)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2025",
		spdx_id = "CC-BY-4.0",
		title = "Storage Small Box Game Jackets"
	)

	dice <- sbgj_dice(paper = paper)
	stones <- sbgj_glass_stones(paper = paper)
	pawns <- sbgj_pawns(paper = paper)
	pdice <- sbgj_polyhedral_dice(paper = paper)
	output_c <- tempfile(fileext = ".pdf")
	qpdf::pdf_combine(c(dice, stones, pawns, pdice), output_c)
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

#' @rdname sbgj_storage
#' @export
sbgj_dice <- function(output = NULL, ..., paper = c("letter", "a4"), instructions = FALSE) {
	check_dots_empty()
	assert_runtime_dependencies()

	paper <- tolower(paper)
	paper <- match.arg(paper)
	output <- pnpmisc:::normalize_output(output)

	dir <- get_data_dir()
	pic <- normalizePath(file.path(dir, "dice.jpg"), mustWork = FALSE)
	if (!file.exists(pic)) {
		download.file(
			"https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Mannen_spelen_dobbelspel%2C_op_de_grond_zit_aap_met_speelkaarten_Titelpagina_voor_De_alea_libri_duo%2C_Amsterdam_1642_De_alea_libri_duo_%28titel_op_object%29%2C_RP-P-1878-A-819.jpg/1193px-thumbnail.jpg",
			pic
		)
	}
	bm_pic <- magick::image_read(pic) |>
		as_bm_pixmap() |>
		bm_trim(right = 150L, left = 075L, top = 550L, bottom = 250L)
	bm_front <- rasterGrob(bm_pic, height = 1)

	envir <- piecepackr::game_systems()
	# df <- ppdf::dice_dice(x = rep(1:4 - 0.5, 6L),
	#                       y = rep(6:1 - 0.5, each = 4L),
	#                       rank = rep(1:6, 4L),
	#                       suit = rep(6:1, 4L))
	df <- ppdf::dice_dice(
		x = rep(1:4 - 0.5, 7L),
		y = rep((6 / 7) * (7:1 - 0.5) + 0.1, each = 4L),
		rank = rep(1:6, length.out = 28L),
		suit = rep(6:1, length.out = 28L)
	)
	dice <- piecepackr::pmap_piece(
		df,
		piecepackr::pieceGrob,
		scale = 1.1,
		envir = envir,
		draw = FALSE,
		default.units = "in"
	)

	front <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_front)))
	back <- dice

	spine <- gList(rectGrob(gp = gpar(col = NA, fill = "black")), spineTextGrob("Dice"))
	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2025",
		spdx_id = "CC-BY-4.0",
		title = "Dice Small Box Game Jacket"
	)

	credits <- c(
		"* *Mannen spelen dobbelspel, op de grond zit aap met speelkaarten.* (Men playing dice, monkey sitting on the ground with playing cards) by Cornelis van Dalen",
		"",
		"  + https://commons.wikimedia.org/wiki/File:Mannen_spelen_dobbelspel,_op_de_grond_zit_aap_met_speelkaarten_Titelpagina_voor_De_alea_libri_duo,_Amsterdam_1642_De_alea_libri_duo_(titel_op_object),_RP-P-1878-A-819.jpg",
		"  + Public Domain",
		"  + Cropped to fit cover"
	)
	inner <- creditsGrob(xmp, credits, icons = FALSE)

	output <- pdf_create_jacket(
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

#' @rdname sbgj_storage
#' @export
sbgj_glass_stones <- function(output = NULL, ..., paper = c("letter", "a4"), instructions = FALSE) {
	check_dots_empty()
	assert_runtime_dependencies()

	paper <- tolower(paper)
	paper <- match.arg(paper)
	output <- pnpmisc:::normalize_output(output)

	dir <- get_data_dir()
	pic <- normalizePath(file.path(dir, "glass_stones.jpg"), mustWork = FALSE)
	if (!file.exists(pic)) {
		download.file(
			"https://i2.pickpik.com/photos/301/542/392/glass-beads-glass-blue-decoration-a9ef02658d94bb22dd25203b4aa9f858.jpg",
			pic
		)
	}
	bm_pic <- magick::image_read(pic) |> as_bm_pixmap()
	nc <- ncol(bm_pic)
	bm_front <- rasterGrob(bm_pic[, seq.int(ceiling(nc / 2) - 24L, nc)], height = 1)
	bm_back <- rasterGrob(bm_pic[, seq.int(1L, floor(nc / 2) + 24L)], height = 1)

	front <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_front)))
	back <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_back)))

	spine <- gList(rectGrob(gp = gpar(col = NA, fill = "black")), spineTextGrob("Glass Stones"))
	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2025",
		spdx_id = "CC-BY-ND-4.0",
		title = "Glass Stones Small Box Game Jacket"
	)

	credits <- c(
		"* *assorted glass stones*",
		"",
		"  + https://www.pickpik.com/glass-beads-glass-blue-decoration-bead-beads-136538",
		"  + PickPik Terms of Service https://www.pickpik.com/terms-of-service",
		"",
		"    > The images provided by PickPik are free to use for personal and commercial projects",
		"",
		"  + Cropped to fit covers"
	)
	inner <- creditsGrob(xmp, credits, icons = FALSE)

	output <- pdf_create_jacket(
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

#' @rdname sbgj_storage
#' @export
sbgj_pawns <- function(output = NULL, ..., paper = c("letter", "a4"), instructions = FALSE) {
	check_dots_empty()
	assert_runtime_dependencies()

	paper <- tolower(paper)
	paper <- match.arg(paper)
	output <- pnpmisc:::normalize_output(output)

	dir <- get_data_dir()
	# https://pixabay.com/photos/play-stone-multicoloured-characters-1743645/
	pic <- normalizePath(file.path(dir, "wooden_pawns.jpg"), mustWork = FALSE)
	if (!file.exists(pic)) {
		url <- "https://pixabay.com/photos/play-stone-multicoloured-characters-1743645/"
		abort(str_glue("{dQuote(pic)} does not exist.  Download from <{url}>."))
	}
	bm_pic <- magick::image_read(pic) |> as_bm_pixmap()
	nc <- ncol(bm_pic)
	bm_front <- rasterGrob(bm_pic[, seq.int(ceiling(nc / 2), nc)], height = 1)
	bm_back <- rasterGrob(bm_pic[, seq.int(1L, floor(nc / 2))], height = 1)

	front <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_front)))
	back <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_back)))

	spine <- gList(rectGrob(gp = gpar(col = NA, fill = "black")), spineTextGrob("Pawns"))
	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2025",
		spdx_id = "CC-BY-ND-4.0",
		title = "Pawns Small Box Game Jacket"
	)

	credits <- c(
		"* *Play stone, Multicoloured, Characters* by Alexa",
		"",
		"  + https://pixabay.com/photos/play-stone-multicoloured-characters-1743645/",
		"  + https://pixabay.com/users/alexas_fotos-686414/",
		"  + Pixabay Content License https://pixabay.com/service/license-summary/",
		"",
		"    > the Content License allows users to:",
		"    >",
		"    > * Use Content for free",
		"    > * Use Content without having to attribute the author",
		"    > * Modify or adapt Content into new works",
		"",
		"  + Cropped to fit covers"
	)
	inner <- creditsGrob(xmp, credits, icons = FALSE)

	output <- pdf_create_jacket(
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

#' @rdname sbgj_storage
#' @export
sbgj_polyhedral_dice <- function(
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

	dir <- get_data_dir()
	pic <- normalizePath(file.path(dir, "polyhedral_dice.jpg"), mustWork = FALSE)
	if (!file.exists(pic)) {
		download.file(
			"https://i2.pickpik.com/photos/821/950/430/cube-play-role-playing-game-craps-82caba6d073e803940fba76a77c4be8a.jpg",
			pic
		)
	}
	bm_pic <- magick::image_read(pic) |> as_bm_pixmap()
	nc <- ncol(bm_pic)
	bm_front <- rasterGrob(bm_pic[, seq.int(ceiling(nc / 2) - 24L, nc)], height = 1)
	# bm_back <- rasterGrob(bm_pic[, seq.int(1L, floor(nc / 2) + 24L)], height = 1)

	envir <- piecepackr::game_systems()
	df <- ppdf::dice_dice(
		x = rep(1:4 - 0.5, 7L),
		y = rep((6 / 7) * (7:1 - 0.5) + 0.1, each = 4L),
		rank = rep(1:4, 7L),
		suit = rep(6:1, length.out = 28L)
	) |>
		mutate(
			cfg = rep(
				paste0("dice_", c("d4", "numeral", "d8", "d10", "d10_percentile", "d12", "d20")),
				each = 4L
			)
		)
	dice <- piecepackr::pmap_piece(
		df,
		piecepackr::pieceGrob,
		scale = 0.88,
		envir = envir,
		op_scale = 0.01,
		draw = FALSE,
		default.units = "in"
	)

	front <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_front)))
	back <- dice
	# back <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_back)))

	spine <- gList(rectGrob(gp = gpar(col = NA, fill = "black")), spineTextGrob("Polyhedral Dice"))
	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2025",
		spdx_id = "CC-BY-ND-4.0",
		title = "Polyhedral Dice Small Box Game Jacket"
	)

	credits <- c(
		"* *assorted dice lot*",
		"",
		"  + https://www.pickpik.com/cube-play-role-playing-game-craps-colorful-instantaneous-speed-130507",
		"  + PickPik Terms of Service https://www.pickpik.com/terms-of-service",
		"",
		"    > The images provided by PickPik are free to use for personal and commercial projects",
		"",
		"  + Cropped to fit cover"
	)
	inner <- creditsGrob(xmp, credits, icons = FALSE)

	output <- pdf_create_jacket(
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
