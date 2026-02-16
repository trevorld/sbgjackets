#' Create SBG Jackets for Generic Component Storage
#'
#' `sbgj_black_stones()` creates a small box game jacket for black stones.
#' `sbgj_glass_stones()` creates a small box game jacket for glass stones.
#' `sbgj_marbles()` creates a small box game jacket for marbles.
#' `sbgj_pawns()` creates a small box game jacket for pawns.
#' `sbgj_polyhedral_dice()` creates a small box game jacket for polyhedral dice.
#' `sbgj_reversible_discs()` creates a small box game jacket for
#'   reversible discs.
#' `sbgj_white_stones()` creates a small box game jacket for white stones.
#'
#' @param output Output file name.  Defaults to `tempfile(fileext = ".pdf")`.
#' @param ... Should be empty.
#' @inheritParams pnpmisc::pdf_create_jacket
#' @param instructions If `TRUE` then prepend instructions on how to make the jacket to the beginning of the pdf
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @rdname sbgj_storage
#' @export
sbgj_black_stones <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	url <- "https://upload.wikimedia.org/wikipedia/commons/b/b8/Slate_go_pieces_sc.jpg"
	bm_back <- bm_cache_url(url, "slate_black_stones.jpg")

	url <- "https://upload.wikimedia.org/wikipedia/commons/b/b3/Yunzi_double_convex.jpg"
	bm_front <- bm_cache_url(url, "yunzi_go_stones.jpg") |>
		bm_trim(left = 600, top = 200)

	front <- fullGrob(bm_front, height = 1)
	back <- fullGrob(bm_back, height = 1)

	spine <- gList(fullGrob("black"), spineTextGrob("Black Stones"))
	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "Black Stones Small Box Game Jacket"
	)

	credits <- r"(
		* *Double convex yunzi stones and woven baskets for holding them placed on a Go board* by Ralph Unden

		  + https://commons.wikimedia.org/wiki/File:Yunzi_double_convex.jpg
		  + Creative Commons Attribution 2.0 Generic License
		  + Cropped to fit front cover

		* *slate go pieces sc* by Liz West

		  + https://commons.wikimedia.org/wiki/File:Slate_go_pieces_sc.jpg
		  + Creative Commons Attribution 2.0 Generic License
		  + Cropped to fit back cover
	)"
	inner <- creditsGrob(xmp, credits, icons = FALSE)

	output <- pdf_create_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper
	) |>
		pdf_polish_jacket(xmp = xmp, instructions = instructions)
}

#' @rdname sbgj_storage
#' @export
sbgj_dice <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Mannen_spelen_dobbelspel%2C_op_de_grond_zit_aap_met_speelkaarten_Titelpagina_voor_De_alea_libri_duo%2C_Amsterdam_1642_De_alea_libri_duo_%28titel_op_object%29%2C_RP-P-1878-A-819.jpg/1193px-thumbnail.jpg"
	bm_pic <- bm_cache_url(url, "dice.jpg") |>
		bm_trim(right = 150L, left = 075L, top = 550L, bottom = 250L)

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

	front <- fullGrob(bm_pic, height = 1)
	back <- dice

	spine <- gList(fullGrob("black"), spineTextGrob("Dice"))
	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "Dice Small Box Game Jacket"
	)

	credits <- r"(
		* *Mannen spelen dobbelspel, op de grond zit aap met speelkaarten.* (Men playing dice, monkey sitting on the ground with playing cards) by Cornelis van Dalen

		  + https://commons.wikimedia.org/wiki/File:Mannen_spelen_dobbelspel,_op_de_grond_zit_aap_met_speelkaarten_Titelpagina_voor_De_alea_libri_duo,_Amsterdam_1642_De_alea_libri_duo_(titel_op_object),_RP-P-1878-A-819.jpg
		  + Public Domain
		  + Cropped to fit cover
	)"
	cr_grob <- creditsGrob(xmp, credits, icons = FALSE)
	inner <- gList(cr_grob, dice_board_grob())

	output <- pdf_create_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper
	) |>
		pdf_polish_jacket(xmp = xmp, instructions = instructions)
}

dice_board_grob <- function() {
	offsets <- (2 / 3) * seq(-2.5, 2.5, by = 1)
	xs <- unit(2 + offsets, "in")
	ys <- unit(3 + offsets, "in")
	fills <- rep(c("white", "black"), each = 6L * 3L)
	gp_rect <- gpar(col = "grey90", fill = NA, lwd = 2)
	vp <- vp_inner_right()
	grobTree(
		rectGrob(
			x = unit(2, "in"),
			y = unit(3, "in"),
			width = unit(c(8 / 3, 4 / 3), "in"),
			height = unit(c(8 / 3, 4 / 3), "in"),
			gp = gp_rect
		),
		circleGrob(
			x = rep(xs, 6L),
			y = rep(ys, each = 6L),
			r = unit(1 / 8, "in"),
			gp = gpar(col = "black", fill = fills, lwd = 2)
		),
		vp = vp
	)
}

#' @rdname sbgj_storage
#' @export
sbgj_marbles <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	url <- "https://pixnio.com/objects/marble-ball-color-colorful"
	bm_front <- bm_cache_url(url, "marbles_front.jpg", download = FALSE)

	url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/20150720-Glasmurmeln-IMG_7124.jpg/3840px-20150720-Glasmurmeln-IMG_7124.jpg"
	bm_back <- bm_cache_url(url, "marbles_back.jpg")

	front <- fullGrob(bm_front, height = 1)
	back <- fullGrob(bm_back, height = 1)

	spine <- gList(fullGrob("black"), spineTextGrob("Marbles"))
	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-SA-3.0",
		title = "Marbles Small Box Game Jacket"
	)

	credits <- r"(
		* *marble, ball, color, colorful*

		  + https://pixnio.com/objects/marble-ball-color-colorful
		  + CC0 1.0 Public Domain Dedication
		  + Cropped to fit front cover

		* *Gesuender Leben* by Bernhard Schindele

		  + https://commons.wikimedia.org/wiki/File:20150720-Glasmurmeln-IMG_7124.jpg
		  + Creative Commons Attribution-ShareAlike 3.0 Unported License
		  + Cropped to fit back cover
	)"
	inner <- creditsGrob(xmp, credits, icons = FALSE)

	output <- pdf_create_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper
	) |>
		pdf_polish_jacket(xmp = xmp, instructions = instructions)
}

#' @rdname sbgj_storage
#' @export
sbgj_glass_stones <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	url <- "https://i2.pickpik.com/photos/301/542/392/glass-beads-glass-blue-decoration-a9ef02658d94bb22dd25203b4aa9f858.jpg"
	bm_pic <- bm_cache_url(url, "glass_stones.jpg")
	nc <- ncol(bm_pic)
	bm_front <- bm_pic[, seq.int(ceiling(nc / 2) - 24L, nc)]
	bm_back <- bm_pic[, seq.int(1L, floor(nc / 2) + 24L)]

	front <- fullGrob(bm_front, height = 1)
	back <- fullGrob(bm_back, height = 1)

	spine <- gList(fullGrob("black"), spineTextGrob("Glass Stones"))
	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2025",
		spdx_id = "CC-BY-ND-4.0",
		title = "Glass Stones Small Box Game Jacket"
	)

	credits <- r"(
		* *assorted glass stones*

		  + https://www.pickpik.com/glass-beads-glass-blue-decoration-bead-beads-136538
		  + PickPik Terms of Service https://www.pickpik.com/terms-of-service

		    > The images provided by PickPik are free to use for personal and commercial projects

		  + Cropped to fit covers
	)"
	inner <- creditsGrob(xmp, credits, icons = FALSE)

	output <- pdf_create_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper
	) |>
		pdf_polish_jacket(xmp = xmp, instructions = instructions)
}

#' @rdname sbgj_storage
#' @export
sbgj_pawns <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	url <- "https://pixabay.com/photos/play-stone-multicoloured-characters-1743645/"
	bm_pic <- bm_cache_url(url, "wooden_pawns.jpg", download = FALSE)
	nc <- ncol(bm_pic)
	bm_front <- bm_pic[, seq.int(ceiling(nc / 2), nc)]
	bm_back <- bm_pic[, seq.int(1L, floor(nc / 2))]

	front <- fullGrob(bm_front, height = 1)
	back <- fullGrob(bm_back, height = 1)

	spine <- gList(fullGrob("black"), spineTextGrob("Pawns"))
	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2025",
		spdx_id = "CC-BY-ND-4.0",
		title = "Pawns Small Box Game Jacket"
	)

	credits <- r"(
		* *Play stone, Multicoloured, Characters* by Alexa

		  + https://pixabay.com/photos/play-stone-multicoloured-characters-1743645/
		  + https://pixabay.com/users/alexas_fotos-686414/
		  + Pixabay Content License https://pixabay.com/service/license-summary/

		    > the Content License allows users to:
		    >
		    > * Use Content for free
		    > * Use Content without having to attribute the author
		    > * Modify or adapt Content into new works

		  + Cropped to fit covers
	)"
	inner <- creditsGrob(xmp, credits, icons = FALSE)

	output <- pdf_create_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper
	) |>
		pdf_polish_jacket(xmp = xmp, instructions = instructions)
}

#' @rdname sbgj_storage
#' @export
sbgj_polyhedral_dice <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	url <- "https://i2.pickpik.com/photos/821/950/430/cube-play-role-playing-game-craps-82caba6d073e803940fba76a77c4be8a.jpg"
	bm_pic <- bm_cache_url(url, "polyhedral_dice.jpg")
	nc <- ncol(bm_pic)
	bm_front <- bm_pic[, seq.int(ceiling(nc / 2) - 24L, nc)]
	# bm_back <- bm_pic[, seq.int(1L, floor(nc / 2) + 24L)]

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

	front <- fullGrob(bm_front, height = 1)
	back <- dice
	# back <- fullGrob(bm_back, height = 1)

	spine <- gList(fullGrob("black"), spineTextGrob("Polyhedral Dice"))
	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2025",
		spdx_id = "CC-BY-ND-4.0",
		title = "Polyhedral Dice Small Box Game Jacket"
	)

	credits <- r"(
		* *assorted dice lot*

		  + https://www.pickpik.com/cube-play-role-playing-game-craps-colorful-instantaneous-speed-130507
		  + PickPik Terms of Service https://www.pickpik.com/terms-of-service

		    > The images provided by PickPik are free to use for personal and commercial projects

		  + Cropped to fit cover
	)"
	inner <- creditsGrob(xmp, credits, icons = FALSE)

	output <- pdf_create_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper
	) |>
		pdf_polish_jacket(xmp = xmp, instructions = instructions)
}

#' @rdname sbgj_storage
#' @export
sbgj_reversible_discs <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	url <- "https://www.flickr.com/photos/noahbulgaria/226231535"
	bm_front <- bm_cache_url(url, "othello.jpg", download = FALSE)

	front <- fullGrob(bm_front, height = 1)

	back_notes <- r"(
		# Notable games using reversible discs

		* *Games you can play with Othello/Reversi/bicolored pieces* geeklist

		  + https://boardgamegeek.com/geeklist/235864/

		* Glaisher

		  + https://boardgamegeek.com/boardgame/175363/glaisher

		* Io

		  + https://boardgamegeek.com/boardgame/172471/io

		* Ming Mang

		  + http://www.cyningstan.com/game/328/ming-mang

		* Othellito

		  + https://boardgamegeek.com/boardgame/289484/othellito

		* Othello

		  + https://www.worldothello.org/

		* Verto

		  + https://boardgamegeek.com/boardgame/89333/verto

		* Wizard's Garden

		  + https://www.tjgames.com/wizard.html

		* Yonmoque

		  + https://www.gift-box.co.jp/english/yonmoque.html
	)"
	back <- backNotesGrob(back_notes)

	spine <- gList(fullGrob("black"), spineTextGrob("Reversible Discs"))
	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "Reversible Discs Small Box Game Jacket"
	)

	credits <- r"(
		* *Othello! or Reversi* by Noah

		  + https://www.flickr.com/photos/noahbulgaria/226231535
		  + Creative Commons Attribution 2.0 Generic License
		  + Cropped to fit front cover
	)"
	cr_grob <- creditsGrob(xmp, credits, icons = FALSE)
	inner <- gList(cr_grob, reversible_discs_board_grob())

	output <- pdf_create_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper
	) |>
		pdf_polish_jacket(xmp = xmp, instructions = instructions)
}

reversible_discs_board_grob <- function() {
	w <- unit(3 / 4, "in")
	offsets_r <- (3 / 4) * seq(-1.5, 1.5, by = 1)
	offsets_c <- (3 / 4) * seq(-2, 2, by = 1)
	xs_r <- unit(2 + offsets_r, "in")
	ys_r <- unit(3 + offsets_r, "in")
	xs_c <- unit(2 + offsets_c, "in")
	ys_c <- unit(3 + offsets_c, "in")
	fills_c_1 <- c("grey", "white", "black", "white", "grey")
	fills_c_2 <- c("white", "black", "white", "black", "white")
	fills_c_3 <- c("black", "white", "grey", "white", "black")
	fills_c <- c(fills_c_1, fills_c_2, fills_c_3, fills_c_2, fills_c_1)
	vp <- vp_inner_right()
	grobTree(
		rectGrob(
			x = rep(xs_r, 4L),
			y = rep(ys_r, each = 4L),
			width = w,
			height = w,
			gp = gpar(col = "black", fill = NA, lwd = 2)
		),
		circleGrob(
			x = rep(xs_c, 5L),
			y = rep(ys_c, each = 5L),
			r = unit(1 / 8, "in"),
			gp = gpar(col = "black", fill = fills_c, lwd = 2)
		),
		vp = vp
	)
}

#' @rdname sbgj_storage
#' @export
sbgj_white_stones <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	url <- "https://upload.wikimedia.org/wikipedia/commons/6/62/Shell_go_pieces_sc.jpg"
	bm_back <- bm_cache_url(url, "shell_white_stones.jpg")

	url <- "https://upload.wikimedia.org/wikipedia/commons/b/b3/Yunzi_double_convex.jpg"
	bm_front <- bm_cache_url(url, "yunzi_go_stones.jpg") |>
		bm_trim(right = 670, bottom = 60, top = 100)

	front <- fullGrob(bm_front, height = 1)
	back <- fullGrob(bm_back, height = 1)

	spine <- gList(fullGrob("black"), spineTextGrob("White Stones"))
	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "White Stones Small Box Game Jacket"
	)

	credits <- r"(
		* *Double convex yunzi stones and woven baskets for holding them placed on a Go board* by Ralph Unden

		  + https://commons.wikimedia.org/wiki/File:Yunzi_double_convex.jpg
		  + Creative Commons Attribution 2.0 Generic License
		  + Cropped to fit front cover

		* *shell go pieces sc* by Liz West

		  + https://commons.wikimedia.org/wiki/File:Shell_go_pieces_sc.jpg
		  + Creative Commons Attribution 2.0 Generic License
		  + Cropped to fit back cover
	)"
	inner <- creditsGrob(xmp, credits, icons = FALSE)

	output <- pdf_create_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper
	) |>
		pdf_polish_jacket(xmp = xmp, instructions = instructions)
}
