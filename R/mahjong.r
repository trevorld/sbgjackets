#' Create playing card box jacket for Mahjong
#'
#' `pcbj_mahjong()` creates two playing card box jackets for a deck of Mahjong cards.
#'
#' @inheritParams pcbj_english_pattern
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @export
pcbj_mahjong <- function(
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
	pic <- normalizePath(file.path(dir, "mahjong.jpg"), mustWork = FALSE)
	if (!file.exists(pic)) {
		download.file(
			"https://www.publicdomainpictures.net/pictures/280000/velka/mahjong-tiles-1545917019VCL.jpg",
			pic
		)
	}
	bm_pic <- magick::image_read(pic) |>
		as_bm_pixmap() |>
		rasterGrob(height = 1)
	front1 <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_pic)))

	pic <- normalizePath(file.path(dir, "mahjong2.jpg"), mustWork = FALSE)
	if (!file.exists(pic)) {
		download.file(
			"https://images.pexels.com/photos/28996231/pexels-photo-28996231.jpeg",
			pic
		)
	}
	bm_pic <- magick::image_read(pic) |>
		as_bm_pixmap() |>
		rasterGrob(height = 1)
	front2 <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_pic)))

	front <- list(front1, front2)

	back_notes <- c(
		"# Contents",
		"",
		"* 136+ cards",
		"",
		"  + 108 suited cards = 4 x 3 suits x 9 ranks",
		"",
		"    - Suits: Circles, Bamboo, Characters",
		"",
		"  + 28+ Honor cards",
		"",
		"    - 16 \u201cWind\u201d cards = 4 x {{East, South, West, North}}",
		"    - 12 \u201cDragon\u201d cards = 4 x {{White, Green, Red}}",
		"    - 0+ \u201cFlower\u201d cards in uniquely numbered quartets",
		"",
		"  + 0+ Joker cards",
		"",
		"# Links",
		"",
		"* https://www.sloperama.com/mahjongg/"
	)
	back_notes <- paste(back_notes, collapse = "\n") |> marquee::marquee_glue(.trim = FALSE)
	mg <- marquee::marquee_grob(
		back_notes,
		style = credits_style("poker", color = text_col) |>
			marquee::modify_style(
				"img",
				marquee::style(img_asp = 3.5)
			),
		width = unit(pnpmisc:::JACKET_POKER_FRONT_WIDTH, "in"),
		x = unit(1 / 8, "in"),
		y = unit(1, "npc") - unit(1 / 8, "in")
	)

	back <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), mg)
	spine1 <- gList(
		rectGrob(gp = gpar(col = NA, fill = background_col)),
		spineTextGrob("Mahjong (1/2)", col = text_col, size = "poker")
	)
	spine2 <- gList(
		rectGrob(gp = gpar(col = NA, fill = background_col)),
		spineTextGrob("Mahjong (2/2)", col = text_col, size = "poker")
	)
	spine <- list(spine1, spine2)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-ND-4.0",
		title = "Mahjong Playing Card Box Jackets"
	)
	credits <- c(
		"* *Mahjong Tiles* by Peter Griffin",
		"",
		"  + https://www.publicdomainpictures.net/en/view-image.php?image=279568",
		"  + CC0 1.0 Public Domain Dedication",
		"  + Cropped to fit front cover",
		"",
		"* *Traditional Mahjong Tiles Arrangement Close-Up* by Mahmoud Yahyaoui",
		"",
		"  + https://www.pexels.com/photo/traditional-mahjong-tiles-arrangement-close-up-28996231/",
		"  + https://www.pexels.com/license/",
		"  + Cropped to fit front cover"
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
		prepend_instructions(output, paper = paper, orientation = "portrait")
	}

	set_xmp(xmp, output)
	set_docinfo(as_docinfo(xmp), output)
	invisible(output)
}
