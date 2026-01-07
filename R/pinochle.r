#' Create playing card box jacket for Pinochle
#'
#' `pcbj_pinochle()` creates a playing card box jacket for a Pinochle deck.
#'
#' @inheritParams pcbj_english_pattern
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @export
pcbj_pinochle <- function(
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
	pic <- normalizePath(file.path(dir, "pinochle.jpg"), mustWork = FALSE)

	if (!file.exists(pic)) {
		download.file(
			"https://upload.wikimedia.org/wikipedia/commons/thumb/1/1d/Pinochle_meld.jpg/1280px-Pinochle_meld.jpg",
			pic
		)
	}
	bm_pic <- magick::image_read(pic) |>
		as_bm_pixmap() |>
		bm_trim(right = 300L)
	bm_pic <- rasterGrob(bm_pic, height = 1)

	front <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_pic)))

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
	spine <- gList(
		rectGrob(gp = gpar(col = NA, fill = background_col)),
		spineTextGrob("Pinochle", col = text_col, size = "poker"),
		spineIconGrob(2:4, 45, 2.31, text_col, size = "poker")
	)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2025",
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
