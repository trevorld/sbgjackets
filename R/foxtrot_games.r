#' Create playing card box jacket for *Fox in the Forest*
#'
#' `pcbj_fox_in_the_forest()` creates a playing card box jacket for the game *Fox in the Forest*.
#'
#' Note that these print-and-play playing card box jackets are for **Personal Use Only**.
#' @inheritParams sbgj_dominoes_all
#' @inheritParams pnpmisc::pdf_create_jacket
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @rdname pcbj_foxtrot_games
#' @export
pcbj_fox_in_the_forest <- function(
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

	background_col <- "#edc38fff"
	text_col <- "#8b3c29ff"

	current_dev <- grDevices::dev.cur()
	if (current_dev > 1) {
		on.exit(grDevices::dev.set(current_dev), add = TRUE)
	} else {
		on.exit(grDevices::graphics.off(), add = TRUE)
	}

	dir <- get_data_dir()
	pic <- normalizePath(file.path(dir, "Carl_Rungius_Red_Fox.jpg"), mustWork = FALSE)

	if (!file.exists(pic)) {
		download.file(
			"https://upload.wikimedia.org/wikipedia/commons/thumb/a/a2/Red_Fox%2C_Carl_Rungius%2C_1933.jpg/960px-Red_Fox%2C_Carl_Rungius%2C_1933.jpg",
			pic
		)
	}
	bm_pic <- magick::image_read(pic) |> as_bm_pixmap() |> rasterGrob(height = 1)

	front <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_pic)))

	back_notes <- c(
		"# Contents",
		"",
		"* 33 cards (3 suits x ranks 1\u201311)",
		"* 17 scoring tokens",
		"* 2 player reference cards",
		"",
		"# Rules Summary",
		"",
		"* Trick-taking game with 13 tricks per round",
		"* Play complete rounds until 21 points",
		"",
		"  + Tie-breaker: most points in the last round",
		"  + Go to 16 points for a shorter game",
		"  + Go to 35 points for a longer game",
		"",
		"* Setup each round:",
		"",
		"  + Shuffle all cards and deal 13 per player",
		"  + Left over 7 cards form the \u201cdraw deck\u201d; flip its top card over next to it to indicate the trump suit (this card is the \u201cdecree\u201d)",
		"",
		"* If possible follower must play lead suit"
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
		spineTextGrob("The Fox in the Forest", col = text_col, size = "poker"),
		spineIconGrob(2, 30, 1.6, text_col, size = "poker")
	)

	xmp <- xmp(creator = "Trevor L. Davis", title = "The Fox in the Forest Playing Card Box Jacket")
	credits <- c(
		"* *The Fox in the Forest* was designed by Joshua Buergel",
		"",
		"* *The Fox in the Forest* is published by Foxtrot Games http://foxtrotgames.com/forest/",
		"",
		"* This Playing Card Box Jacket is not affiliated, sponsored, nor endorsed by either Joshua Buergel or Foxtrot Games",
		"",
		str_glue(
			"  * The use of a non-stylized {dQuote('The Fox in the Forest')} is intended as fair use to indicate that this playing card box is usable to store a {dQuote('The Fox in the Forest')} deck."
		),
		"",
		"* *Red Fox* by Carl Rungius (1933). Public domain in USA. Cropped to fit front cover."
	)

	inner <- creditsGrob(xmp, credits, icons = TRUE, size = "poker")

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
