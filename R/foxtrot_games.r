#' Create playing card box jacket for *The Fox in the Forest*
#'
#' `pcbj_fox_in_the_forest()` creates a playing card box jacket for the game *The Fox in the Forest*.
#'
#' Note that these print-and-play playing card box jackets are for **Personal Use Only**.
#' @inheritParams pcbj_english_pattern
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @rdname pcbj_foxtrot_games
#' @export
pcbj_fox_in_the_forest <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE,
	double = FALSE
) {
	check_dots_empty()
	assert_runtime_dependencies()
	current_dev <- grDevices::dev.cur()
	on.exit(restore_devices(current_dev), add = TRUE)

	background_col <- "#edc38fff"
	text_col <- "#8b3c29ff"

	url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a2/Red_Fox%2C_Carl_Rungius%2C_1933.jpg/960px-Red_Fox%2C_Carl_Rungius%2C_1933.jpg"
	bm_pic <- bm_cache_url(url, "Carl_Rungius_Red_Fox.jpg")
	front <- fullGrob(bm_pic, height = 1)

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

	back <- gList(fullGrob(background_col), mg)
	spine <- gList(
		fullGrob(background_col),
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

	if (double) {
		spine <- list(spine, spine)
	}
	pdf_create_poker_jacket(
		output = output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper
	) |>
		pdf_polish_jacket(xmp = xmp, instructions = instructions)
}
