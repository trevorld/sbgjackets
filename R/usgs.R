#' Create playing card box jacket for *Wizard*
#'
#' `pcbj_wizard()` creates a playing card box jacket for the game *Wizard*.
#'
#' Note that these print-and-play playing card box jackets are for **Personal Use Only**.
#' @inheritParams pcbj_english_pattern
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @export
pcbj_wizard <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE,
	double = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()
	current_dev <- grDevices::dev.cur()
	on.exit(restore_devices(current_dev), add = TRUE)

	background_col <- "white"
	text_col <- "black"

	url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/%C3%89pinal_-_L%E2%80%99Oiseau_bleu_09.jpg/960px-%C3%89pinal_-_L%E2%80%99Oiseau_bleu_09.jpg"
	bm_pic <- bm_cache_url(url, "wizard_blue_bird.jpg")
	front <- fullGrob(bm_pic, height = 1)

	back_notes <- r"(
		# Contents

		standard 52-card deck + 4 Wizards + 4 Jesters

		# Rules Summary

		* Trick-taking game with increasing number of tricks per round from one up to 60 / # players
		* After deal turn up the next in deck as trump

		  + If Jester or last round no trump
		  + If Wizard then Dealer's choice

		* Each player bids how many tricks they'll take

		  + Sum bids can't equal total tricks
		  + If correct 20 points + 10 points per trick
		  + If wrong -10 points per trick O/U bid

		* Must follow lead suit if possible except Wizard or Jester **may** be played at any time

		  + Jester always loses except if all following cards are also Jesters
		  + First Wizard always wins
	)"
	back_notes <- trim_multistring(back_notes) |> marquee::marquee_glue(.trim = FALSE)
	mg <- marquee::marquee_grob(
		back_notes,
		style = sbgjackets_style("poker", color = text_col),
		width = unit(pnpmisc:::JACKET_POKER_FRONT_WIDTH, "in"),
		x = unit(1 / 8, "in"),
		y = unit(1, "npc") - unit(1 / 8, "in")
	)

	back <- gList(fullGrob(background_col), mg)
	spine <- gList(
		fullGrob(background_col),
		spineTextGrob("Wizard", col = text_col, size = "poker"),
		spineIconGrob(3:6, 45, 1.7, text_col, size = "poker")
	)

	xmp <- xmp(creator = "Trevor L. Davis", title = "Wizard Playing Card Box Jacket")
	credits <- r"(
		* *Wizard* was designed by Ken Fisher

		* *Wizard* is published by U.S. Game Systems, Inc.

		  + <https://www.usgamesinc.com/original-wizard-r-card-game.html>

		* This Playing Card Box Jacket is not affiliated, sponsored, nor endorsed by either Ken Fisher or U.S. Game Systems, Inc.

		  * The use of a non-stylized {dQuote('Wizard')} is intended as fair use to indicate that this playing card box is usable to store a {dQuote('Wizard')} deck.

		* *Images d'{E_acute}pinal : L'Oiseau bleu* by Pellerin & Cie (1860). Public domain in USA.
	)"

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
