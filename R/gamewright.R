#' Create playing card box jacket for *Qwixx*
#'
#' `pcbj_qwixx()` creates a playing card box jacket for the game *Qwixx*.
#'
#' Note that these print-and-play playing card box jackets are for **Personal Use Only**.
#' @inheritParams pcbj_english_pattern
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @export
pcbj_qwixx <- function(
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

	df_dice <- ppdf::dice_dice(
		x = c(0.8, 1.8, 0.8, 1.8, 0.8, 1.8),
		y = c(2.8, 2.8, 1.75, 1.75, 0.7, 0.7),
		suit = c(1L, 6L, 5L, 6L, 4L, 3L),
		rank = c(1L, 6L, 4L, 3L, 5L, 2L)
	)
	envir <- piecepackr::game_systems()
	front <- piecepackr::pmap_piece(
		df_dice,
		piecepackr::pieceGrob,
		scale = 1.2,
		envir = envir,
		draw = FALSE,
		default.units = "in"
	)

	back_notes <- r"(
		# Contents

		Six dice, rules, scoresheets

		# Rules summary

		* Take turns as active player who rolls all dice and call out the sum of white dice
		* All may cross out this white sum in any row
		* Then active player may cross out sum of 1 white die and 1 color die in that color's row
		* If active player doesn't cross out any square on turn must cross out a penalty square
		* Squares must be crossed from left to right but may skip squares
		* Must cross off 5+ squares before rightmost
		* When cross off rightmost also cross out lock and remove that color die
		* May no longer cross out from a locked row
		* Game ends when 2 rows locked or a player crosses their 4th penalty box
	)"
	back <- backNotesGrob(back_notes, col = text_col, size = "poker")

	spine <- gList(
		spineTextGrob("Qwixx", col = text_col, size = "poker"),
		spineIconGrob(2:5, 15, 1.11, text_col, size = "poker")
	)

	xmp <- xmp(creator = "Trevor L. Davis", title = "Qwixx Playing Card Box Jacket")
	credits <- r"(
		* *Qwixx* was designed by Steffen Benndorf

		* *Qwixx* is published by Gamewright

		  + <https://gamewright.com/product/Qwixx>

		* This Playing Card Box Jacket is not affiliated, sponsored, nor endorsed by either Steffen Benndorf or Gamewright

		  * The use of a non-stylized {dQuote('Qwixx')} is intended as fair use to indicate that this playing card box is usable to store a {dQuote('Qwixx')} set.
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
		paper = paper,
		bg = background_col
	) |>
		pdf_polish_jacket(xmp = xmp, instructions = instructions)
}
