#' Create SBG Jacket for Spirograph
#'
#' `sbgj_spirograph()` creates a small box game jacket for
#' [Spirograph](https://spirographicart.com/).
#'
#' Note that this print-and-play small box game jacket is for **Personal Use Only**.
#' @inheritParams sbgj_dominoes_all
#' @inheritParams pnpmisc::pdf_create_jacket
#' @return The output file name invisibly.  As a side effect
#'   creates a pdf file.
#' @export
sbgj_spirograph <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	url <- "http://www.jtbworld.com/images/Spiro%20Various%20Spirographs%20in%20AutoCAD.png"
	bm_pic <- bm_cache_url(url, "spiro.png") |>
		bittermelon::bm_rotate(90) |>
		bm_trim(top = 10L, bottom = 10L)

	front <- fullGrob(bm_pic, height = 1)

	back_notes <- r"(
		# Description

		* A set of tools to help produce {dQuote('roulette')} curves.
		# Contents

		* 2 Spirograph rings (i.e. stators)
		* 19 Spirograph (gear)wheels (i.e. rotors)

		  + 15 circular Spirograph wheels
		  + 4 shaped Spirograph wheels

		* 1 Spirograph rack
		* 14 page guide book
		* reusable putty
		* fine-line pens

		# Links

		* https://spirographicart.com/
		* https://www.playmonster.com/product/spirograph-the-orginal-spirograph-deluxe-set/
		* https://en.wikipedia.org/wiki/Epitrochoid
		* https://en.wikipedia.org/wiki/Hypotrochoid
		* https://en.wikipedia.org/wiki/Spirograph
	)"
	back <- backNotesGrob(back_notes, col = "black")

	spine <- gList(
		fullGrob("black"),
		spineTextGrob("Spirograph", col = "white")
	)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		title = "Spirograph Small Box Game Jacket"
	)

	credits <- r"(
		* *Spiro Various Spirographs in AutoCAD* by JTB World

		  + https://blog.jtbworld.com/2015/04/spiro-12-freeware-app-for-autocad.html
		  + Creative Commons Attribution-Noncommercial-Share Alike 3.0 Unported License
		  + Rotated and cropped to fit front cover

		* The Spirograph was designed by Denys Fisher and has been published by several companies including PlayMonster Group LLC

		  + https://www.playmonster.com/brands/spirograph/

		* This Small Box Game Jacket is not affiliated, sponsored, nor endorsed by Denys Fisher nor any Spirograph publisher.
		  * The use of a non-stylized {dQuote('Spirograph')} is intended as fair use to indicate that this 4x6 photo case is usable to store a Spirograph set.
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
