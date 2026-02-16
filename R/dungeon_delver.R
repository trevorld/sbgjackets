#' Create SBG Jacket for Dungeon Delver
#'
#' `sbgj_dungeon_delver()` creates a small box game jacket for
#' [Dungeon Delver](https://boardgamegeek.com/boardgame/113324/dungeon-delver).
#'
#' Note that this print-and-play small box game jacket is for
#' **Personal Use Only**.
#' @inheritParams sbgj_dominoes_all
#' @inheritParams pnpmisc::pdf_create_jacket
#' @return The output file name invisibly.  As a side effect
#'   creates a pdf file.
#' @export
sbgj_dungeon_delver <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	instructions = FALSE
) {
	check_dots_empty()
	check_sbgjackets_dependencies()

	url <- "boardgamegeek.com/filepage/299906"
	bm_cover <- bm_cache_url(url, "DungeonDelverBacks.pdf", download = FALSE) |>
		bm_extract(1185:2135, 948:1593)

	front <- fullGrob(bm_cover, height = 1)

	back_notes <- r"(
		# Contents

		* 54 Dungeon Delver cards:

		  + 6 hero cards
		  + 21 item cards
		  + 4 trap cards
		  + 23 monster cards

		* 6 hit counters
		* 6 dice: d4, d6, d8, d10, d12, and d20
		* Instructions

		# Rules Summary

		* Choose a **Hero** and place them, their starting weapon, and a number of hit counters equal to their HP in front of you.  Set aside other heroes and shuffle remaining cards face down as a draw pile.

		* Draw cards one at a time and resolve:

		  - **Item:** Keep face up to a max of 3 items.

		    + To stay in limit may choose to discard new or old item.
		    + Use anytime they would help.  If single use then discard.
		    + May hold and discard items (to run away) that can't use.

		  - **Monster:** Roll dice listed on your weapon (d6 if unarmed).

		    + If die equals monster number then reroll.
		    + If die less than monster number take a hit (and add a counter to hero card) or run away by discarding an item.
		    + Then discard monster (even if you take a hit).

		  - **Trap:** Follow instructions, then discard.

		* **Win** by making it through the entire draw pile without your hit counters ever matching your hero's HP (else you **lose**).
	)"
	back <- backNotesGrob(back_notes)

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		title = "Dungeon Delver Small Box Game Jacket"
	)

	credits <- r"(
		* *Original English card backs* by Drew Chamberlain

		  + https://boardgamegeek.com/filepage/299906
		  + Cropped to fit front cover
	)"

	inner <- creditsGrob(xmp, credits, icons = FALSE)
	spine <- gList(
		fullGrob("#C4BEB2"),
		spineTextGrob("Dungeon Delver", col = "#1B0C05"),
		spineIconGrob(1, 20, 1.0, "#1B0C05")
	)

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
