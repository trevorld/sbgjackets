#' Create playing card box jacket for the Decktet
#'
#' `pcbj_decktet()` creates a playing card box jacket for the Decktet.
#'
#' @inheritParams pcbj_english_pattern
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @export
pcbj_decktet <- function(
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

	url <- "https://boardgamegeek.com/image/461294/series-decktet-games"
	bm_pic <- bm_cache_url(url, "decktet_box_cover.jpg", download = FALSE)
	front <- fullGrob(bm_pic, height = 1)

	url <- "https://www.decktet.com/download/decktet-rules.pdf"
	bm_pic <- bm_cache_url(url)
	bm_nr <- bm_pic[965:1770, 1195:2120]
	bm_ext <- bm_pic[270:589, 1156:2116]
	rg_nr <- rasterGrob(bm_nr, y = 0.58, height = 0.5)
	rg_ext <- rasterGrob(bm_ext, y = 0.15, width = 0.8)
	back_notes <- r"(
		# Contents

		Basic: 6 Aces, 3x8 Number-ranks, 6 Crowns
	)"
	back_notes1 <- trim_multistring(back_notes) |> marquee::marquee_glue(.trim = FALSE)
	mg <- marquee::marquee_grob(
		back_notes1,
		style = sbgjackets_style("poker", color = text_col),
		width = unit(pnpmisc:::JACKET_POKER_FRONT_WIDTH, "in"),
		x = unit(1 / 8, "in"),
		y = unit(1, "npc") - unit(1 / 8, "in")
	)
	back_notes2 <- c("Extended: Add 4 Pawns, 4 Courts, 1 Excuse")
	mg2 <- marquee::marquee_grob(
		back_notes2,
		style = sbgjackets_style("poker", color = text_col),
		width = unit(pnpmisc:::JACKET_POKER_FRONT_WIDTH, "in"),
		x = unit(1 / 8, "in"),
		y = unit(0.30, "npc")
	)
	back <- gList(rg_nr, rg_ext, mg, mg2)

	spine <- spineTextGrob("Decktet", col = text_col, size = "poker")

	xmp <- xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-NC-4.0",
		title = "Decktet Playing Card Box Jacket"
	)
	credits <- r"(
		* *Box cover* by P.D. Magnus

		  + <https://boardgamegeek.com/image/461294/series-decktet-games>
		  + Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License
		  + Cropped to fit front cover

		* *The Decktet* by P.D. Magnus

		  + <https://www.decktet.com/download/decktet-rules.pdf>
		  + Creative Commons Attribution-NonCommercial-ShareAlike 4.0 License
		  + Extracted three charts from first page.
	)"

	inner <- creditsGrob(xmp, credits, icons = FALSE, size = "poker")

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
