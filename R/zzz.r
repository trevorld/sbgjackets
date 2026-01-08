#' @import grid
#' @importFrom bittermelon as_bm_pixmap bm_replace bm_trim
#' @importFrom dplyr filter mutate
#' @importFrom grDevices dev.off pdf
#' @importFrom pnpmisc pdf_create_jacket pdf_create_poker_jacket
#' @importFrom rlang abort check_dots_empty .data
#' @importFrom stringr str_c str_glue str_sub str_sub<-
#' @importFrom utils download.file packageVersion
#' @importFrom xmpdf as_docinfo set_bookmarks set_docinfo set_xmp xmp
NULL

assert_runtime_dependencies <- function() {
	stopifnot(
		capabilities("cairo"),
		piecepackr::has_font("Carlito"),
		xmpdf::supports_set_bookmarks(),
		xmpdf::supports_set_docinfo(),
		xmpdf::supports_set_xmp()
	)
}

get_data_dir <- function() {
	dir <- tools::R_user_dir("sbgjackets", "data")
	if (!dir.exists(dir)) {
		dir.create(dir, recursive = TRUE)
	}
	dir
}

readme_markdown_table <- function(df) {
	name <- ifelse(is.na(df$url), df$game, glue::glue("[{df$game}]({df$url})"))
	fn <- df$`function`
	license <- ifelse(
		df$license %in% piecepackr::spdx_license_list$id,
		glue::glue("[{df$license}]({piecepackr::spdx_license_list[df$license, 'url_alt']})"),
		df$license
	)
	table <- data.frame(
		`Game (System/Components)` = name,
		Function = fn,
		License = license,
		check.names = FALSE
	)
	knitr::kable(table, "pipe")
}

df_sbgj <- function() {
	# fmt: skip
	df <- tibble::tribble(
		~game, ~url, ~`function`, ~license,
		"Dice", NA_character_, "`sbgj_dice()`", "CC-BY-4.0",
		"Polyhedral Dice", NA_character_, "`sbgj_polyhedral_dice()`", "CC-BY-ND-4.0",
		"Double-Six Dominoes", "https://www.pagat.com/domino/", "`sbgj_dominoes_double6()`", "CC-BY-4.0",
		"Double-Nine Dominoes", NA_character_, "`sbgj_dominoes_double9()`", "CC-BY-4.0",
		"Double-Twelve Dominoes", NA_character_, "`sbgj_dominoes_double12()`", "CC-BY-4.0",
		"Homeworlds", "https://www.looneylabs.com/games/homeworlds", "`sbgj_homeworlds()`", "Personal Use Only",
		"Ice Duo", "https://www.looneylabs.com/games/ice-duo", "`sbgj_ice_duo()`", "Personal Use Only",
		"Jinxx", "https://www.looneylabs.com/games/jinxx", "`sbgj_jinxx()`", "Personal Use Only",
		"Looney Pyramids", "https://www.looneylabs.com/pyramids-home", "`sbgj_looney_pyramids()`", "Personal Use Only",
		"Martian Chess", "https://www.looneylabs.com/martian_chess", "`sbgj_martian_chess()`", "Personal Use Only",
		"Nomids", "https://www.looneylabs.com/nomids", "`sbgj_nomids()`", "Personal Use Only",
		"Pawns", NA_character_, "`sbgj_pawns()`", "CC-BY-ND-4.0",
		"Shibumi", "https://cambolbro.com/games/shibumi/", "`sbgj_shibumi()`", "Personal Use Only",
		"Glass Stones", NA_character_, "`sbgj_glass_stones()`", "CC-BY-ND-4.0"
	)
	# Looney Labs FAQ says okay to use their images to **create** jackets for personal use
	# but does not grant permission to **share** such jackets
	not_shareable <- c(
		"Homeworlds",
		"Ice Duo",
		"Jinxx",
		"Looney Pyramids",
		"Martian Chess",
		"Nomids"
	)
	df$shareable <- !(df$game %in% not_shareable)
	df
}

df_pcbj <- function() {
	# fmt: skip
	df <- tibble::tribble(
		~game, ~url, ~`function`, ~license,
		"The Fox in the Forest", "https://foxtrotgames.com/forest/", "`pcbj_fox_in_the_forest()`", "Personal Use Only",
		"French Suits: English Pattern", "https://i-p-c-s.org/pattern/ps-48.html", "`pcbj_english_pattern()`", "CC-BY-4.0",
		"German Suits: Bavarian Pattern", "https://i-p-c-s.org/pattern/ps-55.html", "`pcbj_bavarian_pattern()`", "CC-BY-4.0",
		"Mahjong", "https://www.sloperama.com/mahjongg/", "`pcbj_mahjong()`", "CC-BY-ND-4.0",
		"Pinochle", "https://www.pagat.com/marriage/pinmain.html", "`pcbj_pinochle()`", "CC-BY-SA-3.0",
		"Spanish Suits: Castilian Pattern", "https://i-p-c-s.org/pattern/ps-27.html", "`pcbj_castilian_pattern()`", "CC-BY-4.0"
	)
	df$shareable <- TRUE
	df
}
