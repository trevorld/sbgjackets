#' Small Box Game Jacket Grobs
#'
#' `backNotesGrob()`, `creditsGrob()`, `spineIconGrob()`, and
#' `spineTextGrob()` are helper grob functions to help make small
#' box game jackets using [pnpmisc::pdf_create_jacket()].
#' @param title Title of small box game jacket (usually game name)
#' @param col Color of text/icons
#' @param size Target box size.  Either `"4x6"` or `"poker"`.
#' @param players A integer vector of allowed number of players
#' @param minutes An integer of number of minutes (larger number from BGG)
#' @param weight A double of game weight (number from BGG)
#' @return A grid grob object
#' @rdname helper_grobs
#' @examples
#' library("grid", include.only = c("gList", "gpar"))
#' sbgj_example <- function(output = NULL) {
#'   front <- fullGrob("#D55E00")
#'   back <- fullGrob("#009E73")
#'   spine <- gList(fullGrob("black"),
#'                  spineTextGrob("Example Spine"),
#'                  spineIconGrob(2:4, 30, 1.5))
#'   inner <- creditsGrob(icons = TRUE)
#'   pnpmisc::pdf_create_jacket(output, front = front, back = back,
#'                              spine = spine, inner = inner)
#' }
#' @export
spineTextGrob <- function(title, col = "white", size = c("4x6", "poker")) {
	size <- match.arg(size)
	if (size == "4x6") {
		fontsize <- "28"
		x <- unit(0.25, "in")
		y <- unit(0.25, "in")
	} else {
		fontsize <- "16"
		x <- unit(0.125, "in")
		y <- unit(0.200, "in")
	}
	textGrob(
		title,
		x,
		y,
		hjust = 0,
		vjust = 0,
		gp = gpar(fontsize = fontsize, col = col, fontface = "bold")
	)
}

bm_from_title <- function(title, instructions = TRUE) {
	bm <- data.frame(title = title, page = seq.int(by = 2L, length.out = length(title)))
	if (isTRUE(instructions)) {
		bmi <- data.frame(title = "Instructions", page = 1L)
		bm$page <- bm$page + 2L
		bm <- rbind(bmi, bm)
	}
	bm
}

prepend_instructions <- function(
	output,
	paper = getOption("papersize", "letter"),
	orientation = "landscape"
) {
	current_dev <- grDevices::dev.cur()
	pdf(NULL, width = pnpmisc:::JACKET_4x6_WIDTH, height = pnpmisc:::JACKET_4x6_HEIGHT)
	on.exit(invisible(dev.off()), add = TRUE)
	if (current_dev > 1) {
		on.exit(grDevices::dev.set(current_dev), add = TRUE)
	}

	f1 <- pnpmisc::pdf_create_jacket_instructions(
		paper = paper,
		style = sbgjackets_style(),
		orientation = orientation
	)
	mg <- marquee::marquee_grob(
		"This page intentionally left blank.",
		style = sbgjackets_style(),
		x = 0.5,
		y = 0.5,
		hjust = "center-ink",
		vjust = "center"
	)
	f2 <- pnpmisc::pdf_create_blank(paper = paper, orientation = orientation, grob = mg)
	f3 <- tempfile(fileext = ".pdf")

	qpdf::pdf_combine(c(f1, f2, output), f3) |>
		pnpmisc::pdf_compress(output, linearize = TRUE)

	unlink(f1)
	unlink(f2)
	unlink(f3)

	invisible(output)
}

#' @rdname helper_grobs
#' @param xmp A [xmpdf::xmp()] object with copyright/license information.
#' @param credits A character vector of (commonmark) credits to eventually be
#'                passed to [marquee::marquee_grob()].
#'                Will be collapsed to a single string by `paste(collapse = "\n")`,
#'                leading/trailing whitespace will be trimmed,
#'                any tab indentations will be unindented,
#'                and will be processed by [marquee::marquee_glue()].
#' @param icons If `TRUE` include Creative Commons credits for Games-icons.net icons.
#' @param ... Ignored for now.
#' @param x,width [grid::unit()] that will be passed to [marquee::marquee_grob()].
#' @export
creditsGrob <- function(
	xmp = xmpdf::xmp(),
	credits = character(),
	icons = FALSE,
	size = "4x6",
	...,
	width = NULL,
	x = NULL
) {
	check_dots_empty()
	# Prevents `marquee::marque_grob()` from leaving open a graphics device
	current_dev <- grDevices::dev.cur()
	pdf(NULL, width = pnpmisc:::JACKET_4x6_WIDTH, height = pnpmisc:::JACKET_4x6_HEIGHT)
	on.exit(invisible(dev.off()), add = TRUE)
	if (current_dev > 1) {
		on.exit(grDevices::dev.set(current_dev), add = TRUE)
	}
	credits <- paste(credits, collapse = "\n") |>
		trim_multistring()

	if (icons) {
		credits <- c(
			credits,
			"",
			"* Various icons from Game-icons.net",
			"",
			"  + {dQuote('Clockwork')} by Lorc <https://game-icons.net/1x1/lorc/clockwork.html>",
			"  + {dQuote('Person')} by Delapouite <https://game-icons.net/1x1/delapouite/person.html>",
			"  + {dQuote('Weight')} by Delapouite <https://game-icons.net/1x1/delapouite/weight.html>",
			"  + CC BY 3.0 license: <https://creativecommons.org/licenses/by/3.0/>"
		)
	}
	credits <- c(
		"# Credits",
		"",
		credits
	)
	if (size == "4x6") {
		credits <- c(
			credits,
			"",
			"* The Carlito font by {L_stroke}ukasz Dziedzic",
			"",
			"  + <https://fonts.google.com/specimen/Carlito>",
			"  + SIL Open Font License, Version 1.1"
		)
	}

	fn_expr <- try(as.list(sys.call(-1L))[[1L]], silent = TRUE)
	if (!inherits(fn_expr, "try-error")) {
		fn <- as.character(fn_expr)
		if (fn[[1L]] == "getFromNamespace") {
			fn <- eval(fn_expr[[2L]], parent.frame())
		} else {
			fn <- grep("^sbgj_|^pcbj_", fn, value = TRUE)
		}
		if (length(fn) == 1L && exists(fn, getNamespace("sbgjackets"))) {
			generated_by <- c(
				"* Generated in `R` by `sbgjackets::{fn}()`",
				"",
				"  + <https://github.com/trevorld/sbgjackets>",
				"  + MIT license"
			)
		} else {
			generated_by <- c(
				"* Generated in `R` by `pnpmisc::pdf_create_jacket()`",
				"",
				"  + <https://github.com/trevorld/pnpmisc>",
				"  + MIT license"
			)
		}
	}
	if (size == "4x6") {
		credits <- c(credits, "", generated_by)
	}
	if (!is.null(xmp$usage_terms)) {
		license <- xmp$usage_terms
	} else {
		license <- "Personal Use Only"
	}
	credits <- c("# License", "", license, "", credits)
	if (!is.null(xmp$title) && !is.null(xmp$attribution_name)) {
		title <- '*{xmp$title[["x-default"]]}* by {xmp$attribution_name}'
		if (!is.null(xmp$rights)) {
			title <- c(title, "", xmp$rights)
		}
		credits <- c(title, credits)
	}
	credits <- paste(credits, collapse = "\n") |>
		marquee::marquee_glue(.trim = FALSE)
	# cat(credits, sep = "\n")
	mg <- marquee::marquee_grob(
		credits,
		style = sbgjackets_style(size),
		width = width %||% unit(pnpmisc:::JACKET_4x6_FRONT_WIDTH + 1, "in"),
		x = x %||% unit(1 / 8, "in"),
		y = unit(1, "npc") - unit(1 / 8, "in")
	)
	if (is.null(xmp$spdx_id)) {
		grob_cc <- nullGrob()
	} else {
		badge <- piecepackr::spdx_license_list[xmp$spdx_id, "badge"]
		if (is.na(badge)) {
			grob_cc <- nullGrob()
		} else {
			cc_file <- system.file(paste0("extdata/badges/", badge), package = "piecepackr")
			cc_picture <- grImport2::readPicture(cc_file)
			grob_cc <- grImport2::symbolsGrob(
				cc_picture,
				x = unit(pnpmisc:::JACKET_4x6_FRONT_WIDTH / 2, "in"),
				y = unit(0.30, "in"),
				size = unit(0.9, "in")
			)
		}
	}

	gList(grob_cc, mg)
}

#' @rdname helper_grobs
#' @param notes A character vector of (commonmark) back cover notes.
#'              Will be collapsed to a single string by `paste(collapse = "\n")`,
#'              leading/trailing whitespace will be trimmed,
#'              any tab indentations will be unindented,
#'              and will be processed by [marquee::marquee_glue()].
#' @param style A `marquee` style set object
#'              (e.g. from [sbgjackets_style()]).
#' @export
backNotesGrob <- function(
	notes,
	col = "black",
	size = c("4x6", "poker"),
	...,
	style = sbgjackets_style(size, color = col)
) {
	#### check_dots_empty()
	size <- match.arg(size)
	notes <- paste(notes, collapse = "\n") |>
		trim_multistring() |>
		marquee::marquee_glue(.trim = FALSE)
	if (size == "4x6") {
		width <- unit(pnpmisc:::JACKET_4x6_FRONT_WIDTH, "in")
	} else {
		width <- unit(pnpmisc:::JACKET_POKER_FRONT_WIDTH, "in")
	}
	#### Bug in `marquee_grob()` that we need this hack?
	with(
		list(...),
		marquee::marquee_grob(
			notes,
			style = style,
			width = width,
			x = unit(1 / 8, "in"),
			y = unit(1, "npc") - unit(1 / 8, "in")
		)
	)
}

# vermillion <- "#D55E00"
# orange <- "#E69F00"
# bluishgreen <- "#009E73"

#' @rdname helper_grobs
#' @export
spineIconGrob <- function(players, minutes, weight, col = "white", size = "4x6") {
	r <- unit(0.3, "snpc")
	gp_rr <- gpar(col = col, fill = NA, lwd = 2)
	if (size == "4x6") {
		gp_text <- gpar(fontsize = 10, fontfamily = "Carlito", col = col)
		height = unit(17 / 32, "in")
		width = unit(17 / 16, "in")
		y = unit(0.125, "in")
	} else {
		gp_text <- gpar(fontsize = 8, fontfamily = "Carlito", col = col)
		height = unit(14 / 32, "in")
		width = unit(15 / 16, "in")
		y = unit(0.100, "in")
	}
	vp <- viewport(
		x = unit(1, "npc") - unit(0.125, "in"),
		y = y,
		just = c("right", "bottom"),
		height = height,
		width = width
	)
	gl <- gList(
		roundrectGrob(x = 1 / 6, width = 1 / 3, r = r, gp = gp_rr),
		textGrob(format_n_players(players), x = 1 / 6, y = 1 / 4, gp = gp_text),
		roundrectGrob(x = 3 / 6, width = 1 / 3, r = r, gp = gp_rr),
		textGrob(str_glue("{minutes}{prime}"), x = 3 / 6, y = 1 / 4, gp = gp_text),
		roundrectGrob(x = 5 / 6, width = 1 / 3, r = r, gp = gp_rr),
		textGrob(sprintf("%.1f", weight), x = 5 / 6, y = 1 / 4, gp = gp_text)
	)
	non_icons <- gTree(children = gl, vp = vp)
	mask <- grobTree(person_grob(), clockwork_grob(), weight_grob())
	icons <- rectGrob(
		gp = gpar(col = NA, fill = col),
		vp = vpStack(vp, viewport(mask = as.mask(mask)))
	)
	gList(non_icons, icons)
}

clockwork_grob <- function() {
	if (grDevices::dev.cur() == 1L) {
		on.exit(grDevices::graphics.off(), add = TRUE)
	}
	f <- system.file("icons/clockwork.svg", package = "sbgjackets")
	grImport2::pictureGrob(
		grImport2::readPicture(f),
		x = 3 / 6,
		width = 1 / 4,
		y = 2 / 3,
		clip = "off"
	)
}

person_grob <- function() {
	if (grDevices::dev.cur() == 1L) {
		on.exit(grDevices::graphics.off(), add = TRUE)
	}
	f <- system.file("icons/person.svg", package = "sbgjackets")
	grImport2::pictureGrob(
		grImport2::readPicture(f),
		x = 1 / 6,
		width = 1 / 3,
		y = 2 / 3,
		clip = "off"
	)
}

weight_grob <- function() {
	if (grDevices::dev.cur() == 1L) {
		on.exit(grDevices::graphics.off(), add = TRUE)
	}
	f <- system.file("icons/weight.svg", package = "sbgjackets")
	grImport2::pictureGrob(
		grImport2::readPicture(f),
		x = 5 / 6,
		width = 1 / 4,
		y = 2 / 3,
		clip = "off"
	)
}

format_n_players_fn <- function(formatted, x) {
	if (formatted$prev == -1) {
		# initialize
		return(list(prev = x, val = as.character(x)))
	}
	if (x - formatted$prev == 1) {
		# sequence
		if (str_sub(formatted$val, -2L, -2L) != en_dash) {
			return(list(prev = x, val = str_c(formatted$val, en_dash, x)))
		} else {
			str_sub(formatted$val, -1L, -1L) <- x
			formatted$prev <- x
			return(formatted)
		}
	}
	list(prev = x, val = paste0(formatted$val, ", ", x))
}

format_n_players <- function(players) {
	players <- unique(players)
	formatted <- list(prev = -1, val = "")
	Reduce(format_n_players_fn, players, formatted)$val
}

vp_inner_right <- function() {
	viewport(
		width = unit(4, "in"),
		height = unit(6, "in"),
		x = unit(1, "npc") - unit(0.5 * (pnpmisc:::JACKET_4x6_FRONT_WIDTH - 4), "in"),
		just = "right"
	)
}
