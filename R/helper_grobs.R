#' Small Box Game Jacket Grobs
#'
#' `backNotesGrob()`, `creditsGrob()`, `spineIconGrob()`, and
#' `spineTextGrob()` are helper grob functions to help make small
#' box game jackets using [pnpmisc::pdf_create_jacket()].
#' @param title Title of small box game jacket (usually game name)
#' @param col Color of text/icons
#' @param size Target box size.  Either `"4x6"`, `"poker"`, or `"wallet"`.
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
spineTextGrob <- function(title, col = "white", size = c("4x6", "poker", "wallet")) {
	size <- match.arg(size)
	if (size == "4x6") {
		fontsize <- "28"
		x <- unit(0.25, "in")
		y <- unit(0.25, "in")
		hjust <- 0
		vjust <- 0
	} else if (size == "poker") {
		fontsize <- "16"
		x <- unit(0.125, "in")
		y <- unit(0.200, "in")
		hjust <- 0
		vjust <- 0
	} else {
		fontsize <- "16"
		# Offsets from the spine's center (npc 0.5) rather than an edge:
		# `pdf_create_wallet()`'s spine viewport grows symmetrically around
		# its own center as `bleed` increases, so npc 0.5 always maps back
		# to the same logical point (x=4in, y=3in at bleed=0) regardless of
		# bleed, while a bare `unit(2.125, "in")` would silently assume
		# bleed=0.
		x <- unit(0.5, "npc") - unit(1.875, "in")
		y <- unit(0.5, "npc")
		hjust <- 0
		vjust <- 0.5
	}
	textGrob(
		title,
		x,
		y,
		hjust = hjust,
		vjust = vjust,
		gp = gpar(fontsize = fontsize, col = col, fontface = "bold", fontfamily = "Carlito")
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
	ragg::agg_capture(
		width = pnpmisc:::JACKET_4x6_WIDTH,
		height = pnpmisc:::JACKET_4x6_HEIGHT,
		units = "in"
	)
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
	size = c("4x6", "poker", "wallet"),
	...,
	width = NULL,
	x = NULL
) {
	check_dots_empty()
	size <- match.arg(size)
	# Prevents `marquee::marque_grob()` from leaving open a graphics device
	current_dev <- grDevices::dev.cur()
	ragg::agg_capture(
		width = pnpmisc:::JACKET_4x6_WIDTH,
		height = pnpmisc:::JACKET_4x6_HEIGHT,
		units = "in"
	)
	on.exit(invisible(dev.off()), add = TRUE)
	if (current_dev > 1) {
		on.exit(grDevices::dev.set(current_dev), add = TRUE)
	}
	credits <- paste(credits, collapse = "\n") |>
		trim_multistring()

	if (icons) {
		icon_credits <- c(
			"",
			"* Various icons from Game-icons.net",
			"",
			"  + {dQuote('Clockwork')} by Lorc <https://game-icons.net/1x1/lorc/clockwork.html>",
			"  + {dQuote('Person')} by Delapouite <https://game-icons.net/1x1/delapouite/person.html>",
			"  + {dQuote('Weight')} by Delapouite <https://game-icons.net/1x1/delapouite/weight.html>",
			"  + CC BY 3.0 license: <https://creativecommons.org/licenses/by/3.0/>"
		)
	} else {
		icon_credits <- NULL
	}
	if (size == "4x6") {
		font_credits <- c(
			"",
			"* The Carlito font by {L_stroke}ukasz Dziedzic",
			"",
			"  + <https://fonts.google.com/specimen/Carlito>",
			"  + SIL Open Font License, Version 1.1"
		)
	} else {
		font_credits <- NULL
	}

	fn_expr <- try(as.list(sys.call(-1L))[[1L]], silent = TRUE)
	if (!inherits(fn_expr, "try-error") && size == "4x6") {
		fn <- as.character(fn_expr)
		if (fn[[1L]] == "getFromNamespace") {
			fn <- eval(fn_expr[[2L]], parent.frame(n = 2L))
		} else {
			fn <- grep("^sbgj_|^pcbj_|^pcw_", fn, value = TRUE)
		}
		if (length(fn) == 1L && exists(fn, getNamespace("sbgjackets"))) {
			generated_by_credits <- c(
				"",
				"* Generated in `R` by `sbgjackets::{fn}()`",
				"",
				"  + <https://github.com/trevorld/sbgjackets>",
				"  + MIT license"
			)
		} else {
			generated_by_credits <- c(
				"",
				"* Generated in `R` by `pnpmisc::pdf_create_jacket()`",
				"",
				"  + <https://github.com/trevorld/pnpmisc>",
				"  + MIT license"
			)
		}
	} else {
		generated_by_credits <- NULL
	}
	if (!is.null(xmp$usage_terms)) {
		license <- xmp$usage_terms
	} else {
		license <- "Personal Use Only"
	}
	license <- c("# License", "", license)
	if (!is.null(xmp$title) && !is.null(xmp$attribution_name)) {
		title <- '*{xmp$title[["x-default"]]}* by {xmp$attribution_name}'
		if (!is.null(xmp$rights)) {
			title <- c(title, "", xmp$rights)
		}
	} else {
		title <- NULL
	}
	if (size == "wallet") {
		creditsGrob_wallet(
			xmp = xmp,
			credits = credits,
			icon_credits = icon_credits,
			font_credits = font_credits,
			generated_by_credits = generated_by_credits,
			license = license,
			title = title,
			x = x
		)
	} else {
		creditsGrob_jacket(
			xmp = xmp,
			credits = credits,
			icon_credits = icon_credits,
			font_credits = font_credits,
			generated_by_credits = generated_by_credits,
			license = license,
			title = title,
			size = size,
			width = width,
			x = x
		)
	}
}

# Wallet spine layout: two rotated side-strips (left = title/license +
# CC badge, right = credits), clipped by the spine's trapezoid mask.
creditsGrob_wallet <- function(
	xmp,
	credits,
	icon_credits,
	font_credits,
	generated_by_credits,
	license,
	title,
	x
) {
	grob_cc <- license_badge_grob(
		xmp,
		x = unit(0.5, "npc"),
		y = unit(1.0, "npc") - unit(0.4, "in")
	)

	credits <- c(
		"# Credits",
		icon_credits,
		font_credits,
		generated_by_credits,
		"",
		credits
	)
	textl <- c(title, license)
	# `.envir = parent.frame()` reaches back to creditsGrob()'s frame (this
	# function's caller), where the `{fn}` glue placeholder used by
	# `generated_by_credits` would be defined, matching the pre-refactor
	# behavior of calling `marquee_glue()` directly inside creditsGrob().
	textl <- paste(textl, collapse = "\n") |>
		marquee::marquee_glue(.trim = FALSE, .envir = parent.frame())
	# Horizontally centered since the spine mask clips increasingly
	# aggressively away from the middle of this strip, unlike the
	# corner-anchored jackets. Still top-anchored vertically.
	mgl <- marquee::marquee_grob(
		textl,
		style = sbgjackets_style("wallet", align = "center"),
		width = unit(4.75, "in"),
		x = x %||% unit(0.5, "npc"),
		y = unit(1, "npc") - unit(1 / 8, "in") - unit(0.55, "in"),
		hjust = "center-ink"
	)
	# Credits go in the left inside strip (x=[0,2]) of the spine viewport,
	# rotated so text runs along the spine's y-axis. Offset from the
	# spine's center (npc 0.5), see spineTextGrob().
	gl <- gTree(
		children = gList(mgl, grob_cc),
		vp = viewport(
			x = unit(0.5, "npc") - unit(3, "in"),
			y = unit(0.5, "npc"),
			width = unit(4.75, "in"),
			height = unit(1.75, "in"),
			angle = -90
		)
	)
	textr <- paste(credits, collapse = "\n") |>
		marquee::marquee_glue(.trim = FALSE, .envir = parent.frame())
	# Horizontally centered since the spine mask clips increasingly
	# aggressively away from the middle of this strip, unlike the
	# corner-anchored jackets. Still top-anchored vertically.
	mgr <- marquee::marquee_grob(
		textr,
		style = sbgjackets_style("wallet", align = "center"),
		width = unit(4.75, "in"),
		x = x %||% unit(0.5, "npc"),
		y = unit(1, "npc") - unit(1 / 8, "in"),
		hjust = "center-ink"
	)
	# Credits go in the right inside strip (x=[6,8]) of the spine viewport,
	# rotated so text runs along the spine's y-axis. Offset from the
	# spine's center (npc 0.5), see spineTextGrob().
	gr <- gTree(
		children = gList(mgr),
		vp = viewport(
			x = unit(0.5, "npc") + unit(3, "in"),
			y = unit(0.5, "npc"),
			width = unit(4.75, "in"),
			height = unit(1.75, "in"),
			angle = 90
		)
	)
	gList(gl, gr)
}

# 4x6/poker layout: a single corner-anchored license badge + credits block.
creditsGrob_jacket <- function(
	xmp,
	credits,
	icon_credits,
	font_credits,
	generated_by_credits,
	license,
	title,
	size,
	width,
	x
) {
	grob_cc <- license_badge_grob(xmp)

	credits <- c(
		"# Credits",
		"",
		credits,
		icon_credits,
		font_credits,
		generated_by_credits
	)
	text <- c(title, license, "", credits)
	# `.envir = parent.frame()` reaches back to creditsGrob()'s frame (this
	# function's caller), where the `{fn}` glue placeholder used by
	# `generated_by_credits` would be defined, matching the pre-refactor
	# behavior of calling `marquee_glue()` directly inside creditsGrob().
	text <- paste(text, collapse = "\n") |>
		marquee::marquee_glue(.trim = FALSE, .envir = parent.frame())
	mg <- marquee::marquee_grob(
		text,
		style = sbgjackets_style(size),
		width = width %||% unit(pnpmisc:::JACKET_4x6_FRONT_WIDTH + 1, "in"),
		x = x %||% unit(1 / 8, "in"),
		y = unit(1, "npc") - unit(1 / 8, "in")
	)

	gList(grob_cc, mg)
}

license_badge_grob <- function(
	xmp,
	x = unit(pnpmisc:::JACKET_4x6_FRONT_WIDTH / 2, "in"),
	y = unit(0.30, "in")
) {
	if (is.null(xmp$spdx_id)) {
		return(nullGrob())
	}
	badge <- piecepackr::spdx_license_list[xmp$spdx_id, "badge"]
	if (is.na(badge)) {
		return(nullGrob())
	}
	cc_file <- system.file(paste0("extdata/badges/", badge), package = "piecepackr")
	cc_picture <- grImport2::readPicture(cc_file)
	grImport2::symbolsGrob(cc_picture, x = x, y = y, size = unit(0.9, "in"))
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
	size = c("4x6", "poker", "wallet"),
	...,
	style = sbgjackets_style(size, color = col)
) {
	size <- match.arg(size)
	notes <- paste(notes, collapse = "\n") |>
		trim_multistring() |>
		marquee::marquee_glue(.trim = FALSE)
	if (size == "4x6") {
		width <- unit(pnpmisc:::JACKET_4x6_FRONT_WIDTH, "in")
		y_pos <- unit(1, "npc") - unit(1 / 8, "in")
	} else if (size == "poker") {
		width <- unit(pnpmisc:::JACKET_POKER_FRONT_WIDTH, "in")
		y_pos <- unit(1, "npc") - unit(1 / 8, "in")
	} else {
		# Front viewport is 2"x8"; safe zone for wallet is y=[2,6].
		# Offset from the front/back cover's center (npc 0.5) rather than
		# an edge, since `pdf_create_wallet()`'s front/back viewports grow
		# symmetrically around their own center as `bleed` increases; see
		# spineTextGrob().
		width <- unit(1.75, "in")
		y_pos <- unit(0.5, "npc") + unit(1.875, "in")
	}
	# Prevents `marquee::marquee_grob()` from leaving open a graphics device
	current_dev <- grDevices::dev.cur()
	ragg::agg_capture(
		width = pnpmisc:::JACKET_4x6_WIDTH,
		height = pnpmisc:::JACKET_4x6_HEIGHT,
		units = "in"
	)
	on.exit(invisible(dev.off()), add = TRUE)
	if (current_dev > 1) {
		on.exit(grDevices::dev.set(current_dev), add = TRUE)
	}
	# `...` (e.g. `around_table = around_table` in `pcbj_pinochle()`) is bound
	# into scope here so markdown image tags like `![](around_table)` can
	# resolve the object by name when `marquee_grob()` renders them.
	# If {marquee} agrees to look for such objects higher up the call stack
	# instead of just its own environment, this workaround (and `...`) could
	# be dropped in favor of `check_dots_empty()`.
	with(
		list(...),
		marquee::marquee_grob(
			notes,
			style = style,
			width = width,
			x = unit(1 / 8, "in"),
			y = y_pos
		)
	)
}

# vermillion <- "#D55E00"
# orange <- "#E69F00"
# bluishgreen <- "#009E73"

#' @rdname helper_grobs
#' @export
spineIconGrob <- function(
	players,
	minutes,
	weight,
	col = "white",
	size = c("4x6", "poker", "wallet")
) {
	size <- match.arg(size)
	r <- unit(0.3, "snpc")
	gp_rr <- gpar(col = col, fill = NA, lwd = 2)
	if (size == "4x6") {
		gp_text <- gpar(fontsize = 10, fontfamily = "Carlito", col = col)
		height <- unit(17 / 32, "in")
		width <- unit(17 / 16, "in")
		vp <- viewport(
			x = unit(1, "npc") - unit(0.125, "in"),
			y = unit(0.125, "in"),
			just = c("right", "bottom"),
			height = height,
			width = width
		)
	} else if (size == "poker") {
		gp_text <- gpar(fontsize = 8, fontfamily = "Carlito", col = col)
		height <- unit(14 / 32, "in")
		width <- unit(15 / 16, "in")
		vp <- viewport(
			x = unit(1, "npc") - unit(0.125, "in"),
			y = unit(0.100, "in"),
			just = c("right", "bottom"),
			height = height,
			width = width
		)
	} else {
		# wallet
		gp_text <- gpar(fontsize = 6, fontfamily = "Carlito", col = col)
		height <- unit(9 / 32, "in")
		width <- unit(9 / 16, "in")
		# Offset from the spine's center (npc 0.5), see spineTextGrob().
		vp <- viewport(
			x = unit(0.5, "npc") + unit(1.875, "in"),
			y = unit(0.5, "npc"),
			just = c("right", "center"),
			height = height,
			width = width
		)
	}
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
