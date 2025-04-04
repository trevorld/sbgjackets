#' Helper grobs
#'
#' `creditsGrob()`, `spineIconGrob()`, and `spineTextGrob()`
#' are helper grob functions to help make small box game jackets
#' using [pnpmisc::pdf_create_jacket()].
#' @param title Title of small box game jacket (usually game name)
#' @param col Color of text/icons
#' @param players A integer vector of allowed number of players
#' @param minutes An integer of number of minutes (larger number from BGG)
#' @param weight A double of game weight (number from BGG)
#' @return A grid grob object
#' @rdname helper_grobs
#' @examples
#' library("grid", include.only = c("gList", "gpar", "rectGrob"))
#' sbgj_example <- function(output = NULL) {
#'   front <- rectGrob(gp = gpar(col = NA, fill = "#D55E00"))
#'   back <- rectGrob(gp = gpar(col = NA, fill = "#009E73"))
#'   spine <- gList(rectGrob(gp = gpar(col = NA, fill = "black")),
#'                  spineTextGrob("Example Spine"),
#'                  spineIconGrob(2:4, 30, 1.5))
#'   inner <- creditsGrob(icons = TRUE)
#'   pnpmisc::pdf_create_jacket(output, front = front, back = back,
#'                              spine = spine, inner = inner)
#' }
#' @export
spineTextGrob <- function(title, col = "white") {
    textGrob(title, unit(0.25, "in"), unit(0.25, "in"), hjust = 0, vjust = 0,
             gp = gpar(fontsize = "28", col = col))
}

prepend_instructions <- function(output, paper = "letter") {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev), add = TRUE)
    pdf(NULL, width = pnpmisc:::JACKET_WIDTH, height = pnpmisc:::JACKET_HEIGHT)
    on.exit(invisible(dev.off()), add = TRUE)
    f1 <- pnpmisc::pdf_create_jacket_instructions(paper = paper,
                                         style = credits_style())
    mg <- marquee::marquee_grob("This page intentionally left blank.",
                                style = credits_style(),
                                x = 0.5, y = 0.5,
                                hjust = "center-ink", vjust = "center")
    f2 <- pnpmisc::pdf_create_blank(paper = paper,
                                    orientation = "landscape",
                                    grob = mg)
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
#' @param icons If `TRUE` include Creative Commons credits for Games-icons.net icons.
#' @export
creditsGrob <- function(xmp = xmpdf::xmp(), credits = character(), icons = FALSE) {
    # Prevents `marquee::marque_grob()` from leaving open a graphics device
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev), add = TRUE)
    pdf(NULL, width = pnpmisc:::JACKET_WIDTH, height = pnpmisc:::JACKET_HEIGHT)
    on.exit(invisible(dev.off()), add = TRUE)

    if (icons) {
        credits <- c(credits, "",
                  "* Various icons from Game-icons.net",
                  "",
                  "  + {dQuote('Clockwork')} by Lorc https://game-icons.net/1x1/lorc/clockwork.html",
                  "  + {dQuote('Person')} by Delapouite https://game-icons.net/1x1/delapouite/person.html",
                  "  + {dQuote('Weight')} by Delapouite https://game-icons.net/1x1/delapouite/weight.html",
                  "  + CC BY 3.0 license: https://creativecommons.org/licenses/by/3.0/")
                  # "  + {dQuote('Clockwork')} icon by Lorc under CC BY 3.0",
                  # "  + https://game-icons.net/1x1/lorc/clockwork.html",
                  # "  + {dQuote('Person')} icon by Delapouite under CC BY 3.0",
                  # "  + https://game-icons.net/1x1/delapouite/person.html",
                  # "  + {dQuote('Weight')} icon by Delapouite under CC BY 3.0",
                  # "  + https://game-icons.net/1x1/delapouite/weight.html")
    }
    credits <- c("# Credits",
      "",
      credits,
      "",
      "* The Carlito font by \u0142ukasz Dziedzic",
      "",
      "  + https://fonts.google.com/specimen/Carlito",
      "  + SIL Open Font License, Version 1.1")

    fn <- try(as.character(as.list(sys.call(-1L))[[1L]]),
              silent = TRUE)
    if (!inherits(fn, "try-error") &&
        exists(fn, getNamespace("sbgjackets"))) {
        credits <- c(credits,
          "",
          "* Generated in `R` by `sbgjackets::{fn}()`",
          "",
          "  + https://github.com/trevorld/sbgjackets",
          "  + MIT license")
    } else {
        credits <- c(credits,
          "",
          "* Generated in `R` by `pnpmisc::pdf_create_jacket()`",
          "",
          "  + https://github.com/trevorld/pnpmisc",
          "  + MIT license")
    }
    if (!is.null(xmp$usage_terms)) {
        license <- xmp$usage_terms
    } else {
        license <- "Personal Use Only"
    }
    credits <- c("# License", "", license, "", credits)
    if (!is.null(xmp$title) && !is.null(xmp$attribution_name)) {
        title <- str_glue('*{xmp$title[["x-default"]]}* by {xmp$attribution_name}')
        if (!is.null(xmp$rights)) {
            title <- c(title, "", xmp$rights)
        }
        credits <- c(title, credits)
    }
    credits <- paste(credits, collapse = "\n") |> marquee::marquee_glue(.trim = FALSE)
    # cat(credits, sep = "\n")
    mg <- marquee::marquee_grob(credits,
                                style = credits_style(),
                                width = unit(pnpmisc:::JACKET_FACE_WIDTH + 1, "in"),
                                x = unit(1/8, "in"),
                                y = unit(1, "npc") - unit(1/8, "in"))
    if (is.null(xmp$spdx_id)) {
        grob_cc <- nullGrob()
    } else {
        badge <- piecepackr::spdx_license_list[xmp$spdx_id, "badge"]
        if (is.na(badge)) {
            grob_cc <- nullGrob()
        } else {
            cc_file <- system.file(paste0("extdata/badges/", badge),
                                   package="piecepackr")
            cc_picture <- grImport2::readPicture(cc_file)
            grob_cc <- grImport2::symbolsGrob(cc_picture,
                                              x=unit(pnpmisc:::JACKET_FACE_WIDTH / 2, "in"),
                                              y=unit(0.30, "in"),
                                              size=unit(0.9, "in"))
        }
    }

    gList(grob_cc, mg)
}

credits_style <- function() {
    marquee::classic_style(base_size = 10,
        body_font = "Carlito", header_font = "Carlito",
        lineheight = 1.6,
        margin = marquee::trbl(0, bottom = marquee::rem(0.7)),
        bullets = rep("\u2022", 3L)) |>
        marquee::modify_style("h1", border = NA,
                               size = marquee::relative(1.4),
                               border_size = marquee::trbl(NULL),
                               margin = marquee::trbl(NULL),
                               padding = marquee::trbl(NULL)) |>
        marquee::modify_style("ul", padding = marquee::trbl(right = marquee::em(1)))
}

# vermillion <- "#D55E00"
# orange <- "#E69F00"
# bluishgreen <- "#009E73"

#' @rdname helper_grobs
#' @export
spineIconGrob <- function(players, minutes, weight, col = "white") {
    r <- unit(0.3, "snpc")
    gp_text <- gpar(fontsize = 10, fontfamily = "Carlito", col = col)
    gp_rr <- gpar(col = col, fill = NA, lwd = 2)
    vp <- viewport(x = unit(1, "npc") - unit(0.125, "in"),
                   y = unit(0.125, "in"),
                   just = c("right", "bottom"),
                   height = unit(17/32, "in"),
                   width = unit(17/16, "in"))
    non_icons <- grobTree(roundrectGrob(x = 1/6, width = 1/3, r = r, gp = gp_rr),
                          textGrob(format_n_players(players), x = 1/6, y = 1/4, gp = gp_text),
                          roundrectGrob(x = 3/6, width = 1/3, r = r, gp = gp_rr),
                          textGrob(str_glue("{minutes}\u2032"), x = 3/6, y = 1/4, gp = gp_text),
                          roundrectGrob(x = 5/6, width = 1/3, r = r, gp = gp_rr),
                          textGrob(sprintf("%.1f", weight), x = 5/6, y = 1/4, gp = gp_text),
                          vp = vp)
    mask <- grobTree(person_grob(), clockwork_grob(), weight_grob())
    icons <- rectGrob(gp = gpar(col = NA, fill = col),
                      vp = vpStack(vp, viewport(mask = as.mask(mask))))
    gList(non_icons, icons)
}

clockwork_grob <- function() {
    f <- system.file("icons/clockwork.svg", package = "sbgjackets")
    grImport2::pictureGrob(grImport2::readPicture(f),
                           x = 3/6, width = 1/4, y = 2/3, clip = "off")
}

person_grob <- function() {
    f <- system.file("icons/person.svg", package = "sbgjackets")
    grImport2::pictureGrob(grImport2::readPicture(f),
                           x = 1/6, width = 1/3, y = 2/3, clip = "off")
}

weight_grob <- function() {
    f <- system.file("icons/weight.svg", package = "sbgjackets")
    grImport2::pictureGrob(grImport2::readPicture(f),
                           x = 5/6, width = 1/4, y = 2/3, clip = "off")
}

format_n_players_fn <- function(formatted, x) {
    if (formatted$prev == -1) # initialize
        return(list(prev = x, val = as.character(x)))
    if (x - formatted$prev == 1) { # sequence
        if (str_sub(formatted$val, -2L, -2L) != "\u2013") {
            return(list(prev = x, val = str_c(formatted$val, "\u2013", x)))
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

