#' Create SBG Jacket for Looney Pyramids
#'
#' `sbgj_looney_pyramids()` creates a small box game jacket for [Looney Pyramids](https://www.looneylabs.com/pyramids-home).
#' `sbgj_homeworlds()` creates a small box game jacket for [Homeworlds](https://www.looneylabs.com/games/homeworlds).
#' `sbgj_ice_duo()` creates a small box game jacket for [Ice Duo](https://www.looneylabs.com/games/ice-duo).
#' `sbgj_jinxx()` creates a small box game jacket for [Jinxx](https://www.looneylabs.com/games/jinxx).
#' `sbgj_martian_chess()` creates a small box game jacket for [Martian Chess](https://www.looneylabs.com/games/martian-chess).
#' `sbgj_nomids()` creates a small box game jacket for [Nomids](https://www.looneylabs.com/games/nomids).
#' `sbgj_looney_pyramids_all()` creates all of those into a single pdf file.
#'
#' Note that these print-and-play small box game jackets are for **Personal Use Only**.
#' These jackets use images by Looney Labs.  These jackets are **not** for distribution.
#' For more information see the Looney Labs FAQ: <https://faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774>.
#'
#' @param output Output file name.  Defaults to `tempfile(fileext = ".pdf")`.
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @rdname sbgj_looney
#' @export
sbgj_looney_pyramids_all <- function(output = NULL) {
    stopifnot(piecepackr::has_font("Carlito"),
              requireNamespace("ppdf"),
              requireNamespace("xmpdf"),
              xmpdf::supports_set_bookmarks(),
              xmpdf::supports_set_docinfo(),
              xmpdf::supports_set_xmp())

    output <- pnpmisc:::normalize_output(output)

    bm <- data.frame(title = c("Looney Pyramids",
                               "Homeworlds",
                               "Ice Duo",
                               "Jinxx",
                               "Martian Chess",
                               "Martian Chess (Silver)",
                               "Nomids"),
                     page = seq.int(1L, by = 2L, length.out = 7L))

    xmp <- xmpdf::xmp(creator = "Trevor L. Davis",
                      title = "Looney Pyramids Small Box Game Jackets")

    lp <- sbgj_looney_pyramids()
    hm <- sbgj_homeworlds()
    id <- sbgj_ice_duo()
    jx <- sbgj_jinxx()
    nomids <- sbgj_nomids()
    mc1 <- sbgj_martian_chess()
    mc2 <- sbgj_martian_chess(silver = TRUE)
    output_c <- tempfile(fileext = ".pdf")

    qpdf::pdf_combine(c(lp, hm, id, jx, mc1, mc2, nomids), output_c) |>
        pnpmisc::pdf_set_bookmarks(bookmarks = bm) |>
        pnpmisc::pdf_set_xmp(xmp = xmp) |>
        pnpmisc::pdf_set_docinfo(docinfo = xmpdf::as_docinfo(xmp)) |>
        pnpmisc::pdf_compress(output, linearize = TRUE)

    pnpmisc::rm_temp_pdfs()

    invisible(output)
}

#' @rdname sbgj_looney
#' @export
sbgj_looney_pyramids <- function(output = NULL) {
    stopifnot(piecepackr::has_font("Carlito"))

    dir <- tools::R_user_dir("sbgjackets", "data")
    if (!dir.exists(dir))
        dir.create(dir, recursive = TRUE)

    background_col <- "#EFE8D5FF"
    text_col <- "#1C3160FF"

    logo <- normalizePath(file.path(dir, "LP4TopHeader.png"), mustWork = FALSE)
    if (!file.exists(logo)) {
        download.file("https://www.looneylabs.com/sites/default/files/LP4%20Top%20Header.png",
                      logo)
    }
    bm_logo <- magick::image_read(logo) |> as_bm_pixmap() |>
        bm_replace(text_col, "black") |>
        bm_replace(text_col, "#191A17FF") |>
        bm_replace(text_col, "#1C1D1AFF") |>
        rasterGrob(y = 0.96, just = "top", width = unit(0.94, "npc"), interpolate = FALSE)

    zoom <- normalizePath(file.path(dir, "PyramidArcadeZoomBackground.jpg"), mustWork = FALSE)
    if (!file.exists(zoom)) {
        download.file("https://www.looneylabs.com/sites/default/files/marketing_images/PyramidArcadeZoomBackground.jpg",
        zoom)
    }
    pamphlet <- normalizePath(file.path(dir, "Pyramids_Intro_Pamphlet7.pdf"), mustWork = FALSE)
    if (!file.exists(pamphlet))
        download.file("https://www.looneylabs.com/sites/default/files/literature/Pyramids_Intro_Pamphlet7.pdf",
                      pamphlet)

    bm_zoom <- magick::image_read(zoom) |> as_bm_pixmap() |>
        rasterGrob(y = 0, just = "bottom")

    bm_pamphlet <- pnpmisc::pdf_render_bm_pixmap(pamphlet, page = 1L)
    bm_pamphlet <- bm_pamphlet[, seq.int(ncol(bm_pamphlet) / 2 + 1, ncol(bm_pamphlet))]
    bm_pamphlet <- cbind(bm_pamphlet[, seq.int(ncol(bm_pamphlet) / 2 + 1, ncol(bm_pamphlet))],
                         bm_pamphlet[, seq.int(ncol(bm_pamphlet) / 2)])
    bm_pamphlet[1:90, seq.int(ncol(bm_pamphlet) / 2)] <- background_col
    bm_pamphlet[1080:1200, 170:830] <- background_col
    bm_pamphlet <- bm_pamphlet |> 
        bm_replace(background_col) |>
        bm_replace(background_col, "#EDE4D3FF") |>
        bm_replace(background_col, "#EEE5D4FF") |>
        rasterGrob(y = 0.84, just = "top")

    bm_pamphlet2 <- pnpmisc::pdf_render_bm_pixmap(pamphlet, page = 2L)
    bm_pamphlet2 <- bm_pamphlet2[, seq.int(ceiling(0.75 * ncol(bm_pamphlet2)))]
    bm_pamphlet2 <- bm_pamphlet2 |> 
        bm_replace(background_col) |>
        bm_replace(background_col, "#EEE5D4FF") |>
        bm_replace(background_col, "#EDE4D3FF") |>
        rasterGrob(y = 0.05, just = "bottom", width = 0.95)

    #   Alien City
    # * Apophis (Zark City deck, Uno deck, etc., d6, 5 tokens)
    # * Black Ice (3 lightning dice)
    # * BLAM! (chess board)
    # * Caldera (volcano) (4 x 6 trios plus 6 small black plus 5x5 grid)
    # * Closest Ghost (2 ludo boards, 16 pieces of candy)
    # * Freeze Tag (pyramid die, 5x5 grid)
    # * Give or Take (1 pyramid die) deprecated by Nomids
    #   Gleebs and Grues (4x4 grid)
    # * Gnostica
    # * Homeworlds (turn token, optional 3x4 grid) (BYRG)
    # * Hijinks (1 pyramid die, 3x3 board)
    # * Icehouse
    # * Ice Towers
    # * Jinxx (3x3 grid, pyramid die)
    # * Pharaoh (5x5 grid, 2 d6)
    # * Martian Chess (2 4x4 grid)
    # * Penguin Soccer (8x8 board)
    # * Pikemen (chess board)
    # * Powerhouse (3 tokens such as dice, cloth bag)
    #   Pylon (5x6 grid)
    # * Pyramid-Sham-Bo (color reminders)
    #   Quicksand (4 martian coasters)
    # * RAMbots (chessboard plus screens)
    # * Skurdir
    # * Solomids (5x10 grid plus cloth bag)
    # * StarRunners (2 d6)
    #   Synapse-Ice (5x6 grid)
    # * Tic Tac Doh! (3x3 grid)
    # * Treehouse
    # * Zarcana
    # * Zark City
    # * Zendo
    front <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)))
    badges <- c("BlackIce", "Blam", "Volcano", "ClosestGhost", 
                "FreezeTag", "Gnostica", "Homeworlds", "IceHouse", 
                "IceTowers", "Jinxx", "Pharaoh", "MartianChess",
                "PenguinSoccer", "Pikemen", "Powerhouse", "PyramidShamBo",
                "RAMbots", "Skurdir", "StarRunners", "TicTacDoh",
                "Treehouse", "Zarcana", "ZarkCity", "Zendo")
    l_badges <- load_pyramid_badges()
    xs <- rep(1:4 / 4 - 1/8, 6L)
    ys <- 1 - rep(1:6 / 6 - 1/12, each = 4L)
    for (i in seq_along(badges)) {
        vp <- viewport(x = xs[i], y = ys[i], width = 1/4, height = 1/6)
        front[[i + 1L]] <- editGrob(l_badges[[badges[i]]], vp = vp)
    }

    back <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), 
                  bm_logo, bm_pamphlet, bm_pamphlet2)
    spine <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)),
                   spineTextGrob("Looney Pyramids", col = text_col))

    xmp <- xmpdf::xmp(creator = "Trevor L. Davis",
                      title = "Looney Pyramids Small Box Game Jacket")
    credits <- c("* From https://www.looneylabs.com/",
                 "",
                 "  + *Looney Pyramids Intro Pamphlet* (cropped and edited)",
                 # "  + *Looney Pyramids - Zoom*",
                 "  + https://www.looneylabs.com/sites/default/files/LP4%20Top%20Header.png",
                 "  + From https://www.looneylabs.com/pyramid-arcade-game-badges-boards",
                 "",
                 "    the badges of 24 Looney Pyramids games",
                 "  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
                 "",
                 "    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law.")
    cr_grob <- creditsGrob(xmp, credits, icons = FALSE)
    inner <- gList(cr_grob, bank_grob())

    output <- pdf_create_jacket(output = output, front = front, back = back,
                                spine = spine, inner = inner)

    if (xmpdf::supports_set_xmp()) {
        xmpdf::set_xmp(xmp, output)
    }
    if (xmpdf::supports_set_docinfo()) {
        xmpdf::set_docinfo(xmpdf::as_docinfo(xmp), output)
    }
    invisible(output)
}

bank_grob <- function() {
    xs <- unit(c(0.90, 2, 3.10), "in")
    ys <- unit(c(0.8, 1.9, 3, 4.1, 5.2), "in")
    w <- unit(9/16, "in")
    vp <- viewport(width = unit(4, "in"), height = unit(6, "in"),
                   x = unit(1, "npc") - unit(0.5 * (pnpmisc:::JACKET_FACE_WIDTH - 4), "in"), 
                   just = "right")
    grobTree(rectGrob(xs, y = ys[5L], width = w, height = w,
                      gp = gpar(col = "black", fill = "black", lwd = 4)),
             rectGrob(xs, y = ys[4L], width = w, height = w,
                      gp = gpar(col = "black", fill = "#009e73", lwd = 4)), # bluish-green
             rectGrob(xs, y = ys[3L], width = w, height = w,
                      gp = gpar(col = "black", fill = "#D55E00", lwd = 4)), # vermillion
             rectGrob(xs, y = ys[2L], width = w, height = w,
                      gp = gpar(col = "black", fill = "#F0E442", lwd = 4)), # yellow
             rectGrob(xs, y = ys[1L], width = w, height = w,
                      gp = gpar(col = "black", fill = "#0072B2", lwd = 4)),  # blue
             segmentsGrob(x0 = rep(xs - 0.5 * w, 3L),
                          x1 = rep(xs + 0.5 * w, 3L),
                          y0 = rep(ys[4:2] + 0.5 * w,  each = 3L),
                          y1 = rep(ys[4:2] - 0.5 * w, each = 3L),
                          gp = gpar(col = "black", lwd = 4)),
             segmentsGrob(x0 = rep(xs - 0.5 * w, 3L),
                          x1 = rep(xs + 0.5 * w, 3L),
                          y0 = rep(ys[4:2] - 0.5 * w,  each = 3L),
                          y1 = rep(ys[4:2] + 0.5 * w, each = 3L),
                          gp = gpar(col = "black", lwd = 4)),
             # textGrob("X", x = rep(xs, 3L), y = rep(ys[2:4], each = 3L)),
             vp = vp)
}

load_pyramid_badges <- function() {
    dir <- tools::R_user_dir("sbgjackets", "data")
    if (!dir.exists(dir))
        dir.create(dir, recursive = TRUE)

    badges <- c("BlackIce", "ColorWheel", "GiveOrTake",
                "Hijinks", "Homeworlds", "IceDice",
                "IceTowers", "Launchpad23", "LooneyLudo",
                "LunarInvaders", "MartianChess", "PetalBattle",
                "PetriDish", "Pharaoh", "Powerhouse",
                "PyramidShamBo", "Treehouse", "TwinWin",
                "Verticality", "Volcano", "WorldWar5", "ZarkCity",
                "AquariusRising", "Blam", "CarrotsBroccoli",
                "FreezeTag", "Gnostica", "IceHouse",
                "PenguinSoccer", "RAMbots",
                "Skurdir", "Solomids", "Zendo",
                "Apophis", "BowlerRink", "ClosestGhost")
    g_badges <- c("PyramidBall")
    patches <- c("IceToids", "Jinxx", "Kickback",
                 "LavaFlows", "Logger", "Nomids",
                 "NothingBeats", "Pikemen", "Sandships",
                 "StarRunners", "TicTacDoh", "Zarcana")

    l_badges <- list()

    for (badge in badges) {
        Sys.sleep(1)
        f <- normalizePath(file.path(dir, str_glue("{badge}.png")), mustWork = FALSE)
        if (!file.exists(f))
            download.file(str_glue("https://www.looneylabs.com/sites/default/files/{badge}.png"), f)
        l_badges[[badge]] <- magick::image_read(f) |> as_bm_pixmap() |> rasterGrob()
    }
    for (badge in g_badges) {
        Sys.sleep(1)
        f <- normalizePath(file.path(dir, str_glue("{badge}.png")), mustWork = FALSE)
        if (!file.exists(f))
            download.file(str_glue("https://www.looneylabs.com/sites/default/files/{badge}.g.png"), f)
        l_badges[[badge]] <- magick::image_read(f) |> as_bm_pixmap() |> rasterGrob()
    }
    for (badge in patches) {
        Sys.sleep(1)
        f <- normalizePath(file.path(dir, str_glue("{badge}.png")), mustWork = FALSE)
        if (!file.exists(f))
            download.file(str_glue("https://www.looneylabs.com/sites/default/files/{badge}.patch_.png"), f)
        l_badges[[badge]] <- magick::image_read(f) |> as_bm_pixmap() |> rasterGrob()
    }
    l_badges
}

#' @rdname sbgj_looney
#' @export
sbgj_homeworlds <- function(output = NULL) {
    stopifnot(piecepackr::has_font("Carlito"))

    dir <- tools::R_user_dir("sbgjackets", "data")
    if (!dir.exists(dir))
        dir.create(dir, recursive = TRUE)

    cover <- normalizePath(file.path(dir, "HomeworldsBoxFront.jpg"), mustWork = FALSE)
    if (!file.exists(cover)) {
        download.file("https://www.looneylabs.com/sites/default/files/marketing_images/HomeworldsBoxFront.jpg",
                      cover)
    }

    back <- normalizePath(file.path(dir, "HomeworldsBoxBack.jpg"), mustWork = FALSE)
    if (!file.exists(back)) {
        download.file("https://www.looneylabs.com/sites/default/files/marketing_images/HomeworldsBoxBack.jpg",
                      back)
    }

    background_col <- "#EFE8D5FF"
    text_col <- "#1C3160FF"

    bm_cover <- magick::image_read(cover) |> 
        as_bm_pixmap() |> 
        bm_trim(left = 10L, right = 10L) |>
        rasterGrob(height = 0.8)

    bm_back <- magick::image_read(back) |> 
        as_bm_pixmap() |> 
        bm_trim(left = 10L, right = 10L, bottom = 10L, top = 10L) |>
        rasterGrob(height = 0.85)


    front <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), bm_cover)
    back <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), bm_back)
    spine <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)),
                   spineTextGrob("Homeworlds", col = text_col),
                   spineIconGrob(2, 60, 3.43, text_col))

    xmp <- xmpdf::xmp(creator = "Trevor L. Davis",
                      title = "Homeworlds Small Box Game Jacket")
    credits <- c("* From https://www.looneylabs.com/resources/game/Homeworlds",
                 "",
                 "  + *Homeworlds Box Front*",
                 "  + *Homeworlds Box Back*",
                 "  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
                 "",
                 "    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law.")

    inner <- creditsGrob(xmp, credits, icons = TRUE)

    output <- pdf_create_jacket(output = output, front = front, back = back,
                                spine = spine, inner = inner)

    if (xmpdf::supports_set_xmp()) {
        xmpdf::set_xmp(xmp, output)
    }
    if (xmpdf::supports_set_docinfo()) {
        xmpdf::set_docinfo(xmpdf::as_docinfo(xmp), output)
    }
    invisible(output)
}

#' @rdname sbgj_looney
#' @export
sbgj_ice_duo <- function(output = NULL) {
    stopifnot(piecepackr::has_font("Carlito"))

    dir <- tools::R_user_dir("sbgjackets", "data")
    if (!dir.exists(dir))
        dir.create(dir, recursive = TRUE)

    cover <- normalizePath(file.path(dir, "IceDuoBoxFront.jpg"), mustWork = FALSE)
    if (!file.exists(cover)) {
        download.file("https://www.looneylabs.com/sites/default/files/marketing_images/IceDuoBoxFront.jpg",
                      cover)
    }

    back <- normalizePath(file.path(dir, "IceDuoBoxBack.jpg"), mustWork = FALSE)
    if (!file.exists(back)) {
        download.file("https://www.looneylabs.com/sites/default/files/marketing_images/IceDuoBoxBack.jpg",
                      back)
    }

    background_col <- "#EFE8D5FF"
    text_col <- "#1C3160FF"

    bm_cover <- magick::image_read(cover) |> 
        as_bm_pixmap() |> 
        bm_trim(left = 10L, right = 10L) |>
        rasterGrob(height = 0.8)

    bm_back <- magick::image_read(back) |> 
        as_bm_pixmap() |> 
        bm_trim(left = 10L, right = 10L, bottom = 10L, top = 10L) |>
        rasterGrob(height = 0.85)


    front <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), bm_cover)
    back <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), bm_back)
    spine <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)),
                   spineTextGrob("Ice Duo", col = text_col),
                   spineIconGrob(2, 30, 1.50, text_col))

    xmp <- xmpdf::xmp(creator = "Trevor L. Davis",
                      title = "Ice Duo Small Box Game Jacket")
    credits <- c("* From https://www.looneylabs.com/resources/game/Ice%20Duo",
                 "",
                 "  + *Ice Duo Box Front*",
                 "  + *Ice Duo Box Back*",
                 "  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
                 "",
                 "    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law.")

    inner <- creditsGrob(xmp, credits, icons = TRUE)

    output <- pdf_create_jacket(output = output, front = front, back = back,
                                spine = spine, inner = inner)

    if (xmpdf::supports_set_xmp()) {
        xmpdf::set_xmp(xmp, output)
    }
    if (xmpdf::supports_set_docinfo()) {
        xmpdf::set_docinfo(xmpdf::as_docinfo(xmp), output)
    }
    invisible(output)
}

#' @rdname sbgj_looney
#' @export
sbgj_jinxx <- function(output = NULL) {
    stopifnot(piecepackr::has_font("Carlito"))

    dir <- tools::R_user_dir("sbgjackets", "data")
    if (!dir.exists(dir))
        dir.create(dir, recursive = TRUE)

    cover <- normalizePath(file.path(dir, "JinxxFlatBoxFront.png"), mustWork = FALSE)
    if (!file.exists(cover)) {
        download.file("https://www.looneylabs.com/sites/default/files/marketing_images/JinxxFlatBoxFront.png",
                      cover)
    }

    back <- normalizePath(file.path(dir, "JinxxFlatBoxBack.png"), mustWork = FALSE)
    if (!file.exists(back)) {
        download.file("https://www.looneylabs.com/sites/default/files/marketing_images/JinxxFlatBoxBack.png",
                      back)
    }

    background_col <- "#EFE8D5FF"
    text_col <- "#1C3160FF"

    bm_cover <- magick::image_read(cover) |> 
        as_bm_pixmap() |> 
        bm_trim(left = 300L, right = 300L) |>
        rasterGrob(height = 0.8)

    bm_back <- magick::image_read(back) |> 
        as_bm_pixmap() |> 
        bm_trim(left = 300L, right = 300L) |>
        rasterGrob(height = 0.85)


    front <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), bm_cover)
    back <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), bm_back)
    spine <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)),
                   spineTextGrob("Jinxx", col = text_col),
                   spineIconGrob(2:4, 30, 1.50, text_col))

    xmp <- xmpdf::xmp(creator = "Trevor L. Davis",
                      title = "Jinxx Small Box Game Jacket")
    credits <- c("* From https://www.looneylabs.com/resources/game/Jinxx",
                 "",
                 "  + *Jinxx Box Front*",
                 "  + *Jinxx Box Back*",
                 "  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
                 "",
                 "    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law.")

    inner <- creditsGrob(xmp, credits, icons = TRUE)

    output <- pdf_create_jacket(output = output, front = front, back = back,
                                spine = spine, inner = inner)

    if (xmpdf::supports_set_xmp()) {
        xmpdf::set_xmp(xmp, output)
    }
    if (xmpdf::supports_set_docinfo()) {
        xmpdf::set_docinfo(xmpdf::as_docinfo(xmp), output)
    }
    invisible(output)
}

#' @rdname sbgj_looney
#' @param ... Should be left empty.
#' @param silver If `TRUE` make jacket for silver Martian Chess.
#' @export
sbgj_martian_chess <- function(output = NULL, ..., silver = FALSE) {
    check_dots_empty()
    stopifnot(piecepackr::has_font("Carlito"))

    dir <- tools::R_user_dir("sbgjackets", "data")
    if (!dir.exists(dir))
        dir.create(dir, recursive = TRUE)

    cover <- normalizePath(file.path(dir, "MartianChessBoxFront.jpg"), mustWork = FALSE)
    if (!file.exists(cover)) {
        download.file("https://www.looneylabs.com/sites/default/files/marketing_images/MartianChessBoxFront.jpg",
                      cover)
    }

    if (silver) {
        back <- normalizePath(file.path(dir, "MartianChessSilverFlatBoxBack.png"), mustWork = FALSE)
        if (!file.exists(back)) {
            download.file("https://www.looneylabs.com/sites/default/files/marketing_images/MartianChessSilverFlatBoxBack.png",
                          back)
        }
    } else {
        back <- normalizePath(file.path(dir, "MartianChessBoxBack.jpg"), mustWork = FALSE)
        if (!file.exists(back)) {
            download.file("https://www.looneylabs.com/sites/default/files/marketing_images/MartianChessBoxBack.jpg",
                          back)
        }
    }

    background_col <- "#EFE8D5FF"
    text_col <- "#1C3160FF"

    bm_cover <- magick::image_read(cover) |> 
        as_bm_pixmap() |> 
        rasterGrob()
    if (silver) {
        bm_back <- magick::image_read(back) |> 
            as_bm_pixmap() |> 
            bm_trim(left = 300L, right = 300L) |>
            rasterGrob(height = 0.85)
    } else {
        bm_back <- magick::image_read(back) |> 
            as_bm_pixmap() |> 
            bm_trim(bottom = 10L, top = 10L) |>
            rasterGrob()
    }


    front <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), bm_cover)
    back <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), bm_back)
    spine <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)),
                   spineTextGrob("Martian Chess", col = text_col),
                   spineIconGrob(2, 20, 2.42, text_col))

    if (silver) {
        xmp <- xmpdf::xmp(creator = "Trevor L. Davis",
                          title = "Martian Chess (Silver) Small Box Game Jacket")
        credits <- c("* From https://www.looneylabs.com/resources/game/Martian%20Chess",
                     "",
                     "  + *Martian Chess Box Front*",
                     "  + *Martian Chess Silver Box Back*",
                     "  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
                     "",
                     "    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law.")
    } else {
        xmp <- xmpdf::xmp(creator = "Trevor L. Davis",
                          title = "Martian Chess Small Box Game Jacket")
        credits <- c("* From https://www.looneylabs.com/resources/game/Martian%20Chess",
                     "",
                     "  + *Martian Chess Box Back*",
                     "  + *Martian Chess Box Front*",
                     "  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
                     "",
                     "    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law.")
    }
    inner <- creditsGrob(xmp, credits, icons = TRUE)

    output <- pdf_create_jacket(output = output, front = front, back = back,
                                spine = spine, inner = inner)

    if (xmpdf::supports_set_xmp()) {
        xmpdf::set_xmp(xmp, output)
    }
    if (xmpdf::supports_set_docinfo()) {
        xmpdf::set_docinfo(xmpdf::as_docinfo(xmp), output)
    }
    invisible(output)
}

#' @rdname sbgj_looney
#' @export
sbgj_nomids <- function(output = NULL) {
    stopifnot(piecepackr::has_font("Carlito"))

    dir <- tools::R_user_dir("sbgjackets", "data")
    if (!dir.exists(dir))
        dir.create(dir, recursive = TRUE)

    rules <- normalizePath(file.path(dir, "Nomids_Rules6.pdf"), mustWork = FALSE)
    if (!file.exists(rules))
        download.file("https://www.looneylabs.com/sites/default/files/literature/Nomids_Rules6.pdf",
                      rules)
    cover <- normalizePath(file.path(dir, "NomidsBoxFront.jpg"), mustWork = FALSE)
    if (!file.exists(cover)) {
        download.file("https://www.looneylabs.com/sites/default/files/marketing_images/NomidsBoxFront.jpg",
                      cover)
    }

    zoom <- normalizePath(file.path(dir, "PyramidArcadeZoomBackground.jpg"), mustWork = FALSE)
    if (!file.exists(zoom)) {
        download.file("https://www.looneylabs.com/sites/default/files/marketing_images/PyramidArcadeZoomBackground.jpg",
        zoom)
    }

    logo <- normalizePath(file.path(dir, "NomidsLogo_Background.jpg"), mustWork = FALSE)
    if (!file.exists(logo)) {
        download.file("https://www.looneylabs.com/sites/default/files/marketing_images/NomidsLogo_Background.jpg",
        logo)
    }

    background_col <- "#EFE8D5FF"
    text_col <- "#1C3160FF"

    bm_cover <- magick::image_read(cover) |> as_bm_pixmap() |> rasterGrob()

    bm_rules <- pnpmisc::pdf_render_bm_pixmap(rules, page = 1L)
    bm_rules <- bm_rules[, 1:(ncol(bm_rules) / 2)] |>
        bm_replace(background_col) |>
        rasterGrob(y = 0.86, just = "top")

    bm_zoom <- magick::image_read(zoom) |> as_bm_pixmap() |>
        rasterGrob(y = 0, just = "bottom")

    bm_logo <- magick::image_read(logo) |> as_bm_pixmap() |>
        rasterGrob(y = 1, just = "top", height = unit(0.15, "npc"))

    front <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), bm_cover)
    back <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)), bm_logo, bm_rules, bm_zoom)
    spine <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)),
                   spineTextGrob("Nomids", col = text_col),
                   spineIconGrob(2:10, 10, 1.0, text_col))

    xmp <- xmpdf::xmp(creator = "Trevor L. Davis",
                      title = "Nomids Small Box Game Jacket")
    credits <- c("* From https://www.looneylabs.com/resources/game/Nomids",
                 "",
                 "  + *Nomids Logo with Background*",
                 "  + *Nomids Box Front*",
                 "  + *Nomids Rules* (cropped, background tweaked)",
                 "  + *Looney Pyramids - Zoom*",
                 "  + faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774",
                 "",
                 "    > If you only plan to make a single copy, or a few to gift to friends, then you can legally use our images without breaking copyright law.")
    inner <- creditsGrob(xmp, credits, icons = TRUE)

    output <- pdf_create_jacket(output = output, front = front, back = back,
                                spine = spine, inner = inner)

    if (xmpdf::supports_set_xmp()) {
        xmpdf::set_xmp(xmp, output)
    }
    if (xmpdf::supports_set_docinfo()) {
        xmpdf::set_docinfo(xmpdf::as_docinfo(xmp), output)
    }
    invisible(output)
}
