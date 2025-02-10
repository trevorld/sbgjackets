#' Create SBG Jacket for Dominoes
#'
#' `sbgj_dominoes_double6()`, `sbgj_dominoes_double9()`, and `sbgj_dominoes_double12()` creates a small box game jacket for Double-Six Dominoes, Double-Nine Dominoes, and Double-Twelve Dominoes.
#' `sbgj_dominoes_all()` creates all of those into a single pdf file.
#'
#' @param output Output file name.  Defaults to `tempfile(fileext = ".pdf")`.
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @rdname sbgj_dominoes
#' @export
sbgj_dominoes_all <- function(output = NULL) {
    stopifnot(piecepackr::has_font("Carlito"),
              requireNamespace("ppdf"),
              requireNamespace("xmpdf"),
              xmpdf::supports_set_bookmarks(),
              xmpdf::supports_set_docinfo(),
              xmpdf::supports_set_xmp())

    output <- pnpmisc:::normalize_output(output)

    bm <- data.frame(title = c("Double-Six Dominoes",
                               "Double-Nine Dominoes",
                               "Double-Twelve Dominoes"),
                     page = c(1L, 3L, 5L))

    xmp <- xmpdf::xmp(creator = "Trevor L. Davis",
                      date_created = "2025",
                      spdx_id = "CC-BY-4.0",
                      title = "Dominoes Small Box Game Jackets")

    d6d <- sbgj_dominoes_double6()
    d9d <- sbgj_dominoes_double9()
    d12d <- sbgj_dominoes_double12()
    output_c <- tempfile(fileext = ".pdf")

    qpdf::pdf_combine(c(d6d, d9d, d12d), output_c) |>
        pnpmisc::pdf_set_bookmarks(bookmarks = bm) |>
        pnpmisc::pdf_set_xmp(xmp = xmp) |>
        pnpmisc::pdf_set_docinfo(docinfo = xmpdf::as_docinfo(xmp)) |>
        pnpmisc::pdf_compress(output, linearize = TRUE)

    pnpmisc::rm_temp_pdfs()

    invisible(output)
}

#' @rdname sbgj_dominoes
#' @export
sbgj_dominoes_double6 <- function(output = NULL) {
    stopifnot(piecepackr::has_font("Carlito"),
              requireNamespace("ppdf"))

    envir <- piecepackr::game_systems(round = TRUE)
    df <- ppdf::domino_tiles() |> mutate(x = 0.4 +  0.4 * .data$x, y = 0.4 * .data$y)
    dominoes <- piecepackr::pmap_piece(df, piecepackr::pieceGrob,
                                       scale = 0.95 * 0.4, envir = envir,
                                       draw = FALSE, default.units = "in")
    dir <- tools::R_user_dir("sbgjackets", "data")
    if (!dir.exists(dir))
        dir.create(dir, recursive = TRUE)
    pic <- normalizePath(file.path(dir, "Rozrywki_Naukowe_Fig._049.jpg"), mustWork = FALSE)
    if (!file.exists(pic))
        download.file("https://upload.wikimedia.org/wikipedia/commons/b/bb/Rozrywki_Naukowe_Fig._049.jpg",
        pic)
    bm_pic <- magick::image_read(pic) |> as_bm_pixmap() |> rasterGrob(height = 1)

    front <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_pic)))

    back <- dominoes
    spine <- gList(rectGrob(gp = gpar(col = NA, fill = "black")),
                   spineTextGrob("Double-Six Dominoes"))
    xmp <- xmpdf::xmp(creator = "Trevor L. Davis", date_created = "2025",
                      spdx_id = "CC-BY-4.0", 
                      title = "Double-Six Dominoes Small Box Game Jacket")

    credits <- c("* *ilustracja z ksi\u0105\u017cki* (illustration with prints) by Louis Poyet",
                 "",
                 "  + https://commons.wikimedia.org/wiki/File:Rozrywki_Naukowe_Fig._049.jpg",
                 "  + Public Domain",
                 "  + Cropped to fit front cover")
    inner <- creditsGrob(xmp, credits, icons = FALSE)

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

#' @rdname sbgj_dominoes
#' @export
sbgj_dominoes_double9 <- function(output = NULL) {
    stopifnot(piecepackr::has_font("Carlito"),
              requireNamespace("ppdf"))

    envir <- piecepackr::game_systems(round = TRUE)
    df <- ppdf::domino_tiles(n=10) |>
        mutate(alpha = ifelse(.data$suit >= 8L, 1, 0.3)) |>
        # filter(.data$suit >= 8L) |>
        mutate(x = 0.3 + 0.3 * .data$x, y = -0.1 + 0.3 * .data$y)
    dominoes <- piecepackr::pmap_piece(df, piecepackr::pieceGrob,
                                       scale = 0.95 * 0.3, envir = envir,
                                       draw = FALSE, default.units = "in")

    dir <- tools::R_user_dir("sbgjackets", "data")
    if (!dir.exists(dir))
        dir.create(dir, recursive = TRUE)
    # pic <- normalizePath(file.path(dir, "Mexican_Train.jpg"), mustWork = FALSE)
    # if (!file.exists(pic))
    #     download.file("https://upload.wikimedia.org/wikipedia/commons/3/38/Mexican_Train.jpg",
    #     pic)
    # pic <- normalizePath(file.path(dir, "Chicken_Foot_domino_game.jpg"), mustWork = FALSE)
    # if (!file.exists(pic))
    #     download.file("https://upload.wikimedia.org/wikipedia/commons/2/22/Chicken_Foot_domino_game.jpg",
    #     pic)
    # bm_pic <- magick::image_read(pic) |>
    #     as_bm_pixmap() |>
    #     bm_rotate(-90) |>
    #     bm_flip("h") |>
    #     rasterGrob(width = 1)
    pic <- normalizePath(file.path(dir, "optical_illusion_dominoes.jpg"), mustWork = FALSE)
    if (!file.exists(pic))
        download.file("https://media.getty.edu/iiif/image/aec737a6-9166-4a04-a684-b03cbe242173/full/1024,/0/default.jpg?download=aec737a6-9166-4a04-a684-b03cbe242173_1024.jpg&size=medium",
        pic)
    bm_pic <- magick::image_read(pic) |>
        as_bm_pixmap() |>
        bm_trim(right = 80L, top = 50L, bottom = 85L) |>
        rasterGrob(height = 1)

    front <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_pic)))
    back <- dominoes
    spine <- gList(rectGrob(gp = gpar(col = NA, fill = "black")),
                   spineTextGrob("Double-Nine Dominoes"))
    xmp <- xmpdf::xmp(creator = "Trevor L. Davis", date_created = "2025",
                      spdx_id = "CC-BY-4.0", 
                      title = "Double-Nine Dominoes Small Box Game Jacket")
    credits <- c("* *Optical illusion of some dominoes.* by Unknown artist",
                 "",
                 "  + https://www.getty.edu/art/collection/object/107CZC",
                 "  + Public Domain",
                 "  + Cropped to fit front cover")
    #            "* *A game of Chicken Foot.* by yoppy",
    #            "",
    #            "  + https://commons.wikimedia.org/wiki/File:Chicken_Foot_domino_game.jpg",
    #            "  + Creative Commons Attribution 2.0 Generic License",
    #            "  + Rotated 90 degrees, flipped horizontally, and cropped")
    #            "* *A typical Mexican Train layout using double-nine dominoes*",
    #            "   by Liko81",
    #            "  + https://commons.wikimedia.org/wiki/File:Mexican_Train.jpg",
    #            "  + Creative Commons Attribution 3.0 Unported License",
    #            "  + Cropped to fit front cover")
    inner <- creditsGrob(xmp, credits, icons = FALSE)

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

#' @rdname sbgj_dominoes
#' @export
sbgj_dominoes_double12 <- function(output = NULL) {
    stopifnot(piecepackr::has_font("Carlito"),
              requireNamespace("ppdf"))

    envir <- piecepackr::game_systems(round = TRUE)
    df <- ppdf::domino_tiles(n=13) |>
        mutate(alpha = ifelse(.data$suit >= 11L, 1, 0.3)) |>
        # filter(.data$suit >= 8L) |>
        mutate(x = 0.4 + 0.23 * .data$x, y = -0.05 + 0.23 * .data$y)
    dominoes <- piecepackr::pmap_piece(df, piecepackr::pieceGrob,
                                       scale = 0.94 * 0.23, envir = envir,
                                       draw = FALSE, default.units = "in")

    dir <- tools::R_user_dir("sbgjackets", "data")
    if (!dir.exists(dir))
        dir.create(dir, recursive = TRUE)
    # pic <- normalizePath(file.path(dir, "pic1424282.jpg"), mustWork = FALSE)
    # if (!file.exists(pic))
    #     abort(c(str_glue("{shQuote(pic)} not found"),
    #             i = c("Download from <https://boardgamegeek.com/image/1424282/mexican-train>")))
    # pic <- normalizePath(file.path(dir, "People_playing_Mexican_Train.jpg"), mustWork = FALSE)
    # if (!file.exists(pic))
    #     download.file("https://upload.wikimedia.org/wikipedia/commons/thumb/c/ca/People_playing_Mexican_Train.jpg/1280px-People_playing_Mexican_Train.jpg",
    #     pic)
    pic <- normalizePath(file.path(dir, "kittens_playing_dominoes.jpg"), mustWork = FALSE)
    if (!file.exists(pic))
        download.file("https://tile.loc.gov/storage-services/service/pnp/cph/3b50000/3b50000/3b50700/3b50738r.jpg",
        pic)

    bm_pic <- magick::image_read(pic) |>
        as_bm_pixmap() |>
        bm_trim(right = 90L, top = 20L, bottom = 60L) |>
        rasterGrob(height = 1)
    front <- rectGrob(gp = gpar(col = NA, fill = pattern(bm_pic)))
    back <- dominoes
    spine <- gList(rectGrob(gp = gpar(col = NA, fill = "black")),
                   spineTextGrob("Double-Twelve Dominoes"))
    xmp <- xmpdf::xmp(creator = "Trevor L. Davis", date_created = "2025",
                      spdx_id = "CC-BY-4.0", 
                      title = "Double-Twelve Dominoes Small Box Game Jacket")
    credits <- c("* *My Little White Kittens: Playing Dominoes* published by Currier & Ives",
                 "",
                 "  + https://www.loc.gov/resource/cph.3b50738/?st=image",
                 "  + Public Domain",
                 "  + Cropped to fit front cover")
    #              "* *Mexican Train* by Bev Sykes",
    #              "",
    #              "  + https://flickr.com/photos/basykes/3116447054",
    #              "  + Creative Commons Attribution 2.0 Generic License",
    #              "  + Cropped to fit front cover")
    #              "",
    #              "* *Playing Mexican Train in the backyard after a Lobster Feast - The game is on.*",
    #              "   by Matt Robertson",
    #              "  + https://boardgamegeek.com/image/1424282/mexican-train",
    #              "  + Creative Commons Attribution-ShareAlike 3.0 Unported License",
    #              "  + Cropped to fit front cover")
    inner <- creditsGrob(xmp, credits, icons = FALSE)

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
