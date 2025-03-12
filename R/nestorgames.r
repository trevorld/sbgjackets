#' Create SBG Jacket for nestorgames products
#'
#' `sbgj_shibumi()` creates a small box game jacket for [Shibumi](https://cambolbro.com/games/shibumi/).
#' `sbgj_nestorgames_all()` creates all of those into a single pdf file.
#'
#' Note that these print-and-play small box game jackets are for **Personal Use Only**.
#' @inheritParams sbgj_dominoes_all
#' @inheritParams pnpmisc::pdf_create_jacket
#' @return The output file name invisibly.  As a side effect creates a pdf file.
#' @rdname sbgj_nestorgames
#' @export
sbgj_nestorgames_all <- function(output = NULL, ...,
                                 paper = c("letter", "a4"),
                                 instructions = TRUE) {
    check_dots_empty()
    assert_runtime_dependencies()

    paper <- tolower(paper)
    paper <- match.arg(paper)
    output <- pnpmisc:::normalize_output(output)

    bm <- data.frame(title = c("Shibumi"),
                     page = seq.int(1L, by = 2L, length.out = 1L))
    if (isTRUE(instructions)) {
        bmi <- data.frame(title = "Instructions", page = 1L)
        bm$page <- bm$page + 2L
        bm <- rbind(bmi, bm)
    }

    xmp <- xmp(creator = "Trevor L. Davis",
               title = "nestorgames Small Box Game Jackets")

    sh <- sbgj_shibumi(paper = paper)
    output_c <- tempfile(fileext = ".pdf")
    qpdf::pdf_combine(c(sh), output_c)
    if (instructions)
        prepend_instructions(output_c, paper = paper)
    output_c |>
        pnpmisc::pdf_set_bookmarks(bookmarks = bm) |>
        pnpmisc::pdf_set_xmp(xmp = xmp) |>
        pnpmisc::pdf_set_docinfo(docinfo = as_docinfo(xmp)) |>
        pnpmisc::pdf_compress(output, linearize = TRUE)

    pnpmisc::rm_temp_pdfs()

    invisible(output)
}

#' @rdname sbgj_nestorgames
#' @export
sbgj_shibumi <- function(output = NULL, ...,
                                 paper = c("letter", "a4"),
                                 instructions = FALSE) {
    check_dots_empty()
    assert_runtime_dependencies()

    paper <- tolower(paper)
    paper <- match.arg(paper)
    output <- pnpmisc:::normalize_output(output)

    dir <- tools::R_user_dir("sbgjackets", "data")
    if (!dir.exists(dir))
        dir.create(dir, recursive = TRUE)

    background_col <- "white"
    text_col <- "white"

    game <- ppn::read_ppn(system.file("ppn/spoff.ppn", package = "ppn"))[[1L]]
    df_front <- game$dfs[["Setup...."]]

    df_back <- data.frame(piece_side = c("board_face", rep_len("bit_back", 16 * 3)),
                          x = c(2, rep(1:4, 4L), rep(5:8, 8L)),
                          y = c(2, rep(5:8, each = 4L), rep(8:1, each = 4L)),
                          suit = c(4L, rep(1L, 16L), rep(2L, 16L), rep(6L, 16L)),
                          rank = c(4L, rep(9L, 3 * 16)),
                          scale = c(1, rep(0.95, 3 * 16)),
                          cfg = "marbles")



    envir <- piecepackr::game_systems(round = TRUE, shading = TRUE)
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev), add = TRUE)

    l_front <- piecepackr::render_piece(df_front, dev = ragg::agg_capture,
                                        image = "nativeRaster",
                                        envir = envir, op_scale = 0.5,
                                        trans = piecepackr::marbles_transform)
    rg_front <- rasterGrob(l_front$image)
    l_back <- piecepackr::render_piece(df_back, dev = ragg::agg_capture,
                                       image = "nativeRaster",
                                       envir = envir, op_scale = 0.0,
                                       trans = piecepackr::marbles_transform)
    rg_back <- rasterGrob(l_back$image)

    front <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)),
                   rg_front)
    back <- gList(rectGrob(gp = gpar(col = NA, fill = background_col)),
                   rg_back)
    spine <- gList(rectGrob(gp = gpar(col = NA, fill = "black")),
                   spineTextGrob("Shibumi", col = text_col))

    xmp <- xmp(creator = "Trevor L. Davis",
               title = "Shibumi Small Box Game Jacket")
    credits <- c("* The shibumi game system was invented by Cameron Browne",
                 "", "  * https://cambolbro.com/games/shibumi/", "",
                 "* The shibumi game system was published by nestorgames",
                 "", "  * https://nestorgames.com/shibumi", "",
                 "* This Small Box Game Jacket is not affiliated, sponsored, nor endorsed by either Cameron Browne or nestorgames",
                 str_glue("  * The use of a non-stylized {dQuote('Shibumi')} is intended as fair use to indicate that this 4x6 photo case is usable to store a {dQuote('shibumi')} set."))

    inner <- creditsGrob(xmp, credits, icons = FALSE)

    output <- pdf_create_jacket(output = output, front = front, back = back,
                                spine = spine, inner = inner, paper = paper)
    if (instructions)
        prepend_instructions(output, paper = paper)

    set_xmp(xmp, output)
    set_docinfo(as_docinfo(xmp), output)
    invisible(output)
}
