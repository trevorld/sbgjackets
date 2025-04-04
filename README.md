# sbgjackets

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/sbgjackets)](https://cran.r-project.org/package=sbgjackets)
[![R-CMD-check](https://github.com/trevorld/sbgjackets/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/trevorld/sbgjackets/actions)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)
* [Pre-made Jackets](#premade)
* [Differences with Boardgame Barrio's Small Box Game Jackets](#barrio)
* [Related links](#related)

## <a name="overview">Overview</a>

`{sbgjackets}` is an R package to help make print-and-play small box game (SBG) jackets to use in 4x6 photo boxes.

## <a name="installation">Installation</a>

You can install the development version using my [R-universe](https://ropensci.org/r-universe/) universe:


``` r
install.packages('sbgjackets', repos = c('https://trevorld.r-universe.dev', 'https://cloud.r-project.org'))
```

or by using the `{remotes}` package:


``` r
remotes::install_github("trevorld/sbgjackets")
```

## <a name="examples">Examples</a>

This package provides the following grobs to help create small box game (SBG) jackets with `pnpmisc::pdf_create_jacket()`:

  + `creditsGrob()` (to provide credits that go on inside of jacket)
  + `spineIconGrob()` (to provide consistent game info icons on the spine)
  + `spineTextGrob()` (to provide consistent title text on the spine)


``` r
library("grid")
library("sbgjackets")
library("xmpdf")
sbgjackets:::assert_runtime_dependencies()

front <- rectGrob(gp = gpar(col = NA, fill = "#D55E00"))
back <- rectGrob(gp = gpar(col = NA, fill = "#009E73"))
spine <- gList(rectGrob(gp = gpar(col = NA, fill = "black")),
               spineTextGrob("Example Spine"),
               spineIconGrob(2:4, 30, 1.5))

xmp <- xmp(creator = "Trevor L. Davis",
           date_created = "2025",
           spdx_id = "CC-BY-4.0",
           title = "Example Small Box Game Jacket")
credits <- c("* A list of extra credits written in *CommonMark* markdown rendered by `{{marquee}}`",
             "",
             "  + https://commonmark.org/",
             "  + https://github.com/r-lib/marquee")
inner <- creditsGrob(xmp, credits, icons = TRUE)

output <- "example_jacket.pdf"
pnpmisc::pdf_create_jacket(output,
                           front = front, back = back,
                           spine = spine, inner = inner)
set_xmp(xmp, output)
set_docinfo(as_docinfo(xmp), output)
```

## <a name="premade">Pre-made Jackets</a>

This package also provides functions that locally create SBG jackets pdfs for a few select games and game systems.  Pre-made versions for several of these are available for download at <https://trevorldavis.com/piecepackr/pages/print-and-play-pdfs.html>.

| Game (System) | Function | Jacket License |
| --- | --- | --- |
| Double-Six Dominoes | `sbgj_dominoes_double6()` | CC BY 4.0 |
| Double-Nine Dominoes | `sbgj_dominoes_double9()` | CC BY 4.0 |
| Double-Twelve Dominoes | `sbgj_dominoes_double12()` | CC BY 4.0 |
| [Looney Pyramids](https://www.looneylabs.com/pyramids-home) | `sbgj_looney_pyramids()` | Personal Use Only |
| [Homeworlds](https://www.looneylabs.com/games/homeworlds) | `sbgj_homeworlds()` | Personal Use Only |
| [Ice Duo](https://www.looneylabs.com/games/ice-duo) | `sbgj_ice_duo()` | Personal Use Only |
| [Jinxx](https://www.looneylabs.com/games/jinxx) | `sbgj_jinxx()` | Personal Use Only |
| [Martian Chess](https://www.looneylabs.com/games/martian-chess) | `sbgj_martian_chess()` | Personal Use Only |
| [Nomids](https://www.looneylabs.com/games/nomids) | `sbgj_nomids()` | Personal Use Only |
| [Shibumi](https://cambolbro.com/games/shibumi/) | `sbgj_shibumi()` | Personal Use Only |

This package also provides functions that locally create SBG jackets pdfs for a few select generic components:

| Component | Function | Jacket License |
| --- | --- | --- |
| Dice | `sbgj_dice()` | CC BY 4.0 |
| Pawns | `sbgj_pawns()` | CC BY ND 4.0 |
| Glass Stones | `sbgj_glass_stones()` | CC BY ND 4.0 |
| Polyhedral Dice | `sbgj_polyhedral_dice()` | CC BY ND 4.0 |


## <a name="barrio">Differences with Boardgame Barrio's Small Box Game Jackets</a>

[Boardgame Barrio](https://sites.google.com/view/boardgamebarrio) has created over a thousand small box game jackets for 4x6 photo storage boxes.  Here are some of the high-level differences from the jackets produced by `{sbgjackets}` and Boardgame Barrio's jackets:

| Feature | Boardgame Barrio | `{sbgjackets}` |
| --- | --- | --- |
| Quantity | Over 1,400 pre-made jackets | 14 pre-made jackets |
| Process | Made in Microsoft Publisher with a template | Made in `R` with `pnpmisc::pdf_create_jacket()` |
| Spine Font | Calibri (proprietary) | Carlito (libre font, metrically compatible with Calibri) |
| Spine Text Color | Spine text is always white | Spine text may be other colors (e.g. to match cover text color) |
| Spine Icons | Proprietary icons whose style and backround color can each vary between three levels (i.e. green, orange, or red) | Libre icons from game-icons.net that don't vary in style and aren't color-coded (but the icons are chosen to not be too dissimilar from Boardgame Barrio's icons) |
| License | Always "For Personal Use Only" | Sometimes "For Personal Use Only" but sometimes available under various Creative Commons licenses when the image licenses allow |
| Credits | | List of credits to go on inside cover |

## <a name="related">Related links</a>

* [Boardgame Barrio's Small Box Game Jackets](https://sites.google.com/view/boardgamebarrio)
* [Carlito font](https://fonts.google.com/specimen/Carlito)
* [Game-icons.net](https://game-icons.net)
* [Origami Masu Box](https://www.paperkawaii.com/origami-photo-tutorials/masu-box/) and [Origami Masu Box Divider](https://www.paperkawaii.com/origami-photo-tutorials/masu-box-divider/) (6"x6" paper)
* [Origami Baggi Box](http://www.origami-instructions.com/origami-baggi-box.html) (4"x4" paper)
* [Photo Storage Board Game Insert Collection](https://www.reddit.com/r/boardgames/comments/vs6b6q/photo_storage_board_game_insert_collection/)
* [Guide to Organizing Small Box Games](https://web.archive.org/web/20220711005902/https://www.kenkuhn.me/l/guide-to-organizing-small-box-games/)
