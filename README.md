# sbgjackets

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/sbgjackets)](https://cran.r-project.org/package=sbgjackets)
[![R-CMD-check](https://github.com/trevorld/sbgjackets/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/trevorld/sbgjackets/actions)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)
* [Pre-made 4x6 Photo Box Jackets](#premade)
* [Pre-made Playing Card Box Jackets](#premade_card)
* [Differences with Boardgame Barrio's Small Box Game Jackets](#barrio)
* [Related links](#related)

## <a name="overview">Overview</a>

`{sbgjackets}` is an R package to help make print-and-play small box game (SBG) jackets to use in 4x6 photo storage boxes and/or poker deck storage boxes.

## <a name="installation">Installation</a>

You can install the development version using my [R-universe](https://ropensci.org/r-universe/) universe:


``` r
install.packages(
	'sbgjackets',
	repos = c(
		'https://trevorld.r-universe.dev',
		'https://piecepackr.r-universe.dev',
		'https://cloud.r-project.org'
	)
)
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
library("pnpmisc")
library("sbgjackets")
library("xmpdf")
check_sbgjackets_dependencies()

front <- fullGrob("#D55E00")
back <- fullGrob("#009E73")
spine <- gList(fullGrob("black"),
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
pdf_create_jacket(
	output,
	front = front,
	back = back,
	spine = spine,
	inner = inner
) |>
	pdf_polish_jacket(xmp = xmp, instructions = TRUE)
```

## <a name="premade">Pre-made 4x6 Photo Box Jackets</a>

This package also provides functions that locally create SBG jackets pdfs for a few select games (and game systems and generic components).  Pre-made versions for several of these are available for download at <https://trevorldavis.com/piecepackr/pages/print-and-play-pdfs.html>.



|Game (System/Components) |Function                   |License           |
|:------------------------|:--------------------------|:-----------------|
|Dice                     |`sbgj_dice()`              |[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)|
|Polyhedral Dice          |`sbgj_polyhedral_dice()`   |[CC-BY-ND-4.0](https://creativecommons.org/licenses/by-nd/4.0/)|
|[Double-Six Dominoes](https://www.pagat.com/domino/)|`sbgj_dominoes_double6()`  |[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)|
|Double-Nine Dominoes     |`sbgj_dominoes_double9()`  |[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)|
|Double-Twelve Dominoes   |`sbgj_dominoes_double12()` |[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)|
|[Homeworlds](https://www.looneylabs.com/games/homeworlds)|`sbgj_homeworlds()`        |Personal Use Only |
|[Ice Duo](https://www.looneylabs.com/games/ice-duo)|`sbgj_ice_duo()`           |Personal Use Only |
|[Jinxx](https://www.looneylabs.com/games/jinxx)|`sbgj_jinxx()`             |Personal Use Only |
|[Looney Pyramids](https://www.looneylabs.com/pyramids-home)|`sbgj_looney_pyramids()`   |Personal Use Only |
|Marbles                  |`sbgj_marbles()`           |[CC-BY-SA-3.0](https://creativecommons.org/licenses/by-sa/3.0/)|
|[Martian Chess](https://www.looneylabs.com/martian_chess)|`sbgj_martian_chess()`     |Personal Use Only |
|[Nomids](https://www.looneylabs.com/nomids)|`sbgj_nomids()`            |Personal Use Only |
|Pawns                    |`sbgj_pawns()`             |[CC-BY-ND-4.0](https://creativecommons.org/licenses/by-nd/4.0/)|
|Reversible Discs         |`sbgj_reversible_discs()`  |[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)|
|[Dungeon Delver](https://boardgamegeek.com/boardgame/113324/dungeon-delver)|`sbgj_dungeon_delver()`    |Personal Use Only |
|[nestortiles](https://boardgamegeek.com/boardgame/74615/nestortiles)|`sbgj_nestortiles()`       |Personal Use Only |
|[Shibumi](https://cambolbro.com/games/shibumi/)|`sbgj_shibumi()`           |Personal Use Only |
|Black Stones             |`sbgj_black_stones()`      |[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)|
|Glass Stones             |`sbgj_glass_stones()`      |[CC-BY-ND-4.0](https://creativecommons.org/licenses/by-nd/4.0/)|
|White Stones             |`sbgj_white_stones()`      |[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)|
|[Yinsh](https://www.gipf.com/yinsh/)|`sbgj_yinsh()`             |Personal Use Only |



## <a name="premade_card">Pre-made Playing Card Box Jackets</a>

This package also provides functions that locally create playing-card box jackets pdfs for a few select games (and game systems and generic components).  Pre-made versions for several of these are available for download at <https://trevorldavis.com/piecepackr/pages/print-and-play-pdfs.html>.



|Game (System/Components)         |Function                   |License           |
|:--------------------------------|:--------------------------|:-----------------|
|[Bridge](https://www.pagat.com/auctionwhist/bridge.html)|`pcbj_bridge()`            |[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)|
|[Decktet](https://decktet.com)   |`pcbj_decktet()`           |[CC-BY-NC-SA-4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)|
|[Everdeck](https://thewrongtools.wordpress.com/2019/10/10/the-everdeck/)|`pcbj_everdeck()`          |[CC-BY-SA-3.0](https://creativecommons.org/licenses/by-sa/3.0/)|
|[The Fox in the Forest](https://foxtrotgames.com/forest/)|`pcbj_fox_in_the_forest()` |Personal Use Only |
|[French Suits: English Pattern](https://i-p-c-s.org/pattern/ps-48.html)|`pcbj_english_pattern()`   |[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)|
|[German Suits: Bavarian Pattern](https://i-p-c-s.org/pattern/ps-55.html)|`pcbj_bavarian_pattern()`  |[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)|
|[Mahjong](https://www.sloperama.com/mahjongg/)|`pcbj_mahjong()`           |[CC-BY-ND-4.0](https://creativecommons.org/licenses/by-nd/4.0/)|
|[Pinochle](https://www.pagat.com/marriage/pinmain.html)|`pcbj_pinochle()`          |[CC-BY-SA-3.0](https://creativecommons.org/licenses/by-sa/3.0/)|
|[Poker](https://www.pagat.com/poker)|`pcbj_poker()`             |[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)|
|[Spanish Suits: Castilian Pattern](https://i-p-c-s.org/pattern/ps-27.html)|`pcbj_castilian_pattern()` |[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)|
|[Wizard](https://www.usgamesinc.com/original-wizard-r-card-game.html)|`pcbj_wizard()`            |Personal Use Only |



## <a name="barrio">Differences with Boardgame Barrio's Small Box Game Jackets</a>

[Boardgame Barrio](https://sites.google.com/view/boardgamebarrio) has created over a thousand small box game jackets for 4x6 photo storage boxes.  Here are some of the high-level differences from the jackets produced by `{sbgjackets}` and Boardgame Barrio's jackets:

| Feature | Boardgame Barrio | `{sbgjackets}` |
| --- | --- | --- |
| Quantity | Over 1,600 pre-made jackets | 32 pre-made jackets |
| Size | Targets only 4x6 photo storage boxes | Targets 4x6 photo storage boxes as well as playing card boxes |
| Process | Made in Microsoft Publisher with a template | Made in `R` with `pnpmisc::pdf_create_jacket()` |
| Spine Font | Calibri (proprietary) | Carlito (libre font, metrically compatible with Calibri) |
| Spine Text Color | Spine text is always white | Spine text may be other colors (e.g. to match cover text color) |
| Spine Icons | Proprietary icons whose style and backround color can each vary between three levels (i.e. green, orange, or red) | Libre icons from game-icons.net that don't vary in style and aren't color-coded (but the icons are chosen to not be too dissimilar from Boardgame Barrio's icons) |
| License | Always "For Personal Use Only" | Sometimes "For Personal Use Only" but sometimes available under various Creative Commons licenses when the image licenses allow |
| Credits | | List of credits to go on inside cover |

## <a name="related">Related links</a>

Other Small Box Game Jackets:

* [Boardgame Barrio's Small Box Game Jackets](https://sites.google.com/view/boardgamebarrio)

Origami to organize a 4"x6" photo storage box:

* [Origami Masu Box](https://www.paperkawaii.com/origami-photo-tutorials/masu-box/) and [Origami Masu Box Divider](https://www.paperkawaii.com/origami-photo-tutorials/masu-box-divider/) (6"x6" paper)
* [Origami Baggi Box](http://www.origami-instructions.com/origami-baggi-box.html) (4"x4" paper)


3D printable models to organize a 4"x6" photo storage box:

<!---* [Photo Storage Board Game Insert Collection](https://www.reddit.com/r/boardgames/comments/vs6b6q/photo_storage_board_game_insert_collection/)--->

* [Canadian Gamer's Card Game Organizers](https://makerworld.com/en/models/752771-card-game-organizers-for-4x6-photo-craft-caddy#profileId-686706) (Standard Digital File License)
* [endofturn's 4x6 Photo Case Organizer](https://www.printables.com/model/108936-4x6-photo-case-organizer/files) (CC BY-NC 4.0)
* [Gut Shot Games's Photo Caddy Board Game Insert Collection](https://www.printables.com/model/278336-photo-caddy-board-game-insert-collection) (CC BY-NC 4.0)
* [LinearRelation's Modular 4x6 Photo Case Inserts](https://www.printables.com/model/479844-modular-4x6-photo-case-inserts/files) (CC-BY 4.0)
* [passif's Photo Case Inserts](https://www.thingiverse.com/thing:5144296)
* [somedave's Ultimate Modular Photo Keeper Game Storage](https://makerworld.com/en/models/98197-ultimate-modular-photo-keeper-game-storage#profileId-133787)

Some compatible 4"x6" photo storage box brands:

* [IRIS USA](https://www.irisusainc.com/collections/photo-storage-boxes/products/photo-keeper-4-inchx6-inch-12-case)
* Novelinks
* Simply Tidy

Some likely compatible playing card box brands:

* Apacali
* Queekay
* Vicenpal
* Whyogeta

Various guides:

* [Guide to Organizing Small Box Games](https://web.archive.org/web/20220711005902/https://www.kenkuhn.me/l/guide-to-organizing-small-box-games/)

Some games that may fit within a 4"x6" photo storage box:

* https://boardgamegeek.com/geeklist/259450/games-that-fit-in-photo-storage-box
* https://sites.google.com/view/boardgamebarrio/jacket-catalogue

Artistic assets used by `{sbgjackets}`:

* [Carlito font](https://fonts.google.com/specimen/Carlito)
* [Game-icons.net](https://game-icons.net)
