test_that("`pcbj_mahjong()`", {
	skip_if_not(cache_has("mahjong.jpg"))
	skip_if_not(cache_has("mahjong2.jpg"))
	f <- pcbj_mahjong()
	expect_true(file.exists(f))
	unlink(f)
})
