test_that("Storage jackets", {
	skip_if_not(cache_has("dice.jpg"))
	f <- sbgj_dice()
	expect_true(file.exists(f))
	unlink(f)

	skip_if_not(cache_has("glass_stones.jpg"))
	f <- sbgj_glass_stones()
	expect_true(file.exists(f))
	unlink(f)

	skip_if_not(cache_has("wooden_pawns.jpg"))
	f <- sbgj_pawns()
	expect_true(file.exists(f))
	unlink(f)

	skip_if_not(cache_has("polyhedral_dice.jpg"))
	f <- sbgj_polyhedral_dice()
	expect_true(file.exists(f))
	unlink(f)
})
