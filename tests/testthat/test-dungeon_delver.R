test_that("dungeon delver jacket", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("DungeonDelverBacks.pdf"))
	f <- sbgj_dungeon_delver()
	expect_true(file.exists(f))
	unlink(f)
})
