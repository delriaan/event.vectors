library(book.of.workflow);
do.load_unloaded("book.of.features, foreach, doFuture, igraph, matrixStats, plotly, purrr, magrittr");

registerDoFuture();
plan(sequential);
plan(tweak(multisession, workers = 5));

test.evs$space |> discard(is.list) |> unique() |> setorder(jk, from.coord) |> View("test.evs$space");

test.evs$space[
	(mSt >= 0) & (jk == 8) & (from.src %like% "Src3[:]15")# & (to.src %like% "Src[24][:](17|35)$")
	, c(.SD[, .(mGap, mSt, epsilon, epsilon.desc, from.coord, to.coord, from.len, to.len)]
			, map(epsilon, ~list(x = Re(.x), iy = Im(.x))) |> rbindlist()
			)
	, keyby = src.pair
	][c(1,2), print(.SD[order(from.coord, to.coord)])]  %$%
	pracma::dot(x = c(x[1], iy[1]), y = c(x[2], iy[2]))

