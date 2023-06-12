X <- sample(10, 200, TRUE) |> rlang::set_names()
Y <- sample(30, 200, TRUE) |> rlang::set_names()
(index <- rep.int(NA, length(X)));

Z <- purrr::map(unique(X) |> rlang::set_names(), \(i){
		# out.x <- X[which(X %in% i)];
		out.y <- Y[which(X %in% i)];

		index[which(X %in% i)] <<- data.table::frank(Y[which(X %in% i)], ties.method = "dense")
		# print(out.y)
	})

View(Z)

print(index)

data.table(
	index
	, X
	, x.rank = data.table::frank(X, ties.method = "dense")
	, Y
	)[, y.rank := data.table::frank(Y[x.rank], ties.method = "dense")] |>
	View()

Y[data.table::frank(X, ties.method = "dense")] |> unname()

Z2 <- outer(X, unique(X) |> rlang::set_names(), \(x, y) x == y) |>
	print() |>
	which(arr.ind = TRUE) |>
	as.data.table() |>
	(\(x) x[, list(list(unname(Y)[row])), by = col])()

View(Z2)

