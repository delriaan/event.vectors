library(magrittr);

y <- 0;

set.seed(sample(.Random.seed, 1));

y <- sample(3:20, sample(80:100, 1), TRUE) |>
	c(sample(15:40, sample(10:20, 1), TRUE)) %>%
	sample(length(.)) |>
	cumsum() |>
	magrittr::add(runif(n = length(y), min = 2, max = 10)) |>
	round();

dy <- c(0, diff(y))

dy |>
	(\(i){
		freq_i <- table(i);

		freq_i |>
			unlist() |>
			book.of.utilities::ratio(type = "of.sum", d = 6) |>
			log(2) |>
			magrittr::multiply_by(-1) %>%
			.[order(as.numeric(names(.)))] |>
			as.vector() |>
			book.of.utilities::calc.geo_mean()
	})()
