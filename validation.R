# dir(pattern = "^[a-z]{1}[_].+R$", recursive = TRUE) |> purrr::walk(source)

# ~ Initialization ====
# library(event.vectors);
library(magrittr);
library(future);
library(stringi, include.only = "%s+%")
library(tictoc);
library(purrr)

make.test_data <- function(j = 5, n = 5, m = 5, o = 1:10, dest = globalenv(), .debug = FALSE){
#' Make Test Data for Validation
#'
#' \code{make.test_data} creates several \code{\link[data.table]{data.table}} objects to be used to validate the package functionality
#' @param j (integer | 5L) The number of unique values for 'k' to generate
#' @param n (integer | 5L) The number of sources to create (maximum of 10)
#' @param m (integer | 5L) The maximum number of columns to generate for each created object (maximum of 15): each column represents an attribute not related to time
#' @param o (integer[] | 1:10) A vector of integers sampled to create end dates from a randomly-generated start date
#' @param dest (environment) The destination environment object
#' @param .debug (logical | \code{FALSE}) When \code{TRUE}, additional debugging items are printed
#'
#' @return One to \code{n} \code{\link[data.table]{data.table}} objects prefixed as 'test_data'.
#'
	j = max(c(3, abs(as.integer(j))));
	n = max(c(3, abs(as.integer(n)))); ifelse(n > 10, 10, n);
	m = max(c(3, abs(as.integer(m)))); ifelse(m > 15, 15, m);
	o = { ifelse(rlang::has_length(o, 1L)
						 , yes = 1:10
						 , no = ifelse(any(o <= 0)
						 							, yes = abs(o)
						 							, no = ifelse(rlang::has_length(o, 2L)
						 														, yes = `:`(o[1], o[2]) |> sort()
						 														, no =  o))
						 )}

	sequence(n) %>%
	purrr::set_names(paste0("test_data.", stringi::stri_pad_left(., width = 2, pad = "0")))|>
	purrr::map(~{
		set.seed(sample(.Random.seed, 1));
		.src = LETTERS[[.x]];

		.out = purrr::map_dfr(c(1:j), ~list(join_key = rep.int(.x, sample(10:100, 1, TRUE)), src = .src)) %>% data.table::as.data.table();

		.init_date = c(as.Date(sprintf(
				"%s-%s-%s"
				, rep.int(data.table::year(Sys.Date()), nrow(.out))
				, sample(stringi::stri_pad_left(1:12, width = 2, pad = "0"), nrow(.out), TRUE)
				, sample(stringi::stri_pad_left(1:28, width = 2, pad = "0"), nrow(.out), TRUE)
				)));

		.out[
			, c("date.start", "date.end") := list(.init_date, .init_date + rpois(n = length(join_key), lambda = sample(o, length(join_key), TRUE)))
			][
			, paste0("X_", stringi::stri_pad_left(sample(30, m), width = 2, pad = "0")) := purrr::map(1:m, ~sample(runif(1E6), .N, TRUE))
			][
			runif(length(join_key)) > 0.65
			] %>%
			data.table::setkey(join_key, src, date.start, date.end) %>%
			data.table::setcolorder(c("join_key", "date.start", "date.end", "src"))
		}) %>%
	list2env(envir = dest);
}

BLAH <- new.env();
set.seed(sample(100000, 1));
make.test_data(j = 50, n = 3, m = 5, o = c(5, 20), dest = BLAH, .debug = !TRUE);set.seed(sample(100000, 1));
set.seed(sample(100000, 1));
make.test_data(j = 50, n = 3, m = 5, o = c(5, 20), dest = .GlobalEnv, .debug = !TRUE);
tictoc::tic.clear(); tictoc::tic.clearlog();

plan(sequential);
plan(tweak(multisession, workers = 3));
#
# ~ Validation #1 :: event.vectors ====
tictoc::tic("EVSpace Validation Object");
test.evs <- { event.vectors$
	new()$configure(
		src.defs = c(ls(pattern = "^test.+[0-9]$") |>
								 	purrr::modify_at(3, ~paste0(.x, "[(join_key > 3)]")) |>
								 	purrr::modify_at(1, ~paste0(.x, "[lubridate::month(date.start)==1]")) |>
								 	rlang::parse_exprs()
								 , ("BLAH$" %s+% ls(BLAH, pattern = "^test")[1:3]) |>
								 	purrr::modify_at(3, ~paste0(.x, "[(join_key == 1)]")) |>
								 	purrr::modify_at(2, ~paste0(.x, "[lubridate::month(date.start)==8]")) |>
								 	rlang::parse_exprs()
								 )
		, contexts = rlang::parse_exprs("Event_" %s+% LETTERS[1:6])
		, map.fields = replicate(n = 6, c("join_key", "date.start", "date.end"), simplify = FALSE)
		, chatty = TRUE
		);
}

test.evs$make.evs_universe(
	mSt <= quantile(mSt, 0.75)
	, abs(mGap) >= 5
	, abs(mGap) <= 120
	, graph.control = {
			rlang::exprs(
				igraph::E(g)$title	<- igraph::ends(g, igraph::E(g)) %>% apply(1, paste, collapse = " -> ")
				, igraph::V(g)$color <- igraph::V(g)$name %>% stringi::stri_split_fixed(":", simplify = TRUE) %>% .[, 1L] %>% {
						x = .;
						y = purrr::set_names(unique(x), purrr::map_chr(unique(x), ~rgb(runif(1), runif(1), runif(1))))
						purrr::map_chr(x, ~names(y)[which(y == .x)])
					}
				, igraph::V(g)$src <- igraph::V(g)$name %>% stringi::stri_replace_first_regex("[:][0-9]+", "")
				)
		}
	, unit = "days"
	, chatty = TRUE
	)

tictoc::toc(log = TRUE);

test.evs$space |> View()

test.evs$config |> attributes();
test.evs$.__enclos_env__$private$q_table;
test.evs$.__enclos_env__$private$.params$config
test.evs$space[, .(jk, from.coord, to.coord, src.pair, mSt, mGap, mEd, epsilon = as.character(epsilon))] |>
	summarytools::dfSummary() |>
	summarytools::view(method = "browser");
test.evs$space[(jk == 4)] %>% View("Space: jk == 4");
igraph::vertex.attributes(test.evs$evt_graphs$`1`);
test.evs$evt_graphs$`1` |> igraph::V()
test.evs$evt_graphs$`1` |> igraph::E()
test.evs$evt_graphs$`1` |> igraph::edge.attributes()

# ~ Validation #2 :: visNetwork::visIgraph() ====
f2ab <- list(theta = 0.1, gravitationalConstant = -5000, centralGravity = 0.0,  avoidOverlap = 1, damping = 0.7);

test.evs$evt_graphs$`1` %>% {
		g = .
		igraph::V(g)$trace <- retrace.evs(g, test.evs)
		igraph::V(g)$title <- igraph::V(g)$trace |>
			purrr::map_chr(~{
				x = eval(.x, envir = globalenv()) %>%
					.[, purrr::modify_at(.SD[, !"row.filters"], c("start_idx", "end_idx"), as.character)] %>%
					data.table::melt(measure.vars = names(.), variable.name = "key", variable.factor = FALSE)

				kableExtra::kable(x = x, caption = "Retraced data from source") |>
					kableExtra::kable_styling()
			})
		g
	} %>%
	visNetwork::visIgraph(physics = TRUE, type = "full") %>%
	visNetwork::visPhysics(solver = "forceAtlas2Based", timestep = 0.05) %>%
	visNetwork::visOptions(width = "1600", height = "1024") %>%
	visNetwork::visNodes(opacity = 0.5) %>%
	htmltools::html_print(viewer = browseURL)

# ~ Validation #3 :: continuity ----
obs_data <- data.table::rbindlist(list(test_data.01, test_data.02, test_data.03), use.names = FALSE)[, `:=`(Z_1 = sample(letters, .N, TRUE), Z_2 = sample(LETTERS[1:5], .N, TRUE))] |>
	data.table::setkey(join_key, src, date.start);

# >>> PRESCRIPTED TIMEOUT
prescribed_timeout <- continuity(
			data = obs_data
			, map_fields = c(join_key, src)
			, time_fields = c(date.start, date.end)
			, boundary_name = episode
			, timeout = 10
			, show.all = TRUE
			);

str(prescribed_timeout);
View(prescribed_timeout);

# debug(signal_processor)

# >>> TUNED TIMEOUT
tune_timeout <- break_signal(
	y = obs_data$date.start
	, grp = obs_data[, paste(join_key, src, sep = ":")]
	, obs_ctrl = list(min_size = 5L, max_k = 120)
	) |>
	signal_processor(cl_size = 8, .debug = !TRUE);

tune_timeout@best_k

tuned_timeout <- continuity(
	data = obs_data
	, map_fields = c(join_key, Z_1)
	, time_fields = c(date.start, date.end)
	, boundary_name = episode
	, timeout = tune_timeout@best_k
	, show.all = TRUE
	);

View(tuned_timeout);
str(tuned_timeout);

#
# ~ Cleanup ====
plan(sequential);
gc(full = TRUE)
# rm(list = ls())

# ~ pkgdown ----
# usethis::use_pkgdown()
# usethis::use_proprietary_license("Chionesu George")
Sys.setenv(CSS = dir(paste0(getwd(), "/../resources"), pattern = "css$", full.names = TRUE))
Sys.setenv(L2HTML = dir(paste0(getwd(), "/../resources"), pattern = "list2html", full.names = TRUE, recursive = TRUE))

pkgdown::build_site(pkg = "pkg", examples = FALSE, override = list(destination = "../docs"))
# pkgdown::build_article("Continuity-Example", pkg = "pkg")
# pkgdown::build_article("Event-Vectors-Examples", pkg = "pkg")
# pkgdown::build_article("Event-Space-and-Graph-Reduction", pkg = "pkg")
# pkgdown::build_article("Break-Signal", pkg = "pkg")



