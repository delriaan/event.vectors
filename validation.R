# ~ Initialization ====
# library(event.vectors);
library(purrr)
library(stringi)
library(tictoc);
library(future);
#
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

		.out = purrr::map_dfr(c(1:j), ~list(jk = rep.int(.x, sample(10:100, 1, TRUE)), src = .src)) %>% data.table::as.data.table();

		.init_date = c(as.Date(sprintf(
				"%s-%s-%s"
				, rep.int(data.table::year(Sys.Date()), nrow(.out))
				, sample(stringi::stri_pad_left(1:12, width = 2, pad = "0"), nrow(.out), TRUE)
				, sample(stringi::stri_pad_left(1:28, width = 2, pad = "0"), nrow(.out), TRUE)
				)));

		.out[
			, c("date.start", "date.end") := list(.init_date, .init_date + rpois(n = length(jk), lambda = sample(o, length(jk), TRUE)))
			][
			, paste0("X_", stringi::stri_pad_left(sample(30, m), width = 2, pad = "0")) := purrr::map(1:m, ~sample(runif(1E6), .N, TRUE))
			][
			runif(length(jk)) > 0.65
			] %>%
			data.table::setkey(jk, src, date.start, date.end) %>%
			data.table::setcolorder(c("jk", "date.start", "date.end", "src"))
		}) %>%
	list2env(envir = dest);
}

BLAH <- new.env();
set.seed(sample(100000, 1));
make.test_data(j = 50, n = 3, m = 5, o = c(5, 20), dest = BLAH, .debug = !TRUE);set.seed(sample(100000, 1));
set.seed(sample(100000, 1));
make.test_data(j = 50, n = 3, m = 5, o = c(5, 20), dest = .GlobalEnv, .debug = !TRUE);

# ~ Create event.vectors object from test data
tic.clear(); tic.clearlog();

#
# ~ Validation #1 :: event.vectors ====
tic("EVSpace Validation Object");
#
test.evs <- event.vectors$new();
test.evs$
	configure(
		src.defs = c(ls(pattern = "^test") |>
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
		)$
	set.data(chatty = TRUE)$
	set.q_graphs(chatty = TRUE);

test.evs$config |> attributes();
test.evs$.__enclos_env__$private$q_table;

toc(log = TRUE);
#
# ~ Validation #2 :: make.evs_universe() ====
plan(sequential);
plan(tweak(multisession, workers = 7));

tic("EVSpace Universe Validation");
make.evs_universe(
	self = test.evs
	# , mSt >= quantile(mSt, 0.75)
	, abs(mGap) >= lubridate::days(5)
	, time.control = list(0, 100)
	, graph.control = { rlang::exprs(
				igraph::E(g)$title	<- igraph::ends(g, igraph::E(g)) %>% apply(1, paste, collapse = " -> ")
				, igraph::V(g)$color <- igraph::V(g)$name %>% stringi::stri_split_fixed(":", simplify = TRUE) %>% .[, 1L] %>% {
						x = .;
						y = purrr::set_names(unique(x), purrr::map_chr(unique(x), ~rgb(runif(1), runif(1), runif(1))))
						purrr::map_chr(x, ~names(y)[which(y == .x)])
					}
				, igraph::V(g)$src <- igraph::V(g)$name %>% stringi::stri_replace_first_regex("[:][0-9]+", "")
				)
		}
	, units = "minutes"
	, omit.na = !TRUE
	, chatty = TRUE
	);
toc(log = TRUE);

test.evs$space[, .(jk, from.coord, to.coord, src.pair, mSt, mGap, mEd, epsilon = as.character(epsilon))] %>% summarytools::dfSummary();
test.evs$space[(jk == 4)] %>% View("Space: jk == 4");
igraph::vertex.attributes(test.evs$evt_graphs$`1`);
#
# ~ Validation #3 :: evs_retrace() ====
evs_retrace(test.evs);
(event_graph <- test.evs$evt_graphs$`4`);

# ~ Validation #4 :: visNetwork::visIgraph() ====
f2ab <- list(theta = 0.1, gravitationalConstant = -5000, centralGravity = 0.0,  avoidOverlap = 1, damping = 0.7);

event_graph %>% {
		g = .
		igraph::V(g)$title <- igraph::V(g)$trace |>
			purrr::map_chr(~{
				x = eval(.x, envir = globalenv()) %>%
					.[, purrr::modify_at(.SD[, !"row.filters"], c("start_idx", "end_idx"), as.character)] %>%
					melt(measure.vars = names(.), variable.name = "key", variable.factor = FALSE)

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

# ~ Cleanup ====
book.of.workflow::do.save_image(
	"BLAH", "test.evs"
	, file.name = "test_data"
	, save.dir = "experiments"
	, safe = TRUE
	)

rm(list = ls())
plan(sequential);
gc(full = TRUE)


# usethis::use_pkgdown()
# pkgdown::build_site()

