# ~Initialization ====
# library(EVSpace);
library(purrr)
library(tictoc);
library(future);
#
make.test_data <- function(j = 5, n = 5, m = 5, dest = globalenv(), .debug = FALSE){
#' Make Test Data for Validation
#'
#' \code{make.test_data} creates several \code{\link[data.table]{data.table}} objects to be used to validate the package functionality
#' @param j (integer | 5L) The number of unique values for 'k' to generate
#' @param n (integer | 5L) The number of sources to create (maximum of 5)
#' @param m (integer | 5L) The maximum number of columns to generate for each created object (maximum of 15): each column represents an attribute not related to time
#' @param dest (environment) The destination environment object
#' @param .debug (logical | \code{FALSE}) When \code{TRUE}, additional debugging items are printed
#'
#' @return One to \code{n} \code{\link[data.table]{data.table}} objects prefixed as 'test_data'.
#'
	j = max(c(3, abs(as.integer(j))));
	n = max(c(3, abs(as.integer(n)))); ifelse(n > 5, 5, n);
	m = max(c(3, abs(as.integer(m)))); ifelse(m > 15, 15, m);

	sequence(n) %>%
	purrr::set_names(paste0("test_data.", stringi::stri_pad_left(., width = 2, pad = "0"))) %>%
	purrr::map(~{
		set.seed(sample(1:10000, 1));
		.src = LETTERS[[.x]];

		.out = purrr::map_dfr(c(1:j), ~list(jk = rep.int(.x, sample(10:100, 1, TRUE)), src = .src)) %>% data.table::as.data.table();

		.init_date = c(as.Date(sprintf(
				"%s-%s-%s"
				, rep.int(data.table::year(Sys.Date()), nrow(.out))
				, sample(stringi::stri_pad_left(1:12, width = 2, pad = "0"), nrow(.out), TRUE)
				, sample(stringi::stri_pad_left(1:28, width = 2, pad = "0"), nrow(.out), TRUE)
				)));

		.out[
			, c("date.start", "date.end") := list(.init_date, .init_date + rpois(n = length(jk), lambda = sample(1:100, length(jk), TRUE)))
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
make.test_data(j = 50, n = 5, m = 5, dest = BLAH, .debug = !TRUE);
set.seed(sample(100000, 1));

# ~ Create EVSpace object from test data
tic.clear(); tic.clearlog();

#
# ~ Validation #1 :: event.vector.space ====
tic("EVSpace Validation Object");
#
test.evs <- event.vector.space$new();
test.evs$
	configure(
		src.names			= paste0("BLAH$", ls(pattern = "^test_data", envir = BLAH))
		, contexts		= paste0("Src", 1:length(ls(pattern = "^test_data", envir = BLAH)))
		, map.fields	= purrr::map(sequence(length(ls(pattern = "^test_data", envir = BLAH))), ~c("jk", "date.start", "date.end"))
		, row.filters	= purrr::map(sequence(length(ls(pattern = "^test_data", envir = BLAH))), ~rlang::expr(1==1))
		, src.mix 		= "combn"
		, chatty			= TRUE
		, exclude.mix = { c(
				evs_exclude.blender("Data.6", c("Data.4", "Data.7"))
				, evs_exclude.blender("Data.3", c("Data.1", "Data.5"))
			)}
		)$
	set.data(chatty = TRUE)$
	set.q_graphs(chatty = TRUE);

test.evs$config |> attributes();
test.evs$.__enclos_env__$private$q_table;

toc(log = TRUE);
#
# ~ Validation #2 :: make.evs_universe() ====
plan(tweak(multisession, workers = 5));

tic("EVSpace Universe Validation");
make.evs_universe(
	self = test.evs
	# , mSt >= quantile(mSt, 0.75)
	, abs(mGap) >= 10
	, graph.control = { rlang::exprs(
				igraph::E(g)$title	<- igraph::ends(g, igraph::E(g)) %>% apply(1, paste, collapse = " -> ")
				, igraph::V(g)$color <- igraph::V(g)$name %>% stringi::stri_split_fixed(":", simplify = TRUE) %>% .[, 1L] %>% {
						x = .;
						y = purrr::set_names(unique(x), purrr::map_chr(unique(x), ~rgb(runif(1), runif(1), runif(1))))
						purrr::map_chr(x, ~names(y)[which(y == .x)])
					}
			)}
	, omit.na = !TRUE
	, chatty = TRUE
	);
toc(log = TRUE);

test.evs$space[, .(jk, from.coord, to.coord, src.pair, mSt, mGap, mEd, epsilon = as.character(epsilon))] %>% summarytools::dfSummary();
test.evs$space[(jk == 4)] %>% View("Space: jk == 4");
igraph::vertex.attributes(test.evs$evt_graphs$`1`);
#
# ~ Validation #3 :: evs_retrace() ====
evs_retrace(test.evs)

event_graph <- test.evs$evt_graphs$`4`
igraph::V(event_graph)$trace[[1]] %>% eval(envir = globalenv())
test.evs$space[
	(jk == 4) & (src.pair %ilike% "(->) src3")
	, as_mapper(~{
			print(.SD[c(.x, .y)])
			print(from <- epsilon[.x])
			print(to <- epsilon[.y])
			atan2(to - from, to) / (pi/4)
		})(1, 4)
	]

test.evs$space[
	(jk == 4) & (src.pair %ilike% "(->) src3")
	, from_timeframe[1] %>% lubridate::int_aligns()
	]

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
rm(BLAH, make.test_data)
plan(sequential);
gc(full = TRUE)

