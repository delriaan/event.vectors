# ~Initialization ====
# library(EVSpace);
library(data.table)

library(igraph, exclude = c("compose", "simplify", "union", "%->%", "%<-%"))
library(magrittr)
library(purrr)
library(furrr)

library(tictoc);
library(book.of.workflow)
library(future.callr)
plan(callr)

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
	purrr::set_names("test_data." %s+% stringi::stri_pad_left(., width = 2, pad = "0")) %>%
	purrr::map(~{
		set.seed(sample(1:10000, 1));
		.src = LETTERS[[.x]];

		.out = purrr::map_dfr(c(1:j), ~list(jk = rep.int(.x, sample(10:100, 1, TRUE)), src = .src)) %>% as.data.table();

		.init_date = c(as.Date(sprintf(
				"%s-%s-%s"
				, rep.int(year(Sys.Date()), nrow(.out))
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
			setkey(jk, src, date.start, date.end) %>%
			setcolorder(c("jk", "date.start", "date.end", "src"))
		}) %>%
	list2env(envir = dest);
}
# debug(make.test_data);

BLAH <- new.env();
rm(list = ls(pattern = "^test.+data"))
make.test_data(j = 5, n = 5, m = 5, dest = BLAH, .debug = !TRUE);
set.seed(sample(100000, 1));

# ~ Create EVSpace object from test data
tic.clear(); tic.clearlog();
rm(list = ls(pattern = "inspect"));

# ~ Validation #1 ====
tic("EVSpace Validation Object");
#
test.evs <- event.vector.space$new();
# debug(test.evs$configure)
test.evs$
	configure(
		src.names			= "BLAH$" %s+% ls(pattern = "^test_data", envir = BLAH)
		, contexts		= "Data." %s+% 1:length(ls(pattern = "^test_data", envir = BLAH))
		, map.fields	= purrr::map(sequence(length(ls(pattern = "^test_data", envir = BLAH))), ~c("jk", "date.start", "date.end"))
		, row.filters	= purrr::map(sequence(length(ls(pattern = "^test_data", envir = BLAH))), ~rlang::expr(1==1))
		, src.mix = "combn"
		, chatty	= TRUE
		, exclude.mix = { c(
				evs_exclude.blender("Data.6", c("Data.4", "Data.7"))
				, evs_exclude.blender("Data.3", c("Data.1", "Data.5"))
			)}
	)$
	set.data(chatty = TRUE)
# undebug(test.evs$configure)

test.evs$config %>% attributes()
test.evs$.__enclos_env__$private$q_matrix

# debug(test.evs$set.q_graphs)
test.evs$set.q_graphs(chatty = TRUE)
# undebug(test.evs$set.q_graphs)

toc(log = TRUE);
#

# ~ Validation #2 ====
tic("EVSpace Universe Validation");
# debug(make.evs_universe);
make.evs_universe(
	self = test.evs
	, mSt >= quantile(mSt, 0.5)
	, abs(mGap) >= quantile(mGap, 0.5)
	, graph.control = { list(
				quote({ E(g)$title	<- ends(g, E(g)) %>% apply(1, paste, collapse = " -> ")})
				, quote({ V(g)$color <- V(g)$name %>% stri_split_fixed(":", simplify = TRUE) %>% .[, 1L] %>% {
						x = .;
						y = set_names(unique(x), map_chr(unique(x), ~rgb(runif(1), runif(1), runif(1))))
						map_chr(x, ~names(y)[which(y == .x)])
						}
					})
			)}
	, omit.na = !TRUE
	, chatty = TRUE
	);
# undebug(make.evs_universe);
toc(log = TRUE);

test.evs$space[, .(jk, from.coord, to.coord, src.pair, mSt, mGap, mEd, epsilon = as.character(epsilon))] %>% summarytools::dfSummary()

vertex.attributes(test.evs$evt_graphs$`1`)
#
# ~ Validation #3 ====
event_graph <- test.evs$evt_graphs %>% sample(1) %>% .[[1]] %>% print();

f2ab <- list(theta = 0.1, gravitationalConstant = -500, centralGravity = 0.0,  avoidOverlap = 1, damping = 0.7);

event_graph %>%
	visNetwork::visIgraph(physics = TRUE, type = "full") %>%
	visNetwork::visPhysics(solver = "repulsion", timestep = 0.05) %>%
	visNetwork::visOptions(width = "1600", height = "1024") %>%
	visNetwork::visNodes(opacity = 0.5) %>%
	# visNetwork::visEdges(length = 100) %>%
	htmltools::html_print(viewer = browseURL)

rm(BLAH, make.test_data)

# ~ Cleanup ====
plan(sequential);
gc(full = TRUE)

