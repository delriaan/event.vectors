test.evs$space[(jk=="1"), c(from.coord, to.coord)] #%>% unique()
g <- test.evs$evt_graphs$`1` %>% {
	g = .
	igraph::vertex_attr(g) <- igraph::V(g) %>% .$name %>%
  		stringi::stri_split_fixed(":", simplify = TRUE) %>%
  		purrr::array_branch(2) %>%
  		purrr::set_names(c("jk", "start", "end", "source", "order"))
	g
}
igraph::vertex_attr(g)
igraph::edge_attr(g)
igraph::E(g)
