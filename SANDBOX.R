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
cross.time()

mtrx <- matrix(c(s0[4],s1[4],e0[4],e1[4]), nrow = 2, byrow = TRUE)
mtrx
det(mtrx) %>% as.complex() %>% sqrt()
res$value
.beta


usethis::use_pkgdown()