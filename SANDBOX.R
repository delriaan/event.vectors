cross.time <- function(s0, s1, e0, e1, control, chatty = FALSE, ...){
		## Reference: https://www.r-bloggers.com/using-complex-numbers-in-r/
		## Division by Pi/4 makes it easy to tell if one argument is larger than, smaller than, or the same magnitude as the other (same = Pi/4)
		## All computations are in the direction of B.max to A.min when `events.ascending` is TRUE
		require(data.table);
		require(magrittr);
		if (missing(control)){ control <- list(-Inf, Inf) }

		XTIME <- { data.table::data.table(
							beta = e1 - s0
							, mGap = s1 - e0
							, mSt = s1 - s0
							, mEd = e1 - e0
							, from.len = e0 - s0
							, to.len = e1 - s1
							)
					}
		epsilon <- XTIME %$% {
				# Do not algebraically reduce the following with respect to 'mGap': the sign is as important as the arguments
				.out = atan2(mEd, mSt) * atan2((mGap * beta), mGap)
				.tau = sign(to.len - from.len)

				# Scale back down to an angle: `sqrt()` needs to have a complex argument for handling negative arguments
				# The square-root of 'mGap'  differentiates offset events from cases where one event envelopes another
				.out = sqrt(as.complex(.out)) + sqrt(as.complex(mGap)^.tau)

				unlist(.out) |> purrr::modify_if(~Re(.x) |> is.infinite(), ~as.complex(0))
		}
		epsilon.desc  <- {
				c(`1` = "Disjoint", `10` = "Concurrency", `100` = "Full Concurrency", `1000` = "Continuity")[
				as.character({
					cbind(
						((Re(epsilon) != 0) & (Im(epsilon) == 0))
						, ((Re(epsilon) == 0) & (Im(epsilon) != 0))
						, ((Re(epsilon) != 0) & (Im(epsilon) != 0))
						, ((Re(epsilon) == 0) & (Im(epsilon) == 0))
						) %*% (10^c(0:3))
					})
				]
			}

		XTIME[, `:=`(epsilon = epsilon, epsilon.desc = epsilon.desc)][(beta %between% control)]
}

s0 <- rep.int(10, 4)
e0 <- s0 + rep.int(10, 4)
s1 <- s0 + c(4, 7, 12, 10)
e1 <- s1 + c(3, 7, 7, 7)

(input <- data.table(use.case = c("FC", "PC", "DJ", "CT"), s0 = s0, e0 = e0, s1 = s1, e1 = e1))
purrr::pmap(input, cross.time)|> purrr::set_names(input$use.case)

rlang::inject(cross.time(!!!as.list(input)))
