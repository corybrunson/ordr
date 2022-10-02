# U.S. 1973 violent crime arrests
head(USArrests)
# row and column subsets
state_examples <- c("Hawaii", "Mississippi", "North Dakota")
arrests <- c(1L, 2L, 4L)

# pairwise log-ratios of violent crime arrests for two states
arrest_pairs <- combn(arrests, 2L)
arrest_ratios <-
  USArrests[, arrest_pairs[1L, ]] / USArrests[, arrest_pairs[2L, ]]
colnames(arrest_ratios) <- paste(
  colnames(USArrests)[arrest_pairs[1L, ]], "/",
  colnames(USArrests)[arrest_pairs[2L, ]], sep = ""
)
arrest_logratios <- log(arrest_ratios)
arrest_logratios[state_examples, ]

# non-compositional log-ratio analysis
(arrests_lra <- lra(USArrests[, arrests]))
screeplot(arrests_lra)
biplot(arrests_lra, scale = c(1, 0))

# compositional log-ratio analysis
(arrests_lra <- lra(USArrests[, arrests], compositional = TRUE))
biplot(arrests_lra, scale = c(1, 0))
