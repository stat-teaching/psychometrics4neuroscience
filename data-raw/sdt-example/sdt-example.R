library(tidyverse)

data(roc6, package = "MPTinR")
dat <- tibble(roc6) |>
    filter(exp == "Koen-2013_immediate") |>
    separate(id, c("id", "exp2"), sep = ":") |>
    select(-exp, -exp2) |>
    pivot_longer(-id, values_to = "n") |>
    separate(name, into = c("stimulus", "response")) |>
    mutate(
        pid = fct_inorder(factor(id)),
        stimulus = factor(
            stimulus,
            levels = c("NEW", "OLD"),
            labels = c("New", "Old")
        ),
        response = factor(
            response,
            levels = c("3new", "2new", "1new", "1old", "2old", "3old"),
            labels = 1:6
        ) |>
            as.integer(),
        .keep = "unused",
        .before = 1
    )

# Save ordinal data for part 2
dat2 <- dat

# Convert to binary responses for part 1
dat <- dat |>
    mutate(
        response = if_else(response %in% 1:3, "New", "Old") |>
            factor()
    )

# Unaggregate and add trial number to clarify data structure
dat <- uncount(dat, n) |>
    mutate(trial = 1:n(), .by = pid, .after = 1)

saveRDS(dat, "data/sdt-example.rds")
