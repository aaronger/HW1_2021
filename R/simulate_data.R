
# simulate_data

simulate_rw <- function(
                        rw0, # vector of starting values
                        sd_rw = 0.025,
                        n_steps = 10,
                        drift = 0.01, 
                        seed = 1234) {
  set.seed(seed)
  rw <- tibble(state = seq_len(length(rw0)), t0 = rw0) %>%
    group_by(state) %>%
    nest() %>%
    # learning note: if it's difficult to understand what's happening in this code,
    # considering pulling out just the function that's used
    # give it a name and test it out
    mutate(t = map(data, function(x) {
      t0 <- qlogis(as.numeric(x) / 100) 
      # learning note: alternative to writing your own logit function
      trajectory <- cumsum(c(t0, rnorm(n_steps, drift, sd_rw)))
      names(trajectory) <- seq_len(n_steps + 1) - 1
      as_tibble_row(plogis(trajectory) * 100)
    })) %>%
    unnest(c(data, t)) %>%
    select(-t0)
  return(rw)
}
