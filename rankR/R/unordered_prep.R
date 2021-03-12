

unordered_prep<- function(voter_idealpoint){
  voter_idpoint <- fastDummies::dummy_cols(voter_idealpoint)

  voter_idpoint <- voter_idpoint %>% tidyr::pivot_longer(cols = choice_1_a:ncol(voter_idpoint))
  voter_idpoint <- voter_idpoint %>% select(voter_id, name, value)

  stan_data_unordered <<- list(
    J = length(unique(voter_idpoint$voter_id)),
    K = length(unique(voter_idpoint$name)),
    N = length(voter_idpoint$voter_id),
    jj = voter_idpoint$voter_id,
    kk = as.numeric(as.factor(voter_idpoint$name)),
    y = voter_idpoint$value
  )
}
