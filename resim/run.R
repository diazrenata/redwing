source(here::here("resim/resim_fxns.R"))

ts_comp <- BBSsize::granby

ts_gmms <- construct_sampling_gmm(granby)

gsims <- draw_communities_wrapper(granby, ndraws = 50, sampling_gmms = ts_gmms)
