pkgload::load_all()
all_combines <- purrr::map_dfr(seq(2000, nflreadr::most_recent_season(TRUE) + 1),
                               purrr::possibly(pfr_combine,otherwise = tibble::tibble()))

nflversedata::nflverse_save(
  data_frame = all_combines,
  file_name = "combine",
  nflverse_type = "combine measurements",
  release_tag = "combine"
)
