preprocess_data <- function(file1, file2, nicecols = "generic") {
  rbind(file1, file2) %>%
    select(Target, Content, Sample, Cq) %>%
    mutate(Content = paste(Content, Target),
           Sample = ifelse(Sample == "" | is.na(Sample), Content, Sample),
           Cq = round(ifelse(Cq == "", NA, Cq), 2)) %>%
    select(Target, Sample, Cq) %>%
    pivot_wider(
      names_from = Target,
      values_from = Cq,
      values_fn = function(x) paste(x, collapse = ",")
    ) %>%
    {  # Capture piped data with curly braces
      if (nicecols %in% "cov-flu") {
        select(., Sample, ICR, `CoV-2`, InfA, InfB, `RSV A/B`)
      } else {
        relocate(., Sample, ICR)
      }
    }
}

