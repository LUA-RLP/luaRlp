preprocess_data_COV_FLU <- function(COV, FLU){
  rbind(COV, FLU) %>%
    select(Target, Content, Sample, Cq) %>%
    mutate(Content = paste(Content, Target),
           Sample = ifelse(Sample == "" | is.na(Sample), Content, Sample),
           Cq = round(Cq, 2)) %>%
    select(Target, Sample, Cq) %>%
    pivot_wider(
      names_from = Target,
      values_from = Cq,
      values_fn = function(x) paste(x, collapse = ",")
    ) %>%
    mutate(across(c(`CoV-2`, InfA, InfB, `RSV A/B`), as.numeric)) %>%
    select(Sample, ICR, `CoV-2`, InfA, InfB, `RSV A/B`)
}
