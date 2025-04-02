preprocess_data <- function(file1, file2) {
  rbind(file1, file2) %>%
    select(Target, Content, Sample, Cq) %>%
    mutate(Content = paste(Content, Target),
           Sample = ifelse(Sample == "" | is.na(Sample), Content, Sample),
           Cq = round(ifelse(Cq == "", NA, Cq), 2)) %>%
    select(Target, Sample, Cq) %>%
    ## rename EHEC "ICD" to standard "ICR"
    mutate(Target = gsub("ICD", "ICR", Target),
           Sample = gsub("ICD", "ICR", Sample)) %>%
    pivot_wider(
      names_from = Target,
      values_from = Cq,
      values_fn = function(x) paste(x, collapse = ",")
    ) %>%
    relocate(., Sample, ICR)
}


