source(here("src", "utilis", "args.R"))
source(here("src", "demographics", "avg_sd_range.R"))
source(here("src", "demographics", "count_per.R"))

demographics <- function(...) {
    dfs <- process_args(...)
    
    result <- lapply(dfs, function(df) {
        tibble(
            label = c("n", "age", "sex"),
            value = c(
                nrow(df), 
                avg_sd_range(df$age), 
                count_per(df$sex, "0")
            )
        )
    })
    
    simplify_result(result)
}