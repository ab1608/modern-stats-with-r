movies <- read_csv(
  "movies.csv",
  col_types = cols(
    .default = "c",
    `budget_2013$` = "d"
  )
)


movies |> ggplot(aes(x = `budget_2013$`, y = `domgross_2013$`)) + geom_point()
