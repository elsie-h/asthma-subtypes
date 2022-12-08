# ggplot theme
theme_elsie <- function(...) {
  theme_bw(base_size = 10) + 
    theme(
      plot.caption = element_text(hjust = 0, face= "italic"),
      ...
    )
}