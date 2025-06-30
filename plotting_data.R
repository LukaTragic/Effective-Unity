


merged_house <- merged_house %>%
  mutate(ideology_category = case_when(
    nokken_poole_dim1 <= -0.6 ~ "Extremely Liberal",
    nokken_poole_dim1 > -0.6 & nokken_poole_dim1 <= -0.1 ~ "Liberal",
    nokken_poole_dim1 > -0.1 & nokken_poole_dim1 <= 0.1 ~ "Centrist",
    nokken_poole_dim1 > 0.1 & nokken_poole_dim1 <= 0.6 ~ "Conservative",
    nokken_poole_dim1 > 0.6 ~ "Extremely Conservative"
  ))


merged_house$ideology_category <- factor(
  merged_house$ideology_category,
  levels = c("Extremely Liberal", "Liberal", "Centrist", "Conservative", "Extremely Conservative")
)

merged_house


colnames(merged_house)



ggplot(merged_house, aes(x = ideology_category, y = LES_1.0, fill = ideology_category)) +
  stat_summary(fun = mean, geom = "bar", color = "black") +
  facet_wrap(~ house_majority_party) +
  labs(
    title = "Average Legislative Effectiveness by Ideology and House Control",
    x = "Ideological Category",
    y = "Average LES 1.0"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  guides(fill = "none")  # Remove legend if the fill is redundant
