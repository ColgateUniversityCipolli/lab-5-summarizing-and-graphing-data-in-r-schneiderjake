library(tidyverse)
library(xtable)


#open our csv files 
essentia.data <- read_csv("data/essentia.data.csv") 

allentown.data <- read_csv("data/essentia.data.allentown.csv")

#this creates an object for us with all of the numeric column names
features.numeric <- names(essentia.data)[sapply(essentia.data, is.numeric)]

#creating an empty tibble to store our results in 
compared.features <- tibble()

for(feature in features.numeric){
  
  essentia.data.grouped <- essentia.data %>%
    group_by(artist) %>%
    
    #we are summarizing out data and creating min, max, upper and lower forks for each feature of the song 
    summarize(
      min = min(get(feature), na.rm = TRUE),
      LF = quantile(get(feature), .25, na.rm = TRUE) - (1.5 * IQR(get(feature), na.rm = TRUE)),
      UF = quantile(get(feature), .75, na.rm = TRUE) + (1.5 * IQR(get(feature), na.rm = TRUE)),
      max = max(get(feature), na.rm = TRUE)
    ) 
  
  allentown.feature <- allentown.data[[feature]] #we compare the 5 number summary of that feature to allentown 
  
  essentia.data.grouped <- essentia.data.grouped %>% 
    mutate(
      out.of.range = allentown.feature < min | allentown.feature > max , 
      unusual = allentown.feature < LF | allentown.feature > UF , 
      description = case_when(
        out.of.range ~ "Out Of Range",
        unusual ~ "Outlying",
        TRUE ~ "Within Range"
      ),
      feature = feature
    )
  compared.features <- bind_rows(compared.features, essentia.data.grouped)
}

compared.features.filtered <- compared.features|>
  filter(feature == "overall_loudness" | feature == "tempo" | feature == "duration" | feature == "beats_count" |
           feature == "beats_loudness" | feature == "tuning_frequency" | feature == "acoustic" | feature == "electronic" |
           feature == "danceability" | feature == "Tone" | feature == "WC" | feature == "happy")

compared.features.filtered.table <- xtable(compared.features.filtered,
                                           caption = "Artist Features Being Compared to Allentown", 
                                           label = "compared.features.xtable")


essentia.data.pivoted <- essentia.data |>
  pivot_longer(cols = all_of(features.numeric), names_to = "feature", values_to = "value") |>
  filter(feature == "overall_loudness" | feature == "tempo" | feature == "duration" | feature == "beats_count" |
           feature == "beats_loudness" | feature == "tuning_frequency" | feature == "acoustic" | feature == "electronic" |
           feature == "danceability" | feature == "Tone" | feature == "WC" | feature == "happy")

allentown.data.pivoted <- allentown.data |>
  pivot_longer(cols = all_of(features.numeric), names_to = "feature", values_to = "value") |>
  filter(feature == "overall_loudness" | feature == "tempo" | feature == "duration" | feature == "beats_count" |
           feature == "beats_loudness" | feature == "tuning_frequency" | feature == "acoustic" | feature == "electronic" |
           feature == "danceability" | feature == "Tone" | feature == "WC" | feature == "happy")



ggplot(data = essentia.data.pivoted, aes(y = value, fill = artist)) +
  geom_boxplot(width = 0.5, outlier.colour = "red", outlier.shape = 16, outlier.size = 1.5) +  
  facet_wrap(~ feature, scales = "free_y", ncol = 4 ) +  # Different y-axis scale for each feature
  geom_boxplot(data = allentown.data.pivoted, aes(y=value), color = "black", size = .4) +
  labs(
    title = "Feature Distributions Across Artists",
    y = "Feature Value"
  )+
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text labels
    axis.ticks.x = element_blank(),  # Remove x-axis tick marks
    legend.position = "bottom"
  )



