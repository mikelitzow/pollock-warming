# ------------------------------------------------------------
# Age composition + diversity indices (Shannon + Gini-Simpson)
# - Reads CSV
# - Calculates Shannon (H) and Gini-Simpson (1 - sum p^2)
# - Scales Shannon to 0–1 by dividing by ln(S) so it matches
#   the 0–1 scale of Gini-Simpson
# - Plots stacked proportions + white/orange lines in the same
#   style as the original figure
# ------------------------------------------------------------

library(tidyverse)
library(viridis)

theme_set(theme_bw())


# function for Shannon Diversity Index
shannon_diversity <- function(x, base = exp(1), na.rm = TRUE) {
  # Remove NAs if requested
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  
  # Stop if no valid data
  if (length(x) == 0) return(NA_real_)
  
  # Convert to numeric
  x <- as.numeric(x)
  
  # Remove negative values
  if (any(x < 0)) {
    stop("Input contains negative values.")
  }
  
  # Convert to proportions (works for counts or proportions)
  total <- sum(x)
  if (total == 0) return(0)
  p <- x / total
  
  # Remove zeros for log calculation
  p <- p[p > 0]
  
  # Shannon entropy
  H <- -sum(p * log(p, base = base))
  
  return(H)
}

# Gini-Simpson function
gini_simpson <- function(x, na.rm = TRUE) {
  
  # Remove NA values if requested
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  
  # Return NA if no data
  if (length(x) == 0) return(NA_real_)
  
  # Convert to numeric
  x <- as.numeric(x)
  
  # Check for negative values
  if (any(x < 0)) {
    stop("Input contains negative values.")
  }
  
  # Convert to proportions (works for counts or proportions)
  total <- sum(x)
  if (total == 0) return(0)
  p <- x / total
  
  # Gini–Simpson index
  GS <- 1 - sum(p^2)
  
  return(GS)
}


# 1) Read data
dat <- read.csv("./data/assessment_age_diversity.csv")


diversity <- data.frame(Year = 1970:2022,
                        Shannon = apply(dat[,2:10], 1, shannon_diversity),
                        Gini_Simpson = apply(dat[,2:10], 1, gini_simpson)) %>%
  mutate(Shannon = Shannon/max(Shannon))


# make data longer
age_cols <- paste0("Age", 2:10)

dat_long <- dat %>%
  pivot_longer(all_of(age_cols), names_to="Age", values_to="Proportion") %>%
  mutate(Age = factor(Age, levels = age_cols))

ggplot(dat_long, aes(x = Year, y = Proportion, fill = Age)) +
  geom_col(width = 0.95, color = "black", linewidth = 0.25) +
  geom_line(
    data = diversity,
    aes(x = Year, y = Shannon),
    inherit.aes = FALSE,
    color = "orange",
    linewidth = 0.9
  ) +
  geom_line(
    data = diversity,
    aes(x = Year, y = Gini_Simpson),
    inherit.aes = FALSE,
    color = "white",
    linewidth = 0.9
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_viridis_d(direction = 1, breaks = age_cols) +
  labs(x = "Year", y = "Proportion", fill = "Age") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey70", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey85", linewidth = 0.25)
  ) +
  theme(legend.position = "right")


ggsave("./figs/age_comp_diversity.png", width = 8, height = 4, units = 'in')
