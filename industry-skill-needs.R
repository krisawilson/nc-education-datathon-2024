# file path
path <- "Data/datathon datasets/public_use-industry-skills-needs.xlsx"

# read in data
ms <- readxl::read_excel(path = path, sheet = "Industry Skills Needs")

# mutations ----
# scale the rank
ms <- ms |> dplyr::mutate(weighted_rank = 11 - skill_group_rank,
                          inv_rank = 1/skill_group_rank)

# summary statistics for rank
ms |> dplyr::group_by(skill_group_category) |> 
  dplyr::summarise(avg_rank = mean(skill_group_rank),
                   sd_rank = sd(skill_group_rank),
                   n = dplyr::n()) # lower better

# summary stats for weighted rank
ms |> dplyr::group_by(skill_group_category) |> 
  dplyr::summarise(avg_inv_rank = mean(inv_rank),
                   sd_inv_rank = sd(inv_rank),
                   n = dplyr::n()) # higher better

# summary stats for inverse rank
ms |> dplyr::group_by(skill_group_category) |> 
  dplyr::summarise(avg_wt_rank = mean(weighted_rank),
                   sd_wt_rank = sd(weighted_rank),
                   n = dplyr::n()) # higher better

# anova with regular and weighted ranks ----
anova_rank <- aov(skill_group_rank ~ factor(skill_group_category), 
                  data = ms)
TukeyHSD(anova_rank)
anova_wt_rank <- aov(weighted_rank ~ factor(skill_group_category), 
                  data = ms)
TukeyHSD(anova_wt_rank)
anova_inv_rank <- aov(inv_rank ~ factor(skill_group_category), 
                  data = ms)
TukeyHSD(anova_inv_rank)