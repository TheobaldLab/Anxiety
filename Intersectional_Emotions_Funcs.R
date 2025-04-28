# Load required libraries
library(dplyr)
library(ggplot2)
library(forcats)
library(emmeans)
library(gridExtra)

#' Analyze emotions across intersectional identities
#'
#' @param data Data frame containing the emotion and demographic variables
#' @param emotion_col Name of the emotion column to analyze (e.g., "Emotions_4")
#' @param race_col Name of the race column (default: "race_cat")
#' @param gender_col Name of the gender column (default: "gender_cat")
#' @param firstgen_col Name of the first-generation status column (default: "FirstGen")
#' @param min_group_size Minimum number of observations required for a group (default: 10)
#' @param top_n Number of top and bottom groups to display (default: 7)
#' @return A list containing analysis results and plots
analyze_emotion_intersectionally <- function(data, 
                                             emotion_col,
                                             race_col = "race_cat",
                                             gender_col = "gender_cat",
                                             firstgen_col = "FirstGen",
                                             min_group_size = 10,
                                             top_n = 7) {
  
  # Load libraries
  library(dplyr)
  library(ggplot2)
  library(forcats)
  library(emmeans)
  
  # Validate input
  if (!emotion_col %in% names(data)) {
    stop(paste("Emotion column", emotion_col, "not found in data"))
  }
  
  # Create cleaner emotion name
  emotion_name <- gsub("Emotions_", "", emotion_col)
  if (emotion_col == "Emotions_1") emotion_name <- "Curious"
  if (emotion_col == "Emotions_2") emotion_name <- "Excited"
  if (emotion_col == "Emotions_3") emotion_name <- "Confused"
  if (emotion_col == "Emotions_4") emotion_name <- "Anxious"
  if (emotion_col == "Emotions_5") emotion_name <- "Frustrated"
  if (emotion_col == "Emotions_6") emotion_name <- "Bored"
  if (emotion_col == "Emotions_7") emotion_name <- "Surprised"
  
  # Filter data
  analysis_data <- data %>%
    filter(!is.na(get(emotion_col)), !is.na(get(race_col)),
           !is.na(get(gender_col)), !is.na(get(firstgen_col)))
  
  # Create intersectional group
  analysis_data$three_way <- paste(
    analysis_data[[race_col]], 
    analysis_data[[gender_col]], 
    analysis_data[[firstgen_col]],
    sep = " × "
  )
  
  # Calculate stats for intersectional groups
  three_way_stats <- analysis_data %>%
    group_by(three_way, .data[[race_col]], .data[[gender_col]], .data[[firstgen_col]]) %>%
    summarize(
      n = n(),
      mean_emotion = mean(get(emotion_col), na.rm = TRUE),
      sd_emotion = sd(get(emotion_col), na.rm = TRUE),
      se_emotion = sd_emotion / sqrt(n),
      ci_lower = mean_emotion - 1.96 * se_emotion,
      ci_upper = mean_emotion + 1.96 * se_emotion,
      high_emotion_pct = mean(get(emotion_col) >= 4, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    filter(n >= min_group_size)
  
  # Calculate main effects
  overall_mean <- mean(analysis_data[[emotion_col]], na.rm = TRUE)
  
  race_effects <- analysis_data %>%
    group_by(.data[[race_col]]) %>%
    summarize(race_effect = mean(get(emotion_col), na.rm = TRUE) - overall_mean, .groups = "drop")
  
  gender_effects <- analysis_data %>%
    group_by(.data[[gender_col]]) %>%
    summarize(gender_effect = mean(get(emotion_col), na.rm = TRUE) - overall_mean, .groups = "drop")
  
  firstgen_effects <- analysis_data %>%
    group_by(.data[[firstgen_col]]) %>%
    summarize(firstgen_effect = mean(get(emotion_col), na.rm = TRUE) - overall_mean, .groups = "drop")
  
  # Join effects and calculate expected emotion
  three_way_stats <- three_way_stats %>%
    left_join(race_effects, by = race_col) %>%
    left_join(gender_effects, by = gender_col) %>%
    left_join(firstgen_effects, by = firstgen_col) %>%
    mutate(
      expected_emotion = overall_mean + race_effect + gender_effect + firstgen_effect,
      difference = mean_emotion - expected_emotion,
      pct_difference = (difference / expected_emotion) * 100
    )
  
  # ANOVA model
  three_way_model <- aov(as.formula(paste(emotion_col, "~ three_way")), 
                         data = analysis_data %>% filter(three_way %in% three_way_stats$three_way))
  anova_results <- summary(three_way_model)
  
  # Pairwise comparisons (optional, may fail)
  tryCatch({
    emm <- emmeans(three_way_model, specs = "three_way")
    pairwise_results <- pairs(emm, adjust = "tukey")
    sig_pairs <- as.data.frame(summary(pairwise_results)) %>%
      filter(p.value < 0.05) %>%
      arrange(p.value)
  }, error = function(e) {
    message("Warning: Pairwise comparisons failed: ", e$message)
    sig_pairs <- data.frame()
  })
  
  # Formal interaction model
  interaction_model <- lm(as.formula(paste(emotion_col, "~", race_col, "*", gender_col, "*", firstgen_col)), 
                          data = analysis_data)
  anova_interaction <- anova(interaction_model)
  
  ## 📈 VISUALIZATION: PRESERVE "Highest" and "Lowest" Labels
  
  # Get top_n highest and lowest groups separately
  highest_groups <- three_way_stats %>%
    arrange(desc(mean_emotion)) %>%
    head(top_n) %>%
    mutate(group_type = paste("Highest", emotion_name)) %>%
    arrange(desc(mean_emotion))  # make sure highest red first
  
  lowest_groups <- three_way_stats %>%
    arrange(mean_emotion) %>%
    head(top_n) %>%
    mutate(group_type = paste("Lowest", emotion_name)) %>%
    arrange(desc(mean_emotion))  # arrange DESCENDING here too for blue group!
  
  # Bind RED first then BLUE
  top_bottom_groups <- bind_rows(highest_groups, lowest_groups)
  
  # Set factor levels for plotting
  top_bottom_groups$three_way <- factor(top_bottom_groups$three_way, levels = rev(top_bottom_groups$three_way))
  

  p1 <- ggplot(top_bottom_groups, aes(x = three_way, y = mean_emotion, fill = group_type)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    geom_text(aes(label = sprintf("n=%d", n)), vjust = -0.5, size = 3) +
    labs(
      title = paste("Groups with Highest and Lowest", emotion_name, "Levels"),
      subtitle = "Three-way Intersections of Race × Gender × First-Generation Status",
      x = "",
      y = paste("Mean", emotion_name, "Level"),
      fill = ""
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
    coord_flip() +
    scale_fill_manual(values = setNames(c("red", "blue"), 
                                        c(paste("Highest", emotion_name), paste("Lowest", emotion_name))))
  
  # Plot: Percent difference
  top_bottom_diff <- rbind(
    head(three_way_stats %>% arrange(desc(difference)), top_n) %>% mutate(group_type = "Higher Than Expected"),
    head(three_way_stats %>% arrange(difference), top_n) %>% mutate(group_type = "Lower Than Expected")
  )
  
  p2 <- ggplot(top_bottom_diff, aes(x = reorder(three_way, difference), y = pct_difference, fill = group_type)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", pct_difference)), 
              y = ifelse(top_bottom_diff$pct_difference > 0, 
                         top_bottom_diff$pct_difference/2, 
                         top_bottom_diff$pct_difference/2),
              color = "white", size = 3) +
    labs(
      title = paste("Percentage Difference in", emotion_name, "Beyond Main Effects"),
      x = "", y = "Percentage Difference", fill = ""
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8)) +
    coord_flip() +
    scale_fill_manual(values = c("Higher Than Expected" = "red", "Lower Than Expected" = "blue"))
  
  # Plot: Observed vs Expected
  p3 <- ggplot(three_way_stats, aes(x = reorder(three_way, mean_emotion))) +
    geom_point(aes(y = mean_emotion), color = "red", size = 3) +
    geom_point(aes(y = expected_emotion), color = "blue", size = 3) +
    geom_segment(aes(xend = three_way, y = expected_emotion, yend = mean_emotion), color = "grey50") +
    labs(
      title = paste("Observed vs. Expected", emotion_name, "by Group"),
      x = "", y = paste(emotion_name, "Level")
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
  
  # Return results
  return(list(
    emotion_name = emotion_name,
    overall_mean = overall_mean,
    race_effects = race_effects,
    gender_effects = gender_effects,
    firstgen_effects = firstgen_effects,
    three_way_stats = three_way_stats,
    anova_results = anova_results,
    anova_interaction = anova_interaction,
    sig_pairs = sig_pairs,
    plots = list(
      mean_levels = p1,
      pct_difference = p2,
      observed_vs_expected = p3
    )
  ))
}

#' Generate a comprehensive report for emotion analysis
#'
#' @param analysis_results Results from analyze_emotion_intersectionally function
#' @param output_file File path for saving the report (default: NULL, prints to console)
#' @return Invisibly returns the analysis results
generate_emotion_report <- function(analysis_results, output_file = NULL) {
  if (!is.null(output_file)) {
    sink(output_file)
    on.exit(sink())
  }
  
  # Print basic information
  cat("===============================================================\n")
  cat("ANALYSIS OF", toupper(analysis_results$emotion_name), "ACROSS INTERSECTIONAL IDENTITIES\n")
  cat("===============================================================\n\n")
  
  cat("Overall Mean:", round(analysis_results$overall_mean, 3), "\n\n")
  
  # Print main effects
  cat("MAIN EFFECTS:\n\n")
  
  cat("By Race:\n")
  print(analysis_results$race_effects %>% 
          mutate(race_effect = round(race_effect, 3)) %>%
          arrange(desc(race_effect)))
  
  cat("\nBy Gender:\n")
  print(analysis_results$gender_effects %>% 
          mutate(gender_effect = round(gender_effect, 3)) %>%
          arrange(desc(gender_effect)))
  
  cat("\nBy First-Generation Status:\n")
  print(analysis_results$firstgen_effects %>% 
          mutate(firstgen_effect = round(firstgen_effect, 3)) %>%
          arrange(desc(firstgen_effect)))
  
  # Print ANOVA results
  cat("\n\nANOVA RESULTS FOR THREE-WAY INTERSECTION:\n")
  print(analysis_results$anova_results)
  
  # Print formal interaction test
  cat("\n\nFORMAL TEST OF THREE-WAY INTERACTION EFFECT:\n")
  print(analysis_results$anova_interaction)
  
  # Print groups with highest emotion levels
  cat("\n\nGROUPS WITH HIGHEST", toupper(analysis_results$emotion_name), "LEVELS:\n")
  top_groups <- analysis_results$three_way_stats %>% 
    arrange(desc(mean_emotion)) %>%
    head(10)
  top_groups$mean_emotion <- round(top_groups$mean_emotion, 3)
  top_groups$se_emotion <- round(top_groups$se_emotion, 3)
  print(top_groups[, c("three_way", "n", "mean_emotion", "se_emotion")])
  
  # Print groups with lowest emotion levels
  cat("\n\nGROUPS WITH LOWEST", toupper(analysis_results$emotion_name), "LEVELS:\n")
  bottom_groups <- analysis_results$three_way_stats %>% 
    arrange(mean_emotion) %>%
    head(10)
  bottom_groups$mean_emotion <- round(bottom_groups$mean_emotion, 3)
  bottom_groups$se_emotion <- round(bottom_groups$se_emotion, 3)
  print(bottom_groups[, c("three_way", "n", "mean_emotion", "se_emotion")])
  
  # Print groups with highest positive differences
  cat("\n\nGROUPS WITH", toupper(analysis_results$emotion_name), "HIGHER THAN EXPECTED:\n")
  high_diff_groups <- analysis_results$three_way_stats %>% 
    arrange(desc(difference)) %>%
    head(10)
  high_diff_groups$mean_emotion <- round(high_diff_groups$mean_emotion, 3)
  high_diff_groups$expected_emotion <- round(high_diff_groups$expected_emotion, 3)
  high_diff_groups$difference <- round(high_diff_groups$difference, 3)
  high_diff_groups$pct_difference <- round(high_diff_groups$pct_difference, 3)
  print(high_diff_groups[, c("three_way", "n", "mean_emotion", "expected_emotion", "difference", "pct_difference")])
  
  # Print groups with lowest negative differences
  cat("\n\nGROUPS WITH", toupper(analysis_results$emotion_name), "LOWER THAN EXPECTED:\n")
  low_diff_groups <- analysis_results$three_way_stats %>% 
    arrange(difference) %>%
    head(10)
  low_diff_groups$mean_emotion <- round(low_diff_groups$mean_emotion, 3)
  low_diff_groups$expected_emotion <- round(low_diff_groups$expected_emotion, 3)
  low_diff_groups$difference <- round(low_diff_groups$difference, 3)
  low_diff_groups$pct_difference <- round(low_diff_groups$pct_difference, 3)
  print(low_diff_groups[, c("three_way", "n", "mean_emotion", "expected_emotion", "difference", "pct_difference")])
  
  # Print significant pairwise differences
  if (!is.null(analysis_results$sig_pairs) && nrow(analysis_results$sig_pairs) > 0) {
    cat("\n\nSIGNIFICANT PAIRWISE DIFFERENCES (p < 0.05, showing first 20):\n")
    print(head(analysis_results$sig_pairs, 20))
  } else {
    cat("\n\nNo significant pairwise differences found or pairwise comparisons not available.\n")
  }
  
  # Return results invisibly
  invisible(analysis_results)
}

#' Analyze multiple emotions and create a summary
#'
#' @param data Data frame containing the emotion and demographic variables
#' @param emotion_cols Vector of emotion column names to analyze
#' @param race_col Name of the race column (default: "race_cat")
#' @param gender_col Name of the gender column (default: "gender_cat")
#' @param firstgen_col Name of the first-generation status column (default: "FirstGen")
#' @param min_group_size Minimum number of observations required for a group (default: 10)
#' @param output_dir Directory to save reports (default: NULL, prints to console)
#' @return A list containing analysis results for each emotion
analyze_multiple_emotions <- function(data, 
                                      emotion_cols,
                                      race_col = "race_cat",
                                      gender_col = "gender_cat",
                                      firstgen_col = "FirstGen",
                                      min_group_size = 10,
                                      output_dir = NULL) {
  
  # Create output directory if specified
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
  }
  
  # Initialize results list
  all_results <- list()
  
  # Analyze each emotion
  for (emotion_col in emotion_cols) {
    cat("Analyzing", emotion_col, "...\n")
    
    # Run analysis
    results <- analyze_emotion_intersectionally(
      data, 
      emotion_col, 
      race_col, 
      gender_col, 
      firstgen_col, 
      min_group_size
    )
    
    # Generate report
    if (!is.null(output_dir)) {
      output_file <- file.path(output_dir, paste0(results$emotion_name, "_report.txt"))
      generate_emotion_report(results, output_file)
      
      # Save plots
      ggsave(
        file.path(output_dir, paste0(results$emotion_name, "_mean_levels.png")),
        results$plots$mean_levels,
        width = 10, height = 8
      )
      
      ggsave(
        file.path(output_dir, paste0(results$emotion_name, "_pct_difference.png")),
        results$plots$pct_difference,
        width = 10, height = 8
      )
      
      ggsave(
        file.path(output_dir, paste0(results$emotion_name, "_observed_vs_expected.png")),
        results$plots$observed_vs_expected,
        width = 12, height = 8
      )
    } else {
      generate_emotion_report(results)
      
      # Display plots
      print(results$plots$mean_levels)
      print(results$plots$pct_difference)
      print(results$plots$observed_vs_expected)
    }
    
    # Store results
    all_results[[emotion_col]] <- results
  }
  
  # Create comparative visualization across emotions
  if (length(emotion_cols) > 1) {
    # Extract effect sizes for each emotion
    effect_sizes <- data.frame()
    
    for (emotion_col in emotion_cols) {
      results <- all_results[[emotion_col]]
      
      # Get top effects with highest absolute differences
      top_effects <- results$three_way_stats %>%
        arrange(desc(abs(difference))) %>%
        head(6)
      
      # Add emotion name and select needed columns
      top_effects$emotion <- results$emotion_name
      top_effects_subset <- top_effects[, c("three_way", "difference", "pct_difference", "emotion")]
      
      # Add to combined dataframe
      effect_sizes <- rbind(effect_sizes, top_effects_subset)
    }
    
    # Create comparison plot
    p_comparison <- ggplot(effect_sizes, 
                           aes(x = reorder(three_way, abs(difference)), 
                               y = difference, 
                               fill = emotion)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(
        title = "Comparison of Intersectional Effects Across Emotions",
        subtitle = "Difference between observed and expected emotion levels",
        x = "",
        y = "Difference (Observed - Expected)",
        fill = "Emotion"
      ) +
      theme_minimal()
    
    if (!is.null(output_dir)) {
      ggsave(
        file.path(output_dir, "emotion_comparison.png"),
        p_comparison,
        width = 12, height = 10
      )
    } else {
      print(p_comparison)
    }
    
    all_results[["comparison_plot"]] <- p_comparison
  }
  
  return(all_results)
}