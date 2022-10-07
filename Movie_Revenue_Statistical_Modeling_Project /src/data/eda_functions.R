# EDA functions

# stats table
get_stats <- function(data_, field, response, obs_lim = 1) {
  require(dplyr)
  reshape2::melt(data_ %>% select(starts_with(paste0(field, '.')), !!sym(response)),
                 id.vars = 'revenue', variable.name = field) %>% 
    filter(value) %>%
    mutate(!!sym(field) := gsub(paste0(field,'.'),'',!!sym(field))) %>%
    group_by(!!sym(field)) %>%
    summarise(n = n(),
              mean = mean(!!sym(response)),
              median = median(!!sym(response)),
              `std. dev.` = sd(!!sym(response)),
              min = min(!!sym(response)),
              max = max(!!sym(response))) %>%
    filter(n >= obs_lim) %>%
    arrange(desc(n)) %>%
    as.data.frame()
}

# plot mean summaries
plot_means <- function(df_summ, col_name, response, col_lab, obs_lim=1,
                       fill = 'blue', alpha=0.3) {
  require(dplyr)
  require(ggplot2)
  require(scales)
  df_summ %>% 
    filter(n >= obs_lim) %>%
    ggplot(aes(x=reorder(!!sym(col_name), n), y=mean)) +
    geom_bar(stat='identity', fill = fill, alpha = alpha) +
    geom_errorbar(aes(ymin=mean-`std. dev.`/sqrt(n), 
                      ymax=mean+`std. dev.`/sqrt(n)), width=.2) +
    geom_hline(yintercept = mean(df_summ$mean), linetype="dotted") +
    coord_flip() +
    labs(title = paste('Mean Revenue by', col_lab),
         x = '',
         y = 'Mean revenue',
         caption = 'Error bars indicate std. err.') +
    scale_y_continuous(labels = scales::unit_format(prefix = '$',
                                                    scale = 1e-6,
                                                    accuracy = 1,
                                                    unit = 'M'))
}

# similarity matrix
jaccard <- function(df_, a, b) {
  intersection = nrow(df_[df_[a]==TRUE & df_[b]==TRUE,])
  union = nrow(df_[df_[a]==TRUE | df_[b]==TRUE,])
  return (intersection/union)
}

# similarity plot
similarity_plot <- function(df_, col_name, lab_, obs_lim = 1,
                            col_lst = NA,
                            label_set = NA) {
  require(ggplot2)
  require(dplyr)
  require(ggcorrplot)
  
  base_ = df_ %>% select(starts_with(paste0(col_name, '.')))
  if (!is.na(col_lst)) {
    base_ <- base_ %>% select(all_of(col_lst))
  }
  
  if (!is.na(label_set)) {
    colnames(base_) <- label_set
  } else {
    colnames(base_) <- gsub(paste0('^', col_name, '.'), '', colnames(base_))
  }
  jaccard_df = data.frame(matrix(ncol = 0, nrow = 0))
  
  for (i in names(base_)){
    if (nrow(base_[base_[i]==TRUE,]) >= obs_lim) {
      for (ii in names(base_)) {
        if (nrow(base_[base_[ii]==TRUE,]) >= obs_lim) {
          jaccard_df[i,ii] = jaccard(base_, i, ii)}
      }
    }
  }
  ggcorrplot(as.matrix(jaccard_df), 
             hc.order = TRUE,
             outline.col = 'white',
             type = 'lower') +
    scale_fill_distiller('Jaccard', palette = 'Purples', direction = 1) +
    labs(title = paste(lab_, 'Similarity Matrix'),
         caption = expression(paste0('Jaccard index defined as',
                                     over(A*intersect()*B, 
                                          A*union()*B))))
}

# plot revenue by budget
plot_revenue_budget <- function(data_exp) {
  require(ggplot2)
  require(dplyr)
  require(patchwork)
  require(scales)
  
  scat1 <- data_exp %>%
    ggplot(aes(x=budget, y=revenue)) +
    geom_density_2d() +
    geom_smooth() +
    scale_y_continuous(trans = 'log', labels=scales::unit_format(prefix = '$',
                                                                 unit = 'M',
                                                                 scale = 1e-6)) +
    scale_x_continuous(trans = 'log', labels=scales::unit_format(prefix = '$',
                                                                 unit = 'M',
                                                                 scale = 1e-6))
  
  scat1_scales = layer_scales(scat1)
  scat1_ylim = scat1_scales$y$range$range
  scat1_xlim = scat1_scales$x$range$range
  
  d1 <- data_exp %>%
    ggplot(aes(x=budget)) + 
    geom_density(fill = 'blue', alpha=0.2) +
    scale_x_continuous(trans = 'log', limits = exp(scat1_xlim)) +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    ggtitle('Revenue by Budget')
  
  d2 <- data_exp %>%
    ggplot(aes(x=revenue)) + 
    geom_density(fill = 'blue', alpha=0.2) +
    scale_x_continuous(trans = 'log', limits = exp(scat1_ylim)) +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    coord_flip()
  
  
  d1 + plot_spacer() + scat1 + d2 +
    plot_layout(ncol = 2,
                nrow = 2, 
                widths = c(4, 1),
                heights = c(1, 4))
}
