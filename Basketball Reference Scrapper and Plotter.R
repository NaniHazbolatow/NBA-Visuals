#Load Packages
library(dplyr)
library(tidyverse)
library(ggrepel)
library(readr)
library(RCurl)
library(jpeg)
library(ggplot2)

#Freely changeable
names <- c("Kevin Durant", "Stephen Curry", "Trae Young")
slugs <- c("duranke01", "curryst01", 'youngtr01')
bbstats <- c("FG%")

plot_comparison <- function(names, slugs, statistic, percentage, per_player){
  
bbstats <- statistic
url <- c()
df_store <- c()
image_url <- c()

for (j in slugs){
  url[grep(j, slugs)] <- paste0("https://www.basketball-reference.com/players/", 
                                substr(j, 1, 1), "/", j, ".html")
}

for (i in url){
  df <- rvest::read_html(i) %>%
    html_node("#totals") %>%
    html_table()
    
  slice_end <- which(df$Season == "Career", arr.ind=TRUE)
  
  df <- data.frame(df, check.names = FALSE)

  all_cols <- c(c("Tm","Season"), bbstats)
  
  df <- df %>%
    slice(1:(slice_end-1)) %>%
    select(all_cols) 
  
  df <- df %>%
    mutate(Player = rep(names[grep(i, url)], nrow(df)))
  
  df_store[[grep(i, url)]] <- assign(paste0(abbreviate(names[grep(i, url)], 2), "_df"), df)
  
}

combined_df <- df_store %>%
  reduce(full_join, all = TRUE)

if (percentage == TRUE){
  combined_df[[bbstats]] <- combined_df[[bbstats]]  * 100
}

p <- ggplot(combined_df, mapping = aes(x = Season, y = .data[[bbstats]], color = Player, shape = Tm))

p <- p +  geom_point(size = 1.7) +
 scale_y_continuous(limits = c(ceiling(min(combined_df[[bbstats]] , na.rm = TRUE)) - 10, 
                               ceiling(max(combined_df[[bbstats]] , na.rm = TRUE)) + 5), 
                    breaks = seq(0, (ceiling(max(combined_df[[bbstats]], na.rm = TRUE)) + 5), 
                                 round(
                                   ((ceiling(max(combined_df[[bbstats]], na.rm = TRUE)) 
                                        + ceiling(min(combined_df[[bbstats]], na.rm = TRUE)))/10), 0)))


p <- p + geom_line(aes(group = Player))

p <- p+ labs(title = paste0("Comparison ", bbstats),
             subtitle = paste0(min(combined_df$Season)," to ",max(combined_df$Season)), 
             x = "", 
             y = bbstats,
             shape = "Team")# +
#  scale_shape_discrete(labels = c("Brooklyn Nets", "Golden State Warriors", "Oklahoma City Thunder", "Seattle Supersonics"))
   

p <- p + theme_minimal() + theme(plot.title = element_text(size = 17, face="bold", hjust = .5), 
                plot.subtitle = element_text(face = 'italic', size = 13, hjust = .5), 
                axis.text.x=element_text(angle=60, hjust=1, size=10),
                plot.caption = element_text(color = 'gray40'), 
                plot.margin = margin(10, 10, 15, 10),
                panel.grid.major = element_blank())

if (per_player == TRUE){
  p2 <- p + facet_grid(~ Player) +
    theme_bw() + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major = element_blank())
    
  p2 +  guides(color = 'none')
  plot(p2)
}

plot(p)


}


plot_comparison(names = c("Kevin Durant", "Stephen Curry", "Trae Young"),
                            slugs = c("duranke01", "curryst01", 'youngtr01'),
                            statistic = "3P%", 
                            percentage = TRUE,
                per_player = TRUE)

