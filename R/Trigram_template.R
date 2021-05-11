# What total here? 

p_tri <- ggplot(pod_viz2016_42_month, aes(x=date4, y=total, color = trigram)) + 
  geom_area(aes(fill=trigram))+
  labs(title = "Trigrams over time - Podcasting Subreddit", x = "",
       y = "Number of occurences")+
  theme(panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        axis.line = element_line(colour = "grey"),
        legend.position="top",
        legend.spacing.x = unit(0.2, 'cm'),
        legend.spacing.y = unit(2,"cm"), 
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text  = element_text(margin = margin(r = 10, unit = "pt"), size = 8),
        legend.key = element_rect(size = 6, fill = "white"),
        legend.key.size = unit(0.8, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10))+
  scale_x_date(date_breaks = "5 months", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE))))+
  guides(fill= guide_legend(nrow=5), 
         shape = guide_legend(override.aes = list(size = 0.2)),
         color = guide_legend(override.aes = list(size = 0.2, shape =11)))

ggsave(filename="trigram11.pdf", 
       plot = p_tri, 
       device = cairo_pdf, 
       width = 45, 
       height = 20, 
       units = "cm")