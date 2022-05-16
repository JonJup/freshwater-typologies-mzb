scherer_raincloud_plot <- function(data, var, pal, label = NULL){
        
        data$typology_num = as.factor(data$typology)
        data$typology_num = as.numeric(data$typology_num)
        
        df_red <- select(data, c("typology_num", "typology", var))
        
        if (is.null(label)) label <- var
        names(df_red)[3] <- "var"
        df_iqr <-
                df_red |> 
                group_by(typology) |> 
                summarise(m = median(var),
                          q25 = quantile(var, 0.25),
                          q75 = quantile(var, 0.75),
                          n = n()) |> 
                mutate(typology_num = factor(typology)) |> 
                mutate(typology_num = as.numeric(typology_num)) 
        max_x <- max(df_red$var) + 0.1
        min_x <- min(df_red$var) - 0.1
        breaks_var <- seq(min_x:max_x, length.out = 5)
        breaks_var <- round(breaks_var, 2)
        
        out <- ggplot(df_iqr, aes(x = m, y = typology_num - 0.2)) + 
                geom_linerange(
                        data = df_iqr,
                        aes(
                                xmin = min_x, 
                                xmax = m, 
                                y = typology_num, 
                                col = typology
                                ),
                        inherit.aes = FALSE,
                        linetype = "dotted",
                        size = 0.7
                ) + 
                geom_rect(
                        aes(
                                xmin = q25, 
                                xmax = m,
                                ymin = typology_num - .05,
                                ymax = typology_num - .35
                        ),
                        fill = "grey89"
                ) + 
                geom_rect(
                        aes(
                                xmin = q75, 
                                xmax = m,
                                ymin = typology_num - .05,
                                ymax = typology_num - .35
                        ),
                        fill = "grey79"
                ) + 
                geom_segment(
                        aes(x = q25, 
                            xend = q25, 
                            y = typology_num - 0.05, 
                            yend = typology_num - .35,
                            color = typology,
                            color = after_scale(darken(color, .05, space = "HLS")))
                ) +
                geom_segment(
                        aes(x = q75, 
                            xend = q75, 
                            y = typology_num - 0.05, 
                            yend = typology_num - .35,
                            color = typology,
                            color = after_scale(darken(color, .05, space = "HLS")))
                ) + 
                # geom_point(
                #         data = df_red,
                #         aes(color = typology,
                #             x = var),
                #         #shape = "|",
                #         size = 5,
                #         alpha = .33
                # ) + 
                # stat_dots(
                #         data = df_red,
                #         aes(
                #                 x = var,
                #                 y = typology_num,
                #                 color = typology,
                #                 fill = after_scale(lighten(color, .5))
                #        ), 
                #        
                # ) + 
                geom_jitter(
                        data = df_red,
                        aes(
                                x = var, 
                                y = typology_num -.2,
                                color = typology,
                                fill = after_scale(lighten(color, .5))
                        ),
                        width = 0,
                        height = 0.1,
                        alpha = 0.33
                ) + 
                stat_halfeye(
                        data = df_red,
                        aes(
                                x = var,
                                y = typology_num,
                                color = typology,
                                fill = after_scale(lighten(color, .5))
                        ), 
                        shape = 18, 
                        .width = c(0,1), 
                        width = 0.6,
                        adjust = 0.5,
                        point_size = 3,
                        interval_size = 1.8,
                        scale = 0.5
                ) + 
                geom_text(
                        aes(
                                x = m,
                                y = typology_num + .2,
                                label = format(round(m, 2), nsmall = 2),
                                color = typology,
                                color = after_scale(darken(color, 0.5))
                        ),
                        inherit.aes = FALSE,
                        fontface = "bold",
                        size = 3.5
                ) + 
                geom_text(
                        data = df_red |>
                                group_by(typology, typology_num) |>
                                summarize(n = n(), max = max(var, na.rm = T)),
                        aes(
                                x = max + .05,
                                y = typology_num + 0.01,
                                label = glue::glue("n = {n}"),
                                color = typology
                        )
                ) + 
                scale_color_manual(values = pal,
                                   guide = "none") + 
                scale_fill_manual(values = pal,
                                  guide = "none") + 
                coord_cartesian(clip = "off") + 
                theme(panel.grid.major.x = element_line(color = "grey89", size = 0.5),
                              axis.text.y = element_text(color = rev(pal), size = 14, lineheight = .9),
                      axis.ticks.length = unit(0, "lines"), 
                      plot.subtitle = element_text(margin = c(0,0,-10,0)),
                      panel.background = element_rect(fill = "white")
                ) + 
                scale_x_continuous(
                        limits = c(min_x,max_x),
                        
                        expand = c(.001, .001)
                ) + 
                scale_y_continuous(
                        limits = c(0,4),
                        breaks = c(3,2,1), 
                        labels = c("ife", "brt", "bgr"),
                        expand = c(.0,0)
                ) + 
                labs(x = label, 
                     y = NULL) 
        
        
        return(out)
}