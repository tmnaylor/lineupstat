# This function takes a df with two columns, confidence and acc
# where acc = binary accuracy.  It is not called directly by
# the user.  It returns an ROC object of package pROC
make_rocdata <- function(df_confacc) {
    rocobj <- roc(acc ~ confidence, df_confacc)
    hits <-
        as.numeric(coords(rocobj, "all", ret = c("tp")) / nrow(df_confacc))
    fp <-
        as.numeric(coords(rocobj, "all", ret = "fp") / nrow(df_confacc))
    confidence <- seq(1:length(fp))
    pauc = auc(rocobj, partial.auc = c(0, max(fp, na.rm = T)+.001))
    rocobj_plot_list <- list("hits"= hits, "fp"= fp, 
                             "confidence"= confidence, "pauc" = pauc)
}


# This function makes an ROC plot in ggplot, given a dataframe that
# has been processed by make_rocdata, i.e. with hits, fps, confidence
# where fps are false positives
# It could be called with a dataframe that contained those, and not
# nec through make_rocdata
# The function takes three arguments - a dataframe, and two logical
# arguments indicating whether a chance line and area polygon should
# be drawn
make_roc_gg <- function(rocobj_plot_list){
    rocobj_plot_df <- dplyr::data_frame(rocobj_plot_list$hits,
                                 rocobj_plot_list$fp,
                                 rocobj_plot_list$confidence)
    names(rocobj_plot_df) <- c("hits","fp","confidence")

    rocobj_plot_df %>% 
        slice(1:(nrow(rocobj_plot_df)  -1)) %>% 
        ggplot(aes(x = fp, y = hits)) + 
        geom_line(size = 1) +
        geom_point(shape = 21, color = "black", fill = "white", size = 3)+
        # scale_x_continuous(limits = c(1,0)) +
        # scale_y_continuous(limits = c(1,0)) +
        lims(x=c(0,.5)) +
        lims(y = c(0,max(rocobj_plot_df$hits) + .1*max(rocobj_plot_df$hits))) +
        theme_bw(base_size = 14) +
        labs(x = "False Positive %",
             y = "Hits %",
             title = "ROC curve, hits vs false positives, %",
             caption = "Points are confidence levels ")+
        geom_text_repel(aes(label = confidence), 
                        nudge_x = -.01, nudge_y = .02)  -> roc_plot_1
        roc_plot_1 <- roc_plot_1 + geom_abline(slope = 1, 
                                 intercept = 0, 
                                 linetype = 2)
        roc_plot_2 <- roc_plot_1 + geom_abline(slope = 1, 
                                 intercept = 0, 
                                 linetype = 2)
        roc_plot_2 <-
            roc_plot_2 + geom_line(data = rocobj_plot_df, aes(x = fp, y = hits))
        roc_plot_2 <-
            roc_plot_2 + geom_ribbon(aes(ymin = fp, ymax = hits), alpha = 0.2) +
        annotate("text", x = 0.2, y = 0.1, 
                 label = paste("pAUC = ",round(rocobj_plot_list$pauc,2)))
}


# This function is a user level function.  It chains the two roc functions
# together.  The user must pass a dataframe, with one column indicating
# confidence, and another accuracy, and these must be named
make_roc <- function(adf, chance_line = FALSE, auc_poly = FALSE){
    make_rocdata(adf) %>% 
        make_roc_gg() -> rocplot
    return(rocplot)
}
