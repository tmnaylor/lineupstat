# Some rough code for ROC functions

library(pacman)
p_load(tidyverse, magrittr, pROC, plotROC, ggrepel)

# function just to reconstruct data for sample
# First make a function that repeats a number k times
rep_index <- function(index,num){
    rep(index, num)
}


# sample data from Mickes, Wixted et al.
df <- readxl::read_excel("wixdata.xlsx") 
conf_hits  = rep_index(df$confidence,df$hits) 
conf_false = rep_index(df$confidence,df$falsepos)
conf_hits_bin = rep(1,length(conf_hits))
conf_falsepos_bin = rep(0,length(conf_false))
dfhits <- data.frame(conf_hits,conf_hits_bin)
names(dfhits) <-  c("confidence","acc")
dffalse <- data.frame(conf_false,conf_falsepos_bin)
names(dffalse) <-  c("confidence","acc")
na_conf <- rep(3, 33)
na_acc <- rep(3, 33)
df_na <- data.frame(na_conf, na_acc)
names(df_na) <-  c("confidence","acc")
dftot <- rbind(dfhits, dffalse, df_na)
write_csv(dftot,"Mickes_Wixted_seqdata.csv")

# Won't need all that stuff above, assume df will appear as dftot above

# Now create roc object; suggest function to change dftot appearing df
# to the one for the roc plot below
dftot_roc <- roc(acc ~ confidence, dftot)
hits <- as.numeric(coords(dftot_roc, "all", ret=c("tp"))/nrow(dftot))
fp <- as.numeric(coords(dftot_roc, "all", ret="fp")/nrow(dftot))
confidence <- seq(1:length(fp))
zeroline <- c(0,seq(0,1,by = 1/(length(fp)-2)))
dftot_roc_plot <- data.frame(hits,fp,confidence, zeroline)


# Now we creat the plot; note that the section below implements argument
# passing, in principle (add to plot according to arguments)
# But not sure how we would add grouping variable, typical use in the EW
# literature.  Can pROC handle this as it stands?
dftot_roc_plot %>% 
    slice(1:(nrow(dftot_roc_plot)  -1)) %>% 
    ggplot(aes(x = fp, y = hits)) + 
        geom_line(size = 1) +
        geom_point(shape = 21, color = "black", fill = "white", size = 3)+
        # scale_x_continuous(limits = c(1,0)) +
        # scale_y_continuous(limits = c(1,0)) +
        lims(x=c(0,.5)) +
        lims(y = c(0,max(dftot_roc_plot$hits) + .1*max(dftot_roc_plot$hits))) +
        theme_bw(base_size = 14) +
        labs(x = "False Positive %",
             y = "Hits %",
             title = "ROC curve, hits vs false positives, %",
             caption = "Points are confidence levels ")+
        geom_text_repel(aes(label = confidence), 
                        nudge_x = -.01, nudge_y = .02)  -> roc_plot_1


# Section below can be rewritten to augment plot above according
# to arguments passed by users
roc_plot_1 + geom_abline(slope = 1, intercept = 0, linetype = 2) -> roc_plot_2
roc_plot_2 + geom_line(data = dftot_roc_plot, aes(x = fp, y = hits))
roc_plot_2 + geom_ribbon(aes(ymin = fp, ymax = hits), alpha = 0.2)



