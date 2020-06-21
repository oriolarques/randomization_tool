library(tidyverse)
library(gridExtra)
library(xlsx)

# Read measures file
# 9 x columns: initial_box, original_id, left_width, left_height, left_volume, right_width, right_height, right_volume, weight
measures <- read.csv('measures.csv', header = TRUE, sep = ';')
# Fix type of variables and commas for points as decimals
measures$initial_box <- as.factor(measures$initial_box)
for(i in seq_along(names(measures))){
    measures[,i] <- gsub(',', '.', measures[,i])
}
for(i in 3:9){
    measures[,i] <- as.numeric(measures[,i])
    
}

# Calculate the mean volume of both sides
measures$sides_mean <- (measures$left_volume + measures$right_volume)/2
# Calculate zscore of sides_mean
measures$zscore <- (measures$sides_mean - mean(measures$sides_mean))/sd(measures$sides_mean)

total.mice <- 20
n.groups <- 4
mice.grp <- total.mice / n.groups

#ggplot(measures, aes(x = box, y = left_volume ))+
#    geom_point(position = position_jitter(0.2)) +
#    stat_summary(fun = 'mean', geom = 'crossbar')

#ggplot(measures, aes(x = 1, y = zscore))+
#    geom_point(position = position_jitter(0.2)) +
#    stat_summary(fun = 'mean', geom = 'crossbar')


# Remove outliers: Order mice by absolute zscore values to be able to select 
# the first XX mice closest to the average ------------------------------------
measures <- measures %>% 
    arrange(abs(zscore)) %>% 
    slice_min(abs(zscore), n = total.mice)


#by(data = measures, INDICES = measures$box, FUN = function(x){sd(x$sides_mean)})

#prova <- lapply(1:100, function(x){measures[sample(nrow(measures)),]})

### Crear un a llista de 1000 permutacions dels data.frames -------------------
prova <- list()
prova2 <- list()
prova2_2 <- list()
prova2_3 <- list()
prova3 <- list()

for(i in 1:10000){
    prova[[i]] <- measures[sample(nrow(measures)),]
}

x <- list()
for(i in 1:4){
    x[[i]] <- prova[i][[1]]$sides_mean[(mice.grp*(i-1)+1):(mice.grp*i)]
}

for(i in 1:10000){
    for(j in 1:4){
        x[[j]] <- prova[i][[1]]$sides_mean[(mice.grp*(j-1)+1):(mice.grp*j)]
    }
    #prova2[i] <- sd(c(mean(prova[i][[1]][[11]][1:5]), #(1:20/4)
    #                  mean(prova[i][[1]][[11]][6:10]), #(20/4*1+1):(20/4*2)
    #                  mean(prova[i][[1]][[11]][11:15]),
    #                  mean(prova[i][[1]][[11]][16:20])))
    y <- lapply(x, function(x){mean(x)})
    prova2[i] <- sd(unlist(y))

}

for(i in 1:10000){
    for(j in 1:4){
        x[[j]] <- prova[i][[1]]$sides_mean[(mice.grp*(j-1)+1):(mice.grp*j)]
    }
    #prova2_2[i] <- sd(c(sd(prova[i][[1]][[11]][1:5]), 
    #                  sd(prova[i][[1]][[11]][6:10]), 
    #                  sd(prova[i][[1]][[11]][11:15]),
    #                  sd(prova[i][[1]][[11]][16:20])))
    z <- lapply(x, function(x){sd(x)})
    prova2_2[i] <- sd(unlist(z))

}

for(i in 1:10000){
    for(j in 1:4){
        x[[j]] <- prova[i][[1]]$weight[(mice.grp*(j-1)+1):(mice.grp*j)]
    }
    #prova2_2[i] <- sd(c(sd(prova[i][[1]][[11]][1:5]), 
    #                  sd(prova[i][[1]][[11]][6:10]), 
    #                  sd(prova[i][[1]][[11]][11:15]),
    #                  sd(prova[i][[1]][[11]][16:20])))
    w <- lapply(x, function(x){mean(x)})
    prova2_3[i] <- sd(unlist(w))
    
}

for(i in 1:10000){
    prova3[[i]] <- c(prova[[i]], means_sd = prova2[[i]], sd_sd = prova2_2[[i]], weight_sd = prova2_3[[i]])
}




#min_msd <- min(unlist(lapply(prova3, function(x) min(x$means_sd))))
#best_rand <- 0
# Find the index of which the randomization makes the minimum 
#for(i in 1:length(prova3)){
#    if (prova3[i][[1]][[13]] == min_msd){
#        print(i)
#        best_rand <- i
#        break
#    }
#}

# get the indexes of the top 5 randomization tables with less variations in the means
#order(unlist(lapply(prova3, function(x) min(x$means_sd))))[1:10]
#order(unlist(lapply(prova3, function(x) min(x$sd_sd))))[1:10]


best_rank <- order(unlist(lapply(prova3, function(x) min(x$means_sd))))[
                order(unlist(lapply(prova3, function(x) min(x$means_sd))))[1:1000] %in% 
                order(unlist(lapply(prova3, function(x) min(x$weight_sd))))[1:2000]][1:5]

# include the final box number to each mice ---------
prova_plot <- data.frame(prova[6852])
prova_plot$group_final <- c(rep('box1', 6), 
                            rep('box2', 6),
                            rep('box3', 6),
                            rep('box4', 6),
                            rep('box5', 6),
                            rep('box6', 6))




prova_plot<- pivot_longer(data =prova_plot[,c('group_final', 'left_volume', 'right_volume')], 
                          cols = - group_final, 
                          names_to = c('group'),
                          values_to = 'volume')

ggplot(prova_plot, aes(x = group_final, y = volume))+
    geom_violin()+
    geom_point(position = position_jitter(0.2)) +
    stat_summary(fun = 'mean', geom = 'crossbar')



## include the top 5 tables -------------------
final_plot_volume <- list()
final_plot_weight <- list()

for(i in 1:5){
    prova_plot <- data.frame(prova[best_rank[i]])
    
    # Assign box to the mice
    box_final <- 1
    count <- 0
    for(mice in seq_along(prova_plot$initial_box)){
        prova_plot$group_final[mice] <- paste('box', box_final)
        count <- count + 1
        if(count == mice.grp){
            box_final <- box_final + 1
            count <- 0
        }
    }
    #prova_plot$group_final <- c(rep('box1', 5), rep('box2', 5), rep('box3', 5),rep('box4', 5))
    
    #write.csv(prova_plot, file = paste('randomization',i,'.csv'))
    write.xlsx(prova_plot, file = 'randomization.xlsx', 
               sheetName = paste('rand',i),
               append = T)
    
    # select columns group_final, weight, left_volume, right_volume
    prova_plot<- pivot_longer(data = prova_plot[,c('group_final', 'weight', 'left_volume', 'right_volume')], 
                              cols = - c(group_final, weight), 
                              names_to = 'group',
                              values_to = 'volume')
    
    final_plot_volume[[i]] <- ggplot(prova_plot, aes(x = group_final, y = volume))+
        geom_violin()+
        geom_point(position = position_jitter(0.2)) +
        stat_summary(fun = 'mean', geom = 'crossbar')+
        ggtitle(paste('scenario',i, 'volumes'))+
        theme_bw()
    
    final_plot_weight[[i]] <- ggplot(prova_plot, aes(x = group_final, y = weight))+
        geom_point(col = 'orange', alpha = 0.6) +
        stat_summary(fun = 'mean', geom = 'crossbar')+
        ylim(c(15,25))+
        ggtitle(paste('scenario',i, 'weight'))+
        theme_bw()
}

final_plot_file <- c(rbind(final_plot_volume, final_plot_weight))
final_plot_file <- marrangeGrob(grobs = final_plot_file,
                                nrow = 2, ncol = 3)

ggsave(paste0('randomization_volumes.pdf'), final_plot_file)

