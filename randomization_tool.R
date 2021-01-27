library(tidyverse)
library(gridExtra)
library(xlsx)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### 1. Read the measures file  ------------------------------------------------
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

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


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### 2. Define the parameters --------------------------------------------------
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
total.mice <- 20
n.groups <- 4
mice.grp <- total.mice / n.groups
permut <- 10000

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### 3. Remove outliers ---------------------------------------------------------
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Remove outliers: Order mice by absolute zscore values to be able to select 
# the first XX mice closest to the average
measures <- measures %>% 
    arrange(abs(zscore)) %>% 
    slice_min(abs(zscore), n = total.mice)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### 4. Create distribution permutations of mice -------------------------------
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

### 4.1 Create the necessary lists --------------------------------------------
permutations_table <- list()
permutations_mean_sd <- list()
permutations_sd_sd <- list()
permutations_weight_sd <- list()
complete_permutations <- list()
prov_table <- list() 

# 4.2 Create a list with permutations of the tables ---------------------------
for(i in seq_len(permut)){
    permutations_table[[i]] <- measures[sample(nrow(measures)),]
}

# 4.3 Create a list with the standard deviation of the mean of each group ----- 
#      in each permutation
for(i in seq_len(permut)){
    for(j in 1:n.groups){
        prov_table[[j]] <- permutations_table[i][[1]]$sides_mean[(mice.grp*(j-1)+1):(mice.grp*j)]
    }
    prov_group.mean <- lapply(prov_table, function(prov_table){mean(prov_table)})
    permutations_mean_sd[i] <- sd(unlist(prov_group.mean))
}


# 4.4 Create a list with the standard deviation of the sd of the mean of  ----- 
#      each group in each permutation
for(i in seq_len(permut)){
    for(j in 1:n.groups){
        prov_table[[j]] <- permutations_table[i][[1]]$sides_mean[(mice.grp*(j-1)+1):(mice.grp*j)]
    }
    prov_group.sd <- lapply(prov_table, function(prov_table){sd(prov_table)})
    permutations_sd_sd[i] <- sd(unlist(prov_group.sd))

}

# 4.5 Create a list with the standard deviation of the weight of each  ----- 
#       group in each permutation
for(i in seq_len(permut)){
    for(j in 1:n.groups){
        prov_table[[j]] <- permutations_table[i][[1]]$weight[(mice.grp*(j-1)+1):(mice.grp*j)]
    }
    prova_group_weight <- lapply(prov_table, function(prov_table){mean(prov_table)})
    permutations_weight_sd[i] <- sd(unlist(prova_group_weight))
    
}

# 4.6 Join the permutations list with the standard deviation of the -----------
#       mean/sd/weight of each group in each permutation
for(i in seq_len(permut)){
    complete_permutations[[i]] <- c(permutations_table[[i]], 
                                    means_sd = permutations_mean_sd[[i]], 
                                    sd_sd = permutations_sd_sd[[i]], 
                                    weight_sd = permutations_weight_sd[[i]])
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### 5. Get the indexes of the best ranked distribution permutations -----------
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# get the indexes of the top 5 randomization tables with 
# less variations in the mean of tumour size and mice weight

best_rank <- order(unlist(lapply(complete_permutations, 
                                 function(x) min(x$means_sd))))[
                # select the top 1000 permutations with less mean_sd
                order(unlist(lapply(complete_permutations, 
                                    function(x) min(x$means_sd))))[1:1000] %in% 
                # among those 2000 permutations with less weight_sd
                order(unlist(lapply(complete_permutations, 
                                    function(x) min(x$weight_sd))))[1:2000]][1:5]


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### 6. Plot the best 5 distribution permutations ------------------------------
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# create one object to plot the distribution of tumour sizes
final_plot_volume <- list()
# create one object to plot the distribution of mice weight
final_plot_weight <- list()


for(i in 1:5){
    # for each of the best 5 distribution permutations: 
    permutations_plot <- data.frame(permutations_table[best_rank[i]])
    
    # Assign box to the mice
    box_final <- 1
    count <- 0
    # Starting by box 1
    for(mice in seq_along(permutations_plot$initial_box)){
        # assing the same box to the consecutive mice
        permutations_plot$group_final[mice] <- paste('box', box_final)
        count <- count + 1
        # when a group of mice is filled, go to the next box
        if(count == mice.grp){
            box_final <- box_final + 1
            count <- 0
        }
    }

# Create an excel file where each of the 5 permutations  ----------------------
#  is in a different sheet
    write.xlsx(permutations_plot, file = 'randomization.xlsx', 
               sheetName = paste('rand',i),
               append = T)
    
# Plot each of the 5 best distribution permutations ---------------------------    
    # select columns group_final, weight, left_volume, right_volume
    # and pivot to long format for plotting
    permutations_plot<- pivot_longer(data = permutations_plot[,c('group_final', 
                                                                 'weight', 
                                                                 'left_volume', 
                                                                 'right_volume')], 
                              cols = - c(group_final, weight), 
                              names_to = 'group',
                              values_to = 'volume')
    
    # Save the 5 plots of tumour sizes in an object
    final_plot_volume[[i]] <- ggplot(permutations_plot, aes(x = group_final, y = volume))+
        geom_violin()+
        geom_point(position = position_jitter(0.2)) +
        stat_summary(fun = 'mean', geom = 'crossbar')+
        ggtitle(paste('scenario',i, 'volumes'))+
        theme_bw()
    
    # Save the 5 plots of mice weights in an object
    final_plot_weight[[i]] <- ggplot(permutations_plot, aes(x = group_final, y = weight))+
        geom_point(col = 'orange', alpha = 0.6) +
        stat_summary(fun = 'mean', geom = 'crossbar')+
        ylim(c(15,25))+
        ggtitle(paste('scenario',i, 'weight'))+
        theme_bw()
}

# Plot
final_plot_file <- c(rbind(final_plot_volume, final_plot_weight))
final_plot_file <- marrangeGrob(grobs = final_plot_file,
                                nrow = 2, ncol = 3)
ggsave(paste0('randomization_volumes.pdf'), final_plot_file)

