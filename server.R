# Define server logic to read selected file ----
####
# shyny app for rythm metrics computing and display
####
library("shiny")
#library("readtextgrid")
library("AcousticNDLCodeR")
#library("ggplot2")
#library("dplyr")
library("gridExtra")
library("factoextra")
library("FactoMineR")
library("ggpubr")
library("tidyverse")
library("factoextra")
library(cluster)
library(stats)
library("viridis")
library("ggrepel")
library(ggfortify)


options(shiny.maxRequestSize=30*1024^2) 

server <- function(input, output) {
  #options(shiny.maxRequestSize=30*1024^2) 
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. 
    
    req(input$file1)
    #req(input$outliers)
    
    #req(input$groups)
    #remove_outliers = 1
    #if(is.null(input$groups)) return (1)

    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        #df <- read_textgrid(input$file1$datapath[1])
        nFiles<- length(input$file1$datapath)
        files<- input$file1$datapath
        print(files)
        
        have_groups = 0
        if(!is.null(input$groups)) {
          ext <- tools::file_ext(input$groups$datapath)
          validate(need(ext == "xlsx", "Please upload an Excel file"))
          groups <- readxl::read_excel(input$groups$datapath)
          colnames(groups) <- c("file", "group")
          groups$file <- gsub("\\.TextGrid$", "", groups$file)
          have_groups = 1
        }
        
        df<- data.frame("file"= c(1:length(files)),"speechRate"=c(1:length(files)),"PerV"=c(1:length(files)),"PerC"=c(1:length(files)),"VarcoV"=c(1:length(files)),"VarcoC"=c(1:length(files)),"DeltaV"=c(1:length(files)),"DeltaC"=c(1:length(files)),"VrPVI"=c(1:length(files)), "CrPVI"=c(1:length(files)),"VnPVI"=c(1:length(files)),"speechTimeSecs"=c(1:length(files)),"N sounds"=c(1:length(files)) )
        allC <- data.frame("file"=character(0),"durationC"=numeric(0))
        allV <- data.frame("file"=character(0),"durationA"=numeric(0))
        
       
          
          
        
        
        loopIndex<- 0
        for (loopIndex in 1:nFiles){
  
          filetoRead <- files[loopIndex]
          #filetoRead = "/Users/weg/Library/CloudStorage/OneDrive-UniversitatdeBarcelona/git-me/rythm_analysis/testFiles/test.TextGrid"
          #filetoRead = "C:/Users/labfonub99/OneDrive - Universitat de Barcelona/git-me/rythm_analysis/textgrid de praat/cat_Arenys_de_Mar.TextGrid"
          #print(filetoRead)
          
          encoding = readr::guess_encoding(filetoRead) %>% filter(confidence == 1) %>% purrr::pluck("encoding")
       
          fileName<- input$file1$name[loopIndex]
          # phonetic
          if (input$annotation == 1){
            textgrid <- readTextGridRobust(filetoRead, encoding)[[{input$tier}+1]]
            textgrid <- as.data.frame(textgrid)
            
            textgrid$Outcomes <- gsub( "<p:>", "", textgrid$Outcomes)
            textgrid$Outcomes <- gsub( "[ ˈˈ̬̞̝̪̟̠̥̃]", "", textgrid$Outcomes)
            textgrid$Outcomes <- gsub( "[^aeoiujwaəεɑɔ]", "C", textgrid$Outcomes)
            textgrid$Outcomes <- gsub( "[aeoiujwəεɑɔ]", "V", textgrid$Outcomes)
            # orthographic
          } else if (input$annotation == 2){
            textgrid <- readTextGridRobust(filetoRead, encoding)[[{input$tier}+1]]
            textgrid <- as.data.frame(textgrid)
            
            textgrid$Outcomes <- gsub( "[h´,.;:]", "", textgrid$Outcomes)
            textgrid$Outcomes <- gsub( "[^aeioujw]", "C", textgrid$Outcomes)
            textgrid$Outcomes <- gsub( "[aeioujw]", "V", textgrid$Outcomes)
          } else if (input$annotation == 3){
            textgrid <- readTextGridRobust(filetoRead, encoding)[[{input$tier}+1]]
            textgrid <- as.data.frame(textgrid)
            
            textgrid$Outcomes <- gsub( "c", "C", textgrid$Outcomes)
            textgrid$Outcomes <- gsub( "v", "V", textgrid$Outcomes)
          } else if (input$annotation == 4){
            textgrid <- readTextGridRobust(filetoRead, encoding)[[{input$tier}+1]]
            textgrid <- as.data.frame(textgrid)
            
          textgrid$Outcomes[textgrid$Outcomes=="PTK"] <- "C"
          textgrid$Outcomes[textgrid$Outcomes=="a"] <- "V"
        } else if (input$annotation == 5){
          #this is SAMPA
          textgrid <- readTextGridRobust(filetoRead, encoding)[[{input$tier}+1]]
          textgrid <- as.data.frame(textgrid)
          textgrid$Outcomes <- gsub( "[´,.;:']", "", textgrid$Outcomes)
          textgrid$Outcomes <- gsub( "<p:>", "", textgrid$Outcomes)
          textgrid$Outcomes <- gsub( "rr", "r", textgrid$Outcomes)
          textgrid$Outcomes <- gsub( "jj", "L", textgrid$Outcomes)
          textgrid$Outcomes <- gsub( "J", "ñ", textgrid$Outcomes)
          
          textgrid$Outcomes <- gsub( "[^{6QAE3@IO29&U}VYaeiou]", "C", textgrid$Outcomes)
          textgrid$Outcomes <- gsub( "[{6QAE3@IO29&U}VYaeioujw]", "V", textgrid$Outcomes)
          
          textgrid$Outcomes[textgrid$Outcomes=="a"] <- "V"
        }
        
          
          #computes dur in ms for comparison purposes with other metrics (Arvaniti 2011)
          textgrid$duration<-(textgrid$end-textgrid$start)*1000
          textGridNonSilent<-textgrid[textgrid$Outcomes=="C" | textgrid$Outcomes=="V",]
          
          if (input$outliers == 1){
            
            remove_outliers <- function(data, col_name) {
              # Ensure column name is treated correctly
              col <- enquo(col_name)
              
              # Filter out negative values
              data <- data %>% filter(!!col >= 0)
              
              # Calculate interquartile range (IQR)
              Q1 <- quantile(data[[quo_name(col)]], 0.25, na.rm = TRUE)
              Q3 <- quantile(data[[quo_name(col)]], 0.75, na.rm = TRUE)
              IQR <- Q3 - Q1
              
              # Determine lower and upper bounds for outliers
              #lower_bound <- Q1 - 1.5 * IQR
              lower_bound = 0
              upper_bound <- Q3 + 1.5 * IQR
              
              # Filter rows within the acceptable range
              data <- data %>% filter(!!col >= lower_bound & !!col <= upper_bound)
              
              return(data)
            }
           
              
              
            textGridNonSilent = textGridNonSilent %>% remove_outliers(duration)
              
         
          }
          
          
          
          # si no tiene Ci V skip file.
          #unique((textGridNonSilent$Outcomes))
          
          
          speechTime<-sum(textGridNonSilent$duration)
          
          durCs <-textgrid[textgrid$Outcomes=="C",]$duration
          durVs <-textgrid[textgrid$Outcomes=="V",]$duration
          
          dataFileC <- data.frame(rep(fileName, length(durCs)),durCs)
          dataFileV <- data.frame(rep(fileName, length(durVs)),durVs)
          allC<- rbind (dataFileC,allC)
          allV <-rbind (allV,dataFileV)
          
          consonantTime= sum(durCs)
          vowelTime= sum(durVs)
          frequ= table(textgrid$Outcomes)
          nCons =unname(frequ[names(frequ)=="C"])
          nVows =unname(frequ[names(frequ)=="V"])
          speechRate= (nCons+nVows)/(speechTime/1000)
          PercentageV = (vowelTime/speechTime)*100
          PercentageC = (consonantTime/speechTime)*100
          deltaC <- sd(textgrid[textgrid$Outcomes=="C",]$duration)
          deltaV <- sd(textgrid[textgrid$Outcomes=="V",]$duration)
          
          #VarcoC=100*DC/meanC (Dellwo 2006)
          varcoC=100*deltaC/mean(textgrid[textgrid$Outcomes=="C",]$duration)
          varcoV= 100*deltaV/mean(textgrid[textgrid$Outcomes=="V",]$duration)
          
          
          df[loopIndex,"file" ]<- fileName
          df[loopIndex,"speechRate"]<- speechRate
          df[loopIndex,"PerV"]<- PercentageV
          df[loopIndex,"PerC"]<- PercentageC
          df[loopIndex,5]<- varcoV
          df[loopIndex,6]<- varcoC
          df[loopIndex,7]<- deltaV
          df[loopIndex,8]<- deltaC
          df[loopIndex,12]<- speechTime/1000
          df[loopIndex,13]<- nCons+nVows
          
          
          ##################
          # compute the rPVI  FOR VOWELS
          ##################
          
          myA<- which(textgrid$Outcomes=="V" )
          myC<- which(textgrid$Outcomes=="C")
          # Check if all vowel intervals have a C afterwards
          expectedC<- myA+1
          checkingC <- expectedC %in% myC
          haveCafter= data.frame(myA,checkingC)
          myA<- haveCafter$myA[haveCafter$checking==TRUE]
          
          indexA <-0
          difsA <- rep(NA, length(myA))
          for (A in myA){
            indexA=indexA+1
            difsA[indexA] <- abs(textgrid$duration[A]-textgrid$duration[A+1])
          }
          #difsCA ready for further analisys compute
          # rPVI
          VrPVI<- mean(difsA)
          df[loopIndex,9]<- VrPVI
          
          # compute nPVI 
          # It computes the difference between the duration of each vocalic interval 
          # and the one the follows then divides it by the average duration of all vocalic intervals. 
          # The mean of the values obtained is computed and finally multiplied by 100. 
          denom<- mean(textgrid$duration[textgrid$Outcomes=="V"] )
          VnPVI<- 100*(mean(difsA/denom))
          df[loopIndex,11]<- VnPVI
          
          ## compute the rPVI  FOR CONSONANTS
          # Check if all C intervals have a vowel afterwards
          expectedA<- myC+1
          checkingA <- expectedA %in% myA
          haveAafter= data.frame(myC,checkingA)
          myC<- haveAafter$myC[haveAafter$checking==TRUE]
          
          indexC <-0
          difsC <- rep(NA, length(myC))
          for (C in myC){
            indexC=indexC+1
            difsC[indexC] <- abs(textgrid$duration[C]-textgrid$duration[C+1])
          }
          
          #difsC ready for further analisys
          CrPVI<- mean(difsC)
          df[loopIndex,10]<- CrPVI
          
        }
        
        #df creado y completo
        # df
        
        df$file <- gsub("\\.TextGrid$", "", df$file)
        
        df <- as.data.frame(df)
        df$file<- as.factor(df$file)
        
        if (have_groups == 1){
          df_groups= merge(df, groups, by = "file", all = FALSE)
        }
        
        
        
        output$downloadData <- downloadHandler(
          filename = function() {
            paste("data-rhythm-metrics-", Sys.Date(), ".csv", sep = "")
          },
          content = function(file) {
            write.csv(df, file, row.names = FALSE)
          }
        )
        
        df_vowel <- allV %>%
          rename(file = 1) %>%
          rename(duration = durVs) %>%
          mutate(type = "vowel")
        
        df_consonant <- allC %>%
          rename(file = 1) %>%
          rename(duration = durCs) %>%
          mutate(type = "consonant")
        
        # Combine the two datasets
        combined <- bind_rows(df_vowel, df_consonant)
        rm(df_vowel, df_consonant)
        
        
        output$downloadDurations <- downloadHandler(
          filename = function() {
            paste("data-durations-", Sys.Date(), ".csv", sep = "")
          },
          content = function(file) {
            write.csv(combined, file, row.names = FALSE)
          }
        )
        
        
        # allC
        # allV
        
        datos= data.frame( df[,4:11], row.names =df$file )
        
       # res.pca <- PCA(datos, ncp = 3, graph = FALSE)
        #maxClus=length(datos/2)
        
       
        
      
        
        
        if (nFiles>2){
          
          # Standardize the data (optional, if variables have different scales)
          datosNum_scaled <- scale(datos)
          
          # Compute the distance matrix
          dist_matrix <- dist(datosNum_scaled)
          

          find_optimal_clusters <- function(data) {
            silhouette_sums <- numeric()
            silhouette_sums <- c(silhouette_sums, NA) # NA for the first index
            maxGrups <- nrow(data) - 1
            
            # Iterate through potential numbers of clusters
            for (nGroups in 2:maxGrups) {
              # Perform PAM clustering
              pam_result <- pam(data, nGroups)
              
              # Compute silhouette values
              silhouette_values <- silhouette(pam_result)
              sum_silhouette <- sum(silhouette_values[, 3])
              
              # Store the sum of silhouette widths
              silhouette_sums <- c(silhouette_sums, sum_silhouette)
            }
            
            # Determine the optimal number of clusters
            ncluster <- which.max(silhouette_sums)
            
            # Ensure a minimum of 2 clusters
            if (ncluster < 2) {
              ncluster <- 2
            }
            
            return(ncluster)
          }
         
          
          ncluster = find_optimal_clusters(datosNum_scaled)
          
          mds <- dist_matrix %>%
            cmdscale(eig=TRUE) 
          
          mds_data = as.data.frame(mds$points)
          rownames(mds_data) <- df$file
          mds_data$file= as.factor(df$file)

          if (have_groups==1){
            mds_data$group= as.factor(df_groups$group)
            
          }
          
          

          # Plot the MDS result
          mds_plot <- ggplot(mds_data, aes(x = V1, y = V2, color = file, label = file)) +
            geom_point() +
            geom_text(vjust = -0.5, hjust = 0.5) + # Adjust positioning of labels
            theme_light() +
            scale_color_viridis(discrete = TRUE) +
            theme(legend.position = "none") + # Remove legend
            ggtitle("MDS") # Add title

          
        
          
          
          if (have_groups==0){
            output$plot8_mds <- renderPlot({mds_plot})
            } else {
              centroids <- mds_data %>%
                group_by(group) %>%
                summarize(V1 = mean(V1), V2 = mean(V2), .groups = "drop")
              
              
              mds_group <- ggplot(mds_data, aes(x = V1, y = V2, color = group)) +
                geom_point(size=3, alpha = 0.4) + # Plot individual points
                stat_ellipse(geom = "polygon",
                             aes(fill = group), 
                             alpha = 0.1, linetype = 2) + # Ellipses
                geom_text(data = centroids, aes(x = V1, y = V2, label = group),
                          vjust = -0.5, hjust = 0.5, size = 4) + # Group labels at centroids
                theme_light() +
                theme(legend.position = "none") + # Remove legend
                ggtitle("MDS with groups") # Add title
              

              # Plot the centroids
              mds_centroid<- ggplot(centroids, aes(x = V1, y = V2, color = group)) +
                geom_point(size = 3) + # Centroid points
                # stat_ellipse(data = mds_data, aes(x = V1, y = V2, color = Lengua, fill = Lengua), geom = "polygon", alpha = 0.25, linetype = 2) + # Ellipses
                geom_text(aes(label = group), vjust = -0.5, hjust = 0.5, size = 4) + # Labels for centroids
                theme_light() +
                theme(legend.position = "none") + # Remove legend
                ggtitle("Centroids of Groups in MDS")
              
              
              output$plot8_mds <- renderPlot({
                grid.arrange(mds_plot, mds_group, mds_centroid, ncol=3)
                
              }) 

            }
          
          
          
          # Perform hierarchical clustering
          hc <- hclust(dist_matrix, method = "complete")
          #hc$labels <- gsub(".TextGrid", "", hc$labels)
          
          dendo= fviz_dend(hc, 
                    k = ncluster, 
                    cex = 0.7,                     # Label size
                    palette = "jco",               # Color palette see ?ggpubr::ggpar
                    rect = TRUE, 
                    horiz = TRUE,     # Horizontal orientation
                    color_labels_by_k = TRUE, # Add rectangle around groups
                    rect_border = "jco",           # Rectangle color
                    labels_track_height = 0.8)+
            labs(title = "Hierarchical Clustering", caption = "HC based on Euclidean distances method complete, optimal groups based on silouette values.") + 
            theme(plot.title = element_text(hjust = 0.5, size = 14))+
            theme(legend.position = "none") # Remove legend
          
         

          
          # dendro by grup:
          if (have_groups==1){
            
            df_by_group <- df_groups %>% 
              group_by(group) %>% 
              summarise(across(where(is.numeric), median, na.rm = TRUE))
              
            by_group_onlyNum = data.frame( df_by_group[,4:11], row.names =df_by_group$group )
            datosNumgr_scaled <- scale(by_group_onlyNum)
            dist_matrix <- dist(datosNumgr_scaled)
            
            ncluster_gr = find_optimal_clusters(datosNumgr_scaled)
            
             hc <- hclust(dist_matrix, method = "complete")
             
             
            dend_2 <- fviz_dend(hc, 
                                k = ncluster_gr, 
                                cex = 0.9,          # Smaller labels
                                horiz = TRUE, 
                                rect = TRUE, 
                                show_labels = TRUE) + 
              ggtitle("Dendrogram by median of groups") + 
              labs(caption = "HC based on Euclidean distances method complete, optimal groups based on silouette values.") + 
              theme(plot.title = element_text(hjust = 0.5, size = 14))
          }
         
          
          
          if (have_groups==1){
            output$plot6_dengrogram <- renderPlot({grid.arrange(dendo, dend_2, ncol=2)})
            
          }else{
            
            output$plot6_dengrogram <- renderPlot({dendo})
            
          }
          
           
          # Perform PCA with prcomp
          pca_result <- prcomp(datosNum_scaled)

          pca_data <- data.frame(PC1 = pca_result$x[, 1],
                                 PC2 = pca_result$x[, 2])
          
          kmeans_result <- kmeans(pca_data,centers = ncluster)
          
        
          
          if (have_groups == 1) {
            pca_data <- data.frame(PC1 = pca_result$x[, 1],
                                   PC2 = pca_result$x[, 2],
                                   file= as.factor(df$file),
                                   Group = as.factor(df_groups$group))
            
            centroids <- pca_data %>%
              group_by(Group) %>%
              summarize(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
            
          } else {
            pca_data <- data.frame(PC1 = pca_result$x[, 1],
                                   PC2 = pca_result$x[, 2],
                                   file = as.factor(df$file))
          }
          
          pca_data$Cluster <- as.factor(kmeans_result$cluster)
          
          pca_file<-  ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
            geom_point(size = 3) +
            geom_text(aes(label = file), size = 3) +
            scale_color_viridis(discrete = TRUE) +
            labs(title = "PCA groups based on k-means",
                 x = "PC1",
                 y = "PC2") +
            theme_minimal()+
            theme(legend.position = "none") # Remove legend
          
          
          loadings= autoplot(pca_result,
                   data = NULL,                # Exclude data points
                   loadings = TRUE,            # Show loadings
                   loadings.colour = 'lightblue', # Set color for loadings
                   loadings.label = TRUE,      # Display labels for loadings
                   loadings.label.size = 3) +  # Set label size
            theme_light() +                    # Use a light theme
            labs(title = "PCA Loadings Plot",
                 x = "PC1",
                 y = "PC2") +
            theme(legend.position = "none")    # Remove legend if it shows
          
          if (have_groups==0){
            output$plot7_pca <- renderPlot({grid.arrange(pca_file, loadings, ncol=2)})
            
          }else{
            pca_group<- ggplot(pca_data, aes(x = PC1, y = PC2, color = Group)) +
              geom_point(size = 3, alpha=0.1) +
              stat_ellipse(geom = "polygon",
                           aes(fill = Group), 
                           alpha = 0.1, linetype = 2) + # Ellipses
              geom_text(data = centroids, aes(x = PC1, y = PC2, label = Group),
                        vjust = -0.5, hjust = 0.5, size = 4) + # Group labels at centroids
              labs(title = "PCA Plot",
                   x = "PC1",
                   y = "PC2") +
              theme_minimal()+  theme(legend.position = "none") # Remove legend
            
            
            output$plot7_pca <- renderPlot({
              grid.arrange(pca_file, pca_group,loadings, ncol=3)
            }) 
          }
          
          
        # end of graphs that need more than 2 files
        }
        
        
        
        names(allC)[1] <- "file"
        names(allV)[1] <- "file"
        allC$file <- as.character(allC$file)
        allV$file <- as.character(allV$file)
        
        allC$file <- substr(allC$file,1,nchar(allC$file)-9)
        allV$file <- substr(allV$file,1,nchar(allV$file)-9)
        
        allC$file <- as.factor(allC$file)
        allV$file <- as.factor(allV$file)
        
        p1<- ggplot(allC, aes(x=file, y=durCs,colour = file)) +
          geom_violin()+
          geom_boxplot(width=0.2, fill="white")+
          ylab("(ms)")+xlab("File")+
          theme_minimal()+ theme(axis.text.x = element_text(angle = 90))+theme(legend.position = "none")+
          ggtitle("Consonants Duration")
        
        p2<- ggplot(allV, aes(x=file, y=durVs,colour = file)) +
          geom_violin()+
          geom_boxplot(width=0.2, fill="white")+
          ylab("(ms)")+xlab("File")+
          theme_minimal()+ theme(axis.text.x = element_text(angle = 90))+   theme(legend.position = "none")+
          ggtitle("Vowels Duration")
        
        output$plot1 <- renderPlot({
          grid.arrange(p1, p2, ncol=2)
          
        }) 
        
        # classical graphs
        
        crPVI_VnPVI= ggplot(data = df,aes(x = CrPVI, y = VnPVI, label=file, colour = pca_data$Cluster)) +
          geom_point()+
          geom_label_repel(box.padding = 0.5) + # Add labels to points
          theme_minimal()+ theme(legend.position = "none") + labs(subtitle="Consonantal raw Pairwise Variability Index (PVI) by Vocalic normalized PVI", 
                                y="VnPVI", 
                                x="CrPVI", 
                                title="CrPVI by VnPVI", 
                                caption = "Metric Grabe & Low (2002)")

        
        if (have_groups==1) {
          summary_df <- df_groups %>%
            group_by(group) %>%
            summarise(
              mean_CrPVI = mean(CrPVI, na.rm = TRUE),        
              mean_VnPVI = mean(VnPVI, na.rm = TRUE),    
              sd_CrPVI = sd(CrPVI, na.rm = TRUE),            
              sd_VnPVI = sd(VnPVI, na.rm = TRUE)         
            )
          
          crPVI_VnPVI_group = ggplot(data = summary_df, aes(x = mean_CrPVI, y = mean_VnPVI, colour = group, label = group)) +
            geom_point(size = 4, shape = 15) + 
            geom_label_repel(box.padding = 0.5) + # Add labels to points
            geom_errorbar(aes(ymin = mean_VnPVI - sd_VnPVI, ymax = mean_VnPVI + sd_VnPVI), width = 0.1, colour = "grey") +  # Vertical error bars
            geom_errorbarh(aes(xmin = mean_CrPVI - sd_CrPVI, xmax = mean_CrPVI + sd_CrPVI), height = 0.1, colour = "grey") +       # Horizontal error bars
            theme_minimal() + theme(legend.position = "none") +# Remove legend
            labs(subtitle = "Consonantal raw Pairwise Variability Index (PVI) by Vocalic normalized PVI", 
                 y = "VnPVI", 
                 x = "CrPVI", 
                 title = "VnPVI/CrPVI", 
                 caption = "Metrics Grabe & Low (2002).")
        }
        
        
        if (have_groups==1){
          output$plot5 <- renderPlot({
            grid.arrange(crPVI_VnPVI, crPVI_VnPVI_group, ncol=2)
          }) 
        }else{
          output$plot5 <- renderPlot({
            crPVI_VnPVI
          }) 
        }
        
       
    
        varcoV_PerV= ggplot(data = df, aes(x = VarcoV, y = PerV, label=file, colour = pca_data$Cluster)) +
            geom_point()+
            geom_label_repel(box.padding = 0.5) + # Add labels to points
            #geom_errorbar(aes(ymin = df.summary.VV$ymin, ymax = df.summary.VV$ymax))+
            #geom_errorbarh(aes(xmin = df.summary.PV$ymin,xmax = df.summary.PV$ymax))+
            theme_minimal()+ theme(legend.position = "none")+ labs(subtitle="Normalized standard deviation of consonants by percentage of vowel",
                 y="%V", 
                 x="v∆V", 
                 title="v∆V by %V", 
                 caption = "Metric by Dellwo (2006).")
  
        
        
        if (have_groups==1) {
          
          summary_df <- df_groups %>%
            group_by(group) %>%
            summarise(
              mean_VarcoV = mean(VarcoV, na.rm = TRUE),        
              mean_PerV = mean(PerV, na.rm = TRUE),    
              sd_VarcoV = sd(VarcoV, na.rm = TRUE),            
              sd_PerV = sd(PerV, na.rm = TRUE)         
            )
          
          varcoV_PerV_group = ggplot(data = summary_df, aes(x = mean_VarcoV, y = mean_PerV, colour = group, label = group)) +
            geom_point(size = 4, shape = 15) + 
            geom_label_repel(box.padding = 0.5) + # Add labels to points
            geom_errorbar(aes(ymin = mean_PerV - sd_PerV, ymax = mean_PerV + sd_PerV), width = 0.1, colour = "grey") +  # Vertical error bars
            geom_errorbarh(aes(xmin = mean_VarcoV - sd_VarcoV, xmax = mean_VarcoV + sd_VarcoV), height = 0.1, colour = "grey") +       # Horizontal error bars
            theme_minimal() + theme(legend.position = "none") +# Remove legend
            labs(subtitle = "Normalized standard deviation of consonants by percentage of vowel", 
                 x = "v∆V", 
                 y = "%V", 
                 title = "v∆V by %V", 
                 caption = "Metric by Dellwo (2006)")
          
        }
        
        
        if (have_groups==1){
          output$plot2 <- renderPlot({
            grid.arrange(varcoV_PerV, varcoV_PerV_group, ncol=2)
          }) 
        }else{
          output$plot2 <- renderPlot({
            varcoV_PerV
          }) 
        }
        
        
        
        varcos= ggplot(data = df, aes(x = VarcoV, y = VarcoC, label=file, colour = pca_data$Cluster)) +
          geom_point()+
          geom_label_repel(box.padding = 0.5) + # Add labels to points
          #geom_errorbar(aes(ymin = df.summary.VV$ymin, ymax = df.summary.VV$ymax))+
          #geom_errorbarh(aes(xmin = df.summary.PV$ymin,xmax = df.summary.PV$ymax))+
          theme_minimal()+ theme(legend.position = "none")+ labs(subtitle="Normalized standard deviation of durations",
                                                                 y="v∆C", 
                                                                 x="v", 
                                                                 title="v∆V by v∆C", 
                                                                 caption = "Metric by Dellwo (2006).")
        
        
        if (have_groups==1) {
          
          summary_df <- df_groups %>%
            group_by(group) %>%
            summarise(
              mean_VarcoV = mean(VarcoV, na.rm = TRUE),        
              mean_VarcoC = mean(VarcoC, na.rm = TRUE),    
              sd_VarcoV = sd(VarcoV, na.rm = TRUE),            
              sd_VarcoC = sd(VarcoC, na.rm = TRUE)         
            )
          
          varcos_group = ggplot(data = summary_df, aes(x = mean_VarcoV, y = mean_VarcoC, colour = group, label = group)) +
            geom_point(size = 4, shape = 15) + 
            geom_label_repel(box.padding = 0.5) + # Add labels to points
            geom_errorbar(aes(ymin = mean_VarcoC - sd_VarcoC, ymax = mean_VarcoC + sd_VarcoC), width = 0.1, colour = "grey") +  # Vertical error bars
            geom_errorbarh(aes(xmin = mean_VarcoV - sd_VarcoV, xmax = mean_VarcoV + sd_VarcoV), height = 0.1, colour = "grey") +       # Horizontal error bars
            theme_minimal() + theme(legend.position = "none") +# Remove legend
            labs(subtitle = "Normalized standard deviation of durations", 
                 x = "v∆V", 
                 y = "v∆C", 
                 title = "v∆V/v∆C", 
                 caption = "Metric by Dellwo (2006)")
          
        }
        
        
        if (have_groups==1){
          output$plot_varcos <- renderPlot({
            grid.arrange(varcos, varcos_group, ncol=2)
          }) 
        }else{
          output$plot_varcos <- renderPlot({
            varcos
          }) 
        }
        
  ##### perV_delta
        perV_delta = ggplot(data = df, aes(x = PerV, y = DeltaC, colour = pca_data$Cluster, label =file)) +
          geom_point()+
          geom_label_repel(box.padding = 0.5) + # Add labels to points
          theme_minimal()+   theme(legend.position = "none") + # Remove the legend
          labs(subtitle="%V/∆C", 
                                y="∆C", 
                                x="%V", 
                                title="%V/∆C", 
                                caption = "GNU. ")
        
        
        
        if (have_groups==1) {
          
          summary_df <- df_groups %>%
            group_by(group) %>%
            summarise(
              mean_PerV = mean(PerV, na.rm = TRUE),        # Mean of %V
              mean_DeltaC = mean(DeltaC, na.rm = TRUE),    # Mean of ∆C
              sd_PerV = sd(PerV, na.rm = TRUE),            # Standard deviation of %V
              sd_DeltaC = sd(DeltaC, na.rm = TRUE)         # Standard deviation of ∆C
            )
          
          perV_delta_group = ggplot(data = summary_df, aes(x = mean_PerV, y = mean_DeltaC, colour = group, label = group)) +
            geom_point(size = 4, shape = 15) + 
            geom_label_repel(box.padding = 0.5) + # Add labels to points
            geom_errorbar(aes(ymin = mean_DeltaC - sd_DeltaC, ymax = mean_DeltaC + sd_DeltaC), width = 0.1, colour = "grey") +  # Vertical error bars
            geom_errorbarh(aes(xmin = mean_PerV - sd_PerV, xmax = mean_PerV + sd_PerV), height = 0.1, colour = "grey") +       # Horizontal error bars
            theme_minimal() + theme(legend.position = "none") +# Remove legend
            labs(subtitle = "%V/∆C", 
              y = "∆C", 
              x = "%V", 
              title = "%V/∆C", 
              caption = "GNU.")
          
        }
        
        
        if (have_groups==1){
          output$plot3 <- renderPlot({
            grid.arrange(perV_delta, perV_delta_group, ncol=2)
          }) 
        }else{
          output$plot3 <- renderPlot({
            perV_delta
          }) 
        }
          
          
          
        
       
        
        # 4 deltas
        deltas =  ggplot(data = df,aes(x = DeltaV, y = DeltaC, label=file, colour = pca_data$Cluster)) +
          geom_point()+
          geom_label_repel(box.padding = 0.5) + # Add labels to points
          theme_minimal()+theme(legend.position = "none")+labs(subtitle="Standard deviation of the durations", 
                                                               y="∆C", 
                                                               x="∆V",  
                                                               title="∆V/∆C", 
                                                               caption = "Metric by Ramus, Nespor & Mehler (1999)")

        
        if (have_groups==1) {
          summary_df <- df_groups %>%
            group_by(group) %>%
            summarise(
              mean_DeltaV = mean(DeltaV, na.rm = TRUE),        
              mean_DeltaC = mean(DeltaC, na.rm = TRUE),    
              sd_DeltaV = sd(DeltaV, na.rm = TRUE),            
              sd_DeltaC = sd(DeltaC, na.rm = TRUE)         
            )
          
          deltas_group = ggplot(data = summary_df, aes(x = mean_DeltaV, y = mean_DeltaC, colour = group, label = group)) +
            geom_point(size = 4, shape = 15) + 
            geom_label_repel(box.padding = 0.5) + # Add labels to points
            geom_errorbar(aes(ymin = mean_DeltaC - sd_DeltaC, ymax = mean_DeltaC + sd_DeltaC), width = 0.1, colour = "grey") +  # Vertical error bars
            geom_errorbarh(aes(xmin = mean_DeltaV - sd_DeltaV, xmax = mean_DeltaV + sd_DeltaV), height = 0.1, colour = "grey") +       # Horizontal error bars
            theme_minimal() + theme(legend.position = "none") +# Remove legend
            labs(subtitle = "Standard deviation of the durations", 
                 y = "∆C", 
                 x = "∆C", 
                 title = "∆V/∆C", 
                 caption = "Metric by Ramus, Nespor & Mehler (1999)")
        }
        
        
       
        
        if (have_groups==1){
          output$plot4 <- renderPlot({
            grid.arrange(deltas, deltas_group, ncol=2)
          }) 
        }else{
          output$plot4 <- renderPlot({
            deltas
          }) 
        }
        
        
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    #if(input$disp == "head") {
    #  return(head(df))
    #}
    #else {
      return(df)
    #}
    
  })
  
}