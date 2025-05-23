---
title: "R script social media for flower paper, AGEE Klaus et al. 2025"
date: '2025-05-22'
Question: abdesslam.chaiallah@gmail.com
---
This R script includes and describes:
1. Data collection from Flickr and iNaturalist in Switzerland;
2. Content analysis of images using the Google Cloud Vision algorithm for label and object analysis.
3. Visualization of the results

Package installation

```{r}
packages <- c("tidyverse", "dplyr", "pacman", "dplyr", "tibble", "ggplot2",
"writexl", "photosearcher", "httr", "rinat", "lubridate", "data.table", "imgrec",
"cat","tidyr","cowplot","grid")

# Load or install missing packages
lapply(packages, function(package) {
if (!require(package, character.only = TRUE)) {
install.packages(package)
library(package, character.only = TRUE)
}
})
```

1. Data collection 
```{r}
#Flickr : the principal package is photosearcher from the paper of Fox et al. 2020 
#PLEASE GET THE API KEY FROM fLICKR AND STORE IT YOUR DIRECTORY BEFORE RUNNING THE CODE

Flcikr_swiss <- photo_search(mindate_taken = "2004-01-01",
                             maxdate_taken = "2023-12-31",
                             bbox = "6.832224, 47.091689, 6.856805, 47.129356") #coordinates 

#iNaturalist: here you will need the "rinat" package, if you gor an error while installing the package use this line : 
install.packages("rinat", INSTALL_opts="--no-multiarch")  #solution for ERROR: loading failed for 'i386'

iNaturalist_swiss <- get_inat_obs(query = "Switzerland",
                              geo = TRUE,
                              year = 2023,  #define the year
                              month = 12)


#Data cleaning after the collection, for example to remove duplicate images
unique_imageiNaturalist <- iNaturalist_image %>%
  group_by(image_url)  %>%
  filter(duplicated(image_url) | n()==1)         #select unique photos 

```


2. Google Cloud Vision (label analysis)
To run this part, you will need a google vision API, check this link for more informations on how to get a key for free :https://cloud.google.com/vision
Wu used this algo twice in the paper, first for the label analysis to extract only those images showing nature and natural objects (see section 2.3.2. Extraction of objects from images) and after the object analysis to extract the species without diving into a taxonomic analysis.
```{r}
#set up gvision
Sys.setenv(gvision_key = "your key") #Your Google API Key
gvision_init()
####### 1. LABEL ANALYSIS 

#search google vision for FLICKR and Inaturalist images 
results <- get_annotations(images = paste(Mydataset$url), # image url
                           features = "label", # request all available features
                           max_res = 20, # maximum number of results per feature
                           mode = 'url') # determine image type

#parse results
img_data <- parse_annotations(results)
vision_results <- img_data$labels

```

Data analysis of the results of Google Cloud Vision for labels to classify images to Nature vs non Nature images, and from nature images we selected images focusing on species. We followed here the paper of Chai-allah et al. 2025.
```{r}
#Data cleaning by platform
#Turn labels into lower case
data_platforms$description <- tolower(data_platforms$description)
#Filter by 60% confidence 
scoreaccepted_data <- data_platforms %>%
  filter(score > 0.5999999)
#count labels frequency
description <- data.frame(table(scoreaccepted$description))
tags <- tags[order(-tags$Freq),]

#merge original dataset with labels frequency 
FinaldataVision <- inner_join(scoreaccepted_data, 
                              description, 
                              by = "description")

#remove labels less than n=15
VisionFinal <- FinaldataVision %>%
  filter(Freq > 14)

#labels classification 

classificationGV <- left_join(VisionFinal, #our data labels
                          GoogleLabels,   #calssification of Chai-allah et al. 2025
                          by = "description")   


#Classify the images to select nature images, for each image we counted the number of labels and if 50% of the labels are about nature (forest, tree) the image will be classify to nature image.

#images and classification of tags ----
nature_images <- classificationGV %>% 
  group_by(image_url) %>% 
  summarise(
    prop_notnature = sum(class=="no")/n(), 
    prop_nature = sum(class != "no")/n())


#select prop_nature >=0.50 as nature images 
nature_images$imageNature <- ifelse(nature_images$prop_notnature>0.50, "no",
                          "yes")

#select nature images
nature_Flickr_naturalist <- nature_images %>%
  filter(imageNature == "yes")

#from nature images we select those where the labels are focus on species (50%labels are about species example: flower, bird...)
species_images <- nature_Flickr_naturalist %>%
  group_by(image_url) %>% 
  summarise(
    prop_species = sum(type=="specie")/n(), 
    prop_notspecies = sum(type != "specie")/n())


#select prop_species >=0.50 as species images 
species_images$imageSpecies <- ifelse(species_images$prop_notspecies>0.50, "no",
                               "yes")

#By platfrom FILTER FOR IMAGES WITH grass labels : Grass, grassland, meadow and pasture

flickrGRASSLAND <- speciesIMAGES_flickr %>%
  filter(description == "grass" | description == "grassland" | 
         description == "meadow" | description == "pasture") %>%
  group_by(image_url) %>%
  distinct(image_url,owner, datetaken, imageNature, imageSpecies, platform)
```

After this step by selecting species images with grasslans labels, we used Google Cloud Vision for the second time, this time the goal is detect objects. This feature has better capabilities
to accurately detect the species present in the images compared to the “label” feature
(https://cloud.google.com/vision/docs/features-list).

```{r}
#GOOGLE VISION OBJECT FROM species IMAGES with grass labels
#set up gvision
Sys.setenv(gvision_key = "your key") #Your Google API Key
gvision_init()

#search google vision
SpeciesGRASS_Flickr <- get_annotations(images = paste(flickrGRASSLAND$image_url),#image url
                                features = "object", # request all available features
                                max_res = 3, # maximum number of results per feature
                                mode = 'url') # determine image type


#parse results
img_dataN1 <- parse_annotations(SpeciesGRASS_Flickr)
vision_results_Flickr <- img_dataN1$objects


#filter the results by platform to remove objects less tham Frequency = 2 (same appraoch as for labels)
Freqobjects_Flickr<- data.frame(table(vision_results_Flickr$object))
#joint the data of objects with their frequency

Seelected_objects_Flickr <- inner_join(vision_results_Flickr,
                                       Freqobjects_Flickr,
                                       by = "object")
#select only objects with Freq > 1
Seelected_objects_Flickr <- Seelected_objects_Flickr %>%
  filter(Freqobject > 1)

#remove generic objects + zoo and pet + non-nature objects

update_objects <- Seelected_objects_Flickr %>%
  filter(!name %in% c('Person','Animal','Plant','Shoe','Cat',
                      'Dog','Tiger'))  #selected examples

#create groups to to simplify the classifications by grouping objects.
update_objects_Final_Flickr <- update_objects %>%
  mutate(groupname = case_when(
    name %in% c("Duck","Sparrow","Goose","Swan","Falcon","Raven","Eagle","Magpie",
                "Owl","Blue jay","Woodpecker") ~ "Bird",
    name %in% c("Bull") ~ "Cattle",
    name %in% c("Turtle") ~ "Tortoise",
    name %in% c("Chicken","Turkey","Ostrich") ~ "Poultry",
    name %in% c("Moths and butterflies","Butterfly","Dragonfly","Beetle","Bee",
                 "Lady bug","Caterpillar") ~ "Insect",
    TRUE ~ name))  # This handles any other cases not specified

# Select unique objects returned by GCV, meaning within an image, three objects belonging to the group “bird” would be reduced to one “bird” object.

Unique_objects_Flickr <- update_objects_Final_Flickr %>%
  group_by(image_url)  %>%
  arrange(groupname, desc(score)) %>%
  distinct(groupname, .keep_all = TRUE)

```


3. Visualization of the results. In this part, the scripts used to plat the results, including the taxonomy analysis done in PYTHON that you can find in the repository.

Graph 1 about the "Frequent objects in grassland images" see section 3.3 in the results.

```{r}
# SELECT TOP 10 OBJECTS BASED ON FREQUENCY for FLICKR AND INATURALIST
top10flickr <- flickrGCV_F %>%
  top_n(10, Frequency) %>%
  slice(1:10) %>%
  arrange(desc(Frequency))

#PLOT: TOP 10 OBJECTS FLICKR
flickr_GCV_plt <- top10flickr %>%
  ggplot(aes(x=reorder(Object,-Percentage), y=Percentage)) +
  geom_bar(stat="identity", fill="#CB6262", colour="black") + 
  scale_y_continuous(name = "% of images", breaks=seq(0,60,10), limits=c(0, 60)) +
  scale_x_discrete(name = "")+
  geom_text(top10flickr, mapping = aes(label=Frequency),stat="identity", 
            position = "stack", size = 5, hjust = 0.5,
            angle =0, vjust = -0.25, color = "black")+
  theme_bw()+
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 20, color = "black",face="bold"),
        plot.margin = unit(c(0.5,0,0,0.55), "cm"),
        panel.grid.major = element_line(size = 1),
        panel.border = element_blank())
flickr_GCV_plt

#PLOT: TOP 10 OBJECTS INATURALIST (after selection of the top 10)

inatu_GCV_plt <- top10natura %>%
  ggplot(aes(x=reorder(Object,-Percentage), y=Percentage)) +
  geom_bar(stat="identity", fill="#69a84f", colour="black") + 
  scale_y_continuous(name = "% of images", breaks=seq(0,60,10), limits=c(0, 60)) +
  scale_x_discrete(name = "Objects")+
  geom_text(top10natura, mapping = aes(label=Frequency),stat="identity", 
            position = "stack", size = 5, hjust = 0.5,
            angle =0, vjust = -0.25, color = "black")+
  theme_bw()+
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 20, color = "black",face="bold"),
        plot.margin = unit(c(0,0,0,0.55), "cm"),
        panel.grid.major = element_line(size = 1),
        panel.border = element_blank())
inatu_GCV_plt

#MERGE THE TWO PLOTS 
plot_grid(flickr_GCV_plt, inatu_GCV_plt, 
          nrow = 2, 
          labels = c("Flickr", "iNaturalist"), 
          label_y = c(0.95, 1),        # Adjust Y position of both labels
          hjust = -0.025, 
          vjust = -0.09, 
          label_size = 19)
#SAVE
ggsave(filename='objectsGCV.png',device='png',
       width=11, height=10, dpi=400, bg="white")
```

Graph 2 about the "The 20 most frequently photographed familiae in Flickr and iNaturalist wildlife images". See section 3.4 in the results.

```{r}
# SELECT TOP 20 familiae BASED ON FREQUENCY for FLICKR AND INATURALIST

familytop20_flickr <- famFlickrGCV_F %>%
  distinct(family, nameGCV, familyFREQ) %>%
  top_n(20, familyFREQ) %>%
  arrange(desc(familyFREQ))

#plot Flickr 

fam_flickr_plt <- familytop20_flickr %>%
ggplot(aes(y=reorder(family,Percentage), x=Percentage, fill=nameGCV)) +
  geom_bar(stat="identity", colour="black") + 
  scale_x_continuous(name = "% of images", breaks=seq(0,12,3), limits=c(0, 12)) +
  scale_y_discrete(name = "")+
  scale_fill_manual(labels = c("Bird", "Deer","Frog", "Insect", "Plant"),
                    values = c("#4B8BBE","#A0522D","#32CD32","#FFA500","#8B008B"))+
  geom_text(familytop20_flickr, mapping = aes(label=familyFREQ),stat="identity", 
            position = "stack", size = 5, hjust = -0.2,
            angle =0, vjust = 0.5, color = "black")+
  theme_bw()+
  theme(legend.title= element_blank(),
        legend.position = "bottom",
        text = element_text(size = 22, color = "black"),
        axis.text = element_text(size = 15.5, color = "black"),
        axis.title = element_text(size = 16.5, color = "black", face="bold"),
        plot.margin = unit(c(0.5,0,0,0), "cm"),
        panel.grid.major = element_line(size = 1),
        panel.border = element_blank())
fam_flickr_plt

#plot inaturalist (after selection of the top 20)

fam_NATURA_plt <- familytop20_INAT %>%
  ggplot(aes(y=reorder(family,Percentage), x=Percentage, fill=nameGCV)) +
  geom_bar(stat="identity", colour="black") + 
  scale_x_continuous(name = "% of images", breaks=seq(0,12,3), limits=c(0, 12)) +
  scale_y_discrete(name = "")+
  scale_fill_manual(labels = c("Bird","Insect","Plant"),
                    values = c("#4B8BBE","#FFA500","#8B008B"))+
  geom_text(familytop20_INAT, mapping = aes(label=familyFREQ),stat="identity", 
            position = "stack", size = 5, hjust = -0.2,
            angle =0, vjust = 0.5, color = "black")+
  theme_bw()+
  theme(legend.title= element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 15.5, color = "black"),
        axis.title = element_text(size = 16.5, color = "black",face="bold"),
        plot.margin = unit(c(0.5,0,0,0), "cm"),
        panel.grid.major = element_line(size = 1),
        panel.border = element_blank())
fam_NATURA_plt

# Function to extract legend from a ggplot object
get_legend <- function(my_plot) {
  temp <- ggplotGrob(my_plot)
  legend <- gtable::gtable_filter(temp, "guide-box")
  return(legend)
}

# Extract the legend plot of Flickr
legend <- get_legend(fam_flickr_plt)

# First plot without legend
fam_flickr_plt <- fam_flickr_plt + theme(legend.position = "none")


plotcombined <- plot_grid(fam_flickr_plt, fam_NATURA_plt, nrow=1, 
          labels = c("Flickr","iNaturalist"), 
          hjust=0.05, 
          label_x = c(0.01, 0.03),
          label_size = 20)

# Adding the legend at the bottom
final_plot <- plot_grid(plotcombined, legend, ncol = 1, rel_heights = c(0.8, 0.07))

ggsave(filename='familyPLOT.png',device='png',
       width=15, height=12, dpi=400, bg="white")

```

Graph 3 about the "The 20 most frequently photographed genera in Flickr and iNaturalist wildlife images". See section 3.4 in the results.

```{r}

# SELECT TOP 20 genera BASED ON FREQUENCY for FLICKR AND INATURALIST
genustop20_flickr <- genusFLICKRGCV_F %>%
  distinct(genus, nameGCV, genusFREQ) %>%
  top_n(20, genusFREQ) %>%
  arrange(desc(genusFREQ))

# plot Flickr top 20

GEN_flickr_plt <- genustop20_flickr %>%
  ggplot(aes(y=reorder(genus,Percentage), x=Percentage, fill=nameGCV)) +
  geom_bar(stat="identity", colour="black") + 
  scale_x_continuous(name = "% of images", breaks=seq(0,4,1), limits=c(0, 4)) +
  scale_y_discrete(name = "")+
  scale_fill_manual(labels = c("Bird","Frog", "Insect","Plant"),
                    values = c("#4B8BBE","#32CD32","#FFA500","#8B008B"))+
  geom_text(genustop20_flickr, mapping = aes(label=genusFREQ),stat="identity", 
            position = "stack", size = 5, hjust = -0.2,
            angle =0, vjust = 0.5, color = "black")+
  theme_bw()+
  theme(legend.title= element_blank(),
        legend.position = "bottom",
        text = element_text(size = 22, color = "black"),
        axis.text = element_text(size = 15.5, color = "black"),
        axis.title = element_text(size = 16.5, color = "black",face="bold"),
        plot.margin = unit(c(0.5,0,0,0), "cm"),
        panel.grid.major = element_line(size = 1),
        panel.border = element_blank())
GEN_flickr_plt


# plot inaturalist top 20 (after selection of the top 20)

GEN_INAT_plt <- genustop20_INAT %>%
  ggplot(aes(y=reorder(genus,Percentage), x=Percentage, fill=nameGCV)) +
  geom_bar(stat="identity", colour="black") + 
  scale_x_continuous(name = "% of images", breaks=seq(0,4,1), limits=c(0, 4)) +
  scale_y_discrete(name = "")+
  scale_fill_manual(labels = c("Bird", "Insect","Plant"),
                    values = c("#4B8BBE","#FFA500","#8B008B"))+
  geom_text(genustop20_INAT, mapping = aes(label=genusFREQ),stat="identity", 
            position = "stack", size = 5, hjust = -0.2,
            angle =0, vjust = 0.5, color = "black")+
  theme_bw()+
  theme(legend.title= element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 15.5, color = "black"),
        axis.title = element_text(size = 16.5, color = "black",face="bold"),
        plot.margin = unit(c(0.5,0,0,0), "cm"),
        panel.grid.major = element_line(size = 1),
        panel.border = element_blank())
GEN_INAT_plt


# Function to extract legend from a ggplot object
get_legend <- function(my_plot) {
  temp <- ggplotGrob(my_plot)
  legend <- gtable::gtable_filter(temp, "guide-box")
  return(legend)
}

# Extract the legend plot of Flickr genera
legend <- get_legend(GEN_flickr_plt)

# First plot without legend
GEN_flickr_plt <- GEN_flickr_plt + theme(legend.position = "none")


plotcombinedgenus <- plot_grid(GEN_flickr_plt, GEN_INAT_plt, nrow=1,  
                          labels = c("Flickr","iNaturalist"), 
                          hjust=0.05, 
                          label_x = c(0.01, 0.03),
                          label_size = 20)

# Adding the legend at the bottom
final_plot1 <- plot_grid(plotcombinedgenus, legend, ncol = 1, rel_heights = c(0.8, 0.07))

ggsave(filename='genusPLOT.png',device='png',
       width=15, height=12, dpi=400, bg="white")
```










