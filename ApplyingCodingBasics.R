if(!require("palmerpenguins")){
  install.packages("palmerpenguins",repos = 'http://cran.us.r-project.org')
}
install.packages("palmerpenguins")
library(palmerpenguins)
data(package = "palmerpenguins")

penguins
penguins_df <- as.data.frame(penguins)

df_1stcol<-penguins_df[,1]
tbl_1stcol<-penguins[,1]

#####
#10 rows
head(mtcars,10)
class(mtcars$disp)
str(mtcars)

nrow(mtcars)
dim(mtcars)
mean(mtcars[,1])
mtcars[6,10]
mtcars[,4]
mtcars[28,]

mtcars$mpg
mtcars[28:31,]
##################

head(penguins)
table(penguins$species)
is.na(penguins$sex)
sum(is.na(penguins$sex))

# The data -- DO NOT EDIT 
ken_data <- data.frame(
  "ken_name" = c("Ken1", "Ken2", "Ken3", "Ken4", "Ken5", "Ken6", "Ken7", "Allan"),
  "hair_color" = c("Blonde", "Brown", "Black", "Red", "Blonde", "Brown", "Black", "Black"),
  "cowboy_hats_owned" = c(2, 0, 1, 3, 0, 1, 2, 0),
  "favorite_outfit" = c("Casual", "Formal", "Sporty", "Beachwear", "Formal", "Casual", "Sporty", "Casual"),
  "age" = c(25, 27, 26, 28, 29, 30, 26, 27),
  "height_cm" = c(180, 175, 182, 178, 180, 183, 177, 175),
  "weight_kg" = c(75, 70, 80, 77, 76, 78, 79, 70),
  "favorite_hobby" = c("Surfing", "Reading", "Soccer", "Volleyball", "Painting", "Cooking", "Dancing", "Guitar"),
  "favorite_color" = c("Blue", "Green", "Red", "Yellow", "Purple", "Orange", "Pink", "Blue"),
  "shoe_size" = c(10, 9, 11, 10, NA, 11, 10, 9),
  "best_friend" = c("Barbie", "Barbie", "Barbie", "Barbie", "Barbie", "Barbie", "Barbie", NA),
  "is_ken" = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
)
ken_data
str(ken_data)
head(ken_data)

mean(ken_data$cowboy_hats_owned)
hist(ken_data$cowboy_hats_owned)
ken_data$more.than.1_cowboyHat <- ken_data$cowboy_hats_owned > 1
print(paste(sum(ken_data$more.than.1_cowboyHat), "Kens have more than 1 cowboy hat"))

range(ken$age)
range(ken_data$shoe_size, na.rm=TRUE)


correlation <- cor(ken_data$height_cm, ken_data$weight_kg)
print(paste("The correlation between height and weight is", correlation))
plot(ken_data$height_cm, ken_data$weight_kg)

table(ken_data$best_friend)
# looks like everyone's bff is barbie!

# outfits
table(ken_data$favorte_outfit)


# no allan
range(ken_data[1:7,5])

noAllan <- ken_data[1:7,]
also_noAllan <- noAllan <- ken_data[ken_data$is_ken == TRUE,]
range(noAllan$shoe_size)

# Are the sporty Kens taller than the other Kens?
sporty_kens <- mean( ken_data [ken_data$favorite_outfit == "Sporty", "height_cm"])
other_kens_mean <- mean(ken_data[ken_data$favorite_outfit != "Sporty", "height_cm"] )

sporty_kens > other_kens_mean


#####

# First, we'll make a vector to play with
names <- c("Kaladin", "Dalinar", "Szeth")
names[1]
# You can index more than one position at a time too:
names[c(1,2)]

# Make a data frame
df <- data.frame(
  name = c("Rosalind Franklin", "Marie Curie", "Barbara McClintock", "Ada Lovelace", "Dorothy Hodgkin", 
           "Lise Meitner", "Grace Hopper", "Chien-Shiung Wu", "Gerty Cori", "Katherine Johnson"),
  field = c("DNA X-ray crystallography", "Radioactivity", "Genetics", "Computer Programming", "X-ray Crystallography", 
            "Nuclear Physics", "Computer Programming", "Experimental Physics", "Biochemistry", "Orbital Mechanics"),
  school = c("Cambridge", "Sorbonne", "Cornell", "University of London", "Oxford", 
             "University of Berlin", "Yale", "Princeton", "Washington University", "West Virginia University"),
  date_of_birth = c("1920-07-25", "1867-11-07", "1902-06-16", "1815-12-10", "1910-05-12", 
                    "1878-11-07", "1906-12-09", "1912-05-31", "1896-08-15", "1918-08-26"),
  working_region = c("Western Europe", "Western Europe", "North America", "Western Europe", "Western Europe", "Western Europe", "North America", "North America", "North America",  "North America")
)

# To get the first row:
df[1,]
df[,c("school","name")]
library(dplyr)
dplyr::glimpse(df)

# Filtering for scientists born after 1900
filtered_data <- filter(df, as.Date(date_of_birth) > as.Date("1900-01-01"))

# Arranging the filtered data by date of birth
arranged_data <- arrange(filtered_data, date_of_birth)


cell_prop <- read.csv("https://raw.githubusercontent.com/How-to-Learn-to-Code/Rclass-DataScience/main/data/wrangling-files/cellProportions.csv")
glimpse(cell_prop)

cell_prop %>% slice_max(Macrophage) %>% pull(Pericytes.SMC)
cell_prop %>% relocate( Cardiomyocytes, .after = Macrophage) |>head()

cell_pheno <-read.csv("https://raw.githubusercontent.com/How-to-Learn-to-Code/Rclass-DataScience/main/data/wrangling-files/cellPhenotypes.csv" , 
                      sep=",", header= TRUE) 
head(cell_pheno)
cell_merge_df<- merge(x=cell_pheno, y=cell_prop, by.x="id",by.y= "X") %>% head()

left_join(cell_pheno, cell_prop, by = c("id" = "X")) %>% head()

cell_merge_df %>% distinct(genotype,treatment)
cell_merge_df %>% mutate(Cardiomyocyte_percentage = Cardiomyocytes *100) |>head()
cell_merge_df %>% mutate(group=str_split(id,"_", simplify = TRUE)[,2]) %>% head()



