library(tidyr)
library(dplyr)
library(ggplot2)
#Leo el archivo
var <- read.csv("variants_long_table.csv")

head(var)


# Check the dimension of the data
dim(var)

# Check number of rows
nrow(var)

# Check number of columns
ncol(var)

# Display the structure of your R object
str(var)


# Summary statistics of the whole data or specified columns
## For the whole table 
summary(var)

## For non-numerical data
summary(var$SAMPLE)


## For numerical data
summary(var$DP)


# Check the class of your data
class(var)


# Check the class of an object
class(var$CHROM)

typeof(var$CHROM)

# Preview the data using a spreadsheet-style data viewer in RStudio
View(var)


#Subsetting the data
#Filtering columns
# Check columns name
colnames(var)


# Select columns 1, 4, and 5
var[, c(1, 4, 5)]


#Select columns 1, 4, and 5 with default display
select(var, SAMPLE, REF, ALT)
                                  

# Select columns 1, 4, and 5 with selected display
select(var, SAMPLE, REF, ALT) %>% head(3)

# Select all columns except the column “CALLER” with selected display
select(var, -CALLER) %>% head(3)

# Transform the data frame into a tibble

var_tb <- as_tibble(var)

select(var_tb, SAMPLE, REF, ALT) %>% head(3)
# A tibble: 3 × 3
 



#2. Filtering rows

# Select rows with selected display using base R code

var_tb[var_tb$SAMPLE == "SRR13500958",]

# A tibble: 21 × 16
# ℹ 11 more rows
# ℹ 6 more variables: GENE <chr>, EFFECT <chr>, HGVS_C <chr>,
#   HGVS_P <chr>, HGVS_P_1LETTER <chr>, CALLER <chr>
# ℹ Use `print(n = ...)` to see more rows

# Select rows with selected display using dplyr functions

filter(var_tb, SAMPLE == "SRR13500958") %>% head(3)

# A tibble: 3 × 16
# ℹ 6 more variables: GENE <chr>, EFFECT <chr>, HGVS_C <chr>,
#   HGVS_P <chr>, HGVS_P_1LETTER <chr>, CALLER <chr>

#3. Filtering columns and rows
# Select sample type (rows) and variables (columns) with selected display
var_tb %>% filter(SAMPLE == "SRR13500958") %>% select(CHROM, POS, REF, ALT) %>% head(3)

# To select all data related to the sample specified
var_tb %>% filter(SAMPLE == "SRR13500958") %>% select(CHROM, POS, REF, ALT, DP)

# A tibble: 21 × 5

# To select only values for which DP>=500 for the same sample
var_tb %>% filter(SAMPLE == "SRR13500958" & DP>=500) %>% select(CHROM, POS, REF, ALT, DP)

# A tibble: 16 × 5
# To select only values for which DP>=1000 for the same sample
var_tb %>% filter(SAMPLE == "SRR13500958" & DP>=1000) %>% select(CHROM, POS, REF, ALT, DP)

# A tibble: 8 × 5
#Summary Statics

# Count how many rows are associated with each sample in the data 
var_tb %>% count(SAMPLE)

# Sorting the counts 
var_tb %>% count(SAMPLE, sort = TRUE)


# Distribution of genes per sample and counts 
var_tb %>% count(SAMPLE, GENE, sort = TRUE) %>% head()

#Matematica Basica
# Maximum value of column DP
max(var_tb$DP)


# Minimum value of column DP
min(var_tb$DP)


# Mean value of column DP
mean(var_tb$DP)


#Compute operations in new columns

# Compute a LOG2 transformation on the DP values
var_tb_log <- var_tb %>% mutate(DP_log2 = log2(DP))

# View the table columns with the DP_log2 new column appended at the end
head(var_tb_log)


# View a selected content including the new column
select(var_tb_log, SAMPLE, REF, ALT, DP, DP_log2) %>% head()

# Show the maximum value of DP for each sample
var_tb %>% group_by(SAMPLE) %>% summarize(max(DP))


# Show the minimum value of DP for each sample
var_tb %>% group_by(SAMPLE) %>% summarize(min(DP))



#VISUALISATION THE DATA
# Link ggplot2 to a specific data frame
ggplot(data = var_tb)


# Link ggplot2 to specific variables using aesthetics
ggplot(data = var_tb, aes(x=SAMPLE, y=DP))


# Points (left-hand plot)
ggplot(data = var_tb, aes(x=SAMPLE, y=DP)) + geom_point()

# Boxplot (right-hand plot)
ggplot(data = var_tb, aes(x=SAMPLE, y=DP)) + geom_boxplot()



#MAS GRAFICOS
# Points (left-hand plot)
ggplot(data = var_tb, aes(x=SAMPLE, y=DP)) + geom_point() + ylim(0,10000)

# Boxplot (right-hand plot)
ggplot(data = var_tb, aes(x=SAMPLE, y=DP)) + geom_boxplot() + ylim(0,10000)

# Points (left-hand plot)
ggplot(data = var_tb, aes(x=SAMPLE, y=DP)) + geom_point() + scale_y_continuous(name="dp", limits=c(0, 10000))

ggplot(data = var_tb, aes(x=SAMPLE, y=DP)) + geom_boxplot() + scale_y_continuous(name="dp", limits=c(0, 10000))


library(scales)
# Points (left-hand plot)
ggplot(data = var_tb, aes(x=SAMPLE, y=DP)) + geom_point() + scale_y_continuous(trans='log10')

ggplot(data = var_tb, aes(x=SAMPLE, y=DP)) + geom_point() + scale_y_log10()

# Boxplot (right-hand plot)
ggplot(data = var_tb, aes(x=SAMPLE, y=DP)) + geom_boxplot() + scale_y_continuous(trans='log10')

ggplot(data = var_tb, aes(x=SAMPLE, y=DP)) + geom_boxplot() + scale_y_log10()



#Advanced plotting options: colors, shapes, legend
#1. Change colors
# Colours of shapes
ggplot(data = var_tb, aes(x=SAMPLE, y=DP, colour = SAMPLE)) + geom_boxplot() + ylim(0,10000)

# Colours for filling options
ggplot(data = var_tb, aes(x=SAMPLE, y=DP, fill= SAMPLE)) + geom_boxplot() + ylim(0,10000)


# Colours for filling options with manual colors
ggplot(data = var_tb, aes(x=SAMPLE, y=DP, fill= SAMPLE)) + geom_boxplot() + ylim(0,10000) + scale_fill_manual(values=c("#cb6015", "#e1ad01", "#6d0016", "#808000", "#4e3524"))

# Colours for filling options with preset palettes
#install.packages(“RColorBrewer”)

library(RColorBrewer)
ggplot(data = var_tb, aes(x=SAMPLE, y=DP, fill= SAMPLE)) + geom_boxplot() + ylim(0,10000) + scale_fill_brewer(palette="RdYlBu")

display.brewer.all()

#2. Change legend position

ggplot(data = var_tb, aes(x=SAMPLE, y=DP, fill= SAMPLE)) + geom_boxplot() + ylim(0,10000) + scale_fill_brewer(palette="RdYlBu") + theme(legend.position="top")

ggplot(data = var_tb, aes(x=SAMPLE, y=DP, fill= SAMPLE)) + geom_boxplot() + ylim(0,10000) + scale_fill_brewer(palette="RdYlBu") + theme(legend.position="none")

#3. Change plot and axis titles
ggplot(data = var_tb, aes(x=SAMPLE, y=DP, fill= SAMPLE)) + geom_boxplot() + ylim(0,10000) + scale_fill_brewer(palette="RdYlBu") + theme(legend.position="bottom") + labs(title="DP_per_Sample", x="SampleID", y = "DP")

ggplot(data = var_tb, aes(x=SAMPLE, y=DP, fill= SAMPLE)) + geom_boxplot() + ylim(0,10000) + scale_fill_brewer(palette="RdYlBu") + theme(legend.position="bottom") + ggtitle("DP per Sample") + xlab("Sample") + ylab("DP")

#4. Change shapes, colors, and sizes
ggplot(data = var_tb, aes(x=SAMPLE, y=DP)) +  geom_point(shape = 21, fill = "#e4dbc1", color = "#b92e17", size = 6) + ylim(0,10000)

ggplot(data = var_tb, aes(x=SAMPLE, y=DP)) +  geom_point(shape = 23, color = "#e4dbc1", fill = "#b92e17", size = 5, alpha=0.5) + ylim(0,10000)


install.packages('ggpubr')
ggpubr::show_point_shapes()

#ULTIMA PARTE
#Progressing in variants exploration
# View the data 
View(var_tb)

#Distribution of DP values per chromosome and per sample
ggplot(data = var_tb, aes(x=CHROM, y=DP, fill= SAMPLE)) + geom_boxplot() + ylim(0,10000) + scale_fill_brewer(palette="RdYlBu") + labs(title="DP_per_Chromosome") + facet_grid(. ~ SAMPLE)



# Define a variable with plotting options

p_DP_CHROM <- ggplot(data = var_tb, aes(x=CHROM, y=DP, fill= SAMPLE)) + ylim(0,10000) + scale_fill_brewer(palette="RdYlBu") + labs(title="DP_per_Chromosome") + theme(legend.position="bottom")

# Test boxplots with faceting 

p_DP_CHROM + geom_boxplot() + facet_grid(. ~ SAMPLE)

#Combine violin plots and boxplots with faceting

p_DP_CHROM + geom_violin(trim=FALSE) + facet_grid(. ~ SAMPLE) + geom_boxplot(width=0.1)


#Variants effects per sample
#1. Plotting the variants effects

# Count number of different effects per sample
p_EFFECT <- ggplot(data = var_tb, aes(x=EFFECT, fill= SAMPLE)) + scale_fill_brewer(palette="RdBu") + labs(title="Effect_per_Sample") + theme(legend.position="bottom")

p_EFFECT + geom_bar()

# Flip orientation

p_EFFECT_flip <- ggplot(data = var_tb, aes(y=EFFECT, fill= SAMPLE)) + scale_fill_brewer(palette="RdBu") + labs(title="Effect_per_Sample") + theme(legend.position="bottom")

p_EFFECT_flip + geom_bar()


#2. Counting the variants effects

#Count the number of different effects
var_tb %>% count(EFFECT)


# Count the number of different effects and link them to sample information
var_tb %>% count(EFFECT, SAMPLE, sort = TRUE)


#2. Counting and extracting specific effects for all genes
filter(var_tb, EFFECT == "stop_lost" | EFFECT == "stop_gained")


# Filtering option 2 to select for effect on stop
filter(var_tb, EFFECT %in% c("stop_lost", "stop_gained"))


# Filtering on effect and selected columns
filter(var_tb, EFFECT %in% c("stop_lost", "stop_gained")) %>% select(SAMPLE, CHROM, GENE, EFFECT)


#Read depth per position
# Define your variable
p_DP_POS <- ggplot(data = var_tb, aes(x=POS, y=DP, fill= SAMPLE)) + scale_fill_brewer(palette="RdBu") + labs(title="DP_per_Position") + theme(legend.position="bottom")

# Plot
p_DP_POS + geom_point(shape = 21, size = 5)

# Plot with transparency options
p_DP_POS + geom_point(shape = 21, size = 5, alpha = 0.7)

