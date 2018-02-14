
#Load data to the workspace

df <- read.csv("TPA_Final.csv",
               header = T,
               na.strings = c("", "NA"))

#Install Packages

install.packages("lubridate" , lib = "C:/Rpack" , dependencies = TRUE)
install.packages("anytime" , lib = "C:/Rpack" , dependencies = TRUE)
install.packages("reshape2" , lib = "C:/Rpack" , dependencies = TRUE)
install.packages("dplyr" , lib = "C:/Rpack" , dependencies = TRUE)
install.packages("scales" , lib = "C:/Rpack" , dependencies = TRUE)
install.packages("Hmisc" , lib = "C:/Rpack" , dependencies = TRUE)
install.packages("plyr", lib = "C:/Rpack", dependencies = TRUE)
install.packages("ggplot2" , lib = "C:/Rpack", dependencies = TRUE)
install.packages("tidyverse" , lib = "C:/Rpack", dependencies = TRUE)
install.packages("colorRamps" , lib = "C:/Rpack", dependencies = TRUE)

#Load the libraries

library(anytime , lib.loc = "C:/Rpack")
library(lubridate , lib.loc = "C:/Rpack")
library(ggplot2 , lib.loc = "C:/Rpack")
library(Hmisc , lib.loc = "C:/Rpack")
library(plyr , lib.loc = "C:/Rpack")
library(stats , lib.loc = "C:/Rpack")
library(reshape2 , lib.loc = "C:/Rpack")
library(dplyr , lib.loc = "C:/Rpack")
library(scales , lib.loc = "C:/Rpack")
library("backports" , lib.loc = "C:/Rpack")
library(tidyverse , lib.loc = "C:/Rpack")
library(RColorBrewer)
library(colorRamps , lib.loc = "C:/Rpack")


#Structure of the DF

describe(df)
View(df)
summary(df)

#Feature Engineering
#Change the date into POSIXct

df$IL1 <- as.Date(df$IL1 , "%Y-%b-%d")
df$IL2 <- as.Date(df$IL2 , "%Y-%b-%d")
df$IL3 <- as.Date(df$IL3 , "%Y-%b-%d")
df$IL4 <- as.Date(df$IL4 , "%Y-%b-%d")
df$IL5 <- as.Date(df$IL5 , "%Y-%b-%d")
df$IL1.planned <- as.Date(df$IL1.planned , "%Y-%b-%d")
df$IL2.planned <- as.Date(df$IL2.planned , "%Y-%b-%d")
df$IL3.planned <- as.Date(df$IL3.planned , "%Y-%b-%d")
df$IL4.planned <- as.Date(df$IL4.planned , "%Y-%b-%d")
df$IL5.planned <- as.Date(df$IL5.planned , "%Y-%b-%d")
df$IL1.planned.1 <- as.Date(df$IL1.planned.1 , "%Y-%b-%d")
df$IL2.planned.1 <- as.Date(df$IL2.planned.1 , "%Y-%b-%d")
df$IL3.planned.1 <- as.Date(df$IL3.planned.1 , "%Y-%b-%d")
df$IL4.planned.1 <- as.Date(df$IL4.planned.1 , "%Y-%b-%d")
df$IL5.planned.1 <- as.Date(df$IL5.planned.1 , "%Y-%b-%d")


anydate(df$IL1)
anydate(df$IL2)
anydate(df$IL3)
anydate(df$IL4)
anydate(df$IL5)
anydate(df$IL1.planned)
anydate(df$IL2.planned)
anydate(df$IL3.planned)
anydate(df$IL4.planned)
anydate(df$IL5.planned)
anydate(df$IL1.planned.1)
anydate(df$IL2.planned.1)
anydate(df$IL3.planned.1)
anydate(df$IL4.planned.1)
anydate(df$IL5.planned.1)

#Extract Week from date

df$Cw_IL1 <- week(df$IL1)
df$Cw_IL2 <- week(df$IL2)
df$Cw_IL3 <- week(df$IL3)
df$Cw_IL4 <- week(df$IL4)
df$Cw_IL5 <- week(df$IL5)
df$Cw_IL1_planned <- week(df$IL1.planned)
df$Cw_IL2_planned <- week(df$IL2.planned)
df$Cw_IL3_planned <- week(df$IL3.planned)
df$Cw_IL4_planned <- week(df$IL4.planned)
df$Cw_IL5_planned <- week(df$IL5.planned)
df$Cw_IL1_planned_1 <- week(df$IL1.planned.1)
df$Cw_IL2_planned_1 <- week(df$IL2.planned.1)
df$Cw_IL3_planned_1 <- week(df$IL3.planned.1)
df$Cw_IL4_planned_1 <- week(df$IL4.planned.1)
df$Cw_IL5_planned_1 <- week(df$IL5.planned.1)

#Count the IL Freq

count(df, "IL")

#Data Study

aggregate(df$Plan.value..oku.JPY. ~ df$Dept., data = df, sum)

#EDA for Plotting

#Transforming the data for reordering the coloumns

mydf <-
  transform(a, variables = reorder(a$`df$Dept.`,-a$`df$Plan.value..oku.JPY.`))

#Plotting the data (PlanValue vs Department)

ggplot(data = mydf,
       aes(
         x = reorder(a$`df$Dept.`, -a$`df$Plan.value..oku.JPY.`),
         y = a$`df$Plan.value..oku.JPY.`
       )) +
  geom_bar(stat = "identity",
           fill = "blue") +
  labs(x = "Dept", y = "Plan Value", title = "Dept Vs Plan Value")


#########################################################################
count(!is.na(df$X.9.4..1.E.cell.2.Advance))
count(
  !is.na(
    df$X.9.4.1.Capacity.Effects.2.Legal.Obligations.3.New.Products...Technology.4.Reinvestment....Maintenance
  )
)

##########################################################################

#Defining Function

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <-
  function(...,
           plotlist = NULL,
           file,
           cols = 1,
           layout = NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols,
                       nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <-
          as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]],
              vp = viewport(
                layout.pos.row = matchidx$row,
                layout.pos.col = matchidx$col
              ))
      }
    }
  }


p1 <-
  ggplot() + geom_bar(aes(y = df$Plan.value..oku.JPY., x = df$Cw_IL1),
                      data = df,
                      stat = "identity")
p2 <-
  ggplot() + geom_bar(aes(y = df$Plan.value..oku.JPY., x = df$Cw_IL2),
                      data = df,
                      stat = "identity")
p3 <-
  ggplot() + geom_bar(aes(y = df$Plan.value..oku.JPY., x = df$Cw_IL3),
                      data = df,
                      stat = "identity")
p4 <-
  ggplot() + geom_bar(aes(y = df$Plan.value..oku.JPY., x = df$Cw_IL4),
                      data = df,
                      stat = "identity")
p5 <-
  ggplot() + geom_bar(aes(y = df$Plan.value..oku.JPY., x = df$Cw_IL5),
                      data = df,
                      stat = "identity")

multiplot(p1, p2, p3, p4, p5, cols = 2)

######################### Feature Engineering #####################################

df$IL1_Flag1 <- ifelse(df$IL == "IL1", 1, 0)

df$IL2_Flag2 <- ifelse(df$IL == "IL2", 1, 0)

df$IL3_Flag3 <- ifelse(df$IL == "IL3", 1, 0)

df$IL4_Flag4 <- ifelse(df$IL == "IL4", 1, 0)

df$IL5_Flag5 <- ifelse(df$IL == "IL5", 1, 0)

df$IL6_Flag6 <- ifelse(df$IL == "IL6", 1, 0)

############################### Category vars ###################################
names(df)
var_st1 <-
  c(
    "Category.Top.management...1.Emergency..2..Strategic.project..3.Legulation.oblige..4.Nothing...Blank." ,
    "IL1_Flag1" ,
    "IL2_Flag2" ,
    "IL3_Flag3" ,
    "IL4_Flag4" ,
    "IL5_Flag5" ,
    "IL6_Flag6"
  )
cat_mgmt_strat1 <- df[var_st1]
rename(cat_mgmt_strat1, Category = Category.Top.management...1.Emergency..2..Strategic.project..3.Legulation.oblige..4.Nothing...Blank.)
names(cat_mgmt_strat1)[1] <- "Category"

#Checking the stats
count(cat_mgmt_strat1, cat_mgmt_strat1$Category)

#Imputing the missing values
cat_mgmt_strat1$Category[cat_mgmt_strat1$Category == "_"] <- 4
cat_mgmt_strat1$Category[is.na(cat_mgmt_strat1$Category)] <- 4

#Changing to a dataframe
agg_cat1 <- ddply(cat_mgmt_strat1, .(Category), colwise(sum))

#Checking the structure
str(agg_cat1)

#Changing from "wide" format to "long" format for ggplot visualisation
agg_melt1 <- melt(agg_cat1, id.vars = "Category")

#ggplot2 visualization

######################## Function for Custom palette of ggplot2 #################

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#make custom palette

mycols <- gg_color_hue(length(unique(agg_melt1$variable)))
names(mycols) <- unique(agg_melt1$variable)
mycols["IL6_Flag6"] <- "red"
mycols["IL5_Flag5"] <- "green"
mycols["IL4_Flag4"] <- "yellow"
mycols["IL3_Flag3"] <- "blue"

##############################################################################


agg_melt1 <- agg_melt1 %>%
  group_by(Category) %>%
  mutate(p = value / sum(value))

ggplot(data = agg_melt1,
       aes(
         y = value,
         x = Category,
         label = percent(p),
         fill = variable
       )) +
  geom_col(position = "fill") +
  geom_text(position = position_fill(vjust = 0.5), size = 2.7) +
  scale_y_continuous(labels = percent) +
  labs(x = "Category",
       y = "Percentage (%)") +
  scale_fill_manual(values = mycols)



############################# Planned values #####################################
df$variance <- as.numeric((df$Cw_IL5) - (df$Cw_IL5_planned))

var_st_p1 <-
  c(
    "Category.Top.management...1.Emergency..2..Strategic.project..3.Legulation.oblige..4.Nothing...Blank." ,
    "variance"
  )
#Shorthands

#Top Management <- 1
# Emergency <- 2
# Strategic project <- 3
# Legulation  oblige <- 4


cat_mgmt_p1 <- df[var_st_p1]

rename(cat_mgmt_p1, Category = Category.Top.management...1.Emergency..2..Strategic.project..3.Legulation.oblige..4.Nothing...Blank.)

names(cat_mgmt_p1)[1] <- "Category"

#Imputing the missing values

cat_mgmt_p1$Category[cat_mgmt_p1$Category == "_"] <- 4
cat_mgmt_p1$Category[is.na(cat_mgmt_p1$Category)] <- 4
cat_mgmt_p1$variance[is.na(cat_mgmt_p1$variance)] <-
  round(mean(cat_mgmt_p1$variance, na.rm = TRUE))
cat_mgmt_p1$Category <-
  as.numeric(as.character(cat_mgmt_p1$Category))


count(agg_cat_p1, cat_mgmt_p1$Category)

agg_cat_p1 <- ddply(cat_mgmt_p1, .(Category), colwise(sum))
agg_melt_p1 <- melt(agg_cat_p1, id.vars = "Category")

ggplot(data = agg_melt_p1) +
  geom_col(aes(x = Category,
               y = value)) +
  labs(x = "Category" , y = "val")

###########################  Department wise ###################################

var_dep1 <- c(
  "Dept." ,
  "IL1_Flag1" ,
  "IL2_Flag2" ,
  "IL3_Flag3" ,
  "IL4_Flag4" ,
  "IL5_Flag5" ,
  "IL6_Flag6"
)
dept1 <- df[var_dep1]

#Renaming cols for shorthand

names(dept1)[1] <- "Department"

#Checking the stats

count(dept1, dept1$Department)

#Creating "wide" data from "long" format for visualization for Dept Impact

agg_dep1 <- ddply(dept1, .(Department), colwise(sum))

agg_dep_m1 <- melt(agg_dep1, id.vars = "Department")

#GGPlot2 visualization of the trend for Department wise (100% stack)

agg_dep_m1 <- agg_dep_m1 %>%
  group_by(Department) %>%
  mutate(p = value / sum(value))

ggplot(data = agg_dep_m1,
       aes(
         y = value,
         x = Department,
         label = percent(p),
         fill = variable
       )) +
  geom_col(position = "fill") +
  geom_text(position = position_fill(vjust = 0.5), size = 2.7) +
  scale_y_continuous(labels = percent) +
  labs(x = "Department",
       y = "Percentage (%)") +
  scale_fill_manual(values = mycols)

########################## PIC wise ############################################

names(df)

#Renaming shorthand cols

var_pic1 <- c("PIC" ,
              "IL1_Flag1" ,
              "IL2_Flag2" ,
              "IL3_Flag3" ,
              "IL4_Flag4" ,
              "IL5_Flag5" ,
              "IL6_Flag6")
pic1 <- df[var_pic1]

#Checking the stats

x1 <- count(pic1, pic1$PIC)

#Creating "wide" data from "long" format for visualization for PIC Wise data

agg_pic1 <- ddply(pic1, .(PIC), colwise(sum))

agg_pic_m1 <- melt(agg_pic1, id.vars = "PIC")

#Creating top 10 visualization as there are many PIC those who contribute(similaly can create last 10 also)
target <- c("IL3_Flag3" ,
            "IL4_Flag4" ,
            "IL5_Flag5" ,
            "IL6_Flag6")
topweights1 <- agg_pic_m1 %>% filter(variable %in% target) %>%
  arrange_( ~ desc(value)) %>%
  group_by_( ~ variable) %>%
  slice(1:10)

topweights1 <- topweights1[-c(5:11), ]

#GGPLot2 viz for PIC trends

topweights1 <- topweights1 %>%
  group_by(PIC) %>%
  mutate(p = value / sum(value))

ggplot(data = topweights1,
       aes(
         y = value,
         x = PIC,
         label = percent(p),
         fill = variable
       )) +
  geom_col(position = "fill") +
  geom_text(position = position_fill(vjust = 0.5), size = 2.7) +
  scale_y_continuous(labels = percent) +
  labs(x = "PIC",
       y = "Percentage (%)") +
  scale_fill_manual(values = mycols) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

############################# Efficiency Impact ################################

names(df)

#Renaming cols for short-hand in Efficiency Impact
# High <- 1
# Medium <- 2
# Low <- 3
# Nothing <- 4

var_eff1 <-
  c(
    "Efficiency.impact.High...1..Medium..2..Low..3.Nothing...4." ,
    "IL1_Flag1" ,
    "IL2_Flag2" ,
    "IL3_Flag3" ,
    "IL4_Flag4" ,
    "IL5_Flag5" ,
    "IL6_Flag6"
  )

eff1 <- df[var_eff1]

names(eff1)[1] <- "Efficiency"

#Checking the DF Structure

str(eff1)

#Check stats

count(eff1 , eff1$Efficiency)

#Imputing missing values

eff1$Efficiency[is.na(eff1$Efficiency)] <- 4
eff1$Efficiency[eff1$Efficiency == "_"] <- 4
eff1$Efficiency <- as.numeric(as.character(eff1$Efficiency))

#Changing to a dataframe

agg_eff1 <- ddply(eff1, .(Efficiency), colwise(sum))

#Checking the structure

str(agg_eff1)

#Changing from "wide" format to "long" format for ggplot impact visualisation

agg_melt_e1 <- melt(agg_eff1, id.vars = "Efficiency")

y <-
  group_by(agg_melt_e1, Efficiency) %>% mutate(percent = value / sum(value) *
                                                 100)

#Plotting the 100% Stacked Chart for Efficiency Impact

agg_melt_e1 <- agg_melt_e1 %>%
  group_by(Efficiency) %>%
  mutate(p = value / sum(value))

ggplot(data = agg_melt_e1,
       aes(
         y = value,
         x = Efficiency,
         label = percent(p),
         fill = variable
       )) +
  geom_col(position = "fill") +
  geom_text(position = position_fill(vjust = 0.5), size = 2.7) +
  scale_y_continuous(labels = percent) +
  labs(x = "Efficiency",
       y = "Percentage (%)") +
  scale_fill_manual(values = mycols)

############################## End #############################################