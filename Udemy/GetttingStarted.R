# remove.packages("ggplot2")
# $ xcode-select --install <-- needed for mac installation
install.packages("vctrs")
install.packages("ggplot2")

library(ggplot2)

# Look into these:
# 1. Caret 
# https://cran.r-project.org/web/packages/caret/vignettes/caret.html
# install.packages("caret", dependencies = c("Depends", "Suggests"))
# 
# 2. VowPalWabbit (linear models)
# https://github.com/VowpalWabbit/vowpal_wabbit/wiki/Tutorial
# install.packages(??)
# 
# 3. xgboost (tree-based models)
# https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html
# instll.packages("xgboost")
#
# 4. stan (Bayesian stats pkg)
# https://mc-stan.org/
#
# Books: Elements of Statistical Learning (Bible of Data Science), Visual Display of Quantitivate Data (Edward Tufte)
#
# ggplot2 in R for data visualization, but python for data manipulation (Pandas pkg)

N <- 100
counter <- 0
for (i in rnorm(100)) {
  if (i > -1 & i < 1 ) {
    counter <- counter + 1
  }
}

answer <- counter / N
answer

myVector <- c(25, 40) # Stored as Doubles
is.numeric(myVector)  # TRUE
is.integer(myVector)  # FALSE
myVector2 <- c(25L, 40L)  # Stored as Ints
is.numeric(myVector2)     # TRUE
is.integer(myVector2)     # TRUE
?rnorm  # HELP!
?seq

seqA <- seq(1,15,2) # shorthand: 1:15 [beg:end:step]
rep(3, 50)   # replicate 3, 50 times

# mydata <- read.csv(file.choose())

# ggplot(data=mydata[mydata$carat<2.5,], aes(x=carat, y=price, colour=clarity)) +
#  geom_point(alpha=0.1) + 
#  geom_smooth()

qplot(data=diamonds, carat, price, colour=clarity, facets=.~clarity)

c(2) > c(2,3,4)

cohort.data <- read.csv(file.choose())
library(plyr)

# cohort heat map
ggplot(cohort.data, aes(x = month_number, y = reorder(cohort_month, desc(cohort_month)))) +
  geom_raster(aes(fill = percentage)) +
  scale_fill_continuous(guide = FALSE) + # no legend
  geom_text(aes(label=round(cohort.data$percentage, 0)), color = "white") +
  scale_fill_gradient(low = 'red', high = 'green', space = 'Lab') +
  xlab("cohort age") + ylab("cohort") + 
  ggtitle(paste("Retention table (cohort) for","Appointments", "app"))


# animated bar chart - appointments merchants






install.packages("tidyverse")
install.packages("janitor")
install.packages("gganimate")
install.packages("gifski")
install.packages("png")
install.packages("gapminder")
library(tidyverse)
library(janitor)
library(gganimate)
library(gifski)
library(png)
library(gapminder)

function.animateBarCharts <- function (gdp) {
  #select required columns
  gdp <- gdp %>% select(3:15) 
  #filter only country rows
  gdp <- gdp[1:217,]
  gdp_tidy <- gdp %>% 
    mutate_at(vars(contains("YR")),as.numeric) %>% 
    gather(year,value,3:13) %>% 
    janitor::clean_names() %>% 
    mutate(year = as.numeric(stringr::str_sub(year,1,4)))
  
  tidy_fname <- "/Users/dain/Programs/R_Projects/Udemy/data_sets/gdp_tidy.csv"
  write_csv(gdp_tidy, tidy_fname)
  
  
  gdp_tidy <- read_csv(tidy_fname)
  gdp_formatted <- gdp_tidy %>%
    group_by(year) %>%
    # The * 1 makes it possible to have non-integer ranks while sliding
    mutate(rank = rank(-value),
           Value_rel = value/value[rank==1],
           Value_lbl = paste0(" ",round(value/1e9))) %>%
    group_by(country_name) %>% 
    filter(rank <=10) %>%
    ungroup()
  
  staticplot = ggplot(gdp_formatted, aes(rank, group = country_name, 
                                         fill = as.factor(country_name), color = as.factor(country_name))) +
    geom_tile(aes(y = value/2,
                  height = value,
                  width = 0.9), alpha = 0.8, color = NA) +
    geom_text(aes(y = 0, label = paste(country_name, " ")), vjust = 0.2, hjust = 1) +
    geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
    coord_flip(clip = "off", expand = FALSE) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_reverse() +
    guides(color = FALSE, fill = FALSE) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.x = element_line( size=.1, color="grey" ),
          panel.grid.minor.x = element_line( size=.1, color="grey" ),
          plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
          plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
          plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
          plot.background=element_blank(),
          plot.margin = margin(2,2, 2, 4, "cm"))
  
  anim = staticplot + transition_states(year, transition_length = 4, state_length = 1) +
    view_follow(fixed_x = TRUE)  +
    labs(title = 'GDP per Year : {closest_state}',  
         subtitle  =  "Top 10 Countries",
         caption  = "GDP in Billions USD | Data Source: World Bank Data")
  
  # animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
  #        renderer = gifski_renderer("gganim.gif"))
  animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
          renderer = ffmpeg_renderer()) -> for_mp4
  anim_save("animation.mp4", animation = for_mp4 )

}

gdp_data <- read_csv(file.choose())
function.animateBarCharts(gdp_data)

