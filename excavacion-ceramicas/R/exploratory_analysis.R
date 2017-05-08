

library(xlsx)
library(gexGA)
library(dplyr)
library(scales)

# Read the data set
ddata <- read.xlsx("data/piezas.xlsx", 1, 
                   colIndex = c(1:3, 23:25),
                   
                   rowIndex = c(1, 3:840))  # The ommited rows
                                            # contains NA's
head(ddata)  # Look up few observations
tail(ddata)


# Counting by "Tipo.o.Variedad"

count.tipo <- group_by(ddata, Tipo.o.Variedad) %>%
  summarise(value = n()) %>%
  arrange(value)

# Cast to factor to keep the order
count.tipo$Tipo.o.Variedad <- factor(count.tipo$Tipo.o.Variedad,
                                     levels = count.tipo$Tipo.o.Variedad)

# Create a copy and just keep the top
N <- 50
count.tipo2 <- count.tipo[nrow(count.tipo) - N:0, ]
count.tipo2 <- mutate(count.tipo2, labs = paste0(comma(value),
                                                 " (",
                                                 percent(round(value /
                                                                 sum(count.tipo$value), 3)),
                                                 ")"))

# Function for scatter plots

MyBarBlot <- function(data, x.var, y.var,
                      labs,
                      x.title = x.var,
                      y.title = y.var,
                      fill.color){
  p1 <- ggplot(data = data, aes_string(x = x.var, y = y.var,
                                              label  = labs))
  
  p1 <- p1 + geom_bar(fill = fill.color, stat = "identity")
  p1 <- p1 + coord_flip()
  p1 <- p1 + geom_text(fontface = 1, color = "black", size = 2.5, hjust = -0.05)
  
  p1 <- p1 + scale_y_continuous(breaks = function(lims) seq(0, lims[2], length.out = 6),
                                limits = c(0, 1.1*max(data[, y.var])))
  
  p1 <- p1 + theme(panel.grid.major = element_line(colour = "#C7C3A5",
                                                   linetype = "dotted"),
                   axis.ticks = element_blank(),
                   panel.background = element_blank(),
                   axis.title = element_text(size = 9),
                   axis.text = element_text(size = 8),
                   axis.text.x = element_text(angle = 90))
  
  p1  <- p1 + xlab(x.title) + ylab(y.title)
  p1
}


# Make a Scatter plot
pf <- MyBarBlot(count.tipo2, x.var = "Tipo.o.Variedad",
                y.var = "value",
                labs = "labs",
                x.title = "Tipo o Variedad",
                y.title = "Número de piezas",
                fill.color = "RoyalBlue")
