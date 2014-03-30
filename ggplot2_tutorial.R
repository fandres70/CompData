#########################################################################
# ggplot2 examples taken from online tutorial
# link: http://inundata.org/2013/04/10/a-quick-introduction-to-ggplot2/
#########################################################################

require(ggplot2)
require(ggthemes)

# Scatterplots #
#--------------#

ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(size=3)
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point(size=3)

set.seed(10)
d2 <- diamonds[sample(1:dim(diamonds)[1], 1000), ]
ggplot(data=d2, aes(x=carat, y=price, color=color)) + geom_point(size=3)

# Histograms #
#------------#

ggplot(data=faithful, aes(x=waiting)) + geom_histogram(binwidth=30)
ggplot(data=faithful, aes(x=waiting)) + geom_histogram(binwidth=30, col="black")


# Line plots #
#------------#

address <- "https://raw.github.com/karthikram/ggplot-lecture/master/climate.csv"
climate2 <- read.csv(address, raw.names=1)
ggplot(climate, aes(Year, Anomaly10y)) + geom_line()
ggplot(climate, aes(Year, Anomaly10y)) + geom_line() + theme_bw()

p <- ggplot(climate, aes(Year, Anomaly10y)) + geom_line()
p <- p + geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y), alpha=0.1)
p + theme_economist() + scale_fill_economist()

# 3 lines instead of ribbon
ggplot(climate) + geom_line(aes(x=Year, y=Anomaly10y)) + theme_bw()
+ geom_line(aes(x=Year, y=Anomaly10y-Unc10y)) 
+ geom_line(aes(x=Year, y=Anomaly10y+Unc10y)) 

# even better, specify common x in original call
ggplot(climate, aes(x=Year)) + geom_line(aes(y=Anomaly10y)) + theme_bw()
+ geom_line(aes(y=Anomaly10y-Unc10y)) + geom_line(aes(y=Anomaly10y+Unc10y))


# Bar plots #
#-----------#

# original idea, create an aggreagate dataset and specify y, and stat="identity"
d3 <- aggregate(data=d2, carat ~ clarity + cut, length) ##take length of carat (count)
names(d3)[3] <- "count"  ## rename it so it makes sense
ggplot(d3, aes(x=clarity, y=count, fill=cut)) + geom_bar(stat="identity", position="dodge")

# better way (suggested by warning message), do not specify y and use stat="bin"
p <- ggplot(d2, aes(x=clarity, fill=cut)) + geom_bar(stat="bin", position="dodge") 
p + theme_solarized() + scale_fill_solarized()
p  + theme_wsj() + scale_fill_wsj(palette="rgby") ## rgby red_green black_green colors6


# generate variable to use for bar color
climate$sign <- ifelse(climate$Anomaly10y >=0, "Rising", "Falling")
climate$sign <- factor(climate$sign, levels=c("Falling", "Rising"))

ggplot(climate, aes(x=Year, y=Anomaly10y, fill=sign)) + geom_bar(stat="identity")


# Density plots #
#---------------#

ggplot(faithful, aes(waiting)) + geom_density() ## theme_classic() looks like base graphics
ggplot(faithful, aes(waiting)) + geom_density(fill="steelblue", alpha=0.3)
ggplot(faithful, aes(waiting)) + geom_line(stat="density")

#change iris dataset to long
require(reshape2)
df <- melt(iris, id.vars="Species")

require(RColorBrewer) ## for defined palettes
display.brewer.all()  ## show palettes

p <- ggplot(df, aes(x=Species, y=value, fill=variable)) 
p <- p + geom_bar(stat="identity", position="dodge")
p + scale_fill_brewer(palette="Set3") + theme_bw()

# graph with maually specified palette
p <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species))
p <- p + geom_point() + facet_grid(Species ~.)
p + scale_color_manual(values=c("red", "green", "blue"))


# Adding smooth lines #
#---------------------#
p <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species))
p <- p + geom_point(aes(shape=Species), size=3) + geom_smooth(method="lm")
 
p + theme_economist_white()  ## single panel
p + facet_grid(Species ~ .)  ## set vertical facet grid


# Themes #
#--------#
p <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species))
p <- p + geom_point(size=3, shape=16) + facet_wrap(~Species)
p + theme(legend.key = element_rect(fill=NA), 
          legend.position = "bottom",
          strip.background = element_rect(fill=NA),
          axis.title.y = element_text(angle=0))

# themes in ggthemes: economist, solarized (light option), wsj, tufte, igray (inverted gray)


# Adding scale #
#--------------#

library(MASS)
p <- ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot(width=0.2)
p <- p + scale_y_continuous(labels=(paste(1:4, "Kg")), breaks=1000*1:4)
p + theme_bw()

# making sure the grid lines for both graphs are 1cm appart
p <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point(size=4, shape=16)
p <- dd + facet_grid(.~Species) + scale_y_continuous(breaks=2:8, labels=paste(2:8, "cm"))
p + theme_bw()

# using custom color gradient to fill
p <- ggplot(faithful, aes(x=waiting)) + geom_histogram(aes(fill=..count..), color="black", binwidth=2)
p <- p + scale_fill_gradient(low="green", high="red")
p


# Fun with ggmaps #
#-----------------#
require(ggmaps)

# get map based on address match and store it into a gg object
m <- ggmap(get_map(location="Stepanska 43, prague 1", zoom=19, source="osm"))

# manipulatye it like ggplot(), say by adding the location of the address as a point
address <- geocode("Stepanska 43, prague 1")  ## very useful function to get GPS coordinates
m + geom_point(data=address, aes(x=lon, y=lat), size=5, color="red")

###################################################
###################################################

titanic = read.csv("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic3.csv")

titanic$survived <- factor(titanic$survived, levels=c(0, 1), labels=c("Died", "Survived"))
titanic$pclass <- factor(titanic$pclass, levels=c(1, 2, 3), labels=c("First", "Second", "Third"))

titanic$age.g <- cut(titanic$age, 10) ## making a age groups
# after inspection, give meaningful names
levels(titanic$age.g) <- paste(seq(0, 72, by=8), seq(8, 80, by=8), sep="-")

require(plyr)
td <- ddply(titanic, c("pclass", "age.g", "sex"), 
            summarize, ps=sum(survived=="Survived")/length(survived))

p <- ggplot(td, aes(x=age.g, y=ps)) + theme_bw()
p + geom_point()  ## cannot see why we have a clear division in data

# use other scales to see difference
display.brewer.all()
p + geom_point(aes(color=pclass, shape=sex), size=5) + scale_color_brewer(palette="OrRd")

# better in separate panels
p + geom_point(aes(shape=sex, color=sex), size=4) + facet_wrap(~pclass) + theme(axis.text.x=element_text(angle=90))

# sex in separate panels too
p + geom_point(size=4) + facet_grid(sex~pclass) + theme(axis.text.x=element_text(angle=90))

