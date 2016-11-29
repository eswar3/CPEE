###LET'S LEARN THE BASICS OF REPRESENTING A GRAPH IN R

library(igraph)

#EVERY GRAPH HAS TWO MAIN COMPONENTS - NODES AND EDGES
g1 <- graph(c(1,2, 2,3, 3,1)) #EDGE INFORMATION IS GIVEN HERE. Ex: 1--2, 2--3, 3--1
g1
plot(g1)

connections = c(1,2, 2,3, 3,1, 3,4, 4,3, 4,4)
g1 <- graph(connections)
plot(g1)

g1 <- graph(connections, n = 10)
plot(g1, edge.arrow.size=.5)

#edge paths can be directly given as follows
g1 <- graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j)
plot(g1)

#Edges and Vertices (nodes) can be accessed as follows
E(g1)
V(g1)


#Entire graph can be converted to a matrix
g1[]
g1[1,]

g4 <- graph( c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"), 
             isolates=c("Jesse", "Janis", "Jennifer", "Justin") )

#One could access all these attributes in a plot
plot(g4, edge.arrow.size=.5, vertex.color="blue", vertex.size=15, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)

#Many times vertices and edges will have extra attributes 
V(g4)$name
V(g4)$gender <- c(rep('Male', 4), rep('Female', 2), 'Male')
E(g4)$weight <- 10
vertex_attr(g4)
edge_attr(g4)


#We can make use of these attributes in plotting
plot(g4, edge.arrow.size=.5, vertex.color=c( "green", "blue")[1+(V(g4)$gender=="Male")] , vertex.size=15, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)

g4
# IGRAPH DNW- 7 4 MEANS [D = DIRECTED, N = NAMED, W = WEIGHTED] 7 NODES AND 4 EDGES
#   + attr: name (v/c), gender (v/c), weight (e/n)
# + edges (vertex names):
#   [1] John->Jim  Jim ->Jack Jim ->Jack John->John


###EXERCISE MAKE AN EMPTY GRAPH

ex1 <- graph(c(), n = 40)
plot(ex1, vertex.label = NA, vertex.size = 5)

ex1 <- make_empty_graph(40)
plot(ex1, vertex.label = NA, vertex.size = 5)

###FULLY CONNECTED GRAPH
ex1 <- make_full_graph(5)
plot(ex1, vertex.label = NA, vertex.size = 5, edge.arrow.size = 0.3)

###STAR GRAPH
st <- make_star(40)
plot(st, vertex.size=10, vertex.label=NA, edge.arrow.size = 0.3)

###TREE GRAPH
tr <- make_tree(40, children = 3, mode = "undirected")
plot(tr, vertex.size=10, vertex.label=NA) 


###RING GRAPH
rn <- make_ring(40)
plot(rn, vertex.size=10, vertex.label=NA)

###ERDON-RENYI GRAPH
er <- sample_gnm(n=100, m=40) 
plot(er, vertex.size=6, vertex.label=NA)  


###SMALLWORLD
sw <- sample_smallworld(dim=2, size=10, nei=1, p=0.1)
plot(sw, vertex.size=6, vertex.label=NA, layout=layout_in_circle)

###PREFERENTIAL ATTACHMENT
net <-  sample_pa(n=100, power=1.5, m = 1,  directed=F)
plot(net, vertex.size=6, vertex.label=NA)

###REWIRE
rn.rewired <- rewire(rn, each_edge(prob=0.1))
plot(rn.rewired, vertex.size=10, vertex.label=NA)

##MULTIPLE GRAPHS
plot(rn %du% tr, vertex.size=10, vertex.label=NA) 

###CHANGE LAYOUT
k1 <- layout_in_circle(rn.rewired)
k2 <- layout_randomly(rn.rewired)
plot(rn.rewired, layout = k1, vertex.size = 7)
plot(rn.rewired, layout = k2, vertex.size = 7)



nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
plot(net, vertex.label = NA, edge.arrow.size = 0.3)
net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net, vertex.label = NA, edge.arrow.size = 0.3)


plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7) 

##SETTING PLOT ATTRIBUTES MANUALLY
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]
V(net)$size <- V(net)$audience.size*0.7
V(net)$label.color <- "black"
V(net)$label <- NA
# vertex.label= V(net)$media
E(net)$width <- E(net)$weight/6
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12
plot(net, vertex.label = NA)
legend(x=-1.5, y=1.8, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=1, cex=.8, bty="n", ncol=1)

net.news <- net

##LET'S COLOR THE EDGE BASED ON ORIGIN
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]
plot(net, edge.color=edge.col, edge.curved=.1) 

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]
par(mfrow=c(3,5), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, edge.color = edge.col, layout=l, main=layout) }
dev.off()

################################
#IMPROVING PLOTS
################################
###SPARSIFY NETWORK. 
###LET'S PLOT ONLY THOSE EDGES WHOSE WEIGHT IS LARGER THAN THE MEAN
cut.off <- mean(links$weight) 
net.sp <- delete_edges(net, E(net)[weight<cut.off])
plot(net.sp, vertex.label= V(net)$media)


###EXERCISE: PLOT DIFFERENT COLOR FOR "hyperlink" AND "mention" 
###  ... access type using E(net)$type
E(net)$type

plot(net, edge.color = c('dark red', 'slategrey')[(E(net)$type == 'mention')+1])


net.m <- net - E(net)[E(net)$type=="hyperlink"] # another way to delete edges
net.h <- net - E(net)[E(net)$type=="mention"]

# Make sure the nodes stay in place in both plots:
par(mfrow=c(1,2))
l <- layout_with_fr(net)
plot(net.h, vertex.color="orange", layout=l, main="Tie: Hyperlink", vertex.label= V(net)$media)
plot(net.m, vertex.color="lightsteelblue2", layout=l, main="Tie: Mention", vertex.label= V(net)$media)
dev.off()

#######NETWORK DESCRIPTIVES
net <- graph(c(1,2, 2,3, 3,4, 4,1, 4,2, 2,4, 5,3, 5,6))

ecount(net)
vcount(net)

##DEGREE
plot(net)

degree(net, mode="in")
degree(net, mode='out')
degree(net)


##EDGE DENSITY
edge_density(net)
#same as
ecount(net)/(vcount(net)*(vcount(net)-1))

reciprocity(net)

net.sample <-  sample_pa(n=100, power=1.31, m = 1,  directed=F)
plot(net.sample, vertex.label = NA, vertex.size = 5, edge.arrow.size = 0.1)
deg <- degree(net.sample, mode="all")
plot(net.sample, vertex.label = NA, vertex.size = 1+7*log10(deg*2), edge.arrow.size = 0.1)


##DEGREE DISTRIBUTION
deg.dist <- degree_distribution(net.sample, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")
d <- degree(net.sample)
hist(d, breaks = c(1:40))
plot_degree_distribution = function(graph) {
  # calculate degree
  d = degree(graph, mode = "all")
  dd = degree.distribution(graph, mode = "all", cumulative = FALSE)
  degree = 1:max(d)
  probability = dd[-1]
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # plot
  plot(probability ~ degree, log = "xy", xlab = "Degree (log)", ylab = "Probability (log)", 
       col = 1, main = "Degree Distribution")
}
plot_degree_distribution(sample_pa(n=100000, power=1.21, m = 1,  directed=F)) 
##PLAY WITH power AND SEE HOW THE GRAPH VARIES

##CLOSENESS
##inverse average distance to every other vertex
net <- graph(c(1,2, 2,3, 3,4, 4,1, 4,2, 2,4, 5,3, 5,6))
closeness(net, mode="all", weights=NA) 
centr_clo(net, mode="all", normalized=T) 

##BETWEENNESS
##fraction/number of shortest paths that pass through the vertex
betweenness(net)


##HUB AND AUTHORITY
hs <- hub_score(net.news, weights=NA)$vector
as <- authority_score(net.news, weights=NA)$vector

par(mfrow=c(1,2))
l <- layout_with_fr(net.news)
plot(net.news, vertex.size=1+log10(hs+1)*30, main="Hubs", vertex.label = NA, layout = l)
plot(net.news, vertex.size=1+log10(as+1)*30, main="Authorities", vertex.label = NA, layout = l)
dev.off()


###DISTANCE
net <- graph(c(1,2, 2,3, 3,4, 4,1, 4,2, 2,4, 5,3, 5,6))
plot(net)
distances(net, 1, mode = 'all')
distances(net, 1, mode = 'out')
shortest_paths(net, from = 1, to = 6, mode = 'all')

#diameter
plot(net.sample, vertex.size = 4, vertex.label = NA)
diameter(net.sample)
get_diameter(net.sample)
vcol <- rep('gray40', vcount(net.sample))
'gold' -> vcol[get_diameter(net.sample, directed=T)] 
plot(net.sample, vertex.label = NA, vertex.size = 1+7*log10(deg*2), vertex.color = vcol, edge.arrow.size = 0.1)

mean_distance(net.news, directed=F)
mean_distance(net.news, directed=T)
dist.from.NYT <- distances(net.news, v=V(net.news)[media=="NY Times"], to=V(net.news), weights=NA)

news.path <- shortest_paths(net.news, 
                            from = V(net.news)[media=="MSNBC"], 
                            to  = V(net.news)[media=="New York Post"],
                            output = "both") # both path nodes and edges
ecol <- rep("gray80", ecount(net.news))
ecol[unlist(news.path$epath)] <- "orange"
vcol <- rep("gray40", vcount(net.news))
vcol[unlist(news.path$vpath)] <- "gold"
ew <- rep(2, ecount(net))
ew[unlist(news.path$epath)] <- 4
plot(net.news, vertex.color=vcol, edge.color=ecol, 
     edge.width=ew, edge.arrow.mode=0)


##EDGE HIGHLIGHTING - USE THE FUNCTION incident
inc.edges <- incident(net.news,  V(net.news)[media=="Wall Street Journal"], mode="all")
ecol <- rep("gray80", ecount(net.news))
ecol[inc.edges] <- "orange"
vcol <- rep("grey40", vcount(net.news))
vcol[V(net.news)$media=="Wall Street Journal"] <- "gold"
plot(net.news, vertex.color=vcol, edge.color=ecol)


###NEIGHBORS

neigh.nodes <- neighbors(net.news, V(net.news)[media=="Wall Street Journal"], mode="out")
# Set colors to plot the neighbors:
vcol[neigh.nodes] <- "#ff9d00"
plot(net.news, vertex.color=vcol)

##TRIADS
net <- graph(c(1,2, 2,3, 3,4))
plot(net)
triad.census(net)

g <- sample_gnm(15, 45, directed = TRUE)
plot(g)
triad.census(g)
choose(15,3)
sum(triad.census(g))

##CLIQUES
cliques(as.undirected(g))->cliq.g
tail(cliq.g)
inc.edges <- incident(g,  c(10, 12, 14, 15))
ecol <- rep("gray80", ecount(g))
ecol[inc.edges] <- "orange"
highlight.color = c('black', 'gold')
plot(g, vertex.color = highlight.color[V(g)%in%c(10, 12, 14, 15)+1], vertex.label = NA, 
     vertex.size = 10, edge.arrow.size = 0.1)


###COMMUNITY DETECTION
ceb <- cluster_edge_betweenness(net.news) 
dendPlot(ceb, mode="hclust")
plot(ceb, net.news) 
membership(ceb)

kc <- coreness(net.news, mode="all")
plot(net.news, vertex.size=kc*6, vertex.label=kc, vertex.color=c('gray', 'gold', 'green', 'tomato')[kc])
#GO TO - http://igraph.org/r/doc/triad_census.html

#adapted from http://kateto.net/networks-r-igraph
