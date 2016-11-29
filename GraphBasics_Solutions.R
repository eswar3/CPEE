rm(list=(ls(all=TRUE)))
setwd("D:/acads/7306 - Text Mining/influencepropagation&microbloganalysis")

##########################################################################
# The point of this lab is to introduce students to the packages of          
# SNA and Igraph, to cover some basic R commands, to load and manage      
# data, to generate graph visualizations, and to export the data for 
# use elsewhere.                   
##########################################################################

# Loading igraph package
library(igraph) 

# The <- operator sets a variable equal to something. In this case,
# we will set a number of basic R data structures, called "data 
# frames," to hold the contents of the files we will open. 
#
# read.table() is the most common R command for loading data from
# files in which values are in tabular format. The function loads
# the table into a data frame object, which is the basic data type
# for most operations in R. By default, R assumes that the table
# has no header and is delimited by any white space; these
# settings are fine for our purposes here.
# 
# Reading the directly from the "http://sna.stanford.edu/" URL


advice_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Advice.txt')
friendship_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Friendship.txt')
reports_to_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-ReportsTo.txt')

# View(advice_data_frame)
# View(friendship_data_frame)
# View(reports_to_data_frame)

# The attribute data for this lab is in a comma-separated-value
# (CSV) file. read.csv() loads a CSV file into a data frame
# object. In this case, we do have a header row, so we set
# header=T, which tells R that the first row of data contains
# column names.
attributes <- read.csv('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-Attributes.csv', header=T)
attributes

# Terminology in SNA:
# "Ego" : represents an individual
# "Alter" : represents the node to which an individual is directly connected to
# "Tie"   : represents the relationship between Ego and Alter (In this case, if they are connected or not)
 
# Assigning column names to the data frames
colnames(advice_data_frame) <- c('ego', 'alter', 'advice_tie')
#head(advice_data_frame)

colnames(friendship_data_frame) <- c('ego', 'alter', 'friendship_tie')
#head(friendship_data_frame)

colnames(reports_to_data_frame) <- c('ego', 'alter', 'reports_to_tie')
#head(reports_to_data_frame)

# Before we merge these data, we need to make sure 'ego' and 'alter' are the
# same across data sets. We can compare each row using the == syntax. 
# The command below should return TRUE for every row if all ego rows
# are the same for advice and friendship:
advice_data_frame$ego == friendship_data_frame$ego

# That's a lot of output to sort through. Instead, we can just have R return 
# which row entries are not equal using the syntax below:
which(advice_data_frame$ego != friendship_data_frame$ego)

# Repeat for other variables
which(advice_data_frame$alter != friendship_data_frame$alter)
which(reports_to_data_frame$alter != friendship_data_frame$alter)
which(reports_to_data_frame$ego != friendship_data_frame$ego)

# alter and ego columns are same in all the 3 data frames.
# combine the 3 data frames in to a single data frame. 
krack_full_data_frame <- data.frame(ego = advice_data_frame[,1],
                                    alter = advice_data_frame[,2],
                                    advice_tie = advice_data_frame[,3],
                                    friendship_tie = friendship_data_frame[,3], 
                                    reports_to_tie = reports_to_data_frame[,3])
head(krack_full_data_frame)


# Data processing.
# Reduce to non-zero edges so that the edge list only contains
# actual ties of some type.
krack_full_nonzero_edges <- subset(krack_full_data_frame, 
                                   (advice_tie > 0 | friendship_tie > 0 | reports_to_tie > 0))
head(krack_full_nonzero_edges)

# Now we can import our data into a "graph" object using igraph's 
# graph.data.frame() function. Coercing the data into a graph
# object is what allows us to perform network-analysis techniques.
krack_full <- graph.data.frame(krack_full_nonzero_edges) 
head(krack_full)
plot(krack_full, edge.arrow.size = 0.1, layout = layout_with_fr)
summary(krack_full)

# By default, graph.data.frame() treats the first two columns of 
# a data frame as an edge list and any remaining columns as 
# edge attributes. Thus, the 232 edges appearing in the summary()
# output refer to the 232 pairs of vertices that are joined by 
# *any type* of tie. The tie types themselves are listed as edge 
# attributes.

# To get a vector of edges for a specific type of tie, use the 
# get.edge.attribute() function.
get.edge.attribute(krack_full, 'advice_tie')
get.edge.attribute(krack_full, 'friendship_tie')
get.edge.attribute(krack_full, 'reports_to_tie')

# If you would like to symmetrize the network, making all 
# asymmetric ties symmetric, use the as.undirected()
# function: 
krack_full_symmetrized <- as.undirected(krack_full, mode='collapse')
plot(krack_full_symmetrized, edge.arrow.size = 0.1)
summary(krack_full_symmetrized)


# ADDING VERTEX ATTRIBUTES TO A GRAPH OBJECT
# Create a vector of vertex labels, in this case 1:n
attributes = cbind(1:length(attributes[,1]), attributes)

krack_full <- graph.data.frame(d = krack_full_nonzero_edges, 
                               vertices = attributes) 

# Note that we now have 'AGE,' 'TENURE,' 'LEVEL,' and 'DEPT'
# listed alongside 'name' as vertex attributes.
summary(krack_full)

# We can see a list of the values for a given attribute for all of
# the actors in the network.
get.vertex.attribute(krack_full, 'AGE')
get.vertex.attribute(krack_full, 'TENURE')
get.vertex.attribute(krack_full, 'LEVEL')
get.vertex.attribute(krack_full, 'DEPT')


###
# 4. VISUALIZE THE NETWORKS
###

# We can use R's general-purpose plot() method to generate custom
# visualizations of the network.

# R only lets us look at one plot at a time.  To make our work easier
# we will save our plots as PDF files.  To jus create a plot execute 
# the code between the PDF function and "dev.off()".

# In order to save PDF files we must tell R where to put them.  We do
# this with the setwd() command.  You must put the full path to the
# folder where you will output the files here.

# First, let's plot the network with all possible ties.
pdf("1.1_Krackhardt_Full.pdf")
plot(krack_full, edge.arrow.size = 0.1)
# Clearing the Plots
dev.off()

# This is a bit of a jumble, so let's look at the networks for
# single edge types.

# advice only
krack_advice_only <- delete.edges(krack_full, 
                                  E(krack_full)[get.edge.attribute(krack_full,
                                  name = "advice_tie") == 0])
summary(krack_advice_only)
pdf("1.2_Krackhardt_Advice.pdf")
plot(krack_advice_only, edge.arrow.size = 0.1)
# Clearing the Plots
dev.off()

# friendship only
krack_friendship_only <- delete.edges(krack_full, 
                                      E(krack_full)[get.edge.attribute(krack_full, 
                                      name = "friendship_tie") == 0])
summary(krack_friendship_only)
pdf("1.3_Krackhardt_Friendship.pdf")
plot(krack_friendship_only)
dev.off()

# reports-to only

krack_reports_to_only <- delete.edges(krack_full, 
                                      E(krack_full)[get.edge.attribute(krack_full, 
                                      name = "reports_to_tie") == 0])
summary(krack_reports_to_only)
pdf("1.4_Krackhardt_Reports.pdf")
plot(krack_reports_to_only, edge.arrow.size = 0.1)
dev.off()

# Still kind of messy, so let's clean things up a bit. For 
# simplicity, we'll focus on reports_to ties for now.

# First, we can optimize the layout by applying the layout 
# algorithm to the specific set of ties we care about. Here 
# we'll use Fruchterman-Rheingold; other options are 
# described in the igraph help page for "layout," which 
# can be accessed by entering ?layout.

reports_to_layout <- layout.fruchterman.reingold(krack_reports_to_only)
pdf("1.5_Krackhardt_Reports_Fruchterman_Reingold.pdf")
plot(krack_reports_to_only, 
     layout=reports_to_layout,edge.arrow.size = 0.1)
dev.off()

# Now let's color-code vertices by department and clean up the 
# plot by removing vertex labels and shrinking the arrow size. 
dept_vertex_colors = get.vertex.attribute(krack_full,"DEPT")
colors = c('Black', 'Red', 'Blue', 'Yellow', 'Green')
dept_vertex_colors = colors[dept_vertex_colors+1]
pdf("1.6_Krackhardt_Reports_Color.pdf") 
plot(krack_reports_to_only, 
     layout=reports_to_layout, 
     vertex.color=dept_vertex_colors, 
     vertex.label=NA, 
     edge.arrow.size=.1)
dev.off() 
# Now let's set the vertex size by tenure.
tenure_vertex_sizes = get.vertex.attribute(krack_full,"TENURE")

pdf("1.7_Krackhardt_Reports_Vertex_Size.pdf") 
plot(krack_reports_to_only, 
     layout=reports_to_layout, 
     vertex.color=dept_vertex_colors, 
     edge.arrow.size=.1,
     vertex.size=tenure_vertex_sizes)
dev.off() 

##COMMUNITIES
ceb <- cluster_edge_betweenness(krack_reports_to_only) 
plot(ceb, krack_reports_to_only)
ceb_frnd <- cluster_edge_betweenness(krack_friendship_only)
plot(ceb_frnd, krack_reports_to_only, layout = reports_to_layout)
ceb_advi <- cluster_edge_betweenness(krack_advice_only)
plot(ceb_advi, krack_advice_only, layout = reports_to_layout)
# Now let's incorporate additional tie types. We'll use the 
# layout generated by the reports-to ties but overlay the 
# advice and friendship ties in red and blue.

##AUTHORITY PLOT
authority.score(krack_friendship_only)$vector ->a
plot(krack_advice_only, vertex.size = a*20, layout = reports_to_layout)

##HIGHLIGHTING SHORTEST PATH
shortest_paths(krack_reports_to_only, from = 1, to = 13, mode = 'all')->a
vcol <- rep("gray40", vcount(krack_reports_to_only))
vcol[unlist(a$vpath)] <- "gold"
plot(krack_reports_to_only, vertex.color=vcol, layout = reports_to_layout)

##FULL PLOT

tie_type_colors = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5), rgb(0,0,0,0.5))
E(krack_full)$color[ E(krack_full)$advice_tie==1 ] = tie_type_colors[1]
E(krack_full)$color[ E(krack_full)$friendship_tie==1 ] = tie_type_colors[2]
E(krack_full)$color[ E(krack_full)$reports_to_tie==1 ] = tie_type_colors[3]
E(krack_full)$arrow.size=.1 
V(krack_full)$color = dept_vertex_colors
V(krack_full)$frame = dept_vertex_colors

pdf("1.8_Krackhardt_Overlayed_Ties.pdf")
plot(krack_full, 
     layout=reports_to_layout, 
     vertex.color=dept_vertex_colors, 
     vertex.label=NA, 
     edge.arrow.size=.1,
     vertex.size=tenure_vertex_sizes)


# Add a legend. Note that the plot window must be open for this to 
# work.
legend(1, 
       1.25,
       legend = c('Advice', 
                  'Friendship',
                  'Reports To'), 
       col = tie_type_colors, 
       lty=1,
       cex = .7)
dev.off() 

# Another option for visualizing different network ties relative 
# to one another is to overlay the edges from one tie type on the 
# structure generated by another tie type. Here we can use the
# reports-to layout but show the friendship ties:

pdf("1.9_Krackhardt_Overlayed_Structure.pdf")
plot(krack_friendship_only, 
     layout=reports_to_layout, 
     vertex.color=dept_vertex_colors, 
     vertex.label=c(1:21), 
     edge.arrow.size=.2,
     vertex.size=tenure_vertex_sizes, 
     main='Krackhardt High-Tech Managers')
degree(krack_friendship_only)

dev.off() 


###
# 5. EXPORT THE NETWORK
###

# The write.graph() function exports a graph object in various
# formats readable by other programs. There is no explicit
# option for a UCINET data type, but you can export the graph
# as a Pajek object by setting the 'format' parameter to 'pajek.'
# Note that the file will appear in whichever directory is set 
# as the default in R's preferences, unless you previously 
# changed this via setwd().
write.graph(krack_full, file='krack_full.dl', format="pajek")

# For a more general file type (e.g., importable to Excel),
# use the "edgelist" format. Note that neither of these will
# write the attributes; only the ties are maintained.
write.graph(krack_full, file='krack_full.txt', format="edgelist")
