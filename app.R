#Wine ratings application
library(shiny)
library(ggplot2)
library(fmsb)
library(reshape)
library(scales)
#-------------------------------------------------------------------------------
# Load wine ratings csv to data frame:
wineDFraw = read.csv("winemag-data-130k-v2.csv")

#-------------------------------------------------------------------------------
# Filter raw data:
#only want reviews of the top 10 most commonly rated wine varieties:
varieties = sort(table(wineDFraw$variety), decreasing = TRUE)[1:10]
varieties = names(varieties)
wineDF = wineDFraw[wineDFraw$variety %in% varieties,]

#Keep only some columns:
cols = c("country", "description", "points", "price", "variety")
wineDF = wineDF[cols]

#Omit any rows with NA values
wineDF = na.omit(wineDF)
#-------------------------------------------------------------------------------
#App Parameters:
ndesc = 5 # number of descriptor words to display in radar plots
figH = "600px" # height of plot figure in dashboard
figW = "1400px" #width of plot figure in dashboard
colors = c("#F8766D", "#00A9FF", "#00BE67", "#C77CFF", 
           "#00BFC4", "#FF61CC", "#7CAE00", "#CD9600")
#-------------------------------------------------------------------------------
# This function takes wineDF (or a subset of), and outputs a char vec of top ndesc
# descriptive words
descriptors = function(wineDF){
  #Vector of common wine words with no value:
  dumbwords = c(unlist(strsplit(tolower(varieties)," ")),'and','wine','the','for',
                'but','now','are','well',"it's",'this','well','there','into',
                'with','that','flavors','aromas','from','notes','drink','finish',
                'nose','palate','has','red','its','texture','shows','through',
                'vineyard','black','offers','alongside','opens','made','while',
                'delivers','more','good','touch','white','very','years',
                'structure','aging','will','character','some','age','ready',
                'aftertaste','attractive','just','pink','all','yet','franc',
                'fore')
  
  winedescriptors = c()
  
  for (description in wineDF$description){
    description = tolower(description) #convert all characters to lower case
    description = gsub(",", "", description) #remove commas
    description = gsub(".", "", description, fixed = TRUE) #remove periods
    descvec = unlist(strsplit(description, " ")) # string to char vec of words
    descvec = descvec[nchar(descvec)>2] #remove 1 & 2 letter words
    descvec = descvec[!descvec %in% dumbwords] #remove the dumb words
    descvec[descvec=="fruity" | descvec=="fruits"] = "fruit" #standardize fruit descriptor
    
    # combining all descriptive words as char vec:
    winedescriptors = c(winedescriptors,descvec) 
  }
  result = sort(table(winedescriptors), decreasing = TRUE)[1:ndesc]
  #Data frame of top ndesc descriptive words:
  return(data.frame(result))
}
#-------------------------------------------------------------------------------
# This function takes a varietyDF with clusters column, and outputs a group of
# radar plots showing descriptive word frequencies by cluster
radarplotsfig = function(varietyDF){
  clustervals = c(1:max(varietyDF$cluster))
  totclus = max(clustervals)
  m = 1

  par(mfrow=c(2,4), mar = c(m, m, m, m))
  for (clus in clustervals){
    varclusDF = varietyDF[varietyDF$cluster == clus,]
    plotRadar(varclusDF,colors[clus])
  }
}
#-------------------------------------------------------------------------------
# This takes a varietyDF with one cluster value in column, builds one radarchart
plotRadar = function(varclusDF,color){
  data = descriptors(varclusDF)
  radarDF = data.frame(t(data[2]))
  colnames(radarDF) = t(data[1])
  maxv = max(radarDF)
  clusval = varclusDF$cluster[1]
  
  radarDF = rbind(rep(maxv,ndesc) , rep(0,ndesc) , radarDF[,c(5,1,2,3,4)])
  return( 
    radarchart(radarDF, pfcol = scales::alpha(color, 0.88),
                      cglcol = "black", cglty = 1, axistype = 1,
                      caxislabels = ceiling(c(0, maxv/4, maxv/2, maxv*3/4, maxv)),
                      title=paste("Cluster",clusval), vlcex = 1.7, calcex = 1.6,
                      axislabcol='black')
  )
}
#-------------------------------------------------------------------------------
# This creates the kmeans SSE vs. Number of Clusters plot, for each wine variety
plotSSE = function(wineDF){
  SSE = data.frame(clusters=c(1:12))
  for(variety in varieties){
    varietyDF = wineDF[wineDF$variety==variety,]
    wss = NA
    for (i in SSE$clusters){
      wss[i] = kmeans(scale(c(varietyDF$price,varietyDF$points)), centers=i)$tot.withinss
    }
    SSE[variety] = wss
  } 
  SSE.melted <- melt(SSE, id = "clusters")
  names(SSE.melted)[2] = "Variety"
  return(
    ggplot(data = SSE.melted, aes(x = clusters, y = value, color = Variety)) +
      geom_point() + geom_line() + scale_y_continuous(trans='log10') +
      scale_x_continuous(breaks=pretty_breaks()) + 
      xlab("Number of Clusters") + ylab("Sum Squared Error") + 
      ggtitle("SSE by Number of Clusters for each Wine Variety")
  )
}
#-------------------------------------------------------------------------------
# User-Interface Function:
ui <- fluidPage(
  navbarPage(
    "Wine Ratings Analysis",
    tabPanel("Dashboard",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variety", h5("Select which wine variety to analyze:"), choices = as.list(varieties), selected = tail(as.list(varieties),1)),
                 sliderInput("clusters", h5("Select Number of Clusters:"), min = 1, max = 8, value = 4) 
               ),
               mainPanel(
                 h3("Wine Price vs. Rating", align="left"),
                 plotOutput(outputId = "kmPlot", width = figW, height = figH),
                 br(),
                 h3("Descriptors by Cluster", align="left"),
                 p("These charts show the top five most-frequent words used to describe wines in each cluster."),
                 br(),
                 plotOutput(outputId = "radPlots", width = figW, height = figH)
               )
             )),
    tabPanel("Data Collected",
             h3("About the Data:"),
             p("The dataset used for this analysis is a csv file containing wine reviews of roughly 130,000 different wines. The wine reviews were scraped from WineEnthusiast by 'ZACKTHOUTT' on kaggle. Each review contains information such as the country of origin, wine description, price, points rating, wine variety, etc. The dataset can be found at:", 
             a("https://www.kaggle.com/datasets/zynicide/wine-reviews")),
             br(),
             h3("Explore The Raw Dataset:"),
             sliderInput("rowsView", h4("Select rows of the dataset to view:"), min = 0, max = length(wineDFraw[,1])-11, value = 0),
             tableOutput("summaryTable")
             ),
    tabPanel("Analysis Techniques",
             h3("Analysis Techniques"),
             h4("1.) Download Data"),
             p("The data was downloaded from kaggle.com as a csv file. The csv file was loaded into r and converted to a data frame."),
             h4("2.) Filter Data"),
             p("The data frame was filtered so only the 10 most commonly rated wine varieties were considered. This reduced the data set from ~130,000 reviews to ~67,000 reviews. To do this, all wine varieties were counted by how many times they appeared in a review. Then they were ordered by most to least frequent. Any review with a wine variety not in the top 10 most-frequent list was filtered out."),
             p("Next, the uneeded columns were removed from the data frame. The analysis only required the columns: country, description, points, price, and variety."),
             p("Lastly, rows containing any 'NA' values were removed from the data frame, to guarentee completeness of the data. There was enough data that this did not impact the data frame's size much."),
             h4("3.) Cluster Data"),
             p("The data frame was broken up by variety to create 10 new data frames nicknamed varietyDF's."),
             p("Each varietyDF was clustered using the kmeans clustering algorithm. The kmeans clustering algorithm works by randomly assigning n points within a data set. Then, the algorithm calculates the distance of each value in the data set to these random points. Each value in the data set is assigned to one of these random points, based on minimized distance. For each random point, all data set values assigned to it are used to compute the center of mass for that group. The center of mass computed for each group are used as the points in the next iteration of the algorithm. Then, the algorithm reassigns each value in the data set to their new closest point. The algorithm continues until the distance in center of masses for consecutive iterations falls below a certain threshold. The resulting data set values are now clustered based on their minimized distance to each point."),             
             p("The two variables used to cluster were the wine price and the wine points rating. The variables for wine price and wine points rating were rescaled before performing the kmeans algorithm, so that each variable had the same mean and standard deviation. Scaling the data is imperitive because the range of prices are as big as 0 to $3000 whereas the range of points are 80 to 100. Since the kmeans algorith computes distances of points, the distance calculation would almost entirely depend on price if it were not scaled."),
             p("The SSE (sum squared error) was calculated between the values in the data set and their assigned cluster, for various cluster numbers. This was to see what a reasonable range of clusters would be for the interactive component of the application. The plot below shows how the SSE varies by number of clusters, for each wine variety. Based on SSE for each variety by number of clusters, it seemed that a range of 1 to 8 clusters would be a good range for the interactive features of the application."),
             plotOutput("plotSSE"),
             p("After running the kmean clustering algorithm based on number of clusters, the app assigned a cluster number to each varietyDF's review. The cluster number for each wine review was added as a new column in each varietyDF's data frame."),
             h4("4.) Extract Descriptive Words"),
             p("The analysis then extracted the most common words from the wine descriptions in each varietyDF's cluster."),
             p("This required filtering out non-descriptive words from the descriptions."),
             p("The first step to filtering out non-descriptive words was to remove any words with two characters or less."),
             p("Then, a list of non-descriptive words were created through a game of trial-and-error. Once the list was created, all words in the wine descriptions that appeared in the list of non-descriptive words were removed. Here is the list of non-descriptive words created:",em("and, wine, the, for, but, now, are, well, it's, this, well, there, into, with, that, flavors, aromas, from, notes, drink, finish, nose, palate, has, red, its, texture, shows, through, vineyard, black, offers, alongside, opens, made, while, delivers, more, good, touch, white, very, years, structure, aging, will, character, some, age, ready, aftertaste, attractive, just, pink.")),
             p("The words that remained were then counted by their frequency of use in the descriptions of each cluster of the varietyDF's."),
             p("The words were ordered from most to least frequent, and only the top 5 most frequent words were kept for each cluster of the varietyDFs."),
             h4("5.) Visualize Results in rShiny"),
             p("First, the model was made flexible to take inputs of 1.) Number of Clusters and 2.) Wine Variety."),
             p("The rShiny output displays a ggplot scatterplot for the specific wine variety's price vs. points rating. The dots on the plot were colored by their assigned cluster."),
             p("Each varietyDF's cluster's top 5 descriptors were then passed to a function that generated a radarplot, to visualize word usage frequency within a cluster. The radarplots were generated using the radarchart function from the fmsb library.")
             ),
    tabPanel("About",
             h4("About App"),
             p("This app explores how wine flavor profiles vary for different wine prices and ratings. The app uses clustering algorithms to group wines of certain prices and ratings and identifies the most frequent descriptive words used to describe wines of each group. By reducing wine to its descriptions, it helps create a standardized view of wine. Then, users of the app can view how price and ratings compare to this standard. Do $500+ Chardonnays with good ratings have the same level of acidity as < $500 Chardonnays with good ratings? Explore the dashboard to find out!"),
             p("Author: Wylie Borden"),
             p("Created in rShiny")
             )
  )
)

#-------------------------------------------------------------------------------
# Server function:
server <- function(input, output) {
  
  #Create variety-cluster specific DF:
  
  output$kmPlot = renderPlot({
    #Create variety-cluster specific DF:
    varietyDF = wineDF[wineDF$variety==input$variety,]
    set.seed(1)
    clusters = kmeans(scale(varietyDF[,c("price","points")]), input$clusters)
    varietyDF$cluster = clusters$cluster
    varietyDF$Cluster = as.character(clusters$cluster) #character class clusters
    
    #Plot:
    ggplot(varietyDF, aes(price, points, color=Cluster)) + 
      geom_point(size=8) + scale_x_continuous(trans='log10') + 
      xlab("Price US$") + ylab("Rating (0 to 100)") + 
      theme(text = element_text(size = 15)) + ggtitle(input$variety) +
      guides(color = guide_legend(override.aes = list(size = 6))) +
      scale_colour_manual(values = colors[1:input$clusters])
  })    
  
  output$radPlots = renderPlot({
    varietyDF = wineDF[wineDF$variety==input$variety,]
    set.seed(1)
    clusters = kmeans(scale(varietyDF[,c("price","points")]), input$clusters)
    varietyDF$cluster = clusters$cluster
    radarplotsfig(varietyDF)
  })
  
  output$plotSSE = renderPlot({
    plotSSE(wineDF)
  })
  
  output$summaryTable = renderTable({
    wineDFraw[c((input$rowsView+1):(input$rowsView+10)),]
  })
  
}

shinyApp(ui, server)