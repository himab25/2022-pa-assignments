Exercise 4
================

## Fakebook Bus seat selection

We’ll use `tidygraph` package to create and plot the seat mapping of
Fakebook bus. The nodes associated to the seats are W1 , W2 , W3 , W4 ,
W5 , W6 , A, B, C and D. Any type of adjacency works: side, front,back,
diagonal, even across the aisle.

We’ll be relying on these tutorials:

-   <http://users.dimi.uniud.it/~massimo.franceschet/ns/syllabus/make/tidygraph/tidygraph.html>
-   <https://www.data-imaginist.com/2017/introducing-tidygraph/>
-   <https://www.data-imaginist.com/2018/tidygraph-1-1-a-tidy-hope/>

First, we define the network by manually entering all the nodes and the
connections between them. We’ll define `nodes` table that has two
columns: `id` and `name`. We will then define the connections among them
as an edgelist, where each element in column `from` corresponds to a
friend on one end of the relationship and each element in column `to`
corresponds to the person on the other end of this friendship tie.

``` r
# define nodes
node_names <- tibble(
  id   = c(1,2,3,4,5,6,7,8,9,10),
  name = c("W1","W2","W3","W4","W5","W6","A","B","C","D")
)
node_names
```

    ## # A tibble: 10 × 2
    ##       id name 
    ##    <dbl> <chr>
    ##  1     1 W1   
    ##  2     2 W2   
    ##  3     3 W3   
    ##  4     4 W4   
    ##  5     5 W5   
    ##  6     6 W6   
    ##  7     7 A    
    ##  8     8 B    
    ##  9     9 C    
    ## 10    10 D

    ## # A tibble: 17 × 2

``` r
# define connections 
# for each element in `from` there is a corresponding element in `to`
edge_list <- tibble(
  from = c(1,2,3,3,3,3, 3,4,5, 5,6, 6,7,7,8, 8, 9),   
  to   = c(2,7,4,5,8,9,10,9,6,10,8,10,8,9,9,10,10)   
)
edge_list
```

    ## # A tibble: 17 × 2
    ##     from    to
    ##    <dbl> <dbl>
    ##  1     1     2
    ##  2     2     7
    ##  3     3     4
    ##  4     3     5
    ##  5     3     8
    ##  6     3     9
    ##  7     3    10
    ##  8     4     9
    ##  9     5     6
    ## 10     5    10
    ## 11     6     8
    ## 12     6    10
    ## 13     7     8
    ## 14     7     9
    ## 15     8     9
    ## 16     8    10
    ## 17     9    10

We can now combine these tables into a “graph” object that holds all of
our network data.

``` r
# combine this information into a network graph object
friendship_graph <- tbl_graph(nodes = node_names, edges = edge_list, directed = FALSE)
friendship_graph %>% tbl_df %>% print(n=10)
```

    ## Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
    ## Please use `tibble::as_tibble()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

    ## # A tibble: 10 × 2
    ##       id name 
    ##    <dbl> <chr>
    ##  1     1 W1   
    ##  2     2 W2   
    ##  3     3 W3   
    ##  4     4 W4   
    ##  5     5 W5   
    ##  6     6 W6   
    ##  7     7 A    
    ##  8     8 B    
    ##  9     9 C    
    ## 10    10 D

We can now plot this network using `ggraph` package.

``` r
friendship_graph %>% 
    ggraph(layout = 'kk') + 
    geom_edge_link() + 
    geom_node_point(size = 10, colour = 'gray') +
    geom_node_text(aes(label = name), colour = 'steelblue', vjust = 0.4) + 
    ggtitle('Friendship network') + 
    theme_graph()
```

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
    ## found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

![](Exercise_4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

We can use many of the functions in package `tidy_graph` to calculate
things we want to know about this network. For example, we may want to
know the centrality of each node in the network.

``` r
friendship_graph <- friendship_graph %>% 
  activate(nodes) %>% # we need to state we'll be adding to nodes, not edges
  mutate(d_centrality = centrality_degree()) %>%  # adding measure of degree centrality
  mutate(b_centrality = centrality_betweenness()) %>% # adding betweenness centrality
  mutate(c_centrality = centrality_closeness()) # adding closeness centrality
```

    ## Warning in betweenness(graph = graph, v = V(graph), directed = directed, :
    ## 'nobigint' is deprecated since igraph 1.3 and will be removed in igraph 1.4

    ## Warning in betweenness(graph = graph, v = V(graph), directed = directed, :
    ## 'nobigint' is deprecated since igraph 1.3 and will be removed in igraph 1.4

``` r
friendship_graph %>% tbl_df %>% print(n=10)
```

    ## # A tibble: 10 × 5
    ##       id name  d_centrality b_centrality c_centrality
    ##    <dbl> <chr>        <dbl>        <dbl>        <dbl>
    ##  1     1 W1               1        0           0.0333
    ##  2     2 W2               2        8           0.0455
    ##  3     3 W3               5        4.63        0.0625
    ##  4     4 W4               2        0           0.05  
    ##  5     5 W5               3        0.533       0.0476
    ##  6     6 W6               3        0.933       0.0526
    ##  7     7 A                3       14           0.0625
    ##  8     8 B                5        9.03        0.0714
    ##  9     9 C                5        8.6         0.0714
    ## 10    10 D                5        3.27        0.0625

Now let’s plot this with degree centrality determining the size of the
nodes and betweenness determining its color.

``` r
friendship_graph %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(aes(size = d_centrality, colour = b_centrality)) + 
  scale_color_continuous(guide = 'legend') +
  geom_node_text(aes(label = name), colour = 'red', vjust = 1.6) + 
  ggtitle('Friendship network') + 
  theme_graph()
```

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
    ## found in Windows font database

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
    ## found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

![](Exercise_4_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Seat Selection and Analysis

From the friendship graph plotted above and based on the calculation of
centrality :

1.  Seats B,C,D have the highest value of degree centrality (i.e 5)
    which means they have the maximum possible density of ties to
    others. In this case the best option to make more connections to the
    people.

2.  However, seat B has the highest value of betweenness centrality (i.e
    9.033) amongst the three which means it also has the highest
    bottleneck which makes it the most “central” seat with a possibility
    to connect most people.

3.  Lastly, in terms of closeness centrality, seat B again has the
    second best/lowest values (0.071) amongst B,C,D.

Combining all three centrality values together, my choice of seat would
be B as my main intention as a new intern is to make as many informal
connections as possible. However, it would also give me some advantage
of being a “central” person once the friendships have been established
in time based on the second best b_centrality value. Hence, my choice of
seat B would help me develop as many informal connections as possible
that is nicely located at good distance with most connections to
establish good communication; not to forget along with a good
possibility of being an important person as well to some extent which,
however,at this point is not my priority as a new intern.

However, if in future, being the most important “central” person was to
be my goal, then this choice of seat selection would be not so good. I
would have chosen seat A (highest b_centrality) instead at the behest of
having lesser connections.
