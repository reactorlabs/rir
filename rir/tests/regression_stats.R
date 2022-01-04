  a <- b <- 0
  plot(a, b    
       )
example(glm )
d <- c(1,3.8,4.4,5.1, 4,4.2,5, 2.6,5.3, 5.4)
attributes(d) <- list(Size = 5)
str(hclust(d ))
dhc <- as.dendrogram(hclust(dist(USArrests) ))
e <- dhc
dendrapply(e, function(f) str(attributes(f)))
df <- data.frame(id = 4,
                 visit = I(c("","") ))
reshape(df, timevar = "visit" , direction = "wide")
