library(shiny)
library(DBI)
library(eeptools)
library(ISLR)
library(dplyr)
library(cluster)
library(DT)
library(data.table)
library(factoextra)

sqlQuery <- function (query) {
  # Connection to db
  conn <- dbConnect(
    RMySQL::MySQL(),
    dbname = "super_data",
    host = "34.87.121.216",
    user = "root",
    password = "superdata123"
  )
  # Close db connection after function call exits
  on.exit(dbDisconnect(conn))
  
  rs <- dbGetQuery(conn, query)
  return(rs)
}

shinyServer(function(input, output, session) {
  # Getting transaction and customers data from db
  trans <-
    sqlQuery(
      "SELECT TRANSACTIONID, orders.PDTID, PDTNAME, CATEGORY, USERID, ORDERQTY, NETPRICE, PCHASEDATE FROM orders JOIN product ON orders.PDTID = product.PDTID"
    )
  customersRaw  <-
    sqlQuery("SELECT USERID, GENDER, DOB FROM customer")
  
  # Get customer age
  customersRaw$AGE <- NA
  customersRaw$AGE <-
    floor(age_calc(as.Date(customersRaw$DOB), units = "years"))
  
  # Interesting factors: GENDER, DOB, NETPRICE -> PDTID
  date <- Sys.Date()
  cust_stats <- trans %>%
    group_by(USERID) %>%
    summarize(
      TOTAL_SPENDINGS = sum(NETPRICE),
      LAST_PURCHASE = as.numeric(as.Date(strptime(date, "%Y-%m-%d")) - as.Date(strptime(
        max(PCHASEDATE), "%Y-%m-%d"
      ))),
      NUM_TRANSACTIONS = n()
    )
  
  # Merging stats with customer table
  customers <- merge(cust_stats, customersRaw, by = "USERID")
  
  # Transform all non-numeric
  z <-
    customers[, -which(names(customers) %in% c("USERID", "DOB"))]
  z$GENDER[z$GENDER == "M"] <- 1
  z$GENDER[z$GENDER == "F"] <- 2
  z <- as.data.frame(sapply(z, as.numeric))
  zSet <- z
  distance <- dist(zSet)
  
  # ------------------------------- User Inputs ------------------------------------
  updateSelectizeInput(
    session,
    'factors',
    choices = list(
      Factors = c(
        'Customer Age' = 'AGE',
        'Customer Gender' = 'GENDER',
        'Days Since Last Purchase' = 'LAST_PURCHASE',
        'Total Number of Purchases' = 'NUM_TRANSACTIONS',
        'Total Spendings' = 'TOTAL_SPENDINGS'
      )
    ),
    selected = c('TOTAL_SPENDINGS', 'AGE')
  )
  
  observeEvent(input$applyButton, {
    print(input$factors)
    zSet <<- subset(z, select = c(input$factors))
    print(head(zSet, 300))
  })
  
  getDistance <- function() {
    zSet <<- subset(z, select = c(input$factors))
    
    # Calculate Euclidean distance
    if (input$normalize == TRUE) {
      # Normalization to reduce chances of domination of one candidate
      m <- apply(zSet, 2, mean)
      s <- apply(zSet, 2, sd)
      zSet <<- scale(zSet, m, s)
      distance <- dist(zSet)
    } else {
      zSet <<- zSet
      distance <- dist(zSet)
    }
    
    
    return(distance)
  }
  
  # ------------------------- K-Means Clustering -------------------------------
  output$graph_km <- renderPlot({
    # K-Means clustering visualize
    distance <- getDistance()
    kc <<- kmeans(zSet, input$k)
    
    output$km_table <- DT::renderDataTable ({
      # Applying 3 decimal places to table of cluster centers means
      kc_table <<- kc$centers
      is.num <- sapply(kc_table, is.numeric)
      kc_table[is.num] <<-
        sapply(kc_table[is.num], round, digits = 3)
      kc_table
    }, height = 750, width = 800)
    
    # K-Means visualization
    fviz_cluster(
      kc,
      data = z,
      geom = "point",
      stand = FALSE,
      ellipse.type = "norm",
      main = "K-Means Cluster Plot",
      xlab = "Euclidean Distance X",
      ylab = "Euclidean Distance Y"
    ) + theme_minimal()
  }, height = 750, width = 800)
  
  output$km_table1 <- DT::renderDataTable ({
    distance <- getDistance()
    kc <- kmeans(zSet, input$k)
    
    # Applying 3 decimal places to table of cluster centers means
    kc_table <<- kc$centers
    is.num <- sapply(kc_table, is.numeric)
    kc_table[is.num] <<-
      sapply(kc_table[is.num], round, digits = 3)
    kc_table
  }, height = 750, width = 800)
  
  
  # ------------------------- Hierarchical Clustering -------------------------------
  output$graph_hc <- renderPlot({
    # Hierarchical clustering using dendrogram (complete linkage)
    distance <- getDistance()
    hc.cut <<-
      hcut(distance, k = input$k, hc_method = "complete")
    
    hc.cut$labels <- customers$USERID
    fviz_dend(hc.cut,
              show_labels = TRUE,
              rect = TRUE,
              main = "Hierarchical Cluster Dendrogram (Complete Linkage)")
  }, height = 750, width = 800)
  
  output$hc_table <- DT::renderDataTable ({
    # Plot cluster means table
    distance <- getDistance()

    # Perform hierarchical clustering using complete linkage method
    hc.cut <-
      hcut(distance, k = input$k, hc_method = "complete")
    
    # Get cluster average
    hc_table <<- (aggregate(zSet, list(hc.cut$cluster), mean))
    hc_table <<- hc_table[, -c(1, 1)]
    
    # Round to 3 decimal places
    is.num <- sapply(hc_table, is.numeric)
    hc_table[is.num] <<-
      sapply(hc_table[is.num], round, digits = 3)
    hc_table
  }, height = 750, width = 800)
  
  output$graph_ha <- renderPlot({
    # Hierarchical clustering using dendrogram (average linkage)
    distance <- getDistance()
    ha.cut <<-
      hcut(distance, k = input$k, hc_method = "average")
    
    ha.cut$labels <- customers$USERID
    fviz_dend(ha.cut,
              show_labels = TRUE,
              rect = TRUE,
              main = "Hierarchical Cluster Dendrogram (Average Linkage)")
  }, height = 750, width = 800)
  
  output$ha_table <- DT::renderDataTable ({
    # Plot cluster means table
    distance <- getDistance()
    ha.cut <- hcut(distance, k = input$k, hc_method = "average")
    
    ha_table <<- (aggregate(zSet, list(ha.cut$cluster), mean))
    ha_table <<- ha_table[, -c(1, 1)]
    
    # Round to 3 decimal places
    is.num <- sapply(ha_table, is.numeric)
    ha_table[is.num] <<-
      sapply(ha_table[is.num], round, digits = 3)
    ha_table
  }, height = 750, width = 800)
  
  # ------------------------- Evaluate Clustering -------------------------------
  
  output$hc_si <- renderPlot({
    # Silhuoette Plot
    distance <- getDistance()
    customers.c <- hclust(distance)
    plot(silhouette(cutree(customers.c, input$k), distance), main =
           "Silhuoette Plot Evaluation")
  }, height = 750, width = 800)
  
  output$hc_scree <- renderPlot({
    # Scree Plot (sum of square within clusters)
    distance <- getDistance()
    wss <- (nrow(zSet) - 1) * sum(apply(zSet, 2, var))
    for (i in 2:20)
      wss[i] <- sum(kmeans(zSet, centers = i)$withinss)
    plot(
      1:20,
      wss,
      type = "b",
      xlab = "Number of Clusters",
      ylab = "Within-Cluster Sum of Square",
      main = "Scree Plot Evaluation"
    )
  }, height = 750, width = 800)
  
  # ------------------------- Cluster Recommendations -------------------------------
  output$cr_table <- DT::renderDataTable ({
    # Reading related transactional data from sql db
    orders <-
      sqlQuery(
        "SELECT TRANSACTIONID, PDTID, orders.USERID, CLUSTER FROM orders JOIN customer ON orders.USERID=customer.USERID"
      )
    products <- sqlQuery("SELECT PDTNAME, PDTID FROM product")
    
    # Get top 12 pdtid group by cluster
    result <-
      orders %>% group_by(CLUSTER, PDTID) %>% summarise(Freq = n()) %>% top_n(12, Freq)  #%>% arrange_(~ desc(CLUSTER))
    result <- merge(result, products, "PDTID")
    
    # Rearranging and renaming data col
    col_order <- c("CLUSTER", "PDTID", "PDTNAME",
                   "Freq")
    result <- result[, col_order]
    colnames(result)[colnames(result) == "Freq"] <- "FREQUENCY"
    result <-
      result %>% arrange_(~ desc(FREQUENCY)) %>% arrange_(~ (CLUSTER))
    
    result
  }, height = 750, width = 800)
  
  # ------------------------- Generate Report -------------------------------
  observeEvent(input$exportButton, {
    # Check which tab is currently active
    method <- input$tab
    if (input$tab == "K-Means Clustering") {
      currentCluster <- kc$cluster
      currentTable <- kc_table
      
    } else if (input$tab == "Hierarchical Clustering (Complete)") {
      currentCluster <- hc.cut$cluster
      currentTable <- hc_table
      
    } else if (input$tab == "Hierarchical Clustering (Average)") {
      currentCluster <- ha.cut$cluster
      currentTable <- ha_table
      
    } else {
      showModal(modalDialog(
        title = "Selection Error",
        paste0(
          "Cluster evaluation selected! Please select a clustering method instead!"
        ),
        easyClose = TRUE,
        footer = NULL
      ))
      
      return(NULL)
    }
    
    print(method)
    print(nrow(zSet))
    print(input$normalize)
    print(currentCluster)
    print(currentTable)
    
    # Associate customers with its respective cluster
    uid <- customers[1]
    c_clusters <- data.frame(uid, currentCluster)
    
    
    # Connection to db
    conn <- dbConnect(
      RMySQL::MySQL(),
      dbname = "super_data",
      host = "34.87.121.216",
      user = "root",
      password = "superdata123"
    )
    
    # Close db connection after function call exits
    on.exit(dbDisconnect(conn))
    
    withProgress(message = 'Exporting: ', value = 0, {
      n <- nrow(c_clusters)
      
      for (row in 1:n) {
        # Increment the progress bar
        incProgress(1 / n, detail = paste(row, "/", n))
        
        # Get cluster number and its associated user id
        uid <- c_clusters[row, "USERID"]
        group <- c_clusters[row, "currentCluster"]
        
        # Format sql query
        sql <- "UPDATE customer SET CLUSTER='"
        sql <- paste(sql, group, sep = "")
        sql <- paste(sql, "' WHERE USERID='", sep = "")
        sql <- paste(sql, uid, sep = "")
        sql <- paste(sql, "'", sep = "")
        
        print (sql)
        dbGetQuery(conn, sql)
      }
      print ("Customer association with cluster complete!")
      
    })
    
    # Format statistical col info into strings to be inserted to db
    age <- ""
    gender <- ""
    total_spendings <- ""
    transactions <- ""
    last_purchase <- ""
    
    for (row in 1:nrow(currentTable)) {
      if ("AGE" %in% colnames(currentTable))
      {
        age <- paste(age, currentTable[row, "AGE"], sep = " ")
      }
      
      if ("GENDER" %in% colnames(currentTable))
      {
        gender <- paste(gender, currentTable[row, "GENDER"], sep = " ")
      }
      
      if ("TOTAL_SPENDINGS" %in% colnames(currentTable))
      {
        total_spendings <-
          paste(total_spendings, currentTable[row, "TOTAL_SPENDINGS"], sep = " ")
      }
      
      if ("NUM_TRANSACTIONS" %in% colnames(currentTable))
      {
        transactions <-
          paste(transactions, currentTable[row, "NUM_TRANSACTIONS"], sep = " ")
      }
      
      if ("LAST_PURCHASE" %in% colnames(currentTable))
      {
        last_purchase <-
          paste(last_purchase, currentTable[row, "LAST_PURCHASE"], sep = " ")
      }
    }
    
    # Insert cluster information statistic info
    timestamp <- Sys.time()
    timestamp <- substr(timestamp, 1, nchar(timestamp))
    
    sql <-
      "INSERT INTO cluster(METHOD, DISTANCE, NUM_CUSTOMERS, NUM_CLUSTERS, NORMALIZE,
    AGE, GENDER, TOTAL_SPENDINGS, TRANSACTIONS, LAST_PURCHASE)
    VALUES('"
    sql <- paste(sql, input$tab, sep = "")
    sql <- paste(sql, "Euclidean", sep = "', '")
    sql <- paste(sql, nrow(customersRaw), sep = "', '")
    sql <- paste(sql, input$k, sep = "', '")
    sql <- paste(sql, input$normalize, sep = "', '")
    sql <- paste(sql, age, sep = "', '")
    sql <- paste(sql, gender, sep = "', '")
    sql <- paste(sql, total_spendings, sep = "', '")
    sql <- paste(sql, transactions, sep = "', '")
    sql <- paste(sql, last_purchase, sep = "', '")
    sql <- paste(sql, "');", sep = "")
    dbGetQuery(conn, sql)
    
    showModal(modalDialog(
      title = input$tab,
      paste0(
        "Successfully associated all customers with clusters and exported all statistical information to the database!"
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # ------------------------- Generate Recommendations -------------------------------
  
  observeEvent(input$crButton, {
    withProgress(message = 'Generating Recommendations: ', value = 0, {
      # Increment the progress bar
      incProgress(1 / 100, detail = paste('Computing Queries'))
      
      # Reading related transactional data from sql db
      orders <-
        sqlQuery(
          "SELECT TRANSACTIONID, PDTID, orders.USERID, CLUSTER FROM orders JOIN customer ON orders.USERID=customer.USERID"
        )
      products <- sqlQuery("SELECT PDTNAME, PDTID FROM product")
      
      # Get top 12 pdtid group by cluster
      result <-
        orders %>% group_by(CLUSTER, PDTID) %>% summarise(Freq = n()) %>% top_n(12, Freq)  #%>% arrange_(~ desc(CLUSTER))
      result <- merge(result, products, "PDTID")
      
      # Rearranging and renaming data col
      col_order <- c("CLUSTER", "PDTID", "PDTNAME",
                     "Freq")
      result <- result[, col_order]
      colnames(result)[colnames(result) == "Freq"] <- "FREQUENCY"
      result <-
        result %>% arrange_(~ desc(FREQUENCY)) %>% arrange_(~ (CLUSTER))
      cr_table <<- result
      
      
      # Connection to db
      conn <- dbConnect(
        RMySQL::MySQL(),
        dbname = "super_data",
        host = "34.87.121.216",
        user = "root",
        password = "superdata123"
      )
      
      # Close db connection after function call exits
      on.exit(dbDisconnect(conn))
      
      # Loop through table to insert sql result on cluster recommendations
      sqlQuery("TRUNCATE TABLE cluster_recommendation;")
      
      # Loop through table to insert sql result on cluster recommendations
      n <- nrow(result)
      for (row in 1:n) {
        # Increment the progress bar
        incProgress(1 / n, detail = paste(row, "/", n))
        
        sql <-
          "INSERT INTO cluster_recommendation(CLUSTER, PDTID, PDTNAME, FREQUENCY) VALUES('"
        result[row, "CLUSTER"]
        result[row, "PDTID"]
        result[row, "PDTNAME"]
        result[row, "FREQUENCY"]
        pdtname <- gsub("'", "", result[row, "PDTNAME"])
        
        sql <- paste(sql, result[row, "CLUSTER"], sep = "")
        sql <- paste(sql, result[row, "PDTID"], sep = "', '")
        sql <- paste(sql, pdtname, sep = "', '")
        sql <- paste(sql, result[row, "FREQUENCY"], sep = "', '")
        sql <- paste(sql, "');", sep = "")
        dbGetQuery(conn, sql)
      }
      
    })
    
    showModal(
      modalDialog(
        title = "Cluster Recommendations",
        paste0("Successfully generated all cluster recommendations!"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
})
