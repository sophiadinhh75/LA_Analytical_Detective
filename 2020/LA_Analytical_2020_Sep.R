data <- (X2020_09_Sep)

date_counts <- table(data$`DATE OCC`)
date_counts <- sort(date_counts, decreasing = TRUE)
head(date_counts, 3)
top_dates <- names(head(date_counts, 3))
top_dates


for (d in top_dates) {
  cat("\nDATE:", d, "\n")
  
  area_counts <- table(
    data$`AREA NAME`[data$`DATE OCC` == d]
  )
  
  print(head(sort(area_counts, decreasing = TRUE), 3))
}



for (d in top_dates) {
  cat("\nDATE:", d, "\n")
  time_counts <- table(
    data$`TIME OCC`[data$`DATE OCC` == d]
  )
  
  print(head(sort(time_counts, decreasing = TRUE), 3))
}



for (d in top_dates) {
  
  areas <- table(
    data$`AREA NAME`[
      data$`DATE OCC` == d
    ]
  )
  
  top_areas <- names(head(sort(areas, decreasing = TRUE), 3))
  
  for (a in top_areas) {
    
    times <- table(
      data$`TIME OCC`[
        data$`DATE OCC` == d &
          data$`AREA NAME` == a
      ]
    )
    
    cat("\nDATE:", d, "| AREA:", a, "\n")
    print(head(sort(times, decreasing = TRUE), 3))
  }
}



for (d in top_dates) {
  
  areas <- table(
    data$`AREA NAME`[
      data$`DATE OCC` == d
    ]
  )
  
  top_areas <- names(head(sort(areas, decreasing = TRUE), 3))
  
  for (a in top_areas) {
    
    times <- table(
      data$`TIME OCC`[
        data$`DATE OCC` == d &
          data$`AREA NAME` == a
      ]
    )
    
    top_times <- names(head(sort(times, decreasing = TRUE), 3))
    
    for (t in top_times) {
      
      crimes <- table(
        data$`Crm Cd Desc`[
          data$`DATE OCC` == d &
            data$`AREA NAME` == a &
            data$`TIME OCC` == t
        ]
      )
      
      cat("\nDATE:", d, "| AREA:", a, "| TIME:", t, "\n")
      print(head(sort(crimes, decreasing = TRUE), 3))
    }
  }
}



for (d in top_dates) {
  
  areas <- table(
    data$`AREA NAME`[
      data$`DATE OCC` == d
    ]
  )
  
  top_areas <- names(head(sort(areas, decreasing = TRUE), 3))
  
  for (a in top_areas) {
    
    times <- table(
      data$`TIME OCC`[
        data$`DATE OCC` == d &
          data$`AREA NAME` == a
      ]
    )
    
    top_times <- names(head(sort(times, decreasing = TRUE), 3))
    
    for (t in top_times) {
      
      crimes <- table(
        data$`Crm Cd Desc`[
          data$`DATE OCC` == d &
            data$`AREA NAME` == a &
            data$`TIME OCC` == t
        ]
      )
      
      top_crimes <- names(head(sort(crimes, decreasing = TRUE), 3))
      
      for (crime in top_crimes) {
        
        premises <- table(
          data$`Premis Desc`[
            data$`DATE OCC` == d &
              data$`AREA NAME` == a &
              data$`TIME OCC` == t &
              data$`Crm Cd Desc` == crime
          ]
        )
        
        cat("\nDATE:", d, "| AREA:", a, "| TIME:", t, "| CRIME:", crime, "\n")
        
        print(head(sort(premises, decreasing = TRUE), 3))
      }
    }
  }
}

data <- X2020_09_Sep

# -------------------------------
# TOP 3 DATES
# -------------------------------

date_counts <- sort(table(data$`DATE OCC`), decreasing = TRUE)
top_dates <- names(head(date_counts, 3))

for (d in top_dates) {
  
  date_total <- sum(data$`DATE OCC` == d)
  
  cat("\n")
  cat("On", d, "there were", date_total, "total crimes.\n")
  
  # TOP 3 AREAS
  areas <- sort(
    table(data$`AREA NAME`[data$`DATE OCC` == d]),
    decreasing = TRUE
  )
  
  top_areas <- names(head(areas, 3))
  
  for (a in top_areas) {
    
    area_total <- areas[a]
    
    cat("The", a, "area had", area_total,
        "crimes on", d, ".\n")
    
    # TOP 3 TIMES
    times <- sort(
      table(
        data$`TIME OCC`[
          data$`DATE OCC` == d &
            data$`AREA NAME` == a
        ]
      ),
      decreasing = TRUE
    )
    
    top_times <- names(head(times, 3))
    
    for (t in top_times) {
      
      time_total <- times[t]
      
      cat("At", t, "in the", a,
          "area, there were", time_total,
          "crimes on", d, ".\n")
      
      # TOP 3 CRIMES
      crimes <- sort(
        table(
          data$`Crm Cd Desc`[
            data$`DATE OCC` == d &
              data$`AREA NAME` == a &
              data$`TIME OCC` == t
          ]
        ),
        decreasing = TRUE
      )
      
      top_crimes <- names(head(crimes, 3))
      
      for (crime in top_crimes) {
        
        crime_total <- crimes[crime]
        
        cat("The most common crime category at this time was",
            crime, "with", crime_total,
            "incidents.\n")
        
        # TOP 3 PREMISES
        premises <- sort(
          table(
            data$`Premis Desc`[
              data$`DATE OCC` == d &
                data$`AREA NAME` == a &
                data$`TIME OCC` == t &
                data$`Crm Cd Desc` == crime
            ]
          ),
          decreasing = TRUE
        )
        
        top_premises <- names(head(premises, 3))
        
        for (p in top_premises) {
          
          premise_total <- premises[p]
          
          cat("The", p, "premise was associated with",
              premise_total, "incidents of", crime,
              "at", t, "in the", a,
              "area on", d, ".\n")
        }
      }
    }
  }
}