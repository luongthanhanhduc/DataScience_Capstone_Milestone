get_smaller_data <- function(org_filename, new_filename, keep) {
  read_con <- file(org_filename, "rt")
  write_con <- file(new_filename, "wt")
  while (TRUE) {
    lines <- readLines(read_con, 50000) # read 50000 line at a time
    if (length(lines) < 1) {
      break
    } else {
      prob <- runif(length(lines))
      keep_indices <- which(prob < keep)
      lines <- lines[keep_indices]
      
      # retaining data for further analysis
      writeLines(lines, write_con)
    }
  }
  # close connections
  close(read_con)
  close(write_con)
}

count_num_lines <- function(filename) {
  num_lines <- 0
  read_con <- file(filename, "rt")
  
  while (TRUE) {
    lines <- readLines(read_con, 50000) # read 50000 line at a time
    if (length(lines) < 1) {
      break
    } else {
      num_lines <- num_lines + length(lines)
    }
  }
  # close connections
  close(read_con)
  num_lines
}

count_words <- function(filename) {
  count <- 0
  read_con <- file(filename, "rt")
  
  while (TRUE) {
    lines <- readLines(read_con, 50000) # read 50000 line at a time
    if (length(lines) < 1) {
      break
    } else {
      count <- count + length(unlist(strsplit(lines, split = " ")))
    }
  }
  # close connections
  close(read_con)
  count
}

# preprocessing data
preproccess <- function(input_file, output_file) {
  print(input_file)
  read_con <- file(input_file, "rt")
  write_con <- file(output_file, "wt")
  # get the list of bad words in English
  profane_words <- read.csv(file = "en_bad_words.txt", header = FALSE)[, 1]
  while (TRUE) {
    lines <- readLines(read_con, 50000) # read 50000 line at a time
    if (length(lines) < 1) {
      break
    } else {
     lines <- gsub("[[:punct:]]", "", lines) # remove all punctuation in text
     lines <- gsub("[[:digit:]]", "", lines) # remove all digits in text
     lines <- tolower(lines) # convert all text into lower case
     #lines <- gsub("[^[:alnum:]]", " ", lines) # remove all numeric or alphabet character
     for (i in 1:((length(profane_words) / 100)+1)) {
       pattern <- paste0("[", paste(profane_words[((i-1)*100 + 1) : 
                                                    min(i * 100, length(profane_words))], 
                                    collapse = "|"),"]")
       print(i)
       lines <- gsub(pattern, "", lines)# remove profane words  
     }
     lines <- gsub("  ", " ", lines) # remove extra space
     writeLines(lines, write_con)
    }
  }
  # close connections
  close(read_con)
  close(write_con)
}

# get n-grams distribution from the list of text files
get_ngrams_dist <- function(file_list, n) {
  word_dist <- data.frame(ngram = c(), freq = c())
  for (input_file in file_list) {
    print(input_file)
    read_con <- file(input_file, "rt")
    while (TRUE) {
      lines <- readLines(read_con, 50000) # read 50000 line at a time
      if (length(lines) < 1) {
        break
      } else {
        word_count <- data.frame(as.table(textcnt(lines, n, method = "string")))
        colnames(word_count) <- c("ngram", "freq")
        word_dist <- rbind(word_dist, word_count)
        word_dist <- aggregate(freq~ngram,data=word_dist,FUN=sum)
      }
    }
    # close connections
    close(read_con)
  }
  word_dist
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}