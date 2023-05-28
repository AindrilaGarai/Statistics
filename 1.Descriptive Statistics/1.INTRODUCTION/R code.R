data <- c(11.1, 13.0, 9.9, 13.3, 14.5, 11.0, 10.8,
          10.0, 10.6, 11.4, 12.5, 12.6, 15.2, 13.7,
          11.3, 14.2, 12.4, 12.4, 12.3, 13.9, 13.1,
          13.7, 10.9, 13.4, 12.5, 14.5, 13.4, 15.9,
          13.5, 13.6, 12.5, 10.2, 9.4, 12.0, 11.1,
          14.3, 12.6, 13.6, 12.6, 11.4, 13.9, 13.9,
          11.4, 11.9, 15.7, 12.6, 14.4, 11.6, 14.5,
          15.3)

range <- max(data) - min(data) #range.default()
class_width <- 0.9 #given
C <- round((range+1)/class_width) # round function in numerical analysis
first_lowest_class_limit <- 9 #(by default=floor(min(data)))


class_limit <- function(data,class_width)
{
  upper_limit <- NULL
  lower_limit <- first_lowest_class_limit
  new_upper_limit <- 0
  new_lower_limit <- 0
  while(max(data)>new_upper_limit)
  {
    new_upper_limit <- max(lower_limit)+class_width
    upper_limit <- c(upper_limit, new_upper_limit)
    
    new_lower_limit <- ceiling(new_upper_limit)
    lower_limit <- c(lower_limit, new_lower_limit)
    
  }
  lower_limit <- lower_limit[-(which.max(lower_limit))]
  rtn <- data.frame(lower_limit, upper_limit )
  rtn
}
class_limit <- class_limit(data,0.9)


class_boundary <- function(class_limit)
{
  lim <- class_limit
  gap <- (lim$lower_limit[2] - lim$upper_limit[1]) /2
  lower_boundary <- lim$lower_limit - gap
  upper_boundary <- lim$upper_limit + gap
  rtn <- data.frame(lower_boundary, upper_boundary )
  return(rtn)
}
class_boundary <- class_boundary(class_limit)


class_mark <- function(class_limit)
{
  len <- dim(class_limit)[1]
  classMark <- numeric(len)
  for(i in 1:len)
  {
    classMark[i] <- sum(class_limit[i,])/2
  }
  data.frame(classMark)
}
class_mark <- class_mark(class_limit)


freq <- function(data,class_limit)
{
  lim <- class_limit
  len <- dim(lim)[1]
  fre <- numeric(len)
  for(i in 1:len)
  {
    vec <- numeric(length = length(data))
    for(j in 1:length(data))
    {
      vec[j] <- (data[j] >= lim$lower_limit[i] && data[j] <= lim$upper_limit[i])
      
      fre[i] <- sum(vec)
    }
  }
  return(data.frame(fre))
}
freq <- freq(data,class_limit)


cumulative <- function(freq, type)
{
  len <- dim(freq)[1]
  vec <- numeric(len)
  if(type=="increasing")
  {
    for (i in 1:len) 
    {
      vec[i] <- sum(freq[1:i,])
    }
    return(data.frame(vec))
  }
  if(type=="decreasing")
  {
    for (i in len:1) 
    {
      vec[len-i+1] <- sum(freq[len-i+1:i,])
    }
    return(data.frame(vec))
  }
}
cum_in <- cumulative(freq,"increasing")
cum_de <- cumulative(freq,"decreasing")


relativeFreq <- function(freq)
{
  len = dim(freq)[1]
  vec <- numeric(len)
  for(i in 1:len)
  {
    vec[i] <- freq[i,]/sum(freq[,1])
  }
  return(data.frame(vec))
}
rel_freq <- relativeFreq(freq)
sum(rel_freq)


Freqden <- function(freq)
{
  len = dim(freq)[1]
  vec <- numeric(len)
  for(i in 1:len)
  {
    vec[i] <- round(freq[i,]/class_width, 2)
  }
  return(data.frame(vec))
}
freqden <- Freqden(freq)


freq_table <- data.frame(c(class_limit, class_boundary, freq, cum_in, cum_de, rel_freq, freqden))
freq_table # final output
