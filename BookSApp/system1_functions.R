#system1_fuctions.R
#Load data and make available a function for retreiving genre specific recommendations

#load data - pre-computed genre recommendations
genre_recs_margin = as.data.frame(read.csv('data/genre_recommendations_margin_S1A.csv',
                                           header = T))
#define function
system1_recs = function(sel_genre, n = 10){return(genre_recs_margin[1:n, sel_genre])}