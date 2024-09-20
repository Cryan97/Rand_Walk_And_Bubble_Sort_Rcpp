#----- Setup -----

# Clearing workspace
rm(list=ls())

# setting the current working directory
setwd("C:\\Users\\conor\\OneDrive\\Documents\\Data Analytics Masters\\Stage 1\\Semester 3\\Data Programming with C\\Assignment\\Assignment 2")

# loading required packages using pacman
pacman::p_load(rbenchmark,Rcpp,inline)

#----- Question 1 - Random Walk -----
#----- Part 1 -----
# Random walk over N roads with S simulations
# returns the probability (relative frequency) of reaching the point 
# (-1,3) when starting from the origin (0,0) and having an equally likely
# probability of moving up,down,left or right in any given move.

body_randwalk <- '
    
    //convert input value (N) number of roads (moves) per iteration to an int
    int N_roads = as<int>(N);
    
    //convert input value (S) number of simulations (random walks) to an int
    int Sims = as<int>(S);
    
    //convert input value (seed) the random seed to an int
    int Seed = as<int>(seed);
    
    // Setting the random seed using srand
    srand(Seed);
    
    //creating integer variable to store number of times the tourist centre was reached
    int No_Successes = 0;
    
    //creating boolean variable to store result of each simulation
    bool res = false;
    
    //for loop to simulate S random walks consisting of N moves in each walk
    for (int i = 1; i <= Sims; i++) {
        
        //calling the random walk function and assigning result to res
        res = random_walk(N_roads);
        
        //if res is true (i.e. the tourist centre was reached) then increase
        //the value of No_Successes by 1 and continue
        if (res == true) {
            No_Successes += 1;
        }
    }
    
    //if the number of simulations is (less than or) equal to 1, 
    //return whether or not the tourist centre was reached
    if (Sims <= 1) {
        return wrap (res);
    }
    
    //else return the probability (no. successes divided by no. simulations) that 
    //the tourist centre was reached
    else if (Sims > 1){
        //creating variable prob to store the relative frequency of successes
        double prob = 0.0;
        
        //typecasting the No_Successes and Sims variables in order to calculate
        //the probability (relative freq) that the tourist centre was reached
        prob = static_cast<double>(No_Successes) / static_cast<double>(Sims);
        
        return wrap (prob);
    }
'

incl <- '
    //creating a function which implements one random walk and returns whether or not the 
    //tourist centre was reached
    bool random_walk(const int &N)
    
    { 
      //creating two integer variables to store x,y coordinates at each move
      int x = 0;
      int y = 0;
      
      //creating integer variable r to hold random value of rand function
      int r = 0;
      
      //create logical vector of length 1 to store whether or not tourist centre was reached
      bool tourist_centre_reached = false; 
      
      //for loop to make move at each step (move tourist up,down,left or right)
      for (int i = 1; i <= N; i++){
      
          // generate random number between 1 & 4 inclusive
          r = rand() % 4 + 1;
          
          // if rand == 1, then move one space right
          if (r == 1){
              x += 1;
          } 
          
          // if rand == 2, then move one space left
          else if (r == 2){
              x -= 1;
          }
          
          // if rand == 3, then move one space up
          else if (r == 3){
              y += 1;
          }
          
          // if rand == 4, then move one space down
          else {
              y -= 1;
          }
          
          // check if tourist centre reached @ (-1,3)
          if (x == -1 && y == 3){
              // change tourist centre to true if (-1,3) reached
              tourist_centre_reached = true;
          }
        }
      
      //after all moves have been made return whether or not tourist
      //centre was reached
      return tourist_centre_reached;
    }
'

# Compiling, linking and loading the Rcpp function and assigning it to randwalk
# I include my incl string in order to load the ranndom_walk cpp function into
# my overall random walk function. The incl function is designed to calculate one
# random walk and my overall function is designed to facilitate multiple simulations
# of random walks.
randwalk <- cxxfunction(signature(N = "integer",S = "integer",seed = "integer"),
                        body = body_randwalk,
                        include = incl,
                        plugin = "Rcpp",
                        verbose = TRUE)

# printing the result of the random walk with 100 moves (and 1 simulation)
# the tourist reaches the tourist centre in this instance (output is TRUE)
randwalk(100,1,1759)

#----- Part 2 -----
# Approximating the probability that a tourist locates the tourist information
# centre within 20 moves (roads) i.e. 20 moves or less using 1000 simulations
prob_q1_part2 <- randwalk(20,1000,1759)

# printing result
print(gsub("[\r\n]"," ",paste0("The probability that a tourist locates the tourist information
centre within 20 roads is ",prob_q1_part2)))

#----- Part 3 -----
# See PDF


#----- Part 4 -----
# Random walk over N roads with S simulations
# returns the Manhattan distance between the origin (0,0) and the final location
# (x,y) reached after N moves with each move having an equally likely
# probability of moving up,down,left or right.

body_randwalk_dist <- '
    
    //convert input value (N) number of roads (moves) per iteration to an int
    int N_roads = as<int>(N);
    
    //convert input value (S) number of simulations (random walks) to an int
    int Sims = as<int>(S);
    
    //convert input value (seed) the random seed to an int
    int Seed = as<int>(seed);
    
    // Setting the random seed using srand
    srand(Seed);
    
    //creating numeric variable to store the total (Manhattan) distance
    //the tourist reaches from the origin (0,0) after N moves and S simulations
    double tot_Man_dist = 0.0;
    
    //creating numeric variable to store the average (Manhattan) distance
    //the tourist reaches from the origin (0,0) after N moves and S simulations
    double avg_Man_dist = 0.0;
    
    //for loop to simulate S random walks consisting of N moves in each walk
    for (int i = 1; i <= Sims; i++) {
        
        //calling the random walk dist function and addind the resulting
        // manhattan distance to the tot_Man_dist variable
        tot_Man_dist += random_walk_dist(N_roads);
      }

    //calculating the average Manhattan distance over S simulations
    avg_Man_dist = tot_Man_dist / static_cast<double>(Sims);
    
    return wrap (avg_Man_dist);
'

incl_dist <- '

    double random_walk_dist(const int &N)
    
    { 
      //creating two integer variables to store x,y coordinates at each move
      int x = 0;
      int y = 0;
      
      //creating r int to hold random value of rand function
      int r = 0;
      
      //creating numeric variable to store the total (Manhattan) distance
      //the tourist reaches from the origin (0,0) after N moves
      double man_dist = 0.0;
      
      //for loop to make move at each step (move tourist up,down,left or right)
      for (int i = 1; i <= N; i++){
      
          // generate random number between 1 & 4 inclusive
          r = rand() % 4 + 1;
          
          // if rand == 1, then move one space right
          if (r == 1){
              x += 1;
          } 
          
          // if rand == 2, then move one space left
          else if (r == 2){
              x -= 1;
          }
          
          // if rand == 3, then move one space up
          else if (r == 3){
              y += 1;
          }
          
          // if rand == 4, then move one space down
          else {
              y -= 1;
          }
        }
      
      //calculating the Manhattan distance from origin (0,0) after N moves 
      man_dist = abs(x) + abs(y);
      
      //returning Manhattan distance from the origin after N moves
      return man_dist;
    }
'

# compile, link and load the random walk distance Rcpp function
randwalk_dist <- cxxfunction(signature(N = "integer",S = "integer",seed = "integer"),
                        body = body_randwalk_dist,
                        include = incl_dist,
                        plugin = "Rcpp",
                        verbose = TRUE)

# printing the average (Manhattan) distance the tourist reaches after making
# 50 moves accross 1000 simulations
randwalk_dist(50,1000,1759)

#----- Question 2 - Bubble Sort -----

# body of the C ++ bubblesort function which now takes NA values
# and moves them to the end of the vector
# stored in an R character string
body_bubblesort2_NA <- '
	IntegerVector xx = clone( x );	// use of clone ()
	int n = xx.size();	// no. of elements
	int temp;	// temporary storage of swap value
	int n_nas = 0; // no. of NAs in input vector
	
	for( int f = 0; f <= n-1; f ++){
	  // inital loop over each element to check for NAs and count them
    if( xx[ f ] == NA_INTEGER ){
      n_nas += 1; // increase NA counter by 1 if element is NA
    }
	}
	
	for( int l = 0; l <= (n-1-n_nas); l ++){
    // loop over the number of non-NA elements in the vector 
    if( xx[ l ] == NA_INTEGER ){
      // loop starting from end of vector to check if we can swap
      // the NA element with a non-NA element
      for( int m = n-1; m >= 0; m --){
        if( xx[ m ] != NA_INTEGER ){
          temp = xx[ m ]; // setting the temp value to the non-NA value
          xx[ l ] = temp; // setting the element that was previously NA to the non-NA value
          xx[ m ] = NA_INTEGER; // setting the element at other end of vector to NA
          break; // swap is complete therefore break out of inner loop & move onto next element
        }
      }
    }
	}
	
	// original bubblesort algorithm where we only loop over Non-NA elements
	// All NA values are already at end of vector due to above code
	for( int k = 1; k <= (n-1-n_nas); k ++ ){	// for pass k
		// loop over pairs of elements
		for( int i = 0; i < (n-1-n_nas); i++ ){
			if( xx[ i ] > xx[ i +1 ] ){
				temp = xx[ i + 1 ];
				xx[ i + 1 ] = xx[ i ];
				xx[ i ] = temp;
			}	// end of if
		}	// end of loop over array pairs
	}	// end of loop over passes
	return( wrap( xx ));
'


# compile, link and load the bubblesort function
bubblesort2_NA <- cxxfunction( signature( x = "integer" ),
                            body = body_bubblesort2_NA,
                            plugin = "Rcpp")

# create an R integer vector to input to bubblesort
x2 <- as.integer( sample(1:100, size = 100, replace = FALSE ) )
x2[4] <- NA_integer_
x2[7] <- NA_integer_
x2[24] <- NA_integer_
x2[68] <- NA_integer_
x2[72] <- NA_integer_
x2[91] <- NA_integer_

# call
bubblesort2_NA( x2 ) # returns sorted x2
x2 # original x2 is not sorted
