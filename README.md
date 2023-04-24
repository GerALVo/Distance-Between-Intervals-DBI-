# DBI
Distance Between Intervals (DBI) was introduced in Lo Valvo et al. (in press).

This pairwise distance works with continuous variables coded as intervals, whether they are groups of or single observations. DBI does not require summarizing the information in one value as classic distances do (e.g., Manhattan and Euclidean distances) reducing the loss of information. DBI also captures the complex overlap/non-overlap scenarios that could be given by a pair of intervals.

Here you will find DBI's application for R (DBIfunction.R) and the supplementary material to run the examples from Lo Valvo et al. (in press).

The easiest way to use the function is running the following line in R (it could take a few seconds):

	source("https://raw.github.com/GerALVo/Distance-Between-Intervals-DBI-/fa94e30d114c00e3523cb480cc839f9af5506e49/DBIfunction.R")


More information is available by accessing to the function's help by typing "?dbi"


Alternatively, the function can be loaded manually by following these steps:

- Download "DBIfunction.R" in the working directory (the folder where you are working)

- Run the following line in your script or console:

		source("DBIfunction.R")	
			
- Now you should have a function called 'dbi' available to use
	
	

Important!
Note that DBI's function is set to work with one character at a time.
To calculate the distance for two or more characters coded as intervals we recommend the use of the lapply() function
and structuring the input as a dataframe compound a two-column wide dataframe for each character. An example of this 
is given in the functions help (writing "?dbi").
To calculate the distance for two or more different types of characters, we recommend working with the resulting
distance matrices.


Reference:
Lo Valvo, G. A., Lehmann, O. E. R., and Balseiro, D. In press. A novel distance that reduces information loss in continuous characters with few observations. Palaeontologia Electronica.
