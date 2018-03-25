#FLP project - Ford Fulkerson

Author: Matúš Bako

##Descripion

This program is used to compute maximal flow in given graph. The graph is in dimacs format.

##Arguments
* -i parses input and prints it back
* -v prints path with maximal flow
* -f  computes maximal flow of graph

##Compilation and running
To compile, run *make*.

##Tests
*UnitTests.hs* contains basic unit tests of the most important functions. Script *run_end_tests.sh* launches end to end tests verifying correct parsing and correct computation results

##Files
* Dimacs.hs - parsing input file and creating *GraphData* object
* GraphData.hs - main computation functions and structure declarations
* Args.hs - argument parsing
* Ford - main file calling execution