# STAT585Final

## Title: 

Simulating NFL Playoff Teams by Game Results

## Abstract: 

Over the years, football has grown into America's pasttime, and American sports get no bigger than the National Football League.  While websites online allow fans to select winners and losers to create their own playoff field, it does not allow the entrance of ties -- which are possible -- nor does it allow fans to enter specific scores, which can alter playoff seeding.  While simulation analyses are available through various websites, they are not very user-friendly in terms of simulating with a specific game result already factored in.  The R Package, ProjectNFL, is based on these topics by providing NFL weekly game data for users obtained using webscraping, functions to break any ties using split-apply-combine functionality, and provide simulation results for teams to make the playoffs, including adding the ability to add specific game results in the simulation.

## Description:

This is a package with multiple functions to scrape NFL game results from NFL.com, aggregate statistics, calculate head to head and common games results from the scores, break ties in win-loss record for both division and conference, determine rankings in both the division and the conference, and simulate final conference rankings and visualize those results for a specific team.  The main functions of this packages are ```NFLSim``` and ```SeedPlot``` as these run the simulations as specified earlier and make a bar plot of the frequency of seeds for a user-specified team.
