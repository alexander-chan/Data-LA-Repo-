# Data-LA-Repo-

Private Repo for Data LA Project. *For members of UCLA Statistics Only.*

We will do all our edits here together so that we can present the final polished, product to our client when it is finished.

TODO List:
- Comment / clean up code

================================================

- Add buttons to Dashboard to select which data to visualize
- Finalize Dashboard
- Create Powerpoint for our Tuesday presentation

================================================


1. remove hard code from sankey diagram

2. fix logo in dashboard

3. [X] fix tabs so map code runs

4. round quarter 1 using screenshots

5. make function for map code

6. [X] fix colors on mile sewage cleaned bar charts

==================================================

  * Create a Sankey Diagram for the CleanStat Data
      1. [X] Decide which package to use to create Sankey Diagrams 
        - @Alexander-Chan, Min
        - *chosen* https://christophergandrud.github.io/networkD3/#sankey  
           - Can knit to rmarkdown
        - https://www.rdocumentation.org/packages/googleVis/versions/0.6.0/topics/gvisSankey
           - cannot knit to rmarkdown
      2. [x] Merge 2016 CleanStat Data from Quarters 1, 2, 3 and 4
        - Jon (Complete by Wednesday afternoon)
      3. [x] Implement Sankey Diagram with Merged Data Set
        - @Alex and Min (Complete by Thursday night.)
   * Analyze call center data and create a Shiny App with the cleaned data (Joseph and Conor)
      1. Clean the Data as specified on Issue #8 (https://github.com/datala/sanitation-dashboard/issues/8)
      2. Create various summary statistics (using dplyr or base R packages such as tapply() or table())
      3. Create a static Shiny App with this data
      4. Create a live Shiny App that updates as the City's website updates

May 9th Call:
* Introductions
* TODO: Create a Sankey Diagram for the CleanStat Data
* Use RSocrata to retrieve data and automate data dashboard 


