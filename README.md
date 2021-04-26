# We've Got the Power

Link to my visualization solution on Shinyapps:
 https://jgonz73.shinyapps.io/Project3/

 Running this code:
 Once files are downloaded (.R and .csv files), put them into a folder
 The appLate.R one has the visualization with sliders to adjust range for generation
 The app.R does not have the visualization with sliders to adjust range for generation
 
 1.) Download R
 https://www.r-project.org/

 2.) Download RStudio
 https://rstudio.com/products/rstudio/download/
 
 3.) Learn about Shiny (optional, this app uses it as a web framework)
 https://rstudio.com/products/shiny/download
 
 Once RStudio is opened, set to correct directory with command
 "getwd()"
 To get to the correct directory with the files you downloaded from this github,
 setwd(dir)
 dir = directory path to the folder the files are in
 
 4.) install.packages('package') on all libraries listed in the top of the .R file

 5.) Install the .csv file from Kaggle and put it in the same directory as the project. https://www.kaggle.com/chicago/chicago-energy-usage-2010
 
 Once there, you should be able to run the application by using the command runApp() in the console 
 or the "Run App" button on the top right of RStudio.
 
 The application should now start and you can start manipulating the data
