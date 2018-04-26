#'Compute similarity of faces in a lineup; experimental function
#'
#'Function to compute the degree to which each face in a set of faces
#'loads onto a common factor computed from the faces.
#'
#'The faces need to be standardized for inter-pupil distance, and for
#'pupil location prior to running the function
#'
#'The user will be asked to choose a set of faces through a dialog
#'box. These should be jpeg files.
#'
#'There is no argument to the function; it gets what it needs from the 
#'dialog box.  The function prints the lineup array to the viewer
#'pane in R, and reports the loading of each face on the first common
#'factor in a factor analysis (using fa in package psych)
#'
#'
#'@examples
#'sim_face()
#'
#'
#'
#'@references Tredoux, C. (2002). A direct measure of facial similarity 
#'and its relation to human similarity perceptions. 
#'Journal of Experimental Psychology: Applied, 
#'8(3), 180–193. doi:10.1037/1076-898x.8.3.180
#'
# 

library(pacman)
p_load(tidyverse, psych, magrittr, magick, psych, here)


# Helper function
# This function reads and shows an array of faces
show_lineup <- function(file_list){

  # Read faces into img object 'img'
  img <- image_read(file_list)
  
  # Display the faces as an array
  (image_append(image_scale(img, "x150")))
  
}

# Helper function
# Read lineup faces, and make/return
# a dataframe of RGB values for each face, as a vector in the frame 
# Returns a dataframe for further analysis

make_lineup_data <- function(file_list){
  
  fnamesa <- as.character(1:length(file_list))
  fnamesb <- rep("Face",length(file_list))
  fnamesc <- paste(fnamesb, fnamesa, sep="_")

  face_df <- NULL
  
  for (i in 1:length(file_list)){
    temp_face <- image_read(file_list[i])
    temp_face <- image_scale(temp_face, "100")
    temp_face <- image_convert(temp_face,"tiff")
    temp_face_array <- as.integer(temp_face[[1]])
    dim(temp_face_array) <- NULL
    face_df <- cbind(face_df, temp_face_array)
  }
  face_df <- as.data.frame(face_df)
  names(face_df) <- fnamesc
  return (face_df)
}

# Main function
face_sim <- function(){
  root <- paste(here(),"/Faces/*.jpg",sep = "")
  file_list <- choose.files(default = "root", caption = "Select files",
                            multi = TRUE, filters = Filters,
                            index = nrow(Filters))
  a_face_df <- make_lineup_data(file_list)
  fa_faces <- fa(a_face_df, nfactors = 1)
  z <- fa_faces$loadings[1:length(fa_faces$loadings)]
  cat("\nLineup is shown in the Viewer window,\n")
  cat("Reading from left to right for order,\n")
  cat("values indicate extent to which each face\n")
  cat("matches the other faces in the lineups\n")
  cat("where higher values are a greater match\n\n")
  print(show_lineup(file_list))
  print (z, digits = 3)
}


    

