

mkRoutineFileTOC =
    #
    # For all the files in the specified directory, get a list of the defined Functions
    # based on the ll i.e. IR files.
    #
function(dir, files = list.files(dir, pattern = pattern, full.names = TRUE), byFile = FALSE, names = TRUE,
         pattern = "\\.(ir|ll)$")
{
   toc = lapply(files, getDefinedRoutines, names = names)
   if(byFile)
      structure(toc, names = files)
   else
      data.frame(routine = unlist(toc), file = rep(files, sapply(toc, length)), stringsAsFactors = FALSE)
}




getRoutineFile =
function(routine, toc)
{
    i = match(routine, toc$routine)
    if(is.na(i))
        stop("no routine named ", routine)
    
    toc$file[i]
}
