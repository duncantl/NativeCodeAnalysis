

mkRoutineFileTOC =
    #
    # For all the files in the specified directory, get a list of the defined Functions
    # based on the ll i.e. IR files.
    #
function(dir, files = list.files(dir, pattern = "\\.ll$", full.names = TRUE), byFile = FALSE)
{
   toc = lapply(files, getDefinedRoutines)
   if(byFile)
      structure(toc, names = files)
   else
      data.frame(routine = unlist(toc), file = rep(files, sapply(toc, length)), stringsAsFactors = FALSE)
}

getDefinedRoutines =
    #
    # In the module, find the Functions that have a body.
    #
function(file, module = parseIR(file))
{
    w = sapply(names(module), function(x) isDefinedRoutine(module[[x]]))
    names(module)[w]
} 


isDefinedRoutine =
    #
    # Check the element `fun` in the Module is a Function and has at least one BasicBlock.
    #
function(fun)
{
  is(fun, "Function") && (length(getBlocks(fun)) > 0)
}



getRoutineFile =
function(routine, toc)
{
    i = match(routine, toc$routine)
    if(is.na(i))
        stop("no routine named ", routine)
    
    toc$file[i]
}
