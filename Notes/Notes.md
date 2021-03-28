Multiplication can be a shift-left (or right) (shl) instruction, e.g.,
  2*n
  
  
+ getParent() of an instruction gives the BasicBlock. getAllUsers() says where the value is used.
   + These are different concepts and we want both.  In many cases, you want getAllUsers().

+ getAllUsers() is different from getAllUses().
   +  getAllUsers() calls getAllUses() and then computes the getUser() from each of these.
   + getAllUses returns Use objects and these are the "edge betweens a Value and its users."
      + It allows us to get the collection of `User`s from a given Value.
   
+ Not all routines in a C/C++ file will appear in a Module.
   + This is because the may be inlined.
   + They may not be referenced but local to the translation unit so could be dropped.
   + Of course, they could be #ifdef'ed out 
