if(FALSE) {
  .Call("bob", 1, 2)
} else {
   a = 2
   b = 3
   .C("jane", a, b, TRUE)
}

f = function(a)
{
    if(FALSE)
        .Call("check")

    .Call("real")

    if(FALSE)
        .C("fake")
    else
        .C("real2")
}
