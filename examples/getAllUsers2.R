getAllUsers2 =
function(x)
{
    ans = u = getAllUsers(x)
    while(length(u)) {
        u =  unlist(lapply(u, getAllUsers))
        u = setdiff(u, ans)
        ans = union(ans, u)
    }
    ans
}
