int f(int x)
{
    return(2*x + 1);
}

void
foo(int *x, int *len)
{
    for(int i = 0;  i < *len; i++)
	x[i] = f(x[i]);
}


void
bar(int *x, int *len, int *ans)
{
    for(int i = 0;  i < *len; i++)
	ans[i] = f(x[i]);
}
