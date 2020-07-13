long unsigned int factorial(int n)
{
  long unsigned int product = n;

  if (n==0){
    return 1;
  }else if (n>0){
    while((--n)>0)
      product *= n;

    return product;
  }else{
    return 0;
  }
}

int
count_numbers(int n)
{
  int i=0;
  while(n>0){
    n = n/10;
    i++;
  }
  return i;
}
