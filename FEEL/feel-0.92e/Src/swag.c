extern int value;

void stack_switch_and_go(int stack,int fun)
{
  value = stack;
  ((void (*)()) fun)();
}

