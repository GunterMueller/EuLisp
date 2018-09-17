#include "defs.h"
#include "lamport.h"

void dijkstra_initialise(DijkstraSemaphore sem)
{
  int i;

  sem->k = 0;
  for (i=0; i<MAX_PROCESSORS; ++i) {
    sem->b[i] = TRUE;
    sem->c[i] = TRUE;
  }
}

void dijkstra_enter(DijkstraSemaphore sem,ProcID i)
{
  int j;

 l0:

  sem->b[i] = FALSE; 

 l1:

  if (sem->k != i) {

    sem->c[i] = TRUE;

    if (sem->b[sem->k]) sem->k = i;
    goto l1;

  }
  else {

    sem->c[i] = FALSE;

    for (j=0; j<RUNNING_PROCESSORS(); ++j)
      if ((j != i) && !sem->c[j]) goto l1;

  }
}

void dijkstra_exit(DijkstraSemaphore sem,ProcID i)
{
  sem->c[i] = TRUE;
  sem->b[i] = TRUE;
}



    
    

  
  
