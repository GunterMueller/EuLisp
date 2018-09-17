typedef int ProcID;
typedef int Flag;

struct dijkstra_semaphore {
  Flag b[MAX_PROCESSORS];
  Flag c[MAX_PROCESSORS];
  ProcID k;
};

typedef struct dijkstra_semaphore* DijkstraSemaphore;

#define NONPROCID (-1)

#ifndef TRUE
#define TRUE (1)
#define FALSE (0)
#endif

extern int system_running_processors;
#define RUNNING_PROCESSORS() (system_running_processors)

extern void dijkstra_initialise(DijkstraSemaphore);
extern void dijkstra_enter(DijkstraSemaphore,ProcID);
extern void dijkstra_exit(DijkstraSemaphore,ProcID);

/* Hacked lamport conversion... */

#define lamport_initialise(s) dijkstra_initialise(s)
#define lamport_enter(s,i) dijkstra_enter(s,i)
#define lamport_exit(s,i) dijkstra_exit(s,i)

#define lamport_semaphore dijkstra_semaphore
#define LamportSemaphore DijkstraSemaphore

