#include <stdio.h>

void *calloc(size_t nmemb, size_t size);

int heap[] = {2, 2, 38, 2, 342, 12, 94, 20, 38, 2, 36, 2, 2, 44, 2, 52, 60, 2, 28, 68, 3};

#define CELLS 10000
#define allocp(val) (0==(((int)val)&2))
#define immediatep(val) (2==(((int)val)&2))
#define imm(type,payload) (2 | (type << 3) | (payload << 8))
#define fixval(n) (((int) n) >> 3)
#define MNULL imm(0, 0)
#define MTRUE imm(1, 1)
#define MFALSE imm(1, 0)
#define aof(inst) ((inst >> 6) & 255)
#define bof(inst) (inst >> 14)
int S, E, C, D;
int *start, fp, *end;

#define car(ptr) *((int *) (((int) ptr) & ~4))
#define cdr(ptr) *((int *) (4 | ((int) ptr)))

int run() {
   while (C != MNULL) {
      int inst = fixval(car(C));
      switch(inst&63) {
         case 11: /* op-load-value */
            printf("Would load value %d\n", fixval(car(cdr(C))));
            C = cdr(cdr(C));
            break;
         default:
            printf("vm: what inst is %d\n", inst&63);
            return 1;
      }
   }
   printf("C is null, what now?\n");
   return 1;
}

void load_heap() {
   int pos = 0;
   int *hp = start;
   int val;
   while (1) {
      int val = heap[pos];
      if (val == 3) { break; }
      *hp = (immediatep(val) ? val : ((int) start) + val);
      hp++;
      pos++;     
   }
   hp -= 2; /* move to last object*/
   S = MNULL;
   E = MNULL;
   C = hp[0];
   E = hp[1];
}

int main(int nargs, char **args) {
   start  = (int *) calloc((sizeof (int *)), 2 * CELLS);
   end = ((int *) start) + 2*CELLS;
   load_heap();   
   return run();
}
