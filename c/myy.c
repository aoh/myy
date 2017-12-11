/*
 ptr:    [00pppppp pppppppp]
 fixnum: [10ffffff ffffff00] -> 12-bit, good for ADC / DAC
 enum:   [10vvvvvv vvvvvv10] -> (null, true, false, halt, ...)
 header: [10ssssss sRtttt11] -> 32 types (16 raw, 16 alloc), 64 words max size
*/


#define word      uint16_t
#define TPROC      0  // all 0
#define TPAIR      3
#define TBYTES    16  // raw 0
#define TBYTESP   17  // raw 1
#define TBYTECODE 18  // raw 2

#define ptr(x)             (x & 0x3fff)
#define car(x)             heap[x+1]
#define cdr(x)             heap[x+2]
#define NREGS              16
#define BIMM               0x8000
#define BMARK              0x4000
#define IMASK              0x8003    // immediate tag and immediate type
#define immp(x)            (x & BIMM)
#define allocp(x)          (!(immp(x)))
#define markp(x)           (x & BMARK)
#define unmark(x)          (x & 0xbfff)
#define tmark(x)           (x ^ BMARK)
#define fixnump(x)         ((x & IMASK) == BIMM)
#define headerp(x)         ((x & IMASK) == IMASK) // immediate and imm type 11
#define fixval(x)          ((x & 0x7fff) >> 2)
#define rawhdrp(x)         ((x) & 64)
#define fixnum(x)          (((word)x << 2) | (word)BIMM)
#define immediate(v, t)    (BIMM | t | (v << 2))  // 10vvvvvvvvvvvvtt
#define highb(x)           (x >> 4)
#define lowb(x)            (x & 15)

#define W                  2

#define header(s, t)       (0x8003 | (t << 2) | (s << 7))
#define hdrtype(h)         ((h >> 2) & 31)
#define hdrsize(h)         ((h >> 7) & 127) // header is not counted

#define HEAPEND            (HEAPSIZE - 17) // # of regs + 1

word regs[16];
word fp = FP;

void rev(word pos) {
   word val = heap[pos];
   word next = heap[unmark(val)];
   heap[pos] = next;
   heap[unmark(val)] = (val&BMARK)^(pos|BMARK);
} 

/* flagged ptr → unflagged ptr */
word chase(word ptr) {
   word val;
   ptr = unmark(ptr);
   val = heap[ptr];
   while (allocp(val) && markp(val)) { // todo: single comparison
      ptr = unmark(val);
      val = heap[ptr];
   }
   return ptr;
}

/* entry: pos = last field of root object, end is header pointer of it */
/* todo: get just the root object  */
void mark(word pos, word end) {
   printf("started marking from root pos %d, end %d\n", pos, end);
   while(pos != end) {
      word val = heap[pos];
      printf("mark %d -> %d\n", pos, val);
      if (immp(val)) {
         printf(" - imm, skipped\n");
         pos--;
      } else if (markp(val)) {
         printf(" - marked, chasing up to parent\n");
         pos = chase(val) - 1;
      } else {
         word hdr = heap[val];
         rev(pos);
         if (rawhdrp(hdr)) {
            printf(" - marked live raw thing\n");
            pos--;
         } else if ( - markp(hdr)) {
            printf("already marked\n");
            pos--;
         } else {
            printf(" - going to mark %d\n", val);
            pos = val + hdrsize(hdr);
         }
      }
   }
   printf("Mark finished at pos %d = end %d\n", pos, end);
}

void compact(word end) {
   word old = 0;
   word new = 0;
   printf("compact --------------------------------------------\n");
   while(old < end) {
      word val = heap[old];
      printf("new %d, old %d, end %d\n", new, old, end);
      if (markp(val)) {
         word hdr;
         uint8_t s;
         heap[new] = val;
         while(markp(heap[new])) {
            rev(new);
         }
         s = hdrsize(heap[new]);
         if (old == new) {
            old += s + 1;
            new += s + 1;
         } else {
            old++; new++; // header already copied
            while(s--) {
               heap[new++] = heap[old++];
            }
         }
      } else {
         old += 1 + hdrsize(val);
      }
   }
   fp = new;
}

void gc(uint8_t n) {
   uint8_t r = 0;
   word oldfp = fp;
   //printf("GC ----------------------------------------\n");
   //printf("gc from fp=%d\n", fp);
   heap[fp] = HREGS;
   //printf("hregs to %d\n", fp);
   for(r=0;r<16;r++) {
      //printf("r%d -> %d\n", r, fp+1+r);
      heap[fp+1+r] = regs[r];
   }
   mark(fp+16, fp);
   compact(fp); // registers should already have reversed values now
   for(r=0;r<16;r++) {
      regs[r] = heap[oldfp+1+r];
   }
   //printf("GC DONE---------------------------------------\n");
}

#define MAXALLOC  10 // testing

int vm(uint16_t entry) {
  uint8_t nargs = 3;
  uint16_t tmp;
  uint16_t h;
  uint8_t *ip;
  uint8_t op;
  regs[0] = entry;
  regs[1] = IHALT; // no MCP yet
  regs[2] = IHALT; // halt cont
  regs[3] = INULL; // very little machine info
  apply:
  // check that we have enough space for maximum allocation amount per frame
  // this avoids need to check for gc at each alloc within the frame and 
  // having to track location of instruction pointer, which cannot move due 
  // to gc this way
  if (fp >= (HEAPEND - MAXALLOC)) {
    gc(MAXALLOC);
  }
  tmp = regs[0];
  if (immp(tmp)) {
    if (tmp == IHALT) {
      if(fixnump(regs[2])) {
         return fixval(regs[2]);
      } else {
         return 127;
      }
    } else {
      return 126;
    }
  }
  h = hdrtype(heap[tmp]);
  if (h == TBYTECODE) {
    dispatch_bytecode:
    ip = (uint8_t *) (heap + tmp + 1);
    dispatch:
    switch(*ip) {
      /*      opname    (highbits lowbits) */
      case 1: // call (reg | nargs)
        op = ip[1];
        nargs = lowb(op);
        regs[0] = regs[highb(op)];
        goto apply;
      case 2: // mov (from | to)
        op = ip[1];
        regs[lowb(op)] = regs[highb(op)];
        ip += 2;
        goto dispatch;
      case 3: // ldi (from | to) offset, indexed from 0
        op = ip[1];
        regs[lowb(op)] = heap[regs[highb(op)] + ip[2]];
        ip += 3;
        goto dispatch;
      case 4: // enter n
        nargs = ip[1];
        goto apply;
      case 5: // RET n = mov n -> r3, call n 1
        op = ip[1];
        regs[0] = regs[2];
        regs[2] = regs[lowb(op)];
        nargs = highb(op);
        goto apply;
      case 6: // lde (load from offset via r0)
        op = ip[1];
        regs[lowb(op)] = heap[regs[0] + highb(op)];
        ip += 2;
        goto dispatch;
      case 7: // eq
        op = ip[1];
        // todo, branch avoidable
        regs[ip[2]] = (regs[highb(op)] == regs[lowb(op)]) ? ITRUE : IFALSE;
        ip += 3;
        goto dispatch;
      case 8: // arity-or-fail
        if (nargs != ip[1]) {
          fail("arity error", nargs);
        }
        ip += 2;
        goto dispatch;
      case  9: // jump-if-eq (a | b) n
        op = ip[1];
        if (regs[lowb(op)] == regs[highb(op)]) {
          ip += ip[2];
        } else {
          ip += 3;
        }
        goto dispatch;
      case 10: // load-literal-enum val reg
        op = ip[1];
        regs[lowb(op)] = immediate(highb(op), 2);
        ip += 2;
        goto dispatch;
      case 11: // load-small-fixnum n reg
        op = ip[1];
        regs[lowb(op)] = fixnum(highb(op));
        ip += 2;
        goto dispatch;
      case 12: { // add a b reg, only positive fixnums
        op = ip[1];
        regs[ip[2]] = fixnum((fixval(regs[highb(op)]) + fixval(regs[lowb(op)])));
        ip += 3;
        goto dispatch; }
      case 13: { // mul a b reg, only positive fixnums
        op = ip[1];
        regs[ip[2]] = fixnum(fixval(regs[highb(op)]) * fixval(regs[lowb(op)]));
        ip += 3;
        goto dispatch; }
      case 14: { // sub a b reg, only positive fixnums
        op = ip[1];
        regs[ip[2]] = fixnum((fixval(regs[highb(op)]) - fixval(regs[lowb(op)])));
        ip += 3;
        goto dispatch; }
      case 15: { // div a b reg, only positive fixnums
        op = ip[1];
        regs[ip[2]] = fixnum(fixval(regs[highb(op)]) / fixval(regs[lowb(op)]));
        ip += 3;
        goto dispatch; }
      case 16: { // bit-and a b reg, only positive fixnums
        op = ip[1];
        regs[ip[2]] = regs[highb(op)] & regs[lowb(op)];
        ip += 3;
        goto dispatch; }
      case 22: { // bit-or a b reg, only positive fixnums
        op = ip[1];
        regs[ip[2]] = regs[highb(op)] | regs[lowb(op)];
        ip += 3;
        goto dispatch; }
      case 23: { // bit-xor a b reg, only positive fixnums
        op = ip[1];
        regs[ip[2]] = (word)(regs[highb(op)] ^ regs[lowb(op)]) | (word)BIMM;
        ip += 3;
        goto dispatch; }
      case 17:  // jif reg amount
        op = ip[1];
        if (regs[lowb(op)] == IFALSE) {
          ip += 3;
        } else {
          ip += ip[2];
        }
        goto dispatch;
      case 18: // car
        op = ip[1];
        regs[lowb(op)] = heap[regs[highb(op)] + 1];
        ip += 2;
        goto dispatch;
      case 19: // cdr
        op = ip[1];
        regs[lowb(op)] = heap[regs[highb(op)] + 2];
        ip += 2;
        goto dispatch;
      case 20: // cons
        op = ip[1];
        heap[fp] = HPAIR;
        heap[fp+1] = regs[highb(op)];
        heap[fp+2] = regs[lowb(op)];
        regs[ip[2]] = fp;
        fp += 3;
        ip += 3;
        goto dispatch;
      case 21: // emit
         op = ip[1];
         regs[lowb(op)] = (put_byte(fixval(regs[highb(op)])) == 1) ? ITRUE : IFALSE;
         ip += 2;
         goto dispatch;
      default:
        return 255;
    }
    ip++;
    goto dispatch;
  } else if (h == TPROC) {
    tmp = heap[tmp+1];
    goto dispatch_bytecode;
  } else {
    return 124;
  }
}
