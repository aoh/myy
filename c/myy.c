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
#define BTAG               BIMM
#define BMARK              0x4000
#define IMASK              0x8003    // immediate tag and immediate type
#define immp(x)            (x & BIMM)
#define tagp(x)            (x & BTAG)
#define ttag(x)            (x ^ BTAG)
#define mask               BMARK
#define isbit_mark(x)      (x & mask)
#define setbit_mark(x)     (x | mask)
#define unsetbit_mark(x)   (x ^ mask)
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

word regs[16];
word fp = 0;

int vm(uint16_t entry) {
  uint8_t nargs = 3;
  uint16_t rator;
  uint16_t h;
  uint8_t *ip;
  uint8_t op;
  regs[0] = entry;
  regs[1] = IHALT; // no MCP yet
  regs[2] = IHALT; // halt cont
  regs[3] = INULL; // very little machine info
  apply:
  rator = regs[0];
  if (immp(rator)) {
    if (rator == IHALT) {
      if(fixnump(regs[2])) {
         return fixval(regs[2]);
      } else {
         return 127;
      }
    } else {
      return 126;
    }
  }
  h = hdrtype(heap[rator]);
  if (h == TBYTECODE) {
    dispatch_bytecode:
    ip = (uint8_t *) (heap + rator + 1);
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
          return 123;
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
      case 10: // load-enum val reg
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
      case 17:  // jif reg amount
        op = ip[1];
        if (regs[lowb(op)] == IFALSE) {
          ip += 3;
        } else {
          ip += ip[2];
        }
        goto dispatch;
      default:
        return 255;
    }
    ip++;
    goto dispatch;
  } else if (h == TPROC) {
    rator = heap[rator+1];
    goto dispatch_bytecode;
  } else {
    return 124;
  }
}
