#include "myy.h"

/*
 ptr:    [00pppppp pppppppp]
 fixnum: [10ffffff ffffff00] -> 12-bit, good for ADC / DAC
 enum:   [10vvvvvv vvvvvv10] -> (null, true, false, halt, ...)
 header: [10ssssss sRtttt11] -> 32 types (16 raw, 16 alloc), 64 words max size
*/


#define STATUSLED 13

#define TPROC      0  // all 0
#define TPAIR      3
#define TBYTES    16  // raw 0
#define TBYTESP   17  // raw 1
#define TBYTECODE 18  // raw 2

#include <Adafruit_DotStar.h>

#define DATAPIN 7
#define CLOCKPIN 8

Adafruit_DotStar strip = Adafruit_DotStar(1, DATAPIN, CLOCKPIN, DOTSTAR_BRG);

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
#define INULL  0x8002
#define ITRUE  0x800a
#define IFALSE 0x8012
#define IHALT  0x801a

#define W                  2

#define header(s, t)       (0x8003 | (t << 2) | (s << 7))
#define hdrtype(h)         ((h >> 2) & 31)
#define hdrsize(h)         ((h >> 7) & 127) // header is not counted

uint32_t vm_ops;
uint32_t vm_calls;
uint32_t vm_start;

uint16_t ticker;

word regs[16];
word fp = 0;

void pixel(uint8_t r, uint8_t g, uint8_t b) {
  uint32_t color = 0x000000;
  color = (g << 16) | (r << 8) | b;
  strip.setPixelColor(0, color);
  strip.show();
}

void evaling() {
  pixel(0, 255, 0);
}

void marking() {
  pixel(255, 0, 0);
}

void collecting() {
  pixel(0, 0, 255);
}
void fail(char *reason) {
  Serial.println("FAIL  ");
  Serial.println(reason);
  int b = 0;
  int db = 1;
  while (1) {
    if (b == 255) {
      db = -1;
    } else if (b == 0) {
      db = 1;
    }
    pixel(255, 0, b);
    b += db;
    delay(3);
  }
}

void win(char *reason) {
  Serial.print("SUCCESS ");
  Serial.println(reason);
  int r = 0;
  int dr = 1;
  while (1) {
    if (r == 255) {
      dr = -1;
    } else if (r == 0) {
      dr = 1;
    }
    pixel(r, 255, 0);
    r += dr;
    delay(3);
  }
}

uint8_t get_byte() {
  uint8_t c;
  int i = 0;
  int d = 1;
  while (Serial.available() < 1) {
    pixel(i,i,i);
    delay(1);
    if (i == 0)
      d = 1;
    if (i == 255)
      d = -1;
    i += d;
  }
  c = Serial.read();
  evaling();
  return c;
}

void setup() {
  word tmp;
  vm_start = 0;
  vm_ops = 0;
  vm_calls = 0;
  Serial.begin(9600);
  while(!Serial) delay(100);
  pinMode(STATUSLED, OUTPUT);
  strip.begin();
  strip.show();
  while (!Serial)
    delay(10);
  pixel(0, 0, 255);
  pixel(0, 255, 0);
  pixel(0, 255, 255);
}

int output(word val, int lim) {
  if (!lim) {
    Serial.print("...");
    return lim;
  }
  lim--;
  if (immp(val)) {
    if (fixnump(val)) {
      Serial.print(fixval(val));
    } else if (val == INULL) {
      Serial.print("()");
    } else if (val == ITRUE) {
      Serial.print("#t");
    } else if (val == IFALSE) {
      Serial.print("#f");
    } else if (val == IHALT) {
      Serial.print("#halt");
    } else if (headerp(val)) {
      int s = hdrsize(val);
      int t = hdrtype(val);
      Serial.print("HDR_");
      Serial.print(s);
      Serial.print("s+");
      Serial.print(t);
      Serial.print("t");
    } else {
      val >>= 1;
      int type = val & 3;
      val >>= 2;
      val = val & 0xfff;
      Serial.print(val);
      Serial.print("v+");
      Serial.print(type);
      Serial.print("t");
    }
  } else {
    word h = heap[val];
    if (!headerp(h)) {
      Serial.print("PTR-TO-NONHDR");
    } else if (rawhdrp(h)) {
      Serial.print("<rawthing>");
    } else {
      int s = hdrsize(h) + 1;
      int t = hdrtype(h);
      if (t == TPAIR) {
        Serial.print("(");
        lim = output(heap[val+1], lim);
        Serial.print(" . ");
        lim = output(heap[val+2], lim);
        Serial.print(")");
      } else {
        Serial.print("[");
        while(s--) {
          lim = output(heap[val], lim);
          val++;
          if (s) 
            Serial.print(" ");
        }
        Serial.print("]");
      }
    }
  }
  return lim;
}

void print_regs() {
  for (int n = 0; n < 16; n++) {
    Serial.print("r");
    Serial.print(n);
    Serial.print("=");
    output(regs[n], 30);
    Serial.print(" ");
  }
  Serial.println();
}

void out(char *name, word ob) {
  Serial.print(name);
  Serial.print(" ");
  output(ob, 30);
  Serial.println();
}

void vm(uint16_t entry) {
  uint8_t nargs = 3; // call with machine info at r3 at startup + mcp & halt cont
  uint16_t rator;
  uint16_t h;
  uint8_t *ip;
  uint8_t op;
  uint32_t steps = 0;
  uint32_t start = millis();
  regs[0] = entry;
  regs[1] = IHALT; // no MCP yet
  regs[2] = IHALT; // halt cont
  regs[3] = INULL;  // very little machine info
  apply:
  steps++;
  //Serial.print("apply with nargs="); Serial.println(nargs);
  //delay(1000);
  rator = regs[0];
  if (immp(rator)) {
    if (rator == IHALT) {
      uint32_t elapsed = millis() - start;
      out("RESULT: ", regs[2]);
      Serial.print("VM took "); Serial.print(steps); Serial.print(" steps in ");
      Serial.print(elapsed); Serial.print("ms -> ");
      Serial.print((steps*1.0F) / elapsed); Serial.println(" ops/ms");
      win("halted"); 
    } else {
      Serial.println(rator);
      fail("immediate operator");
    }
  }
  h = hdrtype(heap[rator]);
  if (h == TBYTECODE) {
    dispatch_bytecode:
    ip = (uint8_t *) (heap + rator + 1);
    dispatch:
    steps++;
    //print_regs();
    //Serial.print("exec "); Serial.println(*ip);
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
        //print_regs();
        regs[0] = regs[2];
        regs[2] = regs[lowb(op)];
        nargs = highb(op);
        goto apply;
      case 6: // lde (load from offset via r0)
        op = ip[1];
        Serial.print("LDE: loading r0[");
        Serial.print(highb(op));
        Serial.print("] -> r");
        Serial.println(lowb(op));
        out("r0 value is", heap[regs[0]]);
        regs[lowb(op)] = heap[regs[0] + highb(op)];
        ip += 2;
        goto dispatch;
      case 8: // arity-or-fail
        if (nargs != ip[1]) {
          fail("arity");
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
        out("reg now has ", regs[lowb(op)]);
        ip += 2;
        goto dispatch;
      case 11: // load-small-fixnum n reg
        op = ip[1];
        regs[lowb(op)] = fixnum(highb(op));
        ip += 2;
        goto dispatch;
      case 12: { // add a b reg, only positive fixnums
        op = ip[1];
        regs[ip[2]] = fixnum(fixval(regs[highb(op)]) + fixval(regs[lowb(op)]));
        ip += 3;
        goto dispatch; }
      case 13: { // mul a b reg, only positive fixnums
        op = ip[1];
        regs[ip[2]] = fixnum(fixval(regs[highb(op)]) * fixval(regs[lowb(op)]));
        ip += 3;
        goto dispatch; }
      case 14: { // sub a b reg, only positive fixnums
        op = ip[1];
        regs[ip[2]] = fixnum(fixval(regs[highb(op)]) - fixval(regs[lowb(op)]));
        ip += 3;
        goto dispatch; }
      case 15: { // div a b reg, only positive fixnums
        op = ip[1];
        regs[ip[2]] = fixnum(fixval(regs[highb(op)]) / fixval(regs[lowb(op)]));
        ip += 3;
        goto dispatch; }
      default:
        fail("bad op");
    }
    ip++;
    goto dispatch;
  } else if (h == TPROC) {
    rator = heap[rator+1];
    goto dispatch_bytecode;
  } else {
    fail("not applicable");
  }
}

void loop() {
  word tmp;
  out("ENTRY ", ENTRY);
  ticker = 1000;
  for (int reg = 0; reg < NREGS; reg++) {
    regs[reg] = INULL;
  }
  vm(ENTRY);
  win("ok");
}

