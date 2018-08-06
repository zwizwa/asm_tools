#ifndef SEQ_PRIM_H
#define SEQ_PRIM_H

typedef int seq_t;
static inline seq_t seqADD(seq_t a, seq_t b) { return a + b; }
static inline seq_t seqSUB(seq_t a, seq_t b) { return a - b; }
static inline seq_t seqMUL(seq_t a, seq_t b) { return a * b; }

#endif
