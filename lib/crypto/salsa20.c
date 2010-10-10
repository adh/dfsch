#include <dfsch/lib/crypto.h>

#define U8V(v) ((uint8_t)(v) & 0xFF)
#define U32TO8_LITTLE(p, v) \
  do { \
    (p)[0] = U8V((v)      ); \
    (p)[1] = U8V((v) >>  8); \
    (p)[2] = U8V((v) >> 16); \
    (p)[3] = U8V((v) >> 24); \
  } while (0)
#define U8TO32_LITTLE(p) \
  (((uint32_t)((p)[0])      ) | \
   ((uint32_t)((p)[1]) <<  8) | \
   ((uint32_t)((p)[2]) << 16) | \
   ((uint32_t)((p)[3]) << 24))

#define ROTL32(v, n) \
  ((uint32_t)((v) << (n)) | ((v) >> (32 - (n))))

#define ROTATE(v,c) (ROTL32(v,c))
#define XOR(v,w) ((v) ^ (w))
#define PLUS(v,w) ((uint32_t)((v) + (w)))
#define PLUSONE(v) (PLUS((v),1))

void dfsch_salsa20_get_keystream_block(dfsch_salsa20_state_t* st,
                                       uint8_t output[64]){
  uint32_t x[16];
  int i;

  for (i = 0;i < 16;++i) x[i] = st->input[i];
  for (i = 20;i > 0;i -= 2) {
    x[ 4] = XOR(x[ 4],ROTATE(PLUS(x[ 0],x[12]), 7));
    x[ 8] = XOR(x[ 8],ROTATE(PLUS(x[ 4],x[ 0]), 9));
    x[12] = XOR(x[12],ROTATE(PLUS(x[ 8],x[ 4]),13));
    x[ 0] = XOR(x[ 0],ROTATE(PLUS(x[12],x[ 8]),18));
    x[ 9] = XOR(x[ 9],ROTATE(PLUS(x[ 5],x[ 1]), 7));
    x[13] = XOR(x[13],ROTATE(PLUS(x[ 9],x[ 5]), 9));
    x[ 1] = XOR(x[ 1],ROTATE(PLUS(x[13],x[ 9]),13));
    x[ 5] = XOR(x[ 5],ROTATE(PLUS(x[ 1],x[13]),18));
    x[14] = XOR(x[14],ROTATE(PLUS(x[10],x[ 6]), 7));
    x[ 2] = XOR(x[ 2],ROTATE(PLUS(x[14],x[10]), 9));
    x[ 6] = XOR(x[ 6],ROTATE(PLUS(x[ 2],x[14]),13));
    x[10] = XOR(x[10],ROTATE(PLUS(x[ 6],x[ 2]),18));
    x[ 3] = XOR(x[ 3],ROTATE(PLUS(x[15],x[11]), 7));
    x[ 7] = XOR(x[ 7],ROTATE(PLUS(x[ 3],x[15]), 9));
    x[11] = XOR(x[11],ROTATE(PLUS(x[ 7],x[ 3]),13));
    x[15] = XOR(x[15],ROTATE(PLUS(x[11],x[ 7]),18));
    x[ 1] = XOR(x[ 1],ROTATE(PLUS(x[ 0],x[ 3]), 7));
    x[ 2] = XOR(x[ 2],ROTATE(PLUS(x[ 1],x[ 0]), 9));
    x[ 3] = XOR(x[ 3],ROTATE(PLUS(x[ 2],x[ 1]),13));
    x[ 0] = XOR(x[ 0],ROTATE(PLUS(x[ 3],x[ 2]),18));
    x[ 6] = XOR(x[ 6],ROTATE(PLUS(x[ 5],x[ 4]), 7));
    x[ 7] = XOR(x[ 7],ROTATE(PLUS(x[ 6],x[ 5]), 9));
    x[ 4] = XOR(x[ 4],ROTATE(PLUS(x[ 7],x[ 6]),13));
    x[ 5] = XOR(x[ 5],ROTATE(PLUS(x[ 4],x[ 7]),18));
    x[11] = XOR(x[11],ROTATE(PLUS(x[10],x[ 9]), 7));
    x[ 8] = XOR(x[ 8],ROTATE(PLUS(x[11],x[10]), 9));
    x[ 9] = XOR(x[ 9],ROTATE(PLUS(x[ 8],x[11]),13));
    x[10] = XOR(x[10],ROTATE(PLUS(x[ 9],x[ 8]),18));
    x[12] = XOR(x[12],ROTATE(PLUS(x[15],x[14]), 7));
    x[13] = XOR(x[13],ROTATE(PLUS(x[12],x[15]), 9));
    x[14] = XOR(x[14],ROTATE(PLUS(x[13],x[12]),13));
    x[15] = XOR(x[15],ROTATE(PLUS(x[14],x[13]),18));
  }
  for (i = 0;i < 16;++i) x[i] = PLUS(x[i],st->input[i]);
  for (i = 0;i < 16;++i) U32TO8_LITTLE(output + 4 * i,x[i]);
  st->input[8]++;
  if (!st->input[8]){
    st->input[9]++;
  }
}
static const char sigma[16] = "expand 32-byte k";

void dfsch_salsa20_setkey(dfsch_salsa20_state_t* st, uint8_t k[32]){
  const char *constants;

  st->input[1] = U8TO32_LITTLE(k + 0);
  st->input[2] = U8TO32_LITTLE(k + 4);
  st->input[3] = U8TO32_LITTLE(k + 8);
  st->input[4] = U8TO32_LITTLE(k + 12);
  k += 16;
  constants = sigma;
  st->input[11] = U8TO32_LITTLE(k + 0);
  st->input[12] = U8TO32_LITTLE(k + 4);
  st->input[13] = U8TO32_LITTLE(k + 8);
  st->input[14] = U8TO32_LITTLE(k + 12);
  st->input[0] = U8TO32_LITTLE(constants + 0);
  st->input[5] = U8TO32_LITTLE(constants + 4);
  st->input[10] = U8TO32_LITTLE(constants + 8);
  st->input[15] = U8TO32_LITTLE(constants + 12);
}

/*
 * This is intended for RNG application: add more system randomnes into state.
 */
void dfsch_salsa20_addkey(dfsch_salsa20_state_t* st, uint8_t k[32]){
  st->input[1] += U8TO32_LITTLE(k + 0);
  st->input[2] += U8TO32_LITTLE(k + 4);
  st->input[3] += U8TO32_LITTLE(k + 8);
  st->input[4] += U8TO32_LITTLE(k + 12);
  k += 16;
  st->input[11] += U8TO32_LITTLE(k + 0);
  st->input[12] += U8TO32_LITTLE(k + 4);
  st->input[13] += U8TO32_LITTLE(k + 8);
  st->input[14] += U8TO32_LITTLE(k + 12);
  st->input[0] += 1;
  st->input[5] += 3;
  st->input[10] += 5;
  st->input[15] += 7;
}


void dfsch_salsa20_setiv(dfsch_salsa20_state_t* st, uint64_t iv){
  st->input[6] = iv & 0xffffffff;
  st->input[7] = (iv >> 32) & 0xffffffff;
  st->input[8] = 0;
  st->input[9] = 0;
}
void dfsch_salsa20_seek(dfsch_salsa20_state_t* st, uint64_t offset){
  st->input[8] = offset & 0xffffffff;
  st->input[9] = (offset >> 32) & 0xffffffff;
}
