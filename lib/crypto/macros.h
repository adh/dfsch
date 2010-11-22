/* fix for MSVC ...evil! */
#ifdef _MSC_VER
   #define CONST64(n) n ## ui64
   typedef unsigned __int64 ulong64;
#else
   #define CONST64(n) n ## ULL
   typedef unsigned long long ulong64;
#endif

/* this is the "32-bit at least" data type 
 * Re-define it to suit your platform but it must be at least 32-bits 
 */
#if defined(__x86_64__) || (defined(__sparc__) && defined(__arch64__))
   typedef unsigned ulong32;
#else
   typedef unsigned long ulong32;
#endif

/* ---- HELPER MACROS ---- */

#define STORE32L(x, y)                                                                     \
     { (y)[3] = (unsigned char)(((x)>>24)&255); (y)[2] = (unsigned char)(((x)>>16)&255);   \
       (y)[1] = (unsigned char)(((x)>>8)&255); (y)[0] = (unsigned char)((x)&255); }

#define LOAD32L(x, y)                            \
     { x = ((unsigned long)((y)[3] & 255)<<24) | \
           ((unsigned long)((y)[2] & 255)<<16) | \
           ((unsigned long)((y)[1] & 255)<<8)  | \
           ((unsigned long)((y)[0] & 255)); }

#define STORE64L(x, y)                                                                     \
     { (y)[7] = (unsigned char)(((x)>>56)&255); (y)[6] = (unsigned char)(((x)>>48)&255);   \
       (y)[5] = (unsigned char)(((x)>>40)&255); (y)[4] = (unsigned char)(((x)>>32)&255);   \
       (y)[3] = (unsigned char)(((x)>>24)&255); (y)[2] = (unsigned char)(((x)>>16)&255);   \
       (y)[1] = (unsigned char)(((x)>>8)&255); (y)[0] = (unsigned char)((x)&255); }

#define LOAD64L(x, y)                                                       \
     { x = (((ulong64)((y)[7] & 255))<<56)|(((ulong64)((y)[6] & 255))<<48)| \
           (((ulong64)((y)[5] & 255))<<40)|(((ulong64)((y)[4] & 255))<<32)| \
           (((ulong64)((y)[3] & 255))<<24)|(((ulong64)((y)[2] & 255))<<16)| \
           (((ulong64)((y)[1] & 255))<<8)|(((ulong64)((y)[0] & 255))); }

#define STORE32H(x, y)                                                                     \
     { (y)[0] = (unsigned char)(((x)>>24)&255); (y)[1] = (unsigned char)(((x)>>16)&255);   \
       (y)[2] = (unsigned char)(((x)>>8)&255); (y)[3] = (unsigned char)((x)&255); }

#define LOAD32H(x, y)                            \
     { x = ((unsigned long)((y)[0] & 255)<<24) | \
           ((unsigned long)((y)[1] & 255)<<16) | \
           ((unsigned long)((y)[2] & 255)<<8)  | \
           ((unsigned long)((y)[3] & 255)); }

#define STORE64H(x, y)                                                                     \
   { (y)[0] = (unsigned char)(((x)>>56)&255); (y)[1] = (unsigned char)(((x)>>48)&255);     \
     (y)[2] = (unsigned char)(((x)>>40)&255); (y)[3] = (unsigned char)(((x)>>32)&255);     \
     (y)[4] = (unsigned char)(((x)>>24)&255); (y)[5] = (unsigned char)(((x)>>16)&255);     \
     (y)[6] = (unsigned char)(((x)>>8)&255); (y)[7] = (unsigned char)((x)&255); }

#define LOAD64H(x, y)                                                      \
   { x = (((ulong64)((y)[0] & 255))<<56)|(((ulong64)((y)[1] & 255))<<48) | \
         (((ulong64)((y)[2] & 255))<<40)|(((ulong64)((y)[3] & 255))<<32) | \
         (((ulong64)((y)[4] & 255))<<24)|(((ulong64)((y)[5] & 255))<<16) | \
         (((ulong64)((y)[6] & 255))<<8)|(((ulong64)((y)[7] & 255))); }

#define BSWAP(x)  ( ((x>>24)&0x000000FFUL) | ((x<<24)&0xFF000000UL)  | \
                    ((x>>8)&0x0000FF00UL)  | ((x<<8)&0x00FF0000UL) )



/* rotates the hard way */
#define ROL(x, y) ( (((unsigned long)(x)<<(unsigned long)((y)&31)) | (((unsigned long)(x)&0xFFFFFFFFUL)>>(unsigned long)(32-((y)&31)))) & 0xFFFFFFFFUL)
#define ROR(x, y) ( ((((unsigned long)(x)&0xFFFFFFFFUL)>>(unsigned long)((y)&31)) | ((unsigned long)(x)<<(unsigned long)(32-((y)&31)))) & 0xFFFFFFFFUL)
#define ROLc(x, y) ( (((unsigned long)(x)<<(unsigned long)((y)&31)) | (((unsigned long)(x)&0xFFFFFFFFUL)>>(unsigned long)(32-((y)&31)))) & 0xFFFFFFFFUL)
#define RORc(x, y) ( ((((unsigned long)(x)&0xFFFFFFFFUL)>>(unsigned long)((y)&31)) | ((unsigned long)(x)<<(unsigned long)(32-((y)&31)))) & 0xFFFFFFFFUL)


#define ROL64(x, y) \
    ( (((x)<<((ulong64)(y)&63)) | \
      (((x)&CONST64(0xFFFFFFFFFFFFFFFF))>>((ulong64)64-((y)&63)))) & CONST64(0xFFFFFFFFFFFFFFFF))

#define ROR64(x, y) \
    ( ((((x)&CONST64(0xFFFFFFFFFFFFFFFF))>>((ulong64)(y)&CONST64(63))) | \
      ((x)<<((ulong64)(64-((y)&CONST64(63)))))) & CONST64(0xFFFFFFFFFFFFFFFF))

#define ROL64c(x, y) \
    ( (((x)<<((ulong64)(y)&63)) | \
      (((x)&CONST64(0xFFFFFFFFFFFFFFFF))>>((ulong64)64-((y)&63)))) & CONST64(0xFFFFFFFFFFFFFFFF))

#define ROR64c(x, y) \
    ( ((((x)&CONST64(0xFFFFFFFFFFFFFFFF))>>((ulong64)(y)&CONST64(63))) | \
      ((x)<<((ulong64)(64-((y)&CONST64(63)))))) & CONST64(0xFFFFFFFFFFFFFFFF))


#ifndef MAX
   #define MAX(x, y) ( ((x)>(y))?(x):(y) )
#endif

#ifndef MIN
   #define MIN(x, y) ( ((x)<(y))?(x):(y) )
#endif

/* extract a byte portably */
#ifdef _MSC_VER
   #define byte(x, n) ((unsigned char)((x) >> (8 * (n))))
#else
   #define byte(x, n) (((x) >> (8 * (n))) & 255)
#endif   


/* a simple macro for making hash "process" functions */
#define HASH_PROCESS(func_name, compress_name, type, block_size)   \
  int func_name (type* md, const unsigned char *in, unsigned long inlen) \
  {                                                                     \
    unsigned long n;                                                    \
    int           err;                                                  \
    while (inlen > 0) {                                                 \
      if (md->curlen == 0 && inlen >= block_size) {                     \
        compress_name (md, (unsigned char *)in);                        \
        md->length += block_size * 8;                                   \
        in             += block_size;                                   \
        inlen          -= block_size;                                   \
      } else {                                                          \
        n = MIN(inlen, (block_size - md->curlen));                      \
        memcpy(md->buf + md->curlen, in, (size_t)n);                    \
        md->curlen += n;                                                \
        in             += n;                                            \
        inlen          -= n;                                            \
        if (md->curlen == block_size) {                                 \
          compress_name (md, md->buf);                                  \
          md->length += 8*block_size;                                   \
          md->curlen = 0;                                               \
        }                                                               \
      }                                                                 \
    }                                                                   \
  }

