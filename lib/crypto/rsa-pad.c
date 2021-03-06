#include <dfsch/lib/crypto.h>
#include <dfsch/bignum.h>
#include <string.h>

static size_t bitlength_to_octets(int l){
  if (l % 8 == 0){
    return l / 8;
  } else {
    return l / 8 +1;
  }
}

static void memxor(uint8_t* dst, uint8_t* src, size_t count){
  while (count){
    *dst ^= *src;
    dst++;
    src++;
    count--;
  }
}

static uint8_t memxmp(uint8_t* dst, uint8_t* src, size_t count){
  uint8_t res = 0;
  while (count){
    res |= *src ^ *dst;
    dst++;
    src++;
    count--;
  }
  return res;
}

static uint8_t memor(uint8_t* src, size_t count){
  uint8_t res = 0;
  while (count){
    res |= *src;
    src++;
    count--;
  }
  return res;
}


static void mgf1(dfsch_crypto_hash_t* hash, 
                 uint8_t* in, size_t ilen,
                 uint8_t* out, size_t olen){
  uint32_t c;
  uint8_t cbuf[4];
  dfsch_crypto_hash_context_t* hc;

  if (!hash){
    hash = DFSCH_CRYPTO_SHA256;
  }

  uint8_t res[hash->result_len];
  
  c = 0;
  while(olen){
    hc = dfsch_crypto_hash_setup(hash, NULL, 0);
    hc->algo->process(hc, in, ilen);
    
    cbuf[3] = c & 0xff;
    cbuf[2] = (c >> 8) & 0xff;
    cbuf[1] = (c >> 16) & 0xff;
    cbuf[0] = (c >> 24) & 0xff;
    c++;
    hc->algo->process(hc, cbuf, 4);
    hc->algo->result(hc, res);
    
    if (olen > hash->result_len){
      memxor(out, res, hash->result_len);
      out += hash->result_len;
      olen -= hash->result_len;
    } else {
      memxor(out, res, olen);
      break;
    }
  }
}

dfsch_object_t* dfsch_crypto_oaep_encode(dfsch_crypto_hash_t* hash,
                                         dfsch_object_t* random_source,
                                         size_t len,
                                         uint8_t* data,
                                         size_t dlen,
                                         uint8_t* label,
                                         size_t llen){
  uint8_t* buf;
  size_t rlen;
  dfsch_crypto_hash_context_t* hc;

  len = bitlength_to_octets(len);

  if (!hash){
    hash = DFSCH_CRYPTO_SHA256;
  }

  rlen = hash->result_len;

  if (dlen + 2*rlen + 2 > len){
    dfsch_error("Message too long", NULL);
  } 
  
  buf = GC_MALLOC_ATOMIC(len);

  memset(buf, 0, len);
  dfsch_random_get_bytes(random_source, buf + 1, rlen);
  hc = dfsch_crypto_hash_setup(hash, NULL, 0);
  hc->algo->process(hc, label, llen);
  hc->algo->result(hc, buf + rlen + 1);

  memcpy(buf + len - dlen, data, dlen);
  buf[len - dlen - 1] = 0x01;

  mgf1(hash, buf + 1, rlen, buf + rlen + 1, len - rlen - 1);
  mgf1(hash, buf + rlen + 1, len - rlen - 1, buf + 1, rlen);

  return dfsch_bignum_to_number(dfsch_bignum_from_bytes(buf, len, 0));
}
dfsch_strbuf_t* dfsch_crypto_oaep_decode(dfsch_crypto_hash_t* hash,
                                         size_t len,
                                         dfsch_object_t* m,
                                         uint8_t* label,
                                         size_t llen){
  uint8_t* buf;
  size_t rlen;
  dfsch_crypto_hash_context_t* hc;
  dfsch_strbuf_t* ms;
  uint8_t* mptr;
  uint8_t cres;
  uint8_t* lh;

  len = bitlength_to_octets(len);

  if (!hash){
    hash = DFSCH_CRYPTO_SHA256;
  }

  rlen = hash->result_len;

  if (2*rlen + 2 > len){
    dfsch_error("Invalid length", NULL);
  } 
  
  buf = GC_MALLOC_ATOMIC(len);
  lh = GC_MALLOC_ATOMIC(rlen);
  hc = dfsch_crypto_hash_setup(hash, NULL, 0);
  hc->algo->process(hc, label, llen);
  hc->algo->result(hc, lh);

  memset(buf, 0, len);
  ms = dfsch_bignum_to_bytes(dfsch_bignum_from_number(m));
  if (ms->len > len){
    dfsch_error("Message too long", m);
  } 
  memcpy(buf + (len - ms->len), ms->ptr, ms->len);

  mgf1(hash, buf + rlen + 1, len - rlen - 1, buf + 1, rlen);
  mgf1(hash, buf + 1, rlen, buf + rlen + 1, len - rlen - 1);

  cres = buf[0];
  cres |= memxmp(lh, buf + rlen + 1, rlen);
  mptr = memchr(buf + 2*rlen + 1, 0x01, len - 2*rlen - 1);
  cres |= memor(buf + 2*rlen + 1, mptr - (buf + 2*rlen + 1));
  
  if (mptr == NULL || cres != 0){
    dfsch_error("Invalid message", m);
  }
  
  mptr++;

  return dfsch_strbuf_create(mptr, len - (mptr - buf));
}

dfsch_object_t* dfsch_crypto_pss_encode(dfsch_crypto_hash_t* hash,
                                        dfsch_object_t* random_source,
                                        size_t bits,
                                        uint8_t* mh,
                                        size_t mhlen){
  uint8_t* buf;
  static uint8_t pad1[8] = {0,0,0,0, 0,0,0,0};
  size_t rlen;
  dfsch_crypto_hash_context_t* hc;
  size_t len;

  len = bitlength_to_octets(bits);

  if (!hash){
    hash = DFSCH_CRYPTO_SHA256;
  }

  rlen = hash->result_len;

  if (2*rlen + 2 > len){
    dfsch_error("Modulus too short", NULL);
  } 

  buf = GC_MALLOC_ATOMIC(len);

  memset(buf, 0, len);
  buf[len-1] = 0xbc;

  dfsch_random_get_bytes(random_source, buf + (len - 2*rlen - 1), rlen);

  hc = dfsch_crypto_hash_setup(hash, NULL, 0);
  hc->algo->process(hc, pad1, 8);
  hc->algo->process(hc, mh, mhlen);
  hc->algo->process(hc, buf + (len - 2*rlen - 1), rlen);
  hc->algo->result(hc, buf + (len - rlen - 1));

  buf[len - 2*rlen - 2] = 0x01;

  mgf1(hash, buf + (len - rlen - 1), rlen, buf, len - rlen - 1);

  if (bits % 8 != 0){
    buf[0] &= (1 << bits % 8) - 1;
  }

  return dfsch_bignum_to_number(dfsch_bignum_from_bytes(buf, len, 0));  
}
int dfsch_crypto_pss_verify(dfsch_crypto_hash_t* hash,
                            size_t bits,
                            dfsch_object_t* s,
                            uint8_t* mh,
                            size_t mhlen){
  uint8_t* buf;
  uint8_t* h;
  static uint8_t pad1[8] = {0,0,0,0, 0,0,0,0};
  size_t rlen;
  dfsch_crypto_hash_context_t* hc;
  size_t len;
  dfsch_strbuf_t* ss;
  uint8_t cres;

  len = bitlength_to_octets(bits);

  if (!hash){
    hash = DFSCH_CRYPTO_SHA256;
  }

  rlen = hash->result_len;

  if (2*rlen + 2 > len){
    dfsch_error("Modulus too short", NULL);
  } 

  buf = GC_MALLOC_ATOMIC(len);

  memset(buf, 0, len);
  ss = dfsch_bignum_to_bytes(dfsch_bignum_from_number(s));
  if (ss->len > len){
    dfsch_error("Signature too long", s);
  } 
  memcpy(buf + (len - ss->len), ss->ptr, ss->len);

  mgf1(hash, buf + (len - rlen - 1), rlen, buf, len - rlen - 1);
  
  if (bits % 8 != 0){
    buf[0] &= (1 << bits % 8) - 1;
  }

  h = GC_MALLOC_ATOMIC(rlen);

  hc = dfsch_crypto_hash_setup(hash, NULL, 0);
  hc->algo->process(hc, pad1, 8);
  hc->algo->process(hc, mh, mhlen);
  hc->algo->process(hc, buf + (len - 2*rlen - 1), rlen);
  hc->algo->result(hc, h);

  cres = buf[len - 2*rlen - 2] ^ 0x01;
  cres |= buf[len-1] ^ 0xbc;
  cres |= memxmp(buf + (len - rlen - 1), h, rlen);
  cres |= memor(buf, len - 2*rlen - 2);

  return cres == 0;
}
