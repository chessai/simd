#include <stdint.h>
#include <stdio.h>

#include "simd_foo.h"

int main () {
  uint8_t source_a[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  uint8_t source_b[10] = { 1, 2, 3, 9, 5, 6, 7, 8, 9, 10 };
  uint8_t target[10];

  avx2_xor_bits_foo(target, 10, source_a, source_b);

  for (int i = 0; i < 10; i++) {
    printf("%i", target[i]);
  }
}

/*
HsInt length_bytearray
  ( StgArrBytes *arr
  ) {
    return ((StgArrBytes*) (((uint8_t*) arr) - 16))->bytes;
  }
*/

/*
#define ALIGNMENT_BYTES 32

uint8_t* align_pos
  ( uint8_t* ptr
  ) {
    uintptr_t r = ((uintptr_t)(ptr) % (uintptr_t)ALIGNMENT_BYTES);
    if (r == 0) {
      return ptr;
    } else {
      return ptr + (ALIGNMENT_BYTES - r);
    }
  }

uint8_t* align_neg
  ( uint8_t* ptr
  ) {
    uintptr_t r = ((uintptr_t)(ptr) % (uintptr_t)ALIGNMENT_BYTES);
    if (r == 0) {
      return ptr;
    } else {
      return ptr - r;
    }
  }

void avx2_xor_bits_foo
  ( uint8_t *target
  , size_t target_length
  , uint8_t *source_a
  , uint8_t *source_b
  ) {
    size_t i;

    for (i = 0; i < target_length; i++) {
      target[i] = source_a[i] ^ source_b[i];
    }
  }
*/

//region_xor_w64(unsigned char *r1, unsigned char *r2, unsigned int len)

//void slow_xor(
/*
void avx2_xor_bits_foo
  ( uint8_t *target
  , size_t target_length
  , uint8_t *source_a
  , uint8_t *source_b
  ) {
#if defined(AVX2_ENABLED)
  size_t i;

  uintptr_t s = (uintptr_t)32;
  uint8_t* sptr = (uint8_t*)s;

  uint8_t* start_pre_a, start_pre_b;
  uint8_t* end_pre_a, end_pre_b;
  uint8_t* start_mid_a, start_mid_b;
  uint8_t* end_mid_a, end_mid_b;
  uint8_t* start_post_a, start_post_b;
  uint8_t* end_post_a, end_post_b;

  start_pre_a = source_a;
  end_pre_a = align_pos(start_pre_a);
  start_mid_a = end_pre_a;
  end_post_a = (uint8_t*)((uintptr_t)(source_a) + (uintptr_t)(target_length));
  start_post_a = align_neg(end_post_a);
  end_mid_a = start_post_a;

  start_pre_b = source_b;
  end_pre_b = align_pos(start_pre_b);
  start_mid_b = end_pre_b;
  end_post_b = (uint8_t*)((uintptr_t)(source_b) + (uintptr_t)(target_length));
  start_post_b = align_neg(end_post_b);
  end_mid_b = start_post_b;

  //for (i = 0; i <
#endif
  }
*/

// #if defined(AVX2_ENABLED)
//   size_t i;
//   uint64_t rem_at_beginning = source_a % 32;
//   uint64_t rem_at_end = source_a + (uint64_t)(target_length / 32);
//   uint8_t *align_addr_beginning =
//
//   for (i = 0; i < target_length; i += 32) {
//     __m256i v_data_a   = *(__m256i *)(source_a + i);
//     __m256i v_data_b   = *(__m256i *)(source_b + i);
//     __m256i v_results  = _mm256_xor_si256(v_data_a, v_data_b);
//     *(__m256i *)(target + i) = v_results;
//   }
// #endif
// }

/*
#include "simd.h"

#include <immintrin.h>
#include <mmintrin.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

void avx2_memcpy(
    uint8_t *target,
    uint8_t *source,
    size_t len) {
#if defined(AVX2_ENABLED)
  size_t aligned_len    = (len / 32) * 32;
  size_t remaining_len  = len - aligned_len;

  size_t i;

  for (i = 0; i < aligned_len; i += 32) {
    __m128i v0 = *(__m128i*)(source + i     );
    __m128i v1 = *(__m128i*)(source + i + 16);

    *(__m128i*)(target + i      ) = v0;
    *(__m128i*)(target + i + 16 ) = v1;
  }

  memcpy(target + aligned_len, source + aligned_len, remaining_len);
#endif
}

void avx2_cmpeq8(
    uint8_t byte,
    uint8_t *target,
    size_t target_length,
    uint8_t *source) {
#if defined(AVX2_ENABLED)
  uint32_t *target32 = (uint32_t *)target;

  __m256i v_comparand = _mm256_set1_epi8(byte);

  uint32_t *out_mask = (uint32_t*)target;

  size_t i;

  for (i = 0; i < target_length * 2; ++i) {
    __m256i v_data_a = *(__m256i *)(source + (i * 32));
    __m256i v_results_a = _mm256_cmpeq_epi8(v_data_a, v_comparand);
    uint32_t mask = (uint32_t)_mm256_movemask_epi8(v_results_a);
    target32[i] = mask;
  }
#endif
}

void avx2_cmpeq8_para(
    uint8_t *bytes,
    size_t bytes_length,
    uint8_t **targets,
    size_t targets_length,
    uint8_t *source) {
#if defined(AVX2_ENABLED)
  size_t i;

  for (i = 0; i < targets_length * 2; ++i) {
    size_t j;

    __m256i v_data_a = *(__m256i *)(source + (i * 32));

    for (j = 0; j < bytes_length; ++j) {
      uint8_t *target     = targets[j];
      uint32_t *target32  = (uint32_t *)target;
      __m256i v_comparand = _mm256_set1_epi8(bytes[j]);
      uint32_t *out_mask  = (uint32_t*)target;
      __m256i v_results_a = _mm256_cmpeq_epi8(v_data_a, v_comparand);
      uint32_t mask       = (uint32_t)_mm256_movemask_epi8(v_results_a);
      target32[i]         = mask;
    }
  }
#endif
}

void avx2_and_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source_a,
    uint8_t *source_b) {
#if defined(AVX2_ENABLED)
  size_t i;

  for (i = 0; i < target_length; i += 32) {
    __m256i v_data_a   = *(__m256i *)(source_a + i);
    __m256i v_data_b   = *(__m256i *)(source_b + i);
    __m256i v_results  = _mm256_and_si256(v_data_a, v_data_b);
    *(__m256i *)(target + i) = v_results;
  }
#endif
}

void avx2_and_not_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source_a,
    uint8_t *source_b) {
#if defined(AVX2_ENABLED)
  size_t i;

  for (i = 0; i < target_length; i += 32) {
    __m256i v_data_a   = *(__m256i *)(source_a + i);
    __m256i v_data_b   = *(__m256i *)(source_b + i);
    __m256i v_results  = _mm256_andnot_si256(v_data_a, v_data_b);
    *(__m256i *)(target + i) = v_results;
  }
#endif
}

void avx2_not_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source) {
#if defined(AVX2_ENABLED)
  __m256i ones = _mm256_set1_epi8(0xff);

  size_t i;

  for (i = 0; i < target_length; i += 32) {
    __m256i v_data     = *(__m256i *)(source + i);
    __m256i v_results  = _mm256_xor_si256(v_data, ones);
    *(__m256i *)(target + i) = v_results;
  }
#endif
}

void avx2_or_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source_a,
    uint8_t *source_b) {
#if defined(AVX2_ENABLED)
  size_t i;

  for (i = 0; i < target_length; i += 32) {
    __m256i v_data_a   = *(__m256i *)(source_a + i);
    __m256i v_data_b   = *(__m256i *)(source_b + i);
    __m256i v_results  = _mm256_or_si256(v_data_a, v_data_b);
    *(__m256i *)(target + i) = v_results;
  }
#endif
}

void avx2_xor_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source_a,
    uint8_t *source_b) {
#if defined(AVX2_ENABLED)
  size_t i;
  uint64_t rem_at_beginning = source_a % 32;
  uint64_t rem_at_end = source_a + (uint64_t)(target_length / 32);
  uint8_t *align_addr_beginning =

  if () {
    continue;
  } else {

  }

  for (i = 0; i < target_length; i += 32) {
    __m256i v_data_a   = *(__m256i *)(source_a + i);
    __m256i v_data_b   = *(__m256i *)(source_b + i);
    __m256i v_results  = _mm256_xor_si256(v_data_a, v_data_b);
    *(__m256i *)(target + i) = v_results;
  }
#endif
}
*/
