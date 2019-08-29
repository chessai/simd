#include "simd.h"

#include <immintrin.h>
#include <mmintrin.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

#define ALIGNMENT_BYTES 32

void avx2_cmpeq8
  ( uint8_t byte
  , uint8_t *target
  , size_t target_length
  , uint8_t *source
  ) {
    uint32_t *target32 = (uint32_t*)target;

    __m256i v_comparand = _mm256_set1_epi8(byte);

    uint32_t *out_mask = (uint32_t*)target;

    size_t i;

    for (i = 0; i < target_length * 2; ++i) {
      __m256i v_data_a = *(__m256i *)(source + (i * 32));
      __m256i v_results_a = _mm256_cmpeq_epi8(v_data_a, v_comparand);
      uint32_t mask = (uint32_t)_mm256_movemask_epi8(v_results_a);
      target32[i] = mask;
    }

  }

    //uint8_t* restrict r_source = __builtin_assume_aligned(source,8);
    //uint8_t* restrict r_target = __builtin_assume_aligned(target,8);

    //uint64_t i;

    //for (int i = 0; i < target_length; i++) {
    //  r_target[i] = r_source[i] == byte;
    //}

/*
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
*/

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

void avx2_and_bits
  ( uint8_t* target
  , size_t target_length
  , uint8_t* r1
  , uint8_t* r2
  ) {
    uint8_t* restrict p1 = __builtin_assume_aligned(r1,8);
    uint8_t* restrict p2 = __builtin_assume_aligned(r2,8);
    uint8_t* restrict p3 = __builtin_assume_aligned(target,8);

    uint64_t i;
    for (i = 0; i < target_length; i++) {
      p3[i] = p1[i] & p2[i];
    }
  }

#define NAND(a,b) ~(a & b)

void avx2_and_not_bits
  ( uint8_t *target
  , size_t target_length
  , uint8_t *source_a
  , uint8_t *source_b
  ) {
    size_t i;

    for (i = 0; i < target_length; i++) {
      target[i] = NAND(source_a[i], source_b[i]);
    }
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

void avx2_or_bits
  ( uint8_t* target
  , size_t target_length
  , uint8_t* r1
  , uint8_t* r2
  ) {
    uint8_t* restrict p1 = __builtin_assume_aligned(r1,8);
    uint8_t* restrict p2 = __builtin_assume_aligned(r2,8);
    uint8_t* restrict p3 = __builtin_assume_aligned(target,8);

    uint64_t i;
    for (i = 0; i < target_length; i++) {
      p3[i] = p1[i] | p2[i];
    }
  }

void avx2_xor_bits
  ( uint8_t* target
  , size_t target_length
  , uint8_t* r1
  , uint8_t* r2
  ) {
    uint8_t* restrict p1 = __builtin_assume_aligned(r1,8);
    uint8_t* restrict p2 = __builtin_assume_aligned(r2,8);
    uint8_t* restrict p3 = __builtin_assume_aligned(target,8);

    uint64_t i;
    for (i = 0; i < target_length; i++) {
      p3[i] = p1[i] ^ p2[i];
    }
  }
