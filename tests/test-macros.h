#ifndef H__adh__test_macros__
#define H__adh__test_macros__

#include <stdio.h>
#include <stdlib.h>

#include <dfsch/lib/cmdopts.h>

static int test__fail_fast = 0;

#define TEST_INIT(argc, argv)                   \
  int test__pass = 0;                           \
  int test__fail = 0;                           \
  test__init_impl((argc), (argv))

#define TEST(name, cond)                        \
  if ((cond)){                                  \
    test__pass++;                               \
    printf("   Test passed: \033[0;32m%s\033[0;39m\n", name);       \
  } else {                                      \
    test__fail++;                                       \
    printf("\033[0;31m!!\033[0;39m Test failed: "\
           "\033[0;31m%s\033[0;39m (%s)\n", name, #cond);       \
    if (test__fail_fast) {                              \
      exit(1);                                          \
    }                                                   \
  }                                                     \

#define TEST_EXIT(fail_status)                                  \
  printf("***** RESULTS: ******\n");                            \
  printf("  Tests passed: %d\n", test__pass);                   \
  printf("  Tests failed: %d\n", test__fail);                   \
  printf("===========================\n");                      \
  printf("  Tests total:  %d\n", test__pass + test__fail);       \
  if (test__fail != 0){                                         \
    exit(fail_status);                                          \
  } else {                                                      \
    exit(0);                                                    \
  }
  

static void test__init_impl(int argc, char** argv){
  dfsch_cmdopts_t* parser = dfsch_cmdopts_make_parser(0);
  dfsch_cmdopts_add_flag_set(parser, 0, "one-test-fail", 1, &test__fail_fast);
  dfsch_cmdopts_parse_argv(parser, argv+1, argc-1);
}

  
#endif
