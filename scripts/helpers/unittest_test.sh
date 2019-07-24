#!/usr/bin/env bash

source unittest.sh

f() {
  return 0
}

g() {
  return 123
}

testcase_test1() {
  true
}

testcase_test2() {
  true
  false
  true
}

testcase_test3() {
  f
}

testcase_test4() {
  g
}

testcase_test5() {
  true
  false
  f
  g
}

testcase_test__skip() {
  true
}

unittest_run
