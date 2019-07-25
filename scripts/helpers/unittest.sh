#!/usr/bin/env bash

# unittest.sh
#
# This library provides a simple unit testing framework for bash shell
# script. After sourcing this library at the head of a script in which
# test cases are defined, call a function 'unittest_run' to test all
# the test cases. Functions whose names start with 'testcase_' are
# automatically recognized and handled as test cases. A test case can
# not be passed unless all the commands called inside the test case
# exits with a non-zero status.
#
# The following script is an example usage of this library.
# --
# #!/bin/bash
#
# source unittest.sh
#
# testcase_test1() {
#   true
# }
#
# testcase_test2() {
#   false
# }
#
# unittest_run
# --
#
# Executing the script, we will get the following output.
# --
# pass: testcase_test1
# fail: testcase_test2
#
# 2 tests: 1 passed, 1 failed.

# Avoid double inclusion of this script.
set +u
if [[ -n "$__UNITTEST_SH_INCLUDED__" ]]; then
  set -u
  return
fi
__UNITTEST_SH_INCLUDED__=yes

# Treat unset variable as an error.
set -u

# Define faces and colors for reporting results.
reset=$(tput sgr0)
red=$(tput setaf 1)
green=$(tput setaf 76)

# Store the name of test script.
__unittest_test_script="${BASH_SOURCE[1]}"

# Store the working directory that each test will run.
__unittest_working_directory="$(pwd)"

# Flag to keep whether a test is passed or failed. Before running each
# test, this flag should be set to false. If the test failed, this
# flag should be set to true.
__unittest_failed=false

# Store source script that an error occured as a stack (an array).
# This variable should be cleared before running a test.
__unittest_source_stack=()

# Store line numbers that an error occurred as a stack (an array).
# This variable should be cleared before running a test.
__unittest_lineno_stack=()

# Store exit status when an error occurred as a stack (an array).
# This variable should be cleared before running a test.
__unittest_status_stack=()

# Prepare stuff before running each test.
#
# @param $1  Test case name.
__unittest_before_running_hook() {
  local testcase

  testcase="$1"
  __unittest_failed=false
  __unittest_source_stack=()
  __unittest_lineno_stack=()
  __unittest_status_stack=()

  # Chenge to the default working directory.
  cd "$__unittest_working_directory" || exit 1
}

# Enable a __unittest_failed flag if an error is detected. This function will
# be registered as a handler to be run when the shell receives ERR
# signal.
__unittest_failed_hook() {
  # Store the exit status of the last command.
  local status="$?"

  # Only if ERR signal was sent from the test script, the test
  # interpretted as fail.
  if [[ "${BASH_SOURCE[1]}" = "$__unittest_test_script" ]]; then
    # Store the location and status that an error occurred.
    __unittest_failed=true
    __unittest_source_stack+=("${BASH_SOURCE[1]}")
    __unittest_lineno_stack+=("${BASH_LINENO[0]}")
    __unittest_status_stack+=("$status")
    return
  fi
}

# Process stuff after running each test. Here, the name of test case
# is categorized into passed tests or failed test. If the test case is
# passed, its name is stored in a variable '__unittest_passed_tests'
# as an array, otherwise it is stored in '__unittest_failed_tests'.
#
# @param $1  Test case name.
__unittest_passed_tests=()
__unittest_failed_tests=()
__unittest_after_running_hook() {
  local testcase

  testcase="$1"
  if [[ $__unittest_failed = false ]]; then
    __unittest_passed_tests+=("$testcase")
  else
    __unittest_failed_tests+=("$testcase")
  fi
}

# Print a short result for a passed test.
#
# @param $1  Test case name.
__unittest_print_result_pass() {
  local testcase

  testcase="$1"
  printf "%spass%s: $testcase\n" "$green" "$reset"
}

# Print a short result for a failed test.
#
# @param $1  Test case name.
__unittest_print_result_fail() {
  local testcase

  testcase="$1"
  printf "%sfail%s: $testcase\n" "$red" "$reset"
  for i in "${!__unittest_status_stack[@]}"; do
    printf "  Exit with status %d at %s:%d\n" \
           "${__unittest_status_stack[$i]}" \
           "${__unittest_source_stack[$i]}" \
           "${__unittest_lineno_stack[$i]}"
  done
}

# Print a short result after running each test.
#
# @param $1  Test case name.
__unittest_print_result() {
  local testcase

  testcase="$1"
  if [[ $__unittest_failed = false ]]; then
    __unittest_print_result_pass "$testcase"
  else
    __unittest_print_result_fail "$testcase"
  fi
}

# Define ERR trap to detect if a test case failed.
trap '__unittest_failed_hook' ERR

# Allow ERR trap to catch the error (exit with a non-zero status)
# inside a function. set -E is not working because it results in
# exiting immediately when an error occurs.
set -o errtrace

# Prepare stuff before running all test cases.
unittest_prepare() {
  # Set the current directory as the working directory in which each
  # test will run.
  __unittest_working_directory="$(pwd)"
}

# Find all the test cases defined in a script. Function names of test
# cases to be executed are stored in a variable '__unittest_tests'
# as an array. Skipped test cases, whose name ends with '__skip' such
# as 'testcase_something__skip', are stored in a variable
# '__unittest_skipped_tests' as an array.
__unittest_tests=()
__unittest_skipped_tests=()
unittest_find_tests() {
  local regex_tests regex_skipped_tests
  regex_tests="^testcase_.*"
  regex_skipped_tests=".*__skip$"

  while IFS= read -r func; do
    if [[ $func =~ $regex_skipped_tests ]]; then
      __unittest_skipped_tests+=("$func")
    else
      __unittest_tests+=("$func")
    fi
  done < <(declare -F | cut -d ' ' -f 3 | grep -e "$regex_tests")
}

# Run found test cases one by one and print the result of each test
# case. If a test case is passed, the name of the test case is stored
# in a variable '__unittest_passed_tests' as an array, otherwise the
# name is stored in '__unittest_failed_tests'.
unittest_run_tests() {
  local testcase

  for testcase in "${__unittest_tests[@]}"; do
    __unittest_before_running_hook "$testcase"
    $testcase
    __unittest_after_running_hook "$testcase"
    __unittest_print_result "$testcase"
  done
}

# Print summary.
unittest_print_summary() {
  printf "\n%d tests found: %d passed, %d failed, %d skipped\n" \
         "$(("${#__unittest_tests[@]}" + "${#__unittest_skipped_tests[@]}"))" \
         "${#__unittest_passed_tests[@]}" \
         "${#__unittest_failed_tests[@]}" \
         "${#__unittest_skipped_tests[@]}"
}

# Test runner.
unittest_run() {
  unittest_prepare
  unittest_find_tests
  unittest_run_tests
  unittest_print_summary
}
