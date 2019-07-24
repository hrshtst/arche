#!/usr/bin/env bash

# Get the path which this script exists.
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# Include unit testing framework.
source "${THIS_DIR}/unittest.sh"

# Include functions to test.
source "${THIS_DIR}/utils.sh"

testcase_has() {
  has "pwd"
  ! has "hoge"
}

unittest_run
