function catch_select_test_case
    if test (count $argv) = 0
        echo "test executable must be specified."
        echo "Usage:"
        echo "  catch_select_test_case ./catch_test"
        return 1
    end

    set -l catch_test $argv[1]
    set -l peco_flags --prompt "TEST CASE>" --select-1
    eval $catch_test --list-test-names-only | peco $peco_flags | read testcase
    set testcase (echo "$testcase" | sed -e 's/,/\\\,/g')

    if [ $testcase ]
        commandline "$catch_test \"$testcase\""
    else
        commandline ""
    end
end
