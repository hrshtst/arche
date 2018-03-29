function catch_select_test_case
    if test (count $argv) = 0
        echo "test executable should be specified."
        echo "Usage:"
        echo "  catch_select_test_case ./catch_test"
        return 1
    end

    set -l catch_test $argv[1]
    # set -l peco_flags --prompt "TEST CASE>" --select-1
    set -l peco_flags --prompt "TEST CASE>"
    # eval $catch_test --list-test-names-only | peco $peco_flags | read -a testcases
    # set -l testcases (eval $catch_test --list-test-names-only | peco $peco_flags)
    eval $catch_test --list-test-names-only | peco $peco_flags > hoge
    set -l testcases ''
    while read -la line
        echo "line: \"$line\""
        set testcases "$testcases" "\"$line\""
        # set -l testcases $testcases"\"$line\" "
    end < hoge

    echo "testcases = " $testcases
    if test (count $testcase) -gt 0
        set -l arg ''
        for tc in $testcases
            echo "test case: $tc"
            # set arg "$arg \"$tc\""
        end
        # echo $arg
        # commandline "$catch_test \"$testcase\""
    else
        commandline ''
    end
end
