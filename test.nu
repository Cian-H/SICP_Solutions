#!/usr/bin/env nu

# This script quickly tests to ensure that none of my changes to libraries break older exercises

# Load ignored paths
let testignore = open ./testignore
| from csv --noheaders
| get column0
| path expand

# Capture the results of each test into a list
let test_results = (ls exercise*/*.scm | par-each { |file|
    let fname = $file.name
    let fpath = $fname | path expand

    if ($fpath in $testignore) {
        print $"($fname):\t(ansi yb)IGNORED...(ansi reset)"
        return "ignored"
    } else {
        let results = (timeit --output { sicp-scheme $fname | complete })
        let time = $results.time
        let outputs = $results.output

        if ($outputs.exit_code == 0) {
            print $"($fname):\t(ansi gb)SUCCESS!(ansi reset) \(($time)\)"
            return "success"
        } else {
            print $"($fname):\t(ansi rb)FAIL!(ansi reset) \(($time)\) \(errcode: ($outputs.exit_code)\)"
            print -e $"(ansi pb)Trace:(ansi reset)\n($outputs.stderr)"
            print -e $"(ansi pb)Output:(ansi reset)\n($outputs.stdout)"
            return "failed"
        }
    }
})

# --- Final Summary ---
let total = ($test_results | length)
let failed_count = ($test_results | where $it == "failed" | length)
let success_count = ($test_results | where $it == "success" | length)
let ignored_count = ($test_results | where $it == "ignored" | length)

print $"\n(ansi bb)========================================(ansi reset)"
print $"Total Tests:\t($total)"
print $"Success:\t(ansi gb)($success_count)(ansi reset)"
print $"Ignored:\t(ansi yb)($ignored_count)(ansi reset)"

if $failed_count > 0 {
    print $"Failed:\t(ansi rb)($failed_count)(ansi reset)"
    print $"\n(ansi rb)FAIL!(ansi reset)"
    exit 1
} else {
    print $"Failed:\t($failed_count)"
    print $"\n(ansi gb)PASSED!(ansi reset)"
}
