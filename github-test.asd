(defsystem github-test
           :author "Alexander Artemenko"
           :license "BSD"
           :class :package-inferred-system
           :pathname "t"
           :depends-on (:github
                        "github-test/core")
           :description "Test system for github"

           :perform (test-op :after (op c)
                             (symbol-call :rove :run c)
                             (clear-system c)))
