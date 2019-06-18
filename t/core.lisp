(defpackage #:github-test/core
  (:use #:cl
        #:github/core
        #:rove
        #:hamcrest/rove))
(in-package github-test/core)


(deftest test-some-staff
    (testing "Replace this test with real staff."
      (assert-that (foo 1 2)
                   (contains 1 2))))
