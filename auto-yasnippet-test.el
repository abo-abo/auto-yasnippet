(require 'ert)
(require 'auto-yasnippet)

(ert-deftest aya--parse ()
  (should (equal (aya--parse "public void `Foo'TestMethod(`Type' param1, `Type' param2)")
                 '("public void "
                   ((idx . 1)
                    (value . "foo")
                    (ucase . t))
                   "TestMethod("
                   ((idx . 2)
                    (value . "type")
                    (ucase . t))
                   " param1, "
                   ((idx . 2)
                    (value . "type")
                    (ucase . t))
                   " param2)")))
  (should (equal (aya--parse "public void ~FooTestMethod(`Type' param1, `Type' param2)")
                 '("public void "
                   ((idx . 1)
                    (value . "fooTestMethod")
                    (ucase . t))
                   "("
                   ((idx . 2)
                    (value . "type")
                    (ucase . t))
                   " param1, "
                   ((idx . 2)
                    (value . "type")
                    (ucase . t))
                   " param2)"))))

(ert-deftest aya--maybe-append-newline ()
  (should (equal (let ((aya-create-with-newline t))
                   (aya--maybe-append-newline "snippet"))
                 "snippet\n"))
  (should (equal (let ((aya-create-with-newline nil))
                   (aya--maybe-append-newline "snippet"))
                 "snippet"))
  (should (equal (let ((aya-create-with-newline t))
                   (aya--maybe-append-newline "snippet\n"))
                 "snippet\n"))
  (should (equal (let ((aya-create-with-newline nil))
                   (aya--maybe-append-newline "snippet\n"))
                 "snippet\n")))

(ert-deftest test-aya--escape-snippet ()
    (let ((input "```\ncode\n```")
          (expected "\\`\\`\\`\ncode\n\\`\\`\\`"))
        (should (equal (aya--escape-snippet input) expected))))

(ert-deftest test-aya--first-char-is-upcase ()
    (let ((input "My string")
          (expected t))
        (should (equal (aya--first-char-is-upcase input) expected))))

(ert-deftest test-aya--history-index-of ()
    (let ((aya-history '("test_zero $1"
                          "test_one $1"
                          "test_two $1"
                          "test_three $1"))
          (input "test_two $1")
          (expected 2))
        (should (equal (aya--history-index-of input) expected))))

(ert-deftest test-aya--history-snippet-of ()
    (let ((aya-history '("test_zero $1"
                          "test_one $1"
                          "test_two $1"
                          "test_three $1"))
          (input 2)
          (expected "test_two $1"))
        (should (equal (aya--history-snippet-of input) expected))))

(ert-deftest test-aya--set-current ()
    (let ((aya-current "")
          (aya-history '())
          (input "new current $1"))
      (aya--set-current input)
      (should (equal aya-current input))
      (should (equal (car aya-history) input))))

(ert-deftest test-aya-next-in-history ()
    (let ((aya-history '("Zero $1" "One $1" "Two $1"))
          (aya-current "One $1"))
        (aya-next-in-history)
        (should (equal aya-current "Two $1"))))

(ert-deftest test-aya-previous-in-history ()
    (let ((aya-history '("Zero $1" "One $1" "Two $1"))
          (aya-current "One $1"))
        (aya-previous-in-history)
        (should (equal aya-current "Zero $1"))))

(ert-deftest test-aya--set-region-exit-point ()
  (let ((input "(${1:something})")
        (expected "(`(yas-selected-text)`${0:something})"))
     (should (equal (aya--set-region-exit-point input) expected)))

  (let ((input "$3($1 = ${2:something})")
        (expected "$3($1 = `(yas-selected-text)`${0:something})"))
     (should (equal (aya--set-region-exit-point input 2) expected))))

;;; auto-yasnippet-test.el ends here
