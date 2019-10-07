;;; Regular expressions

(match-re "([A-Z])" "Amanha")
T
"A"
"A"

(match-re "\\d" "1")
T
"1"


(match-re "(H..).(o..)" "Hello World")
T
"Hello W"
"Hel"
"o W"

(match-re "l+" "Hello World")
T
"ll"

(match-re "Foo" "FOO" :case-fold t)
T
"FOO"
