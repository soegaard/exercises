#lang planet dyoo/whalesong

(boolean? "t")
(boolean? #t)
(boolean? 0)
(boolean? #\t)

(char? "t")
(char? #t)
(char? 0)
(char? #\t)

(char=? #\a #\b)
(char=? #\a #\a)
(char=? #\a #\a #\b)
(char=? #\a #\b #\a)
(char=? #\a #\a #\a)
