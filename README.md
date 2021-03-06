# Nth-Size-Tree-Creator-LISP-
User inputs the size of the tree (binary, ternary, etc.) along with a list of numbers and it returns a corresponding tree.


TO RUN: 
In gcl compiler: 

> (load "lisp_besse.lisp")
> (runTree)

It will then prompt you for the size of the tree (integers >= 2).
	**Size of tree is number of children.
; grader:  mention quaternary trees?  -2

Then will ask for file name for test (include quotes, e.g. "filename").

Returns tree of given size.


**Data in file should be a list.

; grader: pretty minimal external documentation.  -1









Test 1: Basic Test

Size Tree: 4

Data Given: "quaternary.txt"
(36 535 369 837 738 110 817 240 478 483 667 436 186 323 782 256 64 977
815 126 701 888 46 350 830 689 187 833 531 776 979 147 856 949 901 189
92 981 572 307 821 397 911 776 737 899 872 224 674 752 820 541 680 496
532 461 483 642 172 316 312 1 940 957 589 217 671 114 244 109 232 912
575 259 67 86 413 722 540 140 441 791 412 823 492 54 956 572 570 801 15
974 674 529 48 563 748 523 717 10 622 278 229 625 533 880 14 298 992 200
928 210 744 101 172 592 496 472 294 224 678 249 709 438 895 152 129 999
973 354 532 976 669 965 837 585 665 140 574 111 36 701 451 319 114 869
985 49 767 682 215 839 932 645 814 730 187 597 767 856 960 654 642 828
948 667 714 275 623 187 804 535 284 452 261 473 734 982 699 567 431 867
613 858 891 355 153 450 508 881 65 147 161 850 986 401 339 167 53 145)


Quaternary Tree:

((NIL 1 NIL 10 (14) 15 (36)) 36
 ((NIL 46 (NIL 48 NIL 49 (53) 54 NIL) 64 (65 67 86) 92 (101 109)) 110
  ((111 114 114) 126 (NIL 129 NIL 140 NIL 140 (145 147)) 147
   (NIL 152 NIL 153 (161 167) 172 NIL) 172 NIL)
  186
  ((187 187) 187 NIL 189 (NIL 200 NIL 210 (215) 217 (224)) 224
   (229 232))
  240
  ((244 249) 256
   ((NIL 259 (261 275) 278 (284 294) 298 NIL) 307 NIL 312 NIL 316
    (319))
   323 (339) 350 (354 355)))
 369
 ((NIL 397 (401) 412 NIL 413 (431)) 436
  ((438) 441 (450 451 452) 461 NIL 472 (473)) 478 (483) 483
  ((492 496) 496 (508 523 529) 531 (532) 532 (533 535)))
 535
 ((((540) 541 (NIL 563 (567) 570 NIL 572 NIL) 572
    ((574) 575 (585) 589 (592 597 613) 622 (623 625 642)) 642
    (NIL 645 NIL 654 NIL 665 (667)))
   667 ((669) 671 (674) 674 (678) 680 (682)) 689 (699 701) 701
   ((709 714) 717 NIL 722 (730 734) 737 NIL))
  738
  (((744) 748 NIL 752 (767 767) 776 NIL) 776 NIL 782
   (NIL 791 NIL 801 (804) 814 NIL) 815 NIL)
  817 ((820) 821 (823 828) 830 NIL 833 (837)) 837
  (((839 850 856) 856 (858 867 869) 872 NIL 880 (881)) 888
   ((891 895 899) 901 NIL 911 (NIL 912 NIL 928 (932) 940 (948)) 949
    (NIL 956 NIL 957 (960 965 973) 974 (976)))
   977 NIL 979 (NIL 981 (982 985 986) 992 NIL 999 NIL))))



Test 2: Testing larger tree

Size Tree: 10

Data Given: (same as Test 1)

10th Tree:


((1 10 14 15 36) 36
 (NIL 46 NIL 48 (49 53) 54 NIL 64 (65) 67 NIL 86 NIL 92 NIL 101 NIL 109
      NIL)
 110
 ((111 114) 114 NIL 126 (129 140 140 145 147) 147 (152 153 161 167 172)
  172 NIL 186 (187 187) 187 NIL 189 (200 210 215) 217 (224) 224
  (229 232))
 240
 (NIL 244 (249) 256 NIL 259 (261 275) 278 (284 294 298) 307 NIL 312 NIL
      316 (319) 323 (339) 350 (354 355))
 369
 (NIL 397 (401) 412 NIL 413 (431) 436 NIL 438 NIL 441 (450) 451 (452)
      461 NIL 472 (473))
 478
 (NIL 483 NIL 483 NIL 492 (496) 496 (508) 523 NIL 529 NIL 531 (532) 532
      NIL 533 (535))
 535
 ((540) 541 (563 567 570 572) 572
  (NIL 574 NIL 575 NIL 585 NIL 589 NIL 592 NIL 597 (613) 622 (623) 625
       NIL 642 NIL)
  642 (645 654 665 667) 667 (669 671 674) 674 (678) 680 (682) 689
  (699 701) 701 (709 714 717 722 730 734) 737 NIL)
 738
 (NIL 744 NIL 748 NIL 752 (767 767) 776 NIL 776 NIL 782 NIL 791 NIL 801
      (804 814) 815 NIL)
 817 (820 821 823 828 830 833 837) 837
 ((839 850 856) 856 (858 867 869 872 880 881) 888 (891 895) 899 NIL 901
  NIL 911 (912 928 932 940 948) 949 (956 957 960 965 973 974 976) 977
  NIL 979 NIL 981 (982 985 986 992 999)))




Test 3: Testing nil

Size Tree: 4

Data Given:"nil.txt" (nil)

Tree:

NIL




Test 4: basic testing

Size Tree: 4

Data Given: "test1.txt" (1 5 9 2 3 4 6 7 8)

Tree:

(NIL 1 (2 3 4) 5 (6 7 8) 9 NIL)



Test 5: Testing same value, (if equal should go to left).

Size Tree: 4

Data Given: "test2.txt (11 27 38 11 11 11 11 11 11 11 28 294 32 12 12 12 12 12 12)

Tree:

((((11) 11 NIL 11 NIL 11 NIL) 11 NIL 11 NIL 11 NIL) 11
 ((12 12 12) 12 NIL 12 NIL 12 NIL) 27 (28 32) 38 (294))



Test 6: Testing sorting of node

Sive Tree: 50

Data Given: "test2.txt" //same as test 5

Tree:
(11 11 11 11 11 11 11 11 12 12 12 12 12 12 27 28 32 38 294)





