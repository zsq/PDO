
(/VERSIONCHECK 2) 

(DEFUN |MYPKG;commutate;3R;1| (|a| |b| $)
  (SPADCALL (SPADCALL |a| |b| (QREFELT $ 7)) (SPADCALL |b| |a| (QREFELT $ 7))
            (QREFELT $ 8))) 

(DEFUN |MyPackage| (#1=#:G712)
  (PROG ()
    (RETURN
     (PROG (#2=#:G713)
       (RETURN
        (COND
         ((LETT #2#
                (|lassocShiftWithFunction| (LIST (|devaluate| #1#))
                                           (HGET |$ConstructorCache|
                                                 '|MyPackage|)
                                           '|domainEqualList|)
                . #3=(|MyPackage|))
          (|CDRwithIncrement| #2#))
         ('T
          (UNWIND-PROTECT (PROG1 (|MyPackage;| #1#) (LETT #2# T . #3#))
            (COND ((NOT #2#) (HREM |$ConstructorCache| '|MyPackage|))))))))))) 

(DEFUN |MyPackage;| (|#1|)
  (PROG (|pv$| $ |dv$| DV$1)
    (RETURN
     (PROGN
      (LETT DV$1 (|devaluate| |#1|) . #1=(|MyPackage|))
      (LETT |dv$| (LIST '|MyPackage| DV$1) . #1#)
      (LETT $ (GETREFV 10) . #1#)
      (QSETREFV $ 0 |dv$|)
      (QSETREFV $ 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #1#))
      (|haddProp| |$ConstructorCache| '|MyPackage| (LIST DV$1) (CONS 1 $))
      (|stuffDomainSlots| $)
      (QSETREFV $ 6 |#1|)
      (SETF |pv$| (QREFELT $ 3))
      $)))) 

(MAKEPROP '|MyPackage| '|infovec|
          (LIST
           '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . *) (6 . -)
              |MYPKG;commutate;3R;1|)
           '#(|commutate| 12) 'NIL
           (CONS (|makeByteWordVec2| 1 'NIL)
                 (CONS '#()
                       (CONS '#()
                             (|makeByteWordVec2| 9
                                                 '(2 6 0 0 0 7 2 6 0 0 0 8 2 0
                                                   6 6 6 9)))))
           '|lookupComplete|)) 
