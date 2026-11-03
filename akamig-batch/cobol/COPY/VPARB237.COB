      *                        /*    PARM  FOR  VSBA2370      */
      *                        /*    DATE  15/06/18   V-003   */
      *                        /*    LENGTH  353508           */
           02  P237-KIJUN      PIC  9(08).
           02  P237-SOTBL.
               03  P237-TEIBL  OCCURS 5.
                   04  P237-SOSKI  OCCURS 700.
                       05  P237-SYSNO      PIC  X(02).
                       05  P237-STNKG      PIC  X(01).
                       05  P237-SOKKG      PIC  X(01).
                       05  P237-SYKKG      PIC  X(01).
                       05  P237-STNNO      PIC  X(03).
                       05  P237-SOKNO      PIC  X(03).
                       05  P237-SYKNO      PIC  X(03).
                       05  P237-STNME      PIC  X(10).
                       05  P237-SYKME      PIC  X(08).
                       05  P237-SOKME      PIC  X(08).
                       05  P237-STNMJ      PIC  X(18).
                       05  P237-SYKMJ      PIC  X(18).
                       05  P237-SOKMJ      PIC  X(18).
                       05  P237-STJCD      PIC  X(05).
                       05  P237-STNGK      PIC  X(01).
                       05  P237-KAGKB      PIC  X(01).
