      *                                /*   TABLE FILE              */
      *                                /*         GINKOU CALENDER   */
      *                                /*   LENGTH =>  80           */
      *                                /*   DATE  91/10/09   V-001  */
           02  BCL-KEY.
               03 BCL-RECID        PIC  X(03).
               03 BCL-RENNO        PIC  9(03).
           02  BCL-CLNDA.
               03 BCL-CDSYM        PIC  9(06).
               03 BCL-KYJFLG       PIC  X(01) OCCURS 31.
           02  FILLER              PIC  X(37).
