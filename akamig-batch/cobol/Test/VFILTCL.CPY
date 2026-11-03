      *                      /*   ( TBLFILE )  CALENDER TABLE  */       FIL00010
      *                      /*   DATE  98/03/09   V-002       */       FIL00020
           02  TCL-KEY.                                                 FIL00030
               03  TCL-RECID   PIC  X(3).                               FIL00040
               03  TCL-RENNO   PIC  9(3).                               FIL00050
           02  TCL-CALEN.                                               FIL00060
               03  TCL-YYMM               PIC  9(6).                    FIL00070
               03  TCL-YASUM  OCCURS  31  PIC  X.                       FIL00080
           02  FILLER          PIC  X(37).                              FIL00090
