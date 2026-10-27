      *                      /* ( CWAFILE )  LENGTH=65      */          VFI00010
      *                      /*  DATE  02/08/13    V-006    */          VFI00020
           02  CM-YMD.                                                  VFI00030
               03  CM-YMD1       PIC  9(06).                            VFI00040
               03  CM-YMD2       PIC  9(06).                            VFI00050
           02  CM-SYSNO          PIC  X(02).                            VFI00060
           02  CM-FLAG.
               03  CM-STEND.
                   04  CM-START  PIC  X(01).
                   04  CM-END    PIC  X(01).
               03  CM-SSEND      PIC  X(01).
               03  CM-JSEND      PIC  X(01).
               03  CM-KYEND      PIC  X(01).
               03  CM-IJYO.
                   04  CM-IJYO1  PIC  X(01).
                   04  CM-IJYO2  PIC  X(01).
                   04  CM-YOBI   PIC  X(01).
               03  CM-MAEND      PIC  X(01).
               03  CM-YOBI2      PIC  X(01).
           02  CM-KGYMD          PIC  9(06).
           02  CM-TTOM.
               03  CM-APLID      PIC  X(08).
               03  CM-ABJ0.
                   04  CM-JMAX   PIC  9(04).
                   04  CM-JCNT   PIC  9(04).
               03  CM-ABL0.
                   04  CM-LFLG   PIC  X(01).
               03  CM-GSESP      PIC  9(02).
               03  CM-YOBI3      PIC  X(16).
