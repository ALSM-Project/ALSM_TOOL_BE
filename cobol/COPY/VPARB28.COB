      *                        /*    PARM  FOR  VSBA0280      */
      *                        /*    DATE  18/05/11   V-006   */
      *                        /*    LENGTH  274              */
           02  P28-TYPE.
               03  P28-TYPRD   PIC  X(1).
               03  P28-TYPKY   PIC  X(1).
               03  P28-TYPAC   PIC  X(1).
           02  P28-KEY.
               03  P28-SYOCD   PIC  X(9).
           02  P28-OPCLK       PIC  X(1).
           02  P28-RTNCD       PIC  X(1).
           02  SMS-R.
               03  P28-SM1.
                   04  SMS-KEY1.
                       05  SMS-HSYCD.
                           06  SMS-HSYCD1  PIC  X(3).
                           06  SMS-HSYCD2  PIC  X(6).
                   04  SMS-HTSCD.
                       05  SMS-HTSCD1      PIC  X(3).
                       05  SMS-HTSCD2      PIC  X(6).
                   04  SMS-SYOME.
                       05  SMS-SYOMH   PIC  X(25).
                       05  SMS-SYOMK   PIC  X(10).
                       05  SMS-SYOMY   PIC  X(10).
                   04  SMS-MAKCD       PIC  9(4).
                   04  SMS-GOJUN       PIC  X(6).
                   04  SMS-SBMMK       PIC  X(2).
                   04  SMS-HOSO        PIC  S9(6)V9(1) COMP-3.
                   04  SMS-KANSAN      PIC  S9(2)V9(1) COMP-3.
                   04  SMS-YAYMD.
                       05  SMS-YAYY    PIC  9(4).
                       05  SMS-YAMM    PIC  9(2).
                       05  SMS-YADD    PIC  9(2).
                   04  SMS-TYAKN       PIC  S9(6)V9(2) COMP-3.
                   04  SMS-TYAKO       PIC  S9(6)V9(2) COMP-3.
                   04  SMS-KCYGM       PIC  X(1).
                   04  SMS-HOUKB.
                       05  SMS-HOUK1   PIC  X(1).
                       05  SMS-HOUK2   PIC  X(1).
                       05  SMS-HOUK3   PIC  X(1).
                       05  SMS-HOUK4   PIC  X(1).
                       05  SMS-HOUK5   PIC  X(1).
                       05  SMS-HOUK6   PIC  X(1).
                       05  SMS-HOUK7   PIC  X(1).
                       05  SMS-HOUK8   PIC  X(1).
                       05  SMS-HOUK9   PIC  X(1).
                       05  SMS-HOUK10  PIC  X(1).
                   04  SMS-TORIA.
                       05  SMS-TORI1   PIC  X(1).
                       05  SMS-TORI2   PIC  X(1).
                       05  SMS-TORI3   PIC  X(1).
                       05  SMS-TORI4   PIC  X(1).
                       05  SMS-TORI5   PIC  X(1).
                       05  SMS-TORI6   PIC  X(1).
                       05  SMS-TORI7   PIC  X(1).
                       05  SMS-TORI8   PIC  X(1).
                   04  SMS-GIGYO       PIC  9(1).
                   04  SMS-SMKUH       PIC  9(1).
                   04  SMS-KKSCD.
                       05  SMS-KKCD1   PIC  X(3).
                       05  SMS-KKCD2   PIC  X(6).
                   04  SMS-SHIKR       PIC S9(8)   COMP-3.
                   04  SMS-SHIKO       PIC S9(8)   COMP-3.
                   04  SMS-KONPO       PIC S9(5)   COMP-3.
                   04  SMS-AYMD.
                       05  SMS-AYY     PIC  9(4).
                       05  SMS-AMM     PIC  9(2).
                       05  SMS-ADD     PIC  9(2).
                   04  SMS-RYMD.
                       05  SMS-RYY     PIC  9(4).
                       05  SMS-RMM     PIC  9(2).
                       05  SMS-RDD     PIC  9(2).
                   04  SMS-DYMD.
                       05  SMS-DYY     PIC  9(4).
                       05  SMS-DMM     PIC  9(2).
                       05  SMS-DDD     PIC  9(2).
                   04  SMS-SYMKK.
                       05  SMS-SYHKK   PIC  X(25).
                       05  SMS-SYKYKK  PIC  X(20).
                   04  SMS-HTYMD.
                       05  SMS-HTYY    PIC  9(4).
                       05  SMS-HTMM    PIC  9(2).
                       05  SMS-HTDD    PIC  9(2).
                   04  SMS-KSSCD       PIC  X(12).
                   04  SMS-ANYAKO      PIC  X(5).
                   04  SMS-SYAKO       PIC  X(4).
                   04  SMS-JANCD       PIC  X(13).
                   04  FILLER          PIC  X(7).
