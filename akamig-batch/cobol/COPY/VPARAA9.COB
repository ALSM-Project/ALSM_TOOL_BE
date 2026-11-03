      *                        /*    PARM  FOR  VSAA1090      */
      *                        /*    DATE  91/10/02   V-001   */
      *                        /*    LENGTH   14              */
           02  CA9-YMDS8           PIC  9(8).
           02  CA9-RYMDS8    REDEFINES  CA9-YMDS8.
               03  CA9-SSS8        PIC  9(2).
               03  CA9-YMD8        PIC  9(6).
               03  CA9-RYMD8 REDEFINES  CA9-YMD8.
                   04  CA9-YYS8    PIC  9(2).
                   04  CA9-MMS8    PIC  9(2).
                   04  CA9-DDS8    PIC  9(2).
           02  CA9-YMDW6           PIC  9(6).
           02  CA9-RYMDW6    REDEFINES  CA9-YMDW6.
               03  CA9-YYW6        PIC  9(2).
               03  CA9-MMW6        PIC  9(2).
               03  CA9-DDW6        PIC  9(2).
