      *                                /*   HANBAI SEIKU HASSEI     */
      *                                /*   LENGTH =>  328          */
      *                                /*   DATE  15/07/28   V-003  */
           02 HSH-SITEK                     PIC  X(01).
           02 HSH-SOKOK                     PIC  X(01).
           02 HSH-DNPNO.
              03 HSH-DNSYK                  PIC  X(01).
              03 HSH-DNREN                  PIC  9(06).
           02 HSH-TKSAI                     PIC  X(01).
           02 HSH-TORIK.
              03 HSH-TURIK                  PIC  X(01).
              03 HSH-THAIK                  PIC  X(01).
           02 HSH-CKUKG.
              03 HSH-CKUKA                  PIC  X(03).
              03 HSH-CKUCK                  PIC  X(02).
           02 HSH-CKUR1 REDEFINES HSH-CKUKG.
              03 HSH-CKR11                  PIC  X(04).
              03 FILLER                     PIC  X(01).
           02 HSH-CKUR2 REDEFINES HSH-CKUKG.
              03 FILLER                     PIC  X(03).
              03 HSH-CKR22                  PIC  X(02).
           02 HSH-TOKCD.
              03 HSH-TKCD1                  PIC  X(04).
              03 HSH-TKCD2                  PIC  X(03).
           02 HSH-HAYMD.
              03 HSH-HAYY                   PIC  9(04).
              03 HSH-HAMM                   PIC  9(02).
              03 HSH-HADD                   PIC  9(02).
           02 HSH-SZSYM.
              03 HSH-SZYY                   PIC  9(04).
              03 HSH-SZMM                   PIC  9(02).
           02 HSH-TKSYM.
              03 HSH-TKYY                   PIC  9(04).
              03 HSH-TKMM                   PIC  9(02).
           02 HSH-HNPNK                     PIC  X(01).
           02 HSH-LINNO                     PIC  9(01).
           02 HSH-SYOCD.
              03 HSH-SYCD1                  PIC  X(03).
              03 HSH-SYCD2                  PIC  X(06).
           02 HSH-SURYO                     PIC S9(05) COMP-3.
           02 HSH-TANKA                     PIC S9(08) COMP-3.
           02 HSH-GENKA                     PIC S9(08) COMP-3.
           02 HSH-FSZFG                     PIC  X(01).
           02 HSH-LDNPN                     PIC  X(07).
           02 HSH-LDNPL                     PIC  9(01).
           02 HSH-LDNPD.
              03 HSH-LDNYY                  PIC  9(04).
              03 HSH-LDNMM                  PIC  9(02).
              03 HSH-LDNDD                  PIC  9(02).
           02 HSH-LDNPT                     PIC  X(02).
           02 HSH-SNEBK                     PIC S9(08) COMP-3.
           02 HSH-ARRITX.
              03  HSH-ARRIT   PIC S9(02)V9 COMP-3.
           02 HSH-MKHAN                     PIC  X(01).
           02 HSH-NTANK                     PIC S9(08) COMP-3.
           02 HSH-HDNPN                     PIC  X(07).
           02 HSH-HDNPL                     PIC  9(01).
           02 HSH-HDNPD.
              03 HSH-HDNYY                  PIC  9(04).
              03 HSH-HDNMM                  PIC  9(02).
              03 HSH-HDNDD                  PIC  9(02).
           02 HSH-HDNPT                     PIC  X(02).
           02 HSH-ZDNPN                     PIC  X(07).
           02 HSH-ZDNPL                     PIC  9(01).
           02 HSH-ZDNPD                     PIC  9(08).
           02 HSH-ZDNPT                     PIC  X(02).
           02 HSH-KETKB                     PIC  X(01).
           02 HSH-URIDA                     PIC  9(08).
           02 HSH-ZHYOS                     PIC  X(01).
           02 HSH-HKHYO                     PIC  X(01).
           02 HSH-GIGYD.
              03 HSH-GIYY                   PIC  9(04).
              03 HSH-GIMM                   PIC  9(02).
              03 HSH-GIDD                   PIC  9(02).
           02 HSH-TOKME                     PIC  X(25).
           02 HSH-HANKB                     PIC  X(01).
           02 HSH-SBMKG                     PIC  X(02).
           02 HSH-SYOME.
              03 HSH-SYOMH                  PIC  X(25).
              03 HSH-SYOMK                  PIC  X(10).
              03 HSH-SYOMY                  PIC  X(10).
           02 HSH-GOJUJ                     PIC  X(06).
           02 HSH-SHIMC                     PIC  9(04).
           02 HSH-SHIMM                     PIC  X(08).
           02 HSH-HJYOT                     PIC  X(01).
           02 HSH-JMKKB                     PIC  X(01).
           02 HSH-MKSDD                     PIC  9(02).
           02 HSH-GIGYK                     PIC  X(01).
           02 HSH-MAKCD                     PIC  9(04).
           02 HSH-SHICD                     PIC  9(04).
           02 HSH-TSIME                     PIC  9(02).
           02 HSH-SEKMM                     PIC  9(02).
           02 HSH-KEIYK                     PIC  X(01).
           02 HSH-MIKKB                     PIC  X(01).
           02 HSH-TSYCD.
              03 HSH-TSCD1                  PIC  X(03).
              03 HSH-TSCD2                  PIC  X(06).
           02 HSH-LEASE                     PIC  X(01).
           02 HSH-SYOKB                     PIC  X(01).
           02 HSH-NSKBN                     PIC  X(02).
           02 HSH-TSFKB                     PIC  X(01).
           02 HSH-THKBN                     PIC  X(01).
           02 HSH-ADNPN                     PIC  X(07).
           02 HSH-ADNPL                     PIC  9(01).
           02 HSH-ADNPD.
              03 HSH-ADNYY                  PIC  9(04).
              03 HSH-ADNMM                  PIC  9(02).
              03 HSH-ADNDD                  PIC  9(02).
           02 HSH-ADNPT                     PIC  X(02).
           02 HSH-ZEIRT                     PIC S9(02)V9(01) COMP-3.
           02 HSH-SHZEI                     PIC S9(09)  COMP-3.
           02 HSH-SYSKG                     PIC  X(01).
           02 HSH-JUIKB                     PIC  X(01).
           02 HSH-SYBKG                     PIC  X(01).
           02 HSH-SYNGK                     PIC S9(08)  COMP-3.
           02 HSH-SHZSK                     PIC  X(01).
           02 FILLER                        PIC  X(04).
           02 HSH-ZNGHK                     PIC  X(01).
           02 HSH-DNSYK2                    PIC  X(01).
           02 FILLER                        PIC  X(08).
