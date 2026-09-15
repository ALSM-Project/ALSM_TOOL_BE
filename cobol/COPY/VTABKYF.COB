      *                               /*    KAISHUU YOTEI FILE       */
      *                               /*    DATE  00/08/08   V-002   */
      *                               /*    LENGTH = 110             */
           02 KEY1.
              03 TOKCD                 PIC  X(07).
              03 NOSCD                 PIC  X(02).
              03 KSYBI.
                 04 KSYSS              PIC  9(02).
                 04 KSYYMD.
                    05 KSYYY           PIC  9(02).
                    05 KSYMM           PIC  9(02).
                    05 KSYDD           PIC  9(02).
              03 KSSBT                 PIC  X(02).
              03 TYTKJ.
                 04 TYTSS              PIC  9(02).
                 04 TYTYMD.
                    05 TYTYY           PIC  9(02).
                    05 TYTMM           PIC  9(02).
                    05 TYTDD           PIC  9(02).
              03 RECKB                 PIC  X(01).
           02 KDTKCD                   PIC  X(07).
           02 KSJK                     PIC  X(01).
           02 SITTS                    PIC  9(02).
           02 PSTKS                    PIC S9(02)V9 COMP-3.
           02 TGKKS                    PIC S9(11)   COMP-3.
           02 KYTGS                    PIC S9(11)   COMP-3.
           02 KYTGT                    PIC S9(11)   COMP-3.
           02 KYTGF                    PIC  X(01).
           02 KFKTF                    PIC  X(01).
           02 SFSKF                    PIC  X(01).
           02 BUNO                     PIC  X(03).
           02 CKUKG.
              03 KANO                  PIC  X(03).
              03 KTIKU.
                 04 KAKRI              PIC  X(01).
                 04 CHIKU              PIC  X(01).
           02 SASYMD.
              03 SASS                  PIC  9(02).
              03 SAKYMD.
                 04 SAYY               PIC  9(02).
                 04 SAMM               PIC  9(02).
                 04 SADD               PIC  9(02).
           02 HEKYMD.
              03 HKSS                  PIC  9(02).
              03 HYMD.
                 04 HKYY               PIC  9(02).
                 04 HKMM               PIC  9(02).
                 04 HKDD               PIC  9(02).
           02 SEKGK                    PIC S9(11)   COMP-3.
           02 OKAGE                    PIC S9(11)   COMP-3.
           02 KSYGK                    PIC S9(11)   COMP-3.
           02 SAGAK                    PIC S9(11)   COMP-3.
           02 HSCKB                    PIC  X(01).
