      *                                  /* TOKUISAKI MASTER       */
      *                                  /* DATE  02/10/17   V-009 */
      *                                  /* LENGTH = 347           */
           02 KEY1.
              03 TOKCD                 PIC  X(07).
           02 TOKMK                    PIC  X(25).
           02 STNCD                    PIC  X(03).
           02 CKUKG.
              03 KANO                  PIC  X(03).
              03 KTIKU.
                 04 KAKRI              PIC  X(01).
                 04 CHIKU              PIC  X(01).
           02 SNKMK.
              03 SINKM                 PIC  X(01) OCCURS 35.
           02 SEYOS                    PIC  X(01).
           02 SEBUN                    PIC  X(01).
           02 YTFLG                    PIC  X(01).
           02 NOFLG                    PIC  X(01).
           02 SHYMD.
              03 SHYY                  PIC  9(04).
              03 SHMM                  PIC  9(02).
              03 SHDD                  PIC  9(02).
           02 TSIMEO                   PIC  X(02).
           02 BOGIE                    PIC S9(07)      COMP-3.
           02 PATRN                    PIC  X(01).
           02 BRICD                    PIC  X(05).
           02 OTLET                    PIC  X(01).
           02 MANAG                    PIC  X(02).
           02 SAIRN                    PIC  X(01).
           02 TSIME                    PIC  9(02).
           02 HNRKB                    PIC  X(01).
           02 KEY2.
              03 STKCD                 PIC  X(07).
           02 SEBUNO                   PIC  X(01).
           02 INKAN                    PIC  X(01).
           02 KEIYK                    PIC  X(01).
           02 MAKHN.
              03 MKHNK                 PIC  X(01) OCCURS 100.
           02 CHENY                    PIC  9(06).
           02 CKUKGO.
              03 KANOO                 PIC  X(03).
              03 KTIKUO.
                 04 KAKRIO             PIC  X(01).
                 04 CHIKUO             PIC  X(01).
           02 TKMEJ                    PIC  X(30).
           02 STNCDO                   PIC  X(03).
           02 BEDSU                    PIC  X(05).
           02 ZASOH                    PIC  X(01).
           02 OYAKO                    PIC  X(01).
           02 UMPRC                    PIC  X(01).
           02 UMPRJ                    PIC  X(01).
           02 GNYSS                    PIC  X(01).
           02 KANKB                    PIC  X(01).
           02 DLTKB                    PIC  X(01).
           02 LKGKB                    PIC  X(01).
           02 SZKKB                    PIC  X(01).
           02 SOKNY                    PIC S9(07)      COMP-3.
           02 GSITE1                   PIC  X(01).
           02 GSITE2                   PIC  X(01).
           02 DTOFG                    PIC  X(01).
           02 CYSHY                    PIC  X(01).
           02 YSIGK                    PIC S9(11)      COMP-3.
           02 KRGNO                    PIC  X(02).
           02 SHZKB                    PIC  X(01).
           02 HBKSJ                    PIC  X(01).
           02 SMANO                    PIC  X(05).
           02 SITET                    PIC  X(01).
           02 KJNAR.
              03 KJAJY                 PIC S9(02)V9    COMP-3.
              03 KJAKA                 PIC S9(02)V9    COMP-3.
           02 ZONE                     PIC  X(02).
           02 SDPKB                    PIC  X(01).
           02 SDPKAD.
              03 SDPSS                 PIC  9(02).
              03 SDPYMD.
                 04 SDPYY              PIC  9(02).
                 04 SDPMM              PIC  9(02).
                 04 SDPDD              PIC  9(02).
           02 MJYTKB                   PIC  X(01).
           02 TUTKB                    PIC  X(01).
           02 SZHSK                    PIC  X(01).
           02 SZTNI                    PIC  X(01).
           02 DLTKS                    PIC  X(01).
           02 LTKGS                    PIC  X(01).
           02 RTKKB                    PIC  X(01).
           02 SIMET.
              03 SIMEJ                 PIC  9(02).
              03 SIMEF                 PIC  9(02).
           02 SZHSKO                   PIC  X(01).
           02 SZHHK.
              03 SZHSS                 PIC  9(02).
              03 SZHYMD.
                 04 SZHYY              PIC  9(02).
                 04 SZHMM              PIC  9(02).
                 04 SZHDD              PIC  9(02).
           02 KEY3.
              03 SEYCD                 PIC  X(07).
           02 SHOKK                    PIC  X(01).
           02 TRNKZ                    PIC  X(02).
           02 OBSFG                    PIC  X(01).
           02 KAKSK                    PIC  X(01).
           02 SEMSK                    PIC  X(01).
           02 STDSK                    PIC  X(01).
