./ ADD NAME=VBKA1560  01.31 20150528 20190909 165402 00931 00657 MCSK009
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1560'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN    SYSTEM             *
      *              <> SUBSYSTEM      : 20SIME SEIKYUSYO              *
      *              <> PG.OUTLINE     : SEIKYU SYO UTIWAKE DATA TO    *
      *                                  MEISAI DATA NO GOUSEI         *
      *                                                                *
      *                 * INPUT FILE   : SEIKYU SYO UTIWAKE DATA       *
      *                                                        (DWKA)  *
      *                 * INPUT FILE   : SEIKYU SYO MEISAI GYO DATA    *
      *                                                        (DWKD)  *
      *                                : KOKYAKU MASTER        (SI.KMS)*
      *                 * SYSIN        : SIMEBI                        *
      *                 * OUTPUT FILE  : SEIKYU SYO PAGE DATA  (DWKB)  *
      *                 * OUTPUT FILE  : ITIRANSEI SEIKYU MEISAI DATA  *
      *                                                        (DWKC)  *
      *                 * OUTPUT FILE  : HONKYSA YOU URIKAKE IJOU      *
      *                                  HASSEI FILE (TUIKA MODE)      *
      *                                                     (HUIFILE)  *
      *                                                                *
      *              <> AUTHOR         : SUZUKEN-M.KITAGAWA            *
      *              <> DATE           : 86/08/22                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) FILE LENGTH        ÍÝº³                                   *
      *      HENSYUU LOGIC      ÍÝº³   : 86/11/12  CSK-H.NARUSE        *
      * (01) SYOHIZEI TUIKA     Â²¶    : 89/03/14  CSK-S.MUNEISHI      *
      * (02) SYOHIZEI TUIKA     ÍÝº³   : 92/06/26  SUZ-M.YOSHIMURA     *
      * (03) YOUKEN HENKOU K-043S      : 93/11/26  SUZ-T.OKUDA         *
      * (04) YOUKEN HENKOU K-062       : 94/05/24  SUZ-Y.GOTOH         *
      * (05) K-122              ÍÝº³   : 95/06/05  CSK-H.MIYATA        *
      * (06) K-275C             ÍÝº³   : 98/04/28  CSK-R.TAKAYANAGI    *
      * (07) K-418              ÍÝº³   : 99/09/20  CSK-K.ITO           *
      * (08) K-407              ÍÝº³   : 99/12/16  CSK-Y.DOYAMA        *
      * (09) K-557              ÍÝº³   : 02/09/19  CSK-M.YOSHIDA       *
      * (10) K-568              ÍÝº³   : 03/07/18  CSK-M.YOSHIDA       *
      * (11) K-568              ÍÝº³   : 03/10/24  CSK-K.MURAYAMA      *
      * (12) K-1228             ÍÝº³   : 10/06/23  CSK-M.KODAMA        *
      * (13) K-1549             ÍÝº³   : 14/02/12  CSK-N.EGUCHI        *
      * (14) K-1553             ÍÝº³   : 15/01/16  CSK-K.KOYAMA        *
      * (15) K-1700             ÍÝº³   : 15/05/28  CSK-Y.YOSHIGA       *
      * (16) K-1780             ÍÝº³   : 15/07/31  CSK-Y.YOSHIGA       *
      * (17) K-1716             ÍÝº³   : 15/09/16  CSK-N.EGUCHI        *
      * (18) H-010  O-10054     ÍÝº³   : 17/02/09  CSK-N.EGUCHI        *
      * (19) H-010  O-10315     ÍÝº³   : 17/11/27  CSK-T.TAKADA        *
      * (20) K-2163             ÍÝº³   : 19/06/26  CSK-M.MATSUSHIMA    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT HUIFILE ASSIGN    TO AS-HUIFILE
                          ORGANIZATION SEQUENTIAL
                          FILE  STATUS S-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(80).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(250).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(20).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(237).
       FD  HUIFILE
           LABEL RECORD STANDARD.
       01  HUI-REC.
           02 FILLER                        PIC  X(260).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLKMS END-EXEC.
       01  KMS REDEFINES DCLKMS.
           COPY VTABKMS SUPPRESS.
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       77  S-CODE                           PIC  X(02).
       01  HUI-R.
           COPY VFILHUI SUPPRESS.
       01  P48-PARM.
           COPY VPARB48  SUPPRESS.
       01  PC0-PARM.
           COPY VPARBC0  SUPPRESS.
       01  P205-PARM.
           COPY VPARB205 SUPPRESS.
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  WKD-R.
           02 WKD-STNCD                     PIC  X(03).
           02 WKD-SKUCD.
              03 WKD-SKCD1                  PIC  X(04).
              03 WKD-SKCD2                  PIC  X(03).
           02 WKD-SNSCD                     PIC  X(02).
           02 WKD-TOKCD.
              03 WKD-TKCD1                  PIC  X(04).
              03 WKD-TKCD2                  PIC  X(03).
           02 WKD-NOSCD                     PIC  X(02).
           02 WKD-DATAB.
              03 WKD-RCKBN                  PIC  X(02).
              03 WKD-DATA1.
                 04 WKD-DEHI1.
                    05 WKD-DEYY1            PIC  9(04).
                    05 WKD-DEMM1            PIC  9(02).
                    05 WKD-DEDD1            PIC  9(02).
                 04 WKD-DENO1.
                    05 WKD-DENO11           PIC  X(01).
                    05 WKD-DENO12           PIC  X(06).
                 04 WKD-GYONO               PIC  9(01).
                 04 WKD-TRKBN               PIC  X(02).
                 04 WKD-SYOCD               PIC  X(09).
                 04 WKD-SYNAM.
                    05 WKD-SHINM            PIC  X(50).
                    05 WKD-SKIYO            PIC  X(40).
                 04 WKD-SURYO               PIC S9(05)  COMP-3.
                 04 WKD-TANKA               PIC S9(08)  COMP-3.
                 04 WKD-KINGK               PIC S9(09)  COMP-3.
                 04 WKD-DENKG               PIC S9(11)  COMP-3.
                 04 WKD-DEHI2.
                    05 WKD-DEYY2            PIC  9(04).
                    05 WKD-DEMM2            PIC  9(02).
                    05 WKD-DEDD2            PIC  9(02).
                 04 WKD-DENO2.
                    05 WKD-DENO21           PIC  X(01).
                    05 WKD-DENO22           PIC  X(06).
                 04 WKD-THKBN               PIC  X(01).
                 04 WKD-BUKBN               PIC  X(01).
                 04 WKD-TRKKJ               PIC  X(08).
                 04 WKD-SYOHM.
                    05 WKD-SYOMH            PIC  X(25).
                    05 WKD-SYOMK            PIC  X(20).
                 04 WKD-SHZEI               PIC S9(11)  COMP-3.
                 04 WKD-ZEGAK               PIC S9(09)  COMP-3.
                 04 WKD-DNGYO               PIC  9(01).
                 04 WKD-ZEIRT               PIC S9(02)V9 COMP-3.
                 04 WKD-DSHZFG              PIC  X(01).
                 04 WKD-KGZKB               PIC  X(01).
                 04 FILLER                  PIC  X(05).
              03 WKD-DATA2     REDEFINES    WKD-DATA1.
                 04 WKD-MIDAA               PIC  X(50).
                 04 WKD-MIDAB               PIC  X(50).
                 04 WKD-TOKTT               PIC S9(09)  COMP-3.
                 04 WKD-SHZTT               PIC S9(09)  COMP-3.
                 04 FILLER                  PIC  X(117).
              03 WKD-DATA3     REDEFINES    WKD-DATA1.
                 04 WKD-MIDAT               PIC  X(50).
                 04 WKD-MIDBT               PIC  X(50).
                 04 WKD-ZRKGT               PIC S9(11)  COMP-3.
                 04 WKD-ZRSZT               PIC S9(11)  COMP-3.
                 04 WKD-ZRITT               PIC S9(02)V9 COMP-3.
                 04 WKD-KGZKT               PIC  X(01).
                 04 FILLER                  PIC  X(112).
       01  WKA-R.
           02 WKA-SKUCD.
              03 WKA-SKCD1                  PIC  X(04).
              03 WKA-SKCD2                  PIC  X(03).
           02 WKA-SNSCD                     PIC  X(02).
           02 WKA-UDATA                     PIC  X(69).
           02 FILLER                        PIC  X(02).
       01  WKC-R.
           02 WKC-SKUCD.
              03 WKC-SKCD1                  PIC  X(04).
              03 WKC-SKCD2                  PIC  X(03).
           02 WKC-SNSCD                     PIC  X(02).
           02 WKC-PAGNO.
              03 WKC-PAGE1                  PIC  9(04).
              03 WKC-PAGE2                  PIC  9(04).
           02 WKC-GYONO                     PIC  X(02).
           02 WKC-RCKBN                     PIC  X(02).
           02 WKC-DATA1.
              03 WKC-UDATA                  PIC  X(69).
              03 WKC-SHZSK                  PIC  X(01).
              03 FILLER                     PIC  X(146).
           02 WKC-DATA2     REDEFINES       WKC-DATA1.
              03 WKC-DEHI1.
                 04 WKC-DEMM1               PIC  9(02).
                 04 WKC-DEDD1               PIC  9(02).
              03 WKC-DENO1                  PIC  X(07).
              03 WKC-TRKKJ                  PIC  X(08).
              03 WKC-DENKG                  PIC S9(09)  COMP-3.
              03 WKC-SYNAM.
                 04 WKC-SHINM               PIC  X(50).
                 04 WKC-SKIYO               PIC  X(40).
              03 WKC-SURYO                  PIC S9(05)  COMP-3.
              03 WKC-TANKA                  PIC S9(08)  COMP-3.
              03 WKC-KINGK                  PIC S9(09)  COMP-3.
              03 WKC-MESHZ                  PIC S9(09)  COMP-3.
              03 WKC-SYOCD                  PIC  X(09).
              03 WKC-SYOHM.
                 04 WKC-SYOMH               PIC  X(25).
                 04 WKC-SYOMK               PIC  X(20).
              03 WKC-BIKOU.
                 04 WKC-DEHI2.
                    05 WKC-DEMM2            PIC  9(02).
                    05 WKC-DEDD2            PIC  9(02).
                 04 WKC-DENO2               PIC  X(07).
                 04 WKC-THKBN               PIC  X(01).
                 04 WKC-BUKBN               PIC  X(01).
              03 WKC-SHZEI                  PIC S9(09)  COMP-3.
              03 WKC-DSHZFG                 PIC  X(01).
              03 WKC-MSHZUM                 PIC  X(01).
              03 WKC-MSZRT                  PIC S9(02)V9 COMP-3.
              03 WKC-DPGK60                 PIC S9(09)  COMP-3.
              03 WKC-KGZH60                 PIC  X(02).
              03 FILLER                     PIC  X(01).
           02 WKC-DATA3     REDEFINES       WKC-DATA1.
              03 WKC-TOKCD.
                 04 WKC-TKCD1               PIC  X(04).
                 04 WKC-TKCD2               PIC  X(03).
              03 WKC-NOSCD                  PIC  X(02).
              03 WKC-MIDAA                  PIC  X(50).
              03 WKC-MIDAB                  PIC  X(50).
              03 WKC-TOKTT                  PIC S9(09)  COMP-3.
              03 WKC-SHZNT                  PIC S9(09)  COMP-3.
              03 WKC-CODEH                  PIC  X(14).
              03 WKC-TNGKK                  PIC S9(09)  COMP-3.
              03 FILLER                     PIC  X(78).
           02 WKC-DATA4     REDEFINES       WKC-DATA1.
              03 WKC-DEHI3.
                 04 WKC-DEMM3               PIC  9(02).
                 04 WKC-DEDD3               PIC  9(02).
              03 WKC-MID2A                  PIC  X(50).
              03 WKC-MID2B                  PIC  X(50).
              03 WKC-DEKIN                  PIC S9(11)  COMP-3.
              03 WKC-SHZKT                  PIC S9(11)  COMP-3.
              03 WKC-DPGK6X                 PIC S9(11)  COMP-3.
              03 WKC-ZERT6X                 PIC S9(02)V9 COMP-3.
              03 WKC-KGZH6X                 PIC  X(02).
              03 FILLER                     PIC  X(90).
           02 WKC-DATA5     REDEFINES       WKC-DATA1.
              03 WKC-MIDAU6                 PIC  X(50).
              03 WKC-MIDAD6                 PIC  X(50).
              03 WKC-ZOKDK                  PIC S9(11)   COMP-3.
              03 WKC-ZSHZI                  PIC S9(11)   COMP-3.
              03 WKC-ZOKGDK                 PIC S9(11)   COMP-3.
              03 WKC-ZEIRT6                 PIC S9(02)V9 COMP-3.
              03 WKC-KGZRT6                 PIC  X(01).
              03 FILLER                     PIC  X(95).
       01  WKB-R.
           02 WKB-SKUCD.
              03 WKB-SKCD1                  PIC  X(04).
              03 WKB-SKCD2                  PIC  X(03).
           02 WKB-SNSCD                     PIC  X(02).
           02 WKB-PAGE1                     PIC  9(04).
           02 WKB-MSHZUM                    PIC  X(01).
           02 FILLER                        PIC  X(06).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-SKUCD              PIC  X(07).
                 04 WIN1-SNSCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-SKUCD              PIC  X(07).
                 04 WIN2-SNSCD              PIC  X(02).
              03 WIN2-KEY2.
                 04 WIN2-DENNO              PIC  X(07).
                 04 WIN2-TRKBN              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-SKUCD                 PIC  X(07).
              03 WSV1-SNSCD                 PIC  X(02).
           02 WSV2-KEY1.
              03 WSV2-SKUCD                 PIC  X(07).
              03 WSV2-SNSCD                 PIC  X(02).
           02 WSV2-KEY2.
              03 WSV2-DENNO                 PIC  X(07).
              03 WSV2-TRKBN                 PIC  X(02).
           02 WSV3-KEY1.
              03 WSV3-SKUCD                 PIC  X(07).
              03 WSV3-SNSCD                 PIC  X(02).
           02 WSV3-GYO                      PIC  9(02).
      *       <¢‰¿ŠiŒˆ’è‘ŠŽE‡ŒviÅ—¦            >
           02 WSV4-MID631.
              03 FILLER                     PIC  X(20) VALUE
                 X'4040475A47DE47A445E7467849E945A045C9424D'.
              03 FILLER                     PIC  X(04) VALUE
                 X'47C44783'.
      *       <¢‰¿ŠiŒˆ’è‘ŠŽE‡ŒviŒyŒ¸Å—¦        >
           02 WSV4-MID632.
              03 FILLER                     PIC  X(20) VALUE
                 X'4040475A47DE47A445E7467849E945A045C9424D'.
              03 FILLER                     PIC  X(08) VALUE
                 X'48A6478A47C44783'.
      *       <¢‰¿ŠiŒˆ’è‘ŠŽE‡Œvi‹ŒÅ—¦          >
           02 WSV4-MID633.
              03 FILLER                     PIC  X(20) VALUE
                 X'4040475A47DE47A445E7467849E945A045C9424D'.
              03 FILLER                     PIC  X(06) VALUE
                 X'4C7B47C44783'.
      *       <¢‰¿ŠiŒˆ’è‘ŠŽE‡Œvi”ñ‰ÛÅ‘ÎÛj    >
           02 WSV4-MID634.
              03 FILLER                     PIC  X(20) VALUE
                 X'4040475A47DE47A445E7467849E945A045C9424D'.
              03 FILLER                     PIC  X(12) VALUE
                 X'48E149D647C446564994425D'.
      *       <¢‰¿ŠiŒˆ’è”­¶‡ŒviÅ—¦            >
           02 WSV4-MID641.
              03 FILLER                     PIC  X(20) VALUE
                 X'4040475A47DE47A445E7464B459945A045C9424D'.
              03 FILLER                     PIC  X(04) VALUE
                 X'47C44783'.
      *       <¢‰¿ŠiŒˆ’è”­¶‡ŒviŒyŒ¸Å—¦        >
           02 WSV4-MID642.
              03 FILLER                     PIC  X(20) VALUE
                 X'4040475A47DE47A445E7464B459945A045C9424D'.
              03 FILLER                     PIC  X(08) VALUE
                 X'48A6478A47C44783'.
      *       <¢‰¿ŠiŒˆ’è”­¶‡Œvi‹ŒÅ—¦          >
           02 WSV4-MID643.
              03 FILLER                     PIC  X(20) VALUE
                 X'4040475A47DE47A445E7464B459945A045C9424D'.
              03 FILLER                     PIC  X(06) VALUE
                 X'4C7B47C44783'.
      *       <¢‰¿ŠiŒˆ’è”­¶‡Œvi”ñ‰ÛÅ‘ÎÛj    >
           02 WSV4-MID644.
              03 FILLER                     PIC  X(20) VALUE
                 X'4040475A47DE47A445E7464B459945A045C9424D'.
              03 FILLER                     PIC  X(12) VALUE
                 X'48E149D647C446564994425D'.
           02 WSV5-KEY.
              03 WSV5-SKUCD                 PIC  X(07).
           02 MSV6-ASTER                    PIC  X(02) VALUE X'446B'.
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
           02 WED1-DATE.
              03 WED1-YY                    PIC  9(04).
              03 WED1-MM                    PIC  9(02).
              03 WED1-DD                    PIC  9(02).
           02 WED1-ZEIRT                    PIC  99.9.
           02 WED1-ZEIRTX REDEFINES WED1-ZEIRT.
              03 WED1-ZEI1                  PIC  X(01).
              03 WED1-ZEI2                  PIC  X(01).
              03 FILLER                     PIC  X(01).
              03 WED1-ZEI3                  PIC  X(01).
           02 WED1-DENO.
              03 WED1-DENOH1                PIC  X(01).
              03 WED1-DENOH2                PIC  X(06).
           02 WED1-RCKBN                    PIC  X(02).
           02 WED1-DSHZFG                   PIC  X(01).
           02 WED1-PAGE                     PIC  ZZ,ZZ9.
           02 WED1-ANSER                    PIC  X(01).
      *       <¢`¢Åž¢>
           02 WED1-MIDAD6.
              03 FILLER                     PIC  X(44) VALUE SPACE.
              03 FILLER                     PIC  X(04)
                                            VALUE X'47C44841'.
              03 FILLER                     PIC  X(02) VALUE SPACE.
           02 WED1-TNGKK                    PIC S9(11) COMP-3.
       01  COUNT-AREA.
           02 WCT1-COUNT.
              03 WCT1-GYO                   PIC  9(02).
              03 WCT1-PAGE                  PIC  9(05).
       01  FLAG-AREA.
           02 WFG1-OVER                     PIC  X(01).
       01  SYSIN-AREA.
           02 SYS-CARD1.
              03 SYS-SIMEB                  PIC  9(02).
              03 FILLER                     PIC  X(78).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE AND
                          WIN2-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M13-LOCK-DB THRU M13-EXIT.
           OPEN INPUT  WKAFILE
                       WKDFILE
                OUTPUT WKCFILE
                       WKBFILE
                EXTEND HUIFILE.
           IF S-CODE = '35'
              OPEN OUTPUT HUIFILE.
           CALL 'VSBA2280' USING P228-PARM.
           PERFORM M10-SYSIN1   THRU M10-EXIT.
           PERFORM M11-READ-WKA THRU M11-EXIT.
           PERFORM M12-READ-WKD THRU M12-EXIT.
           MOVE LOW-VALUE   TO WSV5-KEY.
           MOVE ZERO        TO WFG1-OVER.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : SYSIN READ                                           *
      *----------------------------------------------------------------*
       M10-SYSIN1.
           MOVE LOW-VALUE   TO SYS-CARD1.
           ACCEPT SYS-CARD1 FROM SYSIN.
           IF SYS-CARD1 = LOW-VALUE
              CALL 'VSBABEND'.
           IF SYS-SIMEB NOT = '20' AND '31' AND '15'
              CALL 'VSBABEND'.
       M10-L01.
           MOVE P228-DATEX        TO WED1-DATE.
           IF SYS-SIMEB = '31'
              IF P228-DD NOT < 25
                 MOVE P228-DATEX  TO PC0-RYMDS8
              ELSE
                 IF P228-MM = 1
                    COMPUTE WED1-YY = WED1-YY - 1
                    MOVE 12       TO WED1-MM
                 ELSE
                    COMPUTE WED1-MM = WED1-MM - 1
                 END-IF
                 MOVE WED1-DATE   TO PC0-RYMDS8
              END-IF
              CALL 'VSBA1200' USING PC0-PARM
              MOVE PC0-RYMDS8     TO WED1-DATE
           ELSE
              MOVE SYS-SIMEB      TO WED1-DD.
           MOVE WED1-DATE         TO P205-YMDS8-X.
           MOVE '0'               TO P205-KGZKB.
           CALL 'VSBA2050' USING P205-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKAFILE (¾²·Õ³¼Ö ³ÁÜ¹ ÃÞ-À)                     *
      *----------------------------------------------------------------*
       M11-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE WKA-SKUCD TO WIN1-SKUCD.
           MOVE WKA-SNSCD TO WIN1-SNSCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKDFILE (¾²·Õ³¼Ö Ò²»²·ÞÖ³ ÃÞ-À)                 *
      *----------------------------------------------------------------*
       M12-READ-WKD.
           READ WKDFILE INTO WKD-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           MOVE WKD-SKUCD TO WIN2-SKUCD.
           MOVE WKD-SNSCD TO WIN2-SNSCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.3 : LOCK-DB                                              *
      *----------------------------------------------------------------*
       M13-LOCK-DB.
           MOVE '   KMS' TO P89-TBLID ( 1 ).
           MOVE 'S'      TO P89-LTYPE ( 1 ).
           MOVE ALL '*'  TO P89-TABLE ( 2 ).
           CALL 'VSBA0890' USING P89-PARM.
       M13-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        (MATCHING)                                 *
      *----------------------------------------------------------------*
       M2-MAIN.
           MOVE 1  TO WCT1-PAGE.
           MOVE 0  TO WCT1-GYO.
           MOVE 19 TO WSV3-GYO.
           MOVE SPACE TO WED1-RCKBN.
           MOVE '1'   TO WED1-DSHZFG.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-WKAONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKDONL THRU M23-EXIT.
           PERFORM M24-SKPOUT THRU M24-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           PERFORM M211-SKUOUT THRU M211-EXIT.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M212-SKMOUT THRU M212-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
           IF WCT1-PAGE > 9999
              IF WFG1-OVER = ZERO
                 DISPLAY 'C VBKA1560 SEIKYU-MEISAI 9,999 OVER'
                 UPON CONSOLE
              END-IF
              MOVE  WCT1-PAGE   TO  WED1-PAGE
              DISPLAY 'C VBKA1560 SEIKYU-SAKI=' WSV2-SKUCD '('
                                                WSV2-SNSCD ')'
                                        ' PAGE=' WED1-PAGE
              UPON CONSOLE
              MOVE '1'    TO  WFG1-OVER.
           MOVE WSV2-KEY1  TO WSV3-KEY1.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : SKUOUT (¾²·Õ³ Ò²»² ÃÞ-À OUT : ¾²·Õ³ ³ÁÜ¹ Úº-ÄÞ)    *
      *----------------------------------------------------------------*
       M211-SKUOUT.
           MOVE SPACE      TO WKC-R.
           MOVE WKA-SKUCD  TO WKC-SKUCD.
           MOVE WKA-SNSCD  TO WKC-SNSCD.
           MOVE 0          TO WKC-PAGE1.
           MOVE 1          TO WKC-PAGE2.
           MOVE '00'       TO WKC-GYONO.
           MOVE '50'       TO WKC-RCKBN.
           MOVE WKA-UDATA  TO WKC-UDATA.
           IF WKA-SKUCD NOT = WSV5-SKUCD
              MOVE WKA-SKUCD  TO WSV5-SKUCD
              MOVE WKA-SKUCD  TO TOKCD OF KMS
              PERFORM M2111-SELECT-KMS THRU M2111-EXIT.
           MOVE SHZSK OF KMS  TO WKC-SHZSK.
           WRITE WKC-REC FROM WKC-R.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.1 : SELECT-KMS                                       *
      *----------------------------------------------------------------*
       M2111-SELECT-KMS.
           EXEC SQL WHENEVER NOT FOUND  GO TO M2111-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M2111-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M2111-ERR END-EXEC.
           EXEC SQL SELECT C5
              INTO :DCLKMS.C5
              FROM    KMS
              WHERE A0=:DCLKMS.A0
           END-EXEC.
           GO TO M2111-EXIT.
       M2111-L01.
           MOVE SPACE   TO SHZSK OF KMS.
           GO TO M2111-EXIT.
       M2111-ERR.
           DISPLAY 'R VBKA1560 TABLE(SI.KMS) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M2111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : SKMOUT (¾²·Õ³ Ò²»² ÃÞ-À OUT : ¾²·Õ³ Ò²»²·ÞÖ³ Úº-ÄÞ)*
      *----------------------------------------------------------------*
       M212-SKMOUT.
           IF WCT1-GYO NOT < WSV3-GYO
              ADD  1  TO WCT1-PAGE
              MOVE 0  TO WCT1-GYO
              MOVE 29 TO WSV3-GYO.
           IF WKD-RCKBN = '60'
              MOVE '60'      TO WED1-RCKBN
              IF WKD-DSHZFG NOT = '1'
                 MOVE SPACE  TO WED1-DSHZFG
              END-IF
              PERFORM M2121-SKMOUT60 THRU M2121-EXIT
              GO TO M212-L01.
           IF WKD-RCKBN = '63' OR '64'
              PERFORM M2123-SKMOUT63 THRU M2123-EXIT
           ELSE
              IF WKD-RCKBN = '65'
                 PERFORM M2122-SKMOUT65 THRU M2122-EXIT
              ELSE
                 PERFORM M2124-SKMOUT66 THRU M2124-EXIT.
       M212-L01.
           PERFORM M12-READ-WKD THRU M12-EXIT.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.1 : SKMOUT60                                         *
      *----------------------------------------------------------------*
       M2121-SKMOUT60.
           ADD 1 TO WCT1-GYO.
           MOVE SPACE      TO WKC-R.
           MOVE WKD-SKUCD  TO WKC-SKUCD.
           MOVE WKD-SNSCD  TO WKC-SNSCD.
           MOVE 0          TO WKC-PAGE1.
           MOVE WCT1-PAGE  TO WKC-PAGE2.
           MOVE WCT1-GYO   TO WKC-GYONO.
           MOVE WKD-RCKBN  TO WKC-RCKBN.
      *
           MOVE WKD-DENO1  TO WIN2-DENNO.
           MOVE WKD-TRKBN  TO WIN2-TRKBN.
           IF ( WCT1-GYO = 1 ) OR
              ( WIN2-KEY2  NOT = WSV2-KEY2 )
              MOVE WKD-DEMM1 TO WKC-DEMM1
              MOVE WKD-DEDD1 TO WKC-DEDD1
              MOVE WKD-TRKKJ TO WKC-TRKKJ
              MOVE WKD-DENO1 TO WED1-DENO
              PERFORM M21211-DENNO-EDIT THRU M21211-EXIT
              MOVE WED1-DENO TO WKC-DENO1
           ELSE
              MOVE ALL '0'  TO WKC-DEHI1
              MOVE SPACE    TO WKC-DENO1
              MOVE SPACE    TO WKC-TRKKJ.
           MOVE WIN2-KEY2  TO WSV2-KEY2.
      *
           MOVE WKD-DENKG  TO WKC-DENKG.
           MOVE WKD-SYNAM  TO WKC-SYNAM.
           MOVE WKD-SURYO  TO WKC-SURYO.
           MOVE WKD-TANKA  TO WKC-TANKA.
           MOVE WKD-KINGK  TO WKC-KINGK.
           MOVE WKD-SYOCD  TO WKC-SYOCD.
           MOVE WKD-SYOHM  TO WKC-SYOHM.
           MOVE WKD-DEMM2  TO WKC-DEMM2.
           MOVE WKD-DEDD2  TO WKC-DEDD2.
           MOVE WKD-DENO2  TO WED1-DENO.
           PERFORM M21211-DENNO-EDIT THRU M21211-EXIT.
           MOVE WED1-DENO  TO WKC-DENO2.
           MOVE WKD-THKBN  TO WKC-THKBN.
           MOVE WKD-BUKBN  TO WKC-BUKBN.
           MOVE WKD-SHZEI  TO WKC-SHZEI.
           MOVE WKD-ZEGAK  TO WKC-MESHZ.
           MOVE WKD-DSHZFG TO WKC-DSHZFG.
           MOVE SPACE      TO WKC-MSHZUM.
           MOVE WKD-ZEIRT  TO WKC-MSZRT.
           COMPUTE WKC-DPGK60 = WKD-DENKG + WKD-SHZEI.
           IF WKD-KGZKB = '1'
              MOVE MSV6-ASTER TO WKC-KGZH60
           ELSE
              MOVE SPACE      TO WKC-KGZH60.
           WRITE WKC-REC FROM WKC-R.
       M2121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.1.1 : DENPNO EDIT                                    *
      *----------------------------------------------------------------*
       M21211-DENNO-EDIT.
      *** BU-KOYU ***
           IF WED1-DENO  = SPACE
              GO TO M21211-EXIT.
           IF ( WKD-STNCD = '011' OR '016' OR '904' ) AND
              ( WED1-DENOH1 = SPACE )
              STRING WED1-DENOH2 ' '
                 DELIMITED BY SIZE INTO WED1-DENO
              GO TO M21211-EXIT.
       M21211-L01.
           MOVE '0'         TO WED1-DENOH1.
       M21211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.2 : SKMOUT65                                         *
      *----------------------------------------------------------------*
       M2122-SKMOUT65.
           ADD 1 TO WCT1-GYO.
           MOVE SPACE      TO WKC-R.
           MOVE WKD-SKUCD  TO WKC-SKUCD.
           MOVE WKD-SNSCD  TO WKC-SNSCD.
           MOVE 0          TO WKC-PAGE1.
           MOVE WCT1-PAGE  TO WKC-PAGE2.
           MOVE WCT1-GYO   TO WKC-GYONO.
           MOVE WKD-RCKBN  TO WKC-RCKBN.
           MOVE WKD-TOKCD  TO WKC-TOKCD.
           MOVE WKD-NOSCD  TO WKC-NOSCD.
           MOVE WKD-MIDAA  TO WKC-MIDAA.
           MOVE WKD-MIDAB  TO WKC-MIDAB.
           MOVE WKD-TOKTT  TO WKC-TOKTT.
           MOVE WKD-SHZTT  TO WKC-SHZNT.
           COMPUTE WED1-TNGKK = WKD-TOKTT + WKD-SHZTT.
           MOVE WED1-TNGKK TO WKC-TNGKK.
           WRITE WKC-REC FROM WKC-R.
      *
           IF WCT1-PAGE > 1
              IF WCT1-GYO = 29
                 GO TO M2122-EXIT
              ELSE NEXT SENTENCE
           ELSE
              IF WCT1-GYO = 19
                 GO TO M2122-EXIT
              ELSE NEXT SENTENCE.
      *
           ADD 1 TO WCT1-GYO.
           MOVE SPACE      TO WKC-R.
           MOVE WKD-SKUCD  TO WKC-SKUCD.
           MOVE WKD-SNSCD  TO WKC-SNSCD.
           MOVE 0          TO WKC-PAGE1.
           MOVE WCT1-PAGE  TO WKC-PAGE2.
           MOVE WCT1-GYO   TO WKC-GYONO.
           MOVE '65'       TO WKC-RCKBN.
           MOVE SPACE      TO WKC-TOKCD.
           MOVE SPACE      TO WKC-NOSCD.
           MOVE SPACE      TO WKC-MIDAA.
           MOVE SPACE      TO WKC-MIDAB.
           MOVE ZERO       TO WKC-TOKTT.
           MOVE ZERO       TO WKC-SHZNT.
           MOVE ZERO       TO WKC-TNGKK.
           WRITE WKC-REC FROM WKC-R.
       M2122-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.3 : SKMOUT63 OR 64                                   *
      *----------------------------------------------------------------*
       M2123-SKMOUT63.
           ADD 1 TO WCT1-GYO.
           MOVE SPACE      TO WKC-R.
           MOVE WKD-SKUCD  TO WKC-SKUCD.
           MOVE WKD-SNSCD  TO WKC-SNSCD.
           MOVE 0          TO WKC-PAGE1.
           MOVE WCT1-PAGE  TO WKC-PAGE2.
           MOVE WCT1-GYO   TO WKC-GYONO.
           MOVE WKD-RCKBN  TO WKC-RCKBN.
      *
           MOVE WKD-DEMM1  TO WKC-DEMM3.
           MOVE WKD-DEDD1  TO WKC-DEDD3.
           PERFORM M21231-MID2B-PROC THRU M21231-EXIT.
           MOVE WKD-DENKG  TO WKC-DEKIN.
           MOVE WKD-SHZEI  TO WKC-SHZKT.
           COMPUTE WKC-DPGK6X = WKD-DENKG + WKD-SHZEI.
           MOVE WKD-ZEIRT  TO WKC-ZERT6X.
           IF WKD-KGZKB = '1'
              MOVE MSV6-ASTER TO WKC-KGZH6X
           ELSE
              MOVE SPACE      TO WKC-KGZH6X.
           WRITE WKC-REC FROM WKC-R.
       M2123-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.3.1 : MIDASI GYO B (SKMOUT63 OR 64)                  *
      *----------------------------------------------------------------*
       M21231-MID2B-PROC.
           MOVE WKD-ZEIRT     TO WED1-ZEIRT.
           IF WED1-ZEI1 = ZERO
              IF WED1-ZEI3 = ZERO
                 STRING WED1-ZEI2 '%)'
                    DELIMITED BY SIZE INTO P48-INDAT-X
                 MOVE 3       TO P48-INLEN
              ELSE
                 STRING WED1-ZEI2 '.' WED1-ZEI3 '%)'
                    DELIMITED BY SIZE INTO P48-INDAT-X
                 MOVE 5       TO P48-INLEN
              END-IF
           ELSE
              IF WED1-ZEI3 = ZERO
                 STRING WED1-ZEI1 WED1-ZEI2 '%)'
                    DELIMITED BY SIZE INTO P48-INDAT-X
                 MOVE 4       TO P48-INLEN
              ELSE
                 STRING WED1-ZEI1 WED1-ZEI2 '.' WED1-ZEI3 '%)'
                    DELIMITED BY SIZE INTO P48-INDAT-X
                 MOVE 6       TO P48-INLEN
              END-IF
           END-IF.
           CALL 'VSBA0480' USING P48-PARM.
       M21231-L02.
           IF WKD-ZEIRT = P205-SHZRT
              IF WKD-RCKBN = '63'
                 STRING WSV4-MID631 P48-OTDAT-X
                    DELIMITED BY SIZE INTO WKC-MID2B
              ELSE
                 STRING WSV4-MID641 P48-OTDAT-X
                    DELIMITED BY SIZE INTO WKC-MID2B
              END-IF
           ELSE
              IF WKD-KGZKB = '1'
                 IF WKD-RCKBN = '63'
                    STRING WSV4-MID632 P48-OTDAT-X
                       DELIMITED BY SIZE INTO WKC-MID2B
                 ELSE
                    STRING WSV4-MID642 P48-OTDAT-X
                       DELIMITED BY SIZE INTO WKC-MID2B
                 END-IF
              ELSE
                 IF WKD-ZEIRT = ZERO
                    IF WKD-RCKBN = '63'
                       MOVE WSV4-MID634 TO WKC-MID2B
                    ELSE
                       MOVE WSV4-MID644 TO WKC-MID2B
                    END-IF
                 ELSE
                    IF WKD-RCKBN = '63'
                       STRING WSV4-MID633 P48-OTDAT-X
                          DELIMITED BY SIZE INTO WKC-MID2B
                    ELSE
                       STRING WSV4-MID643 P48-OTDAT-X
                          DELIMITED BY SIZE INTO WKC-MID2B
                    END-IF
                 END-IF
              END-IF
           END-IF.
       M21231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.4 : SKMOUT66                                         *
      *----------------------------------------------------------------*
       M2124-SKMOUT66.
           ADD 1 TO WCT1-GYO.
      *
           IF WKD-ZRITT NOT = ZERO
              IF ((WCT1-PAGE = 1 AND WCT1-GYO = 19) OR
                  ((WCT1-PAGE NOT = 1) AND WCT1-GYO = 29))
                 ADD  1  TO WCT1-PAGE
                 MOVE 1  TO WCT1-GYO
                 MOVE 29 TO WSV3-GYO.
      *
           MOVE SPACE      TO WKC-R.
           MOVE WKD-SKUCD  TO WKC-SKUCD.
           MOVE WKD-SNSCD  TO WKC-SNSCD.
           MOVE 0          TO WKC-PAGE1.
           MOVE WCT1-PAGE  TO WKC-PAGE2.
           MOVE WCT1-GYO   TO WKC-GYONO.
           MOVE WKD-RCKBN  TO WKC-RCKBN.
      *
           MOVE WKD-MIDAT  TO WKC-MIDAU6.
           MOVE WKD-MIDBT  TO WKC-MIDAD6.
           MOVE WKD-ZRKGT  TO WKC-ZOKDK.
           MOVE WKD-ZRSZT  TO WKC-ZSHZI.
           MOVE ZERO       TO WKC-ZOKGDK.
           MOVE WKD-ZRITT  TO WKC-ZEIRT6.
           MOVE WKD-KGZKT  TO WKC-KGZRT6.
           WRITE WKC-REC FROM WKC-R.
      *
           IF WKD-ZRITT = ZERO
              GO TO M2124-EXIT.
      *
           ADD 1 TO WCT1-GYO.
           MOVE WKD-SKUCD     TO WKC-SKUCD.
           MOVE WKD-SNSCD     TO WKC-SNSCD.
           MOVE 0             TO WKC-PAGE1.
           MOVE WCT1-PAGE     TO WKC-PAGE2.
           MOVE WCT1-GYO      TO WKC-GYONO.
           MOVE '66'          TO WKC-RCKBN.
      *
           MOVE SPACE         TO WKC-MIDAU6.
           MOVE WED1-MIDAD6   TO WKC-MIDAD6.
           COMPUTE WKC-ZOKDK = WKD-ZRKGT + WKD-ZRSZT.
           MOVE ZERO          TO WKC-ZSHZI.
           MOVE ZERO          TO WKC-ZOKGDK.
           MOVE WKD-ZRITT     TO WKC-ZEIRT6.
           MOVE WKD-KGZKT     TO WKC-KGZRT6.
           WRITE WKC-REC FROM WKC-R.
       M2124-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           MOVE SPACE     TO WED1-DSHZFG.
           PERFORM M211-SKUOUT THRU M211-EXIT.
           MOVE WIN1-KEY1 TO WSV3-KEY1.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKDFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKDONL.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M231-HUIOUT  THRU M231-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
           MOVE HIGH-VALUE TO WSV3-KEY1.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1 : HUI-OUT                                            *
      *----------------------------------------------------------------*
       M231-HUIOUT.
           MOVE SPACE          TO HUI-R.
           MOVE 'VBKA1560'     TO HUI-PRGME.
           MOVE WIN2-KEY1      TO HUI-DKEY.
           MOVE '¾²·Õ³Ò²»² DATA ÉÐ' TO HUI-MSGNI.
           MOVE WKD-R          TO HUI-DATAB.
           MOVE  'Z '    TO   S-CODE.
           WRITE HUI-REC FROM HUI-R.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA1560 HUIFILE WRITE ERROR !'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              PERFORM M3-FINL THRU M3-EXIT
              CALL 'VSBABEND'.
           PERFORM M12-READ-WKD THRU M12-EXIT.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.4 : SKPOUT (¾²·Õ³¼Ö Íß-¼Þ ÃÞ-À OUT)                      *
      *----------------------------------------------------------------*
       M24-SKPOUT.
           IF WSV3-KEY1 = HIGH-VALUE
              GO TO M24-EXIT.
           MOVE SPACE      TO WKB-R.
           MOVE WSV3-SKUCD TO WKB-SKUCD.
           MOVE WSV3-SNSCD TO WKB-SNSCD.
           MOVE WKC-PAGE2  TO WKB-PAGE1.
           IF WED1-RCKBN  = '60' AND
              WED1-DSHZFG = '1'
              MOVE ZERO    TO WKB-MSHZUM
           ELSE
              MOVE '1'     TO WKB-MSHZUM.
           WRITE WKB-REC FROM WKB-R.
       M24-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKDFILE
                 WKCFILE
                 WKBFILE
                 HUIFILE.
       M3-L01.
           IF WFG1-OVER = '1'
              DISPLAY 'C VBKA1560 KAIHATUKA-CALL!! Y KEY-IN'
              UPON CONSOLE
              ACCEPT WED1-ANSER FROM CONSOLE
              IF WED1-ANSER = 'Y'
                 CONTINUE
              ELSE
                 GO TO M3-L01
              END-IF
           ELSE
              DISPLAY '*** ´×° Å¼ ***'.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA1560 END                                           *
      *----------------------------------------------------------------*
