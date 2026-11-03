./ ADD NAME=VBKA1580  01.03 20150731 20190726 113900 00414 00403 MCSK012
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1580'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : KAKAXX  SYSTEM                *
      *              <> PG.OUTLINE     : CONTROL BREAK                 *
      *                 * INPUT  FILE  : Ä³¹ÞÂ¾²·­³Ò²»² ÃÞ-À     (DWKC)*
      *                 * OUTPUT FILE  : Ä³¹ÞÂ¾²·­³ ÄØË·Ò²»² ÃÞ-À(DWKD)*
      *                 * OUTPUT FILE  : ¹ÂÃ² ÄØ¹¼¥ÊÂ¾² ºÞ³¹² ´×-Ø½Ä   *
      *                                                          (PRT1)*
      *              <> AUTHOR         : SUZUKEN-A.YAMAMOTO            *
      *              <> DATE           : 86/09/10                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) KAISYU-45                 : 89/03/10  CSK-A.ONITSUKA      *
      * (02) YOUKEN HENKOU K-062       : 94/05/25  SUZ-Y.GOTOH         *
      * (03) H-020  M-0018 ÍÝº³        : 95/03/28  CSK-H.MIYATA        *
      * (04) K-275C        ÍÝº³        : 98/04/28  CSK-R.TAKAYANAGI    *
      * (05) K-407         ÍÝº³        : 99/12/16  CSK-Y.DOYAMA        *
      * (06) K-1549        ÍÝº³        : 14/02/10  CSK-N.EGUCHI        *
      * (07) K-1780        ÍÝº³        : 15/07/31  CSK-Y.YOSHIGA       *
      * (08) K-2163        ÍÝº³        : 19/06/25  CSK-M.MATSUSHIMA    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           C01 IS SKPCH01.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
           SELECT LPFILE  ASSIGN    TO S-PRT1.
       DATA DIVISION.
       FILE SECTION.
       FD  WKCFILE RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(147).
       FD  WKDFILE RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(252).
       FD  LPFILE RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD OMITTED.
       01  LP-REC.
           02 FILLER                        PIC  X(132).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  PC5-PARM.
           COPY VPARBC5 SUPPRESS.
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  WKC-R.
           02 WKC-STNCD                     PIC  X(03).
           02 WKC-SKUCD                     PIC  X(07).
           02 WKC-SNSCD                     PIC  X(02).
           02 WKC-SOKOK                     PIC  X(01).
           02 WKC-CKUKG                     PIC  X(05).
           02 WKC-TOKCD                     PIC  X(07).
           02 WKC-NSKBN                     PIC  X(02).
           02 WKC-DNPH1                     PIC  9(08).
           02 WKC-DNPN1                     PIC  X(07).
           02 WKC-DNPNL                     PIC  9(01).
           02 WKC-TORIK                     PIC  X(02).
           02 WKC-SYOCD                     PIC  X(09).
           02 WKC-SYOME                     PIC  X(45).
           02 WKC-SURYO                     PIC S9(05) COMP-3.
           02 WKC-TANKA                     PIC S9(08) COMP-3.
           02 WKC-KINGK                     PIC S9(09) COMP-3.
           02 WKC-DNPH2                     PIC  9(08).
           02 WKC-DNPN2                     PIC  X(07).
           02 WKC-THKBN                     PIC  X(01).
           02 WKC-BNKKB                     PIC  X(01).
           02 WKC-KETKB                     PIC  X(01).
           02 WKC-SHIMM                     PIC  X(08).
           02 WKC-SHZEI                     PIC S9(09) COMP-3.
           02 WKC-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKC-DSHZFG                    PIC  X(01).
           02 WKC-KGZKB                     PIC  X(01).
       01  WKD-R.
           02 WKD-STNCD                     PIC  X(03).
           02 WKD-SKUCD                     PIC  X(07).
           02 WKD-SNSCD                     PIC  X(02).
           02 WKD-SOKOK                     PIC  X(01).
           02 WKD-CKUKG                     PIC  X(05).
           02 WKD-TOKCD                     PIC  X(07).
           02 WKD-NSKBN                     PIC  X(02).
           02 WKD-DNPH1                     PIC  9(08).
           02 WKD-REKBN                     PIC  X(02).
           02 WKD-DNPN1                     PIC  X(07).
           02 WKD-DNPNL                     PIC  9(01).
           02 WKD-TORIK                     PIC  X(02).
           02 WKD-SYOCD                     PIC  X(09).
           02 WKD-SYOME                     PIC  X(90).
           02 WKD-SURYO                     PIC S9(05) COMP-3.
           02 WKD-TANKA                     PIC S9(08) COMP-3.
           02 WKD-KINGK                     PIC S9(09) COMP-3.
           02 WKD-DNKIN                     PIC S9(11) COMP-3.
           02 WKD-DNPH2                     PIC  9(08).
           02 WKD-DNPN2                     PIC  X(07).
           02 WKD-THKBN                     PIC  X(01).
           02 WKD-BNKKB                     PIC  X(01).
           02 WKD-KETKB                     PIC  X(01).
           02 WKD-TORKJ                     PIC  X(08).
           02 WKD-SYOME-X.
              03 WKD-SYOMH-X                PIC  X(25).
              03 WKD-SYOMKY-X               PIC  X(20).
           02 WKD-SHZEI                     PIC S9(11) COMP-3.
           02 WKD-ZEGAK                     PIC S9(09) COMP-3.
           02 WKD-DNGYO                     PIC  9(01).
           02 WKD-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKD-DSHZFG                    PIC  X(01).
           02 WKD-KGZKB                     PIC  X(01).
       01  INPUT-AREA.
           02 WIN1-KEY1.
              03 WIN1-SKUCD                 PIC  X(07).
              03 WIN1-SNSCD                 PIC  X(02).
              03 WIN1-TOKCD                 PIC  X(07).
              03 WIN1-NSKBN                 PIC  X(02).
              03 WIN1-DNPH1                 PIC  X(08).
              03 WIN1-ZEIRT                 PIC S9(02)V9 COMP-3.
              03 WIN1-KGZKB                 PIC  X(01).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-SKUCD                 PIC  X(07).
              03 WSV1-SNSCD                 PIC  X(02).
              03 WSV1-TOKCD                 PIC  X(07).
              03 WSV1-NSKBN                 PIC  X(02).
              03 WSV1-DNPH1                 PIC  X(08).
              03 WSV1-ZEIRT                 PIC S9(02)V9 COMP-3.
              03 WSV1-KGZKB                 PIC  X(01).
           02 WSV1-SW                       PIC  9(01).
       01  COUNT-AREA.
           02 WCT1-LINE                     PIC S9(03)  COMP.
       01  TOTAL-AREA.
           02 WTT1-KINGK                    PIC S9(13) COMP-3.
           02 WTT2-KINGK                    PIC S9(13) COMP-3.
           02 WTT1-SHZEI                    PIC S9(13) COMP-3.
           02 WTT2-SHZEI                    PIC S9(13) COMP-3.
       01  EDIT-AREA.
           02 WED1-SKUCD.
              03 WED1-SKUCD1                PIC  X(04).
              03 WED1-SKUCD2                PIC  X(03).
           02 WED1-TOKCD.
              03 WED1-TOKCD1                PIC  X(04).
              03 WED1-TOKCD2                PIC  X(03).
           02 WED1-CKUKG.
              03 WED1-CKUKG1                PIC  X(03).
              03 WED1-CKUKG2                PIC  X(02).
           02 WED1-DNPH1                    PIC  9(08).
           02 WED1-DNPH1-X  REDEFINES  WED1-DNPH1.
              03 WED1-SS                    PIC  9(02).
              03 WED1-YMD                   PIC  9(06).
       01  LPH-HEADER.
           02 LPH-HEAD1.
              03 FILLER        PIC  X(132) VALUE SPACE.
           02 LPH-HEAD2.
              03 FILLER        PIC  X(20)  VALUE ' VBKA1580   ** ¹ÂÃ²Ä'.
              03 FILLER        PIC  X(20)  VALUE 'Ø¹¼ ÊÂ¾² ºÞ³¹² ´×- Ø'.
              03 FILLER        PIC  X(17)  VALUE '½Ä **            '.
              03 LPH-YYMMDD    PIC  99/99/99.
              03 FILLER        PIC  X(05)  VALUE ' »¸¾²'.
              03 FILLER        PIC  X(62)  VALUE SPACE.
           02 LPH-HEAD3.
              03 FILLER        PIC X(111)  VALUE SPACE.
              03 FILLER        PIC  X(03)  VALUE '** '.
              03 FILLER        PIC  X(18)  VALUE 'ÁÖ³» º³Ó¸(ÊÝÄÞ) **'.
           02 LPH-HEAD4.
              03 FILLER        PIC  X(20)  VALUE '  ÌÞNO ¾²·Õ³»·º-ÄÞ ¾'.
              03 FILLER        PIC  X(20)  VALUE '²·Õ³É³¾² Ä¸²»·º-ÄÞ É'.
              03 FILLER        PIC  X(20)  VALUE '³¾²º-ÄÞ Á¸·ºÞ³ ÃÞÝËß'.
              03 FILLER        PIC  X(20)  VALUE 'Ö³ËÂÞ¹     ¹ÂÃ²ÄØ¹¼ '.
              03 FILLER        PIC  X(20)  VALUE 'ºÞ³¹²      ¹ÂÃ²ÊÂ¾² '.
              03 FILLER        PIC  X(20)  VALUE 'ºÞ³¹²  ¾Þ²   ¾²·Õ³Ö³'.
              03 FILLER        PIC  X(12)  VALUE '¼·  Õ³¿³ÄØÄÒ'.
       01  LPD-DETLE.
           02 LPD-DETL1.
              03 FILLER        PIC  X(02).
              03 LPD-STNCD     PIC  X(03).
              03 FILLER        PIC  X(03).
              03 LPD-SKUCD     PIC  X(08).
              03 FILLER        PIC  X(06).
              03 LPD-SNSCD     PIC  X(02).
              03 FILLER        PIC  X(05).
              03 LPD-TOKCD     PIC  X(08).
              03 FILLER        PIC  X(05).
              03 LPD-NSKBN     PIC  X(02).
              03 FILLER        PIC  X(04).
              03 LPD-CKUKG     PIC  X(06).
              03 FILLER        PIC  X(03).
              03 LPD-DNPH1     PIC  99/99/99.
              03 FILLER        PIC  X(02).
              03 LPD-KINGK1    PIC  --,---,---,---,--9.
              03 FILLER        PIC  X(02).
              03 LPD-KINGK2    PIC  --,---,---,---,--9.
              03 FILLER        PIC  X(02).
              03 LPD-ZEIRT     PIC  Z9.9.
              03 LPD-KGZKBX    PIC  X(01).
              03 FILLER        PIC  X(20).
       01  LPE-ERROR.
           02 LPE-ERR1.
              03 FILLER       PIC  X(20)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*-------VBKA1580----'.
              03 FILLER       PIC  X(04)  VALUE '---*'.
              03 FILLER       PIC  X(88)  VALUE SPACE.
           02 LPE-ERR2.
              03 FILLER       PIC  X(20)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*                   '.
              03 FILLER       PIC  X(04)  VALUE '   *'.
              03 FILLER       PIC  X(88)  VALUE SPACE.
           02 LPE-ERR3.
              03 FILLER       PIC  X(20)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*    ´×- DATA Å¼ !  '.
              03 FILLER       PIC  X(04)  VALUE '   *'.
              03 FILLER       PIC  X(88)  VALUE SPACE.
           02 LPE-ERR4.
              03 FILLER       PIC  X(20)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*-------------------'.
              03 FILLER       PIC  X(04)  VALUE '---*'.
              03 FILLER       PIC  X(88)  VALUE SPACE.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL WIN1-KEY1 = HIGH-VALUE.
           PERFORM M4-ERR-CHK THRU M4-EXIT.
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKCFILE
                OUTPUT WKDFILE
                       LPFILE.
           CALL 'VSBA2280' USING P228-PARM.
           MOVE P228-DATE TO PC5-YMDS8.
           CALL 'VSBA1250' USING PC5-PARM.
           MOVE PC5-YMDW6 TO LPH-YYMMDD.
           MOVE 99       TO WCT1-LINE.
           PERFORM M11-READ-WKC THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKCFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKC.
           READ WKCFILE INTO WKC-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY1
                   GO TO M11-EXIT.
       M11-L01.
           MOVE WKC-SKUCD TO WIN1-SKUCD.
           MOVE WKC-SNSCD TO WIN1-SNSCD.
           MOVE WKC-TOKCD TO WIN1-TOKCD.
           MOVE WKC-NSKBN TO WIN1-NSKBN.
           MOVE WKC-DNPH1 TO WIN1-DNPH1.
           MOVE WKC-ZEIRT TO WIN1-ZEIRT.
           MOVE WKC-KGZKB TO WIN1-KGZKB.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        (CONTROL BREAK)                            *
      *----------------------------------------------------------------*
       M2-MAIN.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           MOVE ZERO      TO WSV1-SW.
           MOVE ZERO      TO WTT1-KINGK.
           MOVE ZERO      TO WTT2-KINGK.
           MOVE ZERO      TO WTT1-SHZEI.
           MOVE ZERO      TO WTT2-SHZEI.
       M2-L01.
           PERFORM M21-WKD-HENSHU THRU M21-EXIT
                   UNTIL ( WIN1-KEY1 NOT = WSV1-KEY1 ).
       M2-L02.
           PERFORM M22-WKD-WRITE THRU M22-EXIT.
           IF WTT1-KINGK  >  99999999999 OR
              WTT1-KINGK  < -99999999999 OR
              WTT2-KINGK  >  99999999999 OR
              WTT2-KINGK  < -99999999999
              PERFORM M23-ERR-LIST THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : HENSHU                                               *
      *----------------------------------------------------------------*
       M21-WKD-HENSHU.
           IF WKC-KETKB = '1'
              COMPUTE WTT1-KINGK = WTT1-KINGK + WKC-KINGK
              COMPUTE WTT1-SHZEI = WTT1-SHZEI + WKC-SHZEI
           ELSE
              COMPUTE WTT2-KINGK = WTT2-KINGK + WKC-KINGK
              COMPUTE WTT2-SHZEI = WTT2-SHZEI + WKC-SHZEI.
       M21-L01.
           IF WSV1-SW = 0
              MOVE SPACE     TO WKD-R
              MOVE WKC-STNCD TO WKD-STNCD
              MOVE WKC-SKUCD TO WKD-SKUCD
              MOVE WKC-SNSCD TO WKD-SNSCD
              MOVE WKC-SOKOK TO WKD-SOKOK
              MOVE WKC-CKUKG TO WKD-CKUKG
              MOVE WKC-TOKCD TO WKD-TOKCD
              MOVE WKC-NSKBN TO WKD-NSKBN
              MOVE WKC-DNPH1 TO WKD-DNPH1
              MOVE ZERO      TO WKD-DNPNL
              MOVE ZERO      TO WKD-SURYO
              MOVE ZERO      TO WKD-TANKA
              MOVE ZERO      TO WKD-KINGK
              MOVE ZERO      TO WKD-DNPH2
              MOVE ZERO      TO WKD-DNGYO
              MOVE WKC-ZEIRT TO WKD-ZEIRT
              MOVE WKC-KGZKB TO WKD-KGZKB
              MOVE 1         TO WSV1-SW.
           PERFORM M11-READ-WKC THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKDFILE WRITE                                        *
      *----------------------------------------------------------------*
       M22-WKD-WRITE.
           MOVE '63'       TO WKD-REKBN.
           MOVE WTT1-KINGK TO WKD-DNKIN.
           MOVE WTT1-SHZEI TO WKD-SHZEI.
           MOVE WTT1-SHZEI TO WKD-ZEGAK.
           WRITE WKD-REC FROM WKD-R.
       M22-L01.
           MOVE '64'       TO WKD-REKBN.
           MOVE WTT2-KINGK TO WKD-DNKIN.
           MOVE WTT2-SHZEI TO WKD-SHZEI.
           MOVE WTT2-SHZEI TO WKD-ZEGAK.
           WRITE WKD-REC FROM WKD-R.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : ERROR LIST                                           *
      *----------------------------------------------------------------*
       M23-ERR-LIST.
           IF WCT1-LINE NOT < 28
              PERFORM M231-HEADER THRU M231-EXIT
           ELSE
              WRITE LP-REC FROM LPH-HEAD1 AFTER 1.
           MOVE SPACE      TO LPD-DETL1.
           MOVE WKD-STNCD TO LPD-STNCD.
           MOVE WKD-SKUCD TO WED1-SKUCD.
           STRING WED1-SKUCD1 '-' WED1-SKUCD2
              DELIMITED BY SIZE INTO LPD-SKUCD.
           MOVE WKD-SNSCD TO LPD-SNSCD.
           MOVE WKD-TOKCD TO WED1-TOKCD.
           STRING WED1-TOKCD1 '-' WED1-TOKCD2
              DELIMITED BY SIZE INTO LPD-TOKCD.
           MOVE WKD-NSKBN TO LPD-NSKBN.
           MOVE WKD-CKUKG TO WED1-CKUKG.
           STRING WED1-CKUKG1 '-' WED1-CKUKG2
              DELIMITED BY SIZE INTO LPD-CKUKG.
           MOVE WKD-DNPH1  TO WED1-DNPH1.
           MOVE WED1-YMD   TO LPD-DNPH1.
           MOVE WTT1-KINGK TO LPD-KINGK1.
           MOVE WTT2-KINGK TO LPD-KINGK2.
           MOVE WKD-ZEIRT  TO LPD-ZEIRT.
           IF WKD-KGZKB = '1'
              MOVE '*'     TO LPD-KGZKBX
           ELSE
              MOVE SPACE   TO LPD-KGZKBX.
           WRITE LP-REC FROM LPD-DETL1 AFTER 1.
       M23-L01.
           ADD 1           TO WCT1-LINE.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1 : HEADER                                             *
      *----------------------------------------------------------------*
       M231-HEADER.
           WRITE LP-REC FROM LPH-HEAD1 AFTER SKPCH01.
           WRITE LP-REC FROM LPH-HEAD2 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD3 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD4 AFTER 1.
           MOVE ZERO TO WCT1-LINE.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M4   : WRITE ERROR LIST 0 KEN                               *
      *----------------------------------------------------------------*
       M4-ERR-CHK.
           IF WCT1-LINE NOT = 99
              GO TO M4-EXIT.
           WRITE LP-REC FROM LPH-HEAD1 AFTER SKPCH01.
           WRITE LP-REC FROM LPH-HEAD2 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD3 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD4 AFTER 1.
           WRITE LP-REC FROM LPE-ERR1 AFTER 3.
           WRITE LP-REC FROM LPE-ERR2 AFTER 1.
           WRITE LP-REC FROM LPE-ERR3 AFTER 1.
           WRITE LP-REC FROM LPE-ERR2 AFTER 1.
           WRITE LP-REC FROM LPE-ERR4 AFTER 1.
       M4-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKCFILE
                 WKDFILE
                 LPFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA1580 END                                           *
      *----------------------------------------------------------------*
