./ ADD NAME=VSBA0430  01.01 19860823 20140925 103300 00259 00256 MCSK082
       ID  DIVISION.                                                    00010000
       PROGRAM-ID.    'VSBA0430'.                                       00020000
      *----------------------------------------------------------------*00030000
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *00040000
      *              <> SUBSYSTEM   : SUBROUTINE                       *00050000
      *              <> PG.OUTLINE  : BATCH.PRINT.FILE FORMAT HENKAN   *00060000
      *              <> AUTHOR      : CSK-M.TAKAHASHI                  *00070000
      *              <> DATE        : 85/10/28                         *00080000
      *------------------  MAINTENANCE   HISTORY  ---------------------*00090001
      * (1) HNBCD '4','5','6' TSUIKA   : 86/07/14  CSK-M.TAKAHASHI     *00100000
      * (2) HNBCD '7','8' TSUIKA       : 86/08/07  CSK-M.TAKAHASHI     *00110000
      * (03) K-1616           ØºÝÊß²Ù  : 14/09/25  CSK-K.KOYAMA        *00112001
      * (04) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *00112101
      * (05) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *00113001
      *----------------------------------------------------------------*00120000
       ENVIRONMENT DIVISION.                                            00130000
       DATA DIVISION.                                                   00140000
       WORKING-STORAGE SECTION.                                         00150000
      *----------------------------------------------------------------*00160000
      *    WORKING-STORAGE SECTION                                     *00170000
      *----------------------------------------------------------------*00180000
       01  SAVE-AREA.                                                   00190000
           02 WSV1-CCODE.                                               00200000
              03 FILLER                PIC S9(04) COMP VALUE +3599.     00210000
              03 FILLER                PIC S9(04) COMP VALUE +0258.     00220000
              03 FILLER                PIC S9(04) COMP VALUE +0772.     00230000
              03 FILLER                PIC S9(04) COMP VALUE +1286.     00240000
              03 FILLER                PIC S9(04) COMP VALUE +1800.     00250000
           02 FILLER REDEFINES WSV1-CCODE.                              00260000
              03 WSV1-0E               PIC  X(01).                      00270000
              03 WSV1-0F               PIC  X(01).                      00280000
              03 WSV1-01               PIC  X(01).                      00290000
              03 WSV1-02               PIC  X(01).                      00300000
              03 WSV1-03               PIC  X(01).                      00310000
              03 WSV1-04               PIC  X(01).                      00320000
              03 WSV1-05               PIC  X(01).                      00330000
              03 WSV1-06               PIC  X(01).                      00340000
              03 WSV1-07               PIC  X(01).                      00350000
              03 WSV1-08               PIC  X(01).                      00360000
       01  COUNT-AREA.                                                  00370000
           02 WCT1-H                   PIC S9(04) COMP.                 00380000
           02 WCT1-I                   PIC S9(04) COMP.                 00390000
           02 WCT1-J                   PIC S9(04) COMP.                 00400000
           02 WCT1-0E                  PIC S9(04) COMP.                 00410000
           02 WCT1-0F                  PIC S9(04) COMP.                 00420000
           02 WCT3-LEN                 PIC S9(04) COMP.                 00430000
       01  CONVERT-AREA.                                                00440000
           02 WCV1-LPDAT.                                               00450000
              03 WCV1-DATA             PIC  X(01) OCCURS 160 TIMES.     00460000
           02 WCV2-LPDAT.                                               00470000
              03 WCV2-DATA             PIC  X(01) OCCURS 300 TIMES.     00480000
           02 WCV3-LPDAT.                                               00490000
              03 FILLER OCCURS 0 TO 160 DEPENDING ON WCT3-LEN           00500000
                                       PIC  X(01).                      00510000
       LINKAGE SECTION.                                                 00520000
      *----------------------------------------------------------------*00530000
      *    LINKAGE SECTION                                             *00540000
      *----------------------------------------------------------------*00550000
       01  PARAM.                                                       00560000
           COPY VPARB43 SUPPRESS.                                       00570000
      /                                                                 00580000
       PROCEDURE DIVISION USING PARAM.                                  00590000
      *----------------------------------------------------------------*00600000
      *    MAIN-CONTROL                                                *00610000
      *----------------------------------------------------------------*00620000
       MAIN-CONTROL.                                                    00630000
           PERFORM M1-INIT THRU M1-EXIT.                                00640000
           PERFORM M2-MAIN THRU M2-EXIT                                 00650000
              UNTIL ( WCT1-I > P43-DATAL ).                             00660000
           PERFORM M3-FINL THRU M3-EXIT.                                00670000
           GOBACK.                                                      00680000
       MAIN-EXIT.                                                       00690000
           EXIT.                                                        00700000
      *----------------------------------------------------------------*00710000
      *    M1 : INITIAL                                                *00720000
      *----------------------------------------------------------------*00730000
       M1-INIT.                                                         00740000
           MOVE P43-LPDAT TO WCV1-LPDAT.                                00750000
           MOVE SPACE     TO WCV2-LPDAT.                                00760000
           MOVE 1         TO WCT1-H WCT1-I WCT1-J.                      00770000
       M1-EXIT.                                                         00780000
           EXIT.                                                        00790000
      *----------------------------------------------------------------*00800000
      *    M2 : MAIN ( CONVERT )                                       *00810000
      *----------------------------------------------------------------*00820000
       M2-MAIN.                                                         00830000
           IF ( WCT1-H > 20 ) OR ( P43-HNBCD (WCT1-H) = '*' )           00840000
              PERFORM M21-KANA-ONLY THRU M21-EXIT                       00850000
                 UNTIL ( WCT1-I > P43-DATAL )                           00860000
              GO TO M2-EXIT                                             00870000
           ELSE                                                         00880000
              PERFORM M21-KANA-ONLY THRU M21-EXIT                       00890000
                 UNTIL ( WCT1-I > P43-START (WCT1-H) - 1 ).             00900000
       M2-L01.                                                          00910000
           IF P43-HNBCD (WCT1-H) = '1'                                  00920000
              PERFORM M22-KANJI-ONLY THRU M22-EXIT                      00930000
           ELSE IF P43-HNBCD (WCT1-H) = '2'                             00940000
              PERFORM M23-KANA-KNJ-L THRU M23-EXIT                      00950000
           ELSE IF P43-HNBCD (WCT1-H) = '3'                             00960000
              PERFORM M24-KANA-KNJ-R THRU M24-EXIT                      00970000
           ELSE IF P43-HNBCD (WCT1-H) = '4'                             00980000
              PERFORM M25-KANJI-40   THRU M25-EXIT                      00990000
           ELSE IF P43-HNBCD (WCT1-H) = '5'                             01000000
              PERFORM M26-KANJI-32   THRU M26-EXIT                      01010000
           ELSE IF P43-HNBCD (WCT1-H) = '6'                             01020000
              PERFORM M27-KANJI-24   THRU M27-EXIT                      01030000
           ELSE IF P43-HNBCD (WCT1-H) = '7'                             01040000
              PERFORM M28-KANJI-HF-L THRU M28-EXIT                      01050000
           ELSE IF P43-HNBCD (WCT1-H) = '8'                             01060000
              PERFORM M29-KANJI-HF-R THRU M29-EXIT                      01070000
           ELSE                                                         01080000
              DISPLAY 'R VSBA0430 HANBETSU-CODE SHITEI ERROR ! => '     01090000
                 P43-HNBCD (WCT1-H) UPON CONSOLE                        01100000
              CALL 'VSBABEND'.                                          01110000
       M2-L02.                                                          01120000
           ADD 1 TO WCT1-H.                                             01130000
       M2-EXIT.                                                         01140000
           EXIT.                                                        01150000
      *----------------------------------------------------------------*01160000
      *    M21 : KANA ONLY                                             *01170000
      *----------------------------------------------------------------*01180000
       M21-KANA-ONLY.                                                   01190000
           MOVE WCV1-DATA (WCT1-I) TO WCV2-DATA (WCT1-J).               01200000
           ADD 1 TO WCT1-I WCT1-J.                                      01210000
       M21-EXIT.                                                        01220000
           EXIT.                                                        01230000
      *----------------------------------------------------------------*01240000
      *    M22 : KANJI ONLY                                            *01250000
      *----------------------------------------------------------------*01260000
       M22-KANJI-ONLY.                                                  01270000
           MOVE WSV1-0E TO WCV2-DATA (WCT1-J).                          01280000
           ADD 1 TO WCT1-J.                                             01290000
           PERFORM M22-L99 P43-LENGH (WCT1-H) TIMES.                    01300000
           MOVE WSV1-0F TO WCV2-DATA (WCT1-J).                          01310000
           ADD 1 TO WCT1-J.                                             01320000
           GO TO M22-EXIT.                                              01330000
       M22-L99.                                                         01340000
           PERFORM M21-KANA-ONLY THRU M21-EXIT.                         01350000
       M22-EXIT.                                                        01360000
           EXIT.                                                        01370000
      *----------------------------------------------------------------*01380000
      *    M23 : KANA & KANJI ( LEFT )                                 *01390000
      *----------------------------------------------------------------*01400000
       M23-KANA-KNJ-L.                                                  01410000
           MOVE P43-LENGH (WCT1-H) TO WCT3-LEN.                         01420000
           MOVE ZERO TO WCT1-0E WCT1-0F.                                01430000
           UNSTRING WCV1-LPDAT                                          01440000
              INTO WCV3-LPDAT WITH POINTER P43-START (WCT1-H).          01450000
           INSPECT WCV3-LPDAT TALLYING WCT1-0E FOR ALL WSV1-0E.         01460000
           INSPECT WCV3-LPDAT TALLYING WCT1-0F FOR ALL WSV1-0F.         01470000
           PERFORM M23-L99 P43-LENGH (WCT1-H) TIMES.                    01480000
           IF WCT1-0E > WCT1-0F                                         01490000
              MOVE WSV1-0F TO WCV2-DATA (WCT1-J)                        01500000
              ADD 1 TO WCT1-J.                                          01510000
           ADD WCT1-0E WCT1-0F TO WCT1-J.                               01520000
           GO TO M23-EXIT.                                              01530000
       M23-L99.                                                         01540000
           PERFORM M21-KANA-ONLY THRU M21-EXIT.                         01550000
       M23-EXIT.                                                        01560000
           EXIT.                                                        01570000
      *----------------------------------------------------------------*01580000
      *    M24 : KANA & KANJI ( RIGHT )                                *01590000
      *----------------------------------------------------------------*01600000
       M24-KANA-KNJ-R.                                                  01610000
           MOVE P43-LENGH (WCT1-H) TO WCT3-LEN.                         01620000
           MOVE ZERO TO WCT1-0E WCT1-0F.                                01630000
           UNSTRING WCV1-LPDAT                                          01640000
              INTO WCV3-LPDAT WITH POINTER P43-START (WCT1-H).          01650000
           INSPECT WCV3-LPDAT TALLYING WCT1-0E FOR ALL WSV1-0E.         01660000
           INSPECT WCV3-LPDAT TALLYING WCT1-0F FOR ALL WSV1-0F.         01670000
           ADD WCT1-0E WCT1-0F TO WCT1-J.                               01680000
           PERFORM M24-L99 P43-LENGH (WCT1-H) TIMES.                    01690000
           IF WCT1-0E > WCT1-0F                                         01700000
              MOVE WSV1-0F TO WCV2-DATA (WCT1-J)                        01710000
              ADD 1 TO WCT1-J.                                          01720000
           GO TO M24-EXIT.                                              01730000
       M24-L99.                                                         01740000
           PERFORM M21-KANA-ONLY THRU M21-EXIT.                         01750000
       M24-EXIT.                                                        01760000
           EXIT.                                                        01770000
      *----------------------------------------------------------------*01780000
      *    M25 : KANJI-40                                              *01790000
      *----------------------------------------------------------------*01800000
       M25-KANJI-40.                                                    01810000
           MOVE WSV1-01 TO WCV2-DATA (WCT1-J).                          01820000
           ADD 1 TO WCT1-J.                                             01830000
           PERFORM M25-L99 P43-LENGH (WCT1-H) TIMES.                    01840000
           MOVE WSV1-02 TO WCV2-DATA (WCT1-J).                          01850000
           ADD 1 TO WCT1-J.                                             01860000
           GO TO M25-EXIT.                                              01870000
       M25-L99.                                                         01880000
           PERFORM M21-KANA-ONLY THRU M21-EXIT.                         01890000
       M25-EXIT.                                                        01900000
           EXIT.                                                        01910000
      *----------------------------------------------------------------*01920000
      *    M26 : KANJI-32                                              *01930000
      *----------------------------------------------------------------*01940000
       M26-KANJI-32.                                                    01950000
           MOVE WSV1-03 TO WCV2-DATA (WCT1-J).                          01960000
           ADD 1 TO WCT1-J.                                             01970000
           PERFORM M26-L99 P43-LENGH (WCT1-H) TIMES.                    01980000
           MOVE WSV1-04 TO WCV2-DATA (WCT1-J).                          01990000
           ADD 1 TO WCT1-J.                                             02000000
           GO TO M26-EXIT.                                              02010000
       M26-L99.                                                         02020000
           PERFORM M21-KANA-ONLY THRU M21-EXIT.                         02030000
       M26-EXIT.                                                        02040000
           EXIT.                                                        02050000
      *----------------------------------------------------------------*02060000
      *    M27 : KANJI-24                                              *02070000
      *----------------------------------------------------------------*02080000
       M27-KANJI-24.                                                    02090000
           MOVE WSV1-05 TO WCV2-DATA (WCT1-J).                          02100000
           ADD 1 TO WCT1-J.                                             02110000
           PERFORM M27-L99 P43-LENGH (WCT1-H) TIMES.                    02120000
           MOVE WSV1-06 TO WCV2-DATA (WCT1-J).                          02130000
           ADD 1 TO WCT1-J.                                             02140000
           GO TO M27-EXIT.                                              02150000
       M27-L99.                                                         02160000
           PERFORM M21-KANA-ONLY THRU M21-EXIT.                         02170000
       M27-EXIT.                                                        02180000
           EXIT.                                                        02190000
      *----------------------------------------------------------------*02200000
      *    M28 : KANJI-HERF ( LEFT )                                   *02210000
      *----------------------------------------------------------------*02220000
       M28-KANJI-HF-L.                                                  02230000
           MOVE WSV1-07 TO WCV2-DATA (WCT1-J).                          02240000
           ADD 1 TO WCT1-J.                                             02250000
           PERFORM M23-KANA-KNJ-L THRU M23-EXIT.                        02260000
           MOVE WSV1-08 TO WCV2-DATA (WCT1-J).                          02270000
           ADD 1 TO WCT1-J.                                             02280000
       M28-EXIT.                                                        02290000
           EXIT.                                                        02300000
      *----------------------------------------------------------------*02310000
      *    M28 : KANJI-HERF ( RIGHT )                                  *02320000
      *----------------------------------------------------------------*02330000
       M29-KANJI-HF-R.                                                  02340000
           MOVE WSV1-07 TO WCV2-DATA (WCT1-J).                          02350000
           ADD 1 TO WCT1-J.                                             02360000
           PERFORM M24-KANA-KNJ-R THRU M24-EXIT.                        02370000
           MOVE WSV1-08 TO WCV2-DATA (WCT1-J).                          02380000
           ADD 1 TO WCT1-J.                                             02390000
       M29-EXIT.                                                        02400000
           EXIT.                                                        02410000
      *----------------------------------------------------------------*02420000
      *    M3 : FINL                                                   *02430000
      *----------------------------------------------------------------*02440000
       M3-FINL.                                                         02450000
           IF WCT1-J NOT > 196                                          02460000
              SUBTRACT 1 FROM WCT1-J GIVING P43-DATAL                   02470000
              MOVE WCV2-LPDAT TO P43-DATAM                              02480000
              GO TO M3-EXIT.                                            02490000
           DISPLAY 'R VSBA0430 PRINT-DATA OVERFLOW !!' UPON CONSOLE.    02500000
           CALL 'VSBABEND'.                                             02510000
       M3-EXIT.                                                         02520000
           EXIT.                                                        02530000
      *----------------------------------------------------------------*02540000
      *    END  VSBA0430                                               *02550000
      *----------------------------------------------------------------*02560000
