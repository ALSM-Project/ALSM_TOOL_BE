./ ADD NAME=VBKA1220  01.00 19980427 19980427 194345 00222 00222 TSO251
       ID  DIVISION.                                                    00010000
       PROGRAM-ID.     'VBKA1220'.                                      00020000
      *----------------------------------------------------------------*00030000
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *00040000
      *              <> SUBSYSTEM      : 20 SIME SEIKYU SHORI          *00050000
      *              <> PG.OUTLINE     : SIMEBI HENKOU SAKI            *00060000
      *                                  URIKAKEKIN FILE NO SHUUSEI    *00070000
      *                                                                *00080000
      *                 * INPUT FILE   : SHIMEBI HENKOUSAKI            *00090000
      *                                  URIAGEDAKAKEI DATA            *00100000
      *                                                   (DWKA)       *00110000
      *                 * I-O FILE     : URIKAKEKIN FILE  (URIFILE)    *00120000
      *                 * OUTPUT FILE  : URIKAKE IJYOU HASSEI FILE     *00130000
      *                                                   (CUIFILE)    *00140000
      *                                                                *00150000
      *              <> AUTHOR         : SUZUKEN-H.NIIKURA             *00160000
      *              <> DATE           : 86/08/08                      *00170000
      *------------------  MAINTENANCE   HISTORY  ---------------------*00180000
      * (01) SYOHIZEI TUIKA     ÍÝº³   : 89/03/01  CSK-M.AOYAMA        *00190000
      * (02) H-020  M-0051      ÍÝº³   : 95/10/16  CSK-M.KATOH         *00191000
      * (03) K-275C             ÍÝº³   : 98/04/27  CSK-R.TAKAYANAGI    *00192000
      * (04) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *00200000
      *----------------------------------------------------------------*00210000
       ENVIRONMENT DIVISION.                                            00220000
       INPUT-OUTPUT SECTION.                                            00230000
       FILE-CONTROL.                                                    00240000
           SELECT WKAFILE ASSIGN TO S-DWKA.                             00250000
           SELECT URIFILE ASSIGN    TO URIFILE                          00260000
                          ORGANIZATION INDEXED                          00270000
                          ACCESS  MODE RANDOM                           00280000
                          RECORD   KEY KEY-URI                          00290000
                          FILE  STATUS S-CODE.                          00300000
           SELECT CUIFILE ASSIGN    TO AS-CUIFILE                       00310000
                          ORGANIZATION SEQUENTIAL                       00320000
                          FILE  STATUS S-CODE.                          00330000
       DATA DIVISION.                                                   00340000
       FILE SECTION.                                                    00350000
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS                             00360000
           LABEL RECORD STANDARD.                                       00370000
       01  WKA-REC.                                                     00380000
           02 FILLER                        PIC  X(380).                00390000
       FD  URIFILE                                                      00400000
           LABEL RECORD STANDARD.                                       00410000
       01  URI-REC.                                                     00420000
           02 KEY-URI                       PIC  X(09).                 00430000
           02 FILLER                        PIC  X(841).                00440000
       FD  CUIFILE                                                      00450000
           LABEL RECORD STANDARD.                                       00460000
       01  CUI-REC.                                                     00470000
           02 FILLER                        PIC  X(430).                00480000
       WORKING-STORAGE SECTION.                                         00490000
      *----------------------------------------------------------------*00500000
      *    WORKING-STORAGE SECTION                                     *00510000
      *----------------------------------------------------------------*00520000
       77  S-CODE                           PIC  X(02).                 00530000
       01  WKA-R.                                                       00540000
           02 WKA-SITEK                     PIC  X(01).                 00550000
           02 WKA-TOKCD                     PIC  X(07).                 00560000
           02 WKA-SIMEB                     PIC  9(02).                 00570000
           02 WKA-NOSCD                     PIC  X(02).                 00580000
           02 WKA-TOHAT.                                                00590000
              03 WKA-TOET1                  PIC S9(11) COMP-3.          00600000
              03 WKA-TONE1                  PIC S9(11) COMP-3.          00610000
           02 WKA-TOHAZ.                                                00620000
              03 WKA-TOET2                  PIC S9(11) COMP-3.          00630000
              03 WKA-TONE2                  PIC S9(11) COMP-3.          00640000
           02 WKA-ZEHAZ.                                                00650000
              03 WKA-ZEET1                  PIC S9(11) COMP-3.          00660000
              03 WKA-ZENE1                  PIC S9(11) COMP-3.          00670000
           02 WKA-ZETRN.                                                00680000
              03 WKA-ZENMM                  OCCURS 12.                  00690000
                 04 WKA-ZENME               PIC S9(11) COMP-3.          00700000
                 04 WKA-ZENMN               PIC S9(11) COMP-3.          00710000
           02 WKA-SHZEI.                                                00720000
              03 WKA-STOAT.                                             00730000
                 04 WKA-STOE1               PIC S9(11) COMP-3.          00740000
                 04 WKA-STON1               PIC S9(11) COMP-3.          00750000
              03 WKA-STOAZ.                                             00760000
                 04 WKA-STOE2               PIC S9(11) COMP-3.          00770000
                 04 WKA-STON2               PIC S9(11) COMP-3.          00780000
              03 WKA-SZEAZ.                                             00790000
                 04 WKA-SZET1               PIC S9(11) COMP-3.          00800000
                 04 WKA-SZEN1               PIC S9(11) COMP-3.          00810000
              03 WKA-SZTRN.                                             00820000
                 04 WKA-SZNMM               OCCURS 12.                  00830000
                    05 WKA-SZNME            PIC S9(11) COMP-3.          00840000
                    05 WKA-SZNMN            PIC S9(11) COMP-3.          00840100
           02 WKA-SSIMD                     PIC  9(08).                 00840200
       01  URI-R.                                                       00840400
           COPY VFILURI SUPPRESS.                                       00840500
       01  CUI-R.                                                       00840600
           COPY VFILCUI SUPPRESS.                                       00840700
       01  INPUT-AREA.                                                  00840800
           02 WIN1-KEY.                                                 00840900
              03 WIN1-KEYB.                                             00841000
                 04 WIN1-KEYA.                                          00841100
                    05 WIN1-TOKCD           PIC  X(07).                 00841200
                 04 WIN1-NOSCD              PIC  X(02).                 00841300
       PROCEDURE DIVISION.                                              00841400
      *----------------------------------------------------------------*00841500
      *    M0 : CONTROL                                                *00841600
      *----------------------------------------------------------------*00841700
       M0-CONTROL.                                                      00841800
           PERFORM M1-INIT THRU M1-EXIT.                                00841900
           PERFORM M2-MAIN THRU M2-EXIT                                 00842000
                   UNTIL ( WIN1-KEY = HIGH-VALUE ).                     00843000
           PERFORM M3-FINL THRU M3-EXIT.                                00844000
      *                                                                 00845000
       M0-EXIT.                                                         00846000
           STOP RUN.                                                    00847000
      *----------------------------------------------------------------*00848000
      *    M1 : INITIAL                                                *00849000
      *----------------------------------------------------------------*00850000
       M1-INIT.                                                         00860000
           OPEN INPUT  WKAFILE                                          00870000
                I-O    URIFILE                                          00880000
                EXTEND CUIFILE.                                         00890000
           PERFORM M11-READ-WKA THRU M11-EXIT.                          00900000
      *                                                                 00910000
       M1-EXIT.                                                         00920000
           EXIT.                                                        00930000
      *----------------------------------------------------------------*00940000
      *    M1.1 : READ WKAFILE                                         *00950000
      *----------------------------------------------------------------*00960000
       M11-READ-WKA.                                                    00970000
           READ WKAFILE INTO WKA-R                                      00980000
                AT END                                                  00990000
                   MOVE HIGH-VALUE TO WIN1-KEY                          01000000
                   GO TO M11-EXIT.                                      01010000
           MOVE WKA-TOKCD TO WIN1-TOKCD.                                01020000
           MOVE WKA-NOSCD TO WIN1-NOSCD.                                01030000
      *                                                                 01040000
       M11-EXIT.                                                        01050000
           EXIT.                                                        01060000
      *----------------------------------------------------------------*01070000
      *    M2 : MAIN                                                   *01080000
      *----------------------------------------------------------------*01090000
       M2-MAIN.                                                         01100000
           PERFORM M21-NEST THRU M21-EXIT.                              01110000
       M2-L02.                                                          01120000
           PERFORM M11-READ-WKA THRU M11-EXIT.                          01130000
      *                                                                 01140000
       M2-EXIT.                                                         01150000
           EXIT.                                                        01160000
      *----------------------------------------------------------------*01170000
      *    M2.1 : NEST                                                 *01180000
      *----------------------------------------------------------------*01190000
       M21-NEST.                                                        01200000
           MOVE WIN1-KEY TO KEY-URI.                                    01210000
           MOVE 'Z '     TO S-CODE.                                     01220000
           READ URIFILE INTO URI-R INVALID KEY                          01230000
                GO TO M21-L02.                                          01240000
           IF S-CODE NOT = ZERO                                         01250000
              DISPLAY 'R VBKA1220 URIFILE READ ERROR ?!'                01260000
              UPON CONSOLE                                              01270000
              DISPLAY '  S-CODE = ' S-CODE                              01280000
              UPON CONSOLE                                              01290000
              PERFORM M3-FINL THRU M3-EXIT                              01300000
              CALL 'VSBABEND'.                                          01310000
       M21-L01.                                                         01320000
           PERFORM M211-OUT THRU M211-EXIT.                             01330000
           GO TO M21-EXIT.                                              01340000
       M21-L02.                                                         01350000
           PERFORM M212-OUT THRU M212-EXIT.                             01360000
      *                                                                 01370000
       M21-EXIT.                                                        01380000
           EXIT.                                                        01390000
      *----------------------------------------------------------------*01400000
      *    M2.1.1 URIFILE REWRITE                                      *01410000
      *----------------------------------------------------------------*01420000
       M211-OUT.                                                        01430000
           MOVE WKA-TOHAT TO URI-CTHAT.                                 01440000
           MOVE WKA-TOHAZ TO URI-CTHAZ.                                 01450000
           MOVE WKA-ZEHAZ TO URI-CZHAZ.                                 01460000
           MOVE WKA-ZETRN TO URI-CZTRN.                                 01470000
           MOVE WKA-STOAT TO URI-SCHAT.                                 01471000
           MOVE WKA-STOAZ TO URI-SCHAZ.                                 01472000
           MOVE WKA-SZEAZ TO URI-SCZHA.                                 01473000
           MOVE WKA-SZTRN TO URI-SCZTR.                                 01474000
           MOVE 'Z '      TO S-CODE.                                    01480000
           REWRITE URI-REC FROM URI-R.                                  01490000
           IF S-CODE NOT = ZERO                                         01500000
              DISPLAY 'R VBKA1220 URIFILE REWRITE ERROR ?!'             01510000
              UPON CONSOLE                                              01520000
              DISPLAY '  S-CODE = ' S-CODE                              01530000
              UPON CONSOLE                                              01540000
              PERFORM M3-FINL THRU M3-EXIT                              01550000
              CALL 'VSBABEND'.                                          01560000
      *                                                                 01570000
       M211-EXIT.                                                       01580000
           EXIT.                                                        01590000
      *----------------------------------------------------------------*01600000
      *    M2.1.2 CUIFILE WRITE                                        *01610000
      *----------------------------------------------------------------*01620000
       M212-OUT.                                                        01630000
           MOVE SPACES         TO CUI-R.                                01640000
           MOVE 'VBKA1220'     TO CUI-PRGNM.                            01650000
           MOVE WIN1-KEY       TO CUI-KEY.                              01660000
           MOVE 'URIFILE Æ Å¼' TO CUI-MSG.                              01670000
           MOVE WKA-R          TO CUI-DATA.                             01680000
           MOVE 'Z '           TO S-CODE.                               01690000
           WRITE CUI-REC FROM CUI-R.                                    01700000
           IF S-CODE NOT = ZERO                                         01710000
              DISPLAY 'R VBKA1220 CUIFILE WRITE ERROR ?!'               01720000
              UPON CONSOLE                                              01730000
              DISPLAY '  S-CODE = ' S-CODE                              01740000
              UPON CONSOLE                                              01750000
              PERFORM M3-FINL THRU M3-EXIT                              01760000
              CALL 'VSBABEND'.                                          01770000
      *                                                                 01780000
       M212-EXIT.                                                       01790000
           EXIT.                                                        01800000
      *----------------------------------------------------------------*01810000
      *    M3 : FINAL                                                  *01820000
      *----------------------------------------------------------------*01830000
       M3-FINL.                                                         01840000
           CLOSE WKAFILE                                                01850000
                 URIFILE                                                01860000
                 CUIFILE.                                               01870000
      *                                                                 01880000
       M3-EXIT.                                                         01890000
           EXIT.                                                        01900000
