./ ADD NAME=VBKA1230  01.02 19980427 20191125 084644 00931 00918 MCSK004
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1230'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : URIKAKE SYSTEM                *
      *              <> PG.OUTLINE     : MATCHING ( 1 : N )            *
      *                                                                *
      *                 * INPUT FILE   : TOKUISAKI MASTER (TMSFILE)    *
      *                 * INPUT FILE   : URIKAKEKIN FILE  (URIFILE)    *
      *                 * INPUT FILE   : NOSEI MASTER     (NSMFILE)    *
      *                 * OUTPUT FILE  : URIKAKEKIN DATA CHAN (DWKF)   *
      *                 * OUTPUT FILE  : URIKAKEKIN DATA SUZU (DWKE)   *
      *                                                                *
      *              <> AUTHOR         : SUZUKEN-Y.TAKEUZI             *
      *              <> DATE           : 86/08/06                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (02) SOKNO OUTPUT        Â²¶   : 87/12/01  SUZUKEN-C.ADACHI    *
      * (03) SYOHIZEI TUIKA      Â²¶   : 89/03/06  CSK-S.MUNEISHI      *
      * (04) K-122              ÍÝº³   : 95/06/02  CSK-H.MIYATA        *
      * (05) Z-175           ØºÝÊß²Ù   : 96/08/12  CSK-C.FURUTA        *
      * (06) K-275C             ÍÝº³   : 98/04/27  CSK-R.TAKAYANAGI    *
      * (07) K-2190             ÍÝº³   : 19/11/22  CSK-N.EGUCHI        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TMSFILE ASSIGN    TO TMSFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE SEQUENTIAL
                          RECORD   KEY KEY-TMS
                          FILE  STATUS S-CODE.
           SELECT URIFILE ASSIGN    TO URIFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE SEQUENTIAL
                          RECORD   KEY KEY-URI
                          FILE  STATUS S-CODE.
           SELECT NSMFILE ASSIGN    TO NSMFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE RANDOM
                          RECORD   KEY KEY-NSM
                          FILE  STATUS S-CODE.
           SELECT WKEFILE ASSIGN    TO S-DWKE.
           SELECT WKFFILE ASSIGN    TO S-DWKF.
       DATA DIVISION.
       FILE SECTION.
       FD  TMSFILE
           LABEL RECORD STANDARD.
       01  TMS-REC.
           02 KEY-TMS                       PIC  X(07).
           02 FILLER                        PIC  X(293).
       FD  URIFILE
           LABEL RECORD STANDARD.
       01  URI-REC.
           02 KEY-URI                       PIC  X(09).
           02 FILLER                        PIC  X(841).
       FD  NSMFILE
           LABEL RECORD STANDARD.
       01  NSM-REC.
           02 KEY-NSM.
              03 KEY-NTOK                   PIC  X(07).
              03 KEY-NNOS                   PIC  X(02).
           02 FILLER                        PIC  X(91).
       FD  WKEFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKE-REC.
           02 FILLER                        PIC  X(200).
       FD  WKFFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKF-REC.
           02 FILLER                        PIC  X(200).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  TMS-R.
           COPY VFILTMS SUPPRESS.
       01  URI-R.
           COPY VFILURI SUPPRESS.
       01  NSM-R.
           COPY VFILNSM SUPPRESS.
       01  P04-PARM.
           COPY VPARB04.
       01  P05-PARM.
           COPY VPARB05.
       01  P228-PARM.
           COPY VPARB228.
       01  WKE-R.
           02 WKE-STNKG                     PIC  X(01).
           02 WKE-SOKKG                     PIC  X(01).
           02 WKE-TOKCD                     PIC  X(07).
           02 WKE-NOSCD                     PIC  X(02).
           02 WKE-DYYMM                     PIC  9(06).
           02 WKE-SHIME                     PIC  9(02).
           02 WKE-STNCD                     PIC  X(03).
           02 WKE-CKUKG                     PIC  X(05).
           02 WKE-TOKME                     PIC  X(25).
           02 WKE-NOSME                     PIC  X(25).
           02 WKE-SKUCD                     PIC  X(07).
           02 WKE-ZMURI                     PIC S9(13)  COMP-3.
           02 WKE-ZMSEI                     PIC S9(13)  COMP-3.
           02 WKE-ICHYO                     PIC S9(13)  COMP-3.
           02 WKE-NYKIN                     PIC S9(13)  COMP-3.
           02 WKE-ZKRIK                     PIC S9(13)  COMP-3.
           02 WKE-TMKIR.
              03 WKE-TMET1                  PIC S9(13)  COMP-3.
              03 WKE-TMNE1                  PIC S9(13)  COMP-3.
           02 WKE-TMSEI                     PIC S9(13)  COMP-3.
           02 WKE-SAKIG                     PIC S9(13)  COMP-3.
           02 WKE-TMURI                     PIC S9(13)  COMP-3.
           02 WKE-TMHAN                     PIC S9(13)  COMP-3.
           02 WKE-SHZEI.
              03 WKE-SHET1                  PIC S9(13)  COMP-3.
              03 WKE-SHNE1                  PIC S9(13)  COMP-3.
           02 WKE-TSHZE                     PIC S9(13)  COMP-3.
           02 WKE-SOKNO                     PIC  X(03).
           02 FILLER                        PIC  X(15).
       01  WK-SYSINS.
           02 WK-SIMEB                      PIC  X(02).
           02 FILLER                        PIC  X(78).
       01  WK-SYSIN.
           02 WK-SITEK                      PIC  X(01).
           02 FILLER                        PIC  X(01).
           02 WK-SOKOK1        OCCURS   20 .
              03 WK-SOKOK                   PIC  X(01).
           02 FILLER                        PIC  X(58).
       01  WK-SYSINE  REDEFINES WK-SYSIN.
           02 WK-END                        PIC  X(03).
           02 FILLER                        PIC  X(77).
       01  WK-TABLEX.
           02 WK-TABLE         OCCURS   50.
              03 WK-TSITE                   PIC  X(01).
              03 WK-TSOKOX     OCCURS   20.
                 04 WK-TSOKO                PIC  X(01).
       01  WK-SYYMMDD.
           02 WK-SYYMM.
              03 WK-SYY                     PIC  9(04).
              03 WK-SMM                     PIC  9(02).
           02 WK-SDD                        PIC  9(02).
       01  WK-UYYMM.
           02 WK-UYY                        PIC  9(04).
           02 WK-UMM                        PIC  9(02).
       01  WK-CUYYMM.
           02 WK-CUYY                       PIC  9(04).
           02 WK-CUMM                       PIC  9(02).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD              PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD              PIC  X(07).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-TOKCD                 PIC  X(07).
           02 WSV2-KEY1.
              03 WSV2-TOKCD                 PIC  X(07).
       01  WK-STSUU                         PIC  9(02)  COMP-3.
       01  WK-INDEX.
           02 I                             PIC  9(02)  COMP-3.
           02 J                             PIC  9(02)  COMP-3.
           02 K                             PIC  9(02)  COMP-3.
       01  WK-SOEJI.
           02 WCT1-I                        PIC  9(02).
           02 WCT1-J                        PIC  9(02).
           02 WCT1-K                        PIC  9(02).
           02 WCT1-L                        PIC  9(02).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M10 : CONTROL                                               *
      *----------------------------------------------------------------*
           PERFORM M11-INIT THRU M11-EXIT.
           PERFORM M12-MAIN THRU M12-EXIT
                   UNTIL (WIN2-KEY = HIGH-VALUE).
           PERFORM M13-FINL THRU M13-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M11 : INITIAL                                               *
      *----------------------------------------------------------------*
       M11-INIT.
           OPEN INPUT  TMSFILE
                       NSMFILE
                I-O    URIFILE
                OUTPUT WKEFILE
                       WKFFILE.
           CALL 'VSBA0040' USING P04-PARM.
           PERFORM M111-DATA-CHK THRU M111-EXIT.
           PERFORM M112-READ-TMS THRU M112-EXIT.
           PERFORM M113-READ-URI THRU M113-EXIT.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M111 : DATA CHECK                                           *
      *----------------------------------------------------------------*
       M111-DATA-CHK.
           MOVE LOW-VALUE TO WK-SYSINS.
           ACCEPT WK-SYSINS.
           IF WK-SYSINS = LOW-VALUE
              GO TO M111-CNTL-L00.
           IF WK-SIMEB = '20'
              PERFORM M1111-NENGETU THRU M1111-EXIT
              GO TO M111-CNTL-CHK.
           IF WK-SIMEB = '15'
              PERFORM M1111-NENGETU THRU M1111-EXIT
              GO TO M111-CNTL-CHK.
           IF WK-SIMEB = '31'
              PERFORM M1111-NENGETU THRU M1111-EXIT
              GO TO M111-CNTL-CHK.
       M111-CNTL-L00.
           DISPLAY 'R VBKA1230 SIME-DATE ERROR'
           UPON CONSOLE.
           PERFORM M13-FINL THRU M13-EXIT.
           CALL 'VSBABEND'.
       M111-CNTL-CHK.
           MOVE SPACE    TO WK-TABLEX
           MOVE ZERO     TO I.
           MOVE ZERO     TO J.
           ACCEPT WK-SYSIN.
       M111-CNTL-L01.
           ADD  1  TO I.
           IF I     > 50
              IF WK-END = 'END'
                 MOVE 50  TO WK-STSUU
                 GO TO M111-EXIT
              ELSE
                 DISPLAY 'R VBKA1240 TABLE OVER-FLOW ERROR ?!'
                 UPON CONSOLE
                 PERFORM M13-FINL THRU M13-EXIT
                 CALL 'VSBABEND'
           ELSE
              IF WK-END = 'END'
              GO TO M111-CNTL-L04.
           MOVE WK-SITEK TO WK-TSITE (I).
       M111-CNTL-L02.
           ADD 1  TO  J.
           IF  J  > 20
              MOVE ZERO TO J
              GO TO M111-CNTL-L03.
           MOVE WK-SOKOK ( J ) TO WK-TSOKO ( I  J ).
           GO TO M111-CNTL-L02.
       M111-CNTL-L03.
           ACCEPT WK-SYSIN.
           GO TO M111-CNTL-L01.
       M111-CNTL-L04.
           COMPUTE WK-STSUU = I - 1.
       M111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1.1.1 : NENGETU SETTING                                  *
      *----------------------------------------------------------------*
       M1111-NENGETU.
           CALL 'VSBA2280' USING P228-PARM.
           MOVE P228-DATEX    TO WK-SYYMMDD.
           IF WK-SIMEB = '31'
              IF WK-SDD NOT < '25'
                 MOVE WK-SYYMM TO WK-UYYMM
              ELSE
                 MOVE WK-SYYMM TO WK-UYYMM
                 COMPUTE WK-UMM = WK-SMM - 1
                 IF WK-UMM = '00'
                    MOVE '12'  TO WK-UMM
                    COMPUTE WK-UYY = WK-SYY - 1.
           IF WK-SIMEB = '15' OR '20'
              MOVE WK-SYYMM  TO WK-UYYMM.
       M1111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1.2 : READ TMSFILE                                       *
      *----------------------------------------------------------------*
       M112-READ-TMS.
           MOVE 'Z '    TO   S-CODE.
           READ TMSFILE INTO TMS-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M112-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'M VBKA1230 TMSFILE DATA 0 KEN ?!'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE HIGH-VALUE TO WIN1-KEY
              GO TO M112-EXIT.
           MOVE TMS-TOKCD TO WIN1-TOKCD.
       M112-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1.3 : READ URIFILE                                       *
      *----------------------------------------------------------------*
       M113-READ-URI.
           READ URIFILE INTO URI-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M113-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA1230 URIFILE READ ERROR ?!  CODE ='
                       S-CODE  UPON CONSOLE
              PERFORM M13-FINL THRU M13-EXIT
              CALL 'VSBABEND'.
           MOVE URI-TOKCD TO WIN2-TOKCD.
       M113-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M12 : MAIN       (MATCHING)                                 *
      *----------------------------------------------------------------*
       M12-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M121-MATCH THRU M121-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M122-TMSONL THRU M122-EXIT
              ELSE
                 PERFORM M123-URIONL THRU M123-EXIT.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1 : MATCH                                              *
      *----------------------------------------------------------------*
       M121-MATCH.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M1211-MTPROC   THRU M1211-EXIT
                   UNTIL ( WIN2-KEY1 NOT = WSV2-KEY1 ).
       M121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1.1 : MATCHING PROCEDURE                               *
      *----------------------------------------------------------------*
       M1211-MTPROC.
           IF WK-SIMEB = '20'
              PERFORM M12111-SUZU-EDIT   THRU M12111-EXIT
              PERFORM M12112-URIS-KOSHIN THRU M12112-EXIT.
           IF TMS-SIMEB = WK-SIMEB
              PERFORM M12113-CHAN-EDIT   THRU M12113-EXIT
              PERFORM M12114-URIC-KOSHIN THRU M12114-EXIT
              GO TO M1211-L00.
           IF WK-SIMEB = '20'
              IF TMS-SIMEB NOT = '15'
                 IF TMS-SIMEB NOT = '31'
                    PERFORM M12113-CHAN-EDIT   THRU M12113-EXIT
                    PERFORM M12114-URIC-KOSHIN THRU M12114-EXIT.
       M1211-L00.
           PERFORM M12115-URI-REWRITE THRU M12115-EXIT.
       M1211-L01.
           PERFORM M113-READ-URI THRU M113-EXIT.
       M1211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1.1.1 : SUZUKEN SHIME DATA                             *
      *----------------------------------------------------------------*
       M12111-SUZU-EDIT.
           MOVE SPACE     TO WKE-R.
           MOVE SPACE     TO P05-PARM.
           MOVE '4'       TO P05-HNBCD.
           MOVE TMS-STNCD TO P05-STNNO.
           CALL 'VSBA0050' USING P05-PARM P04-PARM.
           IF P05-RTNCD = ZERO
              MOVE P05-STNCD TO WKE-STNKG
           ELSE
              MOVE SPACE     TO WKE-STNKG.
           MOVE SPACE     TO P05-PARM.
           MOVE '3'       TO P05-HNBCD.
           MOVE TMS-CKUNO TO P05-KANO.
           MOVE WKE-STNKG TO P05-STNCD.
           CALL 'VSBA0050' USING P05-PARM P04-PARM.
           IF P05-RTNCD = ZERO
              MOVE P05-SOKNO TO WKE-SOKNO
              PERFORM M121111-TBL-SEARCH THRU M121111-EXIT
           ELSE
              MOVE SPACE     TO WKE-SOKKG
              MOVE SPACE     TO WKE-SOKNO.
           MOVE URI-TOKCD    TO WKE-TOKCD.
           MOVE URI-NOSCD    TO WKE-NOSCD.
           MOVE WK-UYYMM     TO WKE-DYYMM.
           MOVE '20'         TO WKE-SHIME.
           MOVE TMS-STNCD    TO WKE-STNCD.
           MOVE TMS-CKUKG    TO WKE-CKUKG.
           MOVE TMS-TOKME    TO WKE-TOKME.
           IF URI-NOSCD NOT = SPACE
              IF URI-NOSCD NOT = '00'
                 PERFORM M121112-NSM-READ THRU M121112-EXIT
              ELSE
                 MOVE SPACE  TO WKE-NOSME
           ELSE
              MOVE SPACE     TO WKE-NOSME.
           MOVE TMS-SKUCD    TO WKE-SKUCD.
           MOVE URI-ZMURI    TO WKE-ZMURI.
           MOVE URI-ZMSEI    TO WKE-ZMSEI.
           MOVE URI-ICYO     TO WKE-ICHYO.
           COMPUTE WKE-NYKIN  = URI-TOGEN + URI-TOKOG + URI-TOFKO
                              + URI-TOFKA + URI-TOTEG + URI-TOSOS.
           COMPUTE WKE-ZKRIK  = URI-ZMSEI + URI-ICYO - WKE-NYKIN.
           MOVE URI-ZENME (1) TO WKE-TMET1.
           MOVE URI-ZENMN (1) TO WKE-TMNE1.
           MOVE URI-SZNME (1) TO WKE-SHET1.
           MOVE URI-SZNMN (1) TO WKE-SHNE1.
           COMPUTE WKE-TMURI  = URI-ZMURI      + URI-ICYO
                              - ( URI-TOGEN    + URI-TOKOG  +
                                  URI-TOFKO    + URI-TOFKA  +
                                  URI-TOTEG    + URI-TOSOS  )
                              + URI-TOET1      + URI-TONE1
                              + URI-TOET2      + URI-TONE2
                              + URI-STET1      + URI-STNE1
                              + URI-STET2      + URI-STNE2.
           COMPUTE WKE-SAKIG  = URI-ZENME (2)  + URI-ZENMN (2)
                              + URI-ZENME (3)  + URI-ZENMN (3)
                              + URI-ZENME (4)  + URI-ZENMN (4)
                              + URI-ZENME (5)  + URI-ZENMN (5)
                              + URI-ZENME (6)  + URI-ZENMN (6)
                              + URI-ZENME (7)  + URI-ZENMN (7)
                              + URI-ZENME (8)  + URI-ZENMN (8)
                              + URI-ZENME (9)  + URI-ZENMN (9)
                              + URI-ZENME (10) + URI-ZENMN (10)
                              + URI-ZENME (11) + URI-ZENMN (11)
                              + URI-ZENME (12) + URI-ZENMN (12)
                              + URI-SZNME (2)  + URI-SZNMN (2)
                              + URI-SZNME (3)  + URI-SZNMN (3)
                              + URI-SZNME (4)  + URI-SZNMN (4)
                              + URI-SZNME (5)  + URI-SZNMN (5)
                              + URI-SZNME (6)  + URI-SZNMN (6)
                              + URI-SZNME (7)  + URI-SZNMN (7)
                              + URI-SZNME (8)  + URI-SZNMN (8)
                              + URI-SZNME (9)  + URI-SZNMN (9)
                              + URI-SZNME (10) + URI-SZNMN (10)
                              + URI-SZNME (11) + URI-SZNMN (11)
                              + URI-SZNME (12) + URI-SZNMN (12).
           COMPUTE WKE-TMSEI  = WKE-TMURI - WKE-SAKIG.
           COMPUTE WKE-TMHAN  = URI-TOET1 + URI-TONE1 +
                                URI-TOET2 + URI-TONE2.
           COMPUTE WKE-TSHZE  = URI-STET1 + URI-STNE1 +
                                URI-STET2 + URI-STNE2.
           IF WKE-ZMURI NOT = ZERO OR
              WKE-ZMSEI NOT = ZERO OR
              WKE-ICHYO NOT = ZERO OR
              WKE-NYKIN NOT = ZERO OR
              WKE-ZKRIK NOT = ZERO OR
              WKE-TMET1 NOT = ZERO OR
              WKE-TMNE1 NOT = ZERO OR
              WKE-TMSEI NOT = ZERO OR
              WKE-SAKIG NOT = ZERO OR
              WKE-TMURI NOT = ZERO OR
              WKE-TMHAN NOT = ZERO OR
              WKE-SHET1 NOT = ZERO OR
              WKE-SHNE1 NOT = ZERO OR
              WKE-TSHZE NOT = ZERO
              WRITE WKE-REC FROM WKE-R.
       M12111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1.1.1.1 : TABLE SEARCH                                 *
      *----------------------------------------------------------------*
       M121111-TBL-SEARCH.
           MOVE ZERO   TO I.
           MOVE ZERO   TO J.
       M121111-TBL-L01.
           ADD  1  TO I.
           IF I  > 50
              GO TO M121111-TBL-L03.
           IF WKE-STNKG NOT = WK-TSITE ( I )
              GO TO M121111-TBL-L01.
       M121111-TBL-L02.
           ADD  1  TO J.
           IF J > 20 OR
              WK-TSOKO ( I  J ) = SPACE
              MOVE ZERO TO J
              GO TO M121111-TBL-L03.
           IF ( P05-SOKCD = WK-TSOKO ( I  J ) ) OR
              ( WK-TSOKO ( I  J ) = '*')
              MOVE P05-SOKCD TO WKE-SOKKG
              GO TO M121111-EXIT.
           GO TO M121111-TBL-L02.
       M121111-TBL-L03.
           MOVE 'A'     TO WKE-SOKKG.
       M121111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1.1.1.2 : NSMFILE SEARCH                               *
      *----------------------------------------------------------------*
       M121112-NSM-READ.
           IF URI-NOSCD     = SPACE
              MOVE SPACE   TO WKE-NOSME
              GO TO M121112-EXIT.
           IF URI-NOSCD     = '00'
              MOVE SPACE   TO WKE-NOSME
              GO TO M121112-EXIT.
           MOVE URI-TOKCD  TO KEY-NTOK.
           MOVE URI-NOSCD  TO KEY-NNOS.
           MOVE 'Z '       TO S-CODE.
           READ NSMFILE INTO NSM-R.
           IF S-CODE NOT = '00'
              MOVE SPACE   TO WKE-NOSME
              GO TO M121112-EXIT.
           MOVE NSM-NOSME  TO WKE-NOSME.
       M121112-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1.1.2   : SUZUKEN SIME URIFILE UPDATE                  *
      *----------------------------------------------------------------*
       M12112-URIS-KOSHIN.
           MOVE WK-UYYMM    TO WK-CUYYMM.
           IF WK-UMM     = '12'
              COMPUTE WK-CUYY = WK-UYY + 1
              MOVE '01'      TO WK-CUMM
           ELSE
              COMPUTE WK-CUMM = WK-UMM + 1.
           MOVE WK-CUYYMM   TO URI-DATYM.
           MOVE WKE-TMURI   TO URI-ZMURI.
           MOVE WKE-TMSEI   TO URI-ZMSEI.
           MOVE ZERO        TO URI-ICYO.
           MOVE ZERO        TO URI-TOGEN.
           MOVE ZERO        TO URI-TOKOG.
           MOVE ZERO        TO URI-TOFKO.
           MOVE ZERO        TO URI-TOFKA.
           MOVE ZERO        TO URI-TOTEG.
           MOVE ZERO        TO URI-TOSOS.
           MOVE ZERO        TO URI-TOET1.
           MOVE ZERO        TO URI-TONE1.
           MOVE ZERO        TO URI-TOET2.
           MOVE ZERO        TO URI-TONE2.
           COMPUTE URI-ZEET1 = URI-ZENME (2)   +  URI-ZENME (3)
                             + URI-ZENME (4)   +  URI-ZENME (5)
                             + URI-ZENME (6)   +  URI-ZENME (7)
                             + URI-ZENME (8)   +  URI-ZENME (9)
                             + URI-ZENME (10)  +  URI-ZENME (11)
                             + URI-ZENME (12).
           COMPUTE URI-ZENE1 = URI-ZENMN (2)   +  URI-ZENMN (3)
                             + URI-ZENMN (4)   +  URI-ZENMN (5)
                             + URI-ZENMN (6)   +  URI-ZENMN (7)
                             + URI-ZENMN (8)   +  URI-ZENMN (9)
                             + URI-ZENMN (10)  +  URI-ZENMN (11)
                             + URI-ZENMN (12).
           MOVE ZERO        TO URI-STET1.
           MOVE ZERO        TO URI-STNE1.
           MOVE ZERO        TO URI-STET2.
           MOVE ZERO        TO URI-STNE2.
           COMPUTE URI-SZET1 = URI-SZNME (2)   +  URI-SZNME (3)
                             + URI-SZNME (4)   +  URI-SZNME (5)
                             + URI-SZNME (6)   +  URI-SZNME (7)
                             + URI-SZNME (8)   +  URI-SZNME (9)
                             + URI-SZNME (10)  +  URI-SZNME (11)
                             + URI-SZNME (12).
           COMPUTE URI-SZNE1 = URI-SZNMN (2)   +  URI-SZNMN (3)
                             + URI-SZNMN (4)   +  URI-SZNMN (5)
                             + URI-SZNMN (6)   +  URI-SZNMN (7)
                             + URI-SZNMN (8)   +  URI-SZNMN (9)
                             + URI-SZNMN (10)  +  URI-SZNMN (11)
                             + URI-SZNMN (12).
           MOVE ZERO        TO J.
           MOVE 1           TO K.
       M12112-SET1.
           ADD  1   TO J.
           ADD  1   TO K.
           MOVE URI-ZENMN (K) TO URI-ZENMN (J).
           MOVE URI-ZENME (K) TO URI-ZENME (J).
           MOVE URI-SZNMN (K) TO URI-SZNMN (J).
           MOVE URI-SZNME (K) TO URI-SZNME (J).
           IF K      <  12
              GO TO M12112-SET1.
           MOVE ZERO          TO URI-ZENME (K).
           MOVE ZERO          TO URI-ZENMN (K).
           MOVE ZERO          TO URI-SZNME (K).
           MOVE ZERO          TO URI-SZNMN (K).
       M12112-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1.1.5 : URIFILE REWRITE                                *
      *----------------------------------------------------------------*
       M12115-URI-REWRITE.
           REWRITE URI-REC FROM URI-R.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA1230 URIFILE REWRITE ERROR ?!  CODE ='
                       S-CODE  UPON CONSOLE
              PERFORM M13-FINL THRU M13-EXIT
              CALL 'VSBABEND'.
       M12115-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1.1.3 : TOKUISAKI SIME DATA                            *
      *----------------------------------------------------------------*
       M12113-CHAN-EDIT.
           MOVE SPACE     TO WKE-R.
           MOVE SPACE     TO P05-PARM.
           MOVE '4'       TO P05-HNBCD.
           MOVE TMS-STNCD TO P05-STNNO.
           CALL 'VSBA0050' USING P05-PARM P04-PARM.
           IF P05-RTNCD = ZERO
              MOVE P05-STNCD TO WKE-STNKG
           ELSE
              MOVE SPACE     TO WKE-STNKG.
           MOVE SPACE     TO P05-PARM.
           MOVE '3'       TO P05-HNBCD.
           MOVE TMS-CKUNO TO P05-KANO.
           MOVE WKE-STNKG TO P05-STNCD.
           CALL 'VSBA0050' USING P05-PARM P04-PARM.
           IF P05-RTNCD = ZERO
              MOVE P05-SOKNO TO WKE-SOKNO
              PERFORM M121111-TBL-SEARCH THRU M121111-EXIT
           ELSE
              MOVE SPACE     TO WKE-SOKNO
              MOVE SPACE     TO WKE-SOKKG.
           MOVE URI-TOKCD    TO WKE-TOKCD.
           MOVE URI-NOSCD    TO WKE-NOSCD.
           MOVE WK-UYYMM     TO WKE-DYYMM.
           MOVE WK-SIMEB     TO WKE-SHIME.
           MOVE TMS-STNCD    TO WKE-STNCD.
           MOVE TMS-CKUKG    TO WKE-CKUKG.
           MOVE TMS-TOKME    TO WKE-TOKME.
           IF URI-NOSCD NOT = SPACE
              IF URI-NOSCD NOT = '00'
                 PERFORM M121112-NSM-READ THRU M121112-EXIT
              ELSE
                 MOVE SPACE  TO WKE-NOSME
           ELSE
              MOVE SPACE     TO WKE-NOSME.
           MOVE TMS-SKUCD    TO WKE-SKUCD.
           MOVE URI-CZURI    TO WKE-ZMURI.
           MOVE URI-CZSEI    TO WKE-ZMSEI.
           MOVE URI-CICYO    TO WKE-ICHYO.
           COMPUTE WKE-NYKIN  = URI-CTGEN + URI-CTKOG + URI-CTFKO
                              + URI-CTFKA + URI-CTTEG + URI-CTSOS.
           COMPUTE WKE-ZKRIK  = URI-CZSEI + URI-CICYO - WKE-NYKIN.
           MOVE URI-CZNME (1) TO WKE-TMET1.
           MOVE URI-CZNMN (1) TO WKE-TMNE1.
           MOVE URI-SCZME (1) TO WKE-SHET1.
           MOVE URI-SCZMN (1) TO WKE-SHNE1.
           COMPUTE WKE-TMSEI  = WKE-ZKRIK + WKE-TMET1 + WKE-TMNE1
                                          + WKE-SHET1 + WKE-SHNE1.
           COMPUTE WKE-SAKIG  = URI-CZNME (2)  + URI-CZNMN (2)
                              + URI-CZNME (3)  + URI-CZNMN (3)
                              + URI-CZNME (4)  + URI-CZNMN (4)
                              + URI-CZNME (5)  + URI-CZNMN (5)
                              + URI-CZNME (6)  + URI-CZNMN (6)
                              + URI-CZNME (7)  + URI-CZNMN (7)
                              + URI-CZNME (8)  + URI-CZNMN (8)
                              + URI-CZNME (9)  + URI-CZNMN (9)
                              + URI-CZNME (10) + URI-CZNMN (10)
                              + URI-CZNME (11) + URI-CZNMN (11)
                              + URI-CZNME (12) + URI-CZNMN (12)
                              + URI-SCZME (2)  + URI-SCZMN (2)
                              + URI-SCZME (3)  + URI-SCZMN (3)
                              + URI-SCZME (4)  + URI-SCZMN (4)
                              + URI-SCZME (5)  + URI-SCZMN (5)
                              + URI-SCZME (6)  + URI-SCZMN (6)
                              + URI-SCZME (7)  + URI-SCZMN (7)
                              + URI-SCZME (8)  + URI-SCZMN (8)
                              + URI-SCZME (9)  + URI-SCZMN (9)
                              + URI-SCZME (10) + URI-SCZMN (10)
                              + URI-SCZME (11) + URI-SCZMN (11)
                              + URI-SCZME (12) + URI-SCZMN (12).
           COMPUTE WKE-TMURI  = WKE-TMSEI + WKE-SAKIG.
           COMPUTE WKE-TMHAN  = URI-CTET1 + URI-CTNE1 +
                                URI-CTET2 + URI-CTNE2.
           COMPUTE WKE-TSHZE  = URI-SCET1 + URI-SCNE1 +
                                URI-SCET2 + URI-SCNE2.
      *    IF WKE-ZMURI NOT = ZERO OR
      *       WKE-ZMSEI NOT = ZERO OR
      *       WKE-ICHYO NOT = ZERO OR
      *       WKE-NYKIN NOT = ZERO OR
      *       WKE-ZKRIK NOT = ZERO OR
      *       WKE-TMET1 NOT = ZERO OR
      *       WKE-TMNE1 NOT = ZERO OR
      *       WKE-TMSEI NOT = ZERO OR
      *       WKE-SAKIG NOT = ZERO OR
      *       WKE-TMURI NOT = ZERO OR
      *       WKE-TMHAN NOT = ZERO OR
      *       WKE-SHET1 NOT = ZERO OR
      *       WKE-SHNE1 NOT = ZERO OR
      *       WKE-TSHZE NOT = ZERO
              WRITE WKF-REC FROM WKE-R.
       M12113-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1.1.4 : TOKUISAKI SIME URIFILE UPDATE                  *
      *----------------------------------------------------------------*
       M12114-URIC-KOSHIN.
           MOVE WK-UYYMM    TO WK-CUYYMM.
           IF WK-UMM     = '12'
              COMPUTE WK-CUYY = WK-UYY + 1
              MOVE '01'      TO WK-CUMM
           ELSE
              COMPUTE WK-CUMM = WK-UMM + 1.
           MOVE WK-CUYYMM   TO URI-CDATY.
           MOVE WKE-TMURI   TO URI-CZURI.
           MOVE WKE-TMSEI   TO URI-CZSEI.
           MOVE ZERO        TO URI-CICYO.
           MOVE ZERO        TO URI-CTGEN.
           MOVE ZERO        TO URI-CTKOG.
           MOVE ZERO        TO URI-CTFKO.
           MOVE ZERO        TO URI-CTFKA.
           MOVE ZERO        TO URI-CTTEG.
           MOVE ZERO        TO URI-CTSOS.
           MOVE ZERO        TO URI-CTET1.
           MOVE ZERO        TO URI-CTNE1.
           MOVE ZERO        TO URI-CTET2.
           MOVE ZERO        TO URI-CTNE2.
           COMPUTE URI-CZET1 = URI-CZNME (2)   +  URI-CZNME (3)
                             + URI-CZNME (4)   +  URI-CZNME (5)
                             + URI-CZNME (6)   +  URI-CZNME (7)
                             + URI-CZNME (8)   +  URI-CZNME (9)
                             + URI-CZNME (10)  +  URI-CZNME (11)
                             + URI-CZNME (12).
           COMPUTE URI-CZNE1 = URI-CZNMN (2)   +  URI-CZNMN (3)
                             + URI-CZNMN (4)   +  URI-CZNMN (5)
                             + URI-CZNMN (6)   +  URI-CZNMN (7)
                             + URI-CZNMN (8)   +  URI-CZNMN (9)
                             + URI-CZNMN (10)  +  URI-CZNMN (11)
                             + URI-CZNMN (12).
           MOVE ZERO        TO URI-SCET1.
           MOVE ZERO        TO URI-SCNE1.
           MOVE ZERO        TO URI-SCET2.
           MOVE ZERO        TO URI-SCNE2.
           COMPUTE URI-SCZE1 = URI-SCZME (2)   +  URI-SCZME (3)
                             + URI-SCZME (4)   +  URI-SCZME (5)
                             + URI-SCZME (6)   +  URI-SCZME (7)
                             + URI-SCZME (8)   +  URI-SCZME (9)
                             + URI-SCZME (10)  +  URI-SCZME (11)
                             + URI-SCZME (12).
           COMPUTE URI-SCZN1 = URI-SCZMN (2)   +  URI-SCZMN (3)
                             + URI-SCZMN (4)   +  URI-SCZMN (5)
                             + URI-SCZMN (6)   +  URI-SCZMN (7)
                             + URI-SCZMN (8)   +  URI-SCZMN (9)
                             + URI-SCZMN (10)  +  URI-SCZMN (11)
                             + URI-SCZMN (12).
           MOVE ZERO        TO J.
           MOVE 1           TO K.
       M12114-SET1.
           ADD  1   TO J.
           ADD  1   TO K.
           MOVE URI-CZNMN (K) TO URI-CZNMN (J).
           MOVE URI-CZNME (K) TO URI-CZNME (J).
           MOVE URI-SCZMN (K) TO URI-SCZMN (J).
           MOVE URI-SCZME (K) TO URI-SCZME (J).
           IF K      <  12
              GO TO M12114-SET1.
           MOVE ZERO          TO URI-CZNME (K).
           MOVE ZERO          TO URI-CZNMN (K).
           MOVE ZERO          TO URI-SCZME (K).
           MOVE ZERO          TO URI-SCZMN (K).
       M12114-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.2 : TMSFILE ONLY                                       *
      *----------------------------------------------------------------*
       M122-TMSONL.
           PERFORM M112-READ-TMS THRU M112-EXIT.
       M122-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.3 : URIFILE ONLY                                       *
      *----------------------------------------------------------------*
       M123-URIONL.
           IF WK-SIMEB = '20'
              PERFORM M1231-HAISHI THRU M1231-EXIT.
           PERFORM M113-READ-URI THRU M113-EXIT.
       M123-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.3.1 : HAISHI                                           *
      *----------------------------------------------------------------*
       M1231-HAISHI.
           IF URI-ZMURI NOT = ZERO
              GO TO M1231-L02.
           IF URI-ZMSEI NOT = ZERO
              GO TO M1231-L02.
           IF URI-ICYO  NOT = ZERO
              GO TO M1231-L02.
           IF URI-TOGEN NOT = ZERO
              GO TO M1231-L02.
           IF URI-TOKOG NOT = ZERO
              GO TO M1231-L02.
           IF URI-TOFKO NOT = ZERO
              GO TO M1231-L02.
           IF URI-TOFKA NOT = ZERO
              GO TO M1231-L02.
           IF URI-TOTEG NOT = ZERO
              GO TO M1231-L02.
           IF URI-TOSOS NOT = ZERO
              GO TO M1231-L02.
           IF URI-TOET1 NOT = ZERO
              GO TO M1231-L02.
           IF URI-TONE1 NOT = ZERO
              GO TO M1231-L02.
           IF URI-TOET2 NOT = ZERO
              GO TO M1231-L02.
           IF URI-TONE2 NOT = ZERO
              GO TO M1231-L02.
           IF URI-ZEET1 NOT = ZERO
              GO TO M1231-L02.
           IF URI-ZENE1 NOT = ZERO
              GO TO M1231-L02.
           IF URI-STET1 NOT = ZERO
              GO TO M1231-L02.
           IF URI-STNE1 NOT = ZERO
              GO TO M1231-L02.
           IF URI-STET2 NOT = ZERO
              GO TO M1231-L02.
           IF URI-STNE2 NOT = ZERO
              GO TO M1231-L02.
           IF URI-SZET1 NOT = ZERO
              GO TO M1231-L02.
           IF URI-SZNE1 NOT = ZERO
              GO TO M1231-L02.
           MOVE ZERO TO I.
       M1231-L01.
           ADD 1 TO I
           IF URI-ZENME (I) NOT = ZERO
              GO TO M1231-L02.
           IF URI-ZENMN (I) NOT = ZERO
              GO TO M1231-L02.
           IF URI-SZNME (I) NOT = ZERO
              GO TO M1231-L02.
           IF URI-SZNMN (I) NOT = ZERO
              GO TO M1231-L02.
           IF I  < 12
              GO TO M1231-L01.
           GO TO M1231-EXIT.
       M1231-L02.
           PERFORM M12311-HAISHI-SYORI1 THRU M12311-EXIT.
           PERFORM M12312-HAISHI-SYORI2 THRU M12312-EXIT.
       M1231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.3.1.1 : HAISHI SYORI-1                                 *
      *----------------------------------------------------------------*
       M12311-HAISHI-SYORI1.
           MOVE SPACE        TO WKE-R.
           MOVE URI-SITEK    TO WKE-STNKG.
           MOVE 'A'          TO WKE-SOKKG.
           MOVE URI-TOKCD    TO WKE-TOKCD.
           MOVE URI-NOSCD    TO WKE-NOSCD.
           MOVE WK-UYYMM     TO WKE-DYYMM.
           MOVE '20'         TO WKE-SHIME.
           MOVE SPACE        TO WKE-STNCD.
           MOVE SPACE        TO WKE-CKUKG.
           MOVE SPACE        TO WKE-TOKME.
           MOVE SPACE        TO WKE-NOSME.
           MOVE URI-TOKCD    TO WKE-SKUCD.
           MOVE URI-ZMURI    TO WKE-ZMURI.
           MOVE URI-ZMSEI    TO WKE-ZMSEI.
           MOVE URI-ICYO     TO WKE-ICHYO.
           COMPUTE WKE-NYKIN  = URI-TOGEN + URI-TOKOG + URI-TOFKO
                              + URI-TOFKA + URI-TOTEG + URI-TOSOS.
           COMPUTE WKE-ZKRIK  = URI-ZMSEI + URI-ICYO - WKE-NYKIN.
           MOVE URI-ZENME (1) TO WKE-TMET1.
           MOVE URI-ZENMN (1) TO WKE-TMNE1.
           MOVE URI-SZNME (1) TO WKE-SHET1.
           MOVE URI-SZNMN (1) TO WKE-SHNE1.
           COMPUTE WKE-TMURI  = URI-ZMURI      + URI-ICYO
                              - ( URI-TOGEN    + URI-TOKOG  +
                                  URI-TOFKO    + URI-TOFKA  +
                                  URI-TOTEG    + URI-TOSOS  )
                              + URI-TOET1      + URI-TONE1
                              + URI-TOET2      + URI-TONE2
                              + URI-STET1      + URI-STNE1
                              + URI-STET2      + URI-STNE2.
           COMPUTE WKE-SAKIG  = URI-ZENME (2)  + URI-ZENMN (2)
                              + URI-ZENME (3)  + URI-ZENMN (3)
                              + URI-ZENME (4)  + URI-ZENMN (4)
                              + URI-ZENME (5)  + URI-ZENMN (5)
                              + URI-ZENME (6)  + URI-ZENMN (6)
                              + URI-ZENME (7)  + URI-ZENMN (7)
                              + URI-ZENME (8)  + URI-ZENMN (8)
                              + URI-ZENME (9)  + URI-ZENMN (9)
                              + URI-ZENME (10) + URI-ZENMN (10)
                              + URI-ZENME (11) + URI-ZENMN (11)
                              + URI-ZENME (12) + URI-ZENMN (12)
                              + URI-SZNME (2)  + URI-SZNMN (2)
                              + URI-SZNME (3)  + URI-SZNMN (3)
                              + URI-SZNME (4)  + URI-SZNMN (4)
                              + URI-SZNME (5)  + URI-SZNMN (5)
                              + URI-SZNME (6)  + URI-SZNMN (6)
                              + URI-SZNME (7)  + URI-SZNMN (7)
                              + URI-SZNME (8)  + URI-SZNMN (8)
                              + URI-SZNME (9)  + URI-SZNMN (9)
                              + URI-SZNME (10) + URI-SZNMN (10)
                              + URI-SZNME (11) + URI-SZNMN (11)
                              + URI-SZNME (12) + URI-SZNMN (12).
           COMPUTE WKE-TMSEI  = WKE-TMURI - WKE-SAKIG.
           COMPUTE WKE-TMHAN  = URI-TOET1 + URI-TONE1 +
                                URI-TOET2 + URI-TONE2.
           COMPUTE WKE-TSHZE  = URI-STET1 + URI-STNE1 +
                                URI-STET2 + URI-STNE2.
           MOVE SPACE     TO P05-PARM.
           MOVE '1'       TO P05-HNBCD.
           MOVE URI-SITEK TO P05-STNCD.
           MOVE 'A'       TO P05-SOKCD.
           CALL 'VSBA0050' USING P05-PARM P04-PARM.
           IF P05-RTNCD = ZERO
              MOVE P05-SOKNO TO WKE-SOKNO
           ELSE
              MOVE SPACE     TO WKE-SOKNO.
           WRITE WKE-REC FROM WKE-R.
       M12311-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.3.1.2 : HAISHI SYORI-2                                 *
      *----------------------------------------------------------------*
       M12312-HAISHI-SYORI2.
           PERFORM M12112-URIS-KOSHIN THRU M12112-EXIT.
           MOVE WK-UYYMM    TO WK-CUYYMM.
           IF WK-UMM     = '12'
              COMPUTE WK-CUYY = WK-UYY + 1
              MOVE '01'      TO WK-CUMM
           ELSE
              COMPUTE WK-CUMM = WK-UMM + 1.
           MOVE WK-CUYYMM   TO URI-DATYM.
           MOVE WK-CUYYMM   TO URI-CDATY.
           MOVE URI-ZMURI   TO URI-CZURI.
           MOVE URI-ZMSEI   TO URI-CZSEI.
           MOVE URI-ICYO    TO URI-CICYO.
           MOVE URI-TONEW   TO URI-CTNEW.
           MOVE URI-TOHAT   TO URI-CTHAT.
           MOVE URI-TOHAZ   TO URI-CTHAZ.
           MOVE URI-ZEHAZ   TO URI-CZHAZ.
           MOVE URI-ZETRN   TO URI-CZTRN.
           MOVE URI-STHAT   TO URI-SCHAT.
           MOVE URI-STHAZ   TO URI-SCHAZ.
           MOVE URI-SZHAZ   TO URI-SCZHA.
           MOVE URI-SZTRN   TO URI-SCZTR.
           PERFORM M12115-URI-REWRITE THRU M12115-EXIT.
       M12312-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.3 : FINAL                                                *
      *----------------------------------------------------------------*
       M13-FINL.
           CLOSE TMSFILE
                 URIFILE
                 NSMFILE
                 WKEFILE
                 WKFFILE.
       M13-EXIT.
           EXIT.
