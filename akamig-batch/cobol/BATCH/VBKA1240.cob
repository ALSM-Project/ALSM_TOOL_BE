./ ADD NAME=VBKA1240  01.06 20021022 20021023 102858 00478 00477 MCSK034
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1240'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : KAISYU  SYSTEM                *
      *              <> PG.OUTLINE     : MATCHING ( 1 : N )-CNTBK      *
      *                                                                *
      *                 * INPUT FILE   : TOKUISAKI MASTER   (TMSFILE)  *
      *                 * INPUT FILE   : NOSEI MASTER       (NSMFILE)  *
      *                 * INPUT FILE   : TOKUISAKI SIMEB               *
      *                                    URIKAKE-KIN DATA (DWKD)     *
      *                 * OUTPUT FILE  : SHITEN SEIKYUSYO              *
      *                                       UCHIWAKE DATA (DWKA)     *
      *                                                                *
      *              <> AUTHOR         : SUZUKEN-Y.TAKEUZI             *
      *              <> DATE           : 86/08/11                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) Ä¸²»·¥Ò² & É³¾²»·¥Ò² ÍÝº³ : 86/10/22  SUZUKEN-Y.ITO       *
      * (02) TOKME HENSYUU HENKOU      : 86/12/18  CSK-T.KOZAWA        *
      * (03) SYOHIZEI TUIKA       Â²¶  : 89/03/06  CSK-S.MUNEISHI      *
      * (04) Z-175             ØºÝÊß²Ù : 96/08/12  CSK-C.FURUTA        *
      * (05) K-261                ÍÝº³ : 97/09/26  CSK-N.UEMURA        *
      * (06) K-275C               ÍÝº³ : 98/04/27  CSK-R.TAKAYANAGI    *
      * (07) H-010  T-2793        ÍÝº³ : 02/10/22  CSK-T.YAMAGUCHI     *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TMSFILE ASSIGN    TO TMSFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE SEQUENTIAL
                          RECORD   KEY KEY-TMS
                          FILE  STATUS S-CODE.
           SELECT NSMFILE ASSIGN    TO NSMFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE RANDOM
                          RECORD   KEY KEY-NSM
                          FILE  STATUS S-CODE.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
       DATA DIVISION.
       FILE SECTION.
       FD  TMSFILE
           LABEL RECORD STANDARD.
       01  TMS-REC.
           02 KEY-TMS                       PIC  X(07).
           02 FILLER                        PIC  X(293).
       FD  NSMFILE
           LABEL RECORD STANDARD.
       01  NSM-REC.
           02 KEY-NSM.
              03 KEY-NTOK                   PIC  X(07).
              03 KEY-NNOS                   PIC  X(02).
           02 FILLER                        PIC  X(91).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(200).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(280).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  TMS-R.
           COPY VFILTMS SUPPRESS.
       01  NSM-R.
           COPY VFILNSM SUPPRESS.
       01  P04-PARM.
           COPY VPARB04.
       01  P05-PARM.
           COPY VPARB05.
       01  PC0-PARM.
           COPY VPARBC0.
       01  WKD-R.
           02 WKD-STNKG                     PIC  X(01).
           02 WKD-SOKKG                     PIC  X(01).
           02 WKD-TOKCD                     PIC  X(07).
           02 WKD-NOSCD                     PIC  X(02).
           02 WKD-DATE.
              03 WKD-DYYMM                  PIC  9(06).
              03 WKD-SIMEB                  PIC  9(02).
           02 WKD-STNCD                     PIC  X(03).
           02 WKD-CKUKG                     PIC  X(05).
           02 WKD-TOKME                     PIC  X(25).
           02 WKD-NOSME                     PIC  X(25).
           02 WKD-SEKCD                     PIC  X(07).
           02 WKD-ZMURI                     PIC S9(13)  COMP-3.
           02 WKD-ZMSEI                     PIC S9(13)  COMP-3.
           02 WKD-ICHYO                     PIC S9(13)  COMP-3.
           02 WKD-NYKIN                     PIC S9(13)  COMP-3.
           02 WKD-ZMKRI                     PIC S9(13)  COMP-3.
           02 WKD-TMKIR.
              03 WKD-TMET1                  PIC S9(13)  COMP-3.
              03 WKD-TMNE1                  PIC S9(13)  COMP-3.
           02 WKD-TMSEI                     PIC S9(13)  COMP-3.
           02 WKD-SAKIG                     PIC S9(13)  COMP-3.
           02 WKD-TMURI                     PIC S9(13)  COMP-3.
           02 WKD-TMHAN                     PIC S9(13)  COMP-3.
           02 WKD-SHZEI.
              03 WKD-SHET1                  PIC S9(13)  COMP-3.
              03 WKD-SHNE1                  PIC S9(13)  COMP-3.
           02 WKD-TSHZE                     PIC S9(13)  COMP-3.
           02 FILLER                        PIC  X(18).
       01  WKA-R.
           02 WKA-SITEN                     PIC  X(01).
           02 WKA-SESOK                     PIC  X(01).
           02 WKA-SETIK.
              03 WKA-SEKAN                  PIC  X(03).
              03 WKA-SEKAK                  PIC  X(02).
           02 WKA-SEKCD                     PIC  X(07).
           02 WKA-SIMEB                     PIC  9(02).
           02 WKA-SENOS                     PIC  X(02).
           02 WKA-SETNM                     PIC  X(30).
           02 WKA-SENNM                     PIC  X(30).
           02 WKA-SEADD.
              03 WKA-SEADA                  PIC  X(40).
              03 WKA-SEADB                  PIC  X(30).
              03 WKA-SEADN                  PIC  X(30).
           02 WKA-POSNO                     PIC  X(07).
           02 WKA-SIYMD.
              03 WKA-SIYM                   PIC  9(06).
              03 WKA-SIDD                   PIC  9(02).
           02 WKA-SEKBN                     PIC  X(01).
           02 WKA-ZMSEI                     PIC S9(13)  COMP-3.
           02 WKA-NYKIN                     PIC S9(13)  COMP-3.
           02 WKA-TMNE1                     PIC S9(13)  COMP-3.
           02 WKA-ZMKRI                     PIC S9(13)  COMP-3.
           02 WKA-TMHAN                     PIC S9(13)  COMP-3.
           02 WKA-TMSEI                     PIC S9(13)  COMP-3.
           02 WKA-SAKIG                     PIC S9(13)  COMP-3.
           02 WKA-TMURI                     PIC S9(13)  COMP-3.
           02 WKA-ICHYO                     PIC S9(13)  COMP-3.
           02 WKA-SHZNE                     PIC S9(13)  COMP-3.
           02 WKA-TSHZE                     PIC S9(13)  COMP-3.
           02 WKA-THANG                     PIC S9(13)  COMP-3.
           02 FILLER                        PIC  X(02).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-KEY2.
                    05 WIN1-TOKCD           PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-KEY2.
                    05 WIN2-SEKCD           PIC  X(07).
                 04 WIN2-NOSCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-KEY2.
                 04 WSV1-TOKCD              PIC  X(07).
           02 WSV2-KEY1.
              03 WSV2-KEY2.
                 04 WSV2-TOKCD              PIC  X(07).
              03 WSV2-NOSCD                 PIC  X(02).
           02 WSV2-DATA.
              03 WSV2-SEKCD.
                 04 WSV2-SKCD1              PIC  X(01).
                 04 FILLER                  PIC  X(06).
              03 WSV2-SIMEB                 PIC  X(02).
              03 WSV2-DATE                  PIC  X(06).
       01  TOTAL-AREA.
           02 WTT1-ZMSEI                    PIC S9(13)  COMP-3.
           02 WTT1-NYKIN                    PIC S9(13)  COMP-3.
           02 WTT1-TMNE1                    PIC S9(13)  COMP-3.
           02 WTT1-ZMKRI                    PIC S9(13)  COMP-3.
           02 WTT1-TMHAN                    PIC S9(13)  COMP-3.
           02 WTT1-TMSEI                    PIC S9(13)  COMP-3.
           02 WTT1-SAKIG                    PIC S9(13)  COMP-3.
           02 WTT1-TMURI                    PIC S9(13)  COMP-3.
           02 WTT1-ICHYO                    PIC S9(13)  COMP-3.
           02 WTT1-SHZNE                    PIC S9(13)  COMP-3.
           02 WTT1-TSHZE                    PIC S9(13)  COMP-3.
       01  WK-SYSIN.
           02 WK-SITEK                      PIC  X(01).
           02 FILLER                        PIC  X(01).
           02 WK-SOKOK1        OCCURS   60 .
              03 WK-SOKOK                   PIC  X(01).
           02 FILLER                        PIC  X(18).
       01  WK-SYSINE  REDEFINES WK-SYSIN.
           02 WK-END                        PIC  X(03).
           02 FILLER                        PIC  X(77).
       01  WK-TABLEX.
           02 WK-TABLE         OCCURS   50.
              03 WK-TSITE                   PIC  X(01).
              03 WK-TSOKOX     OCCURS   60.
                 04 WK-TSOKO                PIC  X(01).
       01  WK-STSUU                         PIC  9(02)  COMP-3.
       01  WK-INDEX.
           02 I                             PIC  9(02)  COMP-3.
           02 J                             PIC  9(02)  COMP-3.
           02 K                             PIC  9(02)  COMP-3.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE OR
                          WIN2-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  TMSFILE
                       NSMFILE
                       WKDFILE
                OUTPUT WKAFILE.
           CALL 'VSBA0040' USING  P04-PARM.
           PERFORM M11-READ-SYS THRU M11-EXIT.
           PERFORM M12-READ-TMS THRU M12-EXIT.
           PERFORM M13-READ-WKD THRU M13-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ SYSIN                                           *
      *----------------------------------------------------------------*
       M11-READ-SYS.
           MOVE SPACE    TO WK-TABLEX.
           MOVE ZERO     TO I.
           MOVE ZERO     TO J.
           ACCEPT WK-SYSIN FROM SYSIN.
       M11-L01.
           ADD  1  TO I.
           IF I     > 50
              IF WK-END = 'END'
                 MOVE 50  TO WK-STSUU
                 GO TO M11-EXIT
              ELSE
                 DISPLAY 'R VBKA1240 TABLE OVER-FLOW ERROR ?!'
                 UPON CONSOLE
                 PERFORM M3-FINL THRU M3-EXIT
                 CALL 'VSBABEND'
           ELSE
              IF WK-END = 'END'
                 GO TO M11-L04.
           MOVE WK-SITEK TO WK-TSITE (I).
       M11-L02.
           ADD 1  TO  J.
           IF  J  > 60
              MOVE ZERO TO J
              GO TO M11-L03.
           MOVE WK-SOKOK ( J ) TO WK-TSOKO ( I  J ).
           GO TO M11-L02.
       M11-L03.
           ACCEPT WK-SYSIN FROM SYSIN.
           GO TO M11-L01.
       M11-L04.
           COMPUTE WK-STSUU = I - 1.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ TMSFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-TMS.
           MOVE 'Z '    TO   S-CODE.
           READ TMSFILE INTO TMS-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M12-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'M VBKA1240 TMSFILE DATA 0 KEN ?!'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE HIGH-VALUE TO WIN1-KEY
              GO TO M12-EXIT.
           MOVE TMS-TOKCD TO WIN1-TOKCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKDFILE                                         *
      *----------------------------------------------------------------*
       M13-READ-WKD.
           READ WKDFILE INTO WKD-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M13-EXIT.
           MOVE WKD-SEKCD TO WIN2-SEKCD.
           MOVE WKD-NOSCD TO WIN2-NOSCD.
       M13-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        (MATCHING)                                 *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY2 = WIN2-KEY2
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY2 < WIN2-KEY2
                 PERFORM M22-TMSONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKDONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY2 TO WSV2-KEY2.
           PERFORM M211-TOTAL THRU M211-EXIT
                   UNTIL WIN2-KEY2 NOT = WSV2-KEY2.
           PERFORM M12-READ-TMS THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : SURYO TANKA TOTAL                                  *
      *----------------------------------------------------------------*
       M211-TOTAL.
           MOVE WKD-NOSCD TO WSV2-NOSCD.
           MOVE WKD-SEKCD TO WSV2-SEKCD.
           MOVE WKD-SIMEB TO WSV2-SIMEB.
           MOVE WKD-DATE  TO WSV2-DATE.
           MOVE ZERO      TO WTT1-ZMSEI.
           MOVE ZERO      TO WTT1-NYKIN.
           MOVE ZERO      TO WTT1-TMNE1.
           MOVE ZERO      TO WTT1-ZMKRI.
           MOVE ZERO      TO WTT1-TMHAN.
           MOVE ZERO      TO WTT1-TMSEI.
           MOVE ZERO      TO WTT1-SAKIG.
           MOVE ZERO      TO WTT1-TMURI.
           MOVE ZERO      TO WTT1-ICHYO.
           MOVE ZERO      TO WTT1-SHZNE.
           MOVE ZERO      TO WTT1-TSHZE.
           PERFORM M2111-TOTAL THRU M2111-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
           MOVE SPACE     TO WKA-R.
           MOVE SPACE     TO P05-PARM.
           MOVE '4'       TO P05-HNBCD.
           MOVE TMS-STNCD TO P05-STNNO.
           CALL 'VSBA0050' USING P05-PARM P04-PARM.
           IF P05-RTNCD = ZERO
              MOVE P05-STNCD TO WKA-SITEN
           ELSE
              MOVE SPACE     TO WKA-SITEN.
           MOVE SPACE     TO P05-PARM.
           MOVE '3'       TO P05-HNBCD.
           MOVE TMS-CKUNO TO P05-KANO.
           MOVE WKA-SITEN TO P05-STNCD.
           CALL 'VSBA0050' USING P05-PARM P04-PARM.
           IF P05-RTNCD = ZERO
              PERFORM M212-TBL-SEARCH THRU M212-EXIT
           ELSE
              MOVE 'A'     TO WKA-SESOK.
           MOVE TMS-CKUKG  TO WKA-SETIK.
           MOVE WSV2-SEKCD TO WKA-SEKCD.
           MOVE WSV2-SIMEB TO WKA-SIMEB.
           MOVE WSV2-NOSCD TO WKA-SENOS.
           IF ( TMS-INKAN  =  1 )    OR
              ( WSV2-NOSCD = SPACE ) OR
              ( WSV2-SKCD1 = '8' )
              MOVE SPACE      TO WKA-SETNM
           ELSE
              MOVE TMS-TOKKA  TO WKA-SETNM.
           PERFORM M213-READ-NSM THRU M213-EXIT.
           MOVE SPACE      TO WKA-SEADD.
           MOVE SPACE      TO WKA-POSNO.
           IF WSV2-SIMEB = '31'
              MOVE SPACE      TO PC0-PARM
              MOVE WSV2-DATE  TO PC0-RYMDS8
              CALL 'VSBA1200' USING PC0-PARM
              MOVE PC0-RYMDS8 TO WKA-SIYMD
           ELSE
              MOVE WSV2-DATE  TO WKA-SIYM
              MOVE WSV2-SIMEB TO WKA-SIDD.
           MOVE TMS-SEYOS  TO WKA-SEKBN.
           COMPUTE WKA-ZMSEI = WTT1-ZMSEI + WTT1-ICHYO.
           COMPUTE WKA-THANG = WTT1-TMHAN + WTT1-TSHZE.
           MOVE WTT1-NYKIN TO WKA-NYKIN.
           MOVE WTT1-TMNE1 TO WKA-TMNE1.
           MOVE WTT1-ZMKRI TO WKA-ZMKRI.
           MOVE WTT1-TMHAN TO WKA-TMHAN.
           MOVE WTT1-TMSEI TO WKA-TMSEI.
           MOVE WTT1-SAKIG TO WKA-SAKIG.
           MOVE WTT1-TMURI TO WKA-TMURI.
           MOVE WTT1-ICHYO TO WKA-ICHYO.
           MOVE WTT1-SHZNE TO WKA-SHZNE.
           MOVE WTT1-TSHZE TO WKA-TSHZE.
           WRITE WKA-REC FROM WKA-R.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.1: TOTAL                                             *
      *----------------------------------------------------------------*
       M2111-TOTAL.
           COMPUTE WTT1-ZMSEI = WTT1-ZMSEI + WKD-ZMSEI.
           COMPUTE WTT1-NYKIN = WTT1-NYKIN + WKD-NYKIN.
           COMPUTE WTT1-TMNE1 = WTT1-TMNE1 + WKD-TMNE1.
           COMPUTE WTT1-ZMKRI = WTT1-ZMKRI + WKD-ZMKRI.
           COMPUTE WTT1-TMHAN = WTT1-TMHAN + WKD-TMET1.
           COMPUTE WTT1-TMSEI = WTT1-TMSEI + WKD-TMSEI.
           COMPUTE WTT1-SAKIG = WTT1-SAKIG + WKD-SAKIG.
           COMPUTE WTT1-TMURI = WTT1-TMURI + WKD-TMURI.
           COMPUTE WTT1-ICHYO = WTT1-ICHYO + WKD-ICHYO.
           COMPUTE WTT1-SHZNE = WTT1-SHZNE + WKD-SHNE1.
           COMPUTE WTT1-TSHZE = WTT1-TSHZE + WKD-SHET1.
           PERFORM M13-READ-WKD THRU M13-EXIT.
       M2111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : SYSIN TABLE SEARCH                                 *
      *----------------------------------------------------------------*
       M212-TBL-SEARCH.
           MOVE 0      TO I.
           MOVE 0      TO J.
       M212-L01.
           ADD 1    TO I.
           IF I  > 50
              GO TO M212-L03.
           IF WKA-SITEN  NOT = WK-TSITE ( I )
              GO TO M212-L01.
       M212-L02.
           ADD  1   TO J.
           IF J  > 60 OR
              WK-TSOKO ( I J ) = SPACE
              MOVE ZERO TO J
              GO TO M212-L03.
           IF ( P05-SOKCD = WK-TSOKO ( I J ) ) OR
              ( WK-TSOKO ( I J ) = '*' )
              MOVE P05-SOKCD TO WKA-SESOK
              GO TO M212-EXIT.
           GO TO M212-L02.
       M212-L03.
           MOVE 'A' TO WKA-SESOK.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.3 : NSM SEARCH                                         *
      *----------------------------------------------------------------*
       M213-READ-NSM.
           IF TMS-INKAN  =  1
              MOVE SPACE      TO WKA-SENNM
           ELSE
              IF WSV2-NOSCD  =  (  SPACE OR '00'  )
                 MOVE TMS-TOKKA  TO WKA-SENNM
              ELSE
                 MOVE WSV2-SEKCD TO KEY-NTOK
                 MOVE WSV2-NOSCD TO KEY-NNOS
                 MOVE 'Z '       TO S-CODE
                 READ NSMFILE  INTO NSM-R
                 IF S-CODE NOT   = ZERO
                    MOVE SPACE     TO WKA-SETNM
                    MOVE TMS-TOKKA TO WKA-SENNM
                 ELSE
                    MOVE NSM-NOSKA TO WKA-SENNM.
       M213-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : TMSFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-TMSONL.
           PERFORM M12-READ-TMS THRU M12-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKDFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKDONL.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M13-READ-WKD THRU M13-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE TMSFILE
                 NSMFILE
                 WKDFILE
                 WKAFILE.
       M3-EXIT.
           EXIT.
