./ ADD NAME=VSBA2380  01.02 20150623 20150901 175642 00706 00702 MCSK082
       ID  DIVISION.
       PROGRAM-ID.      'VSBA2380'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *
      *              <> SUBSYSTEM   : SUBROUTINE                       *
      *              <> PG.OUTLINE  : SEARCH SOSHIKI TABLE             *
      *                                                                *
      *               *  INPUT.PARM1 :                                 *
      *                WHEN HANBETSU-CODE = '1' STNKG + SOKKG          *
      *                                   = '2' SOKNO + STNKG          *
      *                                   = '3' SYKNO + STNKG          *
      *                                   = '4' STNNO                  *
      *                                   = '5' STNKG + SYKKG          *
      *                                   = '6' STNKG                  *
      *                                   = '7' STNNO + SOKKG          *
      *                                   = '8' STNNO + SYKKG          *
      *                                   = '9' STNNO + SOKNO          *
      *                                   = 'A' STNNO + SYKNO          *
      *                    WO KEY TOSHITE SEARCH SURU )                *
      *                                                                *
      *              <> AUTHOR      : CSK-S.WATANABE                   *
      *              <> DATE        : 98/04/10                         *
      *-----------  MAINTNENCE  HISTORY  ------------------------------*
      *  1. C-073                     : 99/02/01  CSK-Y.FUJITA         *
      *  2. C-110                     : 99/07/06  CSK-T.NAKAMURA       *
      *  3. H-010                     : 00/05/31  CSK-I.HARADA         *
      *  4. K-394                     : 00/08/01  CSK-S.KOZAWA         *
      *  5. K-455                     : 00/11/15  CSK-H.SATO           *
      *  6. K-1758                    : 15/06/23  CSK-H.TAKAKUWA       *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *   WORKING-STORAGE SECTION                                      *
      *----------------------------------------------------------------*
       01  TB-SOSHIKI.
           02 TB-S OCCURS 700  INDEXED BY I.
              03 TB-SYSNO      PIC  X(02).
              03 TB-STNKG      PIC  X(01).
              03 TB-SOKKG      PIC  X(01).
              03 TB-SYKKG      PIC  X(01).
              03 TB-STNNO      PIC  X(03).
              03 TB-SOKNO      PIC  X(03).
              03 TB-SYKNO      PIC  X(03).
              03 TB-STNME      PIC  X(10).
              03 TB-SYKME      PIC  X(08).
              03 TB-SOKME      PIC  X(08).
              03 TB-STNMJ      PIC  X(18).
              03 TB-SYKMJ      PIC  X(18).
              03 TB-SOKMJ      PIC  X(18).
              03 TB-STJCD      PIC  X(05).
              03 TB-STNGK      PIC  X(01).
              03 TB-KAGKB      PIC  X(01).
       01  J                   PIC S9(04) COMP.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINKAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  P237-PARM.
           COPY  VPARB237       SUPPRESS.
       01  P238-PARM.
           COPY  VPARB238       SUPPRESS.
       PROCEDURE DIVISION      USING P238-PARM P237-PARM.
      *----------------------------------------------------------------*
      *    M1 : MAIN CONTROL                                           *
      *----------------------------------------------------------------*
       M1-MAIN-CONTROL.
      *
           MOVE ZERO TO P238-RTNCD.
           IF P238-HNBCD = '1' OR '2' OR '3' OR
                          '5' OR '6'
              GO TO M1-L01
           ELSE
              GO TO M1-L02.
       M1-L01.
           EVALUATE P238-STNKG
               WHEN 'A'
               WHEN '´'
               WHEN 'O'
               WHEN 'M'
               WHEN 'P'
               WHEN '¸'
               WHEN '½'
               WHEN '»'
               WHEN 'G'
               WHEN 'Ä'
               WHEN 'È'
               WHEN 'Ë'
               WHEN 'Å'
               WHEN 'H'
               WHEN 'J'
               WHEN 'N'
                    MOVE P237-TEIBL( 1 ) TO TB-SOSHIKI
               WHEN 'Q'
               WHEN 'S'
               WHEN 'U'
               WHEN '¶'
               WHEN '·'
               WHEN 'Ã'
               WHEN 'Æ'
               WHEN 'Ç'
               WHEN 'Ê'
               WHEN 'C'
               WHEN 'K'
               WHEN 'L'
               WHEN 'T'
               WHEN '±'
                    MOVE P237-TEIBL( 2 ) TO TB-SOSHIKI
               WHEN '³'
               WHEN 'µ'
               WHEN '¿'
               WHEN 'À'
               WHEN 'Â'
               WHEN 'É'
               WHEN 'D'
               WHEN 'E'
               WHEN 'F'
               WHEN 'I'
               WHEN 'R'
               WHEN '²'
               WHEN '¹'
                    MOVE P237-TEIBL( 3 ) TO TB-SOSHIKI
               WHEN '0'
               WHEN '1'
               WHEN '2'
               WHEN '3'
               WHEN '4'
               WHEN '5'
               WHEN '6'
               WHEN '7'
               WHEN '8'
               WHEN '9'
               WHEN 'Y'
               WHEN 'Z'
                    MOVE P237-TEIBL( 4 ) TO TB-SOSHIKI
               WHEN OTHER
                    MOVE P237-TEIBL( 5 ) TO TB-SOSHIKI
           END-EVALUATE.
           GO TO M1-L08.
       M1-L02.
           EVALUATE P238-STNNO
               WHEN '231'
               WHEN '201'
               WHEN '161'
               WHEN '181'
               WHEN '236'
               WHEN '901'
               WHEN '916'
               WHEN '914'
               WHEN '221'
               WHEN '431'
               WHEN '906'
               WHEN '091'
               WHEN '011'
               WHEN '222'
               WHEN '251'
               WHEN '171'
                    MOVE P237-TEIBL( 1 ) TO TB-SOSHIKI
               WHEN '988'
               WHEN '131'
               WHEN '151'
               WHEN '101'
               WHEN '191'
               WHEN '031'
               WHEN '016'
               WHEN '904'
               WHEN '021'
               WHEN '461'
               WHEN '291'
               WHEN '301'
               WHEN '141'
               WHEN '041'
                    MOVE P237-TEIBL( 2 ) TO TB-SOSHIKI
               WHEN '111'
               WHEN '081'
               WHEN '401'
               WHEN '471'
               WHEN '142'
               WHEN '281'
               WHEN '271'
               WHEN '241'
               WHEN '211'
               WHEN '223'
               WHEN '912'
               WHEN '121'
               WHEN '911'
                    MOVE P237-TEIBL( 3 ) TO TB-SOSHIKI
               WHEN '750'
               WHEN '402'
               WHEN '403'
               WHEN '404'
               WHEN '411'
               WHEN '421'
               WHEN '432'
               WHEN '441'
               WHEN '451'
               WHEN '462'
               WHEN '773'
               WHEN '777'
                    MOVE P237-TEIBL( 4 ) TO TB-SOSHIKI
               WHEN OTHER
                    MOVE P237-TEIBL( 5 ) TO TB-SOSHIKI
           END-EVALUATE.
       M1-L08.
           IF        P238-HNBCD = '3'
               PERFORM  M13-SYKNO-STNKG     THRU  M13-EXIT
           ELSE  IF  P238-HNBCD = '1'
               PERFORM  M11-STNKG-SOKKG     THRU  M11-EXIT
           ELSE  IF  P238-HNBCD = '2'
               PERFORM  M12-SOKNO-STNKG     THRU  M12-EXIT
           ELSE  IF  P238-HNBCD = '8'
               PERFORM  M18-STNNO-SYKKG     THRU  M18-EXIT
           ELSE  IF  P238-HNBCD = '4'
               PERFORM  M14-STNNO           THRU  M14-EXIT
           ELSE  IF  P238-HNBCD = '5'
               PERFORM  M15-STNKG-SYKKG     THRU  M15-EXIT
           ELSE  IF  P238-HNBCD = '6'
               PERFORM  M16-STNKG           THRU  M16-EXIT
           ELSE  IF  P238-HNBCD = '7'
               PERFORM  M17-STNNO-SOKKG     THRU  M17-EXIT
           ELSE  IF  P238-HNBCD = '9'
               PERFORM  M19-STNNO-SOKNO     THRU  M19-EXIT
           ELSE  IF  P238-HNBCD = 'A'
               PERFORM  M1A-STNNO-SYKNO     THRU  M1A-EXIT
           ELSE
               MOVE   1   TO  P238-RTNCD.
      *
       M1-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    M1.1 : SEARCH SOSHIKI TABLE KEY = SOKO KIGO & SITEN KIGO    *
      *----------------------------------------------------------------*
       M11-STNKG-SOKKG.
           SET  I  TO 1.
           SEARCH  TB-S
                   AT END MOVE  1 TO P238-RTNCD
                          PERFORM M111-SPACE-SET THRU M111-EXIT
                   WHEN   TB-S ( I ) = ALL '*'
                          MOVE  1 TO P238-RTNCD
                          PERFORM M111-SPACE-SET THRU M111-EXIT
                   WHEN   P238-STNKG = TB-STNKG ( I )    AND
                          P238-SOKKG = TB-SOKKG ( I )
                          MOVE  TB-SYKKG ( I ) TO P238-SYKKG
                          MOVE  TB-STNNO ( I ) TO P238-STNNO
                          MOVE  TB-SOKNO ( I ) TO P238-SOKNO
                          MOVE  TB-SYKNO ( I ) TO P238-SYKNO
                          MOVE  TB-STNME ( I ) TO P238-STNME
                          MOVE  TB-SOKME ( I ) TO P238-SOKME
                          MOVE  TB-SYKME ( I ) TO P238-SYKME
                          MOVE  TB-STNMJ ( I ) TO P238-STNMJ
                          MOVE  TB-SOKMJ ( I ) TO P238-SOKMJ
                          MOVE  TB-SYKMJ ( I ) TO P238-SYKMJ
                          MOVE  TB-STJCD ( I ) TO P238-STJCD
                          MOVE  TB-STNGK ( I ) TO P238-STNGK
                          MOVE  TB-KAGKB ( I ) TO P238-KAGKB.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1.1 : SPACE SET    TABLE KEY = SOKO KIGO & SITEN KIGO    *
      *----------------------------------------------------------------*
       M111-SPACE-SET.
           MOVE  SPACE    TO P238-SYKKG.
           MOVE  SPACE    TO P238-STNNO.
           MOVE  SPACE    TO P238-SOKNO.
           MOVE  SPACE    TO P238-SYKNO.
           MOVE  SPACE    TO P238-STNME.
           MOVE  SPACE    TO P238-SOKME.
           MOVE  SPACE    TO P238-SYKME.
           MOVE  SPACE    TO P238-STNMJ.
           MOVE  SPACE    TO P238-SOKMJ.
           MOVE  SPACE    TO P238-SYKMJ.
           MOVE  SPACE    TO P238-STJCD.
           MOVE  SPACE    TO P238-STNGK.
           MOVE  SPACE    TO P238-KAGKB.
       M111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : SEARCH SOSHIKI TABLE KEY = SOKO-NO & SITEN-KIGO      *
      *----------------------------------------------------------------*
       M12-SOKNO-STNKG.
           SET  I  TO 1.
           SEARCH  TB-S
                   AT END MOVE  1 TO P238-RTNCD
                          PERFORM M121-SPACE-SET THRU M121-EXIT
                   WHEN   TB-S ( I ) = ALL '*'
                          MOVE  1 TO P238-RTNCD
                          PERFORM M121-SPACE-SET THRU M121-EXIT
                   WHEN   P238-SOKNO = TB-SOKNO ( I )  AND
                          P238-STNKG = TB-STNKG ( I )
                          MOVE  TB-SOKKG ( I ) TO P238-SOKKG
                          MOVE  TB-SYKKG ( I ) TO P238-SYKKG
                          MOVE  TB-STNNO ( I ) TO P238-STNNO
                          MOVE  TB-SYKNO ( I ) TO P238-SYKNO
                          MOVE  TB-STNME ( I ) TO P238-STNME
                          MOVE  TB-SOKME ( I ) TO P238-SOKME
                          MOVE  TB-SYKME ( I ) TO P238-SYKME
                          MOVE  TB-STNMJ ( I ) TO P238-STNMJ
                          MOVE  TB-SOKMJ ( I ) TO P238-SOKMJ
                          MOVE  TB-SYKMJ ( I ) TO P238-SYKMJ
                          MOVE  TB-STJCD ( I ) TO P238-STJCD
                          MOVE  TB-STNGK ( I ) TO P238-STNGK
                          MOVE  TB-KAGKB ( I ) TO P238-KAGKB.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1 : SPACE SET    TABLE KEY = SOKO-NO & SITEN-KIGO      *
      *----------------------------------------------------------------*
       M121-SPACE-SET.
           MOVE  SPACE    TO P238-SOKKG.
           MOVE  SPACE    TO P238-SYKKG.
           MOVE  SPACE    TO P238-STNNO.
           MOVE  SPACE    TO P238-SYKNO.
           MOVE  SPACE    TO P238-STNME.
           MOVE  SPACE    TO P238-SOKME.
           MOVE  SPACE    TO P238-SYKME.
           MOVE  SPACE    TO P238-STNMJ.
           MOVE  SPACE    TO P238-SOKMJ.
           MOVE  SPACE    TO P238-SYKMJ.
           MOVE  SPACE    TO P238-STJCD.
           MOVE  SPACE    TO P238-STNGK.
           MOVE  SPACE    TO P238-KAGKB.
       M121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.3 : SEARCH SOSHIKI TABLE KEY = SYOKA-NO & SITEN-KIGO     *
      *----------------------------------------------------------------*
       M13-SYKNO-STNKG.
           SET  I  TO 1.
           SEARCH  TB-S
                   AT END MOVE  1 TO P238-RTNCD
                          PERFORM M131-SPACE-SET THRU M131-EXIT
                   WHEN   TB-S ( I ) = ALL '*'
                          MOVE  1 TO P238-RTNCD
                          PERFORM M131-SPACE-SET THRU M131-EXIT
                   WHEN   P238-SYKNO = TB-SYKNO ( I )  AND
                          P238-STNKG = TB-STNKG ( I )
                          MOVE  TB-SOKKG ( I ) TO P238-SOKKG
                          MOVE  TB-SYKKG ( I ) TO P238-SYKKG
                          MOVE  TB-STNNO ( I ) TO P238-STNNO
                          MOVE  TB-SOKNO ( I ) TO P238-SOKNO
                          MOVE  TB-STNME ( I ) TO P238-STNME
                          MOVE  TB-SOKME ( I ) TO P238-SOKME
                          MOVE  TB-SYKME ( I ) TO P238-SYKME
                          MOVE  TB-STNMJ ( I ) TO P238-STNMJ
                          MOVE  TB-SOKMJ ( I ) TO P238-SOKMJ
                          MOVE  TB-SYKMJ ( I ) TO P238-SYKMJ
                          MOVE  TB-STJCD ( I ) TO P238-STJCD
                          MOVE  TB-STNGK ( I ) TO P238-STNGK
                          MOVE  TB-KAGKB ( I ) TO P238-KAGKB.
       M13-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.3.1 : SPACE SET    TABLE KEY = SYOKA-NO & SITEN-KIGO     *
      *----------------------------------------------------------------*
       M131-SPACE-SET.
           MOVE  SPACE    TO P238-SOKKG.
           MOVE  SPACE    TO P238-SYKKG.
           MOVE  SPACE    TO P238-STNNO.
           MOVE  SPACE    TO P238-SOKNO.
           MOVE  SPACE    TO P238-STNME.
           MOVE  SPACE    TO P238-SOKME.
           MOVE  SPACE    TO P238-SYKME.
           MOVE  SPACE    TO P238-STNMJ.
           MOVE  SPACE    TO P238-SOKMJ.
           MOVE  SPACE    TO P238-SYKMJ.
           MOVE  SPACE    TO P238-STJCD.
           MOVE  SPACE    TO P238-STNGK.
           MOVE  SPACE    TO P238-KAGKB.
       M131-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.4 : SEARCH SOSHIKI TABLE KEY = SITEN-NO.                 *
      *----------------------------------------------------------------*
       M14-STNNO.
           SET  I  TO 1.
           SEARCH  TB-S
                   AT END MOVE  1 TO P238-RTNCD
                          PERFORM M141-SPACE-SET THRU M141-EXIT
                   WHEN   TB-S ( I ) = ALL '*'
                          MOVE  1 TO P238-RTNCD
                          PERFORM M141-SPACE-SET THRU M141-EXIT
                   WHEN   P238-STNNO = TB-STNNO ( I )
                          MOVE  TB-STNKG ( I ) TO P238-STNKG
                          MOVE  TB-SYKKG ( I ) TO P238-SYKKG
                          MOVE  TB-SOKKG ( I ) TO P238-SOKKG
                          MOVE  TB-SOKNO ( I ) TO P238-SOKNO
                          MOVE  TB-SYKNO ( I ) TO P238-SYKNO
                          MOVE  TB-STNME ( I ) TO P238-STNME
                          MOVE  TB-SOKME ( I ) TO P238-SOKME
                          MOVE  TB-SYKME ( I ) TO P238-SYKME
                          MOVE  TB-STNMJ ( I ) TO P238-STNMJ
                          MOVE  TB-SOKMJ ( I ) TO P238-SOKMJ
                          MOVE  TB-SYKMJ ( I ) TO P238-SYKMJ
                          MOVE  TB-STJCD ( I ) TO P238-STJCD
                          MOVE  TB-STNGK ( I ) TO P238-STNGK
                          MOVE  TB-KAGKB ( I ) TO P238-KAGKB.
       M14-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.4.1 : SPACE SET    TABLE KEY = SITEN-NO                  *
      *----------------------------------------------------------------*
       M141-SPACE-SET.
           MOVE  SPACE    TO P238-STNKG.
           MOVE  SPACE    TO P238-SOKKG.
           MOVE  SPACE    TO P238-SYKKG.
           MOVE  SPACE    TO P238-SOKNO.
           MOVE  SPACE    TO P238-SYKNO.
           MOVE  SPACE    TO P238-STNME.
           MOVE  SPACE    TO P238-SOKME.
           MOVE  SPACE    TO P238-SYKME.
           MOVE  SPACE    TO P238-STNMJ.
           MOVE  SPACE    TO P238-SOKMJ.
           MOVE  SPACE    TO P238-SYKMJ.
           MOVE  SPACE    TO P238-STJCD.
           MOVE  SPACE    TO P238-STNGK.
           MOVE  SPACE    TO P238-KAGKB.
       M141-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.5 : SEARCH SOSHIKI TABLE KEY = SITEN-KIGO & SYOKA-KIGO   *
      *----------------------------------------------------------------*
       M15-STNKG-SYKKG.
           SET  I  TO 1.
           SEARCH  TB-S
                   AT END MOVE  1 TO P238-RTNCD
                          PERFORM M151-SPACE-SET THRU M151-EXIT
                   WHEN   TB-S ( I ) = ALL '*'
                          MOVE  1 TO P238-RTNCD
                          PERFORM M151-SPACE-SET THRU M151-EXIT
                   WHEN   P238-STNKG = TB-STNKG ( I )    AND
                          P238-SYKKG = TB-SYKKG ( I )
                          MOVE  TB-SOKKG ( I ) TO P238-SOKKG
                          MOVE  TB-STNNO ( I ) TO P238-STNNO
                          MOVE  TB-SOKNO ( I ) TO P238-SOKNO
                          MOVE  TB-SYKNO ( I ) TO P238-SYKNO
                          MOVE  TB-STNME ( I ) TO P238-STNME
                          MOVE  TB-SOKME ( I ) TO P238-SOKME
                          MOVE  TB-SYKME ( I ) TO P238-SYKME
                          MOVE  TB-STNMJ ( I ) TO P238-STNMJ
                          MOVE  TB-SOKMJ ( I ) TO P238-SOKMJ
                          MOVE  TB-SYKMJ ( I ) TO P238-SYKMJ
                          MOVE  TB-STJCD ( I ) TO P238-STJCD
                          MOVE  TB-STNGK ( I ) TO P238-STNGK
                          MOVE  TB-KAGKB ( I ) TO P238-KAGKB.
       M15-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.5.1 : SPACE SET    TABLE KEY = SITEN-KIGO & SYOKA-KIGO   *
      *----------------------------------------------------------------*
       M151-SPACE-SET.
           MOVE  SPACE    TO P238-SOKKG.
           MOVE  SPACE    TO P238-STNNO.
           MOVE  SPACE    TO P238-SOKNO.
           MOVE  SPACE    TO P238-SYKNO.
           MOVE  SPACE    TO P238-STNME.
           MOVE  SPACE    TO P238-SOKME.
           MOVE  SPACE    TO P238-SYKME.
           MOVE  SPACE    TO P238-STNMJ.
           MOVE  SPACE    TO P238-SOKMJ.
           MOVE  SPACE    TO P238-SYKMJ.
           MOVE  SPACE    TO P238-STJCD.
           MOVE  SPACE    TO P238-STNGK.
           MOVE  SPACE    TO P238-KAGKB.
       M151-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.6 : SEARCH SOSHIKI TABLE KEY = SITEN-KIGO                *
      *----------------------------------------------------------------*
       M16-STNKG.
           SET  I  TO 1.
           SEARCH  TB-S
                   AT END MOVE  1 TO P238-RTNCD
                          PERFORM M161-SPACE-SET THRU M161-EXIT
                   WHEN   TB-S ( I ) = ALL '*'
                          MOVE  1 TO P238-RTNCD
                          PERFORM M161-SPACE-SET THRU M161-EXIT
                   WHEN   P238-STNKG = TB-STNKG ( I )
                          MOVE  TB-SOKKG ( I ) TO P238-SOKKG
                          MOVE  TB-SYKKG ( I ) TO P238-SYKKG
                          MOVE  TB-STNNO ( I ) TO P238-STNNO
                          MOVE  TB-SOKNO ( I ) TO P238-SOKNO
                          MOVE  TB-SYKNO ( I ) TO P238-SYKNO
                          MOVE  TB-STNME ( I ) TO P238-STNME
                          MOVE  TB-SOKME ( I ) TO P238-SOKME
                          MOVE  TB-SYKME ( I ) TO P238-SYKME
                          MOVE  TB-STNMJ ( I ) TO P238-STNMJ
                          MOVE  TB-SOKMJ ( I ) TO P238-SOKMJ
                          MOVE  TB-SYKMJ ( I ) TO P238-SYKMJ
                          MOVE  TB-STJCD ( I ) TO P238-STJCD
                          MOVE  TB-STNGK ( I ) TO P238-STNGK
                          MOVE  TB-KAGKB ( I ) TO P238-KAGKB.
       M16-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.6.1 : SPACE SET    TABLE KEY = SITEN-KIGO                *
      *----------------------------------------------------------------*
       M161-SPACE-SET.
           MOVE  SPACE    TO P238-SOKKG.
           MOVE  SPACE    TO P238-SYKKG.
           MOVE  SPACE    TO P238-STNNO.
           MOVE  SPACE    TO P238-SOKNO.
           MOVE  SPACE    TO P238-SYKNO.
           MOVE  SPACE    TO P238-STNME.
           MOVE  SPACE    TO P238-SOKME.
           MOVE  SPACE    TO P238-SYKME.
           MOVE  SPACE    TO P238-STNMJ.
           MOVE  SPACE    TO P238-SOKMJ.
           MOVE  SPACE    TO P238-SYKMJ.
           MOVE  SPACE    TO P238-STJCD.
           MOVE  SPACE    TO P238-STNGK.
           MOVE  SPACE    TO P238-KAGKB.
       M161-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.7 : SEARCH SOSHIKI TABLE KEY = SOKO KIGO & SITEN NO      *
      *----------------------------------------------------------------*
       M17-STNNO-SOKKG.
           SET  I  TO 1.
           SEARCH  TB-S
                   AT END MOVE  1 TO P238-RTNCD
                          PERFORM M171-SPACE-SET THRU M171-EXIT
                   WHEN   TB-S ( I ) = ALL '*'
                          MOVE  1 TO P238-RTNCD
                          PERFORM M171-SPACE-SET THRU M171-EXIT
                   WHEN   P238-STNNO = TB-STNNO ( I )    AND
                          P238-SOKKG = TB-SOKKG ( I )    AND
                          TB-SYKKG ( I ) NOT = '*'
                          MOVE  TB-STNKG ( I ) TO P238-STNKG
                          MOVE  TB-SYKKG ( I ) TO P238-SYKKG
                          MOVE  TB-SOKNO ( I ) TO P238-SOKNO
                          MOVE  TB-SYKNO ( I ) TO P238-SYKNO
                          MOVE  TB-STNME ( I ) TO P238-STNME
                          MOVE  TB-SOKME ( I ) TO P238-SOKME
                          MOVE  TB-SYKME ( I ) TO P238-SYKME
                          MOVE  TB-STNMJ ( I ) TO P238-STNMJ
                          MOVE  TB-SOKMJ ( I ) TO P238-SOKMJ
                          MOVE  TB-SYKMJ ( I ) TO P238-SYKMJ
                          MOVE  TB-STJCD ( I ) TO P238-STJCD
                          MOVE  TB-STNGK ( I ) TO P238-STNGK
                          MOVE  TB-KAGKB ( I ) TO P238-KAGKB.
       M17-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.7.1 : SPACE SET    TABLE KEY = SOKO-KIGO & SITEN-NO      *
      *----------------------------------------------------------------*
       M171-SPACE-SET.
           MOVE  SPACE    TO P238-STNKG.
           MOVE  SPACE    TO P238-SYKKG.
           MOVE  SPACE    TO P238-SOKNO.
           MOVE  SPACE    TO P238-SYKNO.
           MOVE  SPACE    TO P238-STNME.
           MOVE  SPACE    TO P238-SOKME.
           MOVE  SPACE    TO P238-SYKME.
           MOVE  SPACE    TO P238-STNMJ.
           MOVE  SPACE    TO P238-SOKMJ.
           MOVE  SPACE    TO P238-SYKMJ.
           MOVE  SPACE    TO P238-STJCD.
           MOVE  SPACE    TO P238-STNGK.
           MOVE  SPACE    TO P238-KAGKB.
       M171-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.8 : SEARCH SOSHIKI TABLE KEY = SITEN-NO & SYOKA-KIGOU    *
      *----------------------------------------------------------------*
       M18-STNNO-SYKKG.
           SET  I  TO 1.
           SEARCH  TB-S
                   AT END MOVE  1 TO P238-RTNCD
                          PERFORM M181-SPACE-SET THRU M181-EXIT
                   WHEN   TB-S ( I ) = ALL '*'
                          MOVE  1 TO P238-RTNCD
                          PERFORM M181-SPACE-SET THRU M181-EXIT
                   WHEN   P238-STNNO = TB-STNNO ( I )    AND
                          P238-SYKKG = TB-SYKKG ( I )
                          MOVE  TB-STNKG ( I ) TO P238-STNKG
                          MOVE  TB-SOKKG ( I ) TO P238-SOKKG
                          MOVE  TB-SOKNO ( I ) TO P238-SOKNO
                          MOVE  TB-SYKNO ( I ) TO P238-SYKNO
                          MOVE  TB-STNME ( I ) TO P238-STNME
                          MOVE  TB-SOKME ( I ) TO P238-SOKME
                          MOVE  TB-SYKME ( I ) TO P238-SYKME
                          MOVE  TB-STNMJ ( I ) TO P238-STNMJ
                          MOVE  TB-SOKMJ ( I ) TO P238-SOKMJ
                          MOVE  TB-SYKMJ ( I ) TO P238-SYKMJ
                          MOVE  TB-STJCD ( I ) TO P238-STJCD
                          MOVE  TB-STNGK ( I ) TO P238-STNGK
                          MOVE  TB-KAGKB ( I ) TO P238-KAGKB.
       M18-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.8.1 : SPACE SET    TABLE KEY = SITEN-NO & SYOKA-KIGOU    *
      *----------------------------------------------------------------*
       M181-SPACE-SET.
           MOVE  SPACE    TO P238-STNKG.
           MOVE  SPACE    TO P238-SOKKG.
           MOVE  SPACE    TO P238-SOKNO.
           MOVE  SPACE    TO P238-SYKNO.
           MOVE  SPACE    TO P238-STNME.
           MOVE  SPACE    TO P238-SOKME.
           MOVE  SPACE    TO P238-SYKME.
           MOVE  SPACE    TO P238-STNMJ.
           MOVE  SPACE    TO P238-SOKMJ.
           MOVE  SPACE    TO P238-SYKMJ.
           MOVE  SPACE    TO P238-STJCD.
           MOVE  SPACE    TO P238-STNGK.
           MOVE  SPACE    TO P238-KAGKB.
       M181-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.9 : SEARCH SOSHIKI TABLE KEY = SITEN-NO & SOKO-NO        *
      *----------------------------------------------------------------*
       M19-STNNO-SOKNO.
           SET  I  TO 1.
           SEARCH  TB-S
                   AT END MOVE  1 TO P238-RTNCD
                          PERFORM M191-SPACE-SET THRU M191-EXIT
                   WHEN   TB-S ( I ) = ALL '*'
                          MOVE  1 TO P238-RTNCD
                          PERFORM M191-SPACE-SET THRU M191-EXIT
                   WHEN   P238-STNNO = TB-STNNO ( I )    AND
                          P238-SOKNO = TB-SOKNO ( I )
                          MOVE  TB-STNKG ( I ) TO P238-STNKG
                          MOVE  TB-SOKKG ( I ) TO P238-SOKKG
                          MOVE  TB-SYKKG ( I ) TO P238-SYKKG
                          MOVE  TB-SYKNO ( I ) TO P238-SYKNO
                          MOVE  TB-STNME ( I ) TO P238-STNME
                          MOVE  TB-SOKME ( I ) TO P238-SOKME
                          MOVE  TB-SYKME ( I ) TO P238-SYKME
                          MOVE  TB-STNMJ ( I ) TO P238-STNMJ
                          MOVE  TB-SOKMJ ( I ) TO P238-SOKMJ
                          MOVE  TB-SYKMJ ( I ) TO P238-SYKMJ
                          MOVE  TB-STJCD ( I ) TO P238-STJCD
                          MOVE  TB-STNGK ( I ) TO P238-STNGK
                          MOVE  TB-KAGKB ( I ) TO P238-KAGKB.
       M19-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.9.1 : SPACE SET    TABLE KEY = SITEN-NO & SYOKA-NO       *
      *----------------------------------------------------------------*
       M191-SPACE-SET.
           MOVE  SPACE    TO P238-STNKG.
           MOVE  SPACE    TO P238-SOKKG.
           MOVE  SPACE    TO P238-SYKKG.
           MOVE  SPACE    TO P238-SOKNO.
           MOVE  SPACE    TO P238-STNME.
           MOVE  SPACE    TO P238-SOKME.
           MOVE  SPACE    TO P238-SYKME.
           MOVE  SPACE    TO P238-STNMJ.
           MOVE  SPACE    TO P238-SOKMJ.
           MOVE  SPACE    TO P238-SYKMJ.
           MOVE  SPACE    TO P238-STJCD.
           MOVE  SPACE    TO P238-STNGK.
           MOVE  SPACE    TO P238-KAGKB.
       M191-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.A : SEARCH SOSHIKI TABLE KEY = SITEN-NO & SYOKA-NO       *
      *----------------------------------------------------------------*
       M1A-STNNO-SYKNO.
           SET  I  TO 1.
           SEARCH  TB-S
                   AT END MOVE  1 TO P238-RTNCD
                          PERFORM M1A1-SPACE-SET THRU M1A1-EXIT
                   WHEN   TB-S ( I ) = ALL '*'
                          MOVE  1 TO P238-RTNCD
                          PERFORM M1A1-SPACE-SET THRU M1A1-EXIT
                   WHEN   P238-STNNO = TB-STNNO ( I )    AND
                          P238-SYKNO = TB-SYKNO ( I )
                          MOVE  TB-STNKG ( I ) TO P238-STNKG
                          MOVE  TB-SYKKG ( I ) TO P238-SYKKG
                          MOVE  TB-SOKKG ( I ) TO P238-SOKKG
                          MOVE  TB-SOKNO ( I ) TO P238-SOKNO
                          MOVE  TB-STNME ( I ) TO P238-STNME
                          MOVE  TB-SOKME ( I ) TO P238-SOKME
                          MOVE  TB-SYKME ( I ) TO P238-SYKME
                          MOVE  TB-STNMJ ( I ) TO P238-STNMJ
                          MOVE  TB-SOKMJ ( I ) TO P238-SOKMJ
                          MOVE  TB-SYKMJ ( I ) TO P238-SYKMJ
                          MOVE  TB-STJCD ( I ) TO P238-STJCD
                          MOVE  TB-STNGK ( I ) TO P238-STNGK
                          MOVE  TB-KAGKB ( I ) TO P238-KAGKB.
       M1A-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.A.1 : SPACE SET    TABLE KEY = SITEN-NO & SYOKA-NO       *
      *----------------------------------------------------------------*
       M1A1-SPACE-SET.
           MOVE  SPACE    TO P238-STNKG.
           MOVE  SPACE    TO P238-SYKKG.
           MOVE  SPACE    TO P238-SOKKG.
           MOVE  SPACE    TO P238-SOKNO.
           MOVE  SPACE    TO P238-STNME.
           MOVE  SPACE    TO P238-SOKME.
           MOVE  SPACE    TO P238-SYKME.
           MOVE  SPACE    TO P238-STNMJ.
           MOVE  SPACE    TO P238-SOKMJ.
           MOVE  SPACE    TO P238-SYKMJ.
           MOVE  SPACE    TO P238-STJCD.
           MOVE  SPACE    TO P238-STNGK.
           MOVE  SPACE    TO P238-KAGKB.
       M1A1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *               VSBA2380      END                                *
      *----------------------------------------------------------------*
