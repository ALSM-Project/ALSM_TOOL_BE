       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARENCMP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-A        PIC X(2).
       01  WS-N1       PIC 9(3).
       01  WS-N2       PIC 9(3).
       01  WS-N3       PIC 9(3).
       PROCEDURE DIVISION.
           IF WS-A  =  (  SPACE OR '00'  )
              MOVE 'X' TO WS-A
           END-IF.
           IF ZERO = ( WS-N1 AND WS-N2 AND
                       WS-N3 )
              GO TO FIN.
           MOVE 1 TO WS-N1.
       FIN.
           STOP RUN.
