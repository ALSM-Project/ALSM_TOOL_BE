       IDENTIFICATION DIVISION.
       PROGRAM-ID. WITHUR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
       01  WS-A0        PIC X(4).
       01  WS-F6        PIC X(4).
       PROCEDURE DIVISION.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT A0
              FROM    TMS
              WHERE F6=:WS-F6
              ORDER BY A0
              WITH UR
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           STOP RUN.
