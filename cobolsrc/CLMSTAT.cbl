       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMSTAT.
      ******************************************************************
      *  CLMSTAT - Claim Status Management Program
      *  Workers' Compensation TPA System
      *  HPE NonStop COBOL (Tandem COBOL85)
      *
      *  Manages claim status transitions with full validation,
      *  closure rules, reopening rules, status history audit trail,
      *  litigation sub-tracking, and settlement management.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. TANDEM.
       OBJECT-COMPUTER. TANDEM.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIM-FILE ASSIGN TO "$CLMFL"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CF-CLAIM-NUMBER
               FILE STATUS IS WS-CLM-STATUS.

           SELECT HISTORY-FILE ASSIGN TO "$CLMHIST"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-HIST-STATUS.

           SELECT RESERVE-FILE ASSIGN TO "$RSVFL"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RF-CLAIM-NUMBER
               FILE STATUS IS WS-RSV-STATUS.

           SELECT PAYMENT-FILE ASSIGN TO "$PYMTFL"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PF-CLAIM-NUMBER
               FILE STATUS IS WS-PMT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  CLAIM-FILE.
       01  CLAIM-RECORD.
           05  CF-CLAIM-NUMBER     PIC X(15).
           05  CF-CLAIM-STATUS     PIC 9.
               88  CF-IS-OPEN            VALUE 1.
               88  CF-IS-PENDING         VALUE 2.
               88  CF-IS-ACTIVE          VALUE 3.
               88  CF-IS-CLOSED          VALUE 4.
               88  CF-IS-REOPENED        VALUE 5.
           05  CF-CLAIM-TYPE       PIC 9.
           05  CF-COMPENSABILITY   PIC 9.
               88  CF-COMP-PENDING       VALUE 0.
               88  CF-COMP-ACCEPTED      VALUE 1.
               88  CF-COMP-DENIED        VALUE 2.
               88  CF-COMP-INVESTIGATE   VALUE 3.
           05  CF-ADJUSTER-ID      PIC X(8).
           05  CF-DATE-OF-INJURY   PIC X(8).
           05  CF-DATE-CREATED     PIC X(8).
           05  CF-DATE-CLOSED      PIC X(8).
           05  CF-DATE-REOPENED    PIC X(8).
           05  CF-INJURY-STATE     PIC X(2).
           05  CF-MED-RESERVE      PIC S9(9)V99 COMP-3.
           05  CF-IND-RESERVE      PIC S9(9)V99 COMP-3.
           05  CF-EXP-RESERVE      PIC S9(9)V99 COMP-3.
           05  CF-MED-PAID         PIC S9(9)V99 COMP-3.
           05  CF-IND-PAID         PIC S9(9)V99 COMP-3.
           05  CF-EXP-PAID         PIC S9(9)V99 COMP-3.
           05  CF-LITIGATION-STATUS PIC 9.
               88  CF-NO-LITIGATION      VALUE 0.
               88  CF-ATTY-REP          VALUE 1.
               88  CF-HEARING-SCHED     VALUE 2.
               88  CF-IN-LITIGATION     VALUE 3.
               88  CF-SETTLED           VALUE 4.
               88  CF-ADJUDICATED       VALUE 5.
           05  CF-SETTLEMENT-STATUS PIC 9.
               88  CF-NO-SETTLEMENT      VALUE 0.
               88  CF-SETTLEMENT-PENDING VALUE 1.
               88  CF-SETTLEMENT-OFFERED VALUE 2.
               88  CF-SETTLEMENT-ACCEPTED VALUE 3.
               88  CF-SETTLEMENT-DENIED  VALUE 4.
           05  CF-LAST-STATUS-DATE PIC X(8).
           05  CF-LAST-STATUS-USER PIC X(8).
           05  CF-SUPERVISOR-APPR  PIC X.
               88  CF-NEEDS-APPROVAL     VALUE 'Y'.
               88  CF-NO-APPROVAL-NEEDED VALUE 'N'.

       FD  HISTORY-FILE.
       01  HISTORY-RECORD.
           05  HR-CLAIM-NUMBER     PIC X(15).
           05  HR-STATUS-DATE      PIC X(8).
           05  HR-STATUS-TIME      PIC X(6).
           05  HR-OLD-STATUS       PIC 9.
           05  HR-NEW-STATUS       PIC 9.
           05  HR-OLD-LITIG-STATUS PIC 9.
           05  HR-NEW-LITIG-STATUS PIC 9.
           05  HR-OLD-SETTLE-STATUS PIC 9.
           05  HR-NEW-SETTLE-STATUS PIC 9.
           05  HR-CHANGED-BY       PIC X(8).
           05  HR-SUPERVISOR-ID    PIC X(8).
           05  HR-REASON-CODE      PIC X(4).
           05  HR-REASON-TEXT      PIC X(120).

       FD  RESERVE-FILE.
       01  RESERVE-RECORD.
           05  RF-CLAIM-NUMBER     PIC X(15).
           05  RF-MED-OUTSTANDING  PIC S9(9)V99 COMP-3.
           05  RF-IND-OUTSTANDING  PIC S9(9)V99 COMP-3.
           05  RF-EXP-OUTSTANDING  PIC S9(9)V99 COMP-3.

       FD  PAYMENT-FILE.
       01  PAYMENT-RECORD.
           05  PF-CLAIM-NUMBER     PIC X(15).
           05  PF-UNRECONCILED-AMT PIC S9(9)V99 COMP-3.

       WORKING-STORAGE SECTION.
      *
      *    Cross-program copybook references
           COPY WCCLMCPY
           COPY WCRSVCPY

       01  WS-FILE-STATUSES.
           05  WS-CLM-STATUS       PIC X(2).
           05  WS-HIST-STATUS      PIC X(2).
           05  WS-RSV-STATUS       PIC X(2).
           05  WS-PMT-STATUS       PIC X(2).

       01  WS-STATUS-REQUEST.
           05  WS-REQ-CLAIM-NUMBER PIC X(15).
           05  WS-REQ-NEW-STATUS   PIC 9.
           05  WS-REQ-NEW-LITIG    PIC 9.
           05  WS-REQ-NEW-SETTLE   PIC 9.
           05  WS-REQ-USER-ID      PIC X(8).
           05  WS-REQ-SUPERVISOR   PIC X(8).
           05  WS-REQ-REASON-CODE  PIC X(4).
           05  WS-REQ-REASON-TEXT  PIC X(120).
           05  WS-REQ-FORCE-FLAG   PIC X VALUE 'N'.
               88  WS-FORCE-CLOSE       VALUE 'Y'.

       01  WS-STATUS-RESULT.
           05  WS-RES-RETURN-CODE  PIC 9(2).
               88  WS-STATUS-OK          VALUE 00.
               88  WS-STATUS-WARN        VALUE 04.
               88  WS-STATUS-FAIL        VALUE 08.
               88  WS-STATUS-DENY        VALUE 12.
           05  WS-RES-MESSAGE      PIC X(120).

       01  WS-VALIDATION-FLAGS.
           05  WS-TRANS-VALID      PIC X VALUE 'Y'.
               88  TRANSITION-VALID      VALUE 'Y'.
               88  TRANSITION-INVALID    VALUE 'N'.
           05  WS-CLOSURE-VALID    PIC X VALUE 'Y'.
               88  CLOSURE-VALID         VALUE 'Y'.
               88  CLOSURE-INVALID       VALUE 'N'.
           05  WS-REOPEN-VALID     PIC X VALUE 'Y'.
               88  REOPEN-VALID          VALUE 'Y'.
               88  REOPEN-INVALID        VALUE 'N'.

       01  WS-WORK-FIELDS.
           05  WS-CURRENT-DATE     PIC X(8).
           05  WS-CURRENT-TIME     PIC X(6).
           05  WS-OLD-STATUS       PIC 9.
           05  WS-TOTAL-RESERVES   PIC S9(11)V99.
           05  WS-DOI-YEAR         PIC 9(4).
           05  WS-CURR-YEAR        PIC 9(4).
           05  WS-YEARS-SINCE-DOI  PIC 9(3).

      * Valid status transitions matrix
       01  WS-TRANSITION-TABLE.
      *    From-Status, To-Status, Valid (Y/N)
           05  FILLER PIC X(3) VALUE '11N'.
           05  FILLER PIC X(3) VALUE '12Y'.
           05  FILLER PIC X(3) VALUE '13Y'.
           05  FILLER PIC X(3) VALUE '14Y'.
           05  FILLER PIC X(3) VALUE '15N'.
           05  FILLER PIC X(3) VALUE '21Y'.
           05  FILLER PIC X(3) VALUE '22N'.
           05  FILLER PIC X(3) VALUE '23Y'.
           05  FILLER PIC X(3) VALUE '24Y'.
           05  FILLER PIC X(3) VALUE '25N'.
           05  FILLER PIC X(3) VALUE '31N'.
           05  FILLER PIC X(3) VALUE '32Y'.
           05  FILLER PIC X(3) VALUE '33N'.
           05  FILLER PIC X(3) VALUE '34Y'.
           05  FILLER PIC X(3) VALUE '35N'.
           05  FILLER PIC X(3) VALUE '41N'.
           05  FILLER PIC X(3) VALUE '42N'.
           05  FILLER PIC X(3) VALUE '43N'.
           05  FILLER PIC X(3) VALUE '44N'.
           05  FILLER PIC X(3) VALUE '45Y'.
           05  FILLER PIC X(3) VALUE '51Y'.
           05  FILLER PIC X(3) VALUE '52Y'.
           05  FILLER PIC X(3) VALUE '53Y'.
           05  FILLER PIC X(3) VALUE '54Y'.
           05  FILLER PIC X(3) VALUE '55N'.
       01  WS-TRANS-TBL-R REDEFINES WS-TRANSITION-TABLE.
           05  WS-TRANS-ENTRY OCCURS 25 TIMES.
               10  WS-TRANS-FROM   PIC 9.
               10  WS-TRANS-TO     PIC 9.
               10  WS-TRANS-OK     PIC X.
                   88  WS-IS-VALID-TRANS  VALUE 'Y'.

       01  WS-TRANS-INDEX          PIC 9(2).

      * Statutory reopen periods by state (years)
       01  WS-REOPEN-LIMITS.
           05  FILLER PIC X(4) VALUE 'CA05'.
           05  FILLER PIC X(4) VALUE 'NY07'.
           05  FILLER PIC X(4) VALUE 'TX03'.
           05  FILLER PIC X(4) VALUE 'FL02'.
           05  FILLER PIC X(4) VALUE 'PA03'.
           05  FILLER PIC X(4) VALUE 'IL03'.
           05  FILLER PIC X(4) VALUE 'OH05'.
           05  FILLER PIC X(4) VALUE 'NJ02'.
           05  FILLER PIC X(4) VALUE 'GA02'.
           05  FILLER PIC X(4) VALUE 'VA02'.
       01  WS-REOPEN-TBL-R REDEFINES WS-REOPEN-LIMITS.
           05  WS-REOPEN-ENTRY OCCURS 10 TIMES.
               10  WS-REOPEN-STATE PIC X(2).
               10  WS-REOPEN-YEARS PIC 9(2).
       01  WS-REOPEN-IDX          PIC 9(2).
       01  WS-REOPEN-LIMIT-YRS    PIC 9(2) VALUE 3.

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
      *
      *    Inter-program communication calls
           CALL "RSVRCALC"
           PATHSEND USING "CLMPROC"
           PERFORM 1000-INITIALIZE
           PERFORM 2000-READ-CLAIM
           IF WS-STATUS-OK
               EVALUATE WS-REQ-NEW-STATUS
                   WHEN 4
                       PERFORM 3000-VALIDATE-CLOSURE
                   WHEN 5
                       PERFORM 4000-VALIDATE-REOPEN
                   WHEN OTHER
                       PERFORM 5000-VALIDATE-TRANSITION
               END-EVALUATE
           END-IF
           IF WS-STATUS-OK
               PERFORM 6000-APPLY-STATUS-CHANGE
               PERFORM 7000-WRITE-HISTORY
               PERFORM 8000-HANDLE-LITIGATION
               PERFORM 8500-HANDLE-SETTLEMENT
           END-IF
           PERFORM 9000-CLEANUP
           STOP RUN.

       1000-INITIALIZE.
           OPEN I-O CLAIM-FILE
           OPEN EXTEND HISTORY-FILE
           OPEN INPUT RESERVE-FILE
           OPEN INPUT PAYMENT-FILE
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-CURRENT-TIME
           MOVE 00 TO WS-RES-RETURN-CODE
           MOVE 'Y' TO WS-TRANS-VALID
           MOVE 'Y' TO WS-CLOSURE-VALID
           MOVE 'Y' TO WS-REOPEN-VALID.

       2000-READ-CLAIM.
           MOVE WS-REQ-CLAIM-NUMBER TO CF-CLAIM-NUMBER
           READ CLAIM-FILE
               INVALID KEY
                   MOVE 08 TO WS-RES-RETURN-CODE
                   STRING "Claim " WS-REQ-CLAIM-NUMBER
                       " not found" DELIMITED BY SIZE
                       INTO WS-RES-MESSAGE
           END-READ.

       3000-VALIDATE-CLOSURE.
           PERFORM 3100-CHECK-TRANSITION
           IF TRANSITION-INVALID
               EXIT PARAGRAPH
           END-IF
           PERFORM 3200-CHECK-RESERVES
           PERFORM 3300-CHECK-PAYMENTS
           PERFORM 3400-CHECK-LITIGATION
           IF CLOSURE-INVALID AND NOT WS-FORCE-CLOSE
               MOVE 08 TO WS-RES-RETURN-CODE
           END-IF.

       3100-CHECK-TRANSITION.
           PERFORM 5000-VALIDATE-TRANSITION.

       3200-CHECK-RESERVES.
      *    All reserves must be zero (run down) before closure.
           MOVE WS-REQ-CLAIM-NUMBER TO RF-CLAIM-NUMBER
           READ RESERVE-FILE
               INVALID KEY
                   EXIT PARAGRAPH
           END-READ
           COMPUTE WS-TOTAL-RESERVES =
               RF-MED-OUTSTANDING + RF-IND-OUTSTANDING
               + RF-EXP-OUTSTANDING
           IF WS-TOTAL-RESERVES > 0
               SET CLOSURE-INVALID TO TRUE
               STRING "Cannot close: Outstanding reserves of $"
                   WS-TOTAL-RESERVES
                   " must be zero" DELIMITED BY SIZE
                   INTO WS-RES-MESSAGE
               MOVE 08 TO WS-RES-RETURN-CODE
           END-IF.

       3300-CHECK-PAYMENTS.
      *    All payments must be reconciled.
           MOVE WS-REQ-CLAIM-NUMBER TO PF-CLAIM-NUMBER
           READ PAYMENT-FILE
               INVALID KEY
                   EXIT PARAGRAPH
           END-READ
           IF PF-UNRECONCILED-AMT > 0
               SET CLOSURE-INVALID TO TRUE
               STRING "Cannot close: Unreconciled payments of $"
                   PF-UNRECONCILED-AMT DELIMITED BY SIZE
                   INTO WS-RES-MESSAGE
               MOVE 08 TO WS-RES-RETURN-CODE
           END-IF.

       3400-CHECK-LITIGATION.
      *    Cannot close while in active litigation.
           IF CF-IN-LITIGATION
               SET CLOSURE-INVALID TO TRUE
               MOVE "Cannot close: Claim is in active litigation"
                   TO WS-RES-MESSAGE
               MOVE 08 TO WS-RES-RETURN-CODE
           END-IF
           IF CF-HEARING-SCHED
               SET CLOSURE-INVALID TO TRUE
               MOVE "Cannot close: Hearing is scheduled"
                   TO WS-RES-MESSAGE
               MOVE 08 TO WS-RES-RETURN-CODE
           END-IF.

       4000-VALIDATE-REOPEN.
           PERFORM 4100-CHECK-REOPEN-TRANSITION
           IF TRANSITION-INVALID
               EXIT PARAGRAPH
           END-IF
           PERFORM 4200-CHECK-STATUTORY-PERIOD
           PERFORM 4300-CHECK-SUPERVISOR-APPROVAL
           IF REOPEN-INVALID
               MOVE 08 TO WS-RES-RETURN-CODE
           END-IF.

       4100-CHECK-REOPEN-TRANSITION.
           PERFORM 5000-VALIDATE-TRANSITION.

       4200-CHECK-STATUTORY-PERIOD.
      *    Must be within statutory reopening period for jurisdiction.
           MOVE 3 TO WS-REOPEN-LIMIT-YRS
           PERFORM VARYING WS-REOPEN-IDX FROM 1 BY 1
               UNTIL WS-REOPEN-IDX > 10
               IF CF-INJURY-STATE =
                  WS-REOPEN-STATE(WS-REOPEN-IDX)
                   MOVE WS-REOPEN-YEARS(WS-REOPEN-IDX)
                       TO WS-REOPEN-LIMIT-YRS
               END-IF
           END-PERFORM
           MOVE CF-DATE-OF-INJURY(1:4) TO WS-DOI-YEAR
           MOVE WS-CURRENT-DATE(1:4) TO WS-CURR-YEAR
           COMPUTE WS-YEARS-SINCE-DOI =
               WS-CURR-YEAR - WS-DOI-YEAR
           IF WS-YEARS-SINCE-DOI > WS-REOPEN-LIMIT-YRS
               SET REOPEN-INVALID TO TRUE
               STRING "Cannot reopen: Exceeds " CF-INJURY-STATE
                   " statutory period of "
                   WS-REOPEN-LIMIT-YRS " years"
                   DELIMITED BY SIZE INTO WS-RES-MESSAGE
           END-IF.

       4300-CHECK-SUPERVISOR-APPROVAL.
      *    Reopening always requires supervisor approval.
           IF WS-REQ-SUPERVISOR = SPACES
               SET REOPEN-INVALID TO TRUE
               MOVE "Cannot reopen: Supervisor approval required"
                   TO WS-RES-MESSAGE
           END-IF.

       5000-VALIDATE-TRANSITION.
      *    Check transition matrix for valid from→to status.
           MOVE 'N' TO WS-TRANS-VALID
           PERFORM VARYING WS-TRANS-INDEX FROM 1 BY 1
               UNTIL WS-TRANS-INDEX > 25
               IF CF-CLAIM-STATUS =
                  WS-TRANS-FROM(WS-TRANS-INDEX)
               AND WS-REQ-NEW-STATUS =
                  WS-TRANS-TO(WS-TRANS-INDEX)
                   IF WS-IS-VALID-TRANS(WS-TRANS-INDEX)
                       MOVE 'Y' TO WS-TRANS-VALID
                   END-IF
               END-IF
           END-PERFORM
           IF TRANSITION-INVALID
               MOVE 08 TO WS-RES-RETURN-CODE
               STRING "Invalid transition from status "
                   CF-CLAIM-STATUS " to "
                   WS-REQ-NEW-STATUS DELIMITED BY SIZE
                   INTO WS-RES-MESSAGE
           END-IF.

       6000-APPLY-STATUS-CHANGE.
           MOVE CF-CLAIM-STATUS TO WS-OLD-STATUS
           MOVE WS-REQ-NEW-STATUS TO CF-CLAIM-STATUS
           MOVE WS-CURRENT-DATE TO CF-LAST-STATUS-DATE
           MOVE WS-REQ-USER-ID TO CF-LAST-STATUS-USER
           EVALUATE WS-REQ-NEW-STATUS
               WHEN 4
                   MOVE WS-CURRENT-DATE TO CF-DATE-CLOSED
                   MOVE SPACES TO CF-DATE-REOPENED
               WHEN 5
                   MOVE WS-CURRENT-DATE TO CF-DATE-REOPENED
                   MOVE SPACES TO CF-DATE-CLOSED
           END-EVALUATE
           REWRITE CLAIM-RECORD
           IF WS-CLM-STATUS NOT = '00'
               MOVE 08 TO WS-RES-RETURN-CODE
               MOVE "Failed to update claim record"
                   TO WS-RES-MESSAGE
           ELSE
               MOVE 00 TO WS-RES-RETURN-CODE
               STRING "Status changed from " WS-OLD-STATUS
                   " to " WS-REQ-NEW-STATUS
                   " for claim " WS-REQ-CLAIM-NUMBER
                   DELIMITED BY SIZE INTO WS-RES-MESSAGE
           END-IF.

       7000-WRITE-HISTORY.
           MOVE WS-REQ-CLAIM-NUMBER TO HR-CLAIM-NUMBER
           MOVE WS-CURRENT-DATE TO HR-STATUS-DATE
           MOVE WS-CURRENT-TIME TO HR-STATUS-TIME
           MOVE WS-OLD-STATUS TO HR-OLD-STATUS
           MOVE WS-REQ-NEW-STATUS TO HR-NEW-STATUS
           MOVE CF-LITIGATION-STATUS TO HR-OLD-LITIG-STATUS
           MOVE WS-REQ-NEW-LITIG TO HR-NEW-LITIG-STATUS
           MOVE CF-SETTLEMENT-STATUS TO HR-OLD-SETTLE-STATUS
           MOVE WS-REQ-NEW-SETTLE TO HR-NEW-SETTLE-STATUS
           MOVE WS-REQ-USER-ID TO HR-CHANGED-BY
           MOVE WS-REQ-SUPERVISOR TO HR-SUPERVISOR-ID
           MOVE WS-REQ-REASON-CODE TO HR-REASON-CODE
           MOVE WS-REQ-REASON-TEXT TO HR-REASON-TEXT
           WRITE HISTORY-RECORD.

       8000-HANDLE-LITIGATION.
           IF WS-REQ-NEW-LITIG > 0
           AND WS-REQ-NEW-LITIG NOT = CF-LITIGATION-STATUS
               MOVE WS-REQ-NEW-LITIG TO CF-LITIGATION-STATUS
               EVALUATE CF-LITIGATION-STATUS
                   WHEN 1
                       IF CF-CLAIM-STATUS NOT = 3
                           MOVE 3 TO CF-CLAIM-STATUS
                       END-IF
                   WHEN 3
                       SET CF-NEEDS-APPROVAL TO TRUE
               END-EVALUATE
               REWRITE CLAIM-RECORD
           END-IF.

       8500-HANDLE-SETTLEMENT.
           IF WS-REQ-NEW-SETTLE > 0
           AND WS-REQ-NEW-SETTLE NOT = CF-SETTLEMENT-STATUS
               MOVE WS-REQ-NEW-SETTLE TO CF-SETTLEMENT-STATUS
               IF CF-SETTLEMENT-ACCEPTED
                   MOVE 4 TO CF-LITIGATION-STATUS
                   REWRITE CLAIM-RECORD
               ELSE
                   REWRITE CLAIM-RECORD
               END-IF
           END-IF.

       9000-CLEANUP.
           CLOSE CLAIM-FILE
           CLOSE HISTORY-FILE
           CLOSE RESERVE-FILE
           CLOSE PAYMENT-FILE.
