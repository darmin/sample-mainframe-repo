       IDENTIFICATION DIVISION.
       PROGRAM-ID. PMTVOID.
      *================================================================
      * PMTVOID — Payment Void and Reissue Program
      *
      * Handles voiding of issued checks, reissuing replacement
      * payments, tracking outstanding check aging, processing
      * stale-dated checks (180+ days), escheatment of unclaimed
      * funds, stop payment notifications, reserve reinstatement
      * on void, and full audit trail for void/reissue chains.
      *
      * Workers' Compensation TPA — Payment Processing Subsystem
      *
      * TMF Protected: Yes — all updates within transaction scope
      * Files:
      *   CHKREG  (key-seq) — Check register
      *   CLMRSV  (key-seq) — Claim reserves (for reinstatement)
      *   STOPPAY (seq)     — Stop payment file for bank transmission
      *   ESCHEAT (key-seq) — Escheatment tracking
      *   VOIDLOG (seq)     — Void/reissue audit log
      *   CHKQUEUE(key-seq) — Reissue queue
      *================================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS PERIOD.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHECK-REGISTER-FILE
               ASSIGN TO "$DATA1.WCDATA.CHKREG"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CK-CHECK-NUMBER
               FILE STATUS IS WS-CK-STATUS.

           SELECT CLAIM-RESERVE-FILE
               ASSIGN TO "$DATA1.WCDATA.CLMRSV"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CR-CLAIM-NUMBER
               FILE STATUS IS WS-CR-STATUS.

           SELECT STOP-PAY-FILE
               ASSIGN TO "$DATA2.WCBANK.STOPPAY"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-SP-STATUS.

           SELECT ESCHEATMENT-FILE
               ASSIGN TO "$DATA1.WCDATA.ESCHEAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ES-KEY
               FILE STATUS IS WS-ES-STATUS.

           SELECT VOID-LOG-FILE
               ASSIGN TO "$DATA1.WCDATA.VOIDLOG"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-VL-STATUS.

           SELECT REISSUE-QUEUE-FILE
               ASSIGN TO "$DATA1.WCDATA.CHKQUEUE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RQ-PAYMENT-ID
               FILE STATUS IS WS-RQ-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  CHECK-REGISTER-FILE.
       01  CHECK-REGISTER-REC.
           05  CK-CHECK-NUMBER         PIC 9(8).
           05  CK-CHECK-DATE           PIC 9(8).
           05  CK-PAYMENT-ID           PIC X(10).
           05  CK-CLAIM-NUMBER         PIC X(12).
           05  CK-PAYEE-NAME           PIC X(40).
           05  CK-PAYEE-ADDR-1         PIC X(35).
           05  CK-PAYEE-ADDR-2         PIC X(35).
           05  CK-PAYEE-CITY           PIC X(25).
           05  CK-PAYEE-STATE          PIC X(2).
           05  CK-PAYEE-ZIP            PIC X(10).
           05  CK-AMOUNT               PIC S9(9)V99 COMP-3.
           05  CK-PAYMENT-TYPE         PIC 9(1).
           05  CK-PAYMENT-DESC         PIC X(40).
           05  CK-ACCOUNT-CODE         PIC X(4).
           05  CK-STATUS               PIC X(1).
               88  CK-ISSUED           VALUE "I".
               88  CK-CLEARED          VALUE "C".
               88  CK-VOIDED           VALUE "V".
               88  CK-STALE            VALUE "S".
               88  CK-ESCHEATED        VALUE "E".
           05  CK-VOID-DATE            PIC 9(8).
           05  CK-VOID-REASON-CODE     PIC X(2).
           05  CK-VOID-REASON-TEXT     PIC X(30).
           05  CK-REISSUE-CHECK        PIC 9(8).
           05  CK-ORIGINAL-CHECK       PIC 9(8).
           05  CK-VOID-USER-ID         PIC X(8).

       FD  CLAIM-RESERVE-FILE.
       01  CLAIM-RESERVE-REC.
           05  CR-CLAIM-NUMBER         PIC X(12).
           05  CR-JURISDICTION         PIC X(2).
           05  CR-INJURY-TYPE          PIC 9(2).
           05  CR-MEDICAL-RESERVE      PIC S9(9)V99 COMP-3.
           05  CR-TTD-RESERVE          PIC S9(9)V99 COMP-3.
           05  CR-TPD-RESERVE          PIC S9(9)V99 COMP-3.
           05  CR-PPD-RESERVE          PIC S9(9)V99 COMP-3.
           05  CR-PTD-RESERVE          PIC S9(9)V99 COMP-3.
           05  CR-EXPENSE-RESERVE      PIC S9(9)V99 COMP-3.
           05  CR-TOTAL-OUTSTANDING    PIC S9(9)V99 COMP-3.
           05  CR-TOTAL-PAID           PIC S9(9)V99 COMP-3.
           05  CR-TOTAL-INCURRED       PIC S9(9)V99 COMP-3.

       FD  STOP-PAY-FILE.
       01  STOP-PAY-REC.
           05  SP-RECORD-TYPE          PIC X(1).
               88  SP-HEADER           VALUE "H".
               88  SP-DETAIL           VALUE "D".
               88  SP-TRAILER          VALUE "T".
           05  SP-BANK-ACCOUNT         PIC X(17).
           05  SP-CHECK-NUMBER         PIC 9(8).
           05  SP-AMOUNT               PIC 9(10)V99.
           05  SP-ISSUE-DATE           PIC 9(8).
           05  SP-PAYEE-NAME           PIC X(40).
           05  SP-STOP-REASON          PIC X(2).
           05  SP-REQUEST-DATE         PIC 9(8).
           05  SP-FILLER               PIC X(24).

       FD  ESCHEATMENT-FILE.
       01  ESCHEATMENT-REC.
           05  ES-KEY.
               10  ES-STATE            PIC X(2).
               10  ES-CHECK-NUMBER     PIC 9(8).
           05  ES-PAYEE-NAME           PIC X(40).
           05  ES-PAYEE-ADDR-1         PIC X(35).
           05  ES-PAYEE-CITY           PIC X(25).
           05  ES-PAYEE-STATE          PIC X(2).
           05  ES-PAYEE-ZIP            PIC X(10).
           05  ES-AMOUNT               PIC S9(9)V99 COMP-3.
           05  ES-ISSUE-DATE           PIC 9(8).
           05  ES-STALE-DATE           PIC 9(8).
           05  ES-CLAIM-NUMBER         PIC X(12).
           05  ES-ESCHEAT-STATUS       PIC 9(1).
               88  ES-PENDING          VALUE 0.
               88  ES-REPORTED         VALUE 1.
               88  ES-REMITTED         VALUE 2.
               88  ES-CLAIMED          VALUE 3.
           05  ES-REPORT-DATE          PIC 9(8).
           05  ES-REMIT-DATE           PIC 9(8).
           05  ES-DORMANCY-YEARS       PIC 9(2).

       FD  VOID-LOG-FILE.
       01  VOID-LOG-REC.
           05  VL-TRANSACTION-DATE     PIC 9(8).
           05  VL-TRANSACTION-TIME     PIC 9(6).
           05  VL-ACTION               PIC X(8).
               88  VL-ACT-VOID         VALUE "VOID    ".
               88  VL-ACT-REISSUE      VALUE "REISSUE ".
               88  VL-ACT-STALE        VALUE "STALE   ".
               88  VL-ACT-ESCHEAT      VALUE "ESCHEAT ".
               88  VL-ACT-STOP-PAY     VALUE "STOP-PAY".
               88  VL-ACT-RSV-REINST   VALUE "RSV-RNST".
           05  VL-ORIGINAL-CHECK       PIC 9(8).
           05  VL-REISSUE-CHECK        PIC 9(8).
           05  VL-CLAIM-NUMBER         PIC X(12).
           05  VL-AMOUNT               PIC S9(9)V99 COMP-3.
           05  VL-REASON-CODE          PIC X(2).
           05  VL-REASON-TEXT          PIC X(30).
           05  VL-USER-ID              PIC X(8).
           05  VL-RESERVE-REINSTATED   PIC 9(1).
               88  VL-RSV-YES          VALUE 1.
               88  VL-RSV-NO           VALUE 0.

       FD  REISSUE-QUEUE-FILE.
       01  REISSUE-QUEUE-REC.
           05  RQ-PAYMENT-ID           PIC X(10).
           05  RQ-CLAIM-NUMBER         PIC X(12).
           05  RQ-PAYEE-NAME           PIC X(40).
           05  RQ-PAYEE-ADDR-1         PIC X(35).
           05  RQ-PAYEE-ADDR-2         PIC X(35).
           05  RQ-PAYEE-CITY           PIC X(25).
           05  RQ-PAYEE-STATE          PIC X(2).
           05  RQ-PAYEE-ZIP            PIC X(10).
           05  RQ-AMOUNT               PIC S9(9)V99 COMP-3.
           05  RQ-PAYMENT-TYPE         PIC 9(1).
           05  RQ-PAYMENT-DESC         PIC X(40).
           05  RQ-MULTI-PAYEE-FLAG     PIC 9(1).
           05  RQ-SPLIT-GROUP          PIC X(10).
           05  RQ-STATUS               PIC 9(1).
               88  RQ-READY            VALUE 0.
           05  RQ-ORIGINAL-CHECK       PIC 9(8).

       WORKING-STORAGE SECTION.
      *
      *    Cross-program copybook references
           COPY WCPMTCPY

       01  WS-FILE-STATUSES.
           05  WS-CK-STATUS           PIC X(2).
           05  WS-CR-STATUS           PIC X(2).
           05  WS-SP-STATUS           PIC X(2).
           05  WS-ES-STATUS           PIC X(2).
           05  WS-VL-STATUS           PIC X(2).
           05  WS-RQ-STATUS           PIC X(2).

       01  WS-CURRENT-DATE            PIC 9(8).
       01  WS-CURRENT-TIME            PIC 9(6).

       01  WS-REQUEST.
           05  WS-REQ-ACTION          PIC 9(1).
               88  WS-REQ-VOID        VALUE 1.
               88  WS-REQ-VOID-REISSUE VALUE 2.
               88  WS-REQ-STALE-SCAN  VALUE 3.
               88  WS-REQ-ESCHEAT     VALUE 4.
           05  WS-REQ-CHECK-NUMBER    PIC 9(8).
           05  WS-REQ-REASON-CODE     PIC X(2).
           05  WS-REQ-REASON-TEXT     PIC X(30).
           05  WS-REQ-USER-ID         PIC X(8).
           05  WS-REQ-REISSUE-FLAG    PIC 9(1).
               88  WS-DO-REISSUE      VALUE 1.
               88  WS-NO-REISSUE      VALUE 0.

      * Void reason codes
       01  WS-VOID-REASONS.
           05  FILLER PIC X(32) VALUE "01LOST IN MAIL                ".
           05  FILLER PIC X(32) VALUE "02DAMAGED CHECK               ".
           05  FILLER PIC X(32) VALUE "03WRONG AMOUNT                ".
           05  FILLER PIC X(32) VALUE "04WRONG PAYEE                 ".
           05  FILLER PIC X(32) VALUE "05DUPLICATE PAYMENT           ".
           05  FILLER PIC X(32) VALUE "06CLAIM CLOSED                ".
           05  FILLER PIC X(32) VALUE "07FRAUD SUSPECTED             ".
           05  FILLER PIC X(32) VALUE "08STALE DATED                 ".
           05  FILLER PIC X(32) VALUE "09STOP PAYMENT                ".
           05  FILLER PIC X(32) VALUE "10ESCHEATMENT                 ".
       01  WS-VOID-REASON-TBL REDEFINES WS-VOID-REASONS.
           05  WS-VOID-ENTRY OCCURS 10 TIMES.
               10  WS-VR-CODE         PIC X(2).
               10  WS-VR-TEXT         PIC X(30).

       01  WS-STALE-THRESHOLD-DAYS    PIC 9(3) VALUE 180.
       01  WS-DAYS-OUTSTANDING        PIC 9(5).
       01  WS-CHECK-DATE-NUM          PIC 9(8).
       01  WS-CURRENT-DATE-NUM        PIC 9(8).
       01  WS-REISSUE-PAYMENT-ID      PIC X(10).
       01  WS-REISSUE-SEQ             PIC 9(6) VALUE 0.

      * Escheatment dormancy periods by state
       01  WS-ESCHEAT-DORMANCY.
           05  FILLER PIC X(4) VALUE "CA03".
           05  FILLER PIC X(4) VALUE "NY03".
           05  FILLER PIC X(4) VALUE "TX03".
           05  FILLER PIC X(4) VALUE "FL05".
           05  FILLER PIC X(4) VALUE "IL05".
           05  FILLER PIC X(4) VALUE "PA03".
           05  FILLER PIC X(4) VALUE "OH05".
           05  FILLER PIC X(4) VALUE "  05".
       01  WS-ESCHEAT-TBL REDEFINES WS-ESCHEAT-DORMANCY.
           05  WS-ESCHEAT-ENTRY OCCURS 8 TIMES.
               10  WS-ESCH-STATE      PIC X(2).
               10  WS-ESCH-YEARS      PIC 9(2).

      * Counters
       01  WS-COUNTERS.
           05  WS-VOID-COUNT          PIC 9(6) VALUE 0.
           05  WS-REISSUE-COUNT       PIC 9(6) VALUE 0.
           05  WS-STALE-COUNT         PIC 9(6) VALUE 0.
           05  WS-ESCHEAT-COUNT       PIC 9(6) VALUE 0.
           05  WS-STOP-PAY-COUNT      PIC 9(6) VALUE 0.
           05  WS-RESERVE-REINST-AMT  PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-ERRORS              PIC 9(6) VALUE 0.

       01  WS-I                        PIC 9(2).

       PROCEDURE DIVISION.

       0000-MAIN.
      *
      *    Inter-program communication calls
           CALL "PMTPROC"
           PERFORM 1000-INITIALIZE
           EVALUATE TRUE
               WHEN WS-REQ-VOID
                   PERFORM 2000-VOID-CHECK
               WHEN WS-REQ-VOID-REISSUE
                   PERFORM 2000-VOID-CHECK
                   IF WS-DO-REISSUE
                       PERFORM 3000-REISSUE-CHECK
                   END-IF
               WHEN WS-REQ-STALE-SCAN
                   PERFORM 4000-STALE-DATE-SCAN
               WHEN WS-REQ-ESCHEAT
                   PERFORM 5000-ESCHEATMENT-PROCESS
           END-EVALUATE
           PERFORM 8000-PRINT-SUMMARY
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           OPEN I-O CHECK-REGISTER-FILE
           OPEN I-O CLAIM-RESERVE-FILE
           OPEN OUTPUT STOP-PAY-FILE
           OPEN I-O ESCHEATMENT-FILE
           OPEN EXTEND VOID-LOG-FILE
           OPEN I-O REISSUE-QUEUE-FILE
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-CURRENT-TIME

      *    Write stop pay header
           SET SP-HEADER TO TRUE
           MOVE "WC01-ACCT-1234567" TO SP-BANK-ACCOUNT
           MOVE 0 TO SP-CHECK-NUMBER
           MOVE 0 TO SP-AMOUNT
           MOVE WS-CURRENT-DATE TO SP-ISSUE-DATE
           MOVE "VOID/STOP PAY FILE" TO SP-PAYEE-NAME
           MOVE SPACES TO SP-STOP-REASON
           MOVE WS-CURRENT-DATE TO SP-REQUEST-DATE
           WRITE STOP-PAY-REC.

       2000-VOID-CHECK.
      *    Void a specific check
           MOVE WS-REQ-CHECK-NUMBER TO CK-CHECK-NUMBER
           READ CHECK-REGISTER-FILE
               INVALID KEY
                   DISPLAY "ERROR: Check " WS-REQ-CHECK-NUMBER
                       " not found in register"
                   ADD 1 TO WS-ERRORS
                   GO TO 2000-EXIT
           END-READ

      *    Validate check can be voided
           IF CK-VOIDED
               DISPLAY "ERROR: Check " CK-CHECK-NUMBER
                   " already voided"
               ADD 1 TO WS-ERRORS
               GO TO 2000-EXIT
           END-IF
           IF CK-CLEARED
               DISPLAY "WARNING: Check " CK-CHECK-NUMBER
                   " has cleared — void requires bank coordination"
           END-IF
           IF CK-ESCHEATED
               DISPLAY "ERROR: Check " CK-CHECK-NUMBER
                   " already escheated — cannot void"
               ADD 1 TO WS-ERRORS
               GO TO 2000-EXIT
           END-IF

      *    Void the check
           SET CK-VOIDED TO TRUE
           MOVE WS-CURRENT-DATE TO CK-VOID-DATE
           MOVE WS-REQ-REASON-CODE TO CK-VOID-REASON-CODE
           MOVE WS-REQ-REASON-TEXT TO CK-VOID-REASON-TEXT
           MOVE WS-REQ-USER-ID TO CK-VOID-USER-ID
           REWRITE CHECK-REGISTER-REC
               INVALID KEY
                   DISPLAY "ERROR: Cannot update check register"
                   ADD 1 TO WS-ERRORS
                   GO TO 2000-EXIT
           END-REWRITE

      *    Generate stop payment notification
           PERFORM 2100-GENERATE-STOP-PAY

      *    Reinstate reserves on the claim
           PERFORM 2200-REINSTATE-RESERVES

      *    Write audit log
           MOVE WS-CURRENT-DATE TO VL-TRANSACTION-DATE
           MOVE WS-CURRENT-TIME TO VL-TRANSACTION-TIME
           SET VL-ACT-VOID TO TRUE
           MOVE CK-CHECK-NUMBER TO VL-ORIGINAL-CHECK
           MOVE 0 TO VL-REISSUE-CHECK
           MOVE CK-CLAIM-NUMBER TO VL-CLAIM-NUMBER
           MOVE CK-AMOUNT TO VL-AMOUNT
           MOVE WS-REQ-REASON-CODE TO VL-REASON-CODE
           MOVE WS-REQ-REASON-TEXT TO VL-REASON-TEXT
           MOVE WS-REQ-USER-ID TO VL-USER-ID
           SET VL-RSV-YES TO TRUE
           WRITE VOID-LOG-REC

           ADD 1 TO WS-VOID-COUNT.

       2000-EXIT.
           EXIT.

       2100-GENERATE-STOP-PAY.
      *    Write stop payment record for bank transmission
           SET SP-DETAIL TO TRUE
           MOVE "WC01-ACCT-1234567" TO SP-BANK-ACCOUNT
           MOVE CK-CHECK-NUMBER TO SP-CHECK-NUMBER
           MOVE CK-AMOUNT TO SP-AMOUNT
           MOVE CK-CHECK-DATE TO SP-ISSUE-DATE
           MOVE CK-PAYEE-NAME TO SP-PAYEE-NAME
           MOVE WS-REQ-REASON-CODE TO SP-STOP-REASON
           MOVE WS-CURRENT-DATE TO SP-REQUEST-DATE
           WRITE STOP-PAY-REC
           ADD 1 TO WS-STOP-PAY-COUNT

      *    Log the stop pay action
           MOVE WS-CURRENT-DATE TO VL-TRANSACTION-DATE
           MOVE WS-CURRENT-TIME TO VL-TRANSACTION-TIME
           SET VL-ACT-STOP-PAY TO TRUE
           MOVE CK-CHECK-NUMBER TO VL-ORIGINAL-CHECK
           MOVE 0 TO VL-REISSUE-CHECK
           MOVE CK-CLAIM-NUMBER TO VL-CLAIM-NUMBER
           MOVE CK-AMOUNT TO VL-AMOUNT
           MOVE WS-REQ-REASON-CODE TO VL-REASON-CODE
           MOVE "STOP PAYMENT TO BANK" TO VL-REASON-TEXT
           MOVE WS-REQ-USER-ID TO VL-USER-ID
           SET VL-RSV-NO TO TRUE
           WRITE VOID-LOG-REC.

       2200-REINSTATE-RESERVES.
      *    Add voided amount back to claim reserves
           MOVE CK-CLAIM-NUMBER TO CR-CLAIM-NUMBER
           READ CLAIM-RESERVE-FILE
               INVALID KEY
                   DISPLAY "WARNING: Claim " CK-CLAIM-NUMBER
                       " not found for reserve reinstatement"
                   GO TO 2200-EXIT
           END-READ

      *    Reinstate based on payment type
           EVALUATE CK-PAYMENT-TYPE
               WHEN 1
                   ADD CK-AMOUNT TO CR-MEDICAL-RESERVE
               WHEN 2
                   ADD CK-AMOUNT TO CR-TTD-RESERVE
               WHEN 3
                   ADD CK-AMOUNT TO CR-EXPENSE-RESERVE
               WHEN 5
                   ADD CK-AMOUNT TO CR-EXPENSE-RESERVE
               WHEN OTHER
                   ADD CK-AMOUNT TO CR-EXPENSE-RESERVE
           END-EVALUATE

      *    Adjust totals
           ADD CK-AMOUNT TO CR-TOTAL-OUTSTANDING
           SUBTRACT CK-AMOUNT FROM CR-TOTAL-PAID
      *    Incurred stays the same (paid goes down, outstanding goes up)
           REWRITE CLAIM-RESERVE-REC
               INVALID KEY
                   DISPLAY "ERROR: Cannot reinstate reserves for "
                       CK-CLAIM-NUMBER
           END-REWRITE

           ADD CK-AMOUNT TO WS-RESERVE-REINST-AMT

      *    Log reserve reinstatement
           MOVE WS-CURRENT-DATE TO VL-TRANSACTION-DATE
           MOVE WS-CURRENT-TIME TO VL-TRANSACTION-TIME
           SET VL-ACT-RSV-REINST TO TRUE
           MOVE CK-CHECK-NUMBER TO VL-ORIGINAL-CHECK
           MOVE 0 TO VL-REISSUE-CHECK
           MOVE CK-CLAIM-NUMBER TO VL-CLAIM-NUMBER
           MOVE CK-AMOUNT TO VL-AMOUNT
           MOVE "  " TO VL-REASON-CODE
           MOVE "RESERVE REINSTATED ON VOID" TO VL-REASON-TEXT
           MOVE WS-REQ-USER-ID TO VL-USER-ID
           SET VL-RSV-YES TO TRUE
           WRITE VOID-LOG-REC.

       2200-EXIT.
           EXIT.

       3000-REISSUE-CHECK.
      *    Create reissue entry in check queue
           ADD 1 TO WS-REISSUE-SEQ
           STRING "RI" WS-CURRENT-DATE(3:6)
               WS-REISSUE-SEQ DELIMITED SIZE
               INTO WS-REISSUE-PAYMENT-ID

           MOVE WS-REISSUE-PAYMENT-ID TO RQ-PAYMENT-ID
           MOVE CK-CLAIM-NUMBER TO RQ-CLAIM-NUMBER
           MOVE CK-PAYEE-NAME TO RQ-PAYEE-NAME
           MOVE CK-PAYEE-ADDR-1 TO RQ-PAYEE-ADDR-1
           MOVE CK-PAYEE-ADDR-2 TO RQ-PAYEE-ADDR-2
           MOVE CK-PAYEE-CITY TO RQ-PAYEE-CITY
           MOVE CK-PAYEE-STATE TO RQ-PAYEE-STATE
           MOVE CK-PAYEE-ZIP TO RQ-PAYEE-ZIP
           MOVE CK-AMOUNT TO RQ-AMOUNT
           MOVE CK-PAYMENT-TYPE TO RQ-PAYMENT-TYPE
           STRING "REISSUE OF CHECK #" CK-CHECK-NUMBER
               DELIMITED SIZE INTO RQ-PAYMENT-DESC
           MOVE 0 TO RQ-MULTI-PAYEE-FLAG
           MOVE SPACES TO RQ-SPLIT-GROUP
           SET RQ-READY TO TRUE
           MOVE CK-CHECK-NUMBER TO RQ-ORIGINAL-CHECK

           WRITE REISSUE-QUEUE-REC
               INVALID KEY
                   DISPLAY "ERROR: Cannot queue reissue for check "
                       CK-CHECK-NUMBER
                   ADD 1 TO WS-ERRORS
                   GO TO 3000-EXIT
           END-WRITE

      *    Link original check to reissue (will be updated when printed)
           MOVE WS-REISSUE-PAYMENT-ID TO CK-VOID-REASON-TEXT

      *    Log reissue
           MOVE WS-CURRENT-DATE TO VL-TRANSACTION-DATE
           MOVE WS-CURRENT-TIME TO VL-TRANSACTION-TIME
           SET VL-ACT-REISSUE TO TRUE
           MOVE CK-CHECK-NUMBER TO VL-ORIGINAL-CHECK
           MOVE 0 TO VL-REISSUE-CHECK
           MOVE CK-CLAIM-NUMBER TO VL-CLAIM-NUMBER
           MOVE CK-AMOUNT TO VL-AMOUNT
           MOVE WS-REQ-REASON-CODE TO VL-REASON-CODE
           MOVE "REISSUE QUEUED" TO VL-REASON-TEXT
           MOVE WS-REQ-USER-ID TO VL-USER-ID
           SET VL-RSV-NO TO TRUE
           WRITE VOID-LOG-REC

           ADD 1 TO WS-REISSUE-COUNT.

       3000-EXIT.
           EXIT.

       4000-STALE-DATE-SCAN.
      *    Scan check register for stale-dated checks (180+ days)
           MOVE LOW-VALUES TO CK-CHECK-NUMBER
           START CHECK-REGISTER-FILE KEY >= CK-CHECK-NUMBER
               INVALID KEY GO TO 4000-EXIT
           END-START

           READ CHECK-REGISTER-FILE NEXT
               AT END GO TO 4000-EXIT
           END-READ

           PERFORM UNTIL WS-CK-STATUS NOT = "00"
               IF CK-ISSUED
      *            Calculate days outstanding
                   MOVE CK-CHECK-DATE TO WS-CHECK-DATE-NUM
                   MOVE WS-CURRENT-DATE TO WS-CURRENT-DATE-NUM
      *            Simplified aging: approximate days
                   COMPUTE WS-DAYS-OUTSTANDING =
                       ((WS-CURRENT-DATE-NUM / 10000 -
                         WS-CHECK-DATE-NUM / 10000) * 365)
                       + ((FUNCTION MOD(WS-CURRENT-DATE-NUM / 100, 100)
                         - FUNCTION MOD(WS-CHECK-DATE-NUM / 100, 100))
                         * 30)

                   IF WS-DAYS-OUTSTANDING >= WS-STALE-THRESHOLD-DAYS
      *                Mark as stale
                       SET CK-STALE TO TRUE
                       MOVE WS-CURRENT-DATE TO CK-VOID-DATE
                       MOVE "08" TO CK-VOID-REASON-CODE
                       MOVE "STALE DATED - 180+ DAYS"
                           TO CK-VOID-REASON-TEXT
                       MOVE "SYSTEM  " TO CK-VOID-USER-ID
                       REWRITE CHECK-REGISTER-REC

      *                Log stale check
                       MOVE WS-CURRENT-DATE TO VL-TRANSACTION-DATE
                       MOVE WS-CURRENT-TIME TO VL-TRANSACTION-TIME
                       SET VL-ACT-STALE TO TRUE
                       MOVE CK-CHECK-NUMBER TO VL-ORIGINAL-CHECK
                       MOVE 0 TO VL-REISSUE-CHECK
                       MOVE CK-CLAIM-NUMBER TO VL-CLAIM-NUMBER
                       MOVE CK-AMOUNT TO VL-AMOUNT
                       MOVE "08" TO VL-REASON-CODE
                       MOVE "STALE DATED" TO VL-REASON-TEXT
                       MOVE "SYSTEM  " TO VL-USER-ID
                       SET VL-RSV-NO TO TRUE
                       WRITE VOID-LOG-REC

                       ADD 1 TO WS-STALE-COUNT

      *                Generate stop payment for stale checks
                       PERFORM 2100-GENERATE-STOP-PAY
                   END-IF
               END-IF
               READ CHECK-REGISTER-FILE NEXT
                   AT END GO TO 4000-EXIT
               END-READ
           END-PERFORM.
       4000-EXIT.
           EXIT.

       5000-ESCHEATMENT-PROCESS.
      *    Process unclaimed funds for state escheatment
      *    Stale checks that remain uncashed beyond state dormancy period
           MOVE LOW-VALUES TO CK-CHECK-NUMBER
           START CHECK-REGISTER-FILE KEY >= CK-CHECK-NUMBER
               INVALID KEY GO TO 5000-EXIT
           END-START

           READ CHECK-REGISTER-FILE NEXT
               AT END GO TO 5000-EXIT
           END-READ

           PERFORM UNTIL WS-CK-STATUS NOT = "00"
               IF CK-STALE
                   PERFORM 5100-CHECK-DORMANCY
               END-IF
               READ CHECK-REGISTER-FILE NEXT
                   AT END GO TO 5000-EXIT
               END-READ
           END-PERFORM.
       5000-EXIT.
           EXIT.

       5100-CHECK-DORMANCY.
      *    Determine if stale check has exceeded state dormancy period
           MOVE 5 TO WS-I
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 8
               IF CK-PAYEE-STATE = WS-ESCH-STATE(WS-I)
                   OR WS-ESCH-STATE(WS-I) = SPACES
      *            Calculate years since stale date
                   COMPUTE WS-DAYS-OUTSTANDING =
                       (WS-CURRENT-DATE-NUM / 10000)
                       - (CK-VOID-DATE / 10000)
                   IF WS-DAYS-OUTSTANDING >= WS-ESCH-YEARS(WS-I)
      *                Create escheatment record
                       MOVE CK-PAYEE-STATE TO ES-STATE
                       MOVE CK-CHECK-NUMBER TO ES-CHECK-NUMBER
                       MOVE CK-PAYEE-NAME TO ES-PAYEE-NAME
                       MOVE CK-PAYEE-ADDR-1 TO ES-PAYEE-ADDR-1
                       MOVE CK-PAYEE-CITY TO ES-PAYEE-CITY
                       MOVE CK-PAYEE-STATE TO ES-PAYEE-STATE
                       MOVE CK-PAYEE-ZIP TO ES-PAYEE-ZIP
                       MOVE CK-AMOUNT TO ES-AMOUNT
                       MOVE CK-CHECK-DATE TO ES-ISSUE-DATE
                       MOVE CK-VOID-DATE TO ES-STALE-DATE
                       MOVE CK-CLAIM-NUMBER TO ES-CLAIM-NUMBER
                       SET ES-PENDING TO TRUE
                       MOVE ZEROS TO ES-REPORT-DATE
                       MOVE ZEROS TO ES-REMIT-DATE
                       MOVE WS-ESCH-YEARS(WS-I) TO ES-DORMANCY-YEARS
                       WRITE ESCHEATMENT-REC
                           INVALID KEY
                               CONTINUE
                       END-WRITE

      *                Mark check as escheated
                       SET CK-ESCHEATED TO TRUE
                       REWRITE CHECK-REGISTER-REC

      *                Log escheatment
                       MOVE WS-CURRENT-DATE TO VL-TRANSACTION-DATE
                       MOVE WS-CURRENT-TIME TO VL-TRANSACTION-TIME
                       SET VL-ACT-ESCHEAT TO TRUE
                       MOVE CK-CHECK-NUMBER TO VL-ORIGINAL-CHECK
                       MOVE 0 TO VL-REISSUE-CHECK
                       MOVE CK-CLAIM-NUMBER TO VL-CLAIM-NUMBER
                       MOVE CK-AMOUNT TO VL-AMOUNT
                       MOVE "10" TO VL-REASON-CODE
                       MOVE "ESCHEATMENT - UNCLAIMED FUNDS"
                           TO VL-REASON-TEXT
                       MOVE "SYSTEM  " TO VL-USER-ID
                       SET VL-RSV-NO TO TRUE
                       WRITE VOID-LOG-REC

                       ADD 1 TO WS-ESCHEAT-COUNT
                   END-IF
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       8000-PRINT-SUMMARY.
           DISPLAY "=== VOID/REISSUE PROCESSING SUMMARY ==="
           DISPLAY "Date:                  " WS-CURRENT-DATE
           DISPLAY "Checks voided:         " WS-VOID-COUNT
           DISPLAY "Checks reissued:       " WS-REISSUE-COUNT
           DISPLAY "Stale-dated:           " WS-STALE-COUNT
           DISPLAY "Escheated:             " WS-ESCHEAT-COUNT
           DISPLAY "Stop payments:         " WS-STOP-PAY-COUNT
           DISPLAY "Reserves reinstated:   " WS-RESERVE-REINST-AMT
           DISPLAY "Errors:                " WS-ERRORS.

       9000-TERMINATE.
      *    Write stop pay trailer
           SET SP-TRAILER TO TRUE
           MOVE WS-STOP-PAY-COUNT TO SP-CHECK-NUMBER
           MOVE WS-CURRENT-DATE TO SP-REQUEST-DATE
           WRITE STOP-PAY-REC

           CLOSE CHECK-REGISTER-FILE
           CLOSE CLAIM-RESERVE-FILE
           CLOSE STOP-PAY-FILE
           CLOSE ESCHEATMENT-FILE
           CLOSE VOID-LOG-FILE
           CLOSE REISSUE-QUEUE-FILE.
