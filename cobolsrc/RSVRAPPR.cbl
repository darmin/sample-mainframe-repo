       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSVRAPPR.
      *================================================================
      * RSVRAPPR — Reserve Approval Workflow
      *
      * Implements multi-level authority checking for reserve changes
      * in workers' compensation claims. Routes approvals based on
      * dollar magnitude, enforces authority limits, maintains full
      * audit trail, and schedules mandatory 90-day reviews.
      *
      * Pathway: Called via PATHSEND from screen handler
      * TMF: Yes — all reserve updates are transactional
      * Files: CLMRSV (key-seq), AUTHLEVL (key-seq),
      *        RSVHIST (relative), DIARY (key-seq)
      *================================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           FUNCTION-POINTER IS FPTR.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIM-RESERVE-FILE
               ASSIGN TO "$DATA1.WCDATA.CLMRSV"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CR-CLAIM-NUMBER
               FILE STATUS IS WS-CR-STATUS.

           SELECT AUTHORITY-FILE
               ASSIGN TO "$DATA1.WCDATA.AUTHLEVL"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AU-USER-ID
               FILE STATUS IS WS-AU-STATUS.

           SELECT RESERVE-HISTORY-FILE
               ASSIGN TO "$DATA1.WCDATA.RSVHIST"
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RH-STATUS.

           SELECT DIARY-FILE
               ASSIGN TO "$DATA1.WCDATA.DIARY"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DY-KEY
               FILE STATUS IS WS-DY-STATUS.

       DATA DIVISION.
       FILE SECTION.

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
           05  CR-RESERVE-STATUS       PIC 9(1).
           05  CR-ADJUSTER-ID          PIC X(8).
           05  CR-LAST-REVIEW-DATE     PIC 9(8).
           05  CR-NEXT-REVIEW-DATE     PIC 9(8).
           05  CR-PENDING-APPROVAL     PIC 9(1).
               88  CR-NO-PENDING       VALUE 0.
               88  CR-PENDING-SUPV     VALUE 1.
               88  CR-PENDING-MGR      VALUE 2.
               88  CR-PENDING-VP       VALUE 3.

       FD  AUTHORITY-FILE.
       01  AUTHORITY-REC.
           05  AU-USER-ID             PIC X(8).
           05  AU-USER-NAME           PIC X(30).
           05  AU-ROLE-CODE           PIC 9(1).
               88  AU-ADJUSTER        VALUE 1.
               88  AU-SUPERVISOR      VALUE 2.
               88  AU-MANAGER         VALUE 3.
               88  AU-VP              VALUE 4.
               88  AU-EXECUTIVE       VALUE 5.
           05  AU-MEDICAL-LIMIT       PIC S9(9)V99 COMP-3.
           05  AU-INDEMNITY-LIMIT     PIC S9(9)V99 COMP-3.
           05  AU-EXPENSE-LIMIT       PIC S9(9)V99 COMP-3.
           05  AU-TOTAL-LIMIT         PIC S9(9)V99 COMP-3.
           05  AU-INCREASE-LIMIT      PIC S9(9)V99 COMP-3.
           05  AU-SUPERVISOR-ID       PIC X(8).
           05  AU-MANAGER-ID          PIC X(8).
           05  AU-VP-ID               PIC X(8).

       FD  RESERVE-HISTORY-FILE.
       01  RESERVE-HISTORY-REC.
           05  RH-CLAIM-NUMBER        PIC X(12).
           05  RH-TRANSACTION-DATE    PIC 9(8).
           05  RH-TRANSACTION-TIME    PIC 9(6).
           05  RH-CATEGORY            PIC X(3).
               88  RH-CAT-MEDICAL     VALUE "MED".
               88  RH-CAT-TTD         VALUE "TTD".
               88  RH-CAT-TPD         VALUE "TPD".
               88  RH-CAT-PPD         VALUE "PPD".
               88  RH-CAT-PTD         VALUE "PTD".
               88  RH-CAT-EXPENSE     VALUE "EXP".
           05  RH-BEFORE-AMOUNT       PIC S9(9)V99 COMP-3.
           05  RH-AFTER-AMOUNT        PIC S9(9)V99 COMP-3.
           05  RH-CHANGE-AMOUNT       PIC S9(9)V99 COMP-3.
           05  RH-CHANGE-REASON       PIC X(30).
           05  RH-REQUESTED-BY        PIC X(8).
           05  RH-APPROVED-BY         PIC X(8).
           05  RH-APPROVAL-STATUS     PIC X(1).
               88  RH-AUTO-APPROVED   VALUE "A".
               88  RH-MANUAL-APPROVED VALUE "M".
               88  RH-PENDING         VALUE "P".
               88  RH-DENIED          VALUE "D".

       FD  DIARY-FILE.
       01  DIARY-REC.
           05  DY-KEY.
               10  DY-USER-ID         PIC X(8).
               10  DY-DATE            PIC 9(8).
               10  DY-SEQ             PIC 9(4).
           05  DY-CLAIM-NUMBER        PIC X(12).
           05  DY-MESSAGE             PIC X(80).
           05  DY-PRIORITY            PIC 9(1).
               88  DY-NORMAL          VALUE 1.
               88  DY-URGENT          VALUE 2.
               88  DY-CRITICAL        VALUE 3.
           05  DY-STATUS              PIC 9(1).
               88  DY-UNREAD          VALUE 0.
               88  DY-READ            VALUE 1.
               88  DY-ACTIONED        VALUE 2.

       WORKING-STORAGE SECTION.

       01  WS-CR-STATUS               PIC X(2).
       01  WS-AU-STATUS               PIC X(2).
       01  WS-RH-STATUS               PIC X(2).
       01  WS-DY-STATUS               PIC X(2).

       01  WS-CURRENT-DATE.
           05  WS-CURR-YEAR           PIC 9(4).
           05  WS-CURR-MONTH          PIC 9(2).
           05  WS-CURR-DAY            PIC 9(2).
       01  WS-CURRENT-TIME.
           05  WS-CURR-HOUR           PIC 9(2).
           05  WS-CURR-MIN            PIC 9(2).
           05  WS-CURR-SEC            PIC 9(2).

       01  WS-REQUEST.
           05  WS-REQ-TYPE            PIC 9(1).
               88  WS-REQ-CHANGE      VALUE 1.
               88  WS-REQ-APPROVE     VALUE 2.
               88  WS-REQ-DENY        VALUE 3.
               88  WS-REQ-REVIEW      VALUE 4.
           05  WS-REQ-CLAIM           PIC X(12).
           05  WS-REQ-USER            PIC X(8).
           05  WS-REQ-CATEGORY        PIC X(3).
           05  WS-REQ-NEW-AMOUNT      PIC S9(9)V99 COMP-3.
           05  WS-REQ-REASON          PIC X(30).

       01  WS-RESPONSE.
           05  WS-RESP-STATUS         PIC 9(2).
               88  WS-RESP-SUCCESS    VALUE 00.
               88  WS-RESP-AUTO-APPR  VALUE 01.
               88  WS-RESP-PEND-SUPV  VALUE 10.
               88  WS-RESP-PEND-MGR   VALUE 11.
               88  WS-RESP-PEND-VP    VALUE 12.
               88  WS-RESP-DENIED     VALUE 20.
               88  WS-RESP-NOT-FOUND  VALUE 30.
               88  WS-RESP-AUTH-ERR   VALUE 40.
               88  WS-RESP-FILE-ERR   VALUE 99.
           05  WS-RESP-MESSAGE        PIC X(60).
           05  WS-RESP-APPROVER       PIC X(8).

       01  WS-AUTHORITY-CHECK.
           05  WS-CURRENT-AMOUNT      PIC S9(9)V99 COMP-3.
           05  WS-CHANGE-AMOUNT       PIC S9(9)V99 COMP-3.
           05  WS-ABS-CHANGE          PIC S9(9)V99 COMP-3.
           05  WS-NEW-TOTAL           PIC S9(9)V99 COMP-3.
           05  WS-AUTH-LEVEL-NEEDED   PIC 9(1).
           05  WS-APPROVED-FLAG       PIC 9(1).
               88  WS-IS-APPROVED     VALUE 1.
               88  WS-NOT-APPROVED    VALUE 0.

      * Authority level thresholds
       01  WS-THRESHOLD-TABLE.
           05  WS-ADJUSTER-THRESHOLD  PIC S9(9)V99 COMP-3
                                      VALUE 10000.00.
           05  WS-SUPV-THRESHOLD      PIC S9(9)V99 COMP-3
                                      VALUE 50000.00.
           05  WS-MGR-THRESHOLD       PIC S9(9)V99 COMP-3
                                      VALUE 250000.00.
           05  WS-VP-THRESHOLD        PIC S9(9)V99 COMP-3
                                      VALUE 1000000.00.

      * Increase-specific thresholds (increases require higher approval)
       01  WS-INCREASE-THRESHOLDS.
           05  WS-INCR-ADJUSTER       PIC S9(9)V99 COMP-3
                                      VALUE 5000.00.
           05  WS-INCR-SUPV           PIC S9(9)V99 COMP-3
                                      VALUE 25000.00.
           05  WS-INCR-MGR            PIC S9(9)V99 COMP-3
                                      VALUE 100000.00.

       01  WS-REVIEW-INTERVAL         PIC 9(3) VALUE 90.
       01  WS-NEXT-REVIEW             PIC 9(8).
       01  WS-DIARY-SEQ               PIC 9(4).

       PROCEDURE DIVISION.

       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-REQUEST
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           OPEN I-O CLAIM-RESERVE-FILE
           OPEN INPUT AUTHORITY-FILE
           OPEN EXTEND RESERVE-HISTORY-FILE
           OPEN I-O DIARY-FILE
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-CURRENT-TIME.

       2000-PROCESS-REQUEST.
           EVALUATE TRUE
               WHEN WS-REQ-CHANGE
                   PERFORM 3000-PROCESS-RESERVE-CHANGE
               WHEN WS-REQ-APPROVE
                   PERFORM 4000-PROCESS-APPROVAL
               WHEN WS-REQ-DENY
                   PERFORM 5000-PROCESS-DENIAL
               WHEN WS-REQ-REVIEW
                   PERFORM 6000-SCHEDULE-REVIEW
               WHEN OTHER
                   MOVE 99 TO WS-RESP-STATUS
                   MOVE "Invalid request type" TO WS-RESP-MESSAGE
           END-EVALUATE.

       3000-PROCESS-RESERVE-CHANGE.
      *    Read current claim reserves
           MOVE WS-REQ-CLAIM TO CR-CLAIM-NUMBER
           READ CLAIM-RESERVE-FILE
               INVALID KEY
                   MOVE 30 TO WS-RESP-STATUS
                   MOVE "Claim not found" TO WS-RESP-MESSAGE
                   GO TO 3000-EXIT
           END-READ

      *    Determine current amount for the category
           PERFORM 3100-GET-CURRENT-AMOUNT

      *    Calculate change magnitude
           COMPUTE WS-CHANGE-AMOUNT =
               WS-REQ-NEW-AMOUNT - WS-CURRENT-AMOUNT
           IF WS-CHANGE-AMOUNT < 0
               COMPUTE WS-ABS-CHANGE = WS-CHANGE-AMOUNT * -1
           ELSE
               MOVE WS-CHANGE-AMOUNT TO WS-ABS-CHANGE
           END-IF

      *    Determine required authority level
           PERFORM 3200-DETERMINE-AUTH-LEVEL

      *    Check requestor's authority
           MOVE WS-REQ-USER TO AU-USER-ID
           READ AUTHORITY-FILE
               INVALID KEY
                   MOVE 40 TO WS-RESP-STATUS
                   MOVE "User not found in authority table"
                       TO WS-RESP-MESSAGE
                   GO TO 3000-EXIT
           END-READ

      *    Does user have sufficient authority?
           PERFORM 3300-CHECK-AUTHORITY

           IF WS-IS-APPROVED
      *        Auto-approve — apply immediately
               PERFORM 3400-APPLY-RESERVE-CHANGE
               PERFORM 3500-WRITE-HISTORY-APPROVED
               MOVE 01 TO WS-RESP-STATUS
               MOVE "Reserve change auto-approved within authority"
                   TO WS-RESP-MESSAGE
           ELSE
      *        Route to appropriate approver
               PERFORM 3600-ROUTE-FOR-APPROVAL
           END-IF.

       3000-EXIT.
           EXIT.

       3100-GET-CURRENT-AMOUNT.
           EVALUATE WS-REQ-CATEGORY
               WHEN "MED"
                   MOVE CR-MEDICAL-RESERVE TO WS-CURRENT-AMOUNT
               WHEN "TTD"
                   MOVE CR-TTD-RESERVE TO WS-CURRENT-AMOUNT
               WHEN "TPD"
                   MOVE CR-TPD-RESERVE TO WS-CURRENT-AMOUNT
               WHEN "PPD"
                   MOVE CR-PPD-RESERVE TO WS-CURRENT-AMOUNT
               WHEN "PTD"
                   MOVE CR-PTD-RESERVE TO WS-CURRENT-AMOUNT
               WHEN "EXP"
                   MOVE CR-EXPENSE-RESERVE TO WS-CURRENT-AMOUNT
           END-EVALUATE.

       3200-DETERMINE-AUTH-LEVEL.
      *    For increases, use stricter thresholds
           IF WS-CHANGE-AMOUNT > 0
               EVALUATE TRUE
                   WHEN WS-ABS-CHANGE <= WS-INCR-ADJUSTER
                       MOVE 1 TO WS-AUTH-LEVEL-NEEDED
                   WHEN WS-ABS-CHANGE <= WS-INCR-SUPV
                       MOVE 2 TO WS-AUTH-LEVEL-NEEDED
                   WHEN WS-ABS-CHANGE <= WS-INCR-MGR
                       MOVE 3 TO WS-AUTH-LEVEL-NEEDED
                   WHEN OTHER
                       MOVE 4 TO WS-AUTH-LEVEL-NEEDED
               END-EVALUATE
           ELSE
      *        For decreases, use standard thresholds
               EVALUATE TRUE
                   WHEN WS-ABS-CHANGE <= WS-ADJUSTER-THRESHOLD
                       MOVE 1 TO WS-AUTH-LEVEL-NEEDED
                   WHEN WS-ABS-CHANGE <= WS-SUPV-THRESHOLD
                       MOVE 2 TO WS-AUTH-LEVEL-NEEDED
                   WHEN WS-ABS-CHANGE <= WS-MGR-THRESHOLD
                       MOVE 3 TO WS-AUTH-LEVEL-NEEDED
                   WHEN OTHER
                       MOVE 4 TO WS-AUTH-LEVEL-NEEDED
               END-EVALUATE
           END-IF

      *    Also check new total outstanding against limits
           COMPUTE WS-NEW-TOTAL = CR-TOTAL-OUTSTANDING
               + WS-CHANGE-AMOUNT
           EVALUATE TRUE
               WHEN WS-NEW-TOTAL > WS-VP-THRESHOLD
                   IF WS-AUTH-LEVEL-NEEDED < 4
                       MOVE 4 TO WS-AUTH-LEVEL-NEEDED
                   END-IF
               WHEN WS-NEW-TOTAL > WS-MGR-THRESHOLD
                   IF WS-AUTH-LEVEL-NEEDED < 3
                       MOVE 3 TO WS-AUTH-LEVEL-NEEDED
                   END-IF
           END-EVALUATE.

       3300-CHECK-AUTHORITY.
           SET WS-NOT-APPROVED TO TRUE
           EVALUATE TRUE
               WHEN AU-ADJUSTER
                   IF WS-AUTH-LEVEL-NEEDED <= 1
                       IF WS-ABS-CHANGE <= AU-INCREASE-LIMIT
                           SET WS-IS-APPROVED TO TRUE
                       END-IF
                   END-IF
               WHEN AU-SUPERVISOR
                   IF WS-AUTH-LEVEL-NEEDED <= 2
                       SET WS-IS-APPROVED TO TRUE
                   END-IF
               WHEN AU-MANAGER
                   IF WS-AUTH-LEVEL-NEEDED <= 3
                       SET WS-IS-APPROVED TO TRUE
                   END-IF
               WHEN AU-VP
                   SET WS-IS-APPROVED TO TRUE
               WHEN AU-EXECUTIVE
                   SET WS-IS-APPROVED TO TRUE
           END-EVALUATE.

       3400-APPLY-RESERVE-CHANGE.
           EVALUATE WS-REQ-CATEGORY
               WHEN "MED"
                   MOVE WS-REQ-NEW-AMOUNT TO CR-MEDICAL-RESERVE
               WHEN "TTD"
                   MOVE WS-REQ-NEW-AMOUNT TO CR-TTD-RESERVE
               WHEN "TPD"
                   MOVE WS-REQ-NEW-AMOUNT TO CR-TPD-RESERVE
               WHEN "PPD"
                   MOVE WS-REQ-NEW-AMOUNT TO CR-PPD-RESERVE
               WHEN "PTD"
                   MOVE WS-REQ-NEW-AMOUNT TO CR-PTD-RESERVE
               WHEN "EXP"
                   MOVE WS-REQ-NEW-AMOUNT TO CR-EXPENSE-RESERVE
           END-EVALUATE

      *    Recalculate totals
           COMPUTE CR-TOTAL-OUTSTANDING =
               CR-MEDICAL-RESERVE + CR-TTD-RESERVE +
               CR-TPD-RESERVE + CR-PPD-RESERVE +
               CR-PTD-RESERVE + CR-EXPENSE-RESERVE
           COMPUTE CR-TOTAL-INCURRED =
               CR-TOTAL-PAID + CR-TOTAL-OUTSTANDING

           MOVE WS-CURRENT-DATE TO CR-LAST-REVIEW-DATE
           PERFORM 6100-CALC-NEXT-REVIEW
           MOVE WS-NEXT-REVIEW TO CR-NEXT-REVIEW-DATE
           SET CR-NO-PENDING TO TRUE

           REWRITE CLAIM-RESERVE-REC
               INVALID KEY
                   MOVE 99 TO WS-RESP-STATUS
                   MOVE "Failed to update claim reserve record"
                       TO WS-RESP-MESSAGE
           END-REWRITE.

       3500-WRITE-HISTORY-APPROVED.
           MOVE WS-REQ-CLAIM TO RH-CLAIM-NUMBER
           MOVE WS-CURRENT-DATE TO RH-TRANSACTION-DATE
           MOVE WS-CURRENT-TIME TO RH-TRANSACTION-TIME
           MOVE WS-REQ-CATEGORY TO RH-CATEGORY
           MOVE WS-CURRENT-AMOUNT TO RH-BEFORE-AMOUNT
           MOVE WS-REQ-NEW-AMOUNT TO RH-AFTER-AMOUNT
           MOVE WS-CHANGE-AMOUNT TO RH-CHANGE-AMOUNT
           MOVE WS-REQ-REASON TO RH-CHANGE-REASON
           MOVE WS-REQ-USER TO RH-REQUESTED-BY
           MOVE WS-REQ-USER TO RH-APPROVED-BY
           SET RH-AUTO-APPROVED TO TRUE
           WRITE RESERVE-HISTORY-REC.

       3600-ROUTE-FOR-APPROVAL.
      *    Set pending approval flag on claim
           EVALUATE WS-AUTH-LEVEL-NEEDED
               WHEN 2
                   SET CR-PENDING-SUPV TO TRUE
                   MOVE AU-SUPERVISOR-ID TO WS-RESP-APPROVER
                   MOVE 10 TO WS-RESP-STATUS
                   STRING "Routed to supervisor "
                       AU-SUPERVISOR-ID DELIMITED SIZE
                       " for approval" DELIMITED SIZE
                       INTO WS-RESP-MESSAGE
               WHEN 3
                   SET CR-PENDING-MGR TO TRUE
                   MOVE AU-MANAGER-ID TO WS-RESP-APPROVER
                   MOVE 11 TO WS-RESP-STATUS
                   STRING "Routed to manager "
                       AU-MANAGER-ID DELIMITED SIZE
                       " for approval" DELIMITED SIZE
                       INTO WS-RESP-MESSAGE
               WHEN 4
                   SET CR-PENDING-VP TO TRUE
                   MOVE AU-VP-ID TO WS-RESP-APPROVER
                   MOVE 12 TO WS-RESP-STATUS
                   STRING "Routed to VP "
                       AU-VP-ID DELIMITED SIZE
                       " for approval" DELIMITED SIZE
                       INTO WS-RESP-MESSAGE
           END-EVALUATE

           REWRITE CLAIM-RESERVE-REC

      *    Write pending history record
           MOVE WS-REQ-CLAIM TO RH-CLAIM-NUMBER
           MOVE WS-CURRENT-DATE TO RH-TRANSACTION-DATE
           MOVE WS-CURRENT-TIME TO RH-TRANSACTION-TIME
           MOVE WS-REQ-CATEGORY TO RH-CATEGORY
           MOVE WS-CURRENT-AMOUNT TO RH-BEFORE-AMOUNT
           MOVE WS-REQ-NEW-AMOUNT TO RH-AFTER-AMOUNT
           MOVE WS-CHANGE-AMOUNT TO RH-CHANGE-AMOUNT
           MOVE WS-REQ-REASON TO RH-CHANGE-REASON
           MOVE WS-REQ-USER TO RH-REQUESTED-BY
           MOVE SPACES TO RH-APPROVED-BY
           SET RH-PENDING TO TRUE
           WRITE RESERVE-HISTORY-REC

      *    Create diary entry for approver
           PERFORM 7000-CREATE-DIARY-ENTRY.

       4000-PROCESS-APPROVAL.
      *    Approver approves a pending reserve change
           MOVE WS-REQ-CLAIM TO CR-CLAIM-NUMBER
           READ CLAIM-RESERVE-FILE
               INVALID KEY
                   MOVE 30 TO WS-RESP-STATUS
                   MOVE "Claim not found" TO WS-RESP-MESSAGE
                   GO TO 4000-EXIT
           END-READ

           IF CR-NO-PENDING
               MOVE 20 TO WS-RESP-STATUS
               MOVE "No pending reserve change on this claim"
                   TO WS-RESP-MESSAGE
               GO TO 4000-EXIT
           END-IF

      *    Verify approver has authority for this level
           MOVE WS-REQ-USER TO AU-USER-ID
           READ AUTHORITY-FILE
               INVALID KEY
                   MOVE 40 TO WS-RESP-STATUS
                   MOVE "Approver not in authority table"
                       TO WS-RESP-MESSAGE
                   GO TO 4000-EXIT
           END-READ

           EVALUATE TRUE
               WHEN CR-PENDING-SUPV
                   IF NOT AU-SUPERVISOR AND NOT AU-MANAGER
                       AND NOT AU-VP AND NOT AU-EXECUTIVE
                       MOVE 40 TO WS-RESP-STATUS
                       MOVE "Insufficient authority to approve"
                           TO WS-RESP-MESSAGE
                       GO TO 4000-EXIT
                   END-IF
               WHEN CR-PENDING-MGR
                   IF NOT AU-MANAGER AND NOT AU-VP
                       AND NOT AU-EXECUTIVE
                       MOVE 40 TO WS-RESP-STATUS
                       MOVE "Manager+ authority required"
                           TO WS-RESP-MESSAGE
                       GO TO 4000-EXIT
                   END-IF
               WHEN CR-PENDING-VP
                   IF NOT AU-VP AND NOT AU-EXECUTIVE
                       MOVE 40 TO WS-RESP-STATUS
                       MOVE "VP+ authority required"
                           TO WS-RESP-MESSAGE
                       GO TO 4000-EXIT
                   END-IF
           END-EVALUATE

      *    Apply the pending change
           PERFORM 3100-GET-CURRENT-AMOUNT
           PERFORM 3400-APPLY-RESERVE-CHANGE

           MOVE 00 TO WS-RESP-STATUS
           MOVE "Reserve change approved and applied"
               TO WS-RESP-MESSAGE.

       4000-EXIT.
           EXIT.

       5000-PROCESS-DENIAL.
      *    Approver denies a pending reserve change
           MOVE WS-REQ-CLAIM TO CR-CLAIM-NUMBER
           READ CLAIM-RESERVE-FILE
               INVALID KEY
                   MOVE 30 TO WS-RESP-STATUS
                   MOVE "Claim not found" TO WS-RESP-MESSAGE
                   GO TO 5000-EXIT
           END-READ

           SET CR-NO-PENDING TO TRUE
           REWRITE CLAIM-RESERVE-REC

           MOVE 00 TO WS-RESP-STATUS
           MOVE "Reserve change denied" TO WS-RESP-MESSAGE

      *    Notify requestor via diary
           PERFORM 7000-CREATE-DIARY-ENTRY.

       5000-EXIT.
           EXIT.

       6000-SCHEDULE-REVIEW.
      *    Schedule mandatory 90-day reserve review
           MOVE WS-REQ-CLAIM TO CR-CLAIM-NUMBER
           READ CLAIM-RESERVE-FILE
               INVALID KEY
                   MOVE 30 TO WS-RESP-STATUS
                   MOVE "Claim not found" TO WS-RESP-MESSAGE
                   GO TO 6000-EXIT
           END-READ

           PERFORM 6100-CALC-NEXT-REVIEW
           MOVE WS-NEXT-REVIEW TO CR-NEXT-REVIEW-DATE
           REWRITE CLAIM-RESERVE-REC

      *    Create diary entry for adjuster
           MOVE CR-ADJUSTER-ID TO DY-USER-ID
           MOVE WS-NEXT-REVIEW TO DY-DATE
           ADD 1 TO WS-DIARY-SEQ
           MOVE WS-DIARY-SEQ TO DY-SEQ
           MOVE WS-REQ-CLAIM TO DY-CLAIM-NUMBER
           STRING "MANDATORY 90-DAY RESERVE REVIEW DUE - Claim "
               WS-REQ-CLAIM DELIMITED SIZE
               INTO DY-MESSAGE
           SET DY-URGENT TO TRUE
           SET DY-UNREAD TO TRUE
           WRITE DIARY-REC

           MOVE 00 TO WS-RESP-STATUS
           MOVE "Review scheduled" TO WS-RESP-MESSAGE.

       6000-EXIT.
           EXIT.

       6100-CALC-NEXT-REVIEW.
      *    Calculate next review date (current + 90 days)
      *    Simplified: add 3 months
           MOVE WS-CURRENT-DATE TO WS-NEXT-REVIEW
           ADD 3 TO WS-CURR-MONTH
           IF WS-CURR-MONTH > 12
               SUBTRACT 12 FROM WS-CURR-MONTH
               ADD 1 TO WS-CURR-YEAR
           END-IF
           STRING WS-CURR-YEAR WS-CURR-MONTH WS-CURR-DAY
               DELIMITED SIZE INTO WS-NEXT-REVIEW.

       7000-CREATE-DIARY-ENTRY.
           MOVE WS-RESP-APPROVER TO DY-USER-ID
           MOVE WS-CURRENT-DATE TO DY-DATE
           ADD 1 TO WS-DIARY-SEQ
           MOVE WS-DIARY-SEQ TO DY-SEQ
           MOVE WS-REQ-CLAIM TO DY-CLAIM-NUMBER
           STRING "RESERVE APPROVAL REQUIRED - Claim "
               WS-REQ-CLAIM DELIMITED SIZE
               " change $" DELIMITED SIZE
               INTO DY-MESSAGE
           SET DY-URGENT TO TRUE
           SET DY-UNREAD TO TRUE
           WRITE DIARY-REC.

       9000-TERMINATE.
           CLOSE CLAIM-RESERVE-FILE
           CLOSE AUTHORITY-FILE
           CLOSE RESERVE-HISTORY-FILE
           CLOSE DIARY-FILE.
