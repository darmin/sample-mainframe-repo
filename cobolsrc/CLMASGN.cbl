       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMASGN.
      ******************************************************************
      *  CLMASGN - Claim Assignment Program
      *  Workers' Compensation TPA System
      *  HPE NonStop COBOL (Tandem COBOL85)
      *
      *  Auto-assigns claims to adjusters based on jurisdiction,
      *  injury severity, claim type, and employer size. Supports
      *  workload balancing, supervisor override, team-based
      *  assignment, re-assignment workflow, and notifications.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. TANDEM.
       OBJECT-COMPUTER. TANDEM.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ADJUSTER-FILE ASSIGN TO "$ADJFL"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AF-ADJUSTER-ID
               ALTERNATE RECORD KEY IS AF-JURIS-TEAM
                   WITH DUPLICATES
               FILE STATUS IS WS-ADJ-STATUS.

           SELECT WORKLOAD-FILE ASSIGN TO "$WKLD"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS WF-ADJUSTER-ID
               FILE STATUS IS WS-WKL-STATUS.

           SELECT ASSIGNMENT-LOG ASSIGN TO "$ASGNLOG"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-LOG-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  ADJUSTER-FILE.
       01  ADJUSTER-RECORD.
           05  AF-ADJUSTER-ID      PIC X(8).
           05  AF-ADJUSTER-NAME    PIC X(40).
           05  AF-JURIS-TEAM.
               10  AF-JURISDICTION  PIC X(2).
               10  AF-TEAM-CODE    PIC 9.
                   88  AF-MEDICAL-ONLY-TEAM  VALUE 1.
                   88  AF-LOST-TIME-TEAM     VALUE 2.
                   88  AF-LITIGATION-TEAM    VALUE 3.
                   88  AF-COMPLEX-TEAM       VALUE 4.
                   88  AF-FATALITY-TEAM      VALUE 5.
           05  AF-SUPERVISOR-ID    PIC X(8).
           05  AF-MAX-CASELOAD     PIC 9(4).
           05  AF-CURRENT-OPEN     PIC 9(4).
           05  AF-ACTIVE-FLAG      PIC X.
               88  AF-IS-ACTIVE          VALUE 'Y'.
               88  AF-IS-INACTIVE        VALUE 'N'.
           05  AF-SPECIALTIES.
               10  AF-HANDLES-FATALITY   PIC X VALUE 'N'.
                   88  AF-CAN-FATALITY       VALUE 'Y'.
               10  AF-HANDLES-OCC-DIS    PIC X VALUE 'N'.
                   88  AF-CAN-OCC-DISEASE    VALUE 'Y'.
               10  AF-HANDLES-LITIG      PIC X VALUE 'N'.
                   88  AF-CAN-LITIGATE       VALUE 'Y'.
           05  AF-YEARS-EXPERIENCE PIC 9(2).
           05  AF-AVG-CLOSURE-DAYS PIC 9(5).

       FD  WORKLOAD-FILE.
       01  WORKLOAD-RECORD.
           05  WF-ADJUSTER-ID      PIC X(8).
           05  WF-MED-ONLY-COUNT   PIC 9(4).
           05  WF-LOST-TIME-COUNT  PIC 9(4).
           05  WF-LITIG-COUNT      PIC 9(4).
           05  WF-FATALITY-COUNT   PIC 9(4).
           05  WF-TOTAL-INCURRED   PIC 9(11)V99.
           05  WF-LAST-ASSIGN-DATE PIC X(8).
           05  WF-CAPACITY-PCT     PIC 9(3).

       FD  ASSIGNMENT-LOG.
       01  LOG-RECORD.
           05  LR-CLAIM-NUMBER     PIC X(15).
           05  LR-ACTION-CODE      PIC X(2).
               88  LR-NEW-ASSIGN         VALUE 'NA'.
               88  LR-REASSIGN           VALUE 'RA'.
               88  LR-OVERRIDE           VALUE 'OV'.
           05  LR-FROM-ADJUSTER    PIC X(8).
           05  LR-TO-ADJUSTER      PIC X(8).
           05  LR-ASSIGN-DATE      PIC X(8).
           05  LR-ASSIGN-TIME      PIC X(6).
           05  LR-ASSIGN-REASON    PIC X(60).
           05  LR-SUPERVISOR-ID    PIC X(8).

       WORKING-STORAGE SECTION.
      *
      *    Cross-program copybook references
           COPY WCCLMCPY
           COPY WCEMPCPY

       01  WS-FILE-STATUSES.
           05  WS-ADJ-STATUS       PIC X(2).
           05  WS-WKL-STATUS       PIC X(2).
           05  WS-LOG-STATUS       PIC X(2).

       01  WS-ASSIGNMENT-REQUEST.
           05  WS-REQ-CLAIM-NUMBER PIC X(15).
           05  WS-REQ-INJURY-STATE PIC X(2).
           05  WS-REQ-CLAIM-TYPE   PIC 9.
               88  WS-IS-MED-ONLY        VALUE 1.
               88  WS-IS-LOST-TIME       VALUE 2.
               88  WS-IS-PERMANENT       VALUE 3.
               88  WS-IS-FATALITY        VALUE 5.
               88  WS-IS-OCC-DISEASE     VALUE 6.
           05  WS-REQ-SEVERITY     PIC 9.
               88  WS-SEVERITY-LOW       VALUE 1.
               88  WS-SEVERITY-MED       VALUE 2.
               88  WS-SEVERITY-HIGH      VALUE 3.
               88  WS-SEVERITY-CRITICAL  VALUE 4.
           05  WS-REQ-EMPLOYER-SIZE PIC 9.
               88  WS-EMPLOYER-SMALL     VALUE 1.
               88  WS-EMPLOYER-MED       VALUE 2.
               88  WS-EMPLOYER-LARGE     VALUE 3.
           05  WS-REQ-OVERRIDE-ADJ PIC X(8).
           05  WS-REQ-OVERRIDE-BY  PIC X(8).
           05  WS-REQ-IS-REASSIGN  PIC X VALUE 'N'.
               88  WS-REASSIGNMENT       VALUE 'Y'.
           05  WS-REQ-PREV-ADJ     PIC X(8).

       01  WS-ASSIGNMENT-RESULT.
           05  WS-RES-RETURN-CODE  PIC 9(2).
               88  WS-ASSIGN-OK          VALUE 00.
               88  WS-ASSIGN-WARN        VALUE 04.
               88  WS-ASSIGN-FAIL        VALUE 08.
           05  WS-RES-ADJUSTER-ID  PIC X(8).
           05  WS-RES-ADJUSTER-NAME PIC X(40).
           05  WS-RES-MESSAGE      PIC X(80).

       01  WS-WORK-FIELDS.
           05  WS-TARGET-TEAM      PIC 9.
           05  WS-BEST-ADJUSTER-ID PIC X(8).
           05  WS-BEST-ADJ-NAME   PIC X(40).
           05  WS-BEST-SCORE       PIC 9(5) VALUE 99999.
           05  WS-CURRENT-SCORE    PIC 9(5).
           05  WS-CANDIDATES-FOUND PIC 9(3) VALUE 0.
           05  WS-CAPACITY-USED    PIC 9(3).
           05  WS-SEARCH-KEY.
               10  WS-SRCH-JURIS   PIC X(2).
               10  WS-SRCH-TEAM    PIC 9.
           05  WS-CURRENT-DATE     PIC X(8).
           05  WS-CURRENT-TIME     PIC X(6).
           05  WS-EOF-FLAG         PIC X VALUE 'N'.
               88  WS-AT-EOF            VALUE 'Y'.
               88  WS-NOT-EOF           VALUE 'N'.

      * PATHSEND fields for notification server
       01  WS-PATHSEND-FIELDS.
           05  WS-PS-SERVERCLASS   PIC X(16)
                                   VALUE "$NOTIFY         ".
           05  WS-PS-TIMEOUT       PIC S9(9) COMP VALUE 6000.
           05  WS-PS-ERROR         PIC S9(4) COMP.
           05  WS-PS-REPLY-LEN     PIC S9(4) COMP.

       01  WS-NOTIFICATION-MSG.
           05  WS-NOTIF-TYPE       PIC X(2).
               88  WS-NOTIF-NEW-ASGN    VALUE 'NA'.
               88  WS-NOTIF-REASSIGN    VALUE 'RA'.
           05  WS-NOTIF-CLAIM      PIC X(15).
           05  WS-NOTIF-ADJUSTER   PIC X(8).
           05  WS-NOTIF-ADJ-NAME   PIC X(40).
           05  WS-NOTIF-DETAIL     PIC X(120).

       01  WS-NOTIF-REPLY.
           05  WS-NOTIF-REPLY-CODE PIC 9(2).

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
      *
      *    Inter-program communication calls
           CALL "CLMDIARY"
           PERFORM 1000-INITIALIZE
           IF WS-REQ-OVERRIDE-ADJ NOT = SPACES
               PERFORM 3000-SUPERVISOR-OVERRIDE
           ELSE
               PERFORM 2000-AUTO-ASSIGN
           END-IF
           IF WS-ASSIGN-OK
               PERFORM 4000-UPDATE-WORKLOAD
               PERFORM 5000-WRITE-LOG-ENTRY
               PERFORM 6000-SEND-NOTIFICATION
               IF WS-REASSIGNMENT
                   PERFORM 7000-TRANSFER-DIARIES
               END-IF
           END-IF
           PERFORM 9000-CLEANUP
           STOP RUN.

       1000-INITIALIZE.
           OPEN I-O ADJUSTER-FILE
           OPEN I-O WORKLOAD-FILE
           OPEN EXTEND ASSIGNMENT-LOG
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-CURRENT-TIME
           MOVE 99999 TO WS-BEST-SCORE
           MOVE 0 TO WS-CANDIDATES-FOUND
           MOVE SPACES TO WS-BEST-ADJUSTER-ID
           MOVE 'N' TO WS-EOF-FLAG.

       2000-AUTO-ASSIGN.
           PERFORM 2100-DETERMINE-TEAM
           PERFORM 2200-SEARCH-CANDIDATES
           IF WS-CANDIDATES-FOUND > 0
               MOVE 00 TO WS-RES-RETURN-CODE
               MOVE WS-BEST-ADJUSTER-ID TO WS-RES-ADJUSTER-ID
               MOVE WS-BEST-ADJ-NAME TO WS-RES-ADJUSTER-NAME
               STRING "Auto-assigned to " WS-BEST-ADJ-NAME
                   DELIMITED BY "  " INTO WS-RES-MESSAGE
           ELSE
               MOVE 08 TO WS-RES-RETURN-CODE
               MOVE SPACES TO WS-RES-ADJUSTER-ID
               MOVE "No eligible adjuster found for jurisdiction/team"
                   TO WS-RES-MESSAGE
           END-IF.

       2100-DETERMINE-TEAM.
           EVALUATE TRUE
               WHEN WS-IS-MED-ONLY
                   MOVE 1 TO WS-TARGET-TEAM
               WHEN WS-IS-LOST-TIME
                   MOVE 2 TO WS-TARGET-TEAM
               WHEN WS-IS-FATALITY
                   MOVE 5 TO WS-TARGET-TEAM
               WHEN WS-IS-OCC-DISEASE
                   MOVE 4 TO WS-TARGET-TEAM
               WHEN WS-IS-PERMANENT
                   IF WS-SEVERITY-CRITICAL
                       MOVE 3 TO WS-TARGET-TEAM
                   ELSE
                       MOVE 2 TO WS-TARGET-TEAM
                   END-IF
               WHEN OTHER
                   MOVE 2 TO WS-TARGET-TEAM
           END-EVALUATE.

       2200-SEARCH-CANDIDATES.
           MOVE WS-REQ-INJURY-STATE TO WS-SRCH-JURIS
           MOVE WS-TARGET-TEAM TO WS-SRCH-TEAM
           MOVE WS-SEARCH-KEY TO AF-JURIS-TEAM
           START ADJUSTER-FILE
               KEY IS EQUAL TO AF-JURIS-TEAM
               INVALID KEY
                   PERFORM 2300-FALLBACK-SEARCH
                   EXIT PARAGRAPH
           END-START
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM 2250-READ-AND-SCORE
               UNTIL WS-AT-EOF.

       2250-READ-AND-SCORE.
           READ ADJUSTER-FILE NEXT
               AT END
                   SET WS-AT-EOF TO TRUE
                   EXIT PARAGRAPH
           END-READ
           IF AF-JURISDICTION NOT = WS-REQ-INJURY-STATE
               SET WS-AT-EOF TO TRUE
               EXIT PARAGRAPH
           END-IF
           IF AF-TEAM-CODE NOT = WS-TARGET-TEAM
               EXIT PARAGRAPH
           END-IF
           IF NOT AF-IS-ACTIVE
               EXIT PARAGRAPH
           END-IF
           IF AF-CURRENT-OPEN >= AF-MAX-CASELOAD
               EXIT PARAGRAPH
           END-IF
           IF WS-IS-FATALITY AND NOT AF-CAN-FATALITY
               EXIT PARAGRAPH
           END-IF
           IF WS-IS-OCC-DISEASE AND NOT AF-CAN-OCC-DISEASE
               EXIT PARAGRAPH
           END-IF
           PERFORM 2260-CALCULATE-SCORE
           ADD 1 TO WS-CANDIDATES-FOUND
           IF WS-CURRENT-SCORE < WS-BEST-SCORE
               MOVE WS-CURRENT-SCORE TO WS-BEST-SCORE
               MOVE AF-ADJUSTER-ID TO WS-BEST-ADJUSTER-ID
               MOVE AF-ADJUSTER-NAME TO WS-BEST-ADJ-NAME
           END-IF.

       2260-CALCULATE-SCORE.
      *    Score = weighted combination of caseload utilization,
      *    experience inverse, and average closure days.
      *    Lower score = better candidate.
           COMPUTE WS-CAPACITY-USED =
               (AF-CURRENT-OPEN * 100) / AF-MAX-CASELOAD
           COMPUTE WS-CURRENT-SCORE =
               (WS-CAPACITY-USED * 3)
               + ((99 - AF-YEARS-EXPERIENCE) * 1)
               + (AF-AVG-CLOSURE-DAYS / 10)
      *    Large employer bonus: experienced adjusters preferred
           IF WS-EMPLOYER-LARGE
               IF AF-YEARS-EXPERIENCE > 10
                   SUBTRACT 50 FROM WS-CURRENT-SCORE
               END-IF
           END-IF
      *    High severity: favor adjusters with litigation capability
           IF WS-SEVERITY-HIGH OR WS-SEVERITY-CRITICAL
               IF AF-CAN-LITIGATE
                   SUBTRACT 30 FROM WS-CURRENT-SCORE
               END-IF
           END-IF.

       2300-FALLBACK-SEARCH.
      *    No exact jurisdiction+team match; try same jurisdiction,
      *    any team with appropriate capability.
           MOVE WS-REQ-INJURY-STATE TO AF-JURISDICTION
           MOVE 1 TO AF-TEAM-CODE
           START ADJUSTER-FILE
               KEY IS NOT LESS THAN AF-JURIS-TEAM
               INVALID KEY
                   EXIT PARAGRAPH
           END-START
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM 2350-FALLBACK-READ
               UNTIL WS-AT-EOF.

       2350-FALLBACK-READ.
           READ ADJUSTER-FILE NEXT
               AT END
                   SET WS-AT-EOF TO TRUE
                   EXIT PARAGRAPH
           END-READ
           IF AF-JURISDICTION NOT = WS-REQ-INJURY-STATE
               SET WS-AT-EOF TO TRUE
               EXIT PARAGRAPH
           END-IF
           IF NOT AF-IS-ACTIVE
               EXIT PARAGRAPH
           END-IF
           IF AF-CURRENT-OPEN >= AF-MAX-CASELOAD
               EXIT PARAGRAPH
           END-IF
           PERFORM 2260-CALCULATE-SCORE
           ADD 1 TO WS-CANDIDATES-FOUND
           IF WS-CURRENT-SCORE < WS-BEST-SCORE
               MOVE WS-CURRENT-SCORE TO WS-BEST-SCORE
               MOVE AF-ADJUSTER-ID TO WS-BEST-ADJUSTER-ID
               MOVE AF-ADJUSTER-NAME TO WS-BEST-ADJ-NAME
           END-IF.

       3000-SUPERVISOR-OVERRIDE.
           MOVE WS-REQ-OVERRIDE-ADJ TO AF-ADJUSTER-ID
           READ ADJUSTER-FILE
               INVALID KEY
                   MOVE 08 TO WS-RES-RETURN-CODE
                   MOVE "Override adjuster ID not found"
                       TO WS-RES-MESSAGE
                   EXIT PARAGRAPH
           END-READ
           IF NOT AF-IS-ACTIVE
               MOVE 04 TO WS-RES-RETURN-CODE
               STRING "Warning: Adjuster " AF-ADJUSTER-NAME
                   " is inactive" DELIMITED BY "  "
                   INTO WS-RES-MESSAGE
           ELSE
               MOVE 00 TO WS-RES-RETURN-CODE
           END-IF
           MOVE AF-ADJUSTER-ID TO WS-RES-ADJUSTER-ID
           MOVE AF-ADJUSTER-NAME TO WS-RES-ADJUSTER-NAME
           STRING "Supervisor override by " WS-REQ-OVERRIDE-BY
               " to " AF-ADJUSTER-NAME
               DELIMITED BY "  " INTO WS-RES-MESSAGE.

       4000-UPDATE-WORKLOAD.
           MOVE WS-RES-ADJUSTER-ID TO WF-ADJUSTER-ID
           READ WORKLOAD-FILE
               INVALID KEY
                   EXIT PARAGRAPH
           END-READ
           EVALUATE TRUE
               WHEN WS-IS-MED-ONLY
                   ADD 1 TO WF-MED-ONLY-COUNT
               WHEN WS-IS-LOST-TIME
                   ADD 1 TO WF-LOST-TIME-COUNT
               WHEN WS-IS-FATALITY
                   ADD 1 TO WF-FATALITY-COUNT
               WHEN OTHER
                   ADD 1 TO WF-LOST-TIME-COUNT
           END-EVALUATE
           MOVE WS-CURRENT-DATE TO WF-LAST-ASSIGN-DATE
           REWRITE WORKLOAD-RECORD
      *    Also update adjuster master open count
           MOVE WS-RES-ADJUSTER-ID TO AF-ADJUSTER-ID
           READ ADJUSTER-FILE
               INVALID KEY
                   EXIT PARAGRAPH
           END-READ
           ADD 1 TO AF-CURRENT-OPEN
           REWRITE ADJUSTER-RECORD.

       5000-WRITE-LOG-ENTRY.
           MOVE WS-REQ-CLAIM-NUMBER TO LR-CLAIM-NUMBER
           IF WS-REQ-OVERRIDE-ADJ NOT = SPACES
               SET LR-OVERRIDE TO TRUE
               MOVE WS-REQ-OVERRIDE-BY TO LR-SUPERVISOR-ID
           ELSE IF WS-REASSIGNMENT
               SET LR-REASSIGN TO TRUE
               MOVE SPACES TO LR-SUPERVISOR-ID
           ELSE
               SET LR-NEW-ASSIGN TO TRUE
               MOVE SPACES TO LR-SUPERVISOR-ID
           END-IF
           MOVE WS-REQ-PREV-ADJ TO LR-FROM-ADJUSTER
           MOVE WS-RES-ADJUSTER-ID TO LR-TO-ADJUSTER
           MOVE WS-CURRENT-DATE TO LR-ASSIGN-DATE
           MOVE WS-CURRENT-TIME TO LR-ASSIGN-TIME
           MOVE WS-RES-MESSAGE TO LR-ASSIGN-REASON
           WRITE LOG-RECORD.

       6000-SEND-NOTIFICATION.
           IF WS-REASSIGNMENT
               SET WS-NOTIF-REASSIGN TO TRUE
           ELSE
               SET WS-NOTIF-NEW-ASGN TO TRUE
           END-IF
           MOVE WS-REQ-CLAIM-NUMBER TO WS-NOTIF-CLAIM
           MOVE WS-RES-ADJUSTER-ID TO WS-NOTIF-ADJUSTER
           MOVE WS-RES-ADJUSTER-NAME TO WS-NOTIF-ADJ-NAME
           STRING "Claim " WS-REQ-CLAIM-NUMBER
               " assigned - " WS-REQ-INJURY-STATE
               " jurisdiction, type " WS-REQ-CLAIM-TYPE
               DELIMITED BY "  " INTO WS-NOTIF-DETAIL

           CALL "SERVERCLASS_DIALOG_BEGIN_" USING
               WS-PS-SERVERCLASS
               WS-PS-ERROR

           CALL "SERVERCLASS_DIALOG_SEND_" USING
               WS-NOTIFICATION-MSG
               LENGTH OF WS-NOTIFICATION-MSG
               WS-NOTIF-REPLY
               LENGTH OF WS-NOTIF-REPLY
               WS-PS-REPLY-LEN
               WS-PS-TIMEOUT
               WS-PS-ERROR

           CALL "SERVERCLASS_DIALOG_END_" USING
               WS-PS-ERROR.

       7000-TRANSFER-DIARIES.
      *    When reassigning, transfer all open diaries from the
      *    previous adjuster to the new adjuster for this claim.
      *    This is handled via PATHSEND to the diary server.
           CONTINUE.

       9000-CLEANUP.
           CLOSE ADJUSTER-FILE
           CLOSE WORKLOAD-FILE
           CLOSE ASSIGNMENT-LOG.
