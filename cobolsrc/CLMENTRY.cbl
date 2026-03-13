       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CLMENTRY.
       AUTHOR.        TPA-SYSTEMS.
      *============================================================
      * CLMENTRY -- Claims Entry and Validation Program
      * HPE NonStop COBOL
      * Receives claim data from terminals via Pathway,
      * validates, and forwards to CLMPROC via PATHSEND.
      *============================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  TANDEM.
       OBJECT-COMPUTER.  TANDEM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-CLAIM-RECORD.
           05  WS-CLAIM-ID             PIC X(12).
           05  WS-POLICY-NUMBER        PIC X(16).
           05  WS-CLAIMANT-NAME        PIC X(40).
           05  WS-CLAIMANT-SSN         PIC X(9).
           05  WS-CLAIM-TYPE           PIC 9(2).
               88  CLAIM-TYPE-MEDICAL    VALUE 01.
               88  CLAIM-TYPE-DENTAL     VALUE 02.
               88  CLAIM-TYPE-VISION     VALUE 03.
               88  CLAIM-TYPE-PHARMACY   VALUE 04.
           05  WS-CLAIM-AMOUNT         PIC S9(9)V99 COMP.
           05  WS-DATE-OF-SERVICE      PIC 9(8).
           05  WS-PROVIDER-ID          PIC X(10).
           05  WS-PROVIDER-NAME        PIC X(40).
           05  WS-DIAGNOSIS-CODE       PIC X(7).
           05  WS-DIAGNOSIS-DESC       PIC X(60).

       01  WS-LINE-ITEMS.
           05  WS-LINE-ITEM-COUNT      PIC 9(3) VALUE 0.
           05  WS-LINE-ITEM OCCURS 50 TIMES.
               10  WS-LI-SERVICE-CODE  PIC X(10).
               10  WS-LI-DESCRIPTION   PIC X(60).
               10  WS-LI-BILLED-AMT    PIC S9(9)V99 COMP.
               10  WS-LI-UNITS         PIC 9(3).
               10  WS-LI-SERVICE-DATE  PIC 9(8).

       01  WS-VALIDATION-FLAGS.
           05  WS-VALID-CLAIM          PIC X VALUE 'N'.
               88  CLAIM-IS-VALID        VALUE 'Y'.
               88  CLAIM-IS-INVALID      VALUE 'N'.
           05  WS-ERROR-COUNT          PIC 9(3) VALUE 0.
           05  WS-ERROR-MESSAGES.
               10  WS-ERROR-MSG OCCURS 20 TIMES PIC X(80).

       01  WS-PATHSEND-FIELDS.
           05  WS-SERVER-CLASS         PIC X(32)
               VALUE "$CLMP".
           05  WS-PATHSEND-STATUS      PIC S9(4) COMP.
           05  WS-REPLY-BUFFER         PIC X(4096).
           05  WS-REPLY-LENGTH         PIC S9(9) COMP.

       01  WS-COUNTERS.
           05  WS-CLAIMS-SUBMITTED     PIC 9(7) VALUE 0.
           05  WS-CLAIMS-REJECTED      PIC 9(7) VALUE 0.
           05  WS-TOTAL-AMOUNT         PIC S9(11)V99 COMP VALUE 0.

       01  WS-WORK-FIELDS.
           05  WS-CURRENT-DATE         PIC 9(8).
           05  WS-CURRENT-TIME         PIC 9(6).
           05  WS-GENERATED-ID         PIC X(12).
           05  WS-SEQUENCE-NUM         PIC 9(7) VALUE 0.

       PROCEDURE DIVISION.

       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-CLAIMS
               UNTIL WS-PATHSEND-STATUS = -1
           PERFORM 9000-FINALIZE
           STOP RUN.

       1000-INITIALIZE.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           DISPLAY "CLMENTRY: Claims Entry started at "
               WS-CURRENT-DATE " " WS-CURRENT-TIME
           INITIALIZE WS-CLAIM-RECORD
           INITIALIZE WS-LINE-ITEMS.

       2000-PROCESS-CLAIMS.
           PERFORM 2100-RECEIVE-CLAIM
           IF CLAIM-IS-VALID
               PERFORM 2200-GENERATE-CLAIM-ID
               PERFORM 2300-SEND-TO-PROCESSOR
               ADD 1 TO WS-CLAIMS-SUBMITTED
               ADD WS-CLAIM-AMOUNT TO WS-TOTAL-AMOUNT
           ELSE
               ADD 1 TO WS-CLAIMS-REJECTED
               PERFORM 2400-SEND-ERROR-RESPONSE
           END-IF.

       2100-RECEIVE-CLAIM.
           MOVE 'N' TO WS-VALID-CLAIM
           MOVE 0 TO WS-ERROR-COUNT
           PERFORM 3000-VALIDATE-CLAIM
           IF WS-ERROR-COUNT = 0
               MOVE 'Y' TO WS-VALID-CLAIM
           END-IF.

       2200-GENERATE-CLAIM-ID.
           ADD 1 TO WS-SEQUENCE-NUM
           STRING "CLM"
                  WS-CURRENT-DATE(3:6)
                  WS-SEQUENCE-NUM
               DELIMITED BY SIZE
               INTO WS-GENERATED-ID
           MOVE WS-GENERATED-ID TO WS-CLAIM-ID.

       2300-SEND-TO-PROCESSOR.
      *    Forward claim to CLMPROC server via PATHSEND
           ENTER TAL "SERVERCLASS_SEND_" USING
               WS-SERVER-CLASS
               WS-CLAIM-RECORD
               WS-REPLY-BUFFER
               WS-PATHSEND-STATUS.

       2400-SEND-ERROR-RESPONSE.
           DISPLAY "CLMENTRY: Claim rejected with "
               WS-ERROR-COUNT " errors".

       3000-VALIDATE-CLAIM.
           PERFORM 3100-VALIDATE-POLICY
           PERFORM 3200-VALIDATE-CLAIMANT
           PERFORM 3300-VALIDATE-PROVIDER
           PERFORM 3400-VALIDATE-AMOUNTS
           PERFORM 3500-VALIDATE-DATES
           PERFORM 3600-VALIDATE-LINE-ITEMS.

       3100-VALIDATE-POLICY.
           IF WS-POLICY-NUMBER = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE "Policy number is required"
                   TO WS-ERROR-MSG(WS-ERROR-COUNT)
           END-IF.

       3200-VALIDATE-CLAIMANT.
           IF WS-CLAIMANT-NAME = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE "Claimant name is required"
                   TO WS-ERROR-MSG(WS-ERROR-COUNT)
           END-IF
           IF WS-CLAIMANT-SSN = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE "Claimant SSN is required"
                   TO WS-ERROR-MSG(WS-ERROR-COUNT)
           END-IF.

       3300-VALIDATE-PROVIDER.
           IF WS-PROVIDER-ID = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE "Provider ID is required"
                   TO WS-ERROR-MSG(WS-ERROR-COUNT)
           END-IF.

       3400-VALIDATE-AMOUNTS.
           IF WS-CLAIM-AMOUNT NOT > 0
               ADD 1 TO WS-ERROR-COUNT
               MOVE "Claim amount must be greater than zero"
                   TO WS-ERROR-MSG(WS-ERROR-COUNT)
           END-IF
           IF WS-CLAIM-AMOUNT > 999999.99
               ADD 1 TO WS-ERROR-COUNT
               MOVE "Claim amount exceeds maximum"
                   TO WS-ERROR-MSG(WS-ERROR-COUNT)
           END-IF.

       3500-VALIDATE-DATES.
           IF WS-DATE-OF-SERVICE = 0
               ADD 1 TO WS-ERROR-COUNT
               MOVE "Date of service is required"
                   TO WS-ERROR-MSG(WS-ERROR-COUNT)
           END-IF
           IF WS-DATE-OF-SERVICE > WS-CURRENT-DATE
               ADD 1 TO WS-ERROR-COUNT
               MOVE "Date of service cannot be in the future"
                   TO WS-ERROR-MSG(WS-ERROR-COUNT)
           END-IF.

       3600-VALIDATE-LINE-ITEMS.
           IF WS-LINE-ITEM-COUNT = 0
               ADD 1 TO WS-ERROR-COUNT
               MOVE "At least one line item is required"
                   TO WS-ERROR-MSG(WS-ERROR-COUNT)
           END-IF.

       9000-FINALIZE.
           DISPLAY "CLMENTRY: Session complete"
           DISPLAY "  Claims submitted: " WS-CLAIMS-SUBMITTED
           DISPLAY "  Claims rejected:  " WS-CLAIMS-REJECTED
           DISPLAY "  Total amount:     " WS-TOTAL-AMOUNT.
