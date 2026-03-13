       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ELGCHECK.
       AUTHOR.        TPA-SYSTEMS.
      *============================================================
      * ELGCHECK -- Eligibility Verification Program
      * HPE NonStop COBOL
      * Real-time eligibility lookup against policy master file.
      * Called by PATHSEND from terminal servers and batch jobs.
      *============================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  TANDEM.
       OBJECT-COMPUTER.  TANDEM.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POLICY-FILE ASSIGN TO "$DATA1.POLDATA.POLMAST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PF-POLICY-NUMBER
               FILE STATUS IS WS-FILE-STATUS.

           SELECT DEPENDENT-FILE ASSIGN TO "$DATA1.POLDATA.DEPEND"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DF-KEY
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  POLICY-FILE.
       01  POLICY-RECORD.
           05  PF-POLICY-NUMBER        PIC X(16).
           05  PF-INSURED-NAME         PIC X(40).
           05  PF-INSURED-SSN          PIC X(9).
           05  PF-INSURED-DOB          PIC 9(8).
           05  PF-EMPLOYER-ID          PIC X(10).
           05  PF-GROUP-NUMBER         PIC X(12).
           05  PF-PLAN-TYPE            PIC 9(2).
           05  PF-STATUS               PIC 9(1).
               88  POLICY-ACTIVE         VALUE 1.
               88  POLICY-LAPSED         VALUE 2.
               88  POLICY-CANCELLED      VALUE 3.
               88  POLICY-EXPIRED        VALUE 4.
           05  PF-EFFECTIVE-DATE       PIC 9(8).
           05  PF-TERMINATION-DATE     PIC 9(8).
           05  PF-PREMIUM-AMOUNT       PIC S9(7)V99 COMP-3.
           05  PF-DEDUCTIBLE           PIC S9(7)V99 COMP-3.
           05  PF-DEDUCTIBLE-MET       PIC S9(7)V99 COMP-3.
           05  PF-OOP-MAX              PIC S9(7)V99 COMP-3.
           05  PF-OOP-MET              PIC S9(7)V99 COMP-3.

       FD  DEPENDENT-FILE.
       01  DEPENDENT-RECORD.
           05  DF-KEY.
               10  DF-POLICY-NUMBER    PIC X(16).
               10  DF-SEQUENCE         PIC 9(2).
           05  DF-DEPENDENT-NAME       PIC X(40).
           05  DF-RELATIONSHIP         PIC X(10).
           05  DF-DEPENDENT-DOB        PIC 9(8).
           05  DF-DEPENDENT-SSN        PIC X(9).
           05  DF-STATUS               PIC 9(1).

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS              PIC XX.
       01  WS-CURRENT-DATE             PIC 9(8).

       01  WS-ELIGIBILITY-REQUEST.
           05  WS-REQ-POLICY-NUMBER    PIC X(16).
           05  WS-REQ-MEMBER-SSN       PIC X(9).
           05  WS-REQ-SERVICE-DATE     PIC 9(8).
           05  WS-REQ-SERVICE-TYPE     PIC 9(2).

       01  WS-ELIGIBILITY-RESPONSE.
           05  WS-RSP-STATUS           PIC 9(2).
               88  MEMBER-ELIGIBLE       VALUE 00.
               88  POLICY-NOT-FOUND      VALUE 01.
               88  MEMBER-NOT-FOUND      VALUE 02.
               88  POLICY-INACTIVE       VALUE 03.
               88  COVERAGE-NOT-STARTED  VALUE 04.
               88  COVERAGE-TERMINATED   VALUE 05.
               88  SERVICE-NOT-COVERED   VALUE 06.
           05  WS-RSP-POLICY-NUMBER    PIC X(16).
           05  WS-RSP-MEMBER-NAME      PIC X(40).
           05  WS-RSP-PLAN-TYPE        PIC 9(2).
           05  WS-RSP-EFFECTIVE-DATE   PIC 9(8).
           05  WS-RSP-TERM-DATE        PIC 9(8).
           05  WS-RSP-DEDUCTIBLE       PIC S9(7)V99 COMP-3.
           05  WS-RSP-DEDUCTIBLE-MET   PIC S9(7)V99 COMP-3.
           05  WS-RSP-OOP-MAX          PIC S9(7)V99 COMP-3.
           05  WS-RSP-OOP-MET          PIC S9(7)V99 COMP-3.
           05  WS-RSP-COPAY-AMOUNT     PIC S9(5)V99 COMP-3.

       01  WS-BENEFIT-TABLE.
           05  WS-COPAY-OFFICE         PIC S9(5)V99 VALUE 25.00.
           05  WS-COPAY-SPECIALIST     PIC S9(5)V99 VALUE 50.00.
           05  WS-COPAY-ER             PIC S9(5)V99 VALUE 150.00.
           05  WS-COPAY-URGENT         PIC S9(5)V99 VALUE 75.00.

       PROCEDURE DIVISION.

       0000-MAIN-CONTROL.
           OPEN INPUT POLICY-FILE
           OPEN INPUT DEPENDENT-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           PERFORM 1000-CHECK-ELIGIBILITY
           CLOSE POLICY-FILE
           CLOSE DEPENDENT-FILE
           STOP RUN.

       1000-CHECK-ELIGIBILITY.
      *    Look up the policy
           MOVE WS-REQ-POLICY-NUMBER TO PF-POLICY-NUMBER
           READ POLICY-FILE
               INVALID KEY
                   SET POLICY-NOT-FOUND TO TRUE
                   GO TO 1000-EXIT
           END-READ

      *    Verify policy is active
           IF NOT POLICY-ACTIVE
               SET POLICY-INACTIVE TO TRUE
               GO TO 1000-EXIT
           END-IF

      *    Check coverage dates
           IF WS-REQ-SERVICE-DATE < PF-EFFECTIVE-DATE
               SET COVERAGE-NOT-STARTED TO TRUE
               GO TO 1000-EXIT
           END-IF

           IF PF-TERMINATION-DATE NOT = 0
               AND WS-REQ-SERVICE-DATE > PF-TERMINATION-DATE
               SET COVERAGE-TERMINATED TO TRUE
               GO TO 1000-EXIT
           END-IF

      *    Check if requesting member is insured or dependent
           PERFORM 2000-FIND-MEMBER

      *    If member found, populate response
           IF MEMBER-ELIGIBLE
               PERFORM 3000-POPULATE-BENEFITS
           END-IF

       1000-EXIT.
           EXIT.

       2000-FIND-MEMBER.
      *    First check if SSN matches the insured
           IF WS-REQ-MEMBER-SSN = PF-INSURED-SSN
               SET MEMBER-ELIGIBLE TO TRUE
               MOVE PF-INSURED-NAME TO WS-RSP-MEMBER-NAME
           ELSE
      *        Search dependents
               PERFORM 2100-SEARCH-DEPENDENTS
           END-IF.

       2100-SEARCH-DEPENDENTS.
           MOVE WS-REQ-POLICY-NUMBER TO DF-POLICY-NUMBER
           MOVE 01 TO DF-SEQUENCE

           START DEPENDENT-FILE KEY >= DF-KEY
               INVALID KEY
                   SET MEMBER-NOT-FOUND TO TRUE
                   GO TO 2100-EXIT
           END-START

           PERFORM UNTIL WS-FILE-STATUS NOT = "00"
               READ DEPENDENT-FILE NEXT
                   AT END
                       SET MEMBER-NOT-FOUND TO TRUE
                       GO TO 2100-EXIT
               END-READ

               IF DF-POLICY-NUMBER NOT = WS-REQ-POLICY-NUMBER
                   SET MEMBER-NOT-FOUND TO TRUE
                   GO TO 2100-EXIT
               END-IF

               IF DF-DEPENDENT-SSN = WS-REQ-MEMBER-SSN
                   SET MEMBER-ELIGIBLE TO TRUE
                   MOVE DF-DEPENDENT-NAME TO WS-RSP-MEMBER-NAME
                   GO TO 2100-EXIT
               END-IF
           END-PERFORM.

       2100-EXIT.
           EXIT.

       3000-POPULATE-BENEFITS.
           MOVE PF-POLICY-NUMBER    TO WS-RSP-POLICY-NUMBER
           MOVE PF-PLAN-TYPE        TO WS-RSP-PLAN-TYPE
           MOVE PF-EFFECTIVE-DATE   TO WS-RSP-EFFECTIVE-DATE
           MOVE PF-TERMINATION-DATE TO WS-RSP-TERM-DATE
           MOVE PF-DEDUCTIBLE       TO WS-RSP-DEDUCTIBLE
           MOVE PF-DEDUCTIBLE-MET   TO WS-RSP-DEDUCTIBLE-MET
           MOVE PF-OOP-MAX          TO WS-RSP-OOP-MAX
           MOVE PF-OOP-MET          TO WS-RSP-OOP-MET

      *    Determine copay based on service type
           EVALUATE WS-REQ-SERVICE-TYPE
               WHEN 01
                   MOVE WS-COPAY-OFFICE TO WS-RSP-COPAY-AMOUNT
               WHEN 02
                   MOVE WS-COPAY-SPECIALIST TO WS-RSP-COPAY-AMOUNT
               WHEN 03
                   MOVE WS-COPAY-ER TO WS-RSP-COPAY-AMOUNT
               WHEN 04
                   MOVE WS-COPAY-URGENT TO WS-RSP-COPAY-AMOUNT
               WHEN OTHER
                   MOVE 0 TO WS-RSP-COPAY-AMOUNT
           END-EVALUATE.
