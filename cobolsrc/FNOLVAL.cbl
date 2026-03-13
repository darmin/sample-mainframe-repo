       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNOLVAL.
      ******************************************************************
      *  FNOLVAL - First Notice of Loss Validation Program
      *  Workers' Compensation TPA System
      *  HPE NonStop COBOL (Tandem COBOL85)
      *
      *  Validates all FNOL input fields before claim creation.
      *  Called via PATHSEND from FNOLPROC server or batch feeds.
      *  Returns detailed error messages for each validation failure.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. TANDEM.
       OBJECT-COMPUTER. TANDEM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      *    Cross-program copybook references
           COPY WCCLMCPY
           COPY WCEMPCPY

       01  WS-CURRENT-DATE.
           05  WS-CURR-YEAR        PIC 9(4).
           05  WS-CURR-MONTH       PIC 9(2).
           05  WS-CURR-DAY         PIC 9(2).

       01  WS-ERROR-TABLE.
           05  WS-ERROR-COUNT      PIC 9(3)  VALUE 0.
           05  WS-ERROR-ENTRY OCCURS 50 TIMES.
               10  WS-ERR-FIELD    PIC X(20).
               10  WS-ERR-CODE     PIC X(4).
               10  WS-ERR-MSG      PIC X(60).

       01  WS-VALIDATION-FLAGS.
           05  WS-VALID-FLAG       PIC X     VALUE 'Y'.
               88  VALIDATION-PASSED         VALUE 'Y'.
               88  VALIDATION-FAILED         VALUE 'N'.
           05  WS-FEIN-VALID       PIC X     VALUE 'Y'.
               88  FEIN-OK                   VALUE 'Y'.
               88  FEIN-BAD                  VALUE 'N'.
           05  WS-POLICY-VALID     PIC X     VALUE 'Y'.
               88  POLICY-OK                 VALUE 'Y'.
               88  POLICY-BAD                VALUE 'N'.
           05  WS-SSN-VALID        PIC X     VALUE 'Y'.
               88  SSN-OK                    VALUE 'Y'.
               88  SSN-BAD                   VALUE 'N'.
           05  WS-DOI-VALID        PIC X     VALUE 'Y'.
               88  DOI-OK                    VALUE 'Y'.
               88  DOI-BAD                   VALUE 'N'.

       01  WS-JURISDICTION-TABLE.
           05  WS-JURIS-ENTRY OCCURS 55 TIMES.
               10  WS-JURIS-STATE  PIC X(2).
               10  WS-JURIS-FILING-DAYS PIC 9(3).
               10  WS-JURIS-MONOPOL PIC X.
                   88  MONOPOLISTIC-STATE    VALUE 'Y'.
                   88  COMPETITIVE-STATE     VALUE 'N'.

       01  WS-WORK-FIELDS.
           05  WS-WORK-DATE        PIC 9(8).
           05  WS-WORK-YEAR        PIC 9(4).
           05  WS-WORK-MONTH       PIC 9(2).
           05  WS-WORK-DAY         PIC 9(2).
           05  WS-DAYS-ELAPSED     PIC S9(5) COMP.
           05  WS-AGE-YEARS        PIC 9(3).
           05  WS-FEIN-CHECK       PIC 9(9).
           05  WS-NUMERIC-TEST     PIC X(9).
           05  WS-FILING-LIMIT     PIC 9(3).

      * NCCI body part code ranges
       01  WS-BODY-PART-LIMITS.
           05  WS-BP-MIN           PIC 9(2)  VALUE 10.
           05  WS-BP-MAX           PIC 9(2)  VALUE 99.

      * NCCI nature of injury code ranges
       01  WS-NATURE-LIMITS.
           05  WS-NAT-MIN          PIC 9(2)  VALUE 01.
           05  WS-NAT-MAX          PIC 9(2)  VALUE 99.

      * NCCI cause of injury code ranges
       01  WS-CAUSE-LIMITS.
           05  WS-CAUSE-MIN        PIC 9(2)  VALUE 01.
           05  WS-CAUSE-MAX        PIC 9(2)  VALUE 99.

      * Valid state codes
       01  WS-VALID-STATES.
           05  FILLER PIC X(110) VALUE
               "ALAKAZABORACABORCOBORCTBORDEBORFLBORG"
               "ABORHIBORIDBORILBORINBOIABORKSBORKYB"
               "ORLABORMEBORMDBORMABORNE".
           05  FILLER PIC X(110) VALUE
               "BORMIBORMNBORMSBORMOBORNE2BORMTBORNE"
               "BORNVBORNHBORNJBORNMBORNYBORNC".
           05  FILLER PIC X(80) VALUE
               "BORNDBOROHBOROKBORORBORPABORRI"
               "BORSCBORSDBORTNBORTXBORUTBORVTBORVA".
           05  FILLER PIC X(30) VALUE
               "BORWABOBORWVBORWIBORDCBORWY".
       01  WS-STATE-TABLE REDEFINES WS-VALID-STATES.
           05  WS-STATE-ENTRY OCCURS 55 TIMES.
               10  WS-ST-CODE      PIC X(2).
               10  WS-ST-FILLER    PIC X(4).

      ******************************************************************
      *  Input/Output structures passed via PATHSEND
      ******************************************************************
       01  FNOL-INPUT-RECORD.
           05  FI-EMPLOYER-FEIN    PIC X(9).
           05  FI-POLICY-NUMBER    PIC X(15).
           05  FI-EMPLOYER-NAME    PIC X(60).
           05  FI-EMPLOYEE-SSN     PIC X(9).
           05  FI-EMPLOYEE-LAST    PIC X(30).
           05  FI-EMPLOYEE-FIRST   PIC X(20).
           05  FI-EMPLOYEE-DOB     PIC X(8).
           05  FI-DATE-OF-INJURY   PIC X(8).
           05  FI-DATE-REPORTED    PIC X(8).
           05  FI-INJURY-STATE     PIC X(2).
           05  FI-BODY-PART-CODE   PIC 9(2).
           05  FI-NATURE-CODE      PIC 9(2).
           05  FI-CAUSE-CODE       PIC 9(2).
           05  FI-INJURY-DESC      PIC X(200).
           05  FI-LOST-TIME-DAYS   PIC 9(4).
           05  FI-HOSPITALIZED     PIC X.
               88  CLAIMANT-HOSPITALIZED  VALUE 'Y'.
           05  FI-FATALITY-FLAG    PIC X.
               88  FATALITY-CLAIM         VALUE 'Y'.
           05  FI-TREATING-PHYS    PIC X(50).
           05  FI-WITNESS-NAME     PIC X(50).
           05  FI-EMPLOYER-CONTACT PIC X(50).
           05  FI-EMPLOYER-PHONE   PIC X(12).
           05  FI-OCCUPATION-CODE  PIC 9(4).
           05  FI-HIRE-DATE        PIC X(8).
           05  FI-AVG-WEEKLY-WAGE  PIC 9(7)V99.

       01  FNOL-OUTPUT-RECORD.
           05  FO-RETURN-CODE      PIC 9(2).
               88  FO-VALID              VALUE 00.
               88  FO-WARNINGS           VALUE 04.
               88  FO-ERRORS             VALUE 08.
               88  FO-SEVERE             VALUE 12.
           05  FO-ERROR-COUNT      PIC 9(3).
           05  FO-ERROR-DETAILS.
               10  FO-ERROR-LINE OCCURS 50 TIMES.
                   15  FO-ERR-FIELD PIC X(20).
                   15  FO-ERR-CODE  PIC X(4).
                   15  FO-ERR-MSG   PIC X(60).
           05  FO-JURISDICTION     PIC X(2).
           05  FO-FILING-DEADLINE  PIC X(8).

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
      *
      *    Inter-program communication calls
           CALL "CLMSETUP"
           PATHSEND USING "FNOLPROC"
           PERFORM 1000-INITIALIZE
           PERFORM 2000-VALIDATE-EMPLOYER
           PERFORM 3000-VALIDATE-EMPLOYEE
           PERFORM 4000-VALIDATE-INJURY
           PERFORM 5000-VALIDATE-JURISDICTION
           PERFORM 6000-CHECK-FILING-DEADLINE
           PERFORM 9000-BUILD-RESPONSE
           STOP RUN.

       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE 0 TO WS-ERROR-COUNT
           MOVE 'Y' TO WS-VALID-FLAG
           MOVE 'Y' TO WS-FEIN-VALID
           MOVE 'Y' TO WS-POLICY-VALID
           MOVE 'Y' TO WS-SSN-VALID
           MOVE 'Y' TO WS-DOI-VALID.

       2000-VALIDATE-EMPLOYER.
           PERFORM 2100-VALIDATE-FEIN
           PERFORM 2200-VALIDATE-POLICY
           PERFORM 2300-VALIDATE-EMP-NAME
           PERFORM 2400-VALIDATE-EMP-CONTACT.

       2100-VALIDATE-FEIN.
           IF FI-EMPLOYER-FEIN = SPACES
               PERFORM 8000-ADD-ERROR-FEIN-MISSING
           ELSE
               IF FI-EMPLOYER-FEIN IS NOT NUMERIC
                   PERFORM 8010-ADD-ERROR-FEIN-FORMAT
               ELSE
                   MOVE FI-EMPLOYER-FEIN TO WS-FEIN-CHECK
                   IF WS-FEIN-CHECK = 0
                       PERFORM 8010-ADD-ERROR-FEIN-FORMAT
                   END-IF
               END-IF
           END-IF.

       2200-VALIDATE-POLICY.
           IF FI-POLICY-NUMBER = SPACES
               PERFORM 8020-ADD-ERROR-POLICY-MISSING
           ELSE
               IF FI-POLICY-NUMBER(1:3) IS NOT ALPHABETIC
                   PERFORM 8030-ADD-ERROR-POLICY-FORMAT
               END-IF
           END-IF.

       2300-VALIDATE-EMP-NAME.
           IF FI-EMPLOYER-NAME = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'EMPLOYER-NAME'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'E201'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Employer name is required'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
               SET VALIDATION-FAILED TO TRUE
           END-IF.

       2400-VALIDATE-EMP-CONTACT.
           IF FI-EMPLOYER-CONTACT = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'EMPLOYER-CONTACT'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'W202'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Employer contact recommended for claim processing'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
           END-IF.

       3000-VALIDATE-EMPLOYEE.
           PERFORM 3100-VALIDATE-SSN
           PERFORM 3200-VALIDATE-DOB
           PERFORM 3300-VALIDATE-NAME
           PERFORM 3400-VALIDATE-HIRE-DATE
           PERFORM 3500-VALIDATE-OCCUPATION
           PERFORM 3600-VALIDATE-WAGE.

       3100-VALIDATE-SSN.
           IF FI-EMPLOYEE-SSN = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'EMPLOYEE-SSN'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'E301'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Employee SSN is required'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
               SET VALIDATION-FAILED TO TRUE
           ELSE
               IF FI-EMPLOYEE-SSN IS NOT NUMERIC
                   ADD 1 TO WS-ERROR-COUNT
                   MOVE 'EMPLOYEE-SSN'
                       TO WS-ERR-FIELD(WS-ERROR-COUNT)
                   MOVE 'E302'
                       TO WS-ERR-CODE(WS-ERROR-COUNT)
                   MOVE 'Employee SSN must be 9 numeric digits'
                       TO WS-ERR-MSG(WS-ERROR-COUNT)
                   SET VALIDATION-FAILED TO TRUE
               ELSE
                   IF FI-EMPLOYEE-SSN(1:3) = '000'
                   OR FI-EMPLOYEE-SSN(1:3) = '666'
                   OR FI-EMPLOYEE-SSN(1:1) = '9'
                       ADD 1 TO WS-ERROR-COUNT
                       MOVE 'EMPLOYEE-SSN'
                           TO WS-ERR-FIELD(WS-ERROR-COUNT)
                       MOVE 'E303'
                           TO WS-ERR-CODE(WS-ERROR-COUNT)
                       MOVE 'Invalid SSN area number'
                           TO WS-ERR-MSG(WS-ERROR-COUNT)
                       SET VALIDATION-FAILED TO TRUE
                   END-IF
               END-IF
           END-IF.

       3200-VALIDATE-DOB.
           IF FI-EMPLOYEE-DOB = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'EMPLOYEE-DOB'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'E304'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Employee date of birth is required'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
               SET VALIDATION-FAILED TO TRUE
           ELSE
               MOVE FI-EMPLOYEE-DOB TO WS-WORK-DATE
               PERFORM 7000-VALIDATE-DATE-FORMAT
               IF WS-DOI-VALID = 'N'
                   ADD 1 TO WS-ERROR-COUNT
                   MOVE 'EMPLOYEE-DOB'
                       TO WS-ERR-FIELD(WS-ERROR-COUNT)
                   MOVE 'E305'
                       TO WS-ERR-CODE(WS-ERROR-COUNT)
                   MOVE 'Invalid date of birth format (YYYYMMDD)'
                       TO WS-ERR-MSG(WS-ERROR-COUNT)
                   SET VALIDATION-FAILED TO TRUE
               ELSE
                   COMPUTE WS-AGE-YEARS =
                       WS-CURR-YEAR - WS-WORK-YEAR
                   IF WS-AGE-YEARS < 14 OR WS-AGE-YEARS > 100
                       ADD 1 TO WS-ERROR-COUNT
                       MOVE 'EMPLOYEE-DOB'
                           TO WS-ERR-FIELD(WS-ERROR-COUNT)
                       MOVE 'E306'
                           TO WS-ERR-CODE(WS-ERROR-COUNT)
                       MOVE 'Employee age out of valid range (14-100)'
                           TO WS-ERR-MSG(WS-ERROR-COUNT)
                       SET VALIDATION-FAILED TO TRUE
                   END-IF
               END-IF
           END-IF.

       3300-VALIDATE-NAME.
           IF FI-EMPLOYEE-LAST = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'EMPLOYEE-LAST'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'E307'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Employee last name is required'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
               SET VALIDATION-FAILED TO TRUE
           END-IF
           IF FI-EMPLOYEE-FIRST = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'EMPLOYEE-FIRST'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'E308'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Employee first name is required'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
               SET VALIDATION-FAILED TO TRUE
           END-IF.

       3400-VALIDATE-HIRE-DATE.
           IF FI-HIRE-DATE NOT = SPACES
               MOVE FI-HIRE-DATE TO WS-WORK-DATE
               PERFORM 7000-VALIDATE-DATE-FORMAT
               IF WS-DOI-VALID = 'N'
                   ADD 1 TO WS-ERROR-COUNT
                   MOVE 'HIRE-DATE'
                       TO WS-ERR-FIELD(WS-ERROR-COUNT)
                   MOVE 'W309'
                       TO WS-ERR-CODE(WS-ERROR-COUNT)
                   MOVE 'Invalid hire date format (YYYYMMDD)'
                       TO WS-ERR-MSG(WS-ERROR-COUNT)
               END-IF
           END-IF.

       3500-VALIDATE-OCCUPATION.
           IF FI-OCCUPATION-CODE = 0
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'OCCUPATION-CODE'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'W310'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Occupation code recommended for rating'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
           END-IF.

       3600-VALIDATE-WAGE.
           IF FI-AVG-WEEKLY-WAGE = 0
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'AVG-WEEKLY-WAGE'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'W311'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Average weekly wage needed for benefit calc'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
           ELSE
               IF FI-AVG-WEEKLY-WAGE > 9999999
                   ADD 1 TO WS-ERROR-COUNT
                   MOVE 'AVG-WEEKLY-WAGE'
                       TO WS-ERR-FIELD(WS-ERROR-COUNT)
                   MOVE 'E312'
                       TO WS-ERR-CODE(WS-ERROR-COUNT)
                   MOVE 'Average weekly wage exceeds maximum'
                       TO WS-ERR-MSG(WS-ERROR-COUNT)
                   SET VALIDATION-FAILED TO TRUE
               END-IF
           END-IF.

       4000-VALIDATE-INJURY.
           PERFORM 4100-VALIDATE-DOI
           PERFORM 4200-VALIDATE-BODY-PART
           PERFORM 4300-VALIDATE-NATURE
           PERFORM 4400-VALIDATE-CAUSE
           PERFORM 4500-VALIDATE-DESCRIPTION.

       4100-VALIDATE-DOI.
           IF FI-DATE-OF-INJURY = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'DATE-OF-INJURY'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'E401'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Date of injury is required'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
               SET VALIDATION-FAILED TO TRUE
           ELSE
               MOVE FI-DATE-OF-INJURY TO WS-WORK-DATE
               PERFORM 7000-VALIDATE-DATE-FORMAT
               IF WS-DOI-VALID = 'N'
                   ADD 1 TO WS-ERROR-COUNT
                   MOVE 'DATE-OF-INJURY'
                       TO WS-ERR-FIELD(WS-ERROR-COUNT)
                   MOVE 'E402'
                       TO WS-ERR-CODE(WS-ERROR-COUNT)
                   MOVE 'Invalid date of injury format (YYYYMMDD)'
                       TO WS-ERR-MSG(WS-ERROR-COUNT)
                   SET VALIDATION-FAILED TO TRUE
               ELSE
                   IF WS-WORK-DATE > WS-CURRENT-DATE
                       ADD 1 TO WS-ERROR-COUNT
                       MOVE 'DATE-OF-INJURY'
                           TO WS-ERR-FIELD(WS-ERROR-COUNT)
                       MOVE 'E403'
                           TO WS-ERR-CODE(WS-ERROR-COUNT)
                       MOVE 'Date of injury cannot be in the future'
                           TO WS-ERR-MSG(WS-ERROR-COUNT)
                       SET VALIDATION-FAILED TO TRUE
                   END-IF
               END-IF
           END-IF.

       4200-VALIDATE-BODY-PART.
           IF FI-BODY-PART-CODE < WS-BP-MIN
           OR FI-BODY-PART-CODE > WS-BP-MAX
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'BODY-PART-CODE'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'E404'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Invalid NCCI body part code (10-99)'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
               SET VALIDATION-FAILED TO TRUE
           END-IF.

       4300-VALIDATE-NATURE.
           IF FI-NATURE-CODE < WS-NAT-MIN
           OR FI-NATURE-CODE > WS-NAT-MAX
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'NATURE-CODE'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'E405'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Invalid NCCI nature of injury code (01-99)'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
               SET VALIDATION-FAILED TO TRUE
           END-IF.

       4400-VALIDATE-CAUSE.
           IF FI-CAUSE-CODE < WS-CAUSE-MIN
           OR FI-CAUSE-CODE > WS-CAUSE-MAX
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'CAUSE-CODE'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'E406'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Invalid NCCI cause of injury code (01-99)'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
               SET VALIDATION-FAILED TO TRUE
           END-IF.

       4500-VALIDATE-DESCRIPTION.
           IF FI-INJURY-DESC = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'INJURY-DESC'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'E407'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Injury description is required'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
               SET VALIDATION-FAILED TO TRUE
           END-IF.

       5000-VALIDATE-JURISDICTION.
           MOVE 'N' TO WS-DOI-VALID
           PERFORM VARYING WS-DAYS-ELAPSED
               FROM 1 BY 1 UNTIL WS-DAYS-ELAPSED > 55
               IF FI-INJURY-STATE =
                  WS-ST-CODE(WS-DAYS-ELAPSED)
                   MOVE 'Y' TO WS-DOI-VALID
               END-IF
           END-PERFORM
           IF WS-DOI-VALID = 'N'
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'INJURY-STATE'
                   TO WS-ERR-FIELD(WS-ERROR-COUNT)
               MOVE 'E501'
                   TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'Invalid injury state jurisdiction code'
                   TO WS-ERR-MSG(WS-ERROR-COUNT)
               SET VALIDATION-FAILED TO TRUE
           END-IF.

       6000-CHECK-FILING-DEADLINE.
           IF FI-DATE-OF-INJURY NOT = SPACES
           AND FI-DATE-REPORTED NOT = SPACES
               CONTINUE
           ELSE
               EXIT PARAGRAPH
           END-IF
           EVALUATE FI-INJURY-STATE
               WHEN 'CA'  MOVE 030 TO WS-FILING-LIMIT
               WHEN 'NY'  MOVE 030 TO WS-FILING-LIMIT
               WHEN 'TX'  MOVE 030 TO WS-FILING-LIMIT
               WHEN 'FL'  MOVE 030 TO WS-FILING-LIMIT
               WHEN 'PA'  MOVE 021 TO WS-FILING-LIMIT
               WHEN 'IL'  MOVE 045 TO WS-FILING-LIMIT
               WHEN 'OH'  MOVE 007 TO WS-FILING-LIMIT
               WHEN 'NJ'  MOVE 021 TO WS-FILING-LIMIT
               WHEN 'GA'  MOVE 021 TO WS-FILING-LIMIT
               WHEN 'VA'  MOVE 030 TO WS-FILING-LIMIT
               WHEN OTHER MOVE 030 TO WS-FILING-LIMIT
           END-EVALUATE.

       7000-VALIDATE-DATE-FORMAT.
           MOVE 'Y' TO WS-DOI-VALID
           MOVE WS-WORK-DATE(1:4) TO WS-WORK-YEAR
           MOVE WS-WORK-DATE(5:2) TO WS-WORK-MONTH
           MOVE WS-WORK-DATE(7:2) TO WS-WORK-DAY
           IF WS-WORK-DATE IS NOT NUMERIC
               MOVE 'N' TO WS-DOI-VALID
           ELSE
               IF WS-WORK-YEAR < 1920
               OR WS-WORK-YEAR > 2099
                   MOVE 'N' TO WS-DOI-VALID
               END-IF
               IF WS-WORK-MONTH < 01
               OR WS-WORK-MONTH > 12
                   MOVE 'N' TO WS-DOI-VALID
               END-IF
               IF WS-WORK-DAY < 01
               OR WS-WORK-DAY > 31
                   MOVE 'N' TO WS-DOI-VALID
               END-IF
           END-IF.

       8000-ADD-ERROR-FEIN-MISSING.
           ADD 1 TO WS-ERROR-COUNT
           MOVE 'EMPLOYER-FEIN'
               TO WS-ERR-FIELD(WS-ERROR-COUNT)
           MOVE 'E101'
               TO WS-ERR-CODE(WS-ERROR-COUNT)
           MOVE 'Employer FEIN is required'
               TO WS-ERR-MSG(WS-ERROR-COUNT)
           SET VALIDATION-FAILED TO TRUE.

       8010-ADD-ERROR-FEIN-FORMAT.
           ADD 1 TO WS-ERROR-COUNT
           MOVE 'EMPLOYER-FEIN'
               TO WS-ERR-FIELD(WS-ERROR-COUNT)
           MOVE 'E102'
               TO WS-ERR-CODE(WS-ERROR-COUNT)
           MOVE 'Employer FEIN must be 9 numeric digits'
               TO WS-ERR-MSG(WS-ERROR-COUNT)
           SET VALIDATION-FAILED TO TRUE.

       8020-ADD-ERROR-POLICY-MISSING.
           ADD 1 TO WS-ERROR-COUNT
           MOVE 'POLICY-NUMBER'
               TO WS-ERR-FIELD(WS-ERROR-COUNT)
           MOVE 'E103'
               TO WS-ERR-CODE(WS-ERROR-COUNT)
           MOVE 'Policy number is required'
               TO WS-ERR-MSG(WS-ERROR-COUNT)
           SET VALIDATION-FAILED TO TRUE.

       8030-ADD-ERROR-POLICY-FORMAT.
           ADD 1 TO WS-ERROR-COUNT
           MOVE 'POLICY-NUMBER'
               TO WS-ERR-FIELD(WS-ERROR-COUNT)
           MOVE 'E104'
               TO WS-ERR-CODE(WS-ERROR-COUNT)
           MOVE 'Policy number must start with 3 alpha characters'
               TO WS-ERR-MSG(WS-ERROR-COUNT)
           SET VALIDATION-FAILED TO TRUE.

       9000-BUILD-RESPONSE.
           MOVE WS-ERROR-COUNT TO FO-ERROR-COUNT
           IF VALIDATION-PASSED
               MOVE 00 TO FO-RETURN-CODE
           ELSE
               MOVE 08 TO FO-RETURN-CODE
           END-IF
           MOVE FI-INJURY-STATE TO FO-JURISDICTION
           PERFORM VARYING WS-DAYS-ELAPSED
               FROM 1 BY 1
               UNTIL WS-DAYS-ELAPSED > WS-ERROR-COUNT
               MOVE WS-ERR-FIELD(WS-DAYS-ELAPSED)
                   TO FO-ERR-FIELD(WS-DAYS-ELAPSED)
               MOVE WS-ERR-CODE(WS-DAYS-ELAPSED)
                   TO FO-ERR-CODE(WS-DAYS-ELAPSED)
               MOVE WS-ERR-MSG(WS-DAYS-ELAPSED)
                   TO FO-ERR-MSG(WS-DAYS-ELAPSED)
           END-PERFORM.
