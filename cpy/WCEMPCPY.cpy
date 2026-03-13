      ******************************************************************
      * COPYBOOK:  WCEMPCPY
      * PURPOSE:   Workers' Compensation Employer and Policy Records
      * SYSTEM:    Claims Management System (CMS)
      * PLATFORM:  HPE NonStop / Enscribe Key-Sequenced File
      * FILE:      $DATA1.CLMDB.EMPMSTR (employer master)
      *            $DATA1.CLMDB.EMPPOL  (policy records)
      *            $DATA1.CLMDB.EMPLOC  (location records)
      *            $DATA1.CLMDB.EMPWAGE (wage history)
      * KEY:       WC-EMP-FEIN (primary), alternate key on name
      * AUTHOR:    TPA Systems Development
      * CREATED:   2023-12-01
      * MODIFIED:  2025-01-18 - Added NAICS codes, RTW program fields
      *
      * Contains four record layouts:
      *   1. Employer Master Record
      *   2. Policy Record (per carrier/period)
      *   3. Location Record (multi-location employers)
      *   4. Wage Record Structure (52-week history for AWW calc)
      ******************************************************************
      *
      *================================================================
      * EMPLOYER MASTER RECORD
      * One record per employer (keyed by FEIN). Employers may have
      * multiple policies across carriers and time periods.
      * Record length: 512 bytes (fixed)
      *================================================================
      *
       01  WC-EMPLOYER-MASTER-RECORD.
      *
      *--- Identification
      *
           05  WC-EMP-FEIN                PIC X(9).
           05  WC-EMP-CLIENT-CODE         PIC X(6).
           05  WC-EMP-LEGAL-NAME          PIC X(40).
           05  WC-EMP-DBA-NAME            PIC X(40).
           05  WC-EMP-STATUS              PIC X(2).
               88  WC-EMP-ACTIVE              VALUE "AC".
               88  WC-EMP-INACTIVE            VALUE "IN".
               88  WC-EMP-TERMINATED          VALUE "TM".
               88  WC-EMP-SUSPENDED           VALUE "SU".
      *
      *--- Industry Classification
      *
           05  WC-EMP-INDUSTRY-INFO.
               10  WC-EMP-SIC-CODE        PIC X(4).
               10  WC-EMP-NAICS-CODE      PIC X(6).
               10  WC-EMP-NCCI-CLASS-CODE PIC X(4).
               10  WC-EMP-STATE-CLASS     PIC X(6).
               10  WC-EMP-RISK-GROUP      PIC X(2).
                   88  WC-EMP-RISK-LOW        VALUE "LO".
                   88  WC-EMP-RISK-MEDIUM     VALUE "MD".
                   88  WC-EMP-RISK-HIGH       VALUE "HI".
                   88  WC-EMP-RISK-CRITICAL   VALUE "CR".
      *
      *--- Primary Address
      *
           05  WC-EMP-ADDRESS.
               10  WC-EMP-ADDR-LINE1      PIC X(35).
               10  WC-EMP-ADDR-LINE2      PIC X(35).
               10  WC-EMP-CITY            PIC X(25).
               10  WC-EMP-STATE           PIC X(2).
               10  WC-EMP-ZIP             PIC X(10).
               10  WC-EMP-COUNTY          PIC X(20).
               10  WC-EMP-COUNTRY         PIC X(3).
      *
      *--- Contacts
      *
           05  WC-EMP-PRIMARY-CONTACT.
               10  WC-EMP-CONTACT-NAME    PIC X(30).
               10  WC-EMP-CONTACT-TITLE   PIC X(20).
               10  WC-EMP-CONTACT-PHONE   PIC X(10).
               10  WC-EMP-CONTACT-EXT     PIC X(5).
               10  WC-EMP-CONTACT-FAX     PIC X(10).
               10  WC-EMP-CONTACT-EMAIL   PIC X(40).
      *
           05  WC-EMP-CLAIMS-CONTACT.
               10  WC-EMP-CLM-CONTACT-NM  PIC X(30).
               10  WC-EMP-CLM-CONTACT-PH  PIC X(10).
               10  WC-EMP-CLM-CONTACT-EM  PIC X(40).
      *
      *--- Workforce Information
      *
           05  WC-EMP-WORKFORCE-INFO.
               10  WC-EMP-EMPLOYEE-COUNT  PIC 9(6) COMP.
               10  WC-EMP-FTE-COUNT       PIC 9(6) COMP.
               10  WC-EMP-ANNUAL-PAYROLL  PIC S9(11)V99 COMP.
               10  WC-EMP-PAYROLL-FREQ    PIC X(1).
                   88  WC-EMP-PAY-WEEKLY      VALUE "W".
                   88  WC-EMP-PAY-BIWEEKLY    VALUE "B".
                   88  WC-EMP-PAY-SEMIMONTHLY VALUE "S".
                   88  WC-EMP-PAY-MONTHLY     VALUE "M".
               10  WC-EMP-UNION-FLAG      PIC X(1).
                   88  WC-EMP-IS-UNION        VALUE "Y".
                   88  WC-EMP-NON-UNION       VALUE "N".
      *
      *--- Return-to-Work Program
      *
           05  WC-EMP-RTW-PROGRAM.
               10  WC-EMP-RTW-INDICATOR   PIC X(1).
                   88  WC-EMP-HAS-RTW         VALUE "Y".
                   88  WC-EMP-NO-RTW          VALUE "N".
               10  WC-EMP-RTW-TYPE        PIC X(2).
                   88  WC-EMP-RTW-FORMAL      VALUE "FM".
                   88  WC-EMP-RTW-INFORMAL    VALUE "IF".
                   88  WC-EMP-RTW-TRANSITIONAL VALUE "TR".
               10  WC-EMP-RTW-CONTACT     PIC X(30).
               10  WC-EMP-RTW-PHONE       PIC X(10).
               10  WC-EMP-LIGHT-DUTY-AVAIL PIC X(1).
                   88  WC-EMP-LD-AVAILABLE    VALUE "Y".
                   88  WC-EMP-LD-NOT-AVAIL    VALUE "N".
                   88  WC-EMP-LD-SOMETIMES    VALUE "S".
      *
      *--- Audit
      *
           05  WC-EMP-AUDIT-INFO.
               10  WC-EMP-CREATED-BY      PIC X(8).
               10  WC-EMP-CREATED-TS      PIC X(26).
               10  WC-EMP-MODIFIED-BY     PIC X(8).
               10  WC-EMP-MODIFIED-TS     PIC X(26).
               10  WC-EMP-RECORD-VERSION  PIC 9(6).
      *
           05  WC-EMP-FILLER             PIC X(18).
      *
      *================================================================
      * POLICY RECORD
      * One record per policy period per carrier. An employer may
      * have multiple sequential or overlapping policies. The DOI
      * determines which policy applies to a given claim.
      * File: $DATA1.CLMDB.EMPPOL
      * Key: WC-POL-FEIN + WC-POL-EFFECTIVE-DATE
      * Record length: 384 bytes (fixed)
      *================================================================
      *
       01  WC-POLICY-RECORD.
      *
           05  WC-POL-FEIN                PIC X(9).
           05  WC-POL-POLICY-NUMBER       PIC X(15).
           05  WC-POL-CARRIER-CODE        PIC X(5).
           05  WC-POL-CARRIER-NAME        PIC X(35).
           05  WC-POL-CARRIER-NAIC        PIC X(5).
      *
           05  WC-POL-DATES.
               10  WC-POL-EFFECTIVE-DATE  PIC X(8).
               10  WC-POL-EXPIRATION-DATE PIC X(8).
               10  WC-POL-CANCEL-DATE     PIC X(8).
      *
           05  WC-POL-STATUS              PIC X(2).
               88  WC-POL-IN-FORCE            VALUE "IF".
               88  WC-POL-EXPIRED             VALUE "EX".
               88  WC-POL-CANCELLED           VALUE "CN".
               88  WC-POL-REINSTATED          VALUE "RI".
      *
           05  WC-POL-FINANCIAL-INFO.
               10  WC-POL-PREMIUM-ANNUAL  PIC S9(9)V99 COMP.
               10  WC-POL-DEDUCTIBLE      PIC S9(7)V99 COMP.
               10  WC-POL-SIR-AMOUNT      PIC S9(9)V99 COMP.
               10  WC-POL-AGGREGATE-LIMIT PIC S9(11)V99 COMP.
               10  WC-POL-PER-OCC-LIMIT   PIC S9(9)V99 COMP.
      *
           05  WC-POL-EXPERIENCE-MOD.
               10  WC-POL-EMOD-FACTOR     PIC 9V9999 COMP-3.
               10  WC-POL-EMOD-EFF-DATE   PIC X(8).
               10  WC-POL-EMOD-STATUS     PIC X(2).
                   88  WC-POL-EMOD-FINAL      VALUE "FN".
                   88  WC-POL-EMOD-INTERIM    VALUE "IN".
                   88  WC-POL-EMOD-PENDING    VALUE "PD".
      *
           05  WC-POL-COVERAGE-INFO.
               10  WC-POL-COVERAGE-TYPE   PIC X(2).
                   88  WC-POL-COV-STANDARD    VALUE "ST".
                   88  WC-POL-COV-LARGE-DED   VALUE "LD".
                   88  WC-POL-COV-RETRO       VALUE "RT".
                   88  WC-POL-COV-SELF-INS    VALUE "SI".
               10  WC-POL-MONOPOLISTIC-ST PIC X(1).
                   88  WC-POL-IS-MONOPOLISTIC VALUE "Y".
                   88  WC-POL-NOT-MONOPOL     VALUE "N".
               10  WC-POL-STATES-COVERED  PIC X(100).
      *
           05  WC-POL-TPA-INFO.
               10  WC-POL-TPA-CLIENT-CODE PIC X(6).
               10  WC-POL-SERVICE-OFFICE  PIC X(4).
               10  WC-POL-PROGRAM-CODE    PIC X(4).
      *
           05  WC-POL-AUDIT-INFO.
               10  WC-POL-CREATED-BY      PIC X(8).
               10  WC-POL-CREATED-TS      PIC X(26).
               10  WC-POL-MODIFIED-BY     PIC X(8).
               10  WC-POL-MODIFIED-TS     PIC X(26).
      *
           05  WC-POL-FILLER             PIC X(24).
      *
      *================================================================
      * LOCATION RECORD
      * Multi-location employers require separate records per site.
      * Some jurisdictions require reporting by location.
      * File: $DATA1.CLMDB.EMPLOC
      * Key: WC-LOC-FEIN + WC-LOC-CODE
      * Record length: 256 bytes (fixed)
      *================================================================
      *
       01  WC-LOCATION-RECORD.
      *
           05  WC-LOC-FEIN                PIC X(9).
           05  WC-LOC-CODE               PIC X(6).
           05  WC-LOC-NAME               PIC X(30).
           05  WC-LOC-STATUS             PIC X(2).
               88  WC-LOC-ACTIVE             VALUE "AC".
               88  WC-LOC-CLOSED             VALUE "CL".
      *
           05  WC-LOC-ADDRESS.
               10  WC-LOC-ADDR-LINE1     PIC X(35).
               10  WC-LOC-ADDR-LINE2     PIC X(35).
               10  WC-LOC-CITY           PIC X(25).
               10  WC-LOC-STATE          PIC X(2).
               10  WC-LOC-ZIP            PIC X(10).
               10  WC-LOC-COUNTY         PIC X(20).
      *
           05  WC-LOC-EMPLOYEE-COUNT     PIC 9(5) COMP.
           05  WC-LOC-PAYROLL            PIC S9(9)V99 COMP.
           05  WC-LOC-CLASS-CODE         PIC X(4).
           05  WC-LOC-CONTACT-NAME       PIC X(25).
           05  WC-LOC-CONTACT-PHONE      PIC X(10).
      *
           05  WC-LOC-AUDIT-INFO.
               10  WC-LOC-CREATED-BY     PIC X(8).
               10  WC-LOC-CREATED-TS     PIC X(26).
      *
           05  WC-LOC-FILLER             PIC X(12).
      *
      *================================================================
      * WAGE RECORD STRUCTURE
      * 52-week wage history for Average Weekly Wage (AWW) calculation.
      * AWW is the basis for all indemnity benefit calculations.
      * Most jurisdictions use the 52 weeks prior to DOI.
      * Some states (e.g., PA, NJ) use different lookback periods.
      * File: $DATA1.CLMDB.EMPWAGE
      * Key: WC-WAGE-CLAIM-NUMBER (one record per claim)
      * Record length: 512 bytes (fixed)
      *================================================================
      *
       01  WC-WAGE-RECORD.
      *
           05  WC-WAGE-CLAIM-NUMBER       PIC X(12).
           05  WC-WAGE-EMPLOYEE-ID        PIC X(12).
           05  WC-WAGE-FEIN               PIC X(9).
      *
           05  WC-WAGE-CALCULATION.
               10  WC-WAGE-METHOD         PIC X(2).
                   88  WC-WAGE-52-WEEK        VALUE "52".
                   88  WC-WAGE-26-WEEK        VALUE "26".
                   88  WC-WAGE-13-WEEK        VALUE "13".
                   88  WC-WAGE-ACTUAL-EARN    VALUE "AE".
                   88  WC-WAGE-SIMILAR-EMP    VALUE "SE".
               10  WC-WAGE-COMPUTED-AWW   PIC S9(7)V99 COMP.
               10  WC-WAGE-OVERRIDE-AWW   PIC S9(7)V99 COMP.
               10  WC-WAGE-FINAL-AWW      PIC S9(7)V99 COMP.
               10  WC-WAGE-OVERRIDE-BY    PIC X(8).
               10  WC-WAGE-OVERRIDE-REAS  PIC X(30).
      *
           05  WC-WAGE-COMPONENTS.
               10  WC-WAGE-BASE-SALARY    PIC S9(9)V99 COMP.
               10  WC-WAGE-OVERTIME       PIC S9(7)V99 COMP.
               10  WC-WAGE-TIPS           PIC S9(7)V99 COMP.
               10  WC-WAGE-BONUSES        PIC S9(7)V99 COMP.
               10  WC-WAGE-COMMISSIONS    PIC S9(7)V99 COMP.
               10  WC-WAGE-HOUSING-VALUE  PIC S9(7)V99 COMP.
               10  WC-WAGE-FRINGE-BENE    PIC S9(7)V99 COMP.
               10  WC-WAGE-CONCURRENT-EM  PIC S9(7)V99 COMP.
      *
           05  WC-WAGE-WEEKLY-HISTORY.
               10  WC-WAGE-WEEK OCCURS 52 TIMES.
                   15  WC-WAGE-WK-END-DT  PIC X(8).
                   15  WC-WAGE-WK-HOURS   PIC 99V9 COMP-3.
                   15  WC-WAGE-WK-GROSS   PIC S9(7)V99 COMP.
      *
           05  WC-WAGE-WEEKS-WORKED       PIC 99.
           05  WC-WAGE-TOTAL-GROSS        PIC S9(9)V99 COMP.
           05  WC-WAGE-AVG-HOURS-WEEK     PIC 99V9 COMP-3.
      *
           05  WC-WAGE-EMPLOYMENT-INFO.
               10  WC-WAGE-JOB-TITLE      PIC X(25).
               10  WC-WAGE-HIRE-DATE      PIC X(8).
               10  WC-WAGE-OCCUPATION-CD  PIC X(6).
               10  WC-WAGE-PAY-FREQUENCY  PIC X(1).
               10  WC-WAGE-HOURLY-RATE    PIC S9(5)V99 COMP.
               10  WC-WAGE-SCHED-HRS-WK   PIC 99V9 COMP-3.
      *
           05  WC-WAGE-AUDIT.
               10  WC-WAGE-CREATED-BY     PIC X(8).
               10  WC-WAGE-CREATED-TS     PIC X(26).
               10  WC-WAGE-MODIFIED-BY    PIC X(8).
               10  WC-WAGE-MODIFIED-TS    PIC X(26).
      *
           05  WC-WAGE-FILLER            PIC X(16).
