      *============================================================
      * CLMCOPY -- Shared Claim Record Copybook
      * Used by CLMENTRY, ELGCHECK, and batch programs
      *============================================================

       01  CLAIM-COMMON-RECORD.
           05  CC-CLAIM-ID             PIC X(12).
           05  CC-POLICY-NUMBER        PIC X(16).
           05  CC-CLAIMANT-NAME        PIC X(40).
           05  CC-CLAIMANT-SSN         PIC X(9).
           05  CC-CLAIM-TYPE           PIC 9(2).
           05  CC-CLAIM-AMOUNT         PIC S9(9)V99 COMP-3.
           05  CC-APPROVED-AMOUNT      PIC S9(9)V99 COMP-3.
           05  CC-PAID-AMOUNT          PIC S9(9)V99 COMP-3.
           05  CC-MEMBER-RESP          PIC S9(9)V99 COMP-3.
           05  CC-STATUS               PIC 9(2).
               88  CC-STATUS-NEW         VALUE 00.
               88  CC-STATUS-PENDING     VALUE 01.
               88  CC-STATUS-APPROVED    VALUE 02.
               88  CC-STATUS-DENIED      VALUE 03.
               88  CC-STATUS-PAID        VALUE 04.
               88  CC-STATUS-VOIDED      VALUE 09.
           05  CC-DATE-RECEIVED        PIC 9(8).
           05  CC-DATE-PROCESSED       PIC 9(8).
           05  CC-DATE-PAID            PIC 9(8).
           05  CC-PROVIDER-ID          PIC X(10).
           05  CC-PROVIDER-NAME        PIC X(40).
           05  CC-ADJUSTER-ID          PIC X(8).
           05  CC-DENIAL-REASON        PIC X(80).
           05  CC-LINE-ITEM-COUNT      PIC 9(3).
           05  CC-LINE-ITEMS.
               10  CC-LINE-ITEM OCCURS 50 TIMES.
                   15  CC-LI-SEQ       PIC 9(3).
                   15  CC-LI-SVC-CODE  PIC X(10).
                   15  CC-LI-DESC      PIC X(60).
                   15  CC-LI-BILLED    PIC S9(9)V99 COMP-3.
                   15  CC-LI-ALLOWED   PIC S9(9)V99 COMP-3.
                   15  CC-LI-PAID      PIC S9(9)V99 COMP-3.
                   15  CC-LI-UNITS     PIC 9(3).
                   15  CC-LI-SVC-DATE  PIC 9(8).
                   15  CC-LI-MOD1      PIC X(2).
                   15  CC-LI-MOD2      PIC X(2).
                   15  CC-LI-DIAG-PTR  PIC 9(2).

       01  PROVIDER-COMMON-RECORD.
           05  PC-PROVIDER-ID          PIC X(10).
           05  PC-PROVIDER-NPI         PIC X(10).
           05  PC-PROVIDER-NAME        PIC X(40).
           05  PC-PROVIDER-TYPE        PIC 9(2).
               88  PC-TYPE-PHYSICIAN     VALUE 01.
               88  PC-TYPE-HOSPITAL      VALUE 02.
               88  PC-TYPE-LAB           VALUE 03.
               88  PC-TYPE-PHARMACY      VALUE 04.
               88  PC-TYPE-DME           VALUE 05.
           05  PC-TAX-ID               PIC X(9).
           05  PC-ADDRESS-LINE1        PIC X(40).
           05  PC-ADDRESS-LINE2        PIC X(40).
           05  PC-CITY                 PIC X(30).
           05  PC-STATE                PIC X(2).
           05  PC-ZIP                  PIC X(10).
           05  PC-PHONE                PIC X(10).
           05  PC-PAR-STATUS           PIC X(1).
               88  PC-PAR                VALUE 'Y'.
               88  PC-NON-PAR            VALUE 'N'.
