       IDENTIFICATION DIVISION.
       PROGRAM-ID. KOBOLD-ARENA.

      * TODO: Add some variety to run-away messages (RNG from a list?).

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MONSTERS ASSIGN TO 'MONSTERS.INC'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS FS.

       DATA DIVISION.

       FILE SECTION.
       FD MONSTERS.
       01 MONSTER-FILE.
           05 FILLER-01    PIC X(6)    VALUE "TYPE: ".
           05 MF-TYPE      PIC X(5).
           05 FILLER-02    PIC X(7)    VALUE " NAME: ".
           05 MF-NAME      PIC X(20).
           05 FILLER-03    PIC X(6)    VALUE " ATK: ".
           05 MF-ATTACK    PIC 9(2)    VALUE 0.
           05 FILLER-04    PIC X(9)    VALUE " SP-ATK: ".
           05 MF-SPATTACK  PIC 9(2)    VALUE 0.
           05 FILLER-05    PIC X(6)    VALUE " DEF: ".
           05 MF-DEFENSE   PIC 9(2)    VALUE 0.
           05 FILLER-06    PIC X(9)    VALUE " SP-DEF: ".
           05 MF-SPDEFENSE PIC 9(2)    VALUE 0.

       WORKING-STORAGE SECTION.

       77 FS PIC X(10).
       77 ONE PIC 9 VALUE 1.
       77 IS-INPUT-OK PIC 9.
                   88 INPUT-GOOD VALUE "Y".
                   88 INPUT-BAD  VALUE "N".
       77 PL-DEFEND PIC 9.
       77 MON-DEFEND PIC 9.

       77 DEFEND-RATIO PIC 9V99 VALUE 0.25.

       77 RNG-MIN-VAL PIC 99.
       77 RNG-MAX-VAL PIC 99.

       77 UPPER-TEXT PIC X(30).

      * sum of the stat values for the player
       77 PLAYER-STAT-TOTAL PIC 999 value 150.

       01 GEN-STAT-RANGES.
           05 RANGE-ATK-MAX    PIC 99.
           05 RANGE-ATK-MIN    PIC 99.
           05 RANGE-SPATK-MAX  PIC 99.
           05 RANGE-SPATK-MIN  PIC 99.

           05 RANGE-DEF-MAX    PIC 99.
           05 RANGE-DEF-MIN    PIC 99.
           05 RANGE-SPDEF-MAX  PIC 99.
           05 RANGE-SPDEF-MIN  PIC 99.

           05 RANGE-RATIO-ATK-MAX PIC 99.
           05 RANGE-RATIO-ATK-MIN PIC 99.

           05 RANGE-RATIO-SP-MAX PIC 99.
           05 RANGE-RATIO-SP-MIN PIC 99.

       01 INPUT-LINE PIC X(100).
                   88 INPUT-ATTACK     VALUE "ATTACK" "A".
                   88 INPUT-EXIT       VALUE "EXIT" "X" "RUN".
                   88 INPUT-SHOW       VALUE "SHOW".
                   88 INPUT-SPWATER    VALUE "SPWATER".
                   88 INPUT-SPFIRE     VALUE "SPFIRE".
                   88 INPUT-SPEARTH    VALUE "SPEARTH".
                   88 INPUT-DEFEND     VALUE "DEFEND" "D".

      * Current monster ID in MONSTERS
       01 TMP-NUM      PIC S99V99.
       01 TMP-NUM-BIG  PIC S9(5).
       01 TMP-DEF      PIC S99V99.
       01 TMP-ATK      PIC S99V99.
       01 TMP-UINT     PIC 9(4).
       01 TMP-DOT      PIC 99.
       01 TMP-DOT3     PIC 9(3).

       01 DISPLAY-TEXT PIC X(60) VALUE SPACES.

       01 TMP-ATK-TYPE PIC X(5).
                   88 TA-WATER     VALUE "WATER".
                   88 TA-FIRE      VALUE "FIRE".
                   88 TA-EARTH     VALUE "EARTH".
       01 TMP-DEF-TYPE PIC X(5).
                   88 TD-WATER     VALUE "WATER".
                   88 TD-FIRE      VALUE "FIRE".
                   88 TD-EARTH     VALUE "EARTH".

       01 DO-MONSTER-ATTACK PIC 9 VALUE 0.
       01 INPUT-ERROR-01 PIC X(35).
       01 INPUT-ERROR-02 PIC X(30).

       01 THE-PLAYER.
           05 PL-HEALTH    PIC S9(3)   VALUE 100.
           05 PL-ATTACK    PIC 9(2)    VALUE 0.
           05 PL-SPATTACK  PIC 9(2)    VALUE 0.
           05 PL-DEFENSE   PIC 9(2)    VALUE 0.
           05 PL-SPDEFENSE PIC 9(2)    VALUE 0.
           05 PL-TYPE      PIC X(5).
                   88 PL-WATER     VALUE "WATER".
                   88 PL-FIRE      VALUE "FIRE".
                   88 PL-EARTH     VALUE "EARTH".

       01 CUR-MONSTER.
           05 MON-TYPE     PIC X(5).
                   88 MT-WATER     VALUE "WATER".
                   88 MT-FIRE      VALUE "FIRE".
                   88 MT-EARTH     VALUE "EARTH".

           05 MON-NAME         PIC X(20).
           05 MON-HEALTH       PIC S9(3)   VALUE 100.
           05 MON-ATTACK       PIC 9(2)    VALUE 0.
           05 MON-SPATTACK     PIC 9(2)    VALUE 0.
           05 MON-DEFENSE      PIC 9(2)    VALUE 0.
           05 MON-SPDEFENSE    PIC 9(2)    VALUE 0.
           05 MON-ATK-RATIO    PIC 99      VALUE 0.
           05 MON-SP-RATIO     PIC 99      VALUE 0.

       01 DISP-MONSTER.
           05 FILLER       PIC X(3) VALUE "ID ".
           05 DM-ID        PIC 9(5).
           05 FILLER       PIC X(4) VALUE " HP ".
           05 DM-HEALTH    PIC 9(3) VALUE 100.
           05 FILLER       PIC X(5) VALUE " ATT ".
           05 DM-ATTACK    PIC 9(2) VALUE 0.
           05 FILLER       PIC X(5) VALUE " DEF ".
           05 DM-DEFENSE   PIC 9(2) VALUE 0.
           05 FILLER       PIC X(8) VALUE " SP-ATT ".
           05 DM-SPATTACK  PIC 9(2) VALUE 0.
           05 FILLER       PIC X(8) VALUE " SP-DEF ".
           05 DM-SPDEFENSE PIC 9(2) VALUE 0.
           05 FILLER       PIC X(8) VALUE "   TYPE ".
           05 DM-TYPE      PIC X(5).
           05 FILLER       PIC X(8) VALUE "   NAME ".
           05 DM-NAME      PIC X(20).
           05 DM-RATIO     PIC 99.

       01 WS-CURRENT-DATE-DATA.
           05 WS-CURRENT-DATE.
               10 WS-CURRENT-YEAR  PIC 9(4).
               10 WS-CURRENT-MONTH PIC 9(2).
               10 WS-CURRENT-DAY   PIC 9(2).
           05 WS-CURRENT-TIME.
               10 WS-CURRENT-HOURS         PIC 9(2).
               10 WS-CURRENT-MINUTE        PIC 9(2).
               10 WS-CURRENT-SECOND        PIC 9(2).
               10 WS-CURRENT-MILLISECONDS  PIC 9(2).
           05 WS-DIFF-FROM-GMT     PIC S9(4).

       01 GAME-STATS.
           05 ST-MONSTERS PIC 9(4).
           05 ST-DMG-DEALT PIC 9(6).
           05 ST-DMG-RECEIVED PIC 9(6).
           05 ST-AT-USED PIC 9(4).
           05 ST-SP-USED PIC 9(4).

       SCREEN SECTION.
       01 SCREEN-BATTLE.
           05 SB-MONSTER-NAME BLANK SCREEN LINE 1 COL 2
               PIC X(20)   FROM DM-NAME.
           05 VALUE "TYPE"                 LINE 2 COL 5.
           05 SB-MONSTER-TYPE              LINE 2 COL 15
               PIC X(5)    FROM DM-TYPE.
           05 VALUE "HEALTH"               LINE 3 COL 5.
           05 SB-MONSTER-HEALTH            LINE 3 COL 15
               PIC X(3)    FROM DM-HEALTH.

           05 VALUE "ATK"                  LINE 5 COL 5.
           05 SB-MONSTER-ATK               LINE 5 COL 15
               PIC 9(2)    FROM DM-ATTACK.
           05 VALUE "DEF"                  LINE 6 COL 5.
           05 SB-MONSTER-DEF               LINE 6 COL 15
               PIC 9(2)    FROM DM-DEFENSE.

           05 VALUE "SP-ATK"               LINE 5 COL 25.
           05 SB-MONSTER-SPATK             LINE 5 COL 35
               PIC 9(2)    FROM DM-SPATTACK.
           05 VALUE "SP-DEF"               LINE 6 COL 25.
           05 SB-MONSTER-SPDEF             LINE 6 COL 35
               PIC 9(2)    FROM DM-SPDEFENSE.

           05 VALUE "PLAYER"               LINE 8 COL 2.
           05 VALUE "HEALTH"               LINE 9 COL 5.

           05 VALUE "ATK"                  LINE 11 COL 5.
           05 SB-PLAYER-ATK                LINE 11 COL 15
               PIC 9(2)    FROM PL-ATTACK.
           05 VALUE "DEF"                  LINE 12 COL 5.
           05 SB-PLAYER-DEF                LINE 12 COL 15
               PIC 9(2)    FROM PL-DEFENSE.

           05 VALUE "SP-ATK"               LINE 11 COL 25.
           05 SB-PLAYER-SPATK              LINE 11 COL 35
               PIC 9(2)    FROM PL-SPATTACK.
           05 VALUE "SP-DEF"               LINE 12 COL 25.
           05 SB-PLAYER-SPDEF              LINE 12 COL 35
               PIC 9(2)    FROM PL-SPDEFENSE.

           05 SB-PLAYER-HEALTH             LINE 9 COL 15
               PIC 9(3)    FROM PL-HEALTH.
           05 VALUE "ACTION"               LINE 14 COL 2.
           05 SB-INPUT                     LINE 15 COL 5
               PIC x(10)   USING INPUT-LINE.


       01 SCREEN-INFO.
           05 SI-TEXT    BLANK SCREEN      LINE 3 COL 10
               PIC X(60) FROM DISPLAY-TEXT.
           05 SI-INPUT                     LINE 6 COL 2
               PIC X(2) USING INPUT-LINE.

       01 SCREEN-STATS.
           05 VALUE "MONSTERS FOUGHT" BLANK SCREEN
                                           LINE 2 COL 5.
           05 SS-MONSTERS                  LINE 2 COL 25
               FROM ST-MONSTERS.

           05 VALUE "DAMAGE DEALT"         LINE 3 COL 5.
           05 SS-DMG-DEALT                 LINE 3 COL 25
               FROM ST-DMG-DEALT.

           05 VALUE "DAMAGE RECEIVED"      LINE 4 COL 5.
           05 SS-DMG-RECEIVED              LINE 4 COL 25
               FROM ST-DMG-RECEIVED.

           05 VALUE "ATTACKS USED"         LINE 5 COL 5.
           05 SS-AT-USED                   LINE 5 COL 25
               FROM ST-AT-USED.

           05 VALUE "SP ATTACKS USED"      LINE 6 COL 5.
           05 SS-SP-USED                   LINE 6 COL 25
               FROM ST-SP-USED.

           05 SI-INPUT                     LINE 8 COL 2
               PIC X(2) USING INPUT-LINE.

       01 SCREEN-PLAYER-STATS.
           05 VALUE "INPUT PLAYER STATS. 125 PTS TOTAL."
                                           LINE 2 COL 2.
           05 VALUE "ATK"                  LINE 4 COL 5.
           05 SPS-ATTACK                   LINE 4 COL 15
               USING PL-ATTACK.

           05 VALUE "DEF"                  LINE 5 COL 5.
           05 SPS-DEFENSE                  LINE 5 COL 15
               USING PL-DEFENSE.

           05 VALUE "SPATK"                LINE 6 COL 5.
           05 SPS-ATTACK                   LINE 6 COL 15
               USING PL-SPATTACK.

           05 VALUE "SPDEF"                LINE 7 COL 5.
           05 SPS-SPDEFENSE                LINE 7 COL 15
               USING PL-SPDEFENSE.

           05 VALUE "TYPE"                 LINE 8 COL 5.
           05 SPS-TYPE                     LINE 8 COL 15
               USING PL-TYPE.

           05 SPS-MESSAGE-01               LINE 11 COL 2
               FROM INPUT-ERROR-01.
           05 SPS-MESSAGE-02               LINE 8 COL 22
               FROM INPUT-ERROR-02.

       PROCEDURE DIVISION.

           ACCEPT WS-CURRENT-TIME FROM TIME
           MOVE FUNCTION RANDOM(WS-CURRENT-MILLISECONDS) TO TMP-NUM

           MOVE 0 TO PL-ATTACK
           MOVE 0 TO PL-DEFENSE
           MOVE 0 TO PL-SPATTACK
           MOVE 0 TO PL-SPDEFENSE
           MOVE SPACES TO PL-TYPE
           MOVE "N" TO IS-INPUT-OK

           MOVE ONE TO TMP-NUM-BIG.
           PERFORM UNTIL TMP-NUM-BIG EQUALS ZERO AND INPUT-GOOD
               DISPLAY SCREEN-PLAYER-STATS
               ACCEPT SCREEN-PLAYER-STATS

               ADD PL-ATTACK PL-DEFENSE PL-SPATTACK PL-SPDEFENSE
               GIVING TMP-NUM-BIG

               MOVE PL-TYPE TO UPPER-TEXT
               PERFORM TO-UPPER
               MOVE UPPER-TEXT TO PL-TYPE

               MOVE "N" TO IS-INPUT-OK
               IF PL-WATER OR PL-FIRE OR PL-EARTH
                   MOVE "Y" TO IS-INPUT-OK
                   MOVE SPACES TO INPUT-ERROR-02
               ELSE
                   MOVE "INVALID TYPE" TO INPUT-ERROR-02
               END-IF

               SUBTRACT PLAYER-STAT-TOTAL
                   FROM TMP-NUM-BIG
               MOVE TMP-NUM-BIG TO TMP-DOT3
               MOVE SPACES TO INPUT-ERROR-01

               IF TMP-NUM-BIG IS GREATER THAN ZERO
                   STRING
                       "TOO MANY POINTS ALLOCATED (+" DELIMITED BY SIZE
                       TMP-DOT3 DELIMITED BY SIZE
                       ")" DELIMITED BY SIZE
                       INTO INPUT-ERROR-01
               ELSE IF TMP-NUM-BIG IS LESS THAN ZERO
                   STRING
                       "TOO FEW POINTS ALLOCATED (-" DELIMITED BY SIZE
                       TMP-DOT3 DELIMITED BY SIZE
                       ")" DELIMITED BY SIZE
                       INTO INPUT-ERROR-01
               END-IF
           END-PERFORM.

           OPEN OUTPUT MONSTERS.

           MOVE "ENTERING THE ARENA" TO DISPLAY-TEXT
           PERFORM DISPLAY-INFO-SCREEN

      * generate a monster with stats
      * REPL the attacks
      * when player health is zero, you die and game is over
      * when monster health is zero, it dies, spawn new monster
           PERFORM UNTIL ONE EQUAL ZERO
               PERFORM GENERATE-MONSTER
               PERFORM REPL-LOOP

               IF PL-HEALTH IS LESS THAN OR EQUAL TO 0
                   MOVE "YOU DIED" TO DISPLAY-TEXT
                   PERFORM DISPLAY-INFO-SCREEN
                   GO TO GAME-OVER
               END-IF
               ADD 25 TO PL-HEALTH
           END-PERFORM.

       GAME-OVER.
           CLOSE MONSTERS.

           MOVE SPACES TO INPUT-LINE
           DISPLAY SCREEN-STATS
           ACCEPT SCREEN-STATS
           STOP RUN.

       GENERATE-MONSTER-KOBOLD.
           MOVE "KOBOLD" TO MON-NAME
           MOVE 100 TO MON-HEALTH
           MOVE "FIRE" TO MON-TYPE

           MOVE 65 TO RANGE-ATK-MAX
           MOVE 20 TO RANGE-ATK-MIN
           MOVE 55 TO RANGE-DEF-MAX
           MOVE 15 TO RANGE-DEF-MIN

           MOVE 55 TO RANGE-SPATK-MAX
           MOVE 15 TO RANGE-SPATK-MIN
           MOVE 55 TO RANGE-SPDEF-MAX
           MOVE 15 TO RANGE-SPDEF-MIN

           MOVE 99 TO RANGE-RATIO-ATK-MAX
           MOVE 75 TO RANGE-RATIO-ATK-MIN

           MOVE 60 TO RANGE-RATIO-SP-MAX
           MOVE 40 TO RANGE-RATIO-SP-MIN
           EXIT.

       GENERATE-MONSTER-ROCK.
           MOVE "ROCK" TO MON-NAME
           MOVE 200 TO MON-HEALTH
           MOVE "EARTH" TO MON-TYPE

           MOVE 0 TO RANGE-ATK-MAX
           MOVE 0 TO RANGE-ATK-MIN
           MOVE 0 TO RANGE-SPATK-MAX
           MOVE 0 TO RANGE-SPATK-MIN

           MOVE 80 TO RANGE-DEF-MAX
           MOVE 99 TO RANGE-DEF-MIN
           MOVE 80 TO RANGE-SPDEF-MAX
           MOVE 99 TO RANGE-SPDEF-MIN

           MOVE 0 TO RANGE-RATIO-ATK-MAX
           MOVE 0 TO RANGE-RATIO-ATK-MIN

           MOVE 0 TO RANGE-RATIO-SP-MAX
           MOVE 0 TO RANGE-RATIO-SP-MIN
           EXIT.

       GENERATE-MONSTER-SLIME.
           MOVE "SLIME" TO MON-NAME
           MOVE 75 TO MON-HEALTH
           MOVE "WATER" TO MON-TYPE

           MOVE 40 TO RANGE-ATK-MAX
           MOVE 10 TO RANGE-ATK-MIN
           MOVE 40 TO RANGE-DEF-MAX
           MOVE 10 TO RANGE-DEF-MIN

           MOVE 70 TO RANGE-SPATK-MAX
           MOVE 30 TO RANGE-SPATK-MIN
           MOVE 70 TO RANGE-SPDEF-MAX
           MOVE 30 TO RANGE-SPDEF-MIN

           MOVE 99 TO RANGE-RATIO-ATK-MAX
           MOVE 80 TO RANGE-RATIO-ATK-MIN

           MOVE 20 TO RANGE-RATIO-SP-MAX
           MOVE 00 TO RANGE-RATIO-SP-MIN
           EXIT.

       GENERATE-MONSTER-SNAKE.
           MOVE "SNAKE" TO MON-NAME
           MOVE 100 TO MON-HEALTH

           MOVE 65 TO RANGE-ATK-MAX
           MOVE 20 TO RANGE-ATK-MIN
           MOVE 55 TO RANGE-DEF-MAX
           MOVE 15 TO RANGE-DEF-MIN

           MOVE 55 TO RANGE-SPATK-MAX
           MOVE 15 TO RANGE-SPATK-MIN
           MOVE 55 TO RANGE-SPDEF-MAX
           MOVE 15 TO RANGE-SPDEF-MIN

           MOVE 99 TO RANGE-RATIO-ATK-MAX
           MOVE 75 TO RANGE-RATIO-ATK-MIN

           MOVE 60 TO RANGE-RATIO-SP-MAX
           MOVE 40 TO RANGE-RATIO-SP-MIN

           MOVE FUNCTION RANDOM TO TMP-NUM
           MULTIPLY 3 BY TMP-NUM
           ADD 1 TO TMP-NUM
           MOVE TMP-NUM TO TMP-UINT
           EVALUATE TMP-UINT
               WHEN 1 MOVE "WATER" TO MON-TYPE
               WHEN 2 MOVE "FIRE"  TO MON-TYPE
               WHEN 3 MOVE "EARTH" TO MON-TYPE
           END-EVALUATE
           EXIT.

       GENERATE-MONSTER.
           MOVE ZEROES TO GEN-STAT-RANGES.

           MOVE FUNCTION RANDOM TO TMP-NUM
           MULTIPLY 10 BY TMP-NUM GIVING TMP-DOT

           EVALUATE true
               WHEN TMP-NUM IS EQUAL TO ZERO
                   OR TMP-NUM IS EQUAL TO ONE
                   OR TMP-NUM IS EQUAL TO 4
                   PERFORM GENERATE-MONSTER-SLIME
               WHEN TMP-NUM IS EQUAL TO 2
                   PERFORM GENERATE-MONSTER-ROCK
               WHEN TMP-NUM IS EQUAL TO 5
                   OR TMP-NUM IS EQUAL TO 6
                   PERFORM GENERATE-MONSTER-SNAKE
               WHEN OTHER
                   PERFORM GENERATE-MONSTER-KOBOLD
           END-EVALUATE.

           MOVE RANGE-ATK-MAX TO RNG-MAX-VAL
           MOVE RANGE-ATK-MIN TO RNG-MIN-VAL
           PERFORM GEN-RNG-NUMBER
           MOVE TMP-NUM TO MON-ATTACK

           MOVE RANGE-DEF-MAX TO RNG-MAX-VAL
           MOVE RANGE-DEF-MIN TO RNG-MIN-VAL
           PERFORM GEN-RNG-NUMBER
           MOVE TMP-NUM TO MON-DEFENSE

           MOVE RANGE-SPATK-MAX TO RNG-MAX-VAL
           MOVE RANGE-SPATK-MIN TO RNG-MIN-VAL
           PERFORM GEN-RNG-NUMBER
           MOVE TMP-NUM TO MON-SPATTACK

           MOVE RANGE-SPDEF-MAX TO RNG-MAX-VAL
           MOVE RANGE-SPDEF-MIN TO RNG-MIN-VAL
           PERFORM GEN-RNG-NUMBER
           MOVE TMP-NUM TO MON-SPDEFENSE

           MOVE RANGE-RATIO-ATK-MAX TO RNG-MAX-VAL
           MOVE RANGE-RATIO-ATK-MIN TO RNG-MIN-VAL
           PERFORM GEN-RNG-NUMBER
           MOVE TMP-NUM TO MON-ATK-RATIO

           MOVE RANGE-RATIO-SP-MAX TO RNG-MAX-VAL
           MOVE RANGE-RATIO-SP-MIN TO RNG-MIN-VAL
           PERFORM GEN-RNG-NUMBER
           MOVE TMP-NUM TO MON-SP-RATIO

           ADD ONE TO ST-MONSTERS

           MOVE MON-TYPE TO MF-TYPE
           MOVE MON-NAME TO MF-NAME
           MOVE MON-ATTACK TO MF-ATTACK
           MOVE MON-SPATTACK TO MF-SPATTACK
           MOVE MON-DEFENSE TO MF-DEFENSE
           MOVE MON-SPDEFENSE TO MF-SPDEFENSE.

           MOVE "TYPE: "       TO FILLER-01
           MOVE " NAME: "      TO FILLER-02
           MOVE " ATK: "       TO FILLER-03
           MOVE " SP-ATK: "    TO FILLER-04
           MOVE " DEF: "       TO FILLER-05
           MOVE " SP-DEF: "    TO FILLER-06
           WRITE MONSTER-FILE
           BEFORE ADVANCING ONE LINE
           END-WRITE.

           MOVE "A NEW MONSTER APPROACHES" TO DISPLAY-TEXT
           PERFORM DISPLAY-INFO-SCREEN
           EXIT.


       GEN-RNG-NUMBER.
           MOVE ZERO TO TMP-NUM
           PERFORM UNTIL TMP-NUM IS LESS THAN OR EQUAL TO RNG-MAX-VAL
               AND TMP-NUM IS GREATER THAN OR EQUAL TO RNG-MIN-VAL

               MOVE FUNCTION RANDOM TO TMP-NUM
               MULTIPLY 100 BY TMP-NUM
           END-PERFORM.
           EXIT.

       REPL-LOOP.
           PERFORM UNTIL PL-HEALTH IS LESS THAN OR EQUAL TO ZERO
               OR MON-HEALTH IS LESS THAN OR EQUAL TO ZERO

               MOVE ONE TO DO-MONSTER-ATTACK
               MOVE ZERO TO MON-DEFEND
               MOVE "Y" TO IS-INPUT-OK

               MOVE FUNCTION RANDOM TO TMP-NUM
               MULTIPLY 100 BY TMP-NUM

      *        defend
               IF MON-ATK-RATIO IS LESS THAN OR EQUAL TO TMP-NUM
                   MOVE ONE TO MON-DEFEND
               ELSE
                   MOVE ZERO TO MON-DEFEND
               END-IF

               PERFORM FILL-SCREEN-BATTLE
               DISPLAY SCREEN-BATTLE
               ACCEPT SCREEN-BATTLE

               MOVE INPUT-LINE TO UPPER-TEXT
               PERFORM TO-UPPER
               MOVE UPPER-TEXT TO INPUT-LINE

               EVALUATE TRUE
                   WHEN INPUT-EXIT
                       GO TO RUN-AWAY

                   WHEN INPUT-ATTACK
                       ADD ONE TO ST-AT-USED
                       MOVE MON-DEFENSE TO TMP-DEF
                       MOVE PL-ATTACK TO TMP-ATK
                       PERFORM CALCULATE-DAMAGE

      *                health - total attack value
                       SUBTRACT TMP-NUM
                           FROM MON-HEALTH
                           GIVING MON-HEALTH
                       IF MON-DEFEND IS EQUAL TO ONE
                           MULTIPLY DEFEND-RATIO BY TMP-NUM
                           GIVING TMP-NUM
                           STRING
                               "THE " DELIMITED BY SIZE
                               MON-NAME DELIMITED BY SPACES
                               " BRACED FOR ATTACK" delimited by size
                           INTO DISPLAY-TEXT
                           PERFORM DISPLAY-INFO-SCREEN

                           ADD TMP-NUM TO ST-DMG-DEALT
                           MOVE TMP-NUM TO TMP-DOT

                           STRING
                               "THE " DELIMITED BY SIZE
                               MON-NAME DELIMITED BY SPACES
                               " DEFENDED AND YOU ATTACKED FOR "
                                   DELIMITED BY SIZE
                               TMP-DOT DELIMITED BY SIZE
                               " DAMAGE" DELIMITED BY SIZE
                               INTO DISPLAY-TEXT
                       ELSE
                           ADD TMP-NUM TO ST-DMG-DEALT
                           MOVE TMP-NUM TO TMP-DOT

                           STRING
                               "YOU ATTACKED FOR " DELIMITED BY SIZE
                               TMP-DOT DELIMITED BY SIZE
                               " DAMAGE" DELIMITED BY SIZE
                               INTO DISPLAY-TEXT
                       END-IF
                       PERFORM DISPLAY-INFO-SCREEN

                   WHEN INPUT-SPFIRE OR INPUT-SPWATER OR INPUT-SPEARTH
                       ADD ONE TO ST-SP-USED

                       MOVE MON-SPDEFENSE TO TMP-DEF
                       MOVE PL-SPATTACK TO TMP-ATK
                       PERFORM CALCULATE-DAMAGE

      *                Standard dmg
                       MOVE SPACES TO TMP-ATK-TYPE
                       EVALUATE TRUE
                           WHEN INPUT-SPWATER
                               MOVE "WATER" TO TMP-ATK-TYPE
                           WHEN INPUT-SPFIRE
                               MOVE "FIRE"  TO TMP-ATK-TYPE
                           WHEN INPUT-SPEARTH
                               MOVE "EARTH" TO TMP-ATK-TYPE
                       END-EVALUATE

                       MOVE MON-TYPE TO TMP-DEF-TYPE
                       PERFORM CALCULATE-SP-DAMAGE

                       IF MON-DEFEND IS EQUAL TO ONE
                           MULTIPLY DEFEND-RATIO BY TMP-NUM
                           GIVING TMP-NUM
                           STRING
                               "THE " DELIMITED BY SIZE
                               MON-NAME DELIMITED BY SPACES
                               " BRACED FOR ATTACK" DELIMITED BY SIZE
                               INTO DISPLAY-TEXT
                           PERFORM DISPLAY-INFO-SCREEN

                           ADD TMP-NUM TO ST-DMG-DEALT
                           MOVE TMP-NUM TO TMP-DOT

                           STRING
                               "THE " delimited by size
                               MON-NAME DELIMITED BY SPACES
                               " DEFENDED AND YOU ATTACKED FOR "
                                   DELIMITED BY SIZE
                               TMP-DOT DELIMITED BY SIZE
                               " DAMAGE" DELIMITED BY SIZE
                               INTO DISPLAY-TEXT
                       ELSE
                           ADD TMP-NUM TO ST-DMG-DEALT
                           MOVE TMP-NUM TO TMP-DOT

                           STRING
                               "YOU ATTACKED FOR " DELIMITED BY SIZE
                               TMP-DOT DELIMITED BY SIZE
                               " DAMAGE" DELIMITED BY SIZE
                               INTO DISPLAY-TEXT
                       END-IF

                       SUBTRACT TMP-NUM FROM MON-HEALTH
                       PERFORM DISPLAY-INFO-SCREEN

                   WHEN INPUT-DEFEND
                       MOVE "YOU BRACE FOR IMPACT" TO DISPLAY-TEXT
                       PERFORM DISPLAY-INFO-SCREEN
                       MOVE ONE TO PL-DEFEND
                   WHEN OTHER
                       MOVE "N" TO IS-INPUT-OK
               END-EVALUATE
               MOVE SPACES TO INPUT-LINE

               IF DO-MONSTER-ATTACK EQUAL ONE
                   AND MON-HEALTH IS GREATER THAN ZERO
                   AND INPUT-GOOD

                   MOVE FUNCTION RANDOM TO TMP-NUM
                   MULTIPLY 100 BY TMP-NUM
                   IF MON-DEFEND IS NOT EQUAL TO ONE

                       IF MON-SP-RATIO IS LESS THAN OR EQUAL TO TMP-NUM
                           MOVE PL-DEFENSE TO TMP-DEF
                           MOVE MON-ATTACK TO TMP-ATK
                           PERFORM CALCULATE-DAMAGE

                           ADD TMP-NUM TO ST-DMG-RECEIVED
                           MOVE TMP-NUM TO TMP-DOT
                           STRING
                               MON-NAME DELIMITED BY SPACES
                               " ATTACKS FOR " DELIMITED BY SIZE
                               TMP-DOT DELIMITED BY SIZE
                               " DAMAGE" DELIMITED BY SIZE
                               INTO DISPLAY-TEXT

                       ELSE
                           MOVE PL-SPDEFENSE TO TMP-DEF
                           MOVE MON-SPATTACK TO TMP-ATK
                           MOVE SPACES TO TMP-ATK-TYPE
                           EVALUATE TRUE
                               WHEN MT-WATER
                                   MOVE "WATER" TO TMP-ATK-TYPE
                               WHEN MT-FIRE
                                   MOVE "FIRE"  TO TMP-ATK-TYPE
                               WHEN MT-EARTH
                                   MOVE "EARTH" TO TMP-ATK-TYPE
                           END-EVALUATE

                           MOVE PL-TYPE TO TMP-DEF-TYPE
                           PERFORM CALCULATE-SP-DAMAGE

                           IF PL-DEFEND IS EQUAL ONE
                               MULTIPLY DEFEND-RATIO BY TMP-NUM
                           END-IF

                           ADD TMP-NUM TO ST-DMG-RECEIVED
                           MOVE TMP-NUM TO TMP-DOT

                           STRING
                               "THE " DELIMITED BY SIZE
                               MON-NAME DELIMITED BY SPACES
                               " ATTACKED WITH "
                               DELIMITED BY SIZE
                               MON-TYPE DELIMITED BY SPACES
                               " MAGIC AND DEALS " DELIMITED BY SIZE
                               TMP-DOT DELIMITED BY SIZE
                               " DAMAGE" DELIMITED BY SIZE
                               INTO DISPLAY-TEXT
                       END-IF

                       SUBTRACT TMP-NUM
                           FROM PL-HEALTH
                           GIVING PL-HEALTH
                       MOVE TMP-NUM TO TMP-DOT
                       PERFORM DISPLAY-INFO-SCREEN
                   END-IF
               END-IF
           END-PERFORM.
           EXIT.

      * TMP-DEF is recipient's defence
      * TMP-ATK is agressor's attack
      * TMP-NUM is the value to subtract from health (output)
       CALCULATE-DAMAGE.
      * attack value = player attack - (((100 - monster defense)
      * / 100) * player attack)
      *
      *    100 - monster defense
           SUBTRACT TMP-DEF FROM 100
               GIVING TMP-NUM

      *    (100 - monster defese) / 100
      *    persentage of player attack to deal
           DIVIDE TMP-NUM BY 100
               GIVING TMP-NUM

      *    player attack * persentage = total attack value
           MULTIPLY TMP-NUM BY TMP-ATK GIVING TMP-NUM
           EXIT.

      * TMP-ATK-TYPE attacker's type (input)
      * TMP-DEF-TYPE defender's type (input)
      * TMP-NUM damage value (input and output)
       CALCULATE-SP-DAMAGE.
           EVALUATE TRUE
               WHEN TD-WATER
                   IF TA-FIRE
                       DIVIDE TMP-NUM BY 2 GIVING
                       TMP-NUM
                   ELSE IF TA-EARTH
                       MULTIPLY 2 BY TMP-NUM
                       GIVING TMP-NUM
                   END-IF

               WHEN TD-FIRE
                   IF TA-EARTH
                       DIVIDE TMP-NUM BY 2 GIVING
                       TMP-NUM
                   ELSE IF TA-WATER
                       MULTIPLY 2 BY TMP-NUM
                       GIVING TMP-NUM
                   END-IF

               WHEN TD-EARTH
                   IF TA-WATER
                       DIVIDE TMP-NUM BY 2 GIVING
                       TMP-NUM
                   ELSE IF TA-FIRE
                       MULTIPLY 2 BY TMP-NUM
                       GIVING TMP-NUM
                   END-IF
           END-EVALUATE.
           EXIT.

       FILL-SCREEN-BATTLE.
           MOVE MON-NAME       TO DM-NAME
           MOVE MON-TYPE       TO DM-TYPE
           MOVE MON-TYPE       TO DM-TYPE
           MOVE MON-HEALTH     TO DM-HEALTH
           MOVE MON-ATTACK     TO DM-ATTACK
           MOVE MON-DEFENSE    TO DM-DEFENSE
           MOVE MON-SPATTACK   TO DM-SPATTACK
           MOVE MON-SPDEFENSE  TO DM-SPDEFENSE
           MOVE MON-ATK-RATIO  TO DM-RATIO

           MOVE SPACES TO INPUT-LINE.
           EXIT.

       DISPLAY-INFO-SCREEN.
           MOVE SPACES TO INPUT-LINE
           DISPLAY SCREEN-INFO
           ACCEPT SCREEN-INFO
           MOVE SPACES TO INPUT-LINE
           MOVE SPACES TO DISPLAY-TEXT
           EXIT.

      * FUNCTION UPPER-CASE(UPPER-TEXT) IS FOR THE WEAK
       TO-UPPER.
           INSPECT UPPER-TEXT REPLACING ALL "a" BY "A"
           INSPECT UPPER-TEXT REPLACING ALL "b" BY "B"
           INSPECT UPPER-TEXT REPLACING ALL "c" BY "C"
           INSPECT UPPER-TEXT REPLACING ALL "d" BY "D"
           INSPECT UPPER-TEXT REPLACING ALL "e" BY "E"
           INSPECT UPPER-TEXT REPLACING ALL "f" BY "F"
           INSPECT UPPER-TEXT REPLACING ALL "g" BY "G"
           INSPECT UPPER-TEXT REPLACING ALL "h" BY "H"
           INSPECT UPPER-TEXT REPLACING ALL "i" BY "I"
           INSPECT UPPER-TEXT REPLACING ALL "j" BY "J"
           INSPECT UPPER-TEXT REPLACING ALL "k" BY "K"
           INSPECT UPPER-TEXT REPLACING ALL "l" BY "L"
           INSPECT UPPER-TEXT REPLACING ALL "m" BY "M"
           INSPECT UPPER-TEXT REPLACING ALL "n" BY "N"
           INSPECT UPPER-TEXT REPLACING ALL "o" BY "O"
           INSPECT UPPER-TEXT REPLACING ALL "p" BY "P"
           INSPECT UPPER-TEXT REPLACING ALL "q" BY "Q"
           INSPECT UPPER-TEXT REPLACING ALL "r" BY "R"
           INSPECT UPPER-TEXT REPLACING ALL "s" BY "S"
           INSPECT UPPER-TEXT REPLACING ALL "t" BY "T"
           INSPECT UPPER-TEXT REPLACING ALL "u" BY "U"
           INSPECT UPPER-TEXT REPLACING ALL "v" BY "V"
           INSPECT UPPER-TEXT REPLACING ALL "w" BY "W"
           INSPECT UPPER-TEXT REPLACING ALL "x" BY "X"
           INSPECT UPPER-TEXT REPLACING ALL "y" BY "Y"
           INSPECT UPPER-TEXT REPLACING ALL "z" BY "Z"
           EXIT.

       RUN-AWAY.
           MOVE "YOU TRIED TO RUN AWAY, BUT YOU TRIPPED AND DIED."
               TO DISPLAY-TEXT.
           PERFORM DISPLAY-INFO-SCREEN
           GO TO GAME-OVER.

