       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPG.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77 ONE pic 9 value 1.

       01 INPUT-LINE PIC X(100).
      * Current monster ID in MONSTERS
       01 MONSTER-ID PIC 9(5) VALUE 1.
       01 TMP-NUM PIC s99v99.
       01 DO-MONSTER-ATTACK PIC 9 VALUE 0.

       01 THE-PLAYER.
           05 PL-HEALTH    PIC S9(3) VALUE 100.
           05 PL-ATTACK    PIC 9(2) VALUE 0.
           05 PL-DEFENSE   PIC 9(2) VALUE 0.

       01 MONSTERS OCCURS 100 TIMES.
           05 MON-HEALTH   PIC S9(3) VALUE 100.
           05 MON-ATTACK   PIC 9(2) VALUE 0.
           05 MON-DEFENSE  PIC 9(2) VALUE 0.

       01 DISP-MONSTER.
           05 FILLER       PIC X(3) VALUE "ID ".
           05 DM-ID        PIC 9(5).
           05 FILLER       PIC X(4) VALUE " HP ".
           05 DM-HEALTH    PIC 9(3) VALUE 100.
           05 FILLER       PIC X(5) VALUE " ATT ".
           05 DM-ATTACK    PIC 9(2) VALUE 0.
           05 FILLER       PIC X(5) VALUE " DEF ".
           05 DM-DEFENSE   PIC 9(2) VALUE 0.

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

       PROCEDURE DIVISION.

       MAIN.
      *     PERFORM UNTIL PL-ATTACK > 20 AND PL-ATTACK < 80
      *         DISPLAY "ENTER ATTACK"
      *         ACCEPT PL-ATTACK
      *     END-PERFORM

      *     PERFORM UNTIL PL-DEFENSE > 20 AND PL-DEFENSE < 80
      *         DISPLAY "ENTER DEFENSE"
      *         ACCEPT PL-DEFENSE
      *     END-PERFORM

           MOVE 50 TO PL-ATTACK
           MOVE 50 TO PL-DEFENSE

           DISPLAY "ENTERING ARENA"

      * generate a monster with stats
      * REPL the attacks
      * when player health is zero, you die and game is over
      * when monster health is zero, it dies, spawn new monster
           PERFORM UNTIL ONE EQUAL ZERO
               PERFORM GENERATE-MONSTER
               PERFORM REPL-LOOP

               IF PL-HEALTH < 0
                   DISPLAY "YOU DIED."
                   GO TO GAME-OVER
               end-if
               ADD 1 TO MONSTER-ID
               display " "
           END-PERFORM.

       THE-END.
           DISPLAY "GOODBYE"
           STOP RUN.

       GAME-OVER.
           DISPLAY "TODO: DISPLAY STATS"
           STOP RUN.

       GENERATE-MONSTER.
           MOVE 100 TO MON-HEALTH(MONSTER-ID)

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           MOVE FUNCTION RANDOM(WS-CURRENT-MILLISECONDS) TO TMP-NUM

           MULTIPLY 100 BY TMP-NUM
           MOVE TMP-NUM TO MON-ATTACK(MONSTER-ID)

           MOVE FUNCTION RANDOM TO TMP-NUM
           MULTIPLY 100 BY TMP-NUM
           MOVE TMP-NUM TO MON-DEFENSE(MONSTER-ID)

           DISPLAY "A NEW MONSTER APPROACHES"
           MOVE MONSTER-ID TO DM-ID
           MOVE MON-HEALTH(MONSTER-ID) TO DM-HEALTH
           MOVE MON-ATTACK(MONSTER-ID) TO DM-ATTACK
           MOVE MON-DEFENSE(MONSTER-ID) TO DM-DEFENSE
           DISPLAY DISP-MONSTER

           EXIT.

       REPL-LOOP.
           PERFORM UNTIL PL-HEALTH IS LESS THAN OR EQUAL TO ZERO
               OR MON-HEALTH(MONSTER-ID) IS LESS THAN OR EQUAL TO ZERO

               MOVE 1 TO DO-MONSTER-ATTACK

      * attack value = player attack - ((monster defense / 100) * player
      * attack)
               ACCEPT INPUT-LINE
               MOVE FUNCTION UPPER-CASE(INPUT-LINE) TO INPUT-LINE
               EVALUATE INPUT-LINE
                   WHEN "EXIT"
                       GO TO RUN-AWAY
                   WHEN "ATTACK"
                       DIVIDE MON-DEFENSE(MONSTER-ID) BY 100
                           GIVING TMP-NUM
                       MULTIPLY TMP-NUM BY PL-ATTACK GIVING TMP-NUM
                       display "tmp-num " tmp-num
                       SUBTRACT TMP-NUM
                           FROM PL-ATTACK
                           GIVING TMP-NUM
      *                 IF TMP-NUM < 0
      *                     MOVE 1 TO TMP-NUM
      *                 END-IF
                       SUBTRACT TMP-NUM
                           FROM MON-HEALTH(MONSTER-ID)
                           GIVING MON-HEALTH(MONSTER-ID)
                       DISPLAY "YOU ATTACKED THE MONSTER FOR "
                           TMP-NUM " DAMAGE"
                   WHEN "SHOW"
                       DISPLAY "MONSTER HP: " MON-HEALTH(MONSTER-ID)
                       MOVE 0 TO DO-MONSTER-ATTACK
               END-EVALUATE

               IF DO-MONSTER-ATTACK EQUAL 1
                   AND MON-HEALTH(MONSTER-ID) > 0

      * attack value = mon attack - ((player defense / 100) * mon
      * attack)

                   DIVIDE PL-ATTACK BY 100 GIVING TMP-NUM
                   MULTIPLY TMP-NUM BY MON-ATTACK(MONSTER-ID)
                       GIVING TMP-NUM
                   SUBTRACT TMP-NUM
                       FROM PL-DEFENSE
                       GIVING TMP-NUM
                   SUBTRACT TMP-NUM
                       FROM PL-HEALTH
                       GIVING PL-HEALTH
                   DISPLAY "MONSTER ATTACKS FOR " TMP-NUM
                       " DAMAGE"
               END-IF

               DISPLAY "PLAYER HEALTH: " PL-HEALTH
           END-PERFORM.
           EXIT.

       RUN-AWAY.
           DISPLAY "YOU TRIED TO RUN AWAY, BUT YOU TRIPPED AND DIED."
           EXIT PROGRAM.

