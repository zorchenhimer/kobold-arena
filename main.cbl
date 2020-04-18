       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPG.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77 ONE pic 9 value 1.
       77 IS-INPUT-OK PIC 9.
                   88 INPUT-GOOD VALUE "Y".
                   88 INPUT-BAD  VALUE "N".

       01 INPUT-LINE PIC X(100).
                   88 INPUT-ATTACK     VALUE "ATTACK" "A".
                   88 INPUT-EXIT       VALUE "EXIT" "X".
                   88 INPUT-SHOW       VALUE "SHOW".
                   88 INPUT-SPWATER    VALUE "SPWATER".
                   88 INPUT-SPFIRE     VALUE "SPFIRE".
                   88 INPUT-SPEARTH    VALUE "SPEARTH".

      * Current monster ID in MONSTERS
       01 MONSTER-ID PIC 9(5) VALUE 1.
       01 TMP-NUM  PIC s99v99.
       01 TMP-DEF  PIC s99v99.
       01 TMP-ATK  PIC s99v99.
       01 TMP-UINT PIC 9(4).

       01 TMP-ATK-TYPE PIC X(5).
                   88 TA-WATER     VALUE "WATER".
                   88 TA-FIRE      VALUE "FIRE".
                   88 TA-EARTH     VALUE "EARTH".
       01 TMP-DEF-TYPE PIC X(5).
                   88 TD-WATER     VALUE "WATER".
                   88 TD-FIRE      VALUE "FIRE".
                   88 TD-EARTH     VALUE "EARTH".

       01 DO-MONSTER-ATTACK PIC 9 VALUE 0.

       01 THE-PLAYER.
           05 PL-HEALTH    PIC S9(3)   VALUE 100.
           05 PL-ATTACK    PIC 9(2)    VALUE 0.
           05 PL-SPATTACK  PIC 9(2)    VALUE 0.
           05 PL-DEFENSE   PIC 9(2)    VALUE 0.
           05 PL-SPDEFENSE PIC 9(2)    VALUE 0.
           05 PL-TYPE      PIC X(5).

       01 MONSTERS OCCURS 100 TIMES.
           05 MON-TYPE     PIC X(5).
                   88 MT-WATER     VALUE "WATER".
                   88 MT-FIRE      VALUE "FIRE".
                   88 MT-EARTH     VALUE "EARTH".

           05 MON-HEALTH       PIC S9(3)   VALUE 100.
           05 MON-ATTACK       PIC 9(2)    VALUE 0.
           05 MON-SPATTACK     PIC 9(2)    VALUE 0.
           05 MON-DEFENSE      PIC 9(2)    VALUE 0.
           05 MON-SPDEFENSE    PIC 9(2)    VALUE 0.

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
           05 DM-TYPE      PIC X(5) .

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

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           MOVE FUNCTION RANDOM(WS-CURRENT-MILLISECONDS) TO TMP-NUM
      *     PERFORM UNTIL PL-ATTACK > 20 AND PL-ATTACK < 80
      *         DISPLAY "ENTER ATTACK"
      *         ACCEPT PL-ATTACK
      *     END-PERFORM

      *     PERFORM UNTIL PL-DEFENSE > 20 AND PL-DEFENSE < 80
      *         DISPLAY "ENTER DEFENSE"
      *         ACCEPT PL-DEFENSE
      *     END-PERFORM

      *    TODO: get these from the user
           MOVE 50 TO PL-ATTACK
           MOVE 50 TO PL-DEFENSE
           MOVE 50 TO PL-SPATTACK
           MOVE 50 TO PL-SPDEFENSE
           MOVE "FIRE" TO PL-TYPE

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

           PERFORM GEN-RNG-NUMBER
           MOVE TMP-NUM TO MON-ATTACK(MONSTER-ID)

           PERFORM GEN-RNG-NUMBER
           MOVE TMP-NUM TO MON-DEFENSE(MONSTER-ID)

           PERFORM GEN-RNG-NUMBER
           MOVE TMP-NUM TO MON-SPATTACK(MONSTER-ID)

           PERFORM GEN-RNG-NUMBER
           MOVE TMP-NUM TO MON-SPDEFENSE(MONSTER-ID)

           MOVE FUNCTION RANDOM TO TMP-NUM
           MULTIPLY 3 BY TMP-NUM
           ADD 1 TO TMP-NUM
           MOVE TMP-NUM TO TMP-UINT
           EVALUATE TMP-UINT
               WHEN 1 MOVE "WATER" TO MON-TYPE(MONSTER-ID)
               WHEN 2 MOVE "FIRE"  TO MON-TYPE(MONSTER-ID)
               WHEN 3 MOVE "EARTH" TO MON-TYPE(MONSTER-ID)
           END-EVALUATE

           DISPLAY "A NEW MONSTER APPROACHES"
           MOVE MONSTER-ID TO DM-ID
           MOVE MON-HEALTH(MONSTER-ID) TO DM-HEALTH
           MOVE MON-ATTACK(MONSTER-ID) TO DM-ATTACK
           MOVE MON-DEFENSE(MONSTER-ID) TO DM-DEFENSE
           MOVE MON-SPATTACK(MONSTER-ID) TO DM-SPATTACK
           MOVE MON-SPDEFENSE(MONSTER-ID) TO DM-SPDEFENSE
           MOVE MON-TYPE(MONSTER-ID) TO DM-TYPE
           DISPLAY DISP-MONSTER

           EXIT.

       GEN-RNG-NUMBER.
           MOVE ZERO TO TMP-NUM
           PERFORM UNTIL TMP-NUM IS LESS THAN 70
               AND TMP-NUM IS GREATER THAN 15

               MOVE FUNCTION RANDOM TO TMP-NUM
               MULTIPLY 100 BY TMP-NUM
           END-PERFORM.
           EXIT.

       REPL-LOOP.
           PERFORM UNTIL PL-HEALTH IS LESS THAN OR EQUAL TO ZERO
               OR MON-HEALTH(MONSTER-ID) IS LESS THAN OR EQUAL TO ZERO

               MOVE 1 TO DO-MONSTER-ATTACK
               MOVE "Y" TO IS-INPUT-OK

               ACCEPT INPUT-LINE
               DISPLAY " "
               MOVE FUNCTION UPPER-CASE(INPUT-LINE) TO INPUT-LINE
               EVALUATE TRUE
                   WHEN INPUT-EXIT
                       GO TO RUN-AWAY

                   WHEN INPUT-ATTACK
                       MOVE MON-DEFENSE(MONSTER-ID) TO TMP-DEF
                       MOVE PL-ATTACK TO TMP-ATK
                       PERFORM CALCULATE-DAMAGE
      *                health - total attack value
                       SUBTRACT TMP-NUM
                           FROM MON-HEALTH(MONSTER-ID)
                           GIVING MON-HEALTH(MONSTER-ID)
                       DISPLAY "YOU ATTACKED THE MONSTER FOR "
                           TMP-NUM " DAMAGE"

                   WHEN INPUT-SHOW
                       DISPLAY "MONSTER HP: " MON-HEALTH(MONSTER-ID)
                       MOVE 0 TO DO-MONSTER-ATTACK

                   WHEN INPUT-SPFIRE OR INPUT-SPWATER OR INPUT-SPEARTH
                       MOVE MON-SPDEFENSE(MONSTER-ID) TO TMP-DEF
                       MOVE PL-SPATTACK TO TMP-ATK
                       PERFORM CALCULATE-DAMAGE

                       DISPLAY "PRE SP TMP-NUM " TMP-NUM
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

                       MOVE MON-TYPE(MONSTER-ID) TO TMP-DEF-TYPE
                       PERFORM CALCULATE-SP-DAMAGE

                       SUBTRACT TMP-NUM FROM MON-HEALTH(MONSTER-ID)
                       DISPLAY "YOU ATTACKED THE MONSTER FOR "
                           TMP-NUM " DAMAGE"

                   WHEN OTHER
                       DISPLAY "TRY AGAIN"
                       MOVE "N" TO IS-INPUT-OK
               END-EVALUATE

               IF DO-MONSTER-ATTACK EQUAL 1
                   AND MON-HEALTH(MONSTER-ID) > 0
                   AND INPUT-GOOD

                   MOVE PL-DEFENSE TO TMP-DEF
                   MOVE MON-ATTACK(MONSTER-ID) TO TMP-ATK
                   PERFORM CALCULATE-DAMAGE

                   SUBTRACT TMP-NUM
                       FROM PL-HEALTH
                       GIVING PL-HEALTH
                   DISPLAY "MONSTER ATTACKS FOR " TMP-NUM
                       " DAMAGE"
               END-IF

               DISPLAY "PLAYER HEALTH: " PL-HEALTH
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
                       DISPLAY "WATER VS FIRE"
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

       RUN-AWAY.
           DISPLAY "YOU TRIED TO RUN AWAY, BUT YOU TRIPPED AND DIED."
           EXIT PROGRAM.

