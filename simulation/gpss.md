# GPSS Blocks

GENERATE A(average time), B(Range +-), C(time first transaction), D(maximum number of created transactions), E(priority)
TERMINATE A(number fo decrement)

ADVANCE A(average waiting time), B(Range)

SEIZE ID
RELEASE ID

STORAGE S(ID), #SERVERS
ENTER ID, #SERVERS
LEAVE ID, #SERVERS

QUEUE  ID, #ELEMENTS (default = 1)
DEPART ID, #ELEMENTS (default = 1)

ASSIGN A(parameter's number), B(value to assign), C(kind of parameters: PH, PF, PL, PB)          ; STATIC VARIABLES

TEST X(relation operator: E,NE,G,GE,L,LE),A(verification operator),B(reference value),C (number of the destination block)
     If C is not defined, when the condition is false, blocks until true.
     If C is defined, when the condition is false, jumps to C
     Example: TEST LE C1,240,FIN


-- Breaks sequential movement
TRANSFER 0.40,OPC1, OPC2
TRANSFER BOTH,SEC1, SEC1 ;picks the free one
TRANSFER ALL,EJE1,EJE3,4
[...]


FUNCTION A(function arguments),B(type of the function: C,D,E,L,M)
X1,Y1/X2,Y2/../Xn,Yn

Example:   ID     FUNCTION     RN1, D5
           .4,1/.7,2/.85 ....
                  ....
                  ADVANCE      FN$TRAB

LOGIC X(Logic Operator: Set,Reset,Invert), ID
GATE  A           B,C
      UN, NU (try if the installation is full, free)
      SF, SNF, SE, SNE (try if server is full,not full, empty, not empty)
      LS, LR (Set logic, reset logic)

SAVEVALUE ID, VALUE, TYPE (XH,XF,XL,XB: half word, full word, floating, byte)
X$ID     ; access to the value

MATRIX TYPE,ROWS,COLUMNS              ; MAGATZEM      MATRIX    MH,200,4
MSAVEVALUE NAME,ROW,COLUMN,INFO       ; modify value of a matrix

INITIAL    LSMMyLogic, 1
INITIAL    XH(MySavevalue),10
INITIAL    XF(1),10
INITIAL    XL(1),10
INITIAL    XB(1),10
INITIAL    MX$nom(1,2),5

Ampervariables ???

SPLIT  #N, DEST, C      ; Replicate n-times the current transaction

LOOP   #N, DEST         ; An imperative loop

FUNAVAIL     NAME     ;Installation not available
FAVAIL       NAME     ;Installation available

SUNAVAIL     NAME     ;Storage not available
SAVAIL       NAME     ;Storage is available

TRACE    ;DEBUG
UNTRACE    ;DEBUG

ASSEMBLE #N                      ; Synchronize transactions
GATHER   #N                      ; Synchronize transactions
MATCH    A(the other block match ; Synchronize transactions

PRIORITY NEW_VALUE, BUFFER_OPTA      ; priority over the active transaction

BUFFER                               ; allows to reanalyze the CEC

PREEMPT INSTAL., PRIORITY,...        ; Displaces the transaction that owns the installation
                                           allowing that the new transaction takes it

RETURN INSTAL.     ; Free an installation that was been captured by a transaction

RESET     ;reset the current clock
CLEAR     ;deletes all the transactions, clocks to 0, all servers are set to free.

OUT1       FILEDEF        'Sortida.txt'
GETLIST FILE=nom,END=A,ERR=B,(&var1,..)    ; File=OUT1
BPUTPIC FILE=nom,LINES=2 ;print the results

RVEXPO(i,IIAT)  ; Exponential distirubtion
RVNORM(i,u,r)   ; Normal distribution ABS(RVNORM(i,u,r)

### Examples

      STORAGE     S(CAIXES),3
      GENERATE    7.5,2.5
      TEST LE     C1,240,FIN

ENT   QUEUE       FILA
      ENTER       CAIXES
      DEPART      FILA
      ADVANCE     3.5,1.5
SORT  LEAVE       CAIXES
FIN   TERMINATE

      GENERATE    240
      TEST E      N$ENT, N$SORT ;waits till the last client has left the bank
      TERMINATE   1

      ; START      1

-----------------------------------

      GENERATE    3,2
      GATE NU     TELEF, NEXT
      SEIZE       TELEF
      ADVANCE     6.5,1.5
      RELEASE     TELEF
NEXT  TERMINATE

      GENERATE    480
      TERMINATE   1

      ;START       1

-----------------------------------

          GENEREATE        ,,,5
CICLE     ASSIGN           CARB,50
          QUEUE            INI
          SEIZE            CARG
MAS       ADVANCE          6,1
          LOOP             CARB,MAS
          RELEASE          CARG
          DEPART           INI
          ADVANCE          360,120
          ASSIGN           CARB,50
          QUEUE            FIN
          SEIZE            DESC
MEN       ADVANCE          13,3
          LOOP             DESC
          RELEASE          DESC
          DEPART           FIN
          ADVANCE          300,120
          TRANSFER         ,CICLE

          GENERATE         86400
          TERMINATE        1

          ;START           1

---------------------------------------

          STORAGE          S(MAQ),2

          GNEERATE         10,4
          QUEUE            INV
          ENTER            MAQ
          DEPART           INV
          ADVANCE          15,5
          LEAVE            MAQ
          TERMINATE        1

          GENERATE         90
          SUNAVAIL         MAQ   ;Disable storage MAQ
          ADVANCE          15,3
          SAVAIL           MAQ    ;Able storage MAQ
          TERMINATE

          ;START           3000

---------------------------------------

          GENERATE         6
          QUEUE            ALM
          SEIZE            OPER
          DEPART           ALM
          SPLIT            1,MED    ;start calibration
          ADVANCE          3
MED1      MATCH            MED2     ;wait for calibration
          ADVANCE          2
          RELEASE          OPER
          TERMINATE        1

MED       ADVANCE          5,3
MED2      MATCH            MED1    ;we need to wait for MED1 to be finished before finishing
          TERMINATE

          ;START           200

------------------------------------------

          GENERATE         1
          ADVANCE          5,2
          ADVANCE          RVEXPO(3,0.45)
          ADVANCE          RVEXPO(3,0.45)+RVEXPO(3,0.45)
          ADVANCE          ABS(RVNORML(1,5,3))
          ADVANCE          RVTRI(1,2,3,5)
          TERMINATE        1
