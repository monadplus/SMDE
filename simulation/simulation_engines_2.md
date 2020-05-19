# Process Interaction

Process Interaction: method to create a simulation engine.

Two typlogies for the processes:

- P1: need a server to complete the different actions
- P2: do not need a server but need some time to complete the actions.


P1 chronogram: P1a and P1b share the same server to P1b can't start until P1a has finished. P2 can start because it does not require a server.

P1 Event list (two lists):

- Current Event Chain (CEC): activities that must be processed in this actual time, sorted by priority.
- Future Event Chain (FEC): activities that must be processed in the future, sorted by time.


Algorithm:

1. Initialization of hte model
2. More activities in the CEC ?
    - No (list empty or all activities are blocked in front of a server): go to 5.
3. Move the activites all that we can in its processes (entity leave the model, finds a delay, dins a server that is being used).
    - In case of delay (ADVANCE), the entity must be send to FEC.
4. Go to 2
5. Move the activites from FEC to CEC with smallest time.
6. Update clock with the time from 5.
7. End of simulation ?
    - No, Go to 2.
8. End, write simulation report


Example:

- Interval between generations: (2,2,4,4)
- We only generate 4 entities.

1.        GENERATE     3,1
2.        QUEUE        CUA
3.        ENTER        LATHE
4.        DEPART       CUA
5.        ADVANCE      3
6.        LEAVE        LATHE
7.        TERMINATE    1

Legend: (Id, current position, where the entity wants to go, when the movement will take place)

The entityt (1,-,1,2) of the example:
  - Id 1
  - "-" outisde the system
  - To the GENERATE block
  - From outside to GENERATE block on time 2.

| Pas | Temps | CEC        | FEC                    | Comments                                                    |
|-----|-------|------------|------------------------|-------------------------------------------------------------|
| 1   | Inici |  -         |  -                     |                                                             |
| 2   | 0     |  -         | (1,-,1,2)              | First Xact.                                                 |
| 3   | 2     |(1,-,1,Now) | -                      | Xact from FEC to CEC                                        |
| 4   | 2     |-           | (2,-,1,4), (1,5,6,5)   | First entity in ADVANCE, second entity generated            |
| 5   | 4     |(2,-,1,Now) | (1,5,6,5)              | Second entity from FEC to CEC                               |
| 6   | 4     |(2,2,3,Now) | (1,5,6,5), (3,-,1,8)   | Moving entity 2 to waiting for LATHE, third entity waiting to be generated appears  |

See slides for the whole Process Interaction Example.

There is a table comparing the three simulation enginees: Event Scheduling, Process Interaction, and Activity Scanning.
