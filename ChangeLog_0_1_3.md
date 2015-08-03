  * Bug fixes
    * Removed InstallHandler.java, which is no longer supported in Eclipse Juno (4.2).  Can now install LSLForge on Juno.
    * Fixed issue causing the editor to switch tabs repeatedly when a full recompile occurs
    * Outline view displays a (friendlier) message when project does not have LSLForge support

  * Regressions
    * Navigator view double-click no longer activates correct tab, prior fix causing widespread (minor) side effects. Reverted for now.

  * New features
    * Added Configure->Add/Remove LSL Support Menu

  * Functions
    * llAttachToAvatarTemp
    * llCreateCharacter
    * llDeleteCharacter
    * llEvade
    * llExecCharacterCmd
    * llFleeFrom
    * llGetClosestNavPoint
    * llGetStaticPath
    * llNavigateTo
    * llPatrolPoints
    * llPursue
    * llUpdateCharacter
    * llWanderWithin
    * llTeleportAgent
    * llTeleportAgentGlobalCoords

  * Constants
    * CHARACTER\_AVOIDANCE\_MODE
    * CHARACTER\_CMD\_JUMP
    * CHARACTER\_CMD\_SMOOTH\_STOP
    * CHARACTER\_CMD\_STOP
    * CHARACTER\_DESIRED\_SPEED
    * CHARACTER\_DESIRED\_TURN\_SPEED
    * CHARACTER\_RADIUS
    * CHARACTER\_LENGTH
    * CHARACTER\_MAX\_ACCEL
    * CHARACTER\_MAX\_DECEL
    * CHARACTER\_MAX\_SPEED
    * CHARACTER\_MAX\_TURN\_RADIUS
    * CHARACTER\_ORIENTATION
    * CHARACTER\_TYPE
    * CHARACTER\_TYPE\_A
    * CHARACTER\_TYPE\_B
    * CHARACTER\_TYPE\_C
    * CHARACTER\_TYPE\_D
    * CHARACTER\_TYPE\_NONE
    * FORCE\_DIRECT\_PATH
    * GCNP\_RADIUS
    * GCNP\_STATIC
    * HORIZONTAL
    * PATROL\_PAUSE\_AT\_WAYPOINTS
    * PERMISSION\_TELEPORT
    * PURSUE\_OFFSET
    * PURSUIT\_FUZZ\_FACTOR
    * PURSUIT\_INTERCEPT
    * PURSUIT\_GOAL\_TOLERANCE
    * PU\_EVADE\_HIDDEN
    * PU\_EVADE\_SPOTTED
    * PU\_FAILURE\_DYNAMIC\_PATHFINDING\_DISABLED
    * PU\_FAILURE\_INVALID\_GOAL
    * PU\_FAILURE\_INVALID\_START
    * PU\_FAILURE\_NO\_NAVMESH
    * PU\_FAILURE\_NO\_VALID\_DESTINATION
    * PU\_FAILURE\_OTHER
    * PU\_FAILURE\_PARCEL\_UNREACHABLE
    * PU\_FAILURE\_TARGET\_GONE
    * PU\_FAILURE\_UNREACHABLE
    * PU\_GOAL\_REACHED
    * PU\_SLOWDOWN\_DISTANCE\_REACHED
    * REQUIRE\_LINE\_OF\_SIGHT
    * TRAVERSAL\_TYPE
    * TRAVERSAL\_TYPE\_SLOW
    * TRAVERSAL\_TYPE\_FAST
    * TRAVERSAL\_TYPE\_NONE
    * VERTICAL
    * WANDER\_PAUSE\_AT\_WAYPOINTS