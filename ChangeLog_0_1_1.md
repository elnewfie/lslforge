  * New icons
    * New navigator icons to make LSLForge files more distinctive and recognizable
    * Outline view icons made a little cleaner and more consistent with other eclipse outline styles
  * Code quality improvements:
    * Changes to make all references to lsl as "LSL"
    * Changes to remove warnings, added useful stuff like @override where applicable
  * New Editor view!
    * Both .lsl and .lslp files will open in a combined editor
    * The editor contains two tabs at the bottom; one for the source code (.lslp) and one for the compiled version (.lsl).
    * The compiled tab is read-only to prevent changes
    * The compiled tab's background is a light gray to make it visibly apparent which tab one is in
    * Opening a .lsl file and then subsequently opening the .lslp version (or vice-verse) will not open a second editor window, but instead activate the one already open
    * Both .lslp and .lsl files will appear with syntax coloring.
    * Press Alt-F7 to switch between tabs
  * The outline view will display for both .lsl and .lslp files now, not just .lslp files
  * "Classic" LSL file support (.lsl files with no matching .lslp):
    * Will now open in LSLForge editor
    * Has syntax coloring/outline view enabled
    * Compilation/optimization is not supported, rename to .lslp to activate full support
  * New constants:
    * DENSITY
    * ESTATE\_ACCESS\_ALLOWED\_AGENT\_ADD
    * ESTATE\_ACCESS\_ALLOWED\_AGENT\_REMOVE
    * ESTATE\_ACCESS\_ALLOWED\_GROUP\_ADD
    * ESTATE\_ACCESS\_ALLOWED\_GROUP\_REMOVE
    * ESTATE\_ACCESS\_BANNED\_AGENT\_ADD
    * ESTATE\_ACCESS\_BANNED\_AGENT\_REMOVE
    * FRICTION
    * GRAVITY\_MULTIPLIER
    * KFM\_COMMAND
    * KFM\_CMD\_STOP
    * KFM\_CMD\_PLAY
    * KFM\_CMD\_PAUSE
    * KFM\_DATA
    * KFM\_FORWARD
    * KFM\_LOOP
    * KFM\_MODE
    * KFM\_PING\_PONG
    * KFM\_REVERSE
    * KFM\_ROTATION
    * KFM\_TRANSLATION
    * OBJECT\_PHYSICS\_COST
    * OBJECT\_PRIM\_EQUIVALENCE
    * OBJECT\_SERVER\_COST
    * OBJECT\_STREAMING\_COST
    * PRIM\_PHYSICS\_MATERIAL
    * RC\_DETECT\_PHANTOM
    * RC\_DATA\_FLAGS
    * RC\_GET\_NORMAL
    * RC\_GET\_ROOT\_KEY
    * RC\_GET\_LINK\_NUM
    * RC\_MAX\_HITS
    * RC\_REJECT\_TYPES
    * RC\_REJECT\_AGENTS
    * RC\_REJECT\_PHYSICAL
    * RC\_REJECT\_NONPHYSICAL
    * RC\_REJECT\_LAND
    * RCERR\_UNKNOWN
    * RCERR\_SIM\_PERF\_LOW
    * RCERR\_CAST\_TIME\_EXCEEDED
    * RESTITUTION
  * New Event:
    * transaction\_result
  * New LL`*` functions:
    * llGetMassMKS
    * llGetParcelMusicURL
    * llGetPhysicsMaterial
    * llManageEstateAccess
    * llSetKeyframedMotion
    * llSetPhysicsMaterial
    * llSetRegionPos
    * llTransferLindenDollars