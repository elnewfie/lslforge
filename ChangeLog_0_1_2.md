  * Bug fixes
    * Search results view: double-clicking a result now correctly activates the proper editor/tab
    * Navigator view: double-clicking a result now correctly activates the proper editor/tab

  * New features
    * Outline View window: Added ability to sort outline items alphabetically.
    * Auto-complete now includes global variables and functions defined in script.

  * Functions
    * llGetAgentList


  * Constants
    * AGENT\_LIST\_PARCEL
    * AGENT\_LIST\_PARCEL\_OWNER
    * AGENT\_LIST\_REGION
    * HTTP\_BODY\_MAXLENGTH
    * PRIM\_SLICE

  * Miscellaneous
    * Rewritten LSLForgeOutlinePage, separated into following classes:
      * OutlineBuilder
      * OutlineItem(s)
      * DocumentOutline
    * Should make things a lot easier for future outline-related features.
    * Minimum supported version of Eclipse is 3.6 (Helios).