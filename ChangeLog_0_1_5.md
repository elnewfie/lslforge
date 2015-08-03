Fixed a number of issues causing severe freezes and memory problems when editing a file, including:

  * Outline View:
    * Updates now happen on background thread, no longer freezing the editor.
    * Outline rebuilds now auto-cancel if they become redundant (for example, because another update replaced it)

  * Auto-complete:
    * Various null-checks
    * Uses last good outline results instead of waiting for Outline rebuild to complete

  * LSLForge Native Executable
    * Multiple instances would run, one for each open project.  Modified now to share a single instance globally.
    * Improved logging
    * Minor cleanup of licensing code (removed dummy text)
    * Fixed issue that caused some error messages to not display correctly in Problems View


## Added ##
  * Constants
    * CONTENT\_TYPE\_ATOM
    * CONTENT\_TYPE\_FORM
    * CONTENT\_TYPE\_JSON
    * CONTENT\_TYPE\_LLSD
    * CONTENT\_TYPE\_RSS
    * CONTENT\_TYPE\_XHTML
    * CONTENT\_TYPE\_XML
    * PERMISSION\_OVERRIDE\_ANIMATIONS


  * Functions
    * llGetAnimationOverride
    * llResetAnimationOverride
    * llSetAnimationOverride

  * Events
    * path\_update