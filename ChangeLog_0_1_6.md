This version is largely just updates to add new LSL features.

## Fixes ##
  * Minor logging fix
  * Change to reduce error popups when rebuilding outline view

## Added ##
  * Constants:
    * CHARACTER\_STAY\_WITHIN\_PARCEL
    * ERR\_GENERIC
    * ERR\_MALFORMED\_PARAMS
    * ERR\_PARCEL\_PERMISSIONS
    * ERR\_RUNTIME\_PERMISSIONS
    * ERR\_THROTTLED
    * JSON\_ARRAY
    * JSON\_DELETE
    * JSON\_FALSE
    * JSON\_INVALID
    * JSON\_NULL
    * JSON\_NUMBER
    * JSON\_OBJECT
    * JSON\_STRING
    * JSON\_TRUE
    * OBJECT\_RETURN\_PARCEL
    * OBJECT\_RETURN\_PARCEL\_OWNER
    * OBJECT\_RETURN\_REGION
    * PERMISSION\_RETURN\_OBJECTS
    * PSYS\_PART\_END\_GLOW
    * PSYS\_PART\_RIBBON\_MASK
    * PSYS\_PART\_START\_GLOW

  * Functions
    * llJson2List
    * llJsonGetValue
    * llJsonSetValue
    * llJsonValueType
    * llList2Json
    * llReturnObjectsByID
    * llReturnObjectsByOwner
    * llXorBase64