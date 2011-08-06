%-define(EUNIT_NOAUTO, 1).

-define(TYPE_DOUBLE, 1).
-define(TYPE_FLOAT, 2).
-define(TYPE_INT64, 3).   % Not ZigZag encoded.  Negative numbers
                          % take 10 bytes.  Use TYPE_SINT64 if negative
                          % values are likely.
-define(TYPE_UINT64, 4).
-define(TYPE_INT32, 5).   % Not ZigZag encoded.  Negative numbers
                          % take 10 bytes.  Use TYPE_SINT32 if negative
                          % values are likely.
-define(TYPE_FIXED64, 6).
-define(TYPE_FIXED32, 7).
-define(TYPE_BOOL, 8).
-define(TYPE_STRING, 9).
-define(TYPE_GROUP, 10).   % Tag-delimited aggregate.
-define(TYPE_MESSAGE, 11). % Length-delimited aggregate.

-define(TYPE_BYTES, 12).
-define(TYPE_UINT32, 13).
-define(TYPE_ENUM, 14).
-define(TYPE_SFIXED32, 15).
-define(TYPE_SFIXED64, 16).
-define(TYPE_SINT32, 17).  % Uses ZigZag encoding.
-define(TYPE_SINT64, 18).  % Uses ZigZag encoding.

-define(LABEL_OPTIONAL, 1).
-define(LABEL_REQUIRED, 2).
-define(LABEL_REPEATED, 3).

-define(WT_VARINT, 0).
-define(WT_64BIT, 1).
-define(WT_LEN, 2).
-define(WT_32BIT, 5).

-record(field_desc, {name, 
					 number, 
					 label, 
					 type, 
					 type_name, 
					 default_value}).