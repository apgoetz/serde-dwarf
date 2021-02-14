Provide generic serde Deserializer for Non-self describing formats

`serde_dwarf` provides wrappers to implement derserialize_any for
non-self descriptive data types.

The `Value` types constructed by `serde_dwarf` can then be inspected
or re-serialized in any arbitrary serde data format, and can be
re-deserialized into the rust data type. 

Possible use cases: 

+ jq for bincode
+ serde_transcode for non-self describing data types
