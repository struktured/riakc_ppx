module BytesCache = Cache.Make(Cache.Bytes)(Cache.Bytes)
module BytesBoolCache = Cache.Make(Cache.Bytes)(Cache.Bool)
module BytesIntCache = Cache.Make(Cache.Bytes)(Cache.Int)
module IntBytesCache = Cache.Make(Cache.Int)(Cache.Bytes)
module IntBoolCache = Cache.Make(Cache.Int)(Cache.Bool)
module StringCache = Cache.Make(Cache.String)(Cache.String)
module StringBoolCache = Cache.Make(Cache.String)(Cache.Bool)
module StringIntCache = Cache.Make(Cache.String)(Cache.Int)





