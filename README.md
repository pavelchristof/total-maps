Total maps
==========

  Total maps are maps that contain a value for every key. This library provides
  various flavors of total maps.
  
  Dense maps store values for all keys. Sparse maps store a default value
  and the values which differ from the default.
  
  Bounded maps require the key type to be bounded (have a finite number of
  values) for most of their functionality. Subset maps do not require the
  key to be bounded, instead they are parametized by a finite set of
  valid keys. The key subset is retrieved with help of the excellent
  'reflection' library.
  
  Maps in this library provide most of their functions in typeclasses and so
  the modules are designed to be imported unqualified.
