The runtime allows multiple threads to concurrently execute 
javascript code. Multi-threaded execution
without modifying the execution model of javascript is supported
through immutability and structural sharing.

Objects in javascript can either be owned by a specific realm, or be 
shared between several realms. An object owned by a single realm operates
in a mutable mode - where changes can be made directly to the object.
Shared objects are instead made immutable - with any changes to the objects
causing a copy to be made of the underlying object.

```
OWNED(RC + MUTABLE OBJECT)) --Freeze-> SHARED(ARC + IMMUTABLE OBJECT)
 
SHARED(ARC + IMMUTABLE OBJECT) --Write-> OWNED(RC + MUTABLE OBJECT)) AND SHARED(ARC + IMMUTABLE OBJECT) 
```

Realms can be executed concurrently across a threadpool, and do not have
affinity to a specific thread. Realms are designed to be lightweight enough
that you should be able to start a single realm per incoming request and
dispose of it at the end of a network request.