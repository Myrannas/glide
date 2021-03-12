Glide
=

Glide is an interpreted javascript runtime written entirely in safe rust. 

Glide is still early in development and at present only covers around
5% of the TS262 test suite.

The compiler and runtime of Glide can be compiled and run separately
(if eval is disabled), which can be used in situations where a
smaller runtime with a faster startup is desired. The runtime format is not stable between releases of glide.