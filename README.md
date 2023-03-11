Glide
=

Glide is an interpreted javascript runtime written entirely in safe rust. 

Glide is still early in development and at present only passes around
10% of the TS262 test suite.

The compiler and runtime of Glide can be compiled and run separately
(if eval is disabled), which can be used in situations where a
smaller runtime with a faster startup is desired. The runtime format is not stable between releases of glide.

Feature support table

| Feature | Support |
| --- | --- |
| Arithmetic | partial |
| Functions | partial |
| Classes | partial |
| Generators | none |
| Async | none |
