This example tests the idea of a library communicating with an executable
which may be compiled against a range of library versions.

By using private dependencies the library can understand and interact with executables
built against newer or older versions of the library.

* `lib01` - The library which defines the interface, which can have different versions.

* `main-prog` - A program written against the `lib01` interface, but needs to work with multiple versions.

* `hooks-exe` - The executable which can be compiled against multiple versions of `lib01`.

* `hooks-lib` - A compability library which depends on all versions of lib01 which are supported and provides conversions between the new and old datatypes.
