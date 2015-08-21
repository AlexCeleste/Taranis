
#Taranis

This is very much a work-in-progress. Come back later, there's nothing to run yet.
The compiler is currently being hacked together out of "BitsBasic", a previous port project.

This is a reimplementation of the classic [Blitz Basic](https://github.com/blitz-research/blitzplus)
language (2D), built atop Gambit Scheme and SDL2. The goal is ease of use across
multiple modern platforms including OSX, SteamOS, and Chrome OS. The target output
language will be C.

Compatibility with the original core language is a priority, although some
features will be changed by necessity (no DLLs on non-Windows platforms). A few
new features will be added but will not dominate the language.
