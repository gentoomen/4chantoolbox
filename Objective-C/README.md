###About

This folder contains a set of native Objective-C 4chantoolbox implementations. Like the others, they are meant to be short, basic, and as easily understood as possible, so that readers may get a feel for how the same problems might be solved in various different languages.

One important difference to take note of in this approach vs the other languages hosted thus far is the use of a full, tidied XML parser for extracting information from a document, rather than simple regex string manipulation. This method is employed at its most basic in the raw implementation, and used extensively in ChanKit.
###Organization

Within the Objective-C subproject are two different classes of implementation: **Application-ObjC** and **Application-ChanKit**.

ObjC are strict Objective-C applications which link only against the Foundation framework and will build out of the box, possibly even on GNUStep without modification. They can be built in the following way using GCC or clang:
 `$CC -framework Foundation -o application [source files]`
- - -
ChanKit applications, rather, act as basic frontends to the ChanKit framework available here: [command-Q/ChanKit](http://github.com/command-Q/ChanKit)
A more complex command-line test interface demoing the full capabilities of the framework is available with the project.
The build command for a ChanKit application is:
`$CC -framework Cocoa -framework ChanKit -o application [source files]`

Or if ChanKit.framework is not in your framework search path, explicitly specify the path with `-F $basepath`.
ChanKit itself can be built with Xcode 3.1+ for Mac OS X targets using the project file provided in the repository.
###Known Issues

* Scraper-ObjC will not respond properly to deleted imageposts; n number of new images will be skipped where n is the number of posts deleted since the most recent fetch. This situation is prohibitively complex to track for the scope of this example. This does not affect the ChanKit counterpart, which employs said prohibitive complexity.

###Differences from Script Implementations

At the time of writing the Obj-C implementations differ from the script implementations in the following ways:

* Currently, command line arguments are parsed by the UserDefaults system, which requires all arguments to be of the form `-key value`. This trade-off was made due to the simplicity of the UserDefaults solution and a lack of desire to sacrifice the clarity of this example code with manual parsing or use of getopt. Help text has been edited to reflect these changes.
* Console output is written to stderr rather than stdout. This is NSLog's standard behavior; in the spirit of conveying the common conventions of the language it has not been replaced with a FileHandle to stdout.
* Quieted output has not been implemented, as unlike the script equivalents it requires either wrapping every call to NSLog in an if statement, or the creation of a variadic function wrapper around NSLog.

Other than that, the code is extensively commented and hopefully comprehensible even for those unfamiliar with Objective-C's unorthodox syntax.