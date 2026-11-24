/* Wrap RTExStackImpl.h so that the extension implies C++ to compilers,
 * in case there is a problem with a command line option making
 * everything C++.
 *
 * This file uses throw so must be C++.
 */
#include "RTExStackImpl.h"
