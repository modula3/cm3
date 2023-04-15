/******/ (() => { // webpackBootstrap
/******/ 	var __webpack_modules__ = ({

/***/ 625:
/***/ ((module, exports) => {

exports = module.exports = SemVer

var debug
/* istanbul ignore next */
if (typeof process === 'object' &&
    process.env &&
    process.env.NODE_DEBUG &&
    /\bsemver\b/i.test(process.env.NODE_DEBUG)) {
  debug = function () {
    var args = Array.prototype.slice.call(arguments, 0)
    args.unshift('SEMVER')
    console.log.apply(console, args)
  }
} else {
  debug = function () {}
}

// Note: this is the semver.org version of the spec that it implements
// Not necessarily the package version of this code.
exports.SEMVER_SPEC_VERSION = '2.0.0'

var MAX_LENGTH = 256
var MAX_SAFE_INTEGER = Number.MAX_SAFE_INTEGER ||
  /* istanbul ignore next */ 9007199254740991

// Max safe segment length for coercion.
var MAX_SAFE_COMPONENT_LENGTH = 16

// The actual regexps go on exports.re
var re = exports.re = []
var src = exports.src = []
var R = 0

// The following Regular Expressions can be used for tokenizing,
// validating, and parsing SemVer version strings.

// ## Numeric Identifier
// A single `0`, or a non-zero digit followed by zero or more digits.

var NUMERICIDENTIFIER = R++
src[NUMERICIDENTIFIER] = '0|[1-9]\\d*'
var NUMERICIDENTIFIERLOOSE = R++
src[NUMERICIDENTIFIERLOOSE] = '[0-9]+'

// ## Non-numeric Identifier
// Zero or more digits, followed by a letter or hyphen, and then zero or
// more letters, digits, or hyphens.

var NONNUMERICIDENTIFIER = R++
src[NONNUMERICIDENTIFIER] = '\\d*[a-zA-Z-][a-zA-Z0-9-]*'

// ## Main Version
// Three dot-separated numeric identifiers.

var MAINVERSION = R++
src[MAINVERSION] = '(' + src[NUMERICIDENTIFIER] + ')\\.' +
                   '(' + src[NUMERICIDENTIFIER] + ')\\.' +
                   '(' + src[NUMERICIDENTIFIER] + ')'

var MAINVERSIONLOOSE = R++
src[MAINVERSIONLOOSE] = '(' + src[NUMERICIDENTIFIERLOOSE] + ')\\.' +
                        '(' + src[NUMERICIDENTIFIERLOOSE] + ')\\.' +
                        '(' + src[NUMERICIDENTIFIERLOOSE] + ')'

// ## Pre-release Version Identifier
// A numeric identifier, or a non-numeric identifier.

var PRERELEASEIDENTIFIER = R++
src[PRERELEASEIDENTIFIER] = '(?:' + src[NUMERICIDENTIFIER] +
                            '|' + src[NONNUMERICIDENTIFIER] + ')'

var PRERELEASEIDENTIFIERLOOSE = R++
src[PRERELEASEIDENTIFIERLOOSE] = '(?:' + src[NUMERICIDENTIFIERLOOSE] +
                                 '|' + src[NONNUMERICIDENTIFIER] + ')'

// ## Pre-release Version
// Hyphen, followed by one or more dot-separated pre-release version
// identifiers.

var PRERELEASE = R++
src[PRERELEASE] = '(?:-(' + src[PRERELEASEIDENTIFIER] +
                  '(?:\\.' + src[PRERELEASEIDENTIFIER] + ')*))'

var PRERELEASELOOSE = R++
src[PRERELEASELOOSE] = '(?:-?(' + src[PRERELEASEIDENTIFIERLOOSE] +
                       '(?:\\.' + src[PRERELEASEIDENTIFIERLOOSE] + ')*))'

// ## Build Metadata Identifier
// Any combination of digits, letters, or hyphens.

var BUILDIDENTIFIER = R++
src[BUILDIDENTIFIER] = '[0-9A-Za-z-]+'

// ## Build Metadata
// Plus sign, followed by one or more period-separated build metadata
// identifiers.

var BUILD = R++
src[BUILD] = '(?:\\+(' + src[BUILDIDENTIFIER] +
             '(?:\\.' + src[BUILDIDENTIFIER] + ')*))'

// ## Full Version String
// A main version, followed optionally by a pre-release version and
// build metadata.

// Note that the only major, minor, patch, and pre-release sections of
// the version string are capturing groups.  The build metadata is not a
// capturing group, because it should not ever be used in version
// comparison.

var FULL = R++
var FULLPLAIN = 'v?' + src[MAINVERSION] +
                src[PRERELEASE] + '?' +
                src[BUILD] + '?'

src[FULL] = '^' + FULLPLAIN + '$'

// like full, but allows v1.2.3 and =1.2.3, which people do sometimes.
// also, 1.0.0alpha1 (prerelease without the hyphen) which is pretty
// common in the npm registry.
var LOOSEPLAIN = '[v=\\s]*' + src[MAINVERSIONLOOSE] +
                 src[PRERELEASELOOSE] + '?' +
                 src[BUILD] + '?'

var LOOSE = R++
src[LOOSE] = '^' + LOOSEPLAIN + '$'

var GTLT = R++
src[GTLT] = '((?:<|>)?=?)'

// Something like "2.*" or "1.2.x".
// Note that "x.x" is a valid xRange identifer, meaning "any version"
// Only the first item is strictly required.
var XRANGEIDENTIFIERLOOSE = R++
src[XRANGEIDENTIFIERLOOSE] = src[NUMERICIDENTIFIERLOOSE] + '|x|X|\\*'
var XRANGEIDENTIFIER = R++
src[XRANGEIDENTIFIER] = src[NUMERICIDENTIFIER] + '|x|X|\\*'

var XRANGEPLAIN = R++
src[XRANGEPLAIN] = '[v=\\s]*(' + src[XRANGEIDENTIFIER] + ')' +
                   '(?:\\.(' + src[XRANGEIDENTIFIER] + ')' +
                   '(?:\\.(' + src[XRANGEIDENTIFIER] + ')' +
                   '(?:' + src[PRERELEASE] + ')?' +
                   src[BUILD] + '?' +
                   ')?)?'

var XRANGEPLAINLOOSE = R++
src[XRANGEPLAINLOOSE] = '[v=\\s]*(' + src[XRANGEIDENTIFIERLOOSE] + ')' +
                        '(?:\\.(' + src[XRANGEIDENTIFIERLOOSE] + ')' +
                        '(?:\\.(' + src[XRANGEIDENTIFIERLOOSE] + ')' +
                        '(?:' + src[PRERELEASELOOSE] + ')?' +
                        src[BUILD] + '?' +
                        ')?)?'

var XRANGE = R++
src[XRANGE] = '^' + src[GTLT] + '\\s*' + src[XRANGEPLAIN] + '$'
var XRANGELOOSE = R++
src[XRANGELOOSE] = '^' + src[GTLT] + '\\s*' + src[XRANGEPLAINLOOSE] + '$'

// Coercion.
// Extract anything that could conceivably be a part of a valid semver
var COERCE = R++
src[COERCE] = '(?:^|[^\\d])' +
              '(\\d{1,' + MAX_SAFE_COMPONENT_LENGTH + '})' +
              '(?:\\.(\\d{1,' + MAX_SAFE_COMPONENT_LENGTH + '}))?' +
              '(?:\\.(\\d{1,' + MAX_SAFE_COMPONENT_LENGTH + '}))?' +
              '(?:$|[^\\d])'

// Tilde ranges.
// Meaning is "reasonably at or greater than"
var LONETILDE = R++
src[LONETILDE] = '(?:~>?)'

var TILDETRIM = R++
src[TILDETRIM] = '(\\s*)' + src[LONETILDE] + '\\s+'
re[TILDETRIM] = new RegExp(src[TILDETRIM], 'g')
var tildeTrimReplace = '$1~'

var TILDE = R++
src[TILDE] = '^' + src[LONETILDE] + src[XRANGEPLAIN] + '$'
var TILDELOOSE = R++
src[TILDELOOSE] = '^' + src[LONETILDE] + src[XRANGEPLAINLOOSE] + '$'

// Caret ranges.
// Meaning is "at least and backwards compatible with"
var LONECARET = R++
src[LONECARET] = '(?:\\^)'

var CARETTRIM = R++
src[CARETTRIM] = '(\\s*)' + src[LONECARET] + '\\s+'
re[CARETTRIM] = new RegExp(src[CARETTRIM], 'g')
var caretTrimReplace = '$1^'

var CARET = R++
src[CARET] = '^' + src[LONECARET] + src[XRANGEPLAIN] + '$'
var CARETLOOSE = R++
src[CARETLOOSE] = '^' + src[LONECARET] + src[XRANGEPLAINLOOSE] + '$'

// A simple gt/lt/eq thing, or just "" to indicate "any version"
var COMPARATORLOOSE = R++
src[COMPARATORLOOSE] = '^' + src[GTLT] + '\\s*(' + LOOSEPLAIN + ')$|^$'
var COMPARATOR = R++
src[COMPARATOR] = '^' + src[GTLT] + '\\s*(' + FULLPLAIN + ')$|^$'

// An expression to strip any whitespace between the gtlt and the thing
// it modifies, so that `> 1.2.3` ==> `>1.2.3`
var COMPARATORTRIM = R++
src[COMPARATORTRIM] = '(\\s*)' + src[GTLT] +
                      '\\s*(' + LOOSEPLAIN + '|' + src[XRANGEPLAIN] + ')'

// this one has to use the /g flag
re[COMPARATORTRIM] = new RegExp(src[COMPARATORTRIM], 'g')
var comparatorTrimReplace = '$1$2$3'

// Something like `1.2.3 - 1.2.4`
// Note that these all use the loose form, because they'll be
// checked against either the strict or loose comparator form
// later.
var HYPHENRANGE = R++
src[HYPHENRANGE] = '^\\s*(' + src[XRANGEPLAIN] + ')' +
                   '\\s+-\\s+' +
                   '(' + src[XRANGEPLAIN] + ')' +
                   '\\s*$'

var HYPHENRANGELOOSE = R++
src[HYPHENRANGELOOSE] = '^\\s*(' + src[XRANGEPLAINLOOSE] + ')' +
                        '\\s+-\\s+' +
                        '(' + src[XRANGEPLAINLOOSE] + ')' +
                        '\\s*$'

// Star ranges basically just allow anything at all.
var STAR = R++
src[STAR] = '(<|>)?=?\\s*\\*'

// Compile to actual regexp objects.
// All are flag-free, unless they were created above with a flag.
for (var i = 0; i < R; i++) {
  debug(i, src[i])
  if (!re[i]) {
    re[i] = new RegExp(src[i])
  }
}

exports.parse = parse
function parse (version, options) {
  if (!options || typeof options !== 'object') {
    options = {
      loose: !!options,
      includePrerelease: false
    }
  }

  if (version instanceof SemVer) {
    return version
  }

  if (typeof version !== 'string') {
    return null
  }

  if (version.length > MAX_LENGTH) {
    return null
  }

  var r = options.loose ? re[LOOSE] : re[FULL]
  if (!r.test(version)) {
    return null
  }

  try {
    return new SemVer(version, options)
  } catch (er) {
    return null
  }
}

exports.valid = valid
function valid (version, options) {
  var v = parse(version, options)
  return v ? v.version : null
}

exports.clean = clean
function clean (version, options) {
  var s = parse(version.trim().replace(/^[=v]+/, ''), options)
  return s ? s.version : null
}

exports.SemVer = SemVer

function SemVer (version, options) {
  if (!options || typeof options !== 'object') {
    options = {
      loose: !!options,
      includePrerelease: false
    }
  }
  if (version instanceof SemVer) {
    if (version.loose === options.loose) {
      return version
    } else {
      version = version.version
    }
  } else if (typeof version !== 'string') {
    throw new TypeError('Invalid Version: ' + version)
  }

  if (version.length > MAX_LENGTH) {
    throw new TypeError('version is longer than ' + MAX_LENGTH + ' characters')
  }

  if (!(this instanceof SemVer)) {
    return new SemVer(version, options)
  }

  debug('SemVer', version, options)
  this.options = options
  this.loose = !!options.loose

  var m = version.trim().match(options.loose ? re[LOOSE] : re[FULL])

  if (!m) {
    throw new TypeError('Invalid Version: ' + version)
  }

  this.raw = version

  // these are actually numbers
  this.major = +m[1]
  this.minor = +m[2]
  this.patch = +m[3]

  if (this.major > MAX_SAFE_INTEGER || this.major < 0) {
    throw new TypeError('Invalid major version')
  }

  if (this.minor > MAX_SAFE_INTEGER || this.minor < 0) {
    throw new TypeError('Invalid minor version')
  }

  if (this.patch > MAX_SAFE_INTEGER || this.patch < 0) {
    throw new TypeError('Invalid patch version')
  }

  // numberify any prerelease numeric ids
  if (!m[4]) {
    this.prerelease = []
  } else {
    this.prerelease = m[4].split('.').map(function (id) {
      if (/^[0-9]+$/.test(id)) {
        var num = +id
        if (num >= 0 && num < MAX_SAFE_INTEGER) {
          return num
        }
      }
      return id
    })
  }

  this.build = m[5] ? m[5].split('.') : []
  this.format()
}

SemVer.prototype.format = function () {
  this.version = this.major + '.' + this.minor + '.' + this.patch
  if (this.prerelease.length) {
    this.version += '-' + this.prerelease.join('.')
  }
  return this.version
}

SemVer.prototype.toString = function () {
  return this.version
}

SemVer.prototype.compare = function (other) {
  debug('SemVer.compare', this.version, this.options, other)
  if (!(other instanceof SemVer)) {
    other = new SemVer(other, this.options)
  }

  return this.compareMain(other) || this.comparePre(other)
}

SemVer.prototype.compareMain = function (other) {
  if (!(other instanceof SemVer)) {
    other = new SemVer(other, this.options)
  }

  return compareIdentifiers(this.major, other.major) ||
         compareIdentifiers(this.minor, other.minor) ||
         compareIdentifiers(this.patch, other.patch)
}

SemVer.prototype.comparePre = function (other) {
  if (!(other instanceof SemVer)) {
    other = new SemVer(other, this.options)
  }

  // NOT having a prerelease is > having one
  if (this.prerelease.length && !other.prerelease.length) {
    return -1
  } else if (!this.prerelease.length && other.prerelease.length) {
    return 1
  } else if (!this.prerelease.length && !other.prerelease.length) {
    return 0
  }

  var i = 0
  do {
    var a = this.prerelease[i]
    var b = other.prerelease[i]
    debug('prerelease compare', i, a, b)
    if (a === undefined && b === undefined) {
      return 0
    } else if (b === undefined) {
      return 1
    } else if (a === undefined) {
      return -1
    } else if (a === b) {
      continue
    } else {
      return compareIdentifiers(a, b)
    }
  } while (++i)
}

// preminor will bump the version up to the next minor release, and immediately
// down to pre-release. premajor and prepatch work the same way.
SemVer.prototype.inc = function (release, identifier) {
  switch (release) {
    case 'premajor':
      this.prerelease.length = 0
      this.patch = 0
      this.minor = 0
      this.major++
      this.inc('pre', identifier)
      break
    case 'preminor':
      this.prerelease.length = 0
      this.patch = 0
      this.minor++
      this.inc('pre', identifier)
      break
    case 'prepatch':
      // If this is already a prerelease, it will bump to the next version
      // drop any prereleases that might already exist, since they are not
      // relevant at this point.
      this.prerelease.length = 0
      this.inc('patch', identifier)
      this.inc('pre', identifier)
      break
    // If the input is a non-prerelease version, this acts the same as
    // prepatch.
    case 'prerelease':
      if (this.prerelease.length === 0) {
        this.inc('patch', identifier)
      }
      this.inc('pre', identifier)
      break

    case 'major':
      // If this is a pre-major version, bump up to the same major version.
      // Otherwise increment major.
      // 1.0.0-5 bumps to 1.0.0
      // 1.1.0 bumps to 2.0.0
      if (this.minor !== 0 ||
          this.patch !== 0 ||
          this.prerelease.length === 0) {
        this.major++
      }
      this.minor = 0
      this.patch = 0
      this.prerelease = []
      break
    case 'minor':
      // If this is a pre-minor version, bump up to the same minor version.
      // Otherwise increment minor.
      // 1.2.0-5 bumps to 1.2.0
      // 1.2.1 bumps to 1.3.0
      if (this.patch !== 0 || this.prerelease.length === 0) {
        this.minor++
      }
      this.patch = 0
      this.prerelease = []
      break
    case 'patch':
      // If this is not a pre-release version, it will increment the patch.
      // If it is a pre-release it will bump up to the same patch version.
      // 1.2.0-5 patches to 1.2.0
      // 1.2.0 patches to 1.2.1
      if (this.prerelease.length === 0) {
        this.patch++
      }
      this.prerelease = []
      break
    // This probably shouldn't be used publicly.
    // 1.0.0 "pre" would become 1.0.0-0 which is the wrong direction.
    case 'pre':
      if (this.prerelease.length === 0) {
        this.prerelease = [0]
      } else {
        var i = this.prerelease.length
        while (--i >= 0) {
          if (typeof this.prerelease[i] === 'number') {
            this.prerelease[i]++
            i = -2
          }
        }
        if (i === -1) {
          // didn't increment anything
          this.prerelease.push(0)
        }
      }
      if (identifier) {
        // 1.2.0-beta.1 bumps to 1.2.0-beta.2,
        // 1.2.0-beta.fooblz or 1.2.0-beta bumps to 1.2.0-beta.0
        if (this.prerelease[0] === identifier) {
          if (isNaN(this.prerelease[1])) {
            this.prerelease = [identifier, 0]
          }
        } else {
          this.prerelease = [identifier, 0]
        }
      }
      break

    default:
      throw new Error('invalid increment argument: ' + release)
  }
  this.format()
  this.raw = this.version
  return this
}

exports.inc = inc
function inc (version, release, loose, identifier) {
  if (typeof (loose) === 'string') {
    identifier = loose
    loose = undefined
  }

  try {
    return new SemVer(version, loose).inc(release, identifier).version
  } catch (er) {
    return null
  }
}

exports.diff = diff
function diff (version1, version2) {
  if (eq(version1, version2)) {
    return null
  } else {
    var v1 = parse(version1)
    var v2 = parse(version2)
    var prefix = ''
    if (v1.prerelease.length || v2.prerelease.length) {
      prefix = 'pre'
      var defaultResult = 'prerelease'
    }
    for (var key in v1) {
      if (key === 'major' || key === 'minor' || key === 'patch') {
        if (v1[key] !== v2[key]) {
          return prefix + key
        }
      }
    }
    return defaultResult // may be undefined
  }
}

exports.compareIdentifiers = compareIdentifiers

var numeric = /^[0-9]+$/
function compareIdentifiers (a, b) {
  var anum = numeric.test(a)
  var bnum = numeric.test(b)

  if (anum && bnum) {
    a = +a
    b = +b
  }

  return a === b ? 0
    : (anum && !bnum) ? -1
    : (bnum && !anum) ? 1
    : a < b ? -1
    : 1
}

exports.rcompareIdentifiers = rcompareIdentifiers
function rcompareIdentifiers (a, b) {
  return compareIdentifiers(b, a)
}

exports.major = major
function major (a, loose) {
  return new SemVer(a, loose).major
}

exports.minor = minor
function minor (a, loose) {
  return new SemVer(a, loose).minor
}

exports.patch = patch
function patch (a, loose) {
  return new SemVer(a, loose).patch
}

exports.compare = compare
function compare (a, b, loose) {
  return new SemVer(a, loose).compare(new SemVer(b, loose))
}

exports.compareLoose = compareLoose
function compareLoose (a, b) {
  return compare(a, b, true)
}

exports.rcompare = rcompare
function rcompare (a, b, loose) {
  return compare(b, a, loose)
}

exports.sort = sort
function sort (list, loose) {
  return list.sort(function (a, b) {
    return exports.compare(a, b, loose)
  })
}

exports.rsort = rsort
function rsort (list, loose) {
  return list.sort(function (a, b) {
    return exports.rcompare(a, b, loose)
  })
}

exports.gt = gt
function gt (a, b, loose) {
  return compare(a, b, loose) > 0
}

exports.lt = lt
function lt (a, b, loose) {
  return compare(a, b, loose) < 0
}

exports.eq = eq
function eq (a, b, loose) {
  return compare(a, b, loose) === 0
}

exports.neq = neq
function neq (a, b, loose) {
  return compare(a, b, loose) !== 0
}

exports.gte = gte
function gte (a, b, loose) {
  return compare(a, b, loose) >= 0
}

exports.lte = lte
function lte (a, b, loose) {
  return compare(a, b, loose) <= 0
}

exports.cmp = cmp
function cmp (a, op, b, loose) {
  switch (op) {
    case '===':
      if (typeof a === 'object')
        a = a.version
      if (typeof b === 'object')
        b = b.version
      return a === b

    case '!==':
      if (typeof a === 'object')
        a = a.version
      if (typeof b === 'object')
        b = b.version
      return a !== b

    case '':
    case '=':
    case '==':
      return eq(a, b, loose)

    case '!=':
      return neq(a, b, loose)

    case '>':
      return gt(a, b, loose)

    case '>=':
      return gte(a, b, loose)

    case '<':
      return lt(a, b, loose)

    case '<=':
      return lte(a, b, loose)

    default:
      throw new TypeError('Invalid operator: ' + op)
  }
}

exports.Comparator = Comparator
function Comparator (comp, options) {
  if (!options || typeof options !== 'object') {
    options = {
      loose: !!options,
      includePrerelease: false
    }
  }

  if (comp instanceof Comparator) {
    if (comp.loose === !!options.loose) {
      return comp
    } else {
      comp = comp.value
    }
  }

  if (!(this instanceof Comparator)) {
    return new Comparator(comp, options)
  }

  debug('comparator', comp, options)
  this.options = options
  this.loose = !!options.loose
  this.parse(comp)

  if (this.semver === ANY) {
    this.value = ''
  } else {
    this.value = this.operator + this.semver.version
  }

  debug('comp', this)
}

var ANY = {}
Comparator.prototype.parse = function (comp) {
  var r = this.options.loose ? re[COMPARATORLOOSE] : re[COMPARATOR]
  var m = comp.match(r)

  if (!m) {
    throw new TypeError('Invalid comparator: ' + comp)
  }

  this.operator = m[1]
  if (this.operator === '=') {
    this.operator = ''
  }

  // if it literally is just '>' or '' then allow anything.
  if (!m[2]) {
    this.semver = ANY
  } else {
    this.semver = new SemVer(m[2], this.options.loose)
  }
}

Comparator.prototype.toString = function () {
  return this.value
}

Comparator.prototype.test = function (version) {
  debug('Comparator.test', version, this.options.loose)

  if (this.semver === ANY) {
    return true
  }

  if (typeof version === 'string') {
    version = new SemVer(version, this.options)
  }

  return cmp(version, this.operator, this.semver, this.options)
}

Comparator.prototype.intersects = function (comp, options) {
  if (!(comp instanceof Comparator)) {
    throw new TypeError('a Comparator is required')
  }

  if (!options || typeof options !== 'object') {
    options = {
      loose: !!options,
      includePrerelease: false
    }
  }

  var rangeTmp

  if (this.operator === '') {
    rangeTmp = new Range(comp.value, options)
    return satisfies(this.value, rangeTmp, options)
  } else if (comp.operator === '') {
    rangeTmp = new Range(this.value, options)
    return satisfies(comp.semver, rangeTmp, options)
  }

  var sameDirectionIncreasing =
    (this.operator === '>=' || this.operator === '>') &&
    (comp.operator === '>=' || comp.operator === '>')
  var sameDirectionDecreasing =
    (this.operator === '<=' || this.operator === '<') &&
    (comp.operator === '<=' || comp.operator === '<')
  var sameSemVer = this.semver.version === comp.semver.version
  var differentDirectionsInclusive =
    (this.operator === '>=' || this.operator === '<=') &&
    (comp.operator === '>=' || comp.operator === '<=')
  var oppositeDirectionsLessThan =
    cmp(this.semver, '<', comp.semver, options) &&
    ((this.operator === '>=' || this.operator === '>') &&
    (comp.operator === '<=' || comp.operator === '<'))
  var oppositeDirectionsGreaterThan =
    cmp(this.semver, '>', comp.semver, options) &&
    ((this.operator === '<=' || this.operator === '<') &&
    (comp.operator === '>=' || comp.operator === '>'))

  return sameDirectionIncreasing || sameDirectionDecreasing ||
    (sameSemVer && differentDirectionsInclusive) ||
    oppositeDirectionsLessThan || oppositeDirectionsGreaterThan
}

exports.Range = Range
function Range (range, options) {
  if (!options || typeof options !== 'object') {
    options = {
      loose: !!options,
      includePrerelease: false
    }
  }

  if (range instanceof Range) {
    if (range.loose === !!options.loose &&
        range.includePrerelease === !!options.includePrerelease) {
      return range
    } else {
      return new Range(range.raw, options)
    }
  }

  if (range instanceof Comparator) {
    return new Range(range.value, options)
  }

  if (!(this instanceof Range)) {
    return new Range(range, options)
  }

  this.options = options
  this.loose = !!options.loose
  this.includePrerelease = !!options.includePrerelease

  // First, split based on boolean or ||
  this.raw = range
  this.set = range.split(/\s*\|\|\s*/).map(function (range) {
    return this.parseRange(range.trim())
  }, this).filter(function (c) {
    // throw out any that are not relevant for whatever reason
    return c.length
  })

  if (!this.set.length) {
    throw new TypeError('Invalid SemVer Range: ' + range)
  }

  this.format()
}

Range.prototype.format = function () {
  this.range = this.set.map(function (comps) {
    return comps.join(' ').trim()
  }).join('||').trim()
  return this.range
}

Range.prototype.toString = function () {
  return this.range
}

Range.prototype.parseRange = function (range) {
  var loose = this.options.loose
  range = range.trim()
  // `1.2.3 - 1.2.4` => `>=1.2.3 <=1.2.4`
  var hr = loose ? re[HYPHENRANGELOOSE] : re[HYPHENRANGE]
  range = range.replace(hr, hyphenReplace)
  debug('hyphen replace', range)
  // `> 1.2.3 < 1.2.5` => `>1.2.3 <1.2.5`
  range = range.replace(re[COMPARATORTRIM], comparatorTrimReplace)
  debug('comparator trim', range, re[COMPARATORTRIM])

  // `~ 1.2.3` => `~1.2.3`
  range = range.replace(re[TILDETRIM], tildeTrimReplace)

  // `^ 1.2.3` => `^1.2.3`
  range = range.replace(re[CARETTRIM], caretTrimReplace)

  // normalize spaces
  range = range.split(/\s+/).join(' ')

  // At this point, the range is completely trimmed and
  // ready to be split into comparators.

  var compRe = loose ? re[COMPARATORLOOSE] : re[COMPARATOR]
  var set = range.split(' ').map(function (comp) {
    return parseComparator(comp, this.options)
  }, this).join(' ').split(/\s+/)
  if (this.options.loose) {
    // in loose mode, throw out any that are not valid comparators
    set = set.filter(function (comp) {
      return !!comp.match(compRe)
    })
  }
  set = set.map(function (comp) {
    return new Comparator(comp, this.options)
  }, this)

  return set
}

Range.prototype.intersects = function (range, options) {
  if (!(range instanceof Range)) {
    throw new TypeError('a Range is required')
  }

  return this.set.some(function (thisComparators) {
    return thisComparators.every(function (thisComparator) {
      return range.set.some(function (rangeComparators) {
        return rangeComparators.every(function (rangeComparator) {
          return thisComparator.intersects(rangeComparator, options)
        })
      })
    })
  })
}

// Mostly just for testing and legacy API reasons
exports.toComparators = toComparators
function toComparators (range, options) {
  return new Range(range, options).set.map(function (comp) {
    return comp.map(function (c) {
      return c.value
    }).join(' ').trim().split(' ')
  })
}

// comprised of xranges, tildes, stars, and gtlt's at this point.
// already replaced the hyphen ranges
// turn into a set of JUST comparators.
function parseComparator (comp, options) {
  debug('comp', comp, options)
  comp = replaceCarets(comp, options)
  debug('caret', comp)
  comp = replaceTildes(comp, options)
  debug('tildes', comp)
  comp = replaceXRanges(comp, options)
  debug('xrange', comp)
  comp = replaceStars(comp, options)
  debug('stars', comp)
  return comp
}

function isX (id) {
  return !id || id.toLowerCase() === 'x' || id === '*'
}

// ~, ~> --> * (any, kinda silly)
// ~2, ~2.x, ~2.x.x, ~>2, ~>2.x ~>2.x.x --> >=2.0.0 <3.0.0
// ~2.0, ~2.0.x, ~>2.0, ~>2.0.x --> >=2.0.0 <2.1.0
// ~1.2, ~1.2.x, ~>1.2, ~>1.2.x --> >=1.2.0 <1.3.0
// ~1.2.3, ~>1.2.3 --> >=1.2.3 <1.3.0
// ~1.2.0, ~>1.2.0 --> >=1.2.0 <1.3.0
function replaceTildes (comp, options) {
  return comp.trim().split(/\s+/).map(function (comp) {
    return replaceTilde(comp, options)
  }).join(' ')
}

function replaceTilde (comp, options) {
  var r = options.loose ? re[TILDELOOSE] : re[TILDE]
  return comp.replace(r, function (_, M, m, p, pr) {
    debug('tilde', comp, _, M, m, p, pr)
    var ret

    if (isX(M)) {
      ret = ''
    } else if (isX(m)) {
      ret = '>=' + M + '.0.0 <' + (+M + 1) + '.0.0'
    } else if (isX(p)) {
      // ~1.2 == >=1.2.0 <1.3.0
      ret = '>=' + M + '.' + m + '.0 <' + M + '.' + (+m + 1) + '.0'
    } else if (pr) {
      debug('replaceTilde pr', pr)
      ret = '>=' + M + '.' + m + '.' + p + '-' + pr +
            ' <' + M + '.' + (+m + 1) + '.0'
    } else {
      // ~1.2.3 == >=1.2.3 <1.3.0
      ret = '>=' + M + '.' + m + '.' + p +
            ' <' + M + '.' + (+m + 1) + '.0'
    }

    debug('tilde return', ret)
    return ret
  })
}

// ^ --> * (any, kinda silly)
// ^2, ^2.x, ^2.x.x --> >=2.0.0 <3.0.0
// ^2.0, ^2.0.x --> >=2.0.0 <3.0.0
// ^1.2, ^1.2.x --> >=1.2.0 <2.0.0
// ^1.2.3 --> >=1.2.3 <2.0.0
// ^1.2.0 --> >=1.2.0 <2.0.0
function replaceCarets (comp, options) {
  return comp.trim().split(/\s+/).map(function (comp) {
    return replaceCaret(comp, options)
  }).join(' ')
}

function replaceCaret (comp, options) {
  debug('caret', comp, options)
  var r = options.loose ? re[CARETLOOSE] : re[CARET]
  return comp.replace(r, function (_, M, m, p, pr) {
    debug('caret', comp, _, M, m, p, pr)
    var ret

    if (isX(M)) {
      ret = ''
    } else if (isX(m)) {
      ret = '>=' + M + '.0.0 <' + (+M + 1) + '.0.0'
    } else if (isX(p)) {
      if (M === '0') {
        ret = '>=' + M + '.' + m + '.0 <' + M + '.' + (+m + 1) + '.0'
      } else {
        ret = '>=' + M + '.' + m + '.0 <' + (+M + 1) + '.0.0'
      }
    } else if (pr) {
      debug('replaceCaret pr', pr)
      if (M === '0') {
        if (m === '0') {
          ret = '>=' + M + '.' + m + '.' + p + '-' + pr +
                ' <' + M + '.' + m + '.' + (+p + 1)
        } else {
          ret = '>=' + M + '.' + m + '.' + p + '-' + pr +
                ' <' + M + '.' + (+m + 1) + '.0'
        }
      } else {
        ret = '>=' + M + '.' + m + '.' + p + '-' + pr +
              ' <' + (+M + 1) + '.0.0'
      }
    } else {
      debug('no pr')
      if (M === '0') {
        if (m === '0') {
          ret = '>=' + M + '.' + m + '.' + p +
                ' <' + M + '.' + m + '.' + (+p + 1)
        } else {
          ret = '>=' + M + '.' + m + '.' + p +
                ' <' + M + '.' + (+m + 1) + '.0'
        }
      } else {
        ret = '>=' + M + '.' + m + '.' + p +
              ' <' + (+M + 1) + '.0.0'
      }
    }

    debug('caret return', ret)
    return ret
  })
}

function replaceXRanges (comp, options) {
  debug('replaceXRanges', comp, options)
  return comp.split(/\s+/).map(function (comp) {
    return replaceXRange(comp, options)
  }).join(' ')
}

function replaceXRange (comp, options) {
  comp = comp.trim()
  var r = options.loose ? re[XRANGELOOSE] : re[XRANGE]
  return comp.replace(r, function (ret, gtlt, M, m, p, pr) {
    debug('xRange', comp, ret, gtlt, M, m, p, pr)
    var xM = isX(M)
    var xm = xM || isX(m)
    var xp = xm || isX(p)
    var anyX = xp

    if (gtlt === '=' && anyX) {
      gtlt = ''
    }

    if (xM) {
      if (gtlt === '>' || gtlt === '<') {
        // nothing is allowed
        ret = '<0.0.0'
      } else {
        // nothing is forbidden
        ret = '*'
      }
    } else if (gtlt && anyX) {
      // we know patch is an x, because we have any x at all.
      // replace X with 0
      if (xm) {
        m = 0
      }
      p = 0

      if (gtlt === '>') {
        // >1 => >=2.0.0
        // >1.2 => >=1.3.0
        // >1.2.3 => >= 1.2.4
        gtlt = '>='
        if (xm) {
          M = +M + 1
          m = 0
          p = 0
        } else {
          m = +m + 1
          p = 0
        }
      } else if (gtlt === '<=') {
        // <=0.7.x is actually <0.8.0, since any 0.7.x should
        // pass.  Similarly, <=7.x is actually <8.0.0, etc.
        gtlt = '<'
        if (xm) {
          M = +M + 1
        } else {
          m = +m + 1
        }
      }

      ret = gtlt + M + '.' + m + '.' + p
    } else if (xm) {
      ret = '>=' + M + '.0.0 <' + (+M + 1) + '.0.0'
    } else if (xp) {
      ret = '>=' + M + '.' + m + '.0 <' + M + '.' + (+m + 1) + '.0'
    }

    debug('xRange return', ret)

    return ret
  })
}

// Because * is AND-ed with everything else in the comparator,
// and '' means "any version", just remove the *s entirely.
function replaceStars (comp, options) {
  debug('replaceStars', comp, options)
  // Looseness is ignored here.  star is always as loose as it gets!
  return comp.trim().replace(re[STAR], '')
}

// This function is passed to string.replace(re[HYPHENRANGE])
// M, m, patch, prerelease, build
// 1.2 - 3.4.5 => >=1.2.0 <=3.4.5
// 1.2.3 - 3.4 => >=1.2.0 <3.5.0 Any 3.4.x will do
// 1.2 - 3.4 => >=1.2.0 <3.5.0
function hyphenReplace ($0,
  from, fM, fm, fp, fpr, fb,
  to, tM, tm, tp, tpr, tb) {
  if (isX(fM)) {
    from = ''
  } else if (isX(fm)) {
    from = '>=' + fM + '.0.0'
  } else if (isX(fp)) {
    from = '>=' + fM + '.' + fm + '.0'
  } else {
    from = '>=' + from
  }

  if (isX(tM)) {
    to = ''
  } else if (isX(tm)) {
    to = '<' + (+tM + 1) + '.0.0'
  } else if (isX(tp)) {
    to = '<' + tM + '.' + (+tm + 1) + '.0'
  } else if (tpr) {
    to = '<=' + tM + '.' + tm + '.' + tp + '-' + tpr
  } else {
    to = '<=' + to
  }

  return (from + ' ' + to).trim()
}

// if ANY of the sets match ALL of its comparators, then pass
Range.prototype.test = function (version) {
  if (!version) {
    return false
  }

  if (typeof version === 'string') {
    version = new SemVer(version, this.options)
  }

  for (var i = 0; i < this.set.length; i++) {
    if (testSet(this.set[i], version, this.options)) {
      return true
    }
  }
  return false
}

function testSet (set, version, options) {
  for (var i = 0; i < set.length; i++) {
    if (!set[i].test(version)) {
      return false
    }
  }

  if (version.prerelease.length && !options.includePrerelease) {
    // Find the set of versions that are allowed to have prereleases
    // For example, ^1.2.3-pr.1 desugars to >=1.2.3-pr.1 <2.0.0
    // That should allow `1.2.3-pr.2` to pass.
    // However, `1.2.4-alpha.notready` should NOT be allowed,
    // even though it's within the range set by the comparators.
    for (i = 0; i < set.length; i++) {
      debug(set[i].semver)
      if (set[i].semver === ANY) {
        continue
      }

      if (set[i].semver.prerelease.length > 0) {
        var allowed = set[i].semver
        if (allowed.major === version.major &&
            allowed.minor === version.minor &&
            allowed.patch === version.patch) {
          return true
        }
      }
    }

    // Version has a -pre, but it's not one of the ones we like.
    return false
  }

  return true
}

exports.satisfies = satisfies
function satisfies (version, range, options) {
  try {
    range = new Range(range, options)
  } catch (er) {
    return false
  }
  return range.test(version)
}

exports.maxSatisfying = maxSatisfying
function maxSatisfying (versions, range, options) {
  var max = null
  var maxSV = null
  try {
    var rangeObj = new Range(range, options)
  } catch (er) {
    return null
  }
  versions.forEach(function (v) {
    if (rangeObj.test(v)) {
      // satisfies(v, range, options)
      if (!max || maxSV.compare(v) === -1) {
        // compare(max, v, true)
        max = v
        maxSV = new SemVer(max, options)
      }
    }
  })
  return max
}

exports.minSatisfying = minSatisfying
function minSatisfying (versions, range, options) {
  var min = null
  var minSV = null
  try {
    var rangeObj = new Range(range, options)
  } catch (er) {
    return null
  }
  versions.forEach(function (v) {
    if (rangeObj.test(v)) {
      // satisfies(v, range, options)
      if (!min || minSV.compare(v) === 1) {
        // compare(min, v, true)
        min = v
        minSV = new SemVer(min, options)
      }
    }
  })
  return min
}

exports.minVersion = minVersion
function minVersion (range, loose) {
  range = new Range(range, loose)

  var minver = new SemVer('0.0.0')
  if (range.test(minver)) {
    return minver
  }

  minver = new SemVer('0.0.0-0')
  if (range.test(minver)) {
    return minver
  }

  minver = null
  for (var i = 0; i < range.set.length; ++i) {
    var comparators = range.set[i]

    comparators.forEach(function (comparator) {
      // Clone to avoid manipulating the comparator's semver object.
      var compver = new SemVer(comparator.semver.version)
      switch (comparator.operator) {
        case '>':
          if (compver.prerelease.length === 0) {
            compver.patch++
          } else {
            compver.prerelease.push(0)
          }
          compver.raw = compver.format()
          /* fallthrough */
        case '':
        case '>=':
          if (!minver || gt(minver, compver)) {
            minver = compver
          }
          break
        case '<':
        case '<=':
          /* Ignore maximum versions */
          break
        /* istanbul ignore next */
        default:
          throw new Error('Unexpected operation: ' + comparator.operator)
      }
    })
  }

  if (minver && range.test(minver)) {
    return minver
  }

  return null
}

exports.validRange = validRange
function validRange (range, options) {
  try {
    // Return '*' instead of '' so that truthiness works.
    // This will throw if it's invalid anyway
    return new Range(range, options).range || '*'
  } catch (er) {
    return null
  }
}

// Determine if version is less than all the versions possible in the range
exports.ltr = ltr
function ltr (version, range, options) {
  return outside(version, range, '<', options)
}

// Determine if version is greater than all the versions possible in the range.
exports.gtr = gtr
function gtr (version, range, options) {
  return outside(version, range, '>', options)
}

exports.outside = outside
function outside (version, range, hilo, options) {
  version = new SemVer(version, options)
  range = new Range(range, options)

  var gtfn, ltefn, ltfn, comp, ecomp
  switch (hilo) {
    case '>':
      gtfn = gt
      ltefn = lte
      ltfn = lt
      comp = '>'
      ecomp = '>='
      break
    case '<':
      gtfn = lt
      ltefn = gte
      ltfn = gt
      comp = '<'
      ecomp = '<='
      break
    default:
      throw new TypeError('Must provide a hilo val of "<" or ">"')
  }

  // If it satisifes the range it is not outside
  if (satisfies(version, range, options)) {
    return false
  }

  // From now on, variable terms are as if we're in "gtr" mode.
  // but note that everything is flipped for the "ltr" function.

  for (var i = 0; i < range.set.length; ++i) {
    var comparators = range.set[i]

    var high = null
    var low = null

    comparators.forEach(function (comparator) {
      if (comparator.semver === ANY) {
        comparator = new Comparator('>=0.0.0')
      }
      high = high || comparator
      low = low || comparator
      if (gtfn(comparator.semver, high.semver, options)) {
        high = comparator
      } else if (ltfn(comparator.semver, low.semver, options)) {
        low = comparator
      }
    })

    // If the edge version comparator has a operator then our version
    // isn't outside it
    if (high.operator === comp || high.operator === ecomp) {
      return false
    }

    // If the lowest version comparator has an operator and our version
    // is less than it then it isn't higher than the range
    if ((!low.operator || low.operator === comp) &&
        ltefn(version, low.semver)) {
      return false
    } else if (low.operator === ecomp && ltfn(version, low.semver)) {
      return false
    }
  }
  return true
}

exports.prerelease = prerelease
function prerelease (version, options) {
  var parsed = parse(version, options)
  return (parsed && parsed.prerelease.length) ? parsed.prerelease : null
}

exports.intersects = intersects
function intersects (r1, r2, options) {
  r1 = new Range(r1, options)
  r2 = new Range(r2, options)
  return r1.intersects(r2)
}

exports.coerce = coerce
function coerce (version) {
  if (version instanceof SemVer) {
    return version
  }

  if (typeof version !== 'string') {
    return null
  }

  var match = version.match(re[COERCE])

  if (match == null) {
    return null
  }

  return parse(match[1] +
    '.' + (match[2] || '0') +
    '.' + (match[3] || '0'))
}


/***/ }),

/***/ 126:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

Object.defineProperty(exports, "__esModule", ({ value: true }));
const events_1 = __webpack_require__(680);
const Is = __webpack_require__(593);
var CancellationToken;
(function (CancellationToken) {
    CancellationToken.None = Object.freeze({
        isCancellationRequested: false,
        onCancellationRequested: events_1.Event.None
    });
    CancellationToken.Cancelled = Object.freeze({
        isCancellationRequested: true,
        onCancellationRequested: events_1.Event.None
    });
    function is(value) {
        let candidate = value;
        return candidate && (candidate === CancellationToken.None
            || candidate === CancellationToken.Cancelled
            || (Is.boolean(candidate.isCancellationRequested) && !!candidate.onCancellationRequested));
    }
    CancellationToken.is = is;
})(CancellationToken = exports.CancellationToken || (exports.CancellationToken = {}));
const shortcutEvent = Object.freeze(function (callback, context) {
    let handle = setTimeout(callback.bind(context), 0);
    return { dispose() { clearTimeout(handle); } };
});
class MutableToken {
    constructor() {
        this._isCancelled = false;
    }
    cancel() {
        if (!this._isCancelled) {
            this._isCancelled = true;
            if (this._emitter) {
                this._emitter.fire(undefined);
                this._emitter = undefined;
            }
        }
    }
    get isCancellationRequested() {
        return this._isCancelled;
    }
    get onCancellationRequested() {
        if (this._isCancelled) {
            return shortcutEvent;
        }
        if (!this._emitter) {
            this._emitter = new events_1.Emitter();
        }
        return this._emitter.event;
    }
}
class CancellationTokenSource {
    get token() {
        if (!this._token) {
            // be lazy and create the token only when
            // actually needed
            this._token = new MutableToken();
        }
        return this._token;
    }
    cancel() {
        if (!this._token) {
            // save an object by returning the default
            // cancelled token when cancellation happens
            // before someone asks for the token
            this._token = CancellationToken.Cancelled;
        }
        else {
            this._token.cancel();
        }
    }
    dispose() {
        this.cancel();
    }
}
exports.CancellationTokenSource = CancellationTokenSource;


/***/ }),

/***/ 680:
/***/ ((__unused_webpack_module, exports) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
var Disposable;
(function (Disposable) {
    function create(func) {
        return {
            dispose: func
        };
    }
    Disposable.create = create;
})(Disposable = exports.Disposable || (exports.Disposable = {}));
var Event;
(function (Event) {
    const _disposable = { dispose() { } };
    Event.None = function () { return _disposable; };
})(Event = exports.Event || (exports.Event = {}));
class CallbackList {
    add(callback, context = null, bucket) {
        if (!this._callbacks) {
            this._callbacks = [];
            this._contexts = [];
        }
        this._callbacks.push(callback);
        this._contexts.push(context);
        if (Array.isArray(bucket)) {
            bucket.push({ dispose: () => this.remove(callback, context) });
        }
    }
    remove(callback, context = null) {
        if (!this._callbacks) {
            return;
        }
        var foundCallbackWithDifferentContext = false;
        for (var i = 0, len = this._callbacks.length; i < len; i++) {
            if (this._callbacks[i] === callback) {
                if (this._contexts[i] === context) {
                    // callback & context match => remove it
                    this._callbacks.splice(i, 1);
                    this._contexts.splice(i, 1);
                    return;
                }
                else {
                    foundCallbackWithDifferentContext = true;
                }
            }
        }
        if (foundCallbackWithDifferentContext) {
            throw new Error('When adding a listener with a context, you should remove it with the same context');
        }
    }
    invoke(...args) {
        if (!this._callbacks) {
            return [];
        }
        var ret = [], callbacks = this._callbacks.slice(0), contexts = this._contexts.slice(0);
        for (var i = 0, len = callbacks.length; i < len; i++) {
            try {
                ret.push(callbacks[i].apply(contexts[i], args));
            }
            catch (e) {
                console.error(e);
            }
        }
        return ret;
    }
    isEmpty() {
        return !this._callbacks || this._callbacks.length === 0;
    }
    dispose() {
        this._callbacks = undefined;
        this._contexts = undefined;
    }
}
class Emitter {
    constructor(_options) {
        this._options = _options;
    }
    /**
     * For the public to allow to subscribe
     * to events from this Emitter
     */
    get event() {
        if (!this._event) {
            this._event = (listener, thisArgs, disposables) => {
                if (!this._callbacks) {
                    this._callbacks = new CallbackList();
                }
                if (this._options && this._options.onFirstListenerAdd && this._callbacks.isEmpty()) {
                    this._options.onFirstListenerAdd(this);
                }
                this._callbacks.add(listener, thisArgs);
                let result;
                result = {
                    dispose: () => {
                        this._callbacks.remove(listener, thisArgs);
                        result.dispose = Emitter._noop;
                        if (this._options && this._options.onLastListenerRemove && this._callbacks.isEmpty()) {
                            this._options.onLastListenerRemove(this);
                        }
                    }
                };
                if (Array.isArray(disposables)) {
                    disposables.push(result);
                }
                return result;
            };
        }
        return this._event;
    }
    /**
     * To be kept private to fire an event to
     * subscribers
     */
    fire(event) {
        if (this._callbacks) {
            this._callbacks.invoke.call(this._callbacks, event);
        }
    }
    dispose() {
        if (this._callbacks) {
            this._callbacks.dispose();
            this._callbacks = undefined;
        }
    }
}
Emitter._noop = function () { };
exports.Emitter = Emitter;


/***/ }),

/***/ 593:
/***/ ((__unused_webpack_module, exports) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
function boolean(value) {
    return value === true || value === false;
}
exports.boolean = boolean;
function string(value) {
    return typeof value === 'string' || value instanceof String;
}
exports.string = string;
function number(value) {
    return typeof value === 'number' || value instanceof Number;
}
exports.number = number;
function error(value) {
    return value instanceof Error;
}
exports.error = error;
function func(value) {
    return typeof value === 'function';
}
exports.func = func;
function array(value) {
    return Array.isArray(value);
}
exports.array = array;
function stringArray(value) {
    return array(value) && value.every(elem => string(elem));
}
exports.stringArray = stringArray;


/***/ }),

/***/ 397:
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
var Touch;
(function (Touch) {
    Touch.None = 0;
    Touch.First = 1;
    Touch.Last = 2;
})(Touch = exports.Touch || (exports.Touch = {}));
class LinkedMap {
    constructor() {
        this._map = new Map();
        this._head = undefined;
        this._tail = undefined;
        this._size = 0;
    }
    clear() {
        this._map.clear();
        this._head = undefined;
        this._tail = undefined;
        this._size = 0;
    }
    isEmpty() {
        return !this._head && !this._tail;
    }
    get size() {
        return this._size;
    }
    has(key) {
        return this._map.has(key);
    }
    get(key) {
        const item = this._map.get(key);
        if (!item) {
            return undefined;
        }
        return item.value;
    }
    set(key, value, touch = Touch.None) {
        let item = this._map.get(key);
        if (item) {
            item.value = value;
            if (touch !== Touch.None) {
                this.touch(item, touch);
            }
        }
        else {
            item = { key, value, next: undefined, previous: undefined };
            switch (touch) {
                case Touch.None:
                    this.addItemLast(item);
                    break;
                case Touch.First:
                    this.addItemFirst(item);
                    break;
                case Touch.Last:
                    this.addItemLast(item);
                    break;
                default:
                    this.addItemLast(item);
                    break;
            }
            this._map.set(key, item);
            this._size++;
        }
    }
    delete(key) {
        const item = this._map.get(key);
        if (!item) {
            return false;
        }
        this._map.delete(key);
        this.removeItem(item);
        this._size--;
        return true;
    }
    shift() {
        if (!this._head && !this._tail) {
            return undefined;
        }
        if (!this._head || !this._tail) {
            throw new Error('Invalid list');
        }
        const item = this._head;
        this._map.delete(item.key);
        this.removeItem(item);
        this._size--;
        return item.value;
    }
    forEach(callbackfn, thisArg) {
        let current = this._head;
        while (current) {
            if (thisArg) {
                callbackfn.bind(thisArg)(current.value, current.key, this);
            }
            else {
                callbackfn(current.value, current.key, this);
            }
            current = current.next;
        }
    }
    forEachReverse(callbackfn, thisArg) {
        let current = this._tail;
        while (current) {
            if (thisArg) {
                callbackfn.bind(thisArg)(current.value, current.key, this);
            }
            else {
                callbackfn(current.value, current.key, this);
            }
            current = current.previous;
        }
    }
    values() {
        let result = [];
        let current = this._head;
        while (current) {
            result.push(current.value);
            current = current.next;
        }
        return result;
    }
    keys() {
        let result = [];
        let current = this._head;
        while (current) {
            result.push(current.key);
            current = current.next;
        }
        return result;
    }
    /* JSON RPC run on es5 which has no Symbol.iterator
    public keys(): IterableIterator<K> {
        let current = this._head;
        let iterator: IterableIterator<K> = {
            [Symbol.iterator]() {
                return iterator;
            },
            next():IteratorResult<K> {
                if (current) {
                    let result = { value: current.key, done: false };
                    current = current.next;
                    return result;
                } else {
                    return { value: undefined, done: true };
                }
            }
        };
        return iterator;
    }

    public values(): IterableIterator<V> {
        let current = this._head;
        let iterator: IterableIterator<V> = {
            [Symbol.iterator]() {
                return iterator;
            },
            next():IteratorResult<V> {
                if (current) {
                    let result = { value: current.value, done: false };
                    current = current.next;
                    return result;
                } else {
                    return { value: undefined, done: true };
                }
            }
        };
        return iterator;
    }
    */
    addItemFirst(item) {
        // First time Insert
        if (!this._head && !this._tail) {
            this._tail = item;
        }
        else if (!this._head) {
            throw new Error('Invalid list');
        }
        else {
            item.next = this._head;
            this._head.previous = item;
        }
        this._head = item;
    }
    addItemLast(item) {
        // First time Insert
        if (!this._head && !this._tail) {
            this._head = item;
        }
        else if (!this._tail) {
            throw new Error('Invalid list');
        }
        else {
            item.previous = this._tail;
            this._tail.next = item;
        }
        this._tail = item;
    }
    removeItem(item) {
        if (item === this._head && item === this._tail) {
            this._head = undefined;
            this._tail = undefined;
        }
        else if (item === this._head) {
            this._head = item.next;
        }
        else if (item === this._tail) {
            this._tail = item.previous;
        }
        else {
            const next = item.next;
            const previous = item.previous;
            if (!next || !previous) {
                throw new Error('Invalid list');
            }
            next.previous = previous;
            previous.next = next;
        }
    }
    touch(item, touch) {
        if (!this._head || !this._tail) {
            throw new Error('Invalid list');
        }
        if ((touch !== Touch.First && touch !== Touch.Last)) {
            return;
        }
        if (touch === Touch.First) {
            if (item === this._head) {
                return;
            }
            const next = item.next;
            const previous = item.previous;
            // Unlink the item
            if (item === this._tail) {
                // previous must be defined since item was not head but is tail
                // So there are more than on item in the map
                previous.next = undefined;
                this._tail = previous;
            }
            else {
                // Both next and previous are not undefined since item was neither head nor tail.
                next.previous = previous;
                previous.next = next;
            }
            // Insert the node at head
            item.previous = undefined;
            item.next = this._head;
            this._head.previous = item;
            this._head = item;
        }
        else if (touch === Touch.Last) {
            if (item === this._tail) {
                return;
            }
            const next = item.next;
            const previous = item.previous;
            // Unlink the item.
            if (item === this._head) {
                // next must be defined since item was not tail but is head
                // So there are more than on item in the map
                next.previous = undefined;
                this._head = next;
            }
            else {
                // Both next and previous are not undefined since item was neither head nor tail.
                next.previous = previous;
                previous.next = next;
            }
            item.next = undefined;
            item.previous = this._tail;
            this._tail.next = item;
            this._tail = item;
        }
    }
}
exports.LinkedMap = LinkedMap;


/***/ }),

/***/ 231:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
/// <reference path="./thenable.ts" />

function __export(m) {
    for (var p in m) if (!exports.hasOwnProperty(p)) exports[p] = m[p];
}
Object.defineProperty(exports, "__esModule", ({ value: true }));
const Is = __webpack_require__(593);
const messages_1 = __webpack_require__(234);
exports.RequestType = messages_1.RequestType;
exports.RequestType0 = messages_1.RequestType0;
exports.RequestType1 = messages_1.RequestType1;
exports.RequestType2 = messages_1.RequestType2;
exports.RequestType3 = messages_1.RequestType3;
exports.RequestType4 = messages_1.RequestType4;
exports.RequestType5 = messages_1.RequestType5;
exports.RequestType6 = messages_1.RequestType6;
exports.RequestType7 = messages_1.RequestType7;
exports.RequestType8 = messages_1.RequestType8;
exports.RequestType9 = messages_1.RequestType9;
exports.ResponseError = messages_1.ResponseError;
exports.ErrorCodes = messages_1.ErrorCodes;
exports.NotificationType = messages_1.NotificationType;
exports.NotificationType0 = messages_1.NotificationType0;
exports.NotificationType1 = messages_1.NotificationType1;
exports.NotificationType2 = messages_1.NotificationType2;
exports.NotificationType3 = messages_1.NotificationType3;
exports.NotificationType4 = messages_1.NotificationType4;
exports.NotificationType5 = messages_1.NotificationType5;
exports.NotificationType6 = messages_1.NotificationType6;
exports.NotificationType7 = messages_1.NotificationType7;
exports.NotificationType8 = messages_1.NotificationType8;
exports.NotificationType9 = messages_1.NotificationType9;
const messageReader_1 = __webpack_require__(386);
exports.MessageReader = messageReader_1.MessageReader;
exports.StreamMessageReader = messageReader_1.StreamMessageReader;
exports.IPCMessageReader = messageReader_1.IPCMessageReader;
exports.SocketMessageReader = messageReader_1.SocketMessageReader;
const messageWriter_1 = __webpack_require__(725);
exports.MessageWriter = messageWriter_1.MessageWriter;
exports.StreamMessageWriter = messageWriter_1.StreamMessageWriter;
exports.IPCMessageWriter = messageWriter_1.IPCMessageWriter;
exports.SocketMessageWriter = messageWriter_1.SocketMessageWriter;
const events_1 = __webpack_require__(680);
exports.Disposable = events_1.Disposable;
exports.Event = events_1.Event;
exports.Emitter = events_1.Emitter;
const cancellation_1 = __webpack_require__(126);
exports.CancellationTokenSource = cancellation_1.CancellationTokenSource;
exports.CancellationToken = cancellation_1.CancellationToken;
const linkedMap_1 = __webpack_require__(397);
__export(__webpack_require__(377));
__export(__webpack_require__(609));
var CancelNotification;
(function (CancelNotification) {
    CancelNotification.type = new messages_1.NotificationType('$/cancelRequest');
})(CancelNotification || (CancelNotification = {}));
exports.NullLogger = Object.freeze({
    error: () => { },
    warn: () => { },
    info: () => { },
    log: () => { }
});
var Trace;
(function (Trace) {
    Trace[Trace["Off"] = 0] = "Off";
    Trace[Trace["Messages"] = 1] = "Messages";
    Trace[Trace["Verbose"] = 2] = "Verbose";
})(Trace = exports.Trace || (exports.Trace = {}));
(function (Trace) {
    function fromString(value) {
        value = value.toLowerCase();
        switch (value) {
            case 'off':
                return Trace.Off;
            case 'messages':
                return Trace.Messages;
            case 'verbose':
                return Trace.Verbose;
            default:
                return Trace.Off;
        }
    }
    Trace.fromString = fromString;
    function toString(value) {
        switch (value) {
            case Trace.Off:
                return 'off';
            case Trace.Messages:
                return 'messages';
            case Trace.Verbose:
                return 'verbose';
            default:
                return 'off';
        }
    }
    Trace.toString = toString;
})(Trace = exports.Trace || (exports.Trace = {}));
var TraceFormat;
(function (TraceFormat) {
    TraceFormat["Text"] = "text";
    TraceFormat["JSON"] = "json";
})(TraceFormat = exports.TraceFormat || (exports.TraceFormat = {}));
(function (TraceFormat) {
    function fromString(value) {
        value = value.toLowerCase();
        if (value === 'json') {
            return TraceFormat.JSON;
        }
        else {
            return TraceFormat.Text;
        }
    }
    TraceFormat.fromString = fromString;
})(TraceFormat = exports.TraceFormat || (exports.TraceFormat = {}));
var SetTraceNotification;
(function (SetTraceNotification) {
    SetTraceNotification.type = new messages_1.NotificationType('$/setTraceNotification');
})(SetTraceNotification = exports.SetTraceNotification || (exports.SetTraceNotification = {}));
var LogTraceNotification;
(function (LogTraceNotification) {
    LogTraceNotification.type = new messages_1.NotificationType('$/logTraceNotification');
})(LogTraceNotification = exports.LogTraceNotification || (exports.LogTraceNotification = {}));
var ConnectionErrors;
(function (ConnectionErrors) {
    /**
     * The connection is closed.
     */
    ConnectionErrors[ConnectionErrors["Closed"] = 1] = "Closed";
    /**
     * The connection got disposed.
     */
    ConnectionErrors[ConnectionErrors["Disposed"] = 2] = "Disposed";
    /**
     * The connection is already in listening mode.
     */
    ConnectionErrors[ConnectionErrors["AlreadyListening"] = 3] = "AlreadyListening";
})(ConnectionErrors = exports.ConnectionErrors || (exports.ConnectionErrors = {}));
class ConnectionError extends Error {
    constructor(code, message) {
        super(message);
        this.code = code;
        Object.setPrototypeOf(this, ConnectionError.prototype);
    }
}
exports.ConnectionError = ConnectionError;
var ConnectionStrategy;
(function (ConnectionStrategy) {
    function is(value) {
        let candidate = value;
        return candidate && Is.func(candidate.cancelUndispatched);
    }
    ConnectionStrategy.is = is;
})(ConnectionStrategy = exports.ConnectionStrategy || (exports.ConnectionStrategy = {}));
var ConnectionState;
(function (ConnectionState) {
    ConnectionState[ConnectionState["New"] = 1] = "New";
    ConnectionState[ConnectionState["Listening"] = 2] = "Listening";
    ConnectionState[ConnectionState["Closed"] = 3] = "Closed";
    ConnectionState[ConnectionState["Disposed"] = 4] = "Disposed";
})(ConnectionState || (ConnectionState = {}));
function _createMessageConnection(messageReader, messageWriter, logger, strategy) {
    let sequenceNumber = 0;
    let notificationSquenceNumber = 0;
    let unknownResponseSquenceNumber = 0;
    const version = '2.0';
    let starRequestHandler = undefined;
    let requestHandlers = Object.create(null);
    let starNotificationHandler = undefined;
    let notificationHandlers = Object.create(null);
    let timer;
    let messageQueue = new linkedMap_1.LinkedMap();
    let responsePromises = Object.create(null);
    let requestTokens = Object.create(null);
    let trace = Trace.Off;
    let traceFormat = TraceFormat.Text;
    let tracer;
    let state = ConnectionState.New;
    let errorEmitter = new events_1.Emitter();
    let closeEmitter = new events_1.Emitter();
    let unhandledNotificationEmitter = new events_1.Emitter();
    let disposeEmitter = new events_1.Emitter();
    function createRequestQueueKey(id) {
        return 'req-' + id.toString();
    }
    function createResponseQueueKey(id) {
        if (id === null) {
            return 'res-unknown-' + (++unknownResponseSquenceNumber).toString();
        }
        else {
            return 'res-' + id.toString();
        }
    }
    function createNotificationQueueKey() {
        return 'not-' + (++notificationSquenceNumber).toString();
    }
    function addMessageToQueue(queue, message) {
        if (messages_1.isRequestMessage(message)) {
            queue.set(createRequestQueueKey(message.id), message);
        }
        else if (messages_1.isResponseMessage(message)) {
            queue.set(createResponseQueueKey(message.id), message);
        }
        else {
            queue.set(createNotificationQueueKey(), message);
        }
    }
    function cancelUndispatched(_message) {
        return undefined;
    }
    function isListening() {
        return state === ConnectionState.Listening;
    }
    function isClosed() {
        return state === ConnectionState.Closed;
    }
    function isDisposed() {
        return state === ConnectionState.Disposed;
    }
    function closeHandler() {
        if (state === ConnectionState.New || state === ConnectionState.Listening) {
            state = ConnectionState.Closed;
            closeEmitter.fire(undefined);
        }
        // If the connection is disposed don't sent close events.
    }
    ;
    function readErrorHandler(error) {
        errorEmitter.fire([error, undefined, undefined]);
    }
    function writeErrorHandler(data) {
        errorEmitter.fire(data);
    }
    messageReader.onClose(closeHandler);
    messageReader.onError(readErrorHandler);
    messageWriter.onClose(closeHandler);
    messageWriter.onError(writeErrorHandler);
    function triggerMessageQueue() {
        if (timer || messageQueue.size === 0) {
            return;
        }
        timer = setImmediate(() => {
            timer = undefined;
            processMessageQueue();
        });
    }
    function processMessageQueue() {
        if (messageQueue.size === 0) {
            return;
        }
        let message = messageQueue.shift();
        try {
            if (messages_1.isRequestMessage(message)) {
                handleRequest(message);
            }
            else if (messages_1.isNotificationMessage(message)) {
                handleNotification(message);
            }
            else if (messages_1.isResponseMessage(message)) {
                handleResponse(message);
            }
            else {
                handleInvalidMessage(message);
            }
        }
        finally {
            triggerMessageQueue();
        }
    }
    let callback = (message) => {
        try {
            // We have received a cancellation message. Check if the message is still in the queue
            // and cancel it if allowed to do so.
            if (messages_1.isNotificationMessage(message) && message.method === CancelNotification.type.method) {
                let key = createRequestQueueKey(message.params.id);
                let toCancel = messageQueue.get(key);
                if (messages_1.isRequestMessage(toCancel)) {
                    let response = strategy && strategy.cancelUndispatched ? strategy.cancelUndispatched(toCancel, cancelUndispatched) : cancelUndispatched(toCancel);
                    if (response && (response.error !== void 0 || response.result !== void 0)) {
                        messageQueue.delete(key);
                        response.id = toCancel.id;
                        traceSendingResponse(response, message.method, Date.now());
                        messageWriter.write(response);
                        return;
                    }
                }
            }
            addMessageToQueue(messageQueue, message);
        }
        finally {
            triggerMessageQueue();
        }
    };
    function handleRequest(requestMessage) {
        if (isDisposed()) {
            // we return here silently since we fired an event when the
            // connection got disposed.
            return;
        }
        function reply(resultOrError, method, startTime) {
            let message = {
                jsonrpc: version,
                id: requestMessage.id
            };
            if (resultOrError instanceof messages_1.ResponseError) {
                message.error = resultOrError.toJson();
            }
            else {
                message.result = resultOrError === void 0 ? null : resultOrError;
            }
            traceSendingResponse(message, method, startTime);
            messageWriter.write(message);
        }
        function replyError(error, method, startTime) {
            let message = {
                jsonrpc: version,
                id: requestMessage.id,
                error: error.toJson()
            };
            traceSendingResponse(message, method, startTime);
            messageWriter.write(message);
        }
        function replySuccess(result, method, startTime) {
            // The JSON RPC defines that a response must either have a result or an error
            // So we can't treat undefined as a valid response result.
            if (result === void 0) {
                result = null;
            }
            let message = {
                jsonrpc: version,
                id: requestMessage.id,
                result: result
            };
            traceSendingResponse(message, method, startTime);
            messageWriter.write(message);
        }
        traceReceivedRequest(requestMessage);
        let element = requestHandlers[requestMessage.method];
        let type;
        let requestHandler;
        if (element) {
            type = element.type;
            requestHandler = element.handler;
        }
        let startTime = Date.now();
        if (requestHandler || starRequestHandler) {
            let cancellationSource = new cancellation_1.CancellationTokenSource();
            let tokenKey = String(requestMessage.id);
            requestTokens[tokenKey] = cancellationSource;
            try {
                let handlerResult;
                if (requestMessage.params === void 0 || (type !== void 0 && type.numberOfParams === 0)) {
                    handlerResult = requestHandler
                        ? requestHandler(cancellationSource.token)
                        : starRequestHandler(requestMessage.method, cancellationSource.token);
                }
                else if (Is.array(requestMessage.params) && (type === void 0 || type.numberOfParams > 1)) {
                    handlerResult = requestHandler
                        ? requestHandler(...requestMessage.params, cancellationSource.token)
                        : starRequestHandler(requestMessage.method, ...requestMessage.params, cancellationSource.token);
                }
                else {
                    handlerResult = requestHandler
                        ? requestHandler(requestMessage.params, cancellationSource.token)
                        : starRequestHandler(requestMessage.method, requestMessage.params, cancellationSource.token);
                }
                let promise = handlerResult;
                if (!handlerResult) {
                    delete requestTokens[tokenKey];
                    replySuccess(handlerResult, requestMessage.method, startTime);
                }
                else if (promise.then) {
                    promise.then((resultOrError) => {
                        delete requestTokens[tokenKey];
                        reply(resultOrError, requestMessage.method, startTime);
                    }, error => {
                        delete requestTokens[tokenKey];
                        if (error instanceof messages_1.ResponseError) {
                            replyError(error, requestMessage.method, startTime);
                        }
                        else if (error && Is.string(error.message)) {
                            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed with message: ${error.message}`), requestMessage.method, startTime);
                        }
                        else {
                            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed unexpectedly without providing any details.`), requestMessage.method, startTime);
                        }
                    });
                }
                else {
                    delete requestTokens[tokenKey];
                    reply(handlerResult, requestMessage.method, startTime);
                }
            }
            catch (error) {
                delete requestTokens[tokenKey];
                if (error instanceof messages_1.ResponseError) {
                    reply(error, requestMessage.method, startTime);
                }
                else if (error && Is.string(error.message)) {
                    replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed with message: ${error.message}`), requestMessage.method, startTime);
                }
                else {
                    replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed unexpectedly without providing any details.`), requestMessage.method, startTime);
                }
            }
        }
        else {
            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.MethodNotFound, `Unhandled method ${requestMessage.method}`), requestMessage.method, startTime);
        }
    }
    function handleResponse(responseMessage) {
        if (isDisposed()) {
            // See handle request.
            return;
        }
        if (responseMessage.id === null) {
            if (responseMessage.error) {
                logger.error(`Received response message without id: Error is: \n${JSON.stringify(responseMessage.error, undefined, 4)}`);
            }
            else {
                logger.error(`Received response message without id. No further error information provided.`);
            }
        }
        else {
            let key = String(responseMessage.id);
            let responsePromise = responsePromises[key];
            traceReceivedResponse(responseMessage, responsePromise);
            if (responsePromise) {
                delete responsePromises[key];
                try {
                    if (responseMessage.error) {
                        let error = responseMessage.error;
                        responsePromise.reject(new messages_1.ResponseError(error.code, error.message, error.data));
                    }
                    else if (responseMessage.result !== void 0) {
                        responsePromise.resolve(responseMessage.result);
                    }
                    else {
                        throw new Error('Should never happen.');
                    }
                }
                catch (error) {
                    if (error.message) {
                        logger.error(`Response handler '${responsePromise.method}' failed with message: ${error.message}`);
                    }
                    else {
                        logger.error(`Response handler '${responsePromise.method}' failed unexpectedly.`);
                    }
                }
            }
        }
    }
    function handleNotification(message) {
        if (isDisposed()) {
            // See handle request.
            return;
        }
        let type = undefined;
        let notificationHandler;
        if (message.method === CancelNotification.type.method) {
            notificationHandler = (params) => {
                let id = params.id;
                let source = requestTokens[String(id)];
                if (source) {
                    source.cancel();
                }
            };
        }
        else {
            let element = notificationHandlers[message.method];
            if (element) {
                notificationHandler = element.handler;
                type = element.type;
            }
        }
        if (notificationHandler || starNotificationHandler) {
            try {
                traceReceivedNotification(message);
                if (message.params === void 0 || (type !== void 0 && type.numberOfParams === 0)) {
                    notificationHandler ? notificationHandler() : starNotificationHandler(message.method);
                }
                else if (Is.array(message.params) && (type === void 0 || type.numberOfParams > 1)) {
                    notificationHandler ? notificationHandler(...message.params) : starNotificationHandler(message.method, ...message.params);
                }
                else {
                    notificationHandler ? notificationHandler(message.params) : starNotificationHandler(message.method, message.params);
                }
            }
            catch (error) {
                if (error.message) {
                    logger.error(`Notification handler '${message.method}' failed with message: ${error.message}`);
                }
                else {
                    logger.error(`Notification handler '${message.method}' failed unexpectedly.`);
                }
            }
        }
        else {
            unhandledNotificationEmitter.fire(message);
        }
    }
    function handleInvalidMessage(message) {
        if (!message) {
            logger.error('Received empty message.');
            return;
        }
        logger.error(`Received message which is neither a response nor a notification message:\n${JSON.stringify(message, null, 4)}`);
        // Test whether we find an id to reject the promise
        let responseMessage = message;
        if (Is.string(responseMessage.id) || Is.number(responseMessage.id)) {
            let key = String(responseMessage.id);
            let responseHandler = responsePromises[key];
            if (responseHandler) {
                responseHandler.reject(new Error('The received response has neither a result nor an error property.'));
            }
        }
    }
    function traceSendingRequest(message) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose && message.params) {
                data = `Params: ${JSON.stringify(message.params, null, 4)}\n\n`;
            }
            tracer.log(`Sending request '${message.method} - (${message.id})'.`, data);
        }
        else {
            logLSPMessage('send-request', message);
        }
    }
    function traceSendingNotification(message) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose) {
                if (message.params) {
                    data = `Params: ${JSON.stringify(message.params, null, 4)}\n\n`;
                }
                else {
                    data = 'No parameters provided.\n\n';
                }
            }
            tracer.log(`Sending notification '${message.method}'.`, data);
        }
        else {
            logLSPMessage('send-notification', message);
        }
    }
    function traceSendingResponse(message, method, startTime) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose) {
                if (message.error && message.error.data) {
                    data = `Error data: ${JSON.stringify(message.error.data, null, 4)}\n\n`;
                }
                else {
                    if (message.result) {
                        data = `Result: ${JSON.stringify(message.result, null, 4)}\n\n`;
                    }
                    else if (message.error === void 0) {
                        data = 'No result returned.\n\n';
                    }
                }
            }
            tracer.log(`Sending response '${method} - (${message.id})'. Processing request took ${Date.now() - startTime}ms`, data);
        }
        else {
            logLSPMessage('send-response', message);
        }
    }
    function traceReceivedRequest(message) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose && message.params) {
                data = `Params: ${JSON.stringify(message.params, null, 4)}\n\n`;
            }
            tracer.log(`Received request '${message.method} - (${message.id})'.`, data);
        }
        else {
            logLSPMessage('receive-request', message);
        }
    }
    function traceReceivedNotification(message) {
        if (trace === Trace.Off || !tracer || message.method === LogTraceNotification.type.method) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose) {
                if (message.params) {
                    data = `Params: ${JSON.stringify(message.params, null, 4)}\n\n`;
                }
                else {
                    data = 'No parameters provided.\n\n';
                }
            }
            tracer.log(`Received notification '${message.method}'.`, data);
        }
        else {
            logLSPMessage('receive-notification', message);
        }
    }
    function traceReceivedResponse(message, responsePromise) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose) {
                if (message.error && message.error.data) {
                    data = `Error data: ${JSON.stringify(message.error.data, null, 4)}\n\n`;
                }
                else {
                    if (message.result) {
                        data = `Result: ${JSON.stringify(message.result, null, 4)}\n\n`;
                    }
                    else if (message.error === void 0) {
                        data = 'No result returned.\n\n';
                    }
                }
            }
            if (responsePromise) {
                let error = message.error ? ` Request failed: ${message.error.message} (${message.error.code}).` : '';
                tracer.log(`Received response '${responsePromise.method} - (${message.id})' in ${Date.now() - responsePromise.timerStart}ms.${error}`, data);
            }
            else {
                tracer.log(`Received response ${message.id} without active response promise.`, data);
            }
        }
        else {
            logLSPMessage('receive-response', message);
        }
    }
    function logLSPMessage(type, message) {
        if (!tracer || trace === Trace.Off) {
            return;
        }
        const lspMessage = {
            isLSPMessage: true,
            type,
            message,
            timestamp: Date.now()
        };
        tracer.log(lspMessage);
    }
    function throwIfClosedOrDisposed() {
        if (isClosed()) {
            throw new ConnectionError(ConnectionErrors.Closed, 'Connection is closed.');
        }
        if (isDisposed()) {
            throw new ConnectionError(ConnectionErrors.Disposed, 'Connection is disposed.');
        }
    }
    function throwIfListening() {
        if (isListening()) {
            throw new ConnectionError(ConnectionErrors.AlreadyListening, 'Connection is already listening');
        }
    }
    function throwIfNotListening() {
        if (!isListening()) {
            throw new Error('Call listen() first.');
        }
    }
    function undefinedToNull(param) {
        if (param === void 0) {
            return null;
        }
        else {
            return param;
        }
    }
    function computeMessageParams(type, params) {
        let result;
        let numberOfParams = type.numberOfParams;
        switch (numberOfParams) {
            case 0:
                result = null;
                break;
            case 1:
                result = undefinedToNull(params[0]);
                break;
            default:
                result = [];
                for (let i = 0; i < params.length && i < numberOfParams; i++) {
                    result.push(undefinedToNull(params[i]));
                }
                if (params.length < numberOfParams) {
                    for (let i = params.length; i < numberOfParams; i++) {
                        result.push(null);
                    }
                }
                break;
        }
        return result;
    }
    let connection = {
        sendNotification: (type, ...params) => {
            throwIfClosedOrDisposed();
            let method;
            let messageParams;
            if (Is.string(type)) {
                method = type;
                switch (params.length) {
                    case 0:
                        messageParams = null;
                        break;
                    case 1:
                        messageParams = params[0];
                        break;
                    default:
                        messageParams = params;
                        break;
                }
            }
            else {
                method = type.method;
                messageParams = computeMessageParams(type, params);
            }
            let notificationMessage = {
                jsonrpc: version,
                method: method,
                params: messageParams
            };
            traceSendingNotification(notificationMessage);
            messageWriter.write(notificationMessage);
        },
        onNotification: (type, handler) => {
            throwIfClosedOrDisposed();
            if (Is.func(type)) {
                starNotificationHandler = type;
            }
            else if (handler) {
                if (Is.string(type)) {
                    notificationHandlers[type] = { type: undefined, handler };
                }
                else {
                    notificationHandlers[type.method] = { type, handler };
                }
            }
        },
        sendRequest: (type, ...params) => {
            throwIfClosedOrDisposed();
            throwIfNotListening();
            let method;
            let messageParams;
            let token = undefined;
            if (Is.string(type)) {
                method = type;
                switch (params.length) {
                    case 0:
                        messageParams = null;
                        break;
                    case 1:
                        // The cancellation token is optional so it can also be undefined.
                        if (cancellation_1.CancellationToken.is(params[0])) {
                            messageParams = null;
                            token = params[0];
                        }
                        else {
                            messageParams = undefinedToNull(params[0]);
                        }
                        break;
                    default:
                        const last = params.length - 1;
                        if (cancellation_1.CancellationToken.is(params[last])) {
                            token = params[last];
                            if (params.length === 2) {
                                messageParams = undefinedToNull(params[0]);
                            }
                            else {
                                messageParams = params.slice(0, last).map(value => undefinedToNull(value));
                            }
                        }
                        else {
                            messageParams = params.map(value => undefinedToNull(value));
                        }
                        break;
                }
            }
            else {
                method = type.method;
                messageParams = computeMessageParams(type, params);
                let numberOfParams = type.numberOfParams;
                token = cancellation_1.CancellationToken.is(params[numberOfParams]) ? params[numberOfParams] : undefined;
            }
            let id = sequenceNumber++;
            let result = new Promise((resolve, reject) => {
                let requestMessage = {
                    jsonrpc: version,
                    id: id,
                    method: method,
                    params: messageParams
                };
                let responsePromise = { method: method, timerStart: Date.now(), resolve, reject };
                traceSendingRequest(requestMessage);
                try {
                    messageWriter.write(requestMessage);
                }
                catch (e) {
                    // Writing the message failed. So we need to reject the promise.
                    responsePromise.reject(new messages_1.ResponseError(messages_1.ErrorCodes.MessageWriteError, e.message ? e.message : 'Unknown reason'));
                    responsePromise = null;
                }
                if (responsePromise) {
                    responsePromises[String(id)] = responsePromise;
                }
            });
            if (token) {
                token.onCancellationRequested(() => {
                    connection.sendNotification(CancelNotification.type, { id });
                });
            }
            return result;
        },
        onRequest: (type, handler) => {
            throwIfClosedOrDisposed();
            if (Is.func(type)) {
                starRequestHandler = type;
            }
            else if (handler) {
                if (Is.string(type)) {
                    requestHandlers[type] = { type: undefined, handler };
                }
                else {
                    requestHandlers[type.method] = { type, handler };
                }
            }
        },
        trace: (_value, _tracer, sendNotificationOrTraceOptions) => {
            let _sendNotification = false;
            let _traceFormat = TraceFormat.Text;
            if (sendNotificationOrTraceOptions !== void 0) {
                if (Is.boolean(sendNotificationOrTraceOptions)) {
                    _sendNotification = sendNotificationOrTraceOptions;
                }
                else {
                    _sendNotification = sendNotificationOrTraceOptions.sendNotification || false;
                    _traceFormat = sendNotificationOrTraceOptions.traceFormat || TraceFormat.Text;
                }
            }
            trace = _value;
            traceFormat = _traceFormat;
            if (trace === Trace.Off) {
                tracer = undefined;
            }
            else {
                tracer = _tracer;
            }
            if (_sendNotification && !isClosed() && !isDisposed()) {
                connection.sendNotification(SetTraceNotification.type, { value: Trace.toString(_value) });
            }
        },
        onError: errorEmitter.event,
        onClose: closeEmitter.event,
        onUnhandledNotification: unhandledNotificationEmitter.event,
        onDispose: disposeEmitter.event,
        dispose: () => {
            if (isDisposed()) {
                return;
            }
            state = ConnectionState.Disposed;
            disposeEmitter.fire(undefined);
            let error = new Error('Connection got disposed.');
            Object.keys(responsePromises).forEach((key) => {
                responsePromises[key].reject(error);
            });
            responsePromises = Object.create(null);
            requestTokens = Object.create(null);
            messageQueue = new linkedMap_1.LinkedMap();
            // Test for backwards compatibility
            if (Is.func(messageWriter.dispose)) {
                messageWriter.dispose();
            }
            if (Is.func(messageReader.dispose)) {
                messageReader.dispose();
            }
        },
        listen: () => {
            throwIfClosedOrDisposed();
            throwIfListening();
            state = ConnectionState.Listening;
            messageReader.listen(callback);
        },
        inspect: () => {
            console.log("inspect");
        }
    };
    connection.onNotification(LogTraceNotification.type, (params) => {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        tracer.log(params.message, trace === Trace.Verbose ? params.verbose : undefined);
    });
    return connection;
}
function isMessageReader(value) {
    return value.listen !== void 0 && value.read === void 0;
}
function isMessageWriter(value) {
    return value.write !== void 0 && value.end === void 0;
}
function createMessageConnection(input, output, logger, strategy) {
    if (!logger) {
        logger = exports.NullLogger;
    }
    let reader = isMessageReader(input) ? input : new messageReader_1.StreamMessageReader(input);
    let writer = isMessageWriter(output) ? output : new messageWriter_1.StreamMessageWriter(output);
    return _createMessageConnection(reader, writer, logger, strategy);
}
exports.createMessageConnection = createMessageConnection;


/***/ }),

/***/ 386:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const events_1 = __webpack_require__(680);
const Is = __webpack_require__(593);
let DefaultSize = 8192;
let CR = Buffer.from('\r', 'ascii')[0];
let LF = Buffer.from('\n', 'ascii')[0];
let CRLF = '\r\n';
class MessageBuffer {
    constructor(encoding = 'utf8') {
        this.encoding = encoding;
        this.index = 0;
        this.buffer = Buffer.allocUnsafe(DefaultSize);
    }
    append(chunk) {
        var toAppend = chunk;
        if (typeof (chunk) === 'string') {
            var str = chunk;
            var bufferLen = Buffer.byteLength(str, this.encoding);
            toAppend = Buffer.allocUnsafe(bufferLen);
            toAppend.write(str, 0, bufferLen, this.encoding);
        }
        if (this.buffer.length - this.index >= toAppend.length) {
            toAppend.copy(this.buffer, this.index, 0, toAppend.length);
        }
        else {
            var newSize = (Math.ceil((this.index + toAppend.length) / DefaultSize) + 1) * DefaultSize;
            if (this.index === 0) {
                this.buffer = Buffer.allocUnsafe(newSize);
                toAppend.copy(this.buffer, 0, 0, toAppend.length);
            }
            else {
                this.buffer = Buffer.concat([this.buffer.slice(0, this.index), toAppend], newSize);
            }
        }
        this.index += toAppend.length;
    }
    tryReadHeaders() {
        let result = undefined;
        let current = 0;
        while (current + 3 < this.index && (this.buffer[current] !== CR || this.buffer[current + 1] !== LF || this.buffer[current + 2] !== CR || this.buffer[current + 3] !== LF)) {
            current++;
        }
        // No header / body separator found (e.g CRLFCRLF)
        if (current + 3 >= this.index) {
            return result;
        }
        result = Object.create(null);
        let headers = this.buffer.toString('ascii', 0, current).split(CRLF);
        headers.forEach((header) => {
            let index = header.indexOf(':');
            if (index === -1) {
                throw new Error('Message header must separate key and value using :');
            }
            let key = header.substr(0, index);
            let value = header.substr(index + 1).trim();
            result[key] = value;
        });
        let nextStart = current + 4;
        this.buffer = this.buffer.slice(nextStart);
        this.index = this.index - nextStart;
        return result;
    }
    tryReadContent(length) {
        if (this.index < length) {
            return null;
        }
        let result = this.buffer.toString(this.encoding, 0, length);
        let nextStart = length;
        this.buffer.copy(this.buffer, 0, nextStart);
        this.index = this.index - nextStart;
        return result;
    }
    get numberOfBytes() {
        return this.index;
    }
}
var MessageReader;
(function (MessageReader) {
    function is(value) {
        let candidate = value;
        return candidate && Is.func(candidate.listen) && Is.func(candidate.dispose) &&
            Is.func(candidate.onError) && Is.func(candidate.onClose) && Is.func(candidate.onPartialMessage);
    }
    MessageReader.is = is;
})(MessageReader = exports.MessageReader || (exports.MessageReader = {}));
class AbstractMessageReader {
    constructor() {
        this.errorEmitter = new events_1.Emitter();
        this.closeEmitter = new events_1.Emitter();
        this.partialMessageEmitter = new events_1.Emitter();
    }
    dispose() {
        this.errorEmitter.dispose();
        this.closeEmitter.dispose();
    }
    get onError() {
        return this.errorEmitter.event;
    }
    fireError(error) {
        this.errorEmitter.fire(this.asError(error));
    }
    get onClose() {
        return this.closeEmitter.event;
    }
    fireClose() {
        this.closeEmitter.fire(undefined);
    }
    get onPartialMessage() {
        return this.partialMessageEmitter.event;
    }
    firePartialMessage(info) {
        this.partialMessageEmitter.fire(info);
    }
    asError(error) {
        if (error instanceof Error) {
            return error;
        }
        else {
            return new Error(`Reader recevied error. Reason: ${Is.string(error.message) ? error.message : 'unknown'}`);
        }
    }
}
exports.AbstractMessageReader = AbstractMessageReader;
class StreamMessageReader extends AbstractMessageReader {
    constructor(readable, encoding = 'utf8') {
        super();
        this.readable = readable;
        this.buffer = new MessageBuffer(encoding);
        this._partialMessageTimeout = 10000;
    }
    set partialMessageTimeout(timeout) {
        this._partialMessageTimeout = timeout;
    }
    get partialMessageTimeout() {
        return this._partialMessageTimeout;
    }
    listen(callback) {
        this.nextMessageLength = -1;
        this.messageToken = 0;
        this.partialMessageTimer = undefined;
        this.callback = callback;
        this.readable.on('data', (data) => {
            this.onData(data);
        });
        this.readable.on('error', (error) => this.fireError(error));
        this.readable.on('close', () => this.fireClose());
    }
    onData(data) {
        this.buffer.append(data);
        while (true) {
            if (this.nextMessageLength === -1) {
                let headers = this.buffer.tryReadHeaders();
                if (!headers) {
                    return;
                }
                let contentLength = headers['Content-Length'];
                if (!contentLength) {
                    throw new Error('Header must provide a Content-Length property.');
                }
                let length = parseInt(contentLength);
                if (isNaN(length)) {
                    throw new Error('Content-Length value must be a number.');
                }
                this.nextMessageLength = length;
                // Take the encoding form the header. For compatibility
                // treat both utf-8 and utf8 as node utf8
            }
            var msg = this.buffer.tryReadContent(this.nextMessageLength);
            if (msg === null) {
                /** We haven't recevied the full message yet. */
                this.setPartialMessageTimer();
                return;
            }
            this.clearPartialMessageTimer();
            this.nextMessageLength = -1;
            this.messageToken++;
            var json = JSON.parse(msg);
            this.callback(json);
        }
    }
    clearPartialMessageTimer() {
        if (this.partialMessageTimer) {
            clearTimeout(this.partialMessageTimer);
            this.partialMessageTimer = undefined;
        }
    }
    setPartialMessageTimer() {
        this.clearPartialMessageTimer();
        if (this._partialMessageTimeout <= 0) {
            return;
        }
        this.partialMessageTimer = setTimeout((token, timeout) => {
            this.partialMessageTimer = undefined;
            if (token === this.messageToken) {
                this.firePartialMessage({ messageToken: token, waitingTime: timeout });
                this.setPartialMessageTimer();
            }
        }, this._partialMessageTimeout, this.messageToken, this._partialMessageTimeout);
    }
}
exports.StreamMessageReader = StreamMessageReader;
class IPCMessageReader extends AbstractMessageReader {
    constructor(process) {
        super();
        this.process = process;
        let eventEmitter = this.process;
        eventEmitter.on('error', (error) => this.fireError(error));
        eventEmitter.on('close', () => this.fireClose());
    }
    listen(callback) {
        this.process.on('message', callback);
    }
}
exports.IPCMessageReader = IPCMessageReader;
class SocketMessageReader extends StreamMessageReader {
    constructor(socket, encoding = 'utf-8') {
        super(socket, encoding);
    }
}
exports.SocketMessageReader = SocketMessageReader;


/***/ }),

/***/ 725:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const events_1 = __webpack_require__(680);
const Is = __webpack_require__(593);
let ContentLength = 'Content-Length: ';
let CRLF = '\r\n';
var MessageWriter;
(function (MessageWriter) {
    function is(value) {
        let candidate = value;
        return candidate && Is.func(candidate.dispose) && Is.func(candidate.onClose) &&
            Is.func(candidate.onError) && Is.func(candidate.write);
    }
    MessageWriter.is = is;
})(MessageWriter = exports.MessageWriter || (exports.MessageWriter = {}));
class AbstractMessageWriter {
    constructor() {
        this.errorEmitter = new events_1.Emitter();
        this.closeEmitter = new events_1.Emitter();
    }
    dispose() {
        this.errorEmitter.dispose();
        this.closeEmitter.dispose();
    }
    get onError() {
        return this.errorEmitter.event;
    }
    fireError(error, message, count) {
        this.errorEmitter.fire([this.asError(error), message, count]);
    }
    get onClose() {
        return this.closeEmitter.event;
    }
    fireClose() {
        this.closeEmitter.fire(undefined);
    }
    asError(error) {
        if (error instanceof Error) {
            return error;
        }
        else {
            return new Error(`Writer recevied error. Reason: ${Is.string(error.message) ? error.message : 'unknown'}`);
        }
    }
}
exports.AbstractMessageWriter = AbstractMessageWriter;
class StreamMessageWriter extends AbstractMessageWriter {
    constructor(writable, encoding = 'utf8') {
        super();
        this.writable = writable;
        this.encoding = encoding;
        this.errorCount = 0;
        this.writable.on('error', (error) => this.fireError(error));
        this.writable.on('close', () => this.fireClose());
    }
    write(msg) {
        let json = JSON.stringify(msg);
        let contentLength = Buffer.byteLength(json, this.encoding);
        let headers = [
            ContentLength, contentLength.toString(), CRLF,
            CRLF
        ];
        try {
            // Header must be written in ASCII encoding
            this.writable.write(headers.join(''), 'ascii');
            // Now write the content. This can be written in any encoding
            this.writable.write(json, this.encoding);
            this.errorCount = 0;
        }
        catch (error) {
            this.errorCount++;
            this.fireError(error, msg, this.errorCount);
        }
    }
}
exports.StreamMessageWriter = StreamMessageWriter;
class IPCMessageWriter extends AbstractMessageWriter {
    constructor(process) {
        super();
        this.process = process;
        this.errorCount = 0;
        this.queue = [];
        this.sending = false;
        let eventEmitter = this.process;
        eventEmitter.on('error', (error) => this.fireError(error));
        eventEmitter.on('close', () => this.fireClose);
    }
    write(msg) {
        if (!this.sending && this.queue.length === 0) {
            // See https://github.com/nodejs/node/issues/7657
            this.doWriteMessage(msg);
        }
        else {
            this.queue.push(msg);
        }
    }
    doWriteMessage(msg) {
        try {
            if (this.process.send) {
                this.sending = true;
                this.process.send(msg, undefined, undefined, (error) => {
                    this.sending = false;
                    if (error) {
                        this.errorCount++;
                        this.fireError(error, msg, this.errorCount);
                    }
                    else {
                        this.errorCount = 0;
                    }
                    if (this.queue.length > 0) {
                        this.doWriteMessage(this.queue.shift());
                    }
                });
            }
        }
        catch (error) {
            this.errorCount++;
            this.fireError(error, msg, this.errorCount);
        }
    }
}
exports.IPCMessageWriter = IPCMessageWriter;
class SocketMessageWriter extends AbstractMessageWriter {
    constructor(socket, encoding = 'utf8') {
        super();
        this.socket = socket;
        this.queue = [];
        this.sending = false;
        this.encoding = encoding;
        this.errorCount = 0;
        this.socket.on('error', (error) => this.fireError(error));
        this.socket.on('close', () => this.fireClose());
    }
    write(msg) {
        if (!this.sending && this.queue.length === 0) {
            // See https://github.com/nodejs/node/issues/7657
            this.doWriteMessage(msg);
        }
        else {
            this.queue.push(msg);
        }
    }
    doWriteMessage(msg) {
        let json = JSON.stringify(msg);
        let contentLength = Buffer.byteLength(json, this.encoding);
        let headers = [
            ContentLength, contentLength.toString(), CRLF,
            CRLF
        ];
        try {
            // Header must be written in ASCII encoding
            this.sending = true;
            this.socket.write(headers.join(''), 'ascii', (error) => {
                if (error) {
                    this.handleError(error, msg);
                }
                try {
                    // Now write the content. This can be written in any encoding
                    this.socket.write(json, this.encoding, (error) => {
                        this.sending = false;
                        if (error) {
                            this.handleError(error, msg);
                        }
                        else {
                            this.errorCount = 0;
                        }
                        if (this.queue.length > 0) {
                            this.doWriteMessage(this.queue.shift());
                        }
                    });
                }
                catch (error) {
                    this.handleError(error, msg);
                }
            });
        }
        catch (error) {
            this.handleError(error, msg);
        }
    }
    handleError(error, msg) {
        this.errorCount++;
        this.fireError(error, msg, this.errorCount);
    }
}
exports.SocketMessageWriter = SocketMessageWriter;


/***/ }),

/***/ 234:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const is = __webpack_require__(593);
/**
 * Predefined error codes.
 */
var ErrorCodes;
(function (ErrorCodes) {
    // Defined by JSON RPC
    ErrorCodes.ParseError = -32700;
    ErrorCodes.InvalidRequest = -32600;
    ErrorCodes.MethodNotFound = -32601;
    ErrorCodes.InvalidParams = -32602;
    ErrorCodes.InternalError = -32603;
    ErrorCodes.serverErrorStart = -32099;
    ErrorCodes.serverErrorEnd = -32000;
    ErrorCodes.ServerNotInitialized = -32002;
    ErrorCodes.UnknownErrorCode = -32001;
    // Defined by the protocol.
    ErrorCodes.RequestCancelled = -32800;
    // Defined by VSCode library.
    ErrorCodes.MessageWriteError = 1;
    ErrorCodes.MessageReadError = 2;
})(ErrorCodes = exports.ErrorCodes || (exports.ErrorCodes = {}));
/**
 * An error object return in a response in case a request
 * has failed.
 */
class ResponseError extends Error {
    constructor(code, message, data) {
        super(message);
        this.code = is.number(code) ? code : ErrorCodes.UnknownErrorCode;
        this.data = data;
        Object.setPrototypeOf(this, ResponseError.prototype);
    }
    toJson() {
        return {
            code: this.code,
            message: this.message,
            data: this.data,
        };
    }
}
exports.ResponseError = ResponseError;
/**
 * An abstract implementation of a MessageType.
 */
class AbstractMessageType {
    constructor(_method, _numberOfParams) {
        this._method = _method;
        this._numberOfParams = _numberOfParams;
    }
    get method() {
        return this._method;
    }
    get numberOfParams() {
        return this._numberOfParams;
    }
}
exports.AbstractMessageType = AbstractMessageType;
/**
 * Classes to type request response pairs
 */
class RequestType0 extends AbstractMessageType {
    constructor(method) {
        super(method, 0);
        this._ = undefined;
    }
}
exports.RequestType0 = RequestType0;
class RequestType extends AbstractMessageType {
    constructor(method) {
        super(method, 1);
        this._ = undefined;
    }
}
exports.RequestType = RequestType;
class RequestType1 extends AbstractMessageType {
    constructor(method) {
        super(method, 1);
        this._ = undefined;
    }
}
exports.RequestType1 = RequestType1;
class RequestType2 extends AbstractMessageType {
    constructor(method) {
        super(method, 2);
        this._ = undefined;
    }
}
exports.RequestType2 = RequestType2;
class RequestType3 extends AbstractMessageType {
    constructor(method) {
        super(method, 3);
        this._ = undefined;
    }
}
exports.RequestType3 = RequestType3;
class RequestType4 extends AbstractMessageType {
    constructor(method) {
        super(method, 4);
        this._ = undefined;
    }
}
exports.RequestType4 = RequestType4;
class RequestType5 extends AbstractMessageType {
    constructor(method) {
        super(method, 5);
        this._ = undefined;
    }
}
exports.RequestType5 = RequestType5;
class RequestType6 extends AbstractMessageType {
    constructor(method) {
        super(method, 6);
        this._ = undefined;
    }
}
exports.RequestType6 = RequestType6;
class RequestType7 extends AbstractMessageType {
    constructor(method) {
        super(method, 7);
        this._ = undefined;
    }
}
exports.RequestType7 = RequestType7;
class RequestType8 extends AbstractMessageType {
    constructor(method) {
        super(method, 8);
        this._ = undefined;
    }
}
exports.RequestType8 = RequestType8;
class RequestType9 extends AbstractMessageType {
    constructor(method) {
        super(method, 9);
        this._ = undefined;
    }
}
exports.RequestType9 = RequestType9;
class NotificationType extends AbstractMessageType {
    constructor(method) {
        super(method, 1);
        this._ = undefined;
    }
}
exports.NotificationType = NotificationType;
class NotificationType0 extends AbstractMessageType {
    constructor(method) {
        super(method, 0);
        this._ = undefined;
    }
}
exports.NotificationType0 = NotificationType0;
class NotificationType1 extends AbstractMessageType {
    constructor(method) {
        super(method, 1);
        this._ = undefined;
    }
}
exports.NotificationType1 = NotificationType1;
class NotificationType2 extends AbstractMessageType {
    constructor(method) {
        super(method, 2);
        this._ = undefined;
    }
}
exports.NotificationType2 = NotificationType2;
class NotificationType3 extends AbstractMessageType {
    constructor(method) {
        super(method, 3);
        this._ = undefined;
    }
}
exports.NotificationType3 = NotificationType3;
class NotificationType4 extends AbstractMessageType {
    constructor(method) {
        super(method, 4);
        this._ = undefined;
    }
}
exports.NotificationType4 = NotificationType4;
class NotificationType5 extends AbstractMessageType {
    constructor(method) {
        super(method, 5);
        this._ = undefined;
    }
}
exports.NotificationType5 = NotificationType5;
class NotificationType6 extends AbstractMessageType {
    constructor(method) {
        super(method, 6);
        this._ = undefined;
    }
}
exports.NotificationType6 = NotificationType6;
class NotificationType7 extends AbstractMessageType {
    constructor(method) {
        super(method, 7);
        this._ = undefined;
    }
}
exports.NotificationType7 = NotificationType7;
class NotificationType8 extends AbstractMessageType {
    constructor(method) {
        super(method, 8);
        this._ = undefined;
    }
}
exports.NotificationType8 = NotificationType8;
class NotificationType9 extends AbstractMessageType {
    constructor(method) {
        super(method, 9);
        this._ = undefined;
    }
}
exports.NotificationType9 = NotificationType9;
/**
 * Tests if the given message is a request message
 */
function isRequestMessage(message) {
    let candidate = message;
    return candidate && is.string(candidate.method) && (is.string(candidate.id) || is.number(candidate.id));
}
exports.isRequestMessage = isRequestMessage;
/**
 * Tests if the given message is a notification message
 */
function isNotificationMessage(message) {
    let candidate = message;
    return candidate && is.string(candidate.method) && message.id === void 0;
}
exports.isNotificationMessage = isNotificationMessage;
/**
 * Tests if the given message is a response message
 */
function isResponseMessage(message) {
    let candidate = message;
    return candidate && (candidate.result !== void 0 || !!candidate.error) && (is.string(candidate.id) || is.number(candidate.id) || candidate.id === null);
}
exports.isResponseMessage = isResponseMessage;


/***/ }),

/***/ 377:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const path_1 = __webpack_require__(17);
const os_1 = __webpack_require__(37);
const crypto_1 = __webpack_require__(113);
const net_1 = __webpack_require__(808);
const messageReader_1 = __webpack_require__(386);
const messageWriter_1 = __webpack_require__(725);
function generateRandomPipeName() {
    const randomSuffix = crypto_1.randomBytes(21).toString('hex');
    if (process.platform === 'win32') {
        return `\\\\.\\pipe\\vscode-jsonrpc-${randomSuffix}-sock`;
    }
    else {
        // Mac/Unix: use socket file
        return path_1.join(os_1.tmpdir(), `vscode-${randomSuffix}.sock`);
    }
}
exports.generateRandomPipeName = generateRandomPipeName;
function createClientPipeTransport(pipeName, encoding = 'utf-8') {
    let connectResolve;
    let connected = new Promise((resolve, _reject) => {
        connectResolve = resolve;
    });
    return new Promise((resolve, reject) => {
        let server = net_1.createServer((socket) => {
            server.close();
            connectResolve([
                new messageReader_1.SocketMessageReader(socket, encoding),
                new messageWriter_1.SocketMessageWriter(socket, encoding)
            ]);
        });
        server.on('error', reject);
        server.listen(pipeName, () => {
            server.removeListener('error', reject);
            resolve({
                onConnected: () => { return connected; }
            });
        });
    });
}
exports.createClientPipeTransport = createClientPipeTransport;
function createServerPipeTransport(pipeName, encoding = 'utf-8') {
    const socket = net_1.createConnection(pipeName);
    return [
        new messageReader_1.SocketMessageReader(socket, encoding),
        new messageWriter_1.SocketMessageWriter(socket, encoding)
    ];
}
exports.createServerPipeTransport = createServerPipeTransport;


/***/ }),

/***/ 609:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const net_1 = __webpack_require__(808);
const messageReader_1 = __webpack_require__(386);
const messageWriter_1 = __webpack_require__(725);
function createClientSocketTransport(port, encoding = 'utf-8') {
    let connectResolve;
    let connected = new Promise((resolve, _reject) => {
        connectResolve = resolve;
    });
    return new Promise((resolve, reject) => {
        let server = net_1.createServer((socket) => {
            server.close();
            connectResolve([
                new messageReader_1.SocketMessageReader(socket, encoding),
                new messageWriter_1.SocketMessageWriter(socket, encoding)
            ]);
        });
        server.on('error', reject);
        server.listen(port, '127.0.0.1', () => {
            server.removeListener('error', reject);
            resolve({
                onConnected: () => { return connected; }
            });
        });
    });
}
exports.createClientSocketTransport = createClientSocketTransport;
function createServerSocketTransport(port, encoding = 'utf-8') {
    const socket = net_1.createConnection(port, '127.0.0.1');
    return [
        new messageReader_1.SocketMessageReader(socket, encoding),
        new messageWriter_1.SocketMessageWriter(socket, encoding)
    ];
}
exports.createServerSocketTransport = createServerSocketTransport;


/***/ }),

/***/ 620:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

function __export(m) {
    for (var p in m) if (!exports.hasOwnProperty(p)) exports[p] = m[p];
}
Object.defineProperty(exports, "__esModule", ({ value: true }));
const vscode_1 = __webpack_require__(496);
const vscode_languageserver_protocol_1 = __webpack_require__(192);
const c2p = __webpack_require__(103);
const p2c = __webpack_require__(101);
const Is = __webpack_require__(97);
const async_1 = __webpack_require__(299);
const UUID = __webpack_require__(884);
__export(__webpack_require__(192));
class ConsoleLogger {
    error(message) {
        console.error(message);
    }
    warn(message) {
        console.warn(message);
    }
    info(message) {
        console.info(message);
    }
    log(message) {
        console.log(message);
    }
}
function createConnection(input, output, errorHandler, closeHandler) {
    let logger = new ConsoleLogger();
    let connection = vscode_languageserver_protocol_1.createProtocolConnection(input, output, logger);
    connection.onError((data) => { errorHandler(data[0], data[1], data[2]); });
    connection.onClose(closeHandler);
    let result = {
        listen: () => connection.listen(),
        sendRequest: (type, ...params) => connection.sendRequest(Is.string(type) ? type : type.method, ...params),
        onRequest: (type, handler) => connection.onRequest(Is.string(type) ? type : type.method, handler),
        sendNotification: (type, params) => connection.sendNotification(Is.string(type) ? type : type.method, params),
        onNotification: (type, handler) => connection.onNotification(Is.string(type) ? type : type.method, handler),
        trace: (value, tracer, sendNotificationOrTraceOptions) => {
            const defaultTraceOptions = {
                sendNotification: false,
                traceFormat: vscode_languageserver_protocol_1.TraceFormat.Text
            };
            if (sendNotificationOrTraceOptions === void 0) {
                connection.trace(value, tracer, defaultTraceOptions);
            }
            else if (Is.boolean(sendNotificationOrTraceOptions)) {
                connection.trace(value, tracer, sendNotificationOrTraceOptions);
            }
            else {
                connection.trace(value, tracer, sendNotificationOrTraceOptions);
            }
        },
        initialize: (params) => connection.sendRequest(vscode_languageserver_protocol_1.InitializeRequest.type, params),
        shutdown: () => connection.sendRequest(vscode_languageserver_protocol_1.ShutdownRequest.type, undefined),
        exit: () => connection.sendNotification(vscode_languageserver_protocol_1.ExitNotification.type),
        onLogMessage: (handler) => connection.onNotification(vscode_languageserver_protocol_1.LogMessageNotification.type, handler),
        onShowMessage: (handler) => connection.onNotification(vscode_languageserver_protocol_1.ShowMessageNotification.type, handler),
        onTelemetry: (handler) => connection.onNotification(vscode_languageserver_protocol_1.TelemetryEventNotification.type, handler),
        didChangeConfiguration: (params) => connection.sendNotification(vscode_languageserver_protocol_1.DidChangeConfigurationNotification.type, params),
        didChangeWatchedFiles: (params) => connection.sendNotification(vscode_languageserver_protocol_1.DidChangeWatchedFilesNotification.type, params),
        didOpenTextDocument: (params) => connection.sendNotification(vscode_languageserver_protocol_1.DidOpenTextDocumentNotification.type, params),
        didChangeTextDocument: (params) => connection.sendNotification(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, params),
        didCloseTextDocument: (params) => connection.sendNotification(vscode_languageserver_protocol_1.DidCloseTextDocumentNotification.type, params),
        didSaveTextDocument: (params) => connection.sendNotification(vscode_languageserver_protocol_1.DidSaveTextDocumentNotification.type, params),
        onDiagnostics: (handler) => connection.onNotification(vscode_languageserver_protocol_1.PublishDiagnosticsNotification.type, handler),
        dispose: () => connection.dispose()
    };
    return result;
}
/**
 * An action to be performed when the connection is producing errors.
 */
var ErrorAction;
(function (ErrorAction) {
    /**
     * Continue running the server.
     */
    ErrorAction[ErrorAction["Continue"] = 1] = "Continue";
    /**
     * Shutdown the server.
     */
    ErrorAction[ErrorAction["Shutdown"] = 2] = "Shutdown";
})(ErrorAction = exports.ErrorAction || (exports.ErrorAction = {}));
/**
 * An action to be performed when the connection to a server got closed.
 */
var CloseAction;
(function (CloseAction) {
    /**
     * Don't restart the server. The connection stays closed.
     */
    CloseAction[CloseAction["DoNotRestart"] = 1] = "DoNotRestart";
    /**
     * Restart the server.
     */
    CloseAction[CloseAction["Restart"] = 2] = "Restart";
})(CloseAction = exports.CloseAction || (exports.CloseAction = {}));
class DefaultErrorHandler {
    constructor(name) {
        this.name = name;
        this.restarts = [];
    }
    error(_error, _message, count) {
        if (count && count <= 3) {
            return ErrorAction.Continue;
        }
        return ErrorAction.Shutdown;
    }
    closed() {
        this.restarts.push(Date.now());
        if (this.restarts.length < 5) {
            return CloseAction.Restart;
        }
        else {
            let diff = this.restarts[this.restarts.length - 1] - this.restarts[0];
            if (diff <= 3 * 60 * 1000) {
                vscode_1.window.showErrorMessage(`The ${this.name} server crashed 5 times in the last 3 minutes. The server will not be restarted.`);
                return CloseAction.DoNotRestart;
            }
            else {
                this.restarts.shift();
                return CloseAction.Restart;
            }
        }
    }
}
var RevealOutputChannelOn;
(function (RevealOutputChannelOn) {
    RevealOutputChannelOn[RevealOutputChannelOn["Info"] = 1] = "Info";
    RevealOutputChannelOn[RevealOutputChannelOn["Warn"] = 2] = "Warn";
    RevealOutputChannelOn[RevealOutputChannelOn["Error"] = 3] = "Error";
    RevealOutputChannelOn[RevealOutputChannelOn["Never"] = 4] = "Never";
})(RevealOutputChannelOn = exports.RevealOutputChannelOn || (exports.RevealOutputChannelOn = {}));
var State;
(function (State) {
    State[State["Stopped"] = 1] = "Stopped";
    State[State["Starting"] = 3] = "Starting";
    State[State["Running"] = 2] = "Running";
})(State = exports.State || (exports.State = {}));
var ClientState;
(function (ClientState) {
    ClientState[ClientState["Initial"] = 0] = "Initial";
    ClientState[ClientState["Starting"] = 1] = "Starting";
    ClientState[ClientState["StartFailed"] = 2] = "StartFailed";
    ClientState[ClientState["Running"] = 3] = "Running";
    ClientState[ClientState["Stopping"] = 4] = "Stopping";
    ClientState[ClientState["Stopped"] = 5] = "Stopped";
})(ClientState || (ClientState = {}));
const SupportedSymbolKinds = [
    vscode_languageserver_protocol_1.SymbolKind.File,
    vscode_languageserver_protocol_1.SymbolKind.Module,
    vscode_languageserver_protocol_1.SymbolKind.Namespace,
    vscode_languageserver_protocol_1.SymbolKind.Package,
    vscode_languageserver_protocol_1.SymbolKind.Class,
    vscode_languageserver_protocol_1.SymbolKind.Method,
    vscode_languageserver_protocol_1.SymbolKind.Property,
    vscode_languageserver_protocol_1.SymbolKind.Field,
    vscode_languageserver_protocol_1.SymbolKind.Constructor,
    vscode_languageserver_protocol_1.SymbolKind.Enum,
    vscode_languageserver_protocol_1.SymbolKind.Interface,
    vscode_languageserver_protocol_1.SymbolKind.Function,
    vscode_languageserver_protocol_1.SymbolKind.Variable,
    vscode_languageserver_protocol_1.SymbolKind.Constant,
    vscode_languageserver_protocol_1.SymbolKind.String,
    vscode_languageserver_protocol_1.SymbolKind.Number,
    vscode_languageserver_protocol_1.SymbolKind.Boolean,
    vscode_languageserver_protocol_1.SymbolKind.Array,
    vscode_languageserver_protocol_1.SymbolKind.Object,
    vscode_languageserver_protocol_1.SymbolKind.Key,
    vscode_languageserver_protocol_1.SymbolKind.Null,
    vscode_languageserver_protocol_1.SymbolKind.EnumMember,
    vscode_languageserver_protocol_1.SymbolKind.Struct,
    vscode_languageserver_protocol_1.SymbolKind.Event,
    vscode_languageserver_protocol_1.SymbolKind.Operator,
    vscode_languageserver_protocol_1.SymbolKind.TypeParameter
];
const SupportedCompletionItemKinds = [
    vscode_languageserver_protocol_1.CompletionItemKind.Text,
    vscode_languageserver_protocol_1.CompletionItemKind.Method,
    vscode_languageserver_protocol_1.CompletionItemKind.Function,
    vscode_languageserver_protocol_1.CompletionItemKind.Constructor,
    vscode_languageserver_protocol_1.CompletionItemKind.Field,
    vscode_languageserver_protocol_1.CompletionItemKind.Variable,
    vscode_languageserver_protocol_1.CompletionItemKind.Class,
    vscode_languageserver_protocol_1.CompletionItemKind.Interface,
    vscode_languageserver_protocol_1.CompletionItemKind.Module,
    vscode_languageserver_protocol_1.CompletionItemKind.Property,
    vscode_languageserver_protocol_1.CompletionItemKind.Unit,
    vscode_languageserver_protocol_1.CompletionItemKind.Value,
    vscode_languageserver_protocol_1.CompletionItemKind.Enum,
    vscode_languageserver_protocol_1.CompletionItemKind.Keyword,
    vscode_languageserver_protocol_1.CompletionItemKind.Snippet,
    vscode_languageserver_protocol_1.CompletionItemKind.Color,
    vscode_languageserver_protocol_1.CompletionItemKind.File,
    vscode_languageserver_protocol_1.CompletionItemKind.Reference,
    vscode_languageserver_protocol_1.CompletionItemKind.Folder,
    vscode_languageserver_protocol_1.CompletionItemKind.EnumMember,
    vscode_languageserver_protocol_1.CompletionItemKind.Constant,
    vscode_languageserver_protocol_1.CompletionItemKind.Struct,
    vscode_languageserver_protocol_1.CompletionItemKind.Event,
    vscode_languageserver_protocol_1.CompletionItemKind.Operator,
    vscode_languageserver_protocol_1.CompletionItemKind.TypeParameter
];
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
var DynamicFeature;
(function (DynamicFeature) {
    function is(value) {
        let candidate = value;
        return candidate && Is.func(candidate.register) && Is.func(candidate.unregister) && Is.func(candidate.dispose) && candidate.messages !== void 0;
    }
    DynamicFeature.is = is;
})(DynamicFeature || (DynamicFeature = {}));
class DocumentNotifiactions {
    constructor(_client, _event, _type, _middleware, _createParams, _selectorFilter) {
        this._client = _client;
        this._event = _event;
        this._type = _type;
        this._middleware = _middleware;
        this._createParams = _createParams;
        this._selectorFilter = _selectorFilter;
        this._selectors = new Map();
    }
    static textDocumentFilter(selectors, textDocument) {
        for (const selector of selectors) {
            if (vscode_1.languages.match(selector, textDocument)) {
                return true;
            }
        }
        return false;
    }
    register(_message, data) {
        if (!data.registerOptions.documentSelector) {
            return;
        }
        if (!this._listener) {
            this._listener = this._event(this.callback, this);
        }
        this._selectors.set(data.id, data.registerOptions.documentSelector);
    }
    callback(data) {
        if (!this._selectorFilter || this._selectorFilter(this._selectors.values(), data)) {
            if (this._middleware) {
                this._middleware(data, (data) => this._client.sendNotification(this._type, this._createParams(data)));
            }
            else {
                this._client.sendNotification(this._type, this._createParams(data));
            }
            this.notificationSent(data);
        }
    }
    notificationSent(_data) {
    }
    unregister(id) {
        this._selectors.delete(id);
        if (this._selectors.size === 0 && this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    dispose() {
        this._selectors.clear();
        if (this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
}
class DidOpenTextDocumentFeature extends DocumentNotifiactions {
    constructor(client, _syncedDocuments) {
        super(client, vscode_1.workspace.onDidOpenTextDocument, vscode_languageserver_protocol_1.DidOpenTextDocumentNotification.type, client.clientOptions.middleware.didOpen, (textDocument) => client.code2ProtocolConverter.asOpenTextDocumentParams(textDocument), DocumentNotifiactions.textDocumentFilter);
        this._syncedDocuments = _syncedDocuments;
    }
    get messages() {
        return vscode_languageserver_protocol_1.DidOpenTextDocumentNotification.type;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'synchronization').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.openClose) {
            this.register(this.messages, { id: UUID.generateUuid(), registerOptions: { documentSelector: documentSelector } });
        }
    }
    register(message, data) {
        super.register(message, data);
        if (!data.registerOptions.documentSelector) {
            return;
        }
        let documentSelector = data.registerOptions.documentSelector;
        vscode_1.workspace.textDocuments.forEach((textDocument) => {
            let uri = textDocument.uri.toString();
            if (this._syncedDocuments.has(uri)) {
                return;
            }
            if (vscode_1.languages.match(documentSelector, textDocument)) {
                let middleware = this._client.clientOptions.middleware;
                let didOpen = (textDocument) => {
                    this._client.sendNotification(this._type, this._createParams(textDocument));
                };
                if (middleware.didOpen) {
                    middleware.didOpen(textDocument, didOpen);
                }
                else {
                    didOpen(textDocument);
                }
                this._syncedDocuments.set(uri, textDocument);
            }
        });
    }
    notificationSent(textDocument) {
        super.notificationSent(textDocument);
        this._syncedDocuments.set(textDocument.uri.toString(), textDocument);
    }
}
class DidCloseTextDocumentFeature extends DocumentNotifiactions {
    constructor(client, _syncedDocuments) {
        super(client, vscode_1.workspace.onDidCloseTextDocument, vscode_languageserver_protocol_1.DidCloseTextDocumentNotification.type, client.clientOptions.middleware.didClose, (textDocument) => client.code2ProtocolConverter.asCloseTextDocumentParams(textDocument), DocumentNotifiactions.textDocumentFilter);
        this._syncedDocuments = _syncedDocuments;
    }
    get messages() {
        return vscode_languageserver_protocol_1.DidCloseTextDocumentNotification.type;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'synchronization').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.openClose) {
            this.register(this.messages, { id: UUID.generateUuid(), registerOptions: { documentSelector: documentSelector } });
        }
    }
    notificationSent(textDocument) {
        super.notificationSent(textDocument);
        this._syncedDocuments.delete(textDocument.uri.toString());
    }
    unregister(id) {
        let selector = this._selectors.get(id);
        // The super call removed the selector from the map
        // of selectors.
        super.unregister(id);
        let selectors = this._selectors.values();
        this._syncedDocuments.forEach((textDocument) => {
            if (vscode_1.languages.match(selector, textDocument) && !this._selectorFilter(selectors, textDocument)) {
                let middleware = this._client.clientOptions.middleware;
                let didClose = (textDocument) => {
                    this._client.sendNotification(this._type, this._createParams(textDocument));
                };
                this._syncedDocuments.delete(textDocument.uri.toString());
                if (middleware.didClose) {
                    middleware.didClose(textDocument, didClose);
                }
                else {
                    didClose(textDocument);
                }
            }
        });
    }
}
class DidChangeTextDocumentFeature {
    constructor(_client) {
        this._client = _client;
        this._changeData = new Map();
        this._forcingDelivery = false;
    }
    get messages() {
        return vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'synchronization').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.change !== void 0 && textDocumentSyncOptions.change !== vscode_languageserver_protocol_1.TextDocumentSyncKind.None) {
            this.register(this.messages, {
                id: UUID.generateUuid(),
                registerOptions: Object.assign({}, { documentSelector: documentSelector }, { syncKind: textDocumentSyncOptions.change })
            });
        }
    }
    register(_message, data) {
        if (!data.registerOptions.documentSelector) {
            return;
        }
        if (!this._listener) {
            this._listener = vscode_1.workspace.onDidChangeTextDocument(this.callback, this);
        }
        this._changeData.set(data.id, {
            documentSelector: data.registerOptions.documentSelector,
            syncKind: data.registerOptions.syncKind
        });
    }
    callback(event) {
        // Text document changes are send for dirty changes as well. We don't
        // have dirty / undirty events in the LSP so we ignore content changes
        // with length zero.
        if (event.contentChanges.length === 0) {
            return;
        }
        for (const changeData of this._changeData.values()) {
            if (vscode_1.languages.match(changeData.documentSelector, event.document)) {
                let middleware = this._client.clientOptions.middleware;
                if (changeData.syncKind === vscode_languageserver_protocol_1.TextDocumentSyncKind.Incremental) {
                    let params = this._client.code2ProtocolConverter.asChangeTextDocumentParams(event);
                    if (middleware.didChange) {
                        middleware.didChange(event, () => this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, params));
                    }
                    else {
                        this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, params);
                    }
                }
                else if (changeData.syncKind === vscode_languageserver_protocol_1.TextDocumentSyncKind.Full) {
                    let didChange = (event) => {
                        if (this._changeDelayer) {
                            if (this._changeDelayer.uri !== event.document.uri.toString()) {
                                // Use this force delivery to track boolean state. Otherwise we might call two times.
                                this.forceDelivery();
                                this._changeDelayer.uri = event.document.uri.toString();
                            }
                            this._changeDelayer.delayer.trigger(() => {
                                this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, this._client.code2ProtocolConverter.asChangeTextDocumentParams(event.document));
                            });
                        }
                        else {
                            this._changeDelayer = {
                                uri: event.document.uri.toString(),
                                delayer: new async_1.Delayer(200)
                            };
                            this._changeDelayer.delayer.trigger(() => {
                                this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, this._client.code2ProtocolConverter.asChangeTextDocumentParams(event.document));
                            }, -1);
                        }
                    };
                    if (middleware.didChange) {
                        middleware.didChange(event, didChange);
                    }
                    else {
                        didChange(event);
                    }
                }
            }
        }
    }
    unregister(id) {
        this._changeData.delete(id);
        if (this._changeData.size === 0 && this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    dispose() {
        this._changeDelayer = undefined;
        this._forcingDelivery = false;
        this._changeData.clear();
        if (this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    forceDelivery() {
        if (this._forcingDelivery || !this._changeDelayer) {
            return;
        }
        try {
            this._forcingDelivery = true;
            this._changeDelayer.delayer.forceDelivery();
        }
        finally {
            this._forcingDelivery = false;
        }
    }
}
class WillSaveFeature extends DocumentNotifiactions {
    constructor(client) {
        super(client, vscode_1.workspace.onWillSaveTextDocument, vscode_languageserver_protocol_1.WillSaveTextDocumentNotification.type, client.clientOptions.middleware.willSave, (willSaveEvent) => client.code2ProtocolConverter.asWillSaveTextDocumentParams(willSaveEvent), (selectors, willSaveEvent) => DocumentNotifiactions.textDocumentFilter(selectors, willSaveEvent.document));
    }
    get messages() {
        return vscode_languageserver_protocol_1.WillSaveTextDocumentNotification.type;
    }
    fillClientCapabilities(capabilities) {
        let value = ensure(ensure(capabilities, 'textDocument'), 'synchronization');
        value.willSave = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.willSave) {
            this.register(this.messages, {
                id: UUID.generateUuid(),
                registerOptions: { documentSelector: documentSelector }
            });
        }
    }
}
class WillSaveWaitUntilFeature {
    constructor(_client) {
        this._client = _client;
        this._selectors = new Map();
    }
    get messages() {
        return vscode_languageserver_protocol_1.WillSaveTextDocumentWaitUntilRequest.type;
    }
    fillClientCapabilities(capabilities) {
        let value = ensure(ensure(capabilities, 'textDocument'), 'synchronization');
        value.willSaveWaitUntil = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.willSaveWaitUntil) {
            this.register(this.messages, {
                id: UUID.generateUuid(),
                registerOptions: { documentSelector: documentSelector }
            });
        }
    }
    register(_message, data) {
        if (!data.registerOptions.documentSelector) {
            return;
        }
        if (!this._listener) {
            this._listener = vscode_1.workspace.onWillSaveTextDocument(this.callback, this);
        }
        this._selectors.set(data.id, data.registerOptions.documentSelector);
    }
    callback(event) {
        if (DocumentNotifiactions.textDocumentFilter(this._selectors.values(), event.document)) {
            let middleware = this._client.clientOptions.middleware;
            let willSaveWaitUntil = (event) => {
                return this._client.sendRequest(vscode_languageserver_protocol_1.WillSaveTextDocumentWaitUntilRequest.type, this._client.code2ProtocolConverter.asWillSaveTextDocumentParams(event)).then((edits) => {
                    let vEdits = this._client.protocol2CodeConverter.asTextEdits(edits);
                    return vEdits === void 0 ? [] : vEdits;
                });
            };
            event.waitUntil(middleware.willSaveWaitUntil
                ? middleware.willSaveWaitUntil(event, willSaveWaitUntil)
                : willSaveWaitUntil(event));
        }
    }
    unregister(id) {
        this._selectors.delete(id);
        if (this._selectors.size === 0 && this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    dispose() {
        this._selectors.clear();
        if (this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
}
class DidSaveTextDocumentFeature extends DocumentNotifiactions {
    constructor(client) {
        super(client, vscode_1.workspace.onDidSaveTextDocument, vscode_languageserver_protocol_1.DidSaveTextDocumentNotification.type, client.clientOptions.middleware.didSave, (textDocument) => client.code2ProtocolConverter.asSaveTextDocumentParams(textDocument, this._includeText), DocumentNotifiactions.textDocumentFilter);
    }
    get messages() {
        return vscode_languageserver_protocol_1.DidSaveTextDocumentNotification.type;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'synchronization').didSave = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.save) {
            this.register(this.messages, {
                id: UUID.generateUuid(),
                registerOptions: Object.assign({}, { documentSelector: documentSelector }, { includeText: !!textDocumentSyncOptions.save.includeText })
            });
        }
    }
    register(method, data) {
        this._includeText = !!data.registerOptions.includeText;
        super.register(method, data);
    }
}
class FileSystemWatcherFeature {
    constructor(_client, _notifyFileEvent) {
        this._client = _client;
        this._notifyFileEvent = _notifyFileEvent;
        this._watchers = new Map();
    }
    get messages() {
        return vscode_languageserver_protocol_1.DidChangeWatchedFilesNotification.type;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'workspace'), 'didChangeWatchedFiles').dynamicRegistration = true;
    }
    initialize(_capabilities, _documentSelector) {
    }
    register(_method, data) {
        if (!Array.isArray(data.registerOptions.watchers)) {
            return;
        }
        let disposeables = [];
        for (let watcher of data.registerOptions.watchers) {
            if (!Is.string(watcher.globPattern)) {
                continue;
            }
            let watchCreate = true, watchChange = true, watchDelete = true;
            if (watcher.kind !== void 0 && watcher.kind !== null) {
                watchCreate = (watcher.kind & vscode_languageserver_protocol_1.WatchKind.Create) !== 0;
                watchChange = (watcher.kind & vscode_languageserver_protocol_1.WatchKind.Change) != 0;
                watchDelete = (watcher.kind & vscode_languageserver_protocol_1.WatchKind.Delete) != 0;
            }
            let fileSystemWatcher = vscode_1.workspace.createFileSystemWatcher(watcher.globPattern, !watchCreate, !watchChange, !watchDelete);
            this.hookListeners(fileSystemWatcher, watchCreate, watchChange, watchDelete);
            disposeables.push(fileSystemWatcher);
        }
        this._watchers.set(data.id, disposeables);
    }
    registerRaw(id, fileSystemWatchers) {
        let disposeables = [];
        for (let fileSystemWatcher of fileSystemWatchers) {
            this.hookListeners(fileSystemWatcher, true, true, true, disposeables);
        }
        this._watchers.set(id, disposeables);
    }
    hookListeners(fileSystemWatcher, watchCreate, watchChange, watchDelete, listeners) {
        if (watchCreate) {
            fileSystemWatcher.onDidCreate((resource) => this._notifyFileEvent({
                uri: this._client.code2ProtocolConverter.asUri(resource),
                type: vscode_languageserver_protocol_1.FileChangeType.Created
            }), null, listeners);
        }
        if (watchChange) {
            fileSystemWatcher.onDidChange((resource) => this._notifyFileEvent({
                uri: this._client.code2ProtocolConverter.asUri(resource),
                type: vscode_languageserver_protocol_1.FileChangeType.Changed
            }), null, listeners);
        }
        if (watchDelete) {
            fileSystemWatcher.onDidDelete((resource) => this._notifyFileEvent({
                uri: this._client.code2ProtocolConverter.asUri(resource),
                type: vscode_languageserver_protocol_1.FileChangeType.Deleted
            }), null, listeners);
        }
    }
    unregister(id) {
        let disposeables = this._watchers.get(id);
        if (disposeables) {
            for (let disposable of disposeables) {
                disposable.dispose();
            }
        }
    }
    dispose() {
        this._watchers.forEach((disposeables) => {
            for (let disposable of disposeables) {
                disposable.dispose();
            }
        });
        this._watchers.clear();
    }
}
class TextDocumentFeature {
    constructor(_client, _message) {
        this._client = _client;
        this._message = _message;
        this._providers = new Map();
    }
    get messages() {
        return this._message;
    }
    register(message, data) {
        if (message.method !== this.messages.method) {
            throw new Error(`Register called on wrong feature. Requested ${message.method} but reached feature ${this.messages.method}`);
        }
        if (!data.registerOptions.documentSelector) {
            return;
        }
        let provider = this.registerLanguageProvider(data.registerOptions);
        if (provider) {
            this._providers.set(data.id, provider);
        }
    }
    unregister(id) {
        let provider = this._providers.get(id);
        if (provider) {
            provider.dispose();
        }
    }
    dispose() {
        this._providers.forEach((value) => {
            value.dispose();
        });
        this._providers.clear();
    }
}
exports.TextDocumentFeature = TextDocumentFeature;
class WorkspaceFeature {
    constructor(_client, _message) {
        this._client = _client;
        this._message = _message;
        this._providers = new Map();
    }
    get messages() {
        return this._message;
    }
    register(message, data) {
        if (message.method !== this.messages.method) {
            throw new Error(`Register called on wron feature. Requested ${message.method} but reached feature ${this.messages.method}`);
        }
        let provider = this.registerLanguageProvider(data.registerOptions);
        if (provider) {
            this._providers.set(data.id, provider);
        }
    }
    unregister(id) {
        let provider = this._providers.get(id);
        if (provider) {
            provider.dispose();
        }
    }
    dispose() {
        this._providers.forEach((value) => {
            value.dispose();
        });
        this._providers.clear();
    }
}
class CompletionItemFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.CompletionRequest.type);
    }
    fillClientCapabilities(capabilites) {
        let completion = ensure(ensure(capabilites, 'textDocument'), 'completion');
        completion.dynamicRegistration = true;
        completion.contextSupport = true;
        completion.completionItem = {
            snippetSupport: true,
            commitCharactersSupport: true,
            documentationFormat: [vscode_languageserver_protocol_1.MarkupKind.Markdown, vscode_languageserver_protocol_1.MarkupKind.PlainText],
            deprecatedSupport: true,
            preselectSupport: true
        };
        completion.completionItemKind = { valueSet: SupportedCompletionItemKinds };
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.completionProvider || !documentSelector) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, { documentSelector: documentSelector }, capabilities.completionProvider)
        });
    }
    registerLanguageProvider(options) {
        let triggerCharacters = options.triggerCharacters || [];
        let client = this._client;
        let provideCompletionItems = (document, position, context, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.CompletionRequest.type, client.code2ProtocolConverter.asCompletionParams(document, position, context), token).then(client.protocol2CodeConverter.asCompletionResult, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.CompletionRequest.type, error);
                return Promise.resolve([]);
            });
        };
        let resolveCompletionItem = (item, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.CompletionResolveRequest.type, client.code2ProtocolConverter.asCompletionItem(item), token).then(client.protocol2CodeConverter.asCompletionItem, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.CompletionResolveRequest.type, error);
                return Promise.resolve(item);
            });
        };
        let middleware = this._client.clientOptions.middleware;
        return vscode_1.languages.registerCompletionItemProvider(options.documentSelector, {
            provideCompletionItems: (document, position, token, context) => {
                return middleware.provideCompletionItem
                    ? middleware.provideCompletionItem(document, position, context, token, provideCompletionItems)
                    : provideCompletionItems(document, position, context, token);
            },
            resolveCompletionItem: options.resolveProvider
                ? (item, token) => {
                    return middleware.resolveCompletionItem
                        ? middleware.resolveCompletionItem(item, token, resolveCompletionItem)
                        : resolveCompletionItem(item, token);
                }
                : undefined
        }, ...triggerCharacters);
    }
}
class HoverFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.HoverRequest.type);
    }
    fillClientCapabilities(capabilites) {
        const hoverCapability = (ensure(ensure(capabilites, 'textDocument'), 'hover'));
        hoverCapability.dynamicRegistration = true;
        hoverCapability.contentFormat = [vscode_languageserver_protocol_1.MarkupKind.Markdown, vscode_languageserver_protocol_1.MarkupKind.PlainText];
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.hoverProvider || !documentSelector) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, { documentSelector: documentSelector })
        });
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideHover = (document, position, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.HoverRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asHover, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.HoverRequest.type, error);
                return Promise.resolve(null);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerHoverProvider(options.documentSelector, {
            provideHover: (document, position, token) => {
                return middleware.provideHover
                    ? middleware.provideHover(document, position, token, provideHover)
                    : provideHover(document, position, token);
            }
        });
    }
}
class SignatureHelpFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.SignatureHelpRequest.type);
    }
    fillClientCapabilities(capabilites) {
        let config = ensure(ensure(capabilites, 'textDocument'), 'signatureHelp');
        config.dynamicRegistration = true;
        config.signatureInformation = { documentationFormat: [vscode_languageserver_protocol_1.MarkupKind.Markdown, vscode_languageserver_protocol_1.MarkupKind.PlainText] };
        config.signatureInformation.parameterInformation = { labelOffsetSupport: true };
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.signatureHelpProvider || !documentSelector) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, { documentSelector: documentSelector }, capabilities.signatureHelpProvider)
        });
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let providerSignatureHelp = (document, position, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.SignatureHelpRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asSignatureHelp, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.SignatureHelpRequest.type, error);
                return Promise.resolve(null);
            });
        };
        let middleware = client.clientOptions.middleware;
        let triggerCharacters = options.triggerCharacters || [];
        return vscode_1.languages.registerSignatureHelpProvider(options.documentSelector, {
            provideSignatureHelp: (document, position, token) => {
                return middleware.provideSignatureHelp
                    ? middleware.provideSignatureHelp(document, position, token, providerSignatureHelp)
                    : providerSignatureHelp(document, position, token);
            }
        }, ...triggerCharacters);
    }
}
class DefinitionFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DefinitionRequest.type);
    }
    fillClientCapabilities(capabilites) {
        let definitionSupport = ensure(ensure(capabilites, 'textDocument'), 'definition');
        definitionSupport.dynamicRegistration = true;
        definitionSupport.linkSupport = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.definitionProvider || !documentSelector) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, { documentSelector: documentSelector })
        });
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideDefinition = (document, position, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.DefinitionRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asDefinitionResult, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.DefinitionRequest.type, error);
                return Promise.resolve(null);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerDefinitionProvider(options.documentSelector, {
            provideDefinition: (document, position, token) => {
                return middleware.provideDefinition
                    ? middleware.provideDefinition(document, position, token, provideDefinition)
                    : provideDefinition(document, position, token);
            }
        });
    }
}
class ReferencesFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.ReferencesRequest.type);
    }
    fillClientCapabilities(capabilites) {
        ensure(ensure(capabilites, 'textDocument'), 'references').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.referencesProvider || !documentSelector) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, { documentSelector: documentSelector })
        });
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let providerReferences = (document, position, options, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.ReferencesRequest.type, client.code2ProtocolConverter.asReferenceParams(document, position, options), token).then(client.protocol2CodeConverter.asReferences, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.ReferencesRequest.type, error);
                return Promise.resolve([]);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerReferenceProvider(options.documentSelector, {
            provideReferences: (document, position, options, token) => {
                return middleware.provideReferences
                    ? middleware.provideReferences(document, position, options, token, providerReferences)
                    : providerReferences(document, position, options, token);
            }
        });
    }
}
class DocumentHighlightFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentHighlightRequest.type);
    }
    fillClientCapabilities(capabilites) {
        ensure(ensure(capabilites, 'textDocument'), 'documentHighlight').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.documentHighlightProvider || !documentSelector) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, { documentSelector: documentSelector })
        });
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideDocumentHighlights = (document, position, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.DocumentHighlightRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asDocumentHighlights, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.DocumentHighlightRequest.type, error);
                return Promise.resolve([]);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerDocumentHighlightProvider(options.documentSelector, {
            provideDocumentHighlights: (document, position, token) => {
                return middleware.provideDocumentHighlights
                    ? middleware.provideDocumentHighlights(document, position, token, provideDocumentHighlights)
                    : provideDocumentHighlights(document, position, token);
            }
        });
    }
}
class DocumentSymbolFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentSymbolRequest.type);
    }
    fillClientCapabilities(capabilites) {
        let symbolCapabilities = ensure(ensure(capabilites, 'textDocument'), 'documentSymbol');
        symbolCapabilities.dynamicRegistration = true;
        symbolCapabilities.symbolKind = {
            valueSet: SupportedSymbolKinds
        };
        symbolCapabilities.hierarchicalDocumentSymbolSupport = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.documentSymbolProvider || !documentSelector) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, { documentSelector: documentSelector })
        });
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideDocumentSymbols = (document, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.DocumentSymbolRequest.type, client.code2ProtocolConverter.asDocumentSymbolParams(document), token).then((data) => {
                if (data === null) {
                    return undefined;
                }
                if (data.length === 0) {
                    return [];
                }
                else {
                    let element = data[0];
                    if (vscode_languageserver_protocol_1.DocumentSymbol.is(element)) {
                        return client.protocol2CodeConverter.asDocumentSymbols(data);
                    }
                    else {
                        return client.protocol2CodeConverter.asSymbolInformations(data);
                    }
                }
            }, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.DocumentSymbolRequest.type, error);
                return Promise.resolve([]);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerDocumentSymbolProvider(options.documentSelector, {
            provideDocumentSymbols: (document, token) => {
                return middleware.provideDocumentSymbols
                    ? middleware.provideDocumentSymbols(document, token, provideDocumentSymbols)
                    : provideDocumentSymbols(document, token);
            }
        });
    }
}
class WorkspaceSymbolFeature extends WorkspaceFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.WorkspaceSymbolRequest.type);
    }
    fillClientCapabilities(capabilites) {
        let symbolCapabilities = ensure(ensure(capabilites, 'workspace'), 'symbol');
        symbolCapabilities.dynamicRegistration = true;
        symbolCapabilities.symbolKind = {
            valueSet: SupportedSymbolKinds
        };
    }
    initialize(capabilities) {
        if (!capabilities.workspaceSymbolProvider) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: undefined
        });
    }
    registerLanguageProvider(_options) {
        let client = this._client;
        let provideWorkspaceSymbols = (query, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.WorkspaceSymbolRequest.type, { query }, token).then(client.protocol2CodeConverter.asSymbolInformations, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.WorkspaceSymbolRequest.type, error);
                return Promise.resolve([]);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerWorkspaceSymbolProvider({
            provideWorkspaceSymbols: (query, token) => {
                return middleware.provideWorkspaceSymbols
                    ? middleware.provideWorkspaceSymbols(query, token, provideWorkspaceSymbols)
                    : provideWorkspaceSymbols(query, token);
            }
        });
    }
}
class CodeActionFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.CodeActionRequest.type);
    }
    fillClientCapabilities(capabilites) {
        const cap = ensure(ensure(capabilites, 'textDocument'), 'codeAction');
        cap.dynamicRegistration = true;
        cap.codeActionLiteralSupport = {
            codeActionKind: {
                valueSet: [
                    '',
                    vscode_languageserver_protocol_1.CodeActionKind.QuickFix,
                    vscode_languageserver_protocol_1.CodeActionKind.Refactor,
                    vscode_languageserver_protocol_1.CodeActionKind.RefactorExtract,
                    vscode_languageserver_protocol_1.CodeActionKind.RefactorInline,
                    vscode_languageserver_protocol_1.CodeActionKind.RefactorRewrite,
                    vscode_languageserver_protocol_1.CodeActionKind.Source,
                    vscode_languageserver_protocol_1.CodeActionKind.SourceOrganizeImports
                ]
            }
        };
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.codeActionProvider || !documentSelector) {
            return;
        }
        let codeActionKinds = undefined;
        if (!Is.boolean(capabilities.codeActionProvider)) {
            codeActionKinds = capabilities.codeActionProvider.codeActionKinds;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: { documentSelector: documentSelector, codeActionKinds }
        });
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideCodeActions = (document, range, context, token) => {
            let params = {
                textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                range: client.code2ProtocolConverter.asRange(range),
                context: client.code2ProtocolConverter.asCodeActionContext(context)
            };
            return client.sendRequest(vscode_languageserver_protocol_1.CodeActionRequest.type, params, token).then((values) => {
                if (values === null) {
                    return undefined;
                }
                let result = [];
                for (let item of values) {
                    if (vscode_languageserver_protocol_1.Command.is(item)) {
                        result.push(client.protocol2CodeConverter.asCommand(item));
                    }
                    else {
                        result.push(client.protocol2CodeConverter.asCodeAction(item));
                    }
                    ;
                }
                return result;
            }, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.CodeActionRequest.type, error);
                return Promise.resolve([]);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerCodeActionsProvider(options.documentSelector, {
            provideCodeActions: (document, range, context, token) => {
                return middleware.provideCodeActions
                    ? middleware.provideCodeActions(document, range, context, token, provideCodeActions)
                    : provideCodeActions(document, range, context, token);
            }
        }, options.codeActionKinds
            ? { providedCodeActionKinds: client.protocol2CodeConverter.asCodeActionKinds(options.codeActionKinds) }
            : undefined);
    }
}
class CodeLensFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.CodeLensRequest.type);
    }
    fillClientCapabilities(capabilites) {
        ensure(ensure(capabilites, 'textDocument'), 'codeLens').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.codeLensProvider || !documentSelector) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, { documentSelector: documentSelector }, capabilities.codeLensProvider)
        });
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideCodeLenses = (document, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.CodeLensRequest.type, client.code2ProtocolConverter.asCodeLensParams(document), token).then(client.protocol2CodeConverter.asCodeLenses, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.CodeLensRequest.type, error);
                return Promise.resolve([]);
            });
        };
        let resolveCodeLens = (codeLens, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.CodeLensResolveRequest.type, client.code2ProtocolConverter.asCodeLens(codeLens), token).then(client.protocol2CodeConverter.asCodeLens, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.CodeLensResolveRequest.type, error);
                return codeLens;
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerCodeLensProvider(options.documentSelector, {
            provideCodeLenses: (document, token) => {
                return middleware.provideCodeLenses
                    ? middleware.provideCodeLenses(document, token, provideCodeLenses)
                    : provideCodeLenses(document, token);
            },
            resolveCodeLens: (options.resolveProvider)
                ? (codeLens, token) => {
                    return middleware.resolveCodeLens
                        ? middleware.resolveCodeLens(codeLens, token, resolveCodeLens)
                        : resolveCodeLens(codeLens, token);
                }
                : undefined
        });
    }
}
class DocumentFormattingFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentFormattingRequest.type);
    }
    fillClientCapabilities(capabilites) {
        ensure(ensure(capabilites, 'textDocument'), 'formatting').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.documentFormattingProvider || !documentSelector) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, { documentSelector: documentSelector })
        });
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideDocumentFormattingEdits = (document, options, token) => {
            let params = {
                textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                options: client.code2ProtocolConverter.asFormattingOptions(options)
            };
            return client.sendRequest(vscode_languageserver_protocol_1.DocumentFormattingRequest.type, params, token).then(client.protocol2CodeConverter.asTextEdits, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.DocumentFormattingRequest.type, error);
                return Promise.resolve([]);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerDocumentFormattingEditProvider(options.documentSelector, {
            provideDocumentFormattingEdits: (document, options, token) => {
                return middleware.provideDocumentFormattingEdits
                    ? middleware.provideDocumentFormattingEdits(document, options, token, provideDocumentFormattingEdits)
                    : provideDocumentFormattingEdits(document, options, token);
            }
        });
    }
}
class DocumentRangeFormattingFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentRangeFormattingRequest.type);
    }
    fillClientCapabilities(capabilites) {
        ensure(ensure(capabilites, 'textDocument'), 'rangeFormatting').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.documentRangeFormattingProvider || !documentSelector) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, { documentSelector: documentSelector })
        });
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideDocumentRangeFormattingEdits = (document, range, options, token) => {
            let params = {
                textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                range: client.code2ProtocolConverter.asRange(range),
                options: client.code2ProtocolConverter.asFormattingOptions(options)
            };
            return client.sendRequest(vscode_languageserver_protocol_1.DocumentRangeFormattingRequest.type, params, token).then(client.protocol2CodeConverter.asTextEdits, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.DocumentRangeFormattingRequest.type, error);
                return Promise.resolve([]);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerDocumentRangeFormattingEditProvider(options.documentSelector, {
            provideDocumentRangeFormattingEdits: (document, range, options, token) => {
                return middleware.provideDocumentRangeFormattingEdits
                    ? middleware.provideDocumentRangeFormattingEdits(document, range, options, token, provideDocumentRangeFormattingEdits)
                    : provideDocumentRangeFormattingEdits(document, range, options, token);
            }
        });
    }
}
class DocumentOnTypeFormattingFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentOnTypeFormattingRequest.type);
    }
    fillClientCapabilities(capabilites) {
        ensure(ensure(capabilites, 'textDocument'), 'onTypeFormatting').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.documentOnTypeFormattingProvider || !documentSelector) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, { documentSelector: documentSelector }, capabilities.documentOnTypeFormattingProvider)
        });
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let moreTriggerCharacter = options.moreTriggerCharacter || [];
        let provideOnTypeFormattingEdits = (document, position, ch, options, token) => {
            let params = {
                textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                position: client.code2ProtocolConverter.asPosition(position),
                ch: ch,
                options: client.code2ProtocolConverter.asFormattingOptions(options)
            };
            return client.sendRequest(vscode_languageserver_protocol_1.DocumentOnTypeFormattingRequest.type, params, token).then(client.protocol2CodeConverter.asTextEdits, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.DocumentOnTypeFormattingRequest.type, error);
                return Promise.resolve([]);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerOnTypeFormattingEditProvider(options.documentSelector, {
            provideOnTypeFormattingEdits: (document, position, ch, options, token) => {
                return middleware.provideOnTypeFormattingEdits
                    ? middleware.provideOnTypeFormattingEdits(document, position, ch, options, token, provideOnTypeFormattingEdits)
                    : provideOnTypeFormattingEdits(document, position, ch, options, token);
            }
        }, options.firstTriggerCharacter, ...moreTriggerCharacter);
    }
}
class RenameFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.RenameRequest.type);
    }
    fillClientCapabilities(capabilites) {
        let rename = ensure(ensure(capabilites, 'textDocument'), 'rename');
        rename.dynamicRegistration = true;
        rename.prepareSupport = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.renameProvider || !documentSelector) {
            return;
        }
        let options = Object.assign({}, { documentSelector: documentSelector });
        if (Is.boolean(capabilities.renameProvider)) {
            options.prepareProvider = false;
        }
        else {
            options.prepareProvider = capabilities.renameProvider.prepareProvider;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: options
        });
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideRenameEdits = (document, position, newName, token) => {
            let params = {
                textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                position: client.code2ProtocolConverter.asPosition(position),
                newName: newName
            };
            return client.sendRequest(vscode_languageserver_protocol_1.RenameRequest.type, params, token).then(client.protocol2CodeConverter.asWorkspaceEdit, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.RenameRequest.type, error);
                return Promise.reject(new Error(error.message));
            });
        };
        let prepareRename = (document, position, token) => {
            let params = {
                textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                position: client.code2ProtocolConverter.asPosition(position),
            };
            return client.sendRequest(vscode_languageserver_protocol_1.PrepareRenameRequest.type, params, token).then((result) => {
                if (vscode_languageserver_protocol_1.Range.is(result)) {
                    return client.protocol2CodeConverter.asRange(result);
                }
                else if (result && result.range) {
                    return {
                        range: client.protocol2CodeConverter.asRange(result.range),
                        placeholder: result.placeholder
                    };
                }
                return null;
            }, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.PrepareRenameRequest.type, error);
                return Promise.reject(new Error(error.message));
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerRenameProvider(options.documentSelector, {
            provideRenameEdits: (document, position, newName, token) => {
                return middleware.provideRenameEdits
                    ? middleware.provideRenameEdits(document, position, newName, token, provideRenameEdits)
                    : provideRenameEdits(document, position, newName, token);
            },
            prepareRename: options.prepareProvider
                ? (document, position, token) => {
                    return middleware.prepareRename
                        ? middleware.prepareRename(document, position, token, prepareRename)
                        : prepareRename(document, position, token);
                }
                : undefined
        });
    }
}
class DocumentLinkFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentLinkRequest.type);
    }
    fillClientCapabilities(capabilites) {
        ensure(ensure(capabilites, 'textDocument'), 'documentLink').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.documentLinkProvider || !documentSelector) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, { documentSelector: documentSelector }, capabilities.documentLinkProvider)
        });
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideDocumentLinks = (document, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.DocumentLinkRequest.type, client.code2ProtocolConverter.asDocumentLinkParams(document), token).then(client.protocol2CodeConverter.asDocumentLinks, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.DocumentLinkRequest.type, error);
                Promise.resolve(new Error(error.message));
            });
        };
        let resolveDocumentLink = (link, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.DocumentLinkResolveRequest.type, client.code2ProtocolConverter.asDocumentLink(link), token).then(client.protocol2CodeConverter.asDocumentLink, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.DocumentLinkResolveRequest.type, error);
                Promise.resolve(new Error(error.message));
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerDocumentLinkProvider(options.documentSelector, {
            provideDocumentLinks: (document, token) => {
                return middleware.provideDocumentLinks
                    ? middleware.provideDocumentLinks(document, token, provideDocumentLinks)
                    : provideDocumentLinks(document, token);
            },
            resolveDocumentLink: options.resolveProvider
                ? (link, token) => {
                    return middleware.resolveDocumentLink
                        ? middleware.resolveDocumentLink(link, token, resolveDocumentLink)
                        : resolveDocumentLink(link, token);
                }
                : undefined
        });
    }
}
class ConfigurationFeature {
    constructor(_client) {
        this._client = _client;
        this._listeners = new Map();
    }
    get messages() {
        return vscode_languageserver_protocol_1.DidChangeConfigurationNotification.type;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'workspace'), 'didChangeConfiguration').dynamicRegistration = true;
    }
    initialize() {
        let section = this._client.clientOptions.synchronize.configurationSection;
        if (section !== void 0) {
            this.register(this.messages, {
                id: UUID.generateUuid(),
                registerOptions: {
                    section: section
                }
            });
        }
    }
    register(_message, data) {
        let disposable = vscode_1.workspace.onDidChangeConfiguration((event) => {
            this.onDidChangeConfiguration(data.registerOptions.section, event);
        });
        this._listeners.set(data.id, disposable);
        if (data.registerOptions.section !== void 0) {
            this.onDidChangeConfiguration(data.registerOptions.section, undefined);
        }
    }
    unregister(id) {
        let disposable = this._listeners.get(id);
        if (disposable) {
            this._listeners.delete(id);
            disposable.dispose();
        }
    }
    dispose() {
        for (let disposable of this._listeners.values()) {
            disposable.dispose();
        }
        this._listeners.clear();
    }
    onDidChangeConfiguration(configurationSection, event) {
        let sections;
        if (Is.string(configurationSection)) {
            sections = [configurationSection];
        }
        else {
            sections = configurationSection;
        }
        if (sections !== void 0 && event !== void 0) {
            let affected = sections.some((section) => event.affectsConfiguration(section));
            if (!affected) {
                return;
            }
        }
        let didChangeConfiguration = (sections) => {
            if (sections === void 0) {
                this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeConfigurationNotification.type, { settings: null });
                return;
            }
            this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeConfigurationNotification.type, { settings: this.extractSettingsInformation(sections) });
        };
        let middleware = this.getMiddleware();
        middleware
            ? middleware(sections, didChangeConfiguration)
            : didChangeConfiguration(sections);
    }
    extractSettingsInformation(keys) {
        function ensurePath(config, path) {
            let current = config;
            for (let i = 0; i < path.length - 1; i++) {
                let obj = current[path[i]];
                if (!obj) {
                    obj = Object.create(null);
                    current[path[i]] = obj;
                }
                current = obj;
            }
            return current;
        }
        let resource = this._client.clientOptions.workspaceFolder
            ? this._client.clientOptions.workspaceFolder.uri
            : undefined;
        let result = Object.create(null);
        for (let i = 0; i < keys.length; i++) {
            let key = keys[i];
            let index = key.indexOf('.');
            let config = null;
            if (index >= 0) {
                config = vscode_1.workspace.getConfiguration(key.substr(0, index), resource).get(key.substr(index + 1));
            }
            else {
                config = vscode_1.workspace.getConfiguration(key, resource);
            }
            if (config) {
                let path = keys[i].split('.');
                ensurePath(result, path)[path[path.length - 1]] = config;
            }
        }
        return result;
    }
    getMiddleware() {
        let middleware = this._client.clientOptions.middleware;
        if (middleware.workspace && middleware.workspace.didChangeConfiguration) {
            return middleware.workspace.didChangeConfiguration;
        }
        else {
            return undefined;
        }
    }
}
class ExecuteCommandFeature {
    constructor(_client) {
        this._client = _client;
        this._commands = new Map();
    }
    get messages() {
        return vscode_languageserver_protocol_1.ExecuteCommandRequest.type;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'workspace'), 'executeCommand').dynamicRegistration = true;
    }
    initialize(capabilities) {
        if (!capabilities.executeCommandProvider) {
            return;
        }
        this.register(this.messages, {
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, capabilities.executeCommandProvider)
        });
    }
    register(_message, data) {
        let client = this._client;
        if (data.registerOptions.commands) {
            let disposeables = [];
            for (const command of data.registerOptions.commands) {
                disposeables.push(vscode_1.commands.registerCommand(command, (...args) => {
                    let params = {
                        command,
                        arguments: args
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.ExecuteCommandRequest.type, params).then(undefined, (error) => {
                        client.logFailedRequest(vscode_languageserver_protocol_1.ExecuteCommandRequest.type, error);
                    });
                }));
            }
            this._commands.set(data.id, disposeables);
        }
    }
    unregister(id) {
        let disposeables = this._commands.get(id);
        if (disposeables) {
            disposeables.forEach(disposable => disposable.dispose());
        }
    }
    dispose() {
        this._commands.forEach((value) => {
            value.forEach(disposable => disposable.dispose());
        });
        this._commands.clear();
    }
}
var MessageTransports;
(function (MessageTransports) {
    function is(value) {
        let candidate = value;
        return candidate && vscode_languageserver_protocol_1.MessageReader.is(value.reader) && vscode_languageserver_protocol_1.MessageWriter.is(value.writer);
    }
    MessageTransports.is = is;
})(MessageTransports = exports.MessageTransports || (exports.MessageTransports = {}));
class OnReady {
    constructor(_resolve, _reject) {
        this._resolve = _resolve;
        this._reject = _reject;
        this._used = false;
    }
    get isUsed() {
        return this._used;
    }
    resolve() {
        this._used = true;
        this._resolve();
    }
    reject(error) {
        this._used = true;
        this._reject(error);
    }
}
class BaseLanguageClient {
    constructor(id, name, clientOptions) {
        this._traceFormat = vscode_languageserver_protocol_1.TraceFormat.Text;
        this._features = [];
        this._method2Message = new Map();
        this._dynamicFeatures = new Map();
        this._id = id;
        this._name = name;
        clientOptions = clientOptions || {};
        this._clientOptions = {
            documentSelector: clientOptions.documentSelector || [],
            synchronize: clientOptions.synchronize || {},
            diagnosticCollectionName: clientOptions.diagnosticCollectionName,
            outputChannelName: clientOptions.outputChannelName || this._name,
            revealOutputChannelOn: clientOptions.revealOutputChannelOn || RevealOutputChannelOn.Error,
            stdioEncoding: clientOptions.stdioEncoding || 'utf8',
            initializationOptions: clientOptions.initializationOptions,
            initializationFailedHandler: clientOptions.initializationFailedHandler,
            errorHandler: clientOptions.errorHandler || new DefaultErrorHandler(this._name),
            middleware: clientOptions.middleware || {},
            uriConverters: clientOptions.uriConverters,
            workspaceFolder: clientOptions.workspaceFolder
        };
        this._clientOptions.synchronize = this._clientOptions.synchronize || {};
        this.state = ClientState.Initial;
        this._connectionPromise = undefined;
        this._resolvedConnection = undefined;
        this._initializeResult = undefined;
        if (clientOptions.outputChannel) {
            this._outputChannel = clientOptions.outputChannel;
            this._disposeOutputChannel = false;
        }
        else {
            this._outputChannel = undefined;
            this._disposeOutputChannel = true;
        }
        this._listeners = undefined;
        this._providers = undefined;
        this._diagnostics = undefined;
        this._fileEvents = [];
        this._fileEventDelayer = new async_1.Delayer(250);
        this._onReady = new Promise((resolve, reject) => {
            this._onReadyCallbacks = new OnReady(resolve, reject);
        });
        this._onStop = undefined;
        this._telemetryEmitter = new vscode_languageserver_protocol_1.Emitter();
        this._stateChangeEmitter = new vscode_languageserver_protocol_1.Emitter();
        this._tracer = {
            log: (messageOrDataObject, data) => {
                if (Is.string(messageOrDataObject)) {
                    this.logTrace(messageOrDataObject, data);
                }
                else {
                    this.logObjectTrace(messageOrDataObject);
                }
            },
        };
        this._c2p = c2p.createConverter(clientOptions.uriConverters ? clientOptions.uriConverters.code2Protocol : undefined);
        this._p2c = p2c.createConverter(clientOptions.uriConverters ? clientOptions.uriConverters.protocol2Code : undefined);
        this._syncedDocuments = new Map();
        this.registerBuiltinFeatures();
    }
    get state() {
        return this._state;
    }
    set state(value) {
        let oldState = this.getPublicState();
        this._state = value;
        let newState = this.getPublicState();
        if (newState !== oldState) {
            this._stateChangeEmitter.fire({ oldState, newState });
        }
    }
    getPublicState() {
        if (this.state === ClientState.Running) {
            return State.Running;
        }
        else if (this.state === ClientState.Starting) {
            return State.Starting;
        }
        else {
            return State.Stopped;
        }
    }
    get initializeResult() {
        return this._initializeResult;
    }
    sendRequest(type, ...params) {
        if (!this.isConnectionActive()) {
            throw new Error('Language client is not ready yet');
        }
        this.forceDocumentSync();
        try {
            return this._resolvedConnection.sendRequest(type, ...params);
        }
        catch (error) {
            this.error(`Sending request ${Is.string(type) ? type : type.method} failed.`, error);
            throw error;
        }
    }
    onRequest(type, handler) {
        if (!this.isConnectionActive()) {
            throw new Error('Language client is not ready yet');
        }
        try {
            this._resolvedConnection.onRequest(type, handler);
        }
        catch (error) {
            this.error(`Registering request handler ${Is.string(type) ? type : type.method} failed.`, error);
            throw error;
        }
    }
    sendNotification(type, params) {
        if (!this.isConnectionActive()) {
            throw new Error('Language client is not ready yet');
        }
        this.forceDocumentSync();
        try {
            this._resolvedConnection.sendNotification(type, params);
        }
        catch (error) {
            this.error(`Sending notification ${Is.string(type) ? type : type.method} failed.`, error);
            throw error;
        }
    }
    onNotification(type, handler) {
        if (!this.isConnectionActive()) {
            throw new Error('Language client is not ready yet');
        }
        try {
            this._resolvedConnection.onNotification(type, handler);
        }
        catch (error) {
            this.error(`Registering notification handler ${Is.string(type) ? type : type.method} failed.`, error);
            throw error;
        }
    }
    get clientOptions() {
        return this._clientOptions;
    }
    get protocol2CodeConverter() {
        return this._p2c;
    }
    get code2ProtocolConverter() {
        return this._c2p;
    }
    get onTelemetry() {
        return this._telemetryEmitter.event;
    }
    get onDidChangeState() {
        return this._stateChangeEmitter.event;
    }
    get outputChannel() {
        if (!this._outputChannel) {
            this._outputChannel = vscode_1.window.createOutputChannel(this._clientOptions.outputChannelName ? this._clientOptions.outputChannelName : this._name);
        }
        return this._outputChannel;
    }
    get diagnostics() {
        return this._diagnostics;
    }
    createDefaultErrorHandler() {
        return new DefaultErrorHandler(this._name);
    }
    set trace(value) {
        this._trace = value;
        this.onReady().then(() => {
            this.resolveConnection().then((connection) => {
                connection.trace(this._trace, this._tracer, {
                    sendNotification: false,
                    traceFormat: this._traceFormat
                });
            });
        }, () => {
        });
    }
    data2String(data) {
        if (data instanceof vscode_languageserver_protocol_1.ResponseError) {
            const responseError = data;
            return `  Message: ${responseError.message}\n  Code: ${responseError.code} ${responseError.data ? '\n' + responseError.data.toString() : ''}`;
        }
        if (data instanceof Error) {
            if (Is.string(data.stack)) {
                return data.stack;
            }
            return data.message;
        }
        if (Is.string(data)) {
            return data;
        }
        return data.toString();
    }
    info(message, data) {
        this.outputChannel.appendLine(`[Info  - ${(new Date().toLocaleTimeString())}] ${message}`);
        if (data) {
            this.outputChannel.appendLine(this.data2String(data));
        }
        if (this._clientOptions.revealOutputChannelOn <= RevealOutputChannelOn.Info) {
            this.outputChannel.show(true);
        }
    }
    warn(message, data) {
        this.outputChannel.appendLine(`[Warn  - ${(new Date().toLocaleTimeString())}] ${message}`);
        if (data) {
            this.outputChannel.appendLine(this.data2String(data));
        }
        if (this._clientOptions.revealOutputChannelOn <= RevealOutputChannelOn.Warn) {
            this.outputChannel.show(true);
        }
    }
    error(message, data) {
        this.outputChannel.appendLine(`[Error - ${(new Date().toLocaleTimeString())}] ${message}`);
        if (data) {
            this.outputChannel.appendLine(this.data2String(data));
        }
        if (this._clientOptions.revealOutputChannelOn <= RevealOutputChannelOn.Error) {
            this.outputChannel.show(true);
        }
    }
    logTrace(message, data) {
        this.outputChannel.appendLine(`[Trace - ${(new Date().toLocaleTimeString())}] ${message}`);
        if (data) {
            this.outputChannel.appendLine(this.data2String(data));
        }
    }
    logObjectTrace(data) {
        if (data.isLSPMessage && data.type) {
            this.outputChannel.append(`[LSP   - ${(new Date().toLocaleTimeString())}] `);
        }
        else {
            this.outputChannel.append(`[Trace - ${(new Date().toLocaleTimeString())}] `);
        }
        if (data) {
            this.outputChannel.appendLine(`${JSON.stringify(data)}`);
        }
    }
    needsStart() {
        return this.state === ClientState.Initial || this.state === ClientState.Stopping || this.state === ClientState.Stopped;
    }
    needsStop() {
        return this.state === ClientState.Starting || this.state === ClientState.Running;
    }
    onReady() {
        return this._onReady;
    }
    isConnectionActive() {
        return this.state === ClientState.Running && !!this._resolvedConnection;
    }
    start() {
        if (this._onReadyCallbacks.isUsed) {
            this._onReady = new Promise((resolve, reject) => {
                this._onReadyCallbacks = new OnReady(resolve, reject);
            });
        }
        this._listeners = [];
        this._providers = [];
        // If we restart then the diagnostics collection is reused.
        if (!this._diagnostics) {
            this._diagnostics = this._clientOptions.diagnosticCollectionName
                ? vscode_1.languages.createDiagnosticCollection(this._clientOptions.diagnosticCollectionName)
                : vscode_1.languages.createDiagnosticCollection();
        }
        this.state = ClientState.Starting;
        this.resolveConnection().then((connection) => {
            connection.onLogMessage((message) => {
                switch (message.type) {
                    case vscode_languageserver_protocol_1.MessageType.Error:
                        this.error(message.message);
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Warning:
                        this.warn(message.message);
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Info:
                        this.info(message.message);
                        break;
                    default:
                        this.outputChannel.appendLine(message.message);
                }
            });
            connection.onShowMessage((message) => {
                switch (message.type) {
                    case vscode_languageserver_protocol_1.MessageType.Error:
                        vscode_1.window.showErrorMessage(message.message);
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Warning:
                        vscode_1.window.showWarningMessage(message.message);
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Info:
                        vscode_1.window.showInformationMessage(message.message);
                        break;
                    default:
                        vscode_1.window.showInformationMessage(message.message);
                }
            });
            connection.onRequest(vscode_languageserver_protocol_1.ShowMessageRequest.type, (params) => {
                let messageFunc;
                switch (params.type) {
                    case vscode_languageserver_protocol_1.MessageType.Error:
                        messageFunc = vscode_1.window.showErrorMessage;
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Warning:
                        messageFunc = vscode_1.window.showWarningMessage;
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Info:
                        messageFunc = vscode_1.window.showInformationMessage;
                        break;
                    default:
                        messageFunc = vscode_1.window.showInformationMessage;
                }
                let actions = params.actions || [];
                return messageFunc(params.message, ...actions);
            });
            connection.onTelemetry((data) => {
                this._telemetryEmitter.fire(data);
            });
            connection.listen();
            // Error is handled in the initialize call.
            return this.initialize(connection);
        }).then(undefined, (error) => {
            this.state = ClientState.StartFailed;
            this._onReadyCallbacks.reject(error);
            this.error('Starting client failed', error);
            vscode_1.window.showErrorMessage(`Couldn't start client ${this._name}`);
        });
        return new vscode_1.Disposable(() => {
            if (this.needsStop()) {
                this.stop();
            }
        });
    }
    resolveConnection() {
        if (!this._connectionPromise) {
            this._connectionPromise = this.createConnection();
        }
        return this._connectionPromise;
    }
    initialize(connection) {
        this.refreshTrace(connection, false);
        let initOption = this._clientOptions.initializationOptions;
        let rootPath = this._clientOptions.workspaceFolder
            ? this._clientOptions.workspaceFolder.uri.fsPath
            : this._clientGetRootPath();
        let initParams = {
            processId: process.pid,
            rootPath: rootPath ? rootPath : null,
            rootUri: rootPath ? this._c2p.asUri(vscode_1.Uri.file(rootPath)) : null,
            capabilities: this.computeClientCapabilities(),
            initializationOptions: Is.func(initOption) ? initOption() : initOption,
            trace: vscode_languageserver_protocol_1.Trace.toString(this._trace),
            workspaceFolders: null
        };
        this.fillInitializeParams(initParams);
        return connection.initialize(initParams).then((result) => {
            this._resolvedConnection = connection;
            this._initializeResult = result;
            this.state = ClientState.Running;
            let textDocumentSyncOptions = undefined;
            if (Is.number(result.capabilities.textDocumentSync)) {
                if (result.capabilities.textDocumentSync === vscode_languageserver_protocol_1.TextDocumentSyncKind.None) {
                    textDocumentSyncOptions = {
                        openClose: false,
                        change: vscode_languageserver_protocol_1.TextDocumentSyncKind.None,
                        save: undefined
                    };
                }
                else {
                    textDocumentSyncOptions = {
                        openClose: true,
                        change: result.capabilities.textDocumentSync,
                        save: {
                            includeText: false
                        }
                    };
                }
            }
            else if (result.capabilities.textDocumentSync !== void 0 && result.capabilities.textDocumentSync !== null) {
                textDocumentSyncOptions = result.capabilities.textDocumentSync;
            }
            this._capabilities = Object.assign({}, result.capabilities, { resolvedTextDocumentSync: textDocumentSyncOptions });
            connection.onDiagnostics(params => this.handleDiagnostics(params));
            connection.onRequest(vscode_languageserver_protocol_1.RegistrationRequest.type, params => this.handleRegistrationRequest(params));
            // See https://github.com/Microsoft/vscode-languageserver-node/issues/199
            connection.onRequest('client/registerFeature', params => this.handleRegistrationRequest(params));
            connection.onRequest(vscode_languageserver_protocol_1.UnregistrationRequest.type, params => this.handleUnregistrationRequest(params));
            // See https://github.com/Microsoft/vscode-languageserver-node/issues/199
            connection.onRequest('client/unregisterFeature', params => this.handleUnregistrationRequest(params));
            connection.onRequest(vscode_languageserver_protocol_1.ApplyWorkspaceEditRequest.type, params => this.handleApplyWorkspaceEdit(params));
            connection.sendNotification(vscode_languageserver_protocol_1.InitializedNotification.type, {});
            this.hookFileEvents(connection);
            this.hookConfigurationChanged(connection);
            this.initializeFeatures(connection);
            this._onReadyCallbacks.resolve();
            return result;
        }).then(undefined, (error) => {
            if (this._clientOptions.initializationFailedHandler) {
                if (this._clientOptions.initializationFailedHandler(error)) {
                    this.initialize(connection);
                }
                else {
                    this.stop();
                    this._onReadyCallbacks.reject(error);
                }
            }
            else if (error instanceof vscode_languageserver_protocol_1.ResponseError && error.data && error.data.retry) {
                vscode_1.window.showErrorMessage(error.message, { title: 'Retry', id: "retry" }).then(item => {
                    if (item && item.id === 'retry') {
                        this.initialize(connection);
                    }
                    else {
                        this.stop();
                        this._onReadyCallbacks.reject(error);
                    }
                });
            }
            else {
                if (error && error.message) {
                    vscode_1.window.showErrorMessage(error.message);
                }
                this.error('Server initialization failed.', error);
                this.stop();
                this._onReadyCallbacks.reject(error);
            }
        });
    }
    _clientGetRootPath() {
        let folders = vscode_1.workspace.workspaceFolders;
        if (!folders || folders.length === 0) {
            return undefined;
        }
        let folder = folders[0];
        if (folder.uri.scheme === 'file') {
            return folder.uri.fsPath;
        }
        return undefined;
    }
    stop() {
        this._initializeResult = undefined;
        if (!this._connectionPromise) {
            this.state = ClientState.Stopped;
            return Promise.resolve();
        }
        if (this.state === ClientState.Stopping && this._onStop) {
            return this._onStop;
        }
        this.state = ClientState.Stopping;
        this.cleanUp();
        // unhook listeners
        return this._onStop = this.resolveConnection().then(connection => {
            return connection.shutdown().then(() => {
                connection.exit();
                connection.dispose();
                this.state = ClientState.Stopped;
                this._onStop = undefined;
                this._connectionPromise = undefined;
                this._resolvedConnection = undefined;
            });
        });
    }
    cleanUp(channel = true, diagnostics = true) {
        if (this._listeners) {
            this._listeners.forEach(listener => listener.dispose());
            this._listeners = undefined;
        }
        if (this._providers) {
            this._providers.forEach(provider => provider.dispose());
            this._providers = undefined;
        }
        if (this._syncedDocuments) {
            this._syncedDocuments.clear();
        }
        for (let handler of this._dynamicFeatures.values()) {
            handler.dispose();
        }
        if (channel && this._outputChannel && this._disposeOutputChannel) {
            this._outputChannel.dispose();
            this._outputChannel = undefined;
        }
        if (diagnostics && this._diagnostics) {
            this._diagnostics.dispose();
            this._diagnostics = undefined;
        }
    }
    notifyFileEvent(event) {
        this._fileEvents.push(event);
        this._fileEventDelayer.trigger(() => {
            this.onReady().then(() => {
                this.resolveConnection().then(connection => {
                    if (this.isConnectionActive()) {
                        connection.didChangeWatchedFiles({ changes: this._fileEvents });
                    }
                    this._fileEvents = [];
                });
            }, (error) => {
                this.error(`Notify file events failed.`, error);
            });
        });
    }
    forceDocumentSync() {
        this._dynamicFeatures.get(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type.method).forceDelivery();
    }
    handleDiagnostics(params) {
        if (!this._diagnostics) {
            return;
        }
        let uri = this._p2c.asUri(params.uri);
        let diagnostics = this._p2c.asDiagnostics(params.diagnostics);
        let middleware = this.clientOptions.middleware.handleDiagnostics;
        if (middleware) {
            middleware(uri, diagnostics, (uri, diagnostics) => this.setDiagnostics(uri, diagnostics));
        }
        else {
            this.setDiagnostics(uri, diagnostics);
        }
    }
    setDiagnostics(uri, diagnostics) {
        if (!this._diagnostics) {
            return;
        }
        this._diagnostics.set(uri, diagnostics);
    }
    createConnection() {
        let errorHandler = (error, message, count) => {
            this.handleConnectionError(error, message, count);
        };
        let closeHandler = () => {
            this.handleConnectionClosed();
        };
        return this.createMessageTransports(this._clientOptions.stdioEncoding || 'utf8').then((transports) => {
            return createConnection(transports.reader, transports.writer, errorHandler, closeHandler);
        });
    }
    handleConnectionClosed() {
        // Check whether this is a normal shutdown in progress or the client stopped normally.
        if (this.state === ClientState.Stopping || this.state === ClientState.Stopped) {
            return;
        }
        try {
            if (this._resolvedConnection) {
                this._resolvedConnection.dispose();
            }
        }
        catch (error) {
            // Disposing a connection could fail if error cases.
        }
        let action = CloseAction.DoNotRestart;
        try {
            action = this._clientOptions.errorHandler.closed();
        }
        catch (error) {
            // Ignore errors coming from the error handler.
        }
        this._connectionPromise = undefined;
        this._resolvedConnection = undefined;
        if (action === CloseAction.DoNotRestart) {
            this.error('Connection to server got closed. Server will not be restarted.');
            this.state = ClientState.Stopped;
            this.cleanUp(false, true);
        }
        else if (action === CloseAction.Restart) {
            this.info('Connection to server got closed. Server will restart.');
            this.cleanUp(false, false);
            this.state = ClientState.Initial;
            this.start();
        }
    }
    handleConnectionError(error, message, count) {
        let action = this._clientOptions.errorHandler.error(error, message, count);
        if (action === ErrorAction.Shutdown) {
            this.error('Connection to server is erroring. Shutting down server.');
            this.stop();
        }
    }
    hookConfigurationChanged(connection) {
        vscode_1.workspace.onDidChangeConfiguration(() => {
            this.refreshTrace(connection, true);
        });
    }
    refreshTrace(connection, sendNotification = false) {
        let config = vscode_1.workspace.getConfiguration(this._id);
        let trace = vscode_languageserver_protocol_1.Trace.Off;
        let traceFormat = vscode_languageserver_protocol_1.TraceFormat.Text;
        if (config) {
            const traceConfig = config.get('trace.server', 'off');
            if (typeof traceConfig === 'string') {
                trace = vscode_languageserver_protocol_1.Trace.fromString(traceConfig);
            }
            else {
                trace = vscode_languageserver_protocol_1.Trace.fromString(config.get('trace.server.verbosity', 'off'));
                traceFormat = vscode_languageserver_protocol_1.TraceFormat.fromString(config.get('trace.server.format', 'text'));
            }
        }
        this._trace = trace;
        this._traceFormat = traceFormat;
        connection.trace(this._trace, this._tracer, {
            sendNotification,
            traceFormat: this._traceFormat
        });
    }
    hookFileEvents(_connection) {
        let fileEvents = this._clientOptions.synchronize.fileEvents;
        if (!fileEvents) {
            return;
        }
        let watchers;
        if (Is.array(fileEvents)) {
            watchers = fileEvents;
        }
        else {
            watchers = [fileEvents];
        }
        if (!watchers) {
            return;
        }
        this._dynamicFeatures.get(vscode_languageserver_protocol_1.DidChangeWatchedFilesNotification.type.method).registerRaw(UUID.generateUuid(), watchers);
    }
    registerFeatures(features) {
        for (let feature of features) {
            this.registerFeature(feature);
        }
    }
    registerFeature(feature) {
        this._features.push(feature);
        if (DynamicFeature.is(feature)) {
            let messages = feature.messages;
            if (Array.isArray(messages)) {
                for (let message of messages) {
                    this._method2Message.set(message.method, message);
                    this._dynamicFeatures.set(message.method, feature);
                }
            }
            else {
                this._method2Message.set(messages.method, messages);
                this._dynamicFeatures.set(messages.method, feature);
            }
        }
    }
    registerBuiltinFeatures() {
        this.registerFeature(new ConfigurationFeature(this));
        this.registerFeature(new DidOpenTextDocumentFeature(this, this._syncedDocuments));
        this.registerFeature(new DidChangeTextDocumentFeature(this));
        this.registerFeature(new WillSaveFeature(this));
        this.registerFeature(new WillSaveWaitUntilFeature(this));
        this.registerFeature(new DidSaveTextDocumentFeature(this));
        this.registerFeature(new DidCloseTextDocumentFeature(this, this._syncedDocuments));
        this.registerFeature(new FileSystemWatcherFeature(this, (event) => this.notifyFileEvent(event)));
        this.registerFeature(new CompletionItemFeature(this));
        this.registerFeature(new HoverFeature(this));
        this.registerFeature(new SignatureHelpFeature(this));
        this.registerFeature(new DefinitionFeature(this));
        this.registerFeature(new ReferencesFeature(this));
        this.registerFeature(new DocumentHighlightFeature(this));
        this.registerFeature(new DocumentSymbolFeature(this));
        this.registerFeature(new WorkspaceSymbolFeature(this));
        this.registerFeature(new CodeActionFeature(this));
        this.registerFeature(new CodeLensFeature(this));
        this.registerFeature(new DocumentFormattingFeature(this));
        this.registerFeature(new DocumentRangeFormattingFeature(this));
        this.registerFeature(new DocumentOnTypeFormattingFeature(this));
        this.registerFeature(new RenameFeature(this));
        this.registerFeature(new DocumentLinkFeature(this));
        this.registerFeature(new ExecuteCommandFeature(this));
    }
    fillInitializeParams(params) {
        for (let feature of this._features) {
            if (Is.func(feature.fillInitializeParams)) {
                feature.fillInitializeParams(params);
            }
        }
    }
    computeClientCapabilities() {
        let result = {};
        ensure(result, 'workspace').applyEdit = true;
        let workspaceEdit = ensure(ensure(result, 'workspace'), 'workspaceEdit');
        workspaceEdit.documentChanges = true;
        workspaceEdit.resourceOperations = [vscode_languageserver_protocol_1.ResourceOperationKind.Create, vscode_languageserver_protocol_1.ResourceOperationKind.Rename, vscode_languageserver_protocol_1.ResourceOperationKind.Delete];
        workspaceEdit.failureHandling = vscode_languageserver_protocol_1.FailureHandlingKind.TextOnlyTransactional;
        ensure(ensure(result, 'textDocument'), 'publishDiagnostics').relatedInformation = true;
        for (let feature of this._features) {
            feature.fillClientCapabilities(result);
        }
        return result;
    }
    initializeFeatures(_connection) {
        let documentSelector = this._clientOptions.documentSelector;
        for (let feature of this._features) {
            feature.initialize(this._capabilities, documentSelector);
        }
    }
    handleRegistrationRequest(params) {
        return new Promise((resolve, reject) => {
            for (let registration of params.registrations) {
                const feature = this._dynamicFeatures.get(registration.method);
                if (!feature) {
                    reject(new Error(`No feature implementation for ${registration.method} found. Registration failed.`));
                    return;
                }
                const options = registration.registerOptions || {};
                options.documentSelector = options.documentSelector || this._clientOptions.documentSelector;
                const data = {
                    id: registration.id,
                    registerOptions: options
                };
                feature.register(this._method2Message.get(registration.method), data);
            }
            resolve();
        });
    }
    handleUnregistrationRequest(params) {
        return new Promise((resolve, reject) => {
            for (let unregistration of params.unregisterations) {
                const feature = this._dynamicFeatures.get(unregistration.method);
                if (!feature) {
                    reject(new Error(`No feature implementation for ${unregistration.method} found. Unregistration failed.`));
                    return;
                }
                feature.unregister(unregistration.id);
            }
            ;
            resolve();
        });
    }
    handleApplyWorkspaceEdit(params) {
        // This is some sort of workaround since the version check should be done by VS Code in the Workspace.applyEdit.
        // However doing it here adds some safety since the server can lag more behind then an extension.
        let workspaceEdit = params.edit;
        let openTextDocuments = new Map();
        vscode_1.workspace.textDocuments.forEach((document) => openTextDocuments.set(document.uri.toString(), document));
        let versionMismatch = false;
        if (workspaceEdit.documentChanges) {
            for (const change of workspaceEdit.documentChanges) {
                if (vscode_languageserver_protocol_1.TextDocumentEdit.is(change) && change.textDocument.version && change.textDocument.version >= 0) {
                    let textDocument = openTextDocuments.get(change.textDocument.uri);
                    if (textDocument && textDocument.version !== change.textDocument.version) {
                        versionMismatch = true;
                        break;
                    }
                }
            }
        }
        if (versionMismatch) {
            return Promise.resolve({ applied: false });
        }
        return vscode_1.workspace.applyEdit(this._p2c.asWorkspaceEdit(params.edit)).then((value) => { return { applied: value }; });
    }
    ;
    logFailedRequest(type, error) {
        // If we get a request cancel don't log anything.
        if (error instanceof vscode_languageserver_protocol_1.ResponseError && error.code === vscode_languageserver_protocol_1.ErrorCodes.RequestCancelled) {
            return;
        }
        this.error(`Request ${type.method} failed.`, error);
    }
}
exports.BaseLanguageClient = BaseLanguageClient;


/***/ }),

/***/ 103:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(496);
const proto = __webpack_require__(192);
const Is = __webpack_require__(97);
const protocolCompletionItem_1 = __webpack_require__(213);
const protocolCodeLens_1 = __webpack_require__(628);
const protocolDocumentLink_1 = __webpack_require__(653);
function createConverter(uriConverter) {
    const nullConverter = (value) => value.toString();
    const _uriConverter = uriConverter || nullConverter;
    function asUri(value) {
        return _uriConverter(value);
    }
    function asTextDocumentIdentifier(textDocument) {
        return {
            uri: _uriConverter(textDocument.uri)
        };
    }
    function asVersionedTextDocumentIdentifier(textDocument) {
        return {
            uri: _uriConverter(textDocument.uri),
            version: textDocument.version
        };
    }
    function asOpenTextDocumentParams(textDocument) {
        return {
            textDocument: {
                uri: _uriConverter(textDocument.uri),
                languageId: textDocument.languageId,
                version: textDocument.version,
                text: textDocument.getText()
            }
        };
    }
    function isTextDocumentChangeEvent(value) {
        let candidate = value;
        return !!candidate.document && !!candidate.contentChanges;
    }
    function isTextDocument(value) {
        let candidate = value;
        return !!candidate.uri && !!candidate.version;
    }
    function asChangeTextDocumentParams(arg) {
        if (isTextDocument(arg)) {
            let result = {
                textDocument: {
                    uri: _uriConverter(arg.uri),
                    version: arg.version
                },
                contentChanges: [{ text: arg.getText() }]
            };
            return result;
        }
        else if (isTextDocumentChangeEvent(arg)) {
            let document = arg.document;
            let result = {
                textDocument: {
                    uri: _uriConverter(document.uri),
                    version: document.version
                },
                contentChanges: arg.contentChanges.map((change) => {
                    let range = change.range;
                    return {
                        range: {
                            start: { line: range.start.line, character: range.start.character },
                            end: { line: range.end.line, character: range.end.character }
                        },
                        rangeLength: change.rangeLength,
                        text: change.text
                    };
                })
            };
            return result;
        }
        else {
            throw Error('Unsupported text document change parameter');
        }
    }
    function asCloseTextDocumentParams(textDocument) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
    }
    function asSaveTextDocumentParams(textDocument, includeContent = false) {
        let result = {
            textDocument: asVersionedTextDocumentIdentifier(textDocument)
        };
        if (includeContent) {
            result.text = textDocument.getText();
        }
        return result;
    }
    function asTextDocumentSaveReason(reason) {
        switch (reason) {
            case code.TextDocumentSaveReason.Manual:
                return proto.TextDocumentSaveReason.Manual;
            case code.TextDocumentSaveReason.AfterDelay:
                return proto.TextDocumentSaveReason.AfterDelay;
            case code.TextDocumentSaveReason.FocusOut:
                return proto.TextDocumentSaveReason.FocusOut;
        }
        return proto.TextDocumentSaveReason.Manual;
    }
    function asWillSaveTextDocumentParams(event) {
        return {
            textDocument: asTextDocumentIdentifier(event.document),
            reason: asTextDocumentSaveReason(event.reason)
        };
    }
    function asTextDocumentPositionParams(textDocument, position) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument),
            position: asWorkerPosition(position)
        };
    }
    function asTriggerKind(triggerKind) {
        switch (triggerKind) {
            case code.CompletionTriggerKind.TriggerCharacter:
                return proto.CompletionTriggerKind.TriggerCharacter;
            case code.CompletionTriggerKind.TriggerForIncompleteCompletions:
                return proto.CompletionTriggerKind.TriggerForIncompleteCompletions;
            default:
                return proto.CompletionTriggerKind.Invoked;
        }
    }
    function asCompletionParams(textDocument, position, context) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument),
            position: asWorkerPosition(position),
            context: {
                triggerKind: asTriggerKind(context.triggerKind),
                triggerCharacter: context.triggerCharacter
            }
        };
    }
    function asWorkerPosition(position) {
        return { line: position.line, character: position.character };
    }
    function asPosition(value) {
        if (value === void 0) {
            return undefined;
        }
        else if (value === null) {
            return null;
        }
        return { line: value.line, character: value.character };
    }
    function asRange(value) {
        if (value === void 0 || value === null) {
            return value;
        }
        return { start: asPosition(value.start), end: asPosition(value.end) };
    }
    function asDiagnosticSeverity(value) {
        switch (value) {
            case code.DiagnosticSeverity.Error:
                return proto.DiagnosticSeverity.Error;
            case code.DiagnosticSeverity.Warning:
                return proto.DiagnosticSeverity.Warning;
            case code.DiagnosticSeverity.Information:
                return proto.DiagnosticSeverity.Information;
            case code.DiagnosticSeverity.Hint:
                return proto.DiagnosticSeverity.Hint;
        }
    }
    function asDiagnostic(item) {
        let result = proto.Diagnostic.create(asRange(item.range), item.message);
        if (Is.number(item.severity)) {
            result.severity = asDiagnosticSeverity(item.severity);
        }
        if (Is.number(item.code) || Is.string(item.code)) {
            result.code = item.code;
        }
        if (item.source) {
            result.source = item.source;
        }
        return result;
    }
    function asDiagnostics(items) {
        if (items === void 0 || items === null) {
            return items;
        }
        return items.map(asDiagnostic);
    }
    function asDocumentation(format, documentation) {
        switch (format) {
            case '$string':
                return documentation;
            case proto.MarkupKind.PlainText:
                return { kind: format, value: documentation };
            case proto.MarkupKind.Markdown:
                return { kind: format, value: documentation.value };
            default:
                return `Unsupported Markup content received. Kind is: ${format}`;
        }
    }
    function asCompletionItemKind(value, original) {
        if (original !== void 0) {
            return original;
        }
        return value + 1;
    }
    function asCompletionItem(item) {
        let result = { label: item.label };
        let protocolItem = item instanceof protocolCompletionItem_1.default ? item : undefined;
        if (item.detail) {
            result.detail = item.detail;
        }
        // We only send items back we created. So this can't be something else than
        // a string right now.
        if (item.documentation) {
            if (!protocolItem || protocolItem.documentationFormat === '$string') {
                result.documentation = item.documentation;
            }
            else {
                result.documentation = asDocumentation(protocolItem.documentationFormat, item.documentation);
            }
        }
        if (item.filterText) {
            result.filterText = item.filterText;
        }
        fillPrimaryInsertText(result, item);
        if (Is.number(item.kind)) {
            result.kind = asCompletionItemKind(item.kind, protocolItem && protocolItem.originalItemKind);
        }
        if (item.sortText) {
            result.sortText = item.sortText;
        }
        if (item.additionalTextEdits) {
            result.additionalTextEdits = asTextEdits(item.additionalTextEdits);
        }
        if (item.commitCharacters) {
            result.commitCharacters = item.commitCharacters.slice();
        }
        if (item.command) {
            result.command = asCommand(item.command);
        }
        if (item.preselect === true || item.preselect === false) {
            result.preselect = item.preselect;
        }
        if (protocolItem) {
            if (protocolItem.data !== void 0) {
                result.data = protocolItem.data;
            }
            if (protocolItem.deprecated === true || protocolItem.deprecated === false) {
                result.deprecated = protocolItem.deprecated;
            }
        }
        return result;
    }
    function fillPrimaryInsertText(target, source) {
        let format = proto.InsertTextFormat.PlainText;
        let text;
        let range = undefined;
        if (source.textEdit) {
            text = source.textEdit.newText;
            range = asRange(source.textEdit.range);
        }
        else if (source.insertText instanceof code.SnippetString) {
            format = proto.InsertTextFormat.Snippet;
            text = source.insertText.value;
        }
        else {
            text = source.insertText;
        }
        if (source.range) {
            range = asRange(source.range);
        }
        target.insertTextFormat = format;
        if (source.fromEdit && text && range) {
            target.textEdit = { newText: text, range: range };
        }
        else {
            target.insertText = text;
        }
    }
    function asTextEdit(edit) {
        return { range: asRange(edit.range), newText: edit.newText };
    }
    function asTextEdits(edits) {
        if (edits === void 0 || edits === null) {
            return edits;
        }
        return edits.map(asTextEdit);
    }
    function asReferenceParams(textDocument, position, options) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument),
            position: asWorkerPosition(position),
            context: { includeDeclaration: options.includeDeclaration }
        };
    }
    function asCodeActionContext(context) {
        if (context === void 0 || context === null) {
            return context;
        }
        return proto.CodeActionContext.create(asDiagnostics(context.diagnostics), Is.string(context.only) ? [context.only] : undefined);
    }
    function asCommand(item) {
        let result = proto.Command.create(item.title, item.command);
        if (item.arguments) {
            result.arguments = item.arguments;
        }
        return result;
    }
    function asCodeLens(item) {
        let result = proto.CodeLens.create(asRange(item.range));
        if (item.command) {
            result.command = asCommand(item.command);
        }
        if (item instanceof protocolCodeLens_1.default) {
            if (item.data) {
                result.data = item.data;
            }
            ;
        }
        return result;
    }
    function asFormattingOptions(item) {
        return { tabSize: item.tabSize, insertSpaces: item.insertSpaces };
    }
    function asDocumentSymbolParams(textDocument) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
    }
    function asCodeLensParams(textDocument) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
    }
    function asDocumentLink(item) {
        let result = proto.DocumentLink.create(asRange(item.range));
        if (item.target) {
            result.target = asUri(item.target);
        }
        let protocolItem = item instanceof protocolDocumentLink_1.default ? item : undefined;
        if (protocolItem && protocolItem.data) {
            result.data = protocolItem.data;
        }
        return result;
    }
    function asDocumentLinkParams(textDocument) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
    }
    return {
        asUri,
        asTextDocumentIdentifier,
        asVersionedTextDocumentIdentifier,
        asOpenTextDocumentParams,
        asChangeTextDocumentParams,
        asCloseTextDocumentParams,
        asSaveTextDocumentParams,
        asWillSaveTextDocumentParams,
        asTextDocumentPositionParams,
        asCompletionParams,
        asWorkerPosition,
        asRange,
        asPosition,
        asDiagnosticSeverity,
        asDiagnostic,
        asDiagnostics,
        asCompletionItem,
        asTextEdit,
        asReferenceParams,
        asCodeActionContext,
        asCommand,
        asCodeLens,
        asFormattingOptions,
        asDocumentSymbolParams,
        asCodeLensParams,
        asDocumentLink,
        asDocumentLinkParams
    };
}
exports.createConverter = createConverter;


/***/ }),

/***/ 194:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const UUID = __webpack_require__(884);
const Is = __webpack_require__(97);
const vscode_1 = __webpack_require__(496);
const vscode_languageserver_protocol_1 = __webpack_require__(192);
const client_1 = __webpack_require__(620);
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
class ColorProviderFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentColorRequest.type);
    }
    fillClientCapabilities(capabilites) {
        ensure(ensure(capabilites, 'textDocument'), 'colorProvider').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.colorProvider) {
            return;
        }
        const implCapabilities = capabilities.colorProvider;
        const id = Is.string(implCapabilities.id) && implCapabilities.id.length > 0 ? implCapabilities.id : UUID.generateUuid();
        const selector = implCapabilities.documentSelector || documentSelector;
        if (selector) {
            this.register(this.messages, {
                id,
                registerOptions: Object.assign({}, { documentSelector: selector })
            });
        }
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideColorPresentations = (color, context, token) => {
            const requestParams = {
                color,
                textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(context.document),
                range: client.code2ProtocolConverter.asRange(context.range)
            };
            return client.sendRequest(vscode_languageserver_protocol_1.ColorPresentationRequest.type, requestParams, token).then(this.asColorPresentations.bind(this), (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.ColorPresentationRequest.type, error);
                return Promise.resolve(null);
            });
        };
        let provideDocumentColors = (document, token) => {
            const requestParams = {
                textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document)
            };
            return client.sendRequest(vscode_languageserver_protocol_1.DocumentColorRequest.type, requestParams, token).then(this.asColorInformations.bind(this), (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.ColorPresentationRequest.type, error);
                return Promise.resolve(null);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerColorProvider(options.documentSelector, {
            provideColorPresentations: (color, context, token) => {
                return middleware.provideColorPresentations
                    ? middleware.provideColorPresentations(color, context, token, provideColorPresentations)
                    : provideColorPresentations(color, context, token);
            },
            provideDocumentColors: (document, token) => {
                return middleware.provideDocumentColors
                    ? middleware.provideDocumentColors(document, token, provideDocumentColors)
                    : provideDocumentColors(document, token);
            }
        });
    }
    asColor(color) {
        return new vscode_1.Color(color.red, color.green, color.blue, color.alpha);
    }
    asColorInformations(colorInformation) {
        if (Array.isArray(colorInformation)) {
            return colorInformation.map(ci => {
                return new vscode_1.ColorInformation(this._client.protocol2CodeConverter.asRange(ci.range), this.asColor(ci.color));
            });
        }
        return [];
    }
    asColorPresentations(colorPresentations) {
        if (Array.isArray(colorPresentations)) {
            return colorPresentations.map(cp => {
                let presentation = new vscode_1.ColorPresentation(cp.label);
                presentation.additionalTextEdits = this._client.protocol2CodeConverter.asTextEdits(cp.additionalTextEdits);
                presentation.textEdit = this._client.protocol2CodeConverter.asTextEdit(cp.textEdit);
                return presentation;
            });
        }
        return [];
    }
}
exports.ColorProviderFeature = ColorProviderFeature;


/***/ }),

/***/ 888:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const vscode_1 = __webpack_require__(496);
const vscode_languageserver_protocol_1 = __webpack_require__(192);
class ConfigurationFeature {
    constructor(_client) {
        this._client = _client;
    }
    fillClientCapabilities(capabilities) {
        capabilities.workspace = capabilities.workspace || {};
        capabilities.workspace.configuration = true;
    }
    initialize() {
        let client = this._client;
        client.onRequest(vscode_languageserver_protocol_1.ConfigurationRequest.type, (params, token) => {
            let configuration = (params) => {
                let result = [];
                for (let item of params.items) {
                    let resource = item.scopeUri !== void 0 && item.scopeUri !== null ? this._client.protocol2CodeConverter.asUri(item.scopeUri) : undefined;
                    result.push(this.getConfiguration(resource, item.section !== null ? item.section : undefined));
                }
                return result;
            };
            let middleware = client.clientOptions.middleware.workspace;
            return middleware && middleware.configuration
                ? middleware.configuration(params, token, configuration)
                : configuration(params, token);
        });
    }
    getConfiguration(resource, section) {
        let result = null;
        if (section) {
            let index = section.lastIndexOf('.');
            if (index === -1) {
                result = vscode_1.workspace.getConfiguration(undefined, resource).get(section);
            }
            else {
                let config = vscode_1.workspace.getConfiguration(section.substr(0, index));
                if (config) {
                    result = config.get(section.substr(index + 1));
                }
            }
        }
        else {
            let config = vscode_1.workspace.getConfiguration(undefined, resource);
            result = {};
            for (let key of Object.keys(config)) {
                if (config.has(key)) {
                    result[key] = config.get(key);
                }
            }
        }
        if (!result) {
            return null;
        }
        return result;
    }
}
exports.ConfigurationFeature = ConfigurationFeature;


/***/ }),

/***/ 812:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const UUID = __webpack_require__(884);
const Is = __webpack_require__(97);
const vscode_1 = __webpack_require__(496);
const vscode_languageserver_protocol_1 = __webpack_require__(192);
const client_1 = __webpack_require__(620);
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
class DeclarationFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DeclarationRequest.type);
    }
    fillClientCapabilities(capabilites) {
        let declarationSupport = ensure(ensure(capabilites, 'textDocument'), 'declaration');
        declarationSupport.dynamicRegistration = true;
        declarationSupport.linkSupport = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.declarationProvider) {
            return;
        }
        if (capabilities.declarationProvider === true) {
            if (!documentSelector) {
                return;
            }
            this.register(this.messages, {
                id: UUID.generateUuid(),
                registerOptions: Object.assign({}, { documentSelector: documentSelector })
            });
        }
        else {
            const declCapabilities = capabilities.declarationProvider;
            const id = Is.string(declCapabilities.id) && declCapabilities.id.length > 0 ? declCapabilities.id : UUID.generateUuid();
            const selector = declCapabilities.documentSelector || documentSelector;
            if (selector) {
                this.register(this.messages, {
                    id,
                    registerOptions: Object.assign({}, { documentSelector: selector })
                });
            }
        }
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideDeclaration = (document, position, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.DeclarationRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asDeclarationResult, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.DeclarationRequest.type, error);
                return Promise.resolve(null);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerDeclarationProvider(options.documentSelector, {
            provideDeclaration: (document, position, token) => {
                return middleware.provideDeclaration
                    ? middleware.provideDeclaration(document, position, token, provideDeclaration)
                    : provideDeclaration(document, position, token);
            }
        });
    }
}
exports.DeclarationFeature = DeclarationFeature;


/***/ }),

/***/ 573:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const UUID = __webpack_require__(884);
const Is = __webpack_require__(97);
const vscode_1 = __webpack_require__(496);
const vscode_languageserver_protocol_1 = __webpack_require__(192);
const client_1 = __webpack_require__(620);
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
class FoldingRangeFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.FoldingRangeRequest.type);
    }
    fillClientCapabilities(capabilites) {
        let capability = ensure(ensure(capabilites, 'textDocument'), 'foldingRange');
        capability.dynamicRegistration = true;
        capability.rangeLimit = 5000;
        capability.lineFoldingOnly = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.foldingRangeProvider) {
            return;
        }
        const implCapabilities = capabilities.foldingRangeProvider;
        const id = Is.string(implCapabilities.id) && implCapabilities.id.length > 0 ? implCapabilities.id : UUID.generateUuid();
        const selector = implCapabilities.documentSelector || documentSelector;
        if (selector) {
            this.register(this.messages, {
                id,
                registerOptions: Object.assign({}, { documentSelector: selector })
            });
        }
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideFoldingRanges = (document, _, token) => {
            const requestParams = {
                textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document)
            };
            return client.sendRequest(vscode_languageserver_protocol_1.FoldingRangeRequest.type, requestParams, token).then(this.asFoldingRanges.bind(this), (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.FoldingRangeRequest.type, error);
                return Promise.resolve(null);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerFoldingRangeProvider(options.documentSelector, {
            provideFoldingRanges(document, context, token) {
                return middleware.provideFoldingRanges
                    ? middleware.provideFoldingRanges(document, context, token, provideFoldingRanges)
                    : provideFoldingRanges(document, context, token);
            }
        });
    }
    asFoldingRangeKind(kind) {
        if (kind) {
            switch (kind) {
                case vscode_languageserver_protocol_1.FoldingRangeKind.Comment:
                    return vscode_1.FoldingRangeKind.Comment;
                case vscode_languageserver_protocol_1.FoldingRangeKind.Imports:
                    return vscode_1.FoldingRangeKind.Imports;
                case vscode_languageserver_protocol_1.FoldingRangeKind.Region:
                    return vscode_1.FoldingRangeKind.Region;
            }
        }
        return void 0;
    }
    asFoldingRanges(foldingRanges) {
        if (Array.isArray(foldingRanges)) {
            return foldingRanges.map(r => {
                return new vscode_1.FoldingRange(r.startLine, r.endLine, this.asFoldingRangeKind(r.kind));
            });
        }
        return [];
    }
}
exports.FoldingRangeFeature = FoldingRangeFeature;


/***/ }),

/***/ 391:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const UUID = __webpack_require__(884);
const Is = __webpack_require__(97);
const vscode_1 = __webpack_require__(496);
const vscode_languageserver_protocol_1 = __webpack_require__(192);
const client_1 = __webpack_require__(620);
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
class ImplementationFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.ImplementationRequest.type);
    }
    fillClientCapabilities(capabilites) {
        let implementationSupport = ensure(ensure(capabilites, 'textDocument'), 'implementation');
        implementationSupport.dynamicRegistration = true;
        implementationSupport.linkSupport = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.implementationProvider) {
            return;
        }
        if (capabilities.implementationProvider === true) {
            if (!documentSelector) {
                return;
            }
            this.register(this.messages, {
                id: UUID.generateUuid(),
                registerOptions: Object.assign({}, { documentSelector: documentSelector })
            });
        }
        else {
            const implCapabilities = capabilities.implementationProvider;
            const id = Is.string(implCapabilities.id) && implCapabilities.id.length > 0 ? implCapabilities.id : UUID.generateUuid();
            const selector = implCapabilities.documentSelector || documentSelector;
            if (selector) {
                this.register(this.messages, {
                    id,
                    registerOptions: Object.assign({}, { documentSelector: selector })
                });
            }
        }
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideImplementation = (document, position, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.ImplementationRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asDefinitionResult, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.ImplementationRequest.type, error);
                return Promise.resolve(null);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerImplementationProvider(options.documentSelector, {
            provideImplementation: (document, position, token) => {
                return middleware.provideImplementation
                    ? middleware.provideImplementation(document, position, token, provideImplementation)
                    : provideImplementation(document, position, token);
            }
        });
    }
}
exports.ImplementationFeature = ImplementationFeature;


/***/ }),

/***/ 56:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

function __export(m) {
    for (var p in m) if (!exports.hasOwnProperty(p)) exports[p] = m[p];
}
Object.defineProperty(exports, "__esModule", ({ value: true }));
const cp = __webpack_require__(81);
const fs = __webpack_require__(147);
const SemVer = __webpack_require__(625);
const client_1 = __webpack_require__(620);
const vscode_1 = __webpack_require__(496);
const vscode_languageserver_protocol_1 = __webpack_require__(192);
const colorProvider_1 = __webpack_require__(194);
const configuration_1 = __webpack_require__(888);
const implementation_1 = __webpack_require__(391);
const typeDefinition_1 = __webpack_require__(409);
const workspaceFolders_1 = __webpack_require__(670);
const foldingRange_1 = __webpack_require__(573);
const declaration_1 = __webpack_require__(812);
const Is = __webpack_require__(97);
const processes_1 = __webpack_require__(567);
__export(__webpack_require__(620));
const REQUIRED_VSCODE_VERSION = '^1.30'; // do not change format, updated by `updateVSCode` script
var Executable;
(function (Executable) {
    function is(value) {
        return Is.string(value.command);
    }
    Executable.is = is;
})(Executable || (Executable = {}));
var TransportKind;
(function (TransportKind) {
    TransportKind[TransportKind["stdio"] = 0] = "stdio";
    TransportKind[TransportKind["ipc"] = 1] = "ipc";
    TransportKind[TransportKind["pipe"] = 2] = "pipe";
    TransportKind[TransportKind["socket"] = 3] = "socket";
})(TransportKind = exports.TransportKind || (exports.TransportKind = {}));
var Transport;
(function (Transport) {
    function isSocket(value) {
        let candidate = value;
        return candidate && candidate.kind === TransportKind.socket && Is.number(candidate.port);
    }
    Transport.isSocket = isSocket;
})(Transport || (Transport = {}));
var NodeModule;
(function (NodeModule) {
    function is(value) {
        return Is.string(value.module);
    }
    NodeModule.is = is;
})(NodeModule || (NodeModule = {}));
var StreamInfo;
(function (StreamInfo) {
    function is(value) {
        let candidate = value;
        return candidate && candidate.writer !== void 0 && candidate.reader !== void 0;
    }
    StreamInfo.is = is;
})(StreamInfo || (StreamInfo = {}));
var ChildProcessInfo;
(function (ChildProcessInfo) {
    function is(value) {
        let candidate = value;
        return candidate && candidate.process !== void 0 && typeof candidate.detached === 'boolean';
    }
    ChildProcessInfo.is = is;
})(ChildProcessInfo || (ChildProcessInfo = {}));
class LanguageClient extends client_1.BaseLanguageClient {
    constructor(arg1, arg2, arg3, arg4, arg5) {
        let id;
        let name;
        let serverOptions;
        let clientOptions;
        let forceDebug;
        if (Is.string(arg2)) {
            id = arg1;
            name = arg2;
            serverOptions = arg3;
            clientOptions = arg4;
            forceDebug = !!arg5;
        }
        else {
            id = arg1.toLowerCase();
            name = arg1;
            serverOptions = arg2;
            clientOptions = arg3;
            forceDebug = arg4;
        }
        if (forceDebug === void 0) {
            forceDebug = false;
        }
        super(id, name, clientOptions);
        this._serverOptions = serverOptions;
        this._forceDebug = forceDebug;
        try {
            this.checkVersion();
        }
        catch (error) {
            if (Is.string(error.message)) {
                this.outputChannel.appendLine(error.message);
            }
            throw error;
        }
    }
    checkVersion() {
        let codeVersion = SemVer.parse(vscode_1.version);
        if (!codeVersion) {
            throw new Error(`No valid VS Code version detected. Version string is: ${vscode_1.version}`);
        }
        // Remove the insider pre-release since we stay API compatible.
        if (codeVersion.prerelease && codeVersion.prerelease.length > 0) {
            codeVersion.prerelease = [];
        }
        if (!SemVer.satisfies(codeVersion, REQUIRED_VSCODE_VERSION)) {
            throw new Error(`The language client requires VS Code version ${REQUIRED_VSCODE_VERSION} but received version ${vscode_1.version}`);
        }
    }
    stop() {
        return super.stop().then(() => {
            if (this._serverProcess) {
                let toCheck = this._serverProcess;
                this._serverProcess = undefined;
                if (this._isDetached === void 0 || !this._isDetached) {
                    this.checkProcessDied(toCheck);
                }
                this._isDetached = undefined;
            }
        });
    }
    checkProcessDied(childProcess) {
        if (!childProcess) {
            return;
        }
        setTimeout(() => {
            // Test if the process is still alive. Throws an exception if not
            try {
                process.kill(childProcess.pid, 0);
                processes_1.terminate(childProcess);
            }
            catch (error) {
                // All is fine.
            }
        }, 2000);
    }
    handleConnectionClosed() {
        this._serverProcess = undefined;
        super.handleConnectionClosed();
    }
    createMessageTransports(encoding) {
        function getEnvironment(env) {
            if (!env) {
                return process.env;
            }
            let result = Object.create(null);
            Object.keys(process.env).forEach(key => result[key] = process.env[key]);
            Object.keys(env).forEach(key => result[key] = env[key]);
            return result;
        }
        function startedInDebugMode() {
            let args = process.execArgv;
            if (args) {
                return args.some((arg) => /^--debug=?/.test(arg) || /^--debug-brk=?/.test(arg) || /^--inspect=?/.test(arg) || /^--inspect-brk=?/.test(arg));
            }
            ;
            return false;
        }
        let server = this._serverOptions;
        // We got a function.
        if (Is.func(server)) {
            return server().then((result) => {
                if (client_1.MessageTransports.is(result)) {
                    this._isDetached = !!result.detached;
                    return result;
                }
                else if (StreamInfo.is(result)) {
                    this._isDetached = !!result.detached;
                    return { reader: new vscode_languageserver_protocol_1.StreamMessageReader(result.reader), writer: new vscode_languageserver_protocol_1.StreamMessageWriter(result.writer) };
                }
                else {
                    let cp;
                    if (ChildProcessInfo.is(result)) {
                        cp = result.process;
                        this._isDetached = result.detached;
                    }
                    else {
                        cp = result;
                        this._isDetached = false;
                    }
                    cp.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                    return { reader: new vscode_languageserver_protocol_1.StreamMessageReader(cp.stdout), writer: new vscode_languageserver_protocol_1.StreamMessageWriter(cp.stdin) };
                }
            });
        }
        let json;
        let runDebug = server;
        if (runDebug.run || runDebug.debug) {
            // We are under debugging. So use debug as well.
            if (typeof v8debug === 'object' || this._forceDebug || startedInDebugMode()) {
                json = runDebug.debug;
            }
            else {
                json = runDebug.run;
            }
        }
        else {
            json = server;
        }
        return this._getServerWorkingDir(json.options).then(serverWorkingDir => {
            if (NodeModule.is(json) && json.module) {
                let node = json;
                let transport = node.transport || TransportKind.stdio;
                if (node.runtime) {
                    let args = [];
                    let options = node.options || Object.create(null);
                    if (options.execArgv) {
                        options.execArgv.forEach(element => args.push(element));
                    }
                    args.push(node.module);
                    if (node.args) {
                        node.args.forEach(element => args.push(element));
                    }
                    let execOptions = Object.create(null);
                    execOptions.cwd = serverWorkingDir;
                    execOptions.env = getEnvironment(options.env);
                    let pipeName = undefined;
                    if (transport === TransportKind.ipc) {
                        // exec options not correctly typed in lib
                        execOptions.stdio = [null, null, null, 'ipc'];
                        args.push('--node-ipc');
                    }
                    else if (transport === TransportKind.stdio) {
                        args.push('--stdio');
                    }
                    else if (transport === TransportKind.pipe) {
                        pipeName = vscode_languageserver_protocol_1.generateRandomPipeName();
                        args.push(`--pipe=${pipeName}`);
                    }
                    else if (Transport.isSocket(transport)) {
                        args.push(`--socket=${transport.port}`);
                    }
                    args.push(`--clientProcessId=${process.pid.toString()}`);
                    if (transport === TransportKind.ipc || transport === TransportKind.stdio) {
                        let serverProcess = cp.spawn(node.runtime, args, execOptions);
                        if (!serverProcess || !serverProcess.pid) {
                            return Promise.reject(`Launching server using runtime ${node.runtime} failed.`);
                        }
                        this._serverProcess = serverProcess;
                        serverProcess.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                        if (transport === TransportKind.ipc) {
                            serverProcess.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                            return Promise.resolve({ reader: new vscode_languageserver_protocol_1.IPCMessageReader(serverProcess), writer: new vscode_languageserver_protocol_1.IPCMessageWriter(serverProcess) });
                        }
                        else {
                            return Promise.resolve({ reader: new vscode_languageserver_protocol_1.StreamMessageReader(serverProcess.stdout), writer: new vscode_languageserver_protocol_1.StreamMessageWriter(serverProcess.stdin) });
                        }
                    }
                    else if (transport == TransportKind.pipe) {
                        return vscode_languageserver_protocol_1.createClientPipeTransport(pipeName).then((transport) => {
                            let process = cp.spawn(node.runtime, args, execOptions);
                            if (!process || !process.pid) {
                                return Promise.reject(`Launching server using runtime ${node.runtime} failed.`);
                            }
                            this._serverProcess = process;
                            process.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                            process.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                            return transport.onConnected().then((protocol) => {
                                return { reader: protocol[0], writer: protocol[1] };
                            });
                        });
                    }
                    else if (Transport.isSocket(transport)) {
                        return vscode_languageserver_protocol_1.createClientSocketTransport(transport.port).then((transport) => {
                            let process = cp.spawn(node.runtime, args, execOptions);
                            if (!process || !process.pid) {
                                return Promise.reject(`Launching server using runtime ${node.runtime} failed.`);
                            }
                            this._serverProcess = process;
                            process.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                            process.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                            return transport.onConnected().then((protocol) => {
                                return { reader: protocol[0], writer: protocol[1] };
                            });
                        });
                    }
                }
                else {
                    let pipeName = undefined;
                    return new Promise((resolve, _reject) => {
                        let args = node.args && node.args.slice() || [];
                        if (transport === TransportKind.ipc) {
                            args.push('--node-ipc');
                        }
                        else if (transport === TransportKind.stdio) {
                            args.push('--stdio');
                        }
                        else if (transport === TransportKind.pipe) {
                            pipeName = vscode_languageserver_protocol_1.generateRandomPipeName();
                            args.push(`--pipe=${pipeName}`);
                        }
                        else if (Transport.isSocket(transport)) {
                            args.push(`--socket=${transport.port}`);
                        }
                        args.push(`--clientProcessId=${process.pid.toString()}`);
                        let options = node.options || Object.create(null);
                        options.execArgv = options.execArgv || [];
                        options.cwd = serverWorkingDir;
                        options.silent = true;
                        if (transport === TransportKind.ipc || transport === TransportKind.stdio) {
                            let sp = cp.fork(node.module, args || [], options);
                            this._serverProcess = sp;
                            sp.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                            if (transport === TransportKind.ipc) {
                                sp.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                                resolve({ reader: new vscode_languageserver_protocol_1.IPCMessageReader(this._serverProcess), writer: new vscode_languageserver_protocol_1.IPCMessageWriter(this._serverProcess) });
                            }
                            else {
                                resolve({ reader: new vscode_languageserver_protocol_1.StreamMessageReader(sp.stdout), writer: new vscode_languageserver_protocol_1.StreamMessageWriter(sp.stdin) });
                            }
                        }
                        else if (transport === TransportKind.pipe) {
                            vscode_languageserver_protocol_1.createClientPipeTransport(pipeName).then((transport) => {
                                let sp = cp.fork(node.module, args || [], options);
                                this._serverProcess = sp;
                                sp.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                                sp.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                                transport.onConnected().then((protocol) => {
                                    resolve({ reader: protocol[0], writer: protocol[1] });
                                });
                            });
                        }
                        else if (Transport.isSocket(transport)) {
                            vscode_languageserver_protocol_1.createClientSocketTransport(transport.port).then((transport) => {
                                let sp = cp.fork(node.module, args || [], options);
                                this._serverProcess = sp;
                                sp.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                                sp.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                                transport.onConnected().then((protocol) => {
                                    resolve({ reader: protocol[0], writer: protocol[1] });
                                });
                            });
                        }
                    });
                }
            }
            else if (Executable.is(json) && json.command) {
                let command = json;
                let args = command.args || [];
                let options = Object.assign({}, command.options);
                options.cwd = options.cwd || serverWorkingDir;
                let serverProcess = cp.spawn(command.command, args, options);
                if (!serverProcess || !serverProcess.pid) {
                    return Promise.reject(`Launching server using command ${command.command} failed.`);
                }
                serverProcess.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                this._serverProcess = serverProcess;
                this._isDetached = !!options.detached;
                return Promise.resolve({ reader: new vscode_languageserver_protocol_1.StreamMessageReader(serverProcess.stdout), writer: new vscode_languageserver_protocol_1.StreamMessageWriter(serverProcess.stdin) });
            }
            return Promise.reject(new Error(`Unsupported server configuration ` + JSON.stringify(server, null, 4)));
        });
    }
    registerProposedFeatures() {
        this.registerFeatures(ProposedFeatures.createAll(this));
    }
    registerBuiltinFeatures() {
        super.registerBuiltinFeatures();
        this.registerFeature(new configuration_1.ConfigurationFeature(this));
        this.registerFeature(new typeDefinition_1.TypeDefinitionFeature(this));
        this.registerFeature(new implementation_1.ImplementationFeature(this));
        this.registerFeature(new colorProvider_1.ColorProviderFeature(this));
        this.registerFeature(new workspaceFolders_1.WorkspaceFoldersFeature(this));
        this.registerFeature(new foldingRange_1.FoldingRangeFeature(this));
        this.registerFeature(new declaration_1.DeclarationFeature(this));
    }
    _mainGetRootPath() {
        let folders = vscode_1.workspace.workspaceFolders;
        if (!folders || folders.length === 0) {
            return undefined;
        }
        let folder = folders[0];
        if (folder.uri.scheme === 'file') {
            return folder.uri.fsPath;
        }
        return undefined;
    }
    _getServerWorkingDir(options) {
        let cwd = options && options.cwd;
        if (!cwd) {
            cwd = this.clientOptions.workspaceFolder
                ? this.clientOptions.workspaceFolder.uri.fsPath
                : this._mainGetRootPath();
        }
        if (cwd) {
            // make sure the folder exists otherwise creating the process will fail
            return new Promise(s => {
                fs.lstat(cwd, (err, stats) => {
                    s(!err && stats.isDirectory() ? cwd : undefined);
                });
            });
        }
        return Promise.resolve(undefined);
    }
}
exports.LanguageClient = LanguageClient;
class SettingMonitor {
    constructor(_client, _setting) {
        this._client = _client;
        this._setting = _setting;
        this._listeners = [];
    }
    start() {
        vscode_1.workspace.onDidChangeConfiguration(this.onDidChangeConfiguration, this, this._listeners);
        this.onDidChangeConfiguration();
        return new vscode_1.Disposable(() => {
            if (this._client.needsStop()) {
                this._client.stop();
            }
        });
    }
    onDidChangeConfiguration() {
        let index = this._setting.indexOf('.');
        let primary = index >= 0 ? this._setting.substr(0, index) : this._setting;
        let rest = index >= 0 ? this._setting.substr(index + 1) : undefined;
        let enabled = rest ? vscode_1.workspace.getConfiguration(primary).get(rest, false) : vscode_1.workspace.getConfiguration(primary);
        if (enabled && this._client.needsStart()) {
            this._client.start();
        }
        else if (!enabled && this._client.needsStop()) {
            this._client.stop();
        }
    }
}
exports.SettingMonitor = SettingMonitor;
// Exporting proposed protocol.
var ProposedFeatures;
(function (ProposedFeatures) {
    function createAll(_client) {
        let result = [];
        return result;
    }
    ProposedFeatures.createAll = createAll;
})(ProposedFeatures = exports.ProposedFeatures || (exports.ProposedFeatures = {}));


/***/ }),

/***/ 628:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(496);
class ProtocolCodeLens extends code.CodeLens {
    constructor(range) {
        super(range);
    }
}
exports["default"] = ProtocolCodeLens;


/***/ }),

/***/ 213:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(496);
class ProtocolCompletionItem extends code.CompletionItem {
    constructor(label) {
        super(label);
    }
}
exports["default"] = ProtocolCompletionItem;


/***/ }),

/***/ 101:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(496);
const ls = __webpack_require__(192);
const Is = __webpack_require__(97);
const protocolCompletionItem_1 = __webpack_require__(213);
const protocolCodeLens_1 = __webpack_require__(628);
const protocolDocumentLink_1 = __webpack_require__(653);
var CodeBlock;
(function (CodeBlock) {
    function is(value) {
        let candidate = value;
        return candidate && Is.string(candidate.language) && Is.string(candidate.value);
    }
    CodeBlock.is = is;
})(CodeBlock || (CodeBlock = {}));
function createConverter(uriConverter) {
    const nullConverter = (value) => code.Uri.parse(value);
    const _uriConverter = uriConverter || nullConverter;
    function asUri(value) {
        return _uriConverter(value);
    }
    function asDiagnostics(diagnostics) {
        return diagnostics.map(asDiagnostic);
    }
    function asDiagnostic(diagnostic) {
        let result = new code.Diagnostic(asRange(diagnostic.range), diagnostic.message, asDiagnosticSeverity(diagnostic.severity));
        if (Is.number(diagnostic.code) || Is.string(diagnostic.code)) {
            result.code = diagnostic.code;
        }
        if (diagnostic.source) {
            result.source = diagnostic.source;
        }
        if (diagnostic.relatedInformation) {
            result.relatedInformation = asRelatedInformation(diagnostic.relatedInformation);
        }
        return result;
    }
    function asRelatedInformation(relatedInformation) {
        return relatedInformation.map(asDiagnosticRelatedInformation);
    }
    function asDiagnosticRelatedInformation(information) {
        return new code.DiagnosticRelatedInformation(asLocation(information.location), information.message);
    }
    function asPosition(value) {
        if (!value) {
            return undefined;
        }
        return new code.Position(value.line, value.character);
    }
    function asRange(value) {
        if (!value) {
            return undefined;
        }
        return new code.Range(asPosition(value.start), asPosition(value.end));
    }
    function asDiagnosticSeverity(value) {
        if (value === void 0 || value === null) {
            return code.DiagnosticSeverity.Error;
        }
        switch (value) {
            case ls.DiagnosticSeverity.Error:
                return code.DiagnosticSeverity.Error;
            case ls.DiagnosticSeverity.Warning:
                return code.DiagnosticSeverity.Warning;
            case ls.DiagnosticSeverity.Information:
                return code.DiagnosticSeverity.Information;
            case ls.DiagnosticSeverity.Hint:
                return code.DiagnosticSeverity.Hint;
        }
        return code.DiagnosticSeverity.Error;
    }
    function asHoverContent(value) {
        if (Is.string(value)) {
            return new code.MarkdownString(value);
        }
        else if (CodeBlock.is(value)) {
            let result = new code.MarkdownString();
            return result.appendCodeblock(value.value, value.language);
        }
        else if (Array.isArray(value)) {
            let result = [];
            for (let element of value) {
                let item = new code.MarkdownString();
                if (CodeBlock.is(element)) {
                    item.appendCodeblock(element.value, element.language);
                }
                else {
                    item.appendMarkdown(element);
                }
                result.push(item);
            }
            return result;
        }
        else {
            let result;
            switch (value.kind) {
                case ls.MarkupKind.Markdown:
                    return new code.MarkdownString(value.value);
                case ls.MarkupKind.PlainText:
                    result = new code.MarkdownString();
                    result.appendText(value.value);
                    return result;
                default:
                    result = new code.MarkdownString();
                    result.appendText(`Unsupported Markup content received. Kind is: ${value.kind}`);
                    return result;
            }
        }
    }
    function asDocumentation(value) {
        if (Is.string(value)) {
            return value;
        }
        else {
            switch (value.kind) {
                case ls.MarkupKind.Markdown:
                    return new code.MarkdownString(value.value);
                case ls.MarkupKind.PlainText:
                    return value.value;
                default:
                    return `Unsupported Markup content received. Kind is: ${value.kind}`;
            }
        }
    }
    function asHover(hover) {
        if (!hover) {
            return undefined;
        }
        return new code.Hover(asHoverContent(hover.contents), asRange(hover.range));
    }
    function asCompletionResult(result) {
        if (!result) {
            return undefined;
        }
        if (Array.isArray(result)) {
            let items = result;
            return items.map(asCompletionItem);
        }
        let list = result;
        return new code.CompletionList(list.items.map(asCompletionItem), list.isIncomplete);
    }
    function asCompletionItemKind(value) {
        // Protocol item kind is 1 based, codes item kind is zero based.
        if (ls.CompletionItemKind.Text <= value && value <= ls.CompletionItemKind.TypeParameter) {
            return [value - 1, undefined];
        }
        ;
        return [code.CompletionItemKind.Text, value];
    }
    function asCompletionItem(item) {
        let result = new protocolCompletionItem_1.default(item.label);
        if (item.detail) {
            result.detail = item.detail;
        }
        if (item.documentation) {
            result.documentation = asDocumentation(item.documentation);
            result.documentationFormat = Is.string(item.documentation) ? '$string' : item.documentation.kind;
        }
        ;
        if (item.filterText) {
            result.filterText = item.filterText;
        }
        let insertText = asCompletionInsertText(item);
        if (insertText) {
            result.insertText = insertText.text;
            result.range = insertText.range;
            result.fromEdit = insertText.fromEdit;
        }
        if (Is.number(item.kind)) {
            let [itemKind, original] = asCompletionItemKind(item.kind);
            result.kind = itemKind;
            if (original) {
                result.originalItemKind = original;
            }
        }
        if (item.sortText) {
            result.sortText = item.sortText;
        }
        if (item.additionalTextEdits) {
            result.additionalTextEdits = asTextEdits(item.additionalTextEdits);
        }
        if (Is.stringArray(item.commitCharacters)) {
            result.commitCharacters = item.commitCharacters.slice();
        }
        if (item.command) {
            result.command = asCommand(item.command);
        }
        if (item.deprecated === true || item.deprecated === false) {
            result.deprecated = item.deprecated;
        }
        if (item.preselect === true || item.preselect === false) {
            result.preselect = item.preselect;
        }
        if (item.data !== void 0) {
            result.data = item.data;
        }
        return result;
    }
    function asCompletionInsertText(item) {
        if (item.textEdit) {
            if (item.insertTextFormat === ls.InsertTextFormat.Snippet) {
                return { text: new code.SnippetString(item.textEdit.newText), range: asRange(item.textEdit.range), fromEdit: true };
            }
            else {
                return { text: item.textEdit.newText, range: asRange(item.textEdit.range), fromEdit: true };
            }
        }
        else if (item.insertText) {
            if (item.insertTextFormat === ls.InsertTextFormat.Snippet) {
                return { text: new code.SnippetString(item.insertText), fromEdit: false };
            }
            else {
                return { text: item.insertText, fromEdit: false };
            }
        }
        else {
            return undefined;
        }
    }
    function asTextEdit(edit) {
        if (!edit) {
            return undefined;
        }
        return new code.TextEdit(asRange(edit.range), edit.newText);
    }
    function asTextEdits(items) {
        if (!items) {
            return undefined;
        }
        return items.map(asTextEdit);
    }
    function asSignatureHelp(item) {
        if (!item) {
            return undefined;
        }
        let result = new code.SignatureHelp();
        if (Is.number(item.activeSignature)) {
            result.activeSignature = item.activeSignature;
        }
        else {
            // activeSignature was optional in the past
            result.activeSignature = 0;
        }
        if (Is.number(item.activeParameter)) {
            result.activeParameter = item.activeParameter;
        }
        else {
            // activeParameter was optional in the past
            result.activeParameter = 0;
        }
        if (item.signatures) {
            result.signatures = asSignatureInformations(item.signatures);
        }
        return result;
    }
    function asSignatureInformations(items) {
        return items.map(asSignatureInformation);
    }
    function asSignatureInformation(item) {
        let result = new code.SignatureInformation(item.label);
        if (item.documentation) {
            result.documentation = asDocumentation(item.documentation);
        }
        if (item.parameters) {
            result.parameters = asParameterInformations(item.parameters);
        }
        return result;
    }
    function asParameterInformations(item) {
        return item.map(asParameterInformation);
    }
    function asParameterInformation(item) {
        let result = new code.ParameterInformation(item.label);
        if (item.documentation) {
            result.documentation = asDocumentation(item.documentation);
        }
        ;
        return result;
    }
    function asLocation(item) {
        if (!item) {
            return undefined;
        }
        return new code.Location(_uriConverter(item.uri), asRange(item.range));
    }
    function asDeclarationResult(item) {
        if (!item) {
            return undefined;
        }
        return asLocationResult(item);
    }
    function asDefinitionResult(item) {
        if (!item) {
            return undefined;
        }
        return asLocationResult(item);
    }
    function asLocationLink(item) {
        if (!item) {
            return undefined;
        }
        return {
            targetUri: _uriConverter(item.targetUri),
            targetRange: asRange(item.targetSelectionRange),
            originSelectionRange: asRange(item.originSelectionRange),
            targetSelectionRange: asRange(item.targetSelectionRange)
        };
    }
    function asLocationResult(item) {
        if (!item) {
            return undefined;
        }
        if (Is.array(item)) {
            if (item.length === 0) {
                return [];
            }
            else if (ls.LocationLink.is(item[0])) {
                let links = item;
                return links.map((link) => asLocationLink(link));
            }
            else {
                let locations = item;
                return locations.map((location) => asLocation(location));
            }
        }
        else if (ls.LocationLink.is(item)) {
            return [asLocationLink(item)];
        }
        else {
            return asLocation(item);
        }
    }
    function asReferences(values) {
        if (!values) {
            return undefined;
        }
        return values.map(location => asLocation(location));
    }
    function asDocumentHighlights(values) {
        if (!values) {
            return undefined;
        }
        return values.map(asDocumentHighlight);
    }
    function asDocumentHighlight(item) {
        let result = new code.DocumentHighlight(asRange(item.range));
        if (Is.number(item.kind)) {
            result.kind = asDocumentHighlightKind(item.kind);
        }
        return result;
    }
    function asDocumentHighlightKind(item) {
        switch (item) {
            case ls.DocumentHighlightKind.Text:
                return code.DocumentHighlightKind.Text;
            case ls.DocumentHighlightKind.Read:
                return code.DocumentHighlightKind.Read;
            case ls.DocumentHighlightKind.Write:
                return code.DocumentHighlightKind.Write;
        }
        return code.DocumentHighlightKind.Text;
    }
    function asSymbolInformations(values, uri) {
        if (!values) {
            return undefined;
        }
        return values.map(information => asSymbolInformation(information, uri));
    }
    function asSymbolKind(item) {
        if (item <= ls.SymbolKind.TypeParameter) {
            // Symbol kind is one based in the protocol and zero based in code.
            return item - 1;
        }
        return code.SymbolKind.Property;
    }
    function asSymbolInformation(item, uri) {
        // Symbol kind is one based in the protocol and zero based in code.
        let result = new code.SymbolInformation(item.name, asSymbolKind(item.kind), asRange(item.location.range), item.location.uri ? _uriConverter(item.location.uri) : uri);
        if (item.containerName) {
            result.containerName = item.containerName;
        }
        return result;
    }
    function asDocumentSymbols(values) {
        if (values === void 0 || values === null) {
            return undefined;
        }
        return values.map(asDocumentSymbol);
    }
    function asDocumentSymbol(value) {
        let result = new code.DocumentSymbol(value.name, value.detail || '', asSymbolKind(value.kind), asRange(value.range), asRange(value.selectionRange));
        if (value.children !== void 0 && value.children.length > 0) {
            let children = [];
            for (let child of value.children) {
                children.push(asDocumentSymbol(child));
            }
            result.children = children;
        }
        return result;
    }
    function asCommand(item) {
        let result = { title: item.title, command: item.command };
        if (item.arguments) {
            result.arguments = item.arguments;
        }
        return result;
    }
    function asCommands(items) {
        if (!items) {
            return undefined;
        }
        return items.map(asCommand);
    }
    const kindMapping = new Map();
    kindMapping.set('', code.CodeActionKind.Empty);
    kindMapping.set(ls.CodeActionKind.QuickFix, code.CodeActionKind.QuickFix);
    kindMapping.set(ls.CodeActionKind.Refactor, code.CodeActionKind.Refactor);
    kindMapping.set(ls.CodeActionKind.RefactorExtract, code.CodeActionKind.RefactorExtract);
    kindMapping.set(ls.CodeActionKind.RefactorInline, code.CodeActionKind.RefactorInline);
    kindMapping.set(ls.CodeActionKind.RefactorRewrite, code.CodeActionKind.RefactorRewrite);
    kindMapping.set(ls.CodeActionKind.Source, code.CodeActionKind.Source);
    kindMapping.set(ls.CodeActionKind.SourceOrganizeImports, code.CodeActionKind.SourceOrganizeImports);
    function asCodeActionKind(item) {
        if (item === void 0 || item === null) {
            return undefined;
        }
        let result = kindMapping.get(item);
        if (result) {
            return result;
        }
        let parts = item.split('.');
        result = code.CodeActionKind.Empty;
        for (let part of parts) {
            result = result.append(part);
        }
        return result;
    }
    function asCodeActionKinds(items) {
        if (items === void 0 || items === null) {
            return undefined;
        }
        return items.map(kind => asCodeActionKind(kind));
    }
    function asCodeAction(item) {
        if (item === void 0 || item === null) {
            return undefined;
        }
        let result = new code.CodeAction(item.title);
        if (item.kind !== void 0) {
            result.kind = asCodeActionKind(item.kind);
        }
        if (item.diagnostics) {
            result.diagnostics = asDiagnostics(item.diagnostics);
        }
        if (item.edit) {
            result.edit = asWorkspaceEdit(item.edit);
        }
        if (item.command) {
            result.command = asCommand(item.command);
        }
        return result;
    }
    function asCodeLens(item) {
        if (!item) {
            return undefined;
        }
        let result = new protocolCodeLens_1.default(asRange(item.range));
        if (item.command) {
            result.command = asCommand(item.command);
        }
        if (item.data !== void 0 && item.data !== null) {
            result.data = item.data;
        }
        return result;
    }
    function asCodeLenses(items) {
        if (!items) {
            return undefined;
        }
        return items.map((codeLens) => asCodeLens(codeLens));
    }
    function asWorkspaceEdit(item) {
        if (!item) {
            return undefined;
        }
        let result = new code.WorkspaceEdit();
        if (item.documentChanges) {
            item.documentChanges.forEach(change => {
                if (ls.CreateFile.is(change)) {
                    result.createFile(_uriConverter(change.uri), change.options);
                }
                else if (ls.RenameFile.is(change)) {
                    result.renameFile(_uriConverter(change.oldUri), _uriConverter(change.newUri), change.options);
                }
                else if (ls.DeleteFile.is(change)) {
                    result.deleteFile(_uriConverter(change.uri), change.options);
                }
                else if (ls.TextDocumentEdit.is(change)) {
                    result.set(_uriConverter(change.textDocument.uri), asTextEdits(change.edits));
                }
                else {
                    console.error(`Unknown workspace edit change received:\n${JSON.stringify(change, undefined, 4)}`);
                }
            });
        }
        else if (item.changes) {
            Object.keys(item.changes).forEach(key => {
                result.set(_uriConverter(key), asTextEdits(item.changes[key]));
            });
        }
        return result;
    }
    function asDocumentLink(item) {
        let range = asRange(item.range);
        let target = item.target ? asUri(item.target) : undefined;
        // target must be optional in DocumentLink
        let link = new protocolDocumentLink_1.default(range, target);
        if (item.data !== void 0 && item.data !== null) {
            link.data = item.data;
        }
        return link;
    }
    function asDocumentLinks(items) {
        if (!items) {
            return undefined;
        }
        return items.map(asDocumentLink);
    }
    function asColor(color) {
        return new code.Color(color.red, color.green, color.blue, color.alpha);
    }
    function asColorInformation(ci) {
        return new code.ColorInformation(asRange(ci.range), asColor(ci.color));
    }
    function asColorInformations(colorInformation) {
        if (Array.isArray(colorInformation)) {
            return colorInformation.map(asColorInformation);
        }
        return undefined;
    }
    function asColorPresentation(cp) {
        let presentation = new code.ColorPresentation(cp.label);
        presentation.additionalTextEdits = asTextEdits(cp.additionalTextEdits);
        if (cp.textEdit) {
            presentation.textEdit = asTextEdit(cp.textEdit);
        }
        return presentation;
    }
    function asColorPresentations(colorPresentations) {
        if (Array.isArray(colorPresentations)) {
            return colorPresentations.map(asColorPresentation);
        }
        return undefined;
    }
    function asFoldingRangeKind(kind) {
        if (kind) {
            switch (kind) {
                case ls.FoldingRangeKind.Comment:
                    return code.FoldingRangeKind.Comment;
                case ls.FoldingRangeKind.Imports:
                    return code.FoldingRangeKind.Imports;
                case ls.FoldingRangeKind.Region:
                    return code.FoldingRangeKind.Region;
            }
        }
        return void 0;
    }
    function asFoldingRange(r) {
        return new code.FoldingRange(r.startLine, r.endLine, asFoldingRangeKind(r.kind));
    }
    function asFoldingRanges(foldingRanges) {
        if (Array.isArray(foldingRanges)) {
            return foldingRanges.map(asFoldingRange);
        }
        return void 0;
    }
    return {
        asUri,
        asDiagnostics,
        asDiagnostic,
        asRange,
        asPosition,
        asDiagnosticSeverity,
        asHover,
        asCompletionResult,
        asCompletionItem,
        asTextEdit,
        asTextEdits,
        asSignatureHelp,
        asSignatureInformations,
        asSignatureInformation,
        asParameterInformations,
        asParameterInformation,
        asDeclarationResult,
        asDefinitionResult,
        asLocation,
        asReferences,
        asDocumentHighlights,
        asDocumentHighlight,
        asDocumentHighlightKind,
        asSymbolInformations,
        asSymbolInformation,
        asDocumentSymbols,
        asDocumentSymbol,
        asCommand,
        asCommands,
        asCodeAction,
        asCodeActionKind,
        asCodeActionKinds,
        asCodeLens,
        asCodeLenses,
        asWorkspaceEdit,
        asDocumentLink,
        asDocumentLinks,
        asFoldingRangeKind,
        asFoldingRange,
        asFoldingRanges,
        asColor,
        asColorInformation,
        asColorInformations,
        asColorPresentation,
        asColorPresentations
    };
}
exports.createConverter = createConverter;


/***/ }),

/***/ 653:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(496);
class ProtocolDocumentLink extends code.DocumentLink {
    constructor(range, target) {
        super(range, target);
    }
}
exports["default"] = ProtocolDocumentLink;


/***/ }),

/***/ 409:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const UUID = __webpack_require__(884);
const Is = __webpack_require__(97);
const vscode_1 = __webpack_require__(496);
const vscode_languageserver_protocol_1 = __webpack_require__(192);
const client_1 = __webpack_require__(620);
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
class TypeDefinitionFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.TypeDefinitionRequest.type);
    }
    fillClientCapabilities(capabilites) {
        ensure(ensure(capabilites, 'textDocument'), 'typeDefinition').dynamicRegistration = true;
        let typeDefinitionSupport = ensure(ensure(capabilites, 'textDocument'), 'typeDefinition');
        typeDefinitionSupport.dynamicRegistration = true;
        typeDefinitionSupport.linkSupport = true;
    }
    initialize(capabilities, documentSelector) {
        if (!capabilities.typeDefinitionProvider) {
            return;
        }
        if (capabilities.typeDefinitionProvider === true) {
            if (!documentSelector) {
                return;
            }
            this.register(this.messages, {
                id: UUID.generateUuid(),
                registerOptions: Object.assign({}, { documentSelector: documentSelector })
            });
        }
        else {
            const implCapabilities = capabilities.typeDefinitionProvider;
            const id = Is.string(implCapabilities.id) && implCapabilities.id.length > 0 ? implCapabilities.id : UUID.generateUuid();
            const selector = implCapabilities.documentSelector || documentSelector;
            if (selector) {
                this.register(this.messages, {
                    id,
                    registerOptions: Object.assign({}, { documentSelector: selector })
                });
            }
        }
    }
    registerLanguageProvider(options) {
        let client = this._client;
        let provideTypeDefinition = (document, position, token) => {
            return client.sendRequest(vscode_languageserver_protocol_1.TypeDefinitionRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asDefinitionResult, (error) => {
                client.logFailedRequest(vscode_languageserver_protocol_1.TypeDefinitionRequest.type, error);
                return Promise.resolve(null);
            });
        };
        let middleware = client.clientOptions.middleware;
        return vscode_1.languages.registerTypeDefinitionProvider(options.documentSelector, {
            provideTypeDefinition: (document, position, token) => {
                return middleware.provideTypeDefinition
                    ? middleware.provideTypeDefinition(document, position, token, provideTypeDefinition)
                    : provideTypeDefinition(document, position, token);
            }
        });
    }
}
exports.TypeDefinitionFeature = TypeDefinitionFeature;


/***/ }),

/***/ 299:
/***/ ((__unused_webpack_module, exports) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
class Delayer {
    constructor(defaultDelay) {
        this.defaultDelay = defaultDelay;
        this.timeout = undefined;
        this.completionPromise = undefined;
        this.onSuccess = undefined;
        this.task = undefined;
    }
    trigger(task, delay = this.defaultDelay) {
        this.task = task;
        if (delay >= 0) {
            this.cancelTimeout();
        }
        if (!this.completionPromise) {
            this.completionPromise = new Promise((resolve) => {
                this.onSuccess = resolve;
            }).then(() => {
                this.completionPromise = undefined;
                this.onSuccess = undefined;
                var result = this.task();
                this.task = undefined;
                return result;
            });
        }
        if (delay >= 0 || this.timeout === void 0) {
            this.timeout = setTimeout(() => {
                this.timeout = undefined;
                this.onSuccess(undefined);
            }, delay >= 0 ? delay : this.defaultDelay);
        }
        return this.completionPromise;
    }
    forceDelivery() {
        if (!this.completionPromise) {
            return undefined;
        }
        this.cancelTimeout();
        let result = this.task();
        this.completionPromise = undefined;
        this.onSuccess = undefined;
        this.task = undefined;
        return result;
    }
    isTriggered() {
        return this.timeout !== void 0;
    }
    cancel() {
        this.cancelTimeout();
        this.completionPromise = undefined;
    }
    cancelTimeout() {
        if (this.timeout !== void 0) {
            clearTimeout(this.timeout);
            this.timeout = undefined;
        }
    }
}
exports.Delayer = Delayer;


/***/ }),

/***/ 97:
/***/ ((__unused_webpack_module, exports) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
function boolean(value) {
    return value === true || value === false;
}
exports.boolean = boolean;
function string(value) {
    return typeof value === 'string' || value instanceof String;
}
exports.string = string;
function number(value) {
    return typeof value === 'number' || value instanceof Number;
}
exports.number = number;
function error(value) {
    return value instanceof Error;
}
exports.error = error;
function func(value) {
    return typeof value === 'function';
}
exports.func = func;
function array(value) {
    return Array.isArray(value);
}
exports.array = array;
function stringArray(value) {
    return array(value) && value.every(elem => string(elem));
}
exports.stringArray = stringArray;
function typedArray(value, check) {
    return Array.isArray(value) && value.every(check);
}
exports.typedArray = typedArray;
function thenable(value) {
    return value && func(value.then);
}
exports.thenable = thenable;


/***/ }),

/***/ 567:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const cp = __webpack_require__(81);
const path_1 = __webpack_require__(17);
const isWindows = (process.platform === 'win32');
const isMacintosh = (process.platform === 'darwin');
const isLinux = (process.platform === 'linux');
function terminate(process, cwd) {
    if (isWindows) {
        try {
            // This we run in Atom execFileSync is available.
            // Ignore stderr since this is otherwise piped to parent.stderr
            // which might be already closed.
            let options = {
                stdio: ['pipe', 'pipe', 'ignore']
            };
            if (cwd) {
                options.cwd = cwd;
            }
            cp.execFileSync('taskkill', ['/T', '/F', '/PID', process.pid.toString()], options);
            return true;
        }
        catch (err) {
            return false;
        }
    }
    else if (isLinux || isMacintosh) {
        try {
            var cmd = path_1.join(__dirname, 'terminateProcess.sh');
            var result = cp.spawnSync(cmd, [process.pid.toString()]);
            return result.error ? false : true;
        }
        catch (err) {
            return false;
        }
    }
    else {
        process.kill('SIGKILL');
        return true;
    }
}
exports.terminate = terminate;


/***/ }),

/***/ 884:
/***/ ((__unused_webpack_module, exports) => {

"use strict";
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

Object.defineProperty(exports, "__esModule", ({ value: true }));
class ValueUUID {
    constructor(_value) {
        this._value = _value;
        // empty
    }
    asHex() {
        return this._value;
    }
    equals(other) {
        return this.asHex() === other.asHex();
    }
}
class V4UUID extends ValueUUID {
    constructor() {
        super([
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            '-',
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            '-',
            '4',
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            '-',
            V4UUID._oneOf(V4UUID._timeHighBits),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            '-',
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
        ].join(''));
    }
    static _oneOf(array) {
        return array[Math.floor(array.length * Math.random())];
    }
    static _randomHex() {
        return V4UUID._oneOf(V4UUID._chars);
    }
}
V4UUID._chars = ['0', '1', '2', '3', '4', '5', '6', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
V4UUID._timeHighBits = ['8', '9', 'a', 'b'];
/**
 * An empty UUID that contains only zeros.
 */
exports.empty = new ValueUUID('00000000-0000-0000-0000-000000000000');
function v4() {
    return new V4UUID();
}
exports.v4 = v4;
const _UUIDPattern = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
function isUUID(value) {
    return _UUIDPattern.test(value);
}
exports.isUUID = isUUID;
/**
 * Parses a UUID that is of the format xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx.
 * @param value A uuid string.
 */
function parse(value) {
    if (!isUUID(value)) {
        throw new Error('invalid uuid');
    }
    return new ValueUUID(value);
}
exports.parse = parse;
function generateUuid() {
    return v4().asHex();
}
exports.generateUuid = generateUuid;


/***/ }),

/***/ 670:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const UUID = __webpack_require__(884);
const vscode_1 = __webpack_require__(496);
const vscode_languageserver_protocol_1 = __webpack_require__(192);
function access(target, key) {
    if (target === void 0) {
        return undefined;
    }
    return target[key];
}
class WorkspaceFoldersFeature {
    constructor(_client) {
        this._client = _client;
        this._listeners = new Map();
    }
    get messages() {
        return vscode_languageserver_protocol_1.DidChangeWorkspaceFoldersNotification.type;
    }
    fillInitializeParams(params) {
        let folders = vscode_1.workspace.workspaceFolders;
        if (folders === void 0) {
            params.workspaceFolders = null;
        }
        else {
            params.workspaceFolders = folders.map(folder => this.asProtocol(folder));
        }
    }
    fillClientCapabilities(capabilities) {
        capabilities.workspace = capabilities.workspace || {};
        capabilities.workspace.workspaceFolders = true;
    }
    initialize(capabilities) {
        let client = this._client;
        client.onRequest(vscode_languageserver_protocol_1.WorkspaceFoldersRequest.type, (token) => {
            let workspaceFolders = () => {
                let folders = vscode_1.workspace.workspaceFolders;
                if (folders === void 0) {
                    return null;
                }
                let result = folders.map((folder) => {
                    return this.asProtocol(folder);
                });
                return result;
            };
            let middleware = client.clientOptions.middleware.workspace;
            return middleware && middleware.workspaceFolders
                ? middleware.workspaceFolders(token, workspaceFolders)
                : workspaceFolders(token);
        });
        let value = access(access(access(capabilities, 'workspace'), 'workspaceFolders'), 'changeNotifications');
        let id;
        if (typeof value === 'string') {
            id = value;
        }
        else if (value === true) {
            id = UUID.generateUuid();
        }
        if (id) {
            this.register(this.messages, {
                id: id,
                registerOptions: undefined
            });
        }
    }
    register(_message, data) {
        let id = data.id;
        let client = this._client;
        let disposable = vscode_1.workspace.onDidChangeWorkspaceFolders((event) => {
            let didChangeWorkspaceFolders = (event) => {
                let params = {
                    event: {
                        added: event.added.map(folder => this.asProtocol(folder)),
                        removed: event.removed.map(folder => this.asProtocol(folder))
                    }
                };
                this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeWorkspaceFoldersNotification.type, params);
            };
            let middleware = client.clientOptions.middleware.workspace;
            middleware && middleware.didChangeWorkspaceFolders
                ? middleware.didChangeWorkspaceFolders(event, didChangeWorkspaceFolders)
                : didChangeWorkspaceFolders(event);
        });
        this._listeners.set(id, disposable);
    }
    unregister(id) {
        let disposable = this._listeners.get(id);
        if (disposable === void 0) {
            return;
        }
        this._listeners.delete(id);
        disposable.dispose();
    }
    dispose() {
        for (let disposable of this._listeners.values()) {
            disposable.dispose();
        }
        this._listeners.clear();
    }
    asProtocol(workspaceFolder) {
        if (workspaceFolder === void 0) {
            return null;
        }
        return { uri: this._client.code2ProtocolConverter.asUri(workspaceFolder.uri), name: workspaceFolder.name };
    }
}
exports.WorkspaceFoldersFeature = WorkspaceFoldersFeature;


/***/ }),

/***/ 192:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

function __export(m) {
    for (var p in m) if (!exports.hasOwnProperty(p)) exports[p] = m[p];
}
Object.defineProperty(exports, "__esModule", ({ value: true }));
const vscode_jsonrpc_1 = __webpack_require__(231);
exports.ErrorCodes = vscode_jsonrpc_1.ErrorCodes;
exports.ResponseError = vscode_jsonrpc_1.ResponseError;
exports.CancellationToken = vscode_jsonrpc_1.CancellationToken;
exports.CancellationTokenSource = vscode_jsonrpc_1.CancellationTokenSource;
exports.Disposable = vscode_jsonrpc_1.Disposable;
exports.Event = vscode_jsonrpc_1.Event;
exports.Emitter = vscode_jsonrpc_1.Emitter;
exports.Trace = vscode_jsonrpc_1.Trace;
exports.TraceFormat = vscode_jsonrpc_1.TraceFormat;
exports.SetTraceNotification = vscode_jsonrpc_1.SetTraceNotification;
exports.LogTraceNotification = vscode_jsonrpc_1.LogTraceNotification;
exports.RequestType = vscode_jsonrpc_1.RequestType;
exports.RequestType0 = vscode_jsonrpc_1.RequestType0;
exports.NotificationType = vscode_jsonrpc_1.NotificationType;
exports.NotificationType0 = vscode_jsonrpc_1.NotificationType0;
exports.MessageReader = vscode_jsonrpc_1.MessageReader;
exports.MessageWriter = vscode_jsonrpc_1.MessageWriter;
exports.ConnectionStrategy = vscode_jsonrpc_1.ConnectionStrategy;
exports.StreamMessageReader = vscode_jsonrpc_1.StreamMessageReader;
exports.StreamMessageWriter = vscode_jsonrpc_1.StreamMessageWriter;
exports.IPCMessageReader = vscode_jsonrpc_1.IPCMessageReader;
exports.IPCMessageWriter = vscode_jsonrpc_1.IPCMessageWriter;
exports.createClientPipeTransport = vscode_jsonrpc_1.createClientPipeTransport;
exports.createServerPipeTransport = vscode_jsonrpc_1.createServerPipeTransport;
exports.generateRandomPipeName = vscode_jsonrpc_1.generateRandomPipeName;
exports.createClientSocketTransport = vscode_jsonrpc_1.createClientSocketTransport;
exports.createServerSocketTransport = vscode_jsonrpc_1.createServerSocketTransport;
__export(__webpack_require__(674));
__export(__webpack_require__(121));
function createProtocolConnection(reader, writer, logger, strategy) {
    return vscode_jsonrpc_1.createMessageConnection(reader, writer, logger, strategy);
}
exports.createProtocolConnection = createProtocolConnection;


/***/ }),

/***/ 418:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const vscode_jsonrpc_1 = __webpack_require__(231);
/**
 * A request to list all color symbols found in a given text document. The request's
 * parameter is of type [DocumentColorParams](#DocumentColorParams) the
 * response is of type [ColorInformation[]](#ColorInformation) or a Thenable
 * that resolves to such.
 */
var DocumentColorRequest;
(function (DocumentColorRequest) {
    DocumentColorRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/documentColor');
})(DocumentColorRequest = exports.DocumentColorRequest || (exports.DocumentColorRequest = {}));
/**
 * A request to list all presentation for a color. The request's
 * parameter is of type [ColorPresentationParams](#ColorPresentationParams) the
 * response is of type [ColorInformation[]](#ColorInformation) or a Thenable
 * that resolves to such.
 */
var ColorPresentationRequest;
(function (ColorPresentationRequest) {
    ColorPresentationRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/colorPresentation');
})(ColorPresentationRequest = exports.ColorPresentationRequest || (exports.ColorPresentationRequest = {}));


/***/ }),

/***/ 82:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const vscode_jsonrpc_1 = __webpack_require__(231);
/**
 * The 'workspace/configuration' request is sent from the server to the client to fetch a certain
 * configuration setting.
 *
 * This pull model replaces the old push model were the client signaled configuration change via an
 * event. If the server still needs to react to configuration changes (since the server caches the
 * result of `workspace/configuration` requests) the server should register for an empty configuration
 * change event and empty the cache if such an event is received.
 */
var ConfigurationRequest;
(function (ConfigurationRequest) {
    ConfigurationRequest.type = new vscode_jsonrpc_1.RequestType('workspace/configuration');
})(ConfigurationRequest = exports.ConfigurationRequest || (exports.ConfigurationRequest = {}));


/***/ }),

/***/ 688:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const vscode_jsonrpc_1 = __webpack_require__(231);
// @ts-ignore: to avoid inlining LocatioLink as dynamic import
let __noDynamicImport;
/**
 * A request to resolve the type definition locations of a symbol at a given text
 * document position. The request's parameter is of type [TextDocumentPositioParams]
 * (#TextDocumentPositionParams) the response is of type [Declaration](#Declaration)
 * or a typed array of [DeclarationLink](#DeclarationLink) or a Thenable that resolves
 * to such.
 */
var DeclarationRequest;
(function (DeclarationRequest) {
    DeclarationRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/declaration');
})(DeclarationRequest = exports.DeclarationRequest || (exports.DeclarationRequest = {}));


/***/ }),

/***/ 138:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
const vscode_jsonrpc_1 = __webpack_require__(231);
/**
 * Enum of known range kinds
 */
var FoldingRangeKind;
(function (FoldingRangeKind) {
    /**
     * Folding range for a comment
     */
    FoldingRangeKind["Comment"] = "comment";
    /**
     * Folding range for a imports or includes
     */
    FoldingRangeKind["Imports"] = "imports";
    /**
     * Folding range for a region (e.g. `#region`)
     */
    FoldingRangeKind["Region"] = "region";
})(FoldingRangeKind = exports.FoldingRangeKind || (exports.FoldingRangeKind = {}));
/**
 * A request to provide folding ranges in a document. The request's
 * parameter is of type [FoldingRangeParams](#FoldingRangeParams), the
 * response is of type [FoldingRangeList](#FoldingRangeList) or a Thenable
 * that resolves to such.
 */
var FoldingRangeRequest;
(function (FoldingRangeRequest) {
    FoldingRangeRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/foldingRange');
})(FoldingRangeRequest = exports.FoldingRangeRequest || (exports.FoldingRangeRequest = {}));


/***/ }),

/***/ 759:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const vscode_jsonrpc_1 = __webpack_require__(231);
// @ts-ignore: to avoid inlining LocatioLink as dynamic import
let __noDynamicImport;
/**
 * A request to resolve the implementation locations of a symbol at a given text
 * document position. The request's parameter is of type [TextDocumentPositioParams]
 * (#TextDocumentPositionParams) the response is of type [Definition](#Definition) or a
 * Thenable that resolves to such.
 */
var ImplementationRequest;
(function (ImplementationRequest) {
    ImplementationRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/implementation');
})(ImplementationRequest = exports.ImplementationRequest || (exports.ImplementationRequest = {}));


/***/ }),

/***/ 121:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const Is = __webpack_require__(381);
const vscode_jsonrpc_1 = __webpack_require__(231);
const protocol_implementation_1 = __webpack_require__(759);
exports.ImplementationRequest = protocol_implementation_1.ImplementationRequest;
const protocol_typeDefinition_1 = __webpack_require__(40);
exports.TypeDefinitionRequest = protocol_typeDefinition_1.TypeDefinitionRequest;
const protocol_workspaceFolders_1 = __webpack_require__(942);
exports.WorkspaceFoldersRequest = protocol_workspaceFolders_1.WorkspaceFoldersRequest;
exports.DidChangeWorkspaceFoldersNotification = protocol_workspaceFolders_1.DidChangeWorkspaceFoldersNotification;
const protocol_configuration_1 = __webpack_require__(82);
exports.ConfigurationRequest = protocol_configuration_1.ConfigurationRequest;
const protocol_colorProvider_1 = __webpack_require__(418);
exports.DocumentColorRequest = protocol_colorProvider_1.DocumentColorRequest;
exports.ColorPresentationRequest = protocol_colorProvider_1.ColorPresentationRequest;
const protocol_foldingRange_1 = __webpack_require__(138);
exports.FoldingRangeRequest = protocol_foldingRange_1.FoldingRangeRequest;
const protocol_declaration_1 = __webpack_require__(688);
exports.DeclarationRequest = protocol_declaration_1.DeclarationRequest;
// @ts-ignore: to avoid inlining LocatioLink as dynamic import
let __noDynamicImport;
var DocumentFilter;
(function (DocumentFilter) {
    function is(value) {
        let candidate = value;
        return Is.string(candidate.language) || Is.string(candidate.scheme) || Is.string(candidate.pattern);
    }
    DocumentFilter.is = is;
})(DocumentFilter = exports.DocumentFilter || (exports.DocumentFilter = {}));
/**
 * The `client/registerCapability` request is sent from the server to the client to register a new capability
 * handler on the client side.
 */
var RegistrationRequest;
(function (RegistrationRequest) {
    RegistrationRequest.type = new vscode_jsonrpc_1.RequestType('client/registerCapability');
})(RegistrationRequest = exports.RegistrationRequest || (exports.RegistrationRequest = {}));
/**
 * The `client/unregisterCapability` request is sent from the server to the client to unregister a previously registered capability
 * handler on the client side.
 */
var UnregistrationRequest;
(function (UnregistrationRequest) {
    UnregistrationRequest.type = new vscode_jsonrpc_1.RequestType('client/unregisterCapability');
})(UnregistrationRequest = exports.UnregistrationRequest || (exports.UnregistrationRequest = {}));
var ResourceOperationKind;
(function (ResourceOperationKind) {
    /**
     * Supports creating new files and folders.
     */
    ResourceOperationKind.Create = 'create';
    /**
     * Supports renaming existing files and folders.
     */
    ResourceOperationKind.Rename = 'rename';
    /**
     * Supports deleting existing files and folders.
     */
    ResourceOperationKind.Delete = 'delete';
})(ResourceOperationKind = exports.ResourceOperationKind || (exports.ResourceOperationKind = {}));
var FailureHandlingKind;
(function (FailureHandlingKind) {
    /**
     * Applying the workspace change is simply aborted if one of the changes provided
     * fails. All operations executed before the failing operation stay executed.
     */
    FailureHandlingKind.Abort = 'abort';
    /**
     * All operations are executed transactional. That means they either all
     * succeed or no changes at all are applied to the workspace.
     */
    FailureHandlingKind.Transactional = 'transactional';
    /**
     * If the workspace edit contains only textual file changes they are executed transactional.
     * If resource changes (create, rename or delete file) are part of the change the failure
     * handling startegy is abort.
     */
    FailureHandlingKind.TextOnlyTransactional = 'textOnlyTransactional';
    /**
     * The client tries to undo the operations already executed. But there is no
     * guaruntee that this is succeeding.
     */
    FailureHandlingKind.Undo = 'undo';
})(FailureHandlingKind = exports.FailureHandlingKind || (exports.FailureHandlingKind = {}));
/**
 * Defines how the host (editor) should sync
 * document changes to the language server.
 */
var TextDocumentSyncKind;
(function (TextDocumentSyncKind) {
    /**
     * Documents should not be synced at all.
     */
    TextDocumentSyncKind.None = 0;
    /**
     * Documents are synced by always sending the full content
     * of the document.
     */
    TextDocumentSyncKind.Full = 1;
    /**
     * Documents are synced by sending the full content on open.
     * After that only incremental updates to the document are
     * send.
     */
    TextDocumentSyncKind.Incremental = 2;
})(TextDocumentSyncKind = exports.TextDocumentSyncKind || (exports.TextDocumentSyncKind = {}));
/**
 * The initialize request is sent from the client to the server.
 * It is sent once as the request after starting up the server.
 * The requests parameter is of type [InitializeParams](#InitializeParams)
 * the response if of type [InitializeResult](#InitializeResult) of a Thenable that
 * resolves to such.
 */
var InitializeRequest;
(function (InitializeRequest) {
    InitializeRequest.type = new vscode_jsonrpc_1.RequestType('initialize');
})(InitializeRequest = exports.InitializeRequest || (exports.InitializeRequest = {}));
/**
 * Known error codes for an `InitializeError`;
 */
var InitializeError;
(function (InitializeError) {
    /**
     * If the protocol version provided by the client can't be handled by the server.
     * @deprecated This initialize error got replaced by client capabilities. There is
     * no version handshake in version 3.0x
     */
    InitializeError.unknownProtocolVersion = 1;
})(InitializeError = exports.InitializeError || (exports.InitializeError = {}));
/**
 * The intialized notification is sent from the client to the
 * server after the client is fully initialized and the server
 * is allowed to send requests from the server to the client.
 */
var InitializedNotification;
(function (InitializedNotification) {
    InitializedNotification.type = new vscode_jsonrpc_1.NotificationType('initialized');
})(InitializedNotification = exports.InitializedNotification || (exports.InitializedNotification = {}));
//---- Shutdown Method ----
/**
 * A shutdown request is sent from the client to the server.
 * It is sent once when the client decides to shutdown the
 * server. The only notification that is sent after a shutdown request
 * is the exit event.
 */
var ShutdownRequest;
(function (ShutdownRequest) {
    ShutdownRequest.type = new vscode_jsonrpc_1.RequestType0('shutdown');
})(ShutdownRequest = exports.ShutdownRequest || (exports.ShutdownRequest = {}));
//---- Exit Notification ----
/**
 * The exit event is sent from the client to the server to
 * ask the server to exit its process.
 */
var ExitNotification;
(function (ExitNotification) {
    ExitNotification.type = new vscode_jsonrpc_1.NotificationType0('exit');
})(ExitNotification = exports.ExitNotification || (exports.ExitNotification = {}));
//---- Configuration notification ----
/**
 * The configuration change notification is sent from the client to the server
 * when the client's configuration has changed. The notification contains
 * the changed configuration as defined by the language client.
 */
var DidChangeConfigurationNotification;
(function (DidChangeConfigurationNotification) {
    DidChangeConfigurationNotification.type = new vscode_jsonrpc_1.NotificationType('workspace/didChangeConfiguration');
})(DidChangeConfigurationNotification = exports.DidChangeConfigurationNotification || (exports.DidChangeConfigurationNotification = {}));
//---- Message show and log notifications ----
/**
 * The message type
 */
var MessageType;
(function (MessageType) {
    /**
     * An error message.
     */
    MessageType.Error = 1;
    /**
     * A warning message.
     */
    MessageType.Warning = 2;
    /**
     * An information message.
     */
    MessageType.Info = 3;
    /**
     * A log message.
     */
    MessageType.Log = 4;
})(MessageType = exports.MessageType || (exports.MessageType = {}));
/**
 * The show message notification is sent from a server to a client to ask
 * the client to display a particular message in the user interface.
 */
var ShowMessageNotification;
(function (ShowMessageNotification) {
    ShowMessageNotification.type = new vscode_jsonrpc_1.NotificationType('window/showMessage');
})(ShowMessageNotification = exports.ShowMessageNotification || (exports.ShowMessageNotification = {}));
/**
 * The show message request is sent from the server to the client to show a message
 * and a set of options actions to the user.
 */
var ShowMessageRequest;
(function (ShowMessageRequest) {
    ShowMessageRequest.type = new vscode_jsonrpc_1.RequestType('window/showMessageRequest');
})(ShowMessageRequest = exports.ShowMessageRequest || (exports.ShowMessageRequest = {}));
/**
 * The log message notification is sent from the server to the client to ask
 * the client to log a particular message.
 */
var LogMessageNotification;
(function (LogMessageNotification) {
    LogMessageNotification.type = new vscode_jsonrpc_1.NotificationType('window/logMessage');
})(LogMessageNotification = exports.LogMessageNotification || (exports.LogMessageNotification = {}));
//---- Telemetry notification
/**
 * The telemetry event notification is sent from the server to the client to ask
 * the client to log telemetry data.
 */
var TelemetryEventNotification;
(function (TelemetryEventNotification) {
    TelemetryEventNotification.type = new vscode_jsonrpc_1.NotificationType('telemetry/event');
})(TelemetryEventNotification = exports.TelemetryEventNotification || (exports.TelemetryEventNotification = {}));
/**
 * The document open notification is sent from the client to the server to signal
 * newly opened text documents. The document's truth is now managed by the client
 * and the server must not try to read the document's truth using the document's
 * uri. Open in this sense means it is managed by the client. It doesn't necessarily
 * mean that its content is presented in an editor. An open notification must not
 * be sent more than once without a corresponding close notification send before.
 * This means open and close notification must be balanced and the max open count
 * is one.
 */
var DidOpenTextDocumentNotification;
(function (DidOpenTextDocumentNotification) {
    DidOpenTextDocumentNotification.type = new vscode_jsonrpc_1.NotificationType('textDocument/didOpen');
})(DidOpenTextDocumentNotification = exports.DidOpenTextDocumentNotification || (exports.DidOpenTextDocumentNotification = {}));
/**
 * The document change notification is sent from the client to the server to signal
 * changes to a text document.
 */
var DidChangeTextDocumentNotification;
(function (DidChangeTextDocumentNotification) {
    DidChangeTextDocumentNotification.type = new vscode_jsonrpc_1.NotificationType('textDocument/didChange');
})(DidChangeTextDocumentNotification = exports.DidChangeTextDocumentNotification || (exports.DidChangeTextDocumentNotification = {}));
/**
 * The document close notification is sent from the client to the server when
 * the document got closed in the client. The document's truth now exists where
 * the document's uri points to (e.g. if the document's uri is a file uri the
 * truth now exists on disk). As with the open notification the close notification
 * is about managing the document's content. Receiving a close notification
 * doesn't mean that the document was open in an editor before. A close
 * notification requires a previous open notification to be sent.
 */
var DidCloseTextDocumentNotification;
(function (DidCloseTextDocumentNotification) {
    DidCloseTextDocumentNotification.type = new vscode_jsonrpc_1.NotificationType('textDocument/didClose');
})(DidCloseTextDocumentNotification = exports.DidCloseTextDocumentNotification || (exports.DidCloseTextDocumentNotification = {}));
/**
 * The document save notification is sent from the client to the server when
 * the document got saved in the client.
 */
var DidSaveTextDocumentNotification;
(function (DidSaveTextDocumentNotification) {
    DidSaveTextDocumentNotification.type = new vscode_jsonrpc_1.NotificationType('textDocument/didSave');
})(DidSaveTextDocumentNotification = exports.DidSaveTextDocumentNotification || (exports.DidSaveTextDocumentNotification = {}));
/**
 * A document will save notification is sent from the client to the server before
 * the document is actually saved.
 */
var WillSaveTextDocumentNotification;
(function (WillSaveTextDocumentNotification) {
    WillSaveTextDocumentNotification.type = new vscode_jsonrpc_1.NotificationType('textDocument/willSave');
})(WillSaveTextDocumentNotification = exports.WillSaveTextDocumentNotification || (exports.WillSaveTextDocumentNotification = {}));
/**
 * A document will save request is sent from the client to the server before
 * the document is actually saved. The request can return an array of TextEdits
 * which will be applied to the text document before it is saved. Please note that
 * clients might drop results if computing the text edits took too long or if a
 * server constantly fails on this request. This is done to keep the save fast and
 * reliable.
 */
var WillSaveTextDocumentWaitUntilRequest;
(function (WillSaveTextDocumentWaitUntilRequest) {
    WillSaveTextDocumentWaitUntilRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/willSaveWaitUntil');
})(WillSaveTextDocumentWaitUntilRequest = exports.WillSaveTextDocumentWaitUntilRequest || (exports.WillSaveTextDocumentWaitUntilRequest = {}));
//---- File eventing ----
/**
 * The watched files notification is sent from the client to the server when
 * the client detects changes to file watched by the language client.
 */
var DidChangeWatchedFilesNotification;
(function (DidChangeWatchedFilesNotification) {
    DidChangeWatchedFilesNotification.type = new vscode_jsonrpc_1.NotificationType('workspace/didChangeWatchedFiles');
})(DidChangeWatchedFilesNotification = exports.DidChangeWatchedFilesNotification || (exports.DidChangeWatchedFilesNotification = {}));
/**
 * The file event type
 */
var FileChangeType;
(function (FileChangeType) {
    /**
     * The file got created.
     */
    FileChangeType.Created = 1;
    /**
     * The file got changed.
     */
    FileChangeType.Changed = 2;
    /**
     * The file got deleted.
     */
    FileChangeType.Deleted = 3;
})(FileChangeType = exports.FileChangeType || (exports.FileChangeType = {}));
var WatchKind;
(function (WatchKind) {
    /**
     * Interested in create events.
     */
    WatchKind.Create = 1;
    /**
     * Interested in change events
     */
    WatchKind.Change = 2;
    /**
     * Interested in delete events
     */
    WatchKind.Delete = 4;
})(WatchKind = exports.WatchKind || (exports.WatchKind = {}));
//---- Diagnostic notification ----
/**
 * Diagnostics notification are sent from the server to the client to signal
 * results of validation runs.
 */
var PublishDiagnosticsNotification;
(function (PublishDiagnosticsNotification) {
    PublishDiagnosticsNotification.type = new vscode_jsonrpc_1.NotificationType('textDocument/publishDiagnostics');
})(PublishDiagnosticsNotification = exports.PublishDiagnosticsNotification || (exports.PublishDiagnosticsNotification = {}));
/**
 * How a completion was triggered
 */
var CompletionTriggerKind;
(function (CompletionTriggerKind) {
    /**
     * Completion was triggered by typing an identifier (24x7 code
     * complete), manual invocation (e.g Ctrl+Space) or via API.
     */
    CompletionTriggerKind.Invoked = 1;
    /**
     * Completion was triggered by a trigger character specified by
     * the `triggerCharacters` properties of the `CompletionRegistrationOptions`.
     */
    CompletionTriggerKind.TriggerCharacter = 2;
    /**
     * Completion was re-triggered as current completion list is incomplete
     */
    CompletionTriggerKind.TriggerForIncompleteCompletions = 3;
})(CompletionTriggerKind = exports.CompletionTriggerKind || (exports.CompletionTriggerKind = {}));
/**
 * Request to request completion at a given text document position. The request's
 * parameter is of type [TextDocumentPosition](#TextDocumentPosition) the response
 * is of type [CompletionItem[]](#CompletionItem) or [CompletionList](#CompletionList)
 * or a Thenable that resolves to such.
 *
 * The request can delay the computation of the [`detail`](#CompletionItem.detail)
 * and [`documentation`](#CompletionItem.documentation) properties to the `completionItem/resolve`
 * request. However, properties that are needed for the initial sorting and filtering, like `sortText`,
 * `filterText`, `insertText`, and `textEdit`, must not be changed during resolve.
 */
var CompletionRequest;
(function (CompletionRequest) {
    CompletionRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/completion');
})(CompletionRequest = exports.CompletionRequest || (exports.CompletionRequest = {}));
/**
 * Request to resolve additional information for a given completion item.The request's
 * parameter is of type [CompletionItem](#CompletionItem) the response
 * is of type [CompletionItem](#CompletionItem) or a Thenable that resolves to such.
 */
var CompletionResolveRequest;
(function (CompletionResolveRequest) {
    CompletionResolveRequest.type = new vscode_jsonrpc_1.RequestType('completionItem/resolve');
})(CompletionResolveRequest = exports.CompletionResolveRequest || (exports.CompletionResolveRequest = {}));
//---- Hover Support -------------------------------
/**
 * Request to request hover information at a given text document position. The request's
 * parameter is of type [TextDocumentPosition](#TextDocumentPosition) the response is of
 * type [Hover](#Hover) or a Thenable that resolves to such.
 */
var HoverRequest;
(function (HoverRequest) {
    HoverRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/hover');
})(HoverRequest = exports.HoverRequest || (exports.HoverRequest = {}));
var SignatureHelpRequest;
(function (SignatureHelpRequest) {
    SignatureHelpRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/signatureHelp');
})(SignatureHelpRequest = exports.SignatureHelpRequest || (exports.SignatureHelpRequest = {}));
//---- Goto Definition -------------------------------------
/**
 * A request to resolve the definition location of a symbol at a given text
 * document position. The request's parameter is of type [TextDocumentPosition]
 * (#TextDocumentPosition) the response is of either type [Definition](#Definition)
 * or a typed array of [DefinitionLink](#DefinitionLink) or a Thenable that resolves
 * to such.
 */
var DefinitionRequest;
(function (DefinitionRequest) {
    DefinitionRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/definition');
})(DefinitionRequest = exports.DefinitionRequest || (exports.DefinitionRequest = {}));
/**
 * A request to resolve project-wide references for the symbol denoted
 * by the given text document position. The request's parameter is of
 * type [ReferenceParams](#ReferenceParams) the response is of type
 * [Location[]](#Location) or a Thenable that resolves to such.
 */
var ReferencesRequest;
(function (ReferencesRequest) {
    ReferencesRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/references');
})(ReferencesRequest = exports.ReferencesRequest || (exports.ReferencesRequest = {}));
//---- Document Highlight ----------------------------------
/**
 * Request to resolve a [DocumentHighlight](#DocumentHighlight) for a given
 * text document position. The request's parameter is of type [TextDocumentPosition]
 * (#TextDocumentPosition) the request response is of type [DocumentHighlight[]]
 * (#DocumentHighlight) or a Thenable that resolves to such.
 */
var DocumentHighlightRequest;
(function (DocumentHighlightRequest) {
    DocumentHighlightRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/documentHighlight');
})(DocumentHighlightRequest = exports.DocumentHighlightRequest || (exports.DocumentHighlightRequest = {}));
//---- Document Symbol Provider ---------------------------
/**
 * A request to list all symbols found in a given text document. The request's
 * parameter is of type [TextDocumentIdentifier](#TextDocumentIdentifier) the
 * response is of type [SymbolInformation[]](#SymbolInformation) or a Thenable
 * that resolves to such.
 */
var DocumentSymbolRequest;
(function (DocumentSymbolRequest) {
    DocumentSymbolRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/documentSymbol');
})(DocumentSymbolRequest = exports.DocumentSymbolRequest || (exports.DocumentSymbolRequest = {}));
//---- Workspace Symbol Provider ---------------------------
/**
 * A request to list project-wide symbols matching the query string given
 * by the [WorkspaceSymbolParams](#WorkspaceSymbolParams). The response is
 * of type [SymbolInformation[]](#SymbolInformation) or a Thenable that
 * resolves to such.
 */
var WorkspaceSymbolRequest;
(function (WorkspaceSymbolRequest) {
    WorkspaceSymbolRequest.type = new vscode_jsonrpc_1.RequestType('workspace/symbol');
})(WorkspaceSymbolRequest = exports.WorkspaceSymbolRequest || (exports.WorkspaceSymbolRequest = {}));
/**
 * A request to provide commands for the given text document and range.
 */
var CodeActionRequest;
(function (CodeActionRequest) {
    CodeActionRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/codeAction');
})(CodeActionRequest = exports.CodeActionRequest || (exports.CodeActionRequest = {}));
/**
 * A request to provide code lens for the given text document.
 */
var CodeLensRequest;
(function (CodeLensRequest) {
    CodeLensRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/codeLens');
})(CodeLensRequest = exports.CodeLensRequest || (exports.CodeLensRequest = {}));
/**
 * A request to resolve a command for a given code lens.
 */
var CodeLensResolveRequest;
(function (CodeLensResolveRequest) {
    CodeLensResolveRequest.type = new vscode_jsonrpc_1.RequestType('codeLens/resolve');
})(CodeLensResolveRequest = exports.CodeLensResolveRequest || (exports.CodeLensResolveRequest = {}));
/**
 * A request to to format a whole document.
 */
var DocumentFormattingRequest;
(function (DocumentFormattingRequest) {
    DocumentFormattingRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/formatting');
})(DocumentFormattingRequest = exports.DocumentFormattingRequest || (exports.DocumentFormattingRequest = {}));
/**
 * A request to to format a range in a document.
 */
var DocumentRangeFormattingRequest;
(function (DocumentRangeFormattingRequest) {
    DocumentRangeFormattingRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/rangeFormatting');
})(DocumentRangeFormattingRequest = exports.DocumentRangeFormattingRequest || (exports.DocumentRangeFormattingRequest = {}));
/**
 * A request to format a document on type.
 */
var DocumentOnTypeFormattingRequest;
(function (DocumentOnTypeFormattingRequest) {
    DocumentOnTypeFormattingRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/onTypeFormatting');
})(DocumentOnTypeFormattingRequest = exports.DocumentOnTypeFormattingRequest || (exports.DocumentOnTypeFormattingRequest = {}));
/**
 * A request to rename a symbol.
 */
var RenameRequest;
(function (RenameRequest) {
    RenameRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/rename');
})(RenameRequest = exports.RenameRequest || (exports.RenameRequest = {}));
/**
 * A request to test and perform the setup necessary for a rename.
 */
var PrepareRenameRequest;
(function (PrepareRenameRequest) {
    PrepareRenameRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/prepareRename');
})(PrepareRenameRequest = exports.PrepareRenameRequest || (exports.PrepareRenameRequest = {}));
/**
 * A request to provide document links
 */
var DocumentLinkRequest;
(function (DocumentLinkRequest) {
    DocumentLinkRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/documentLink');
})(DocumentLinkRequest = exports.DocumentLinkRequest || (exports.DocumentLinkRequest = {}));
/**
 * Request to resolve additional information for a given document link. The request's
 * parameter is of type [DocumentLink](#DocumentLink) the response
 * is of type [DocumentLink](#DocumentLink) or a Thenable that resolves to such.
 */
var DocumentLinkResolveRequest;
(function (DocumentLinkResolveRequest) {
    DocumentLinkResolveRequest.type = new vscode_jsonrpc_1.RequestType('documentLink/resolve');
})(DocumentLinkResolveRequest = exports.DocumentLinkResolveRequest || (exports.DocumentLinkResolveRequest = {}));
/**
 * A request send from the client to the server to execute a command. The request might return
 * a workspace edit which the client will apply to the workspace.
 */
var ExecuteCommandRequest;
(function (ExecuteCommandRequest) {
    ExecuteCommandRequest.type = new vscode_jsonrpc_1.RequestType('workspace/executeCommand');
})(ExecuteCommandRequest = exports.ExecuteCommandRequest || (exports.ExecuteCommandRequest = {}));
/**
 * A request sent from the server to the client to modified certain resources.
 */
var ApplyWorkspaceEditRequest;
(function (ApplyWorkspaceEditRequest) {
    ApplyWorkspaceEditRequest.type = new vscode_jsonrpc_1.RequestType('workspace/applyEdit');
})(ApplyWorkspaceEditRequest = exports.ApplyWorkspaceEditRequest || (exports.ApplyWorkspaceEditRequest = {}));


/***/ }),

/***/ 40:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const vscode_jsonrpc_1 = __webpack_require__(231);
// @ts-ignore: to avoid inlining LocatioLink as dynamic import
let __noDynamicImport;
/**
 * A request to resolve the type definition locations of a symbol at a given text
 * document position. The request's parameter is of type [TextDocumentPositioParams]
 * (#TextDocumentPositionParams) the response is of type [Definition](#Definition) or a
 * Thenable that resolves to such.
 */
var TypeDefinitionRequest;
(function (TypeDefinitionRequest) {
    TypeDefinitionRequest.type = new vscode_jsonrpc_1.RequestType('textDocument/typeDefinition');
})(TypeDefinitionRequest = exports.TypeDefinitionRequest || (exports.TypeDefinitionRequest = {}));


/***/ }),

/***/ 942:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
const vscode_jsonrpc_1 = __webpack_require__(231);
/**
 * The `workspace/workspaceFolders` is sent from the server to the client to fetch the open workspace folders.
 */
var WorkspaceFoldersRequest;
(function (WorkspaceFoldersRequest) {
    WorkspaceFoldersRequest.type = new vscode_jsonrpc_1.RequestType0('workspace/workspaceFolders');
})(WorkspaceFoldersRequest = exports.WorkspaceFoldersRequest || (exports.WorkspaceFoldersRequest = {}));
/**
 * The `workspace/didChangeWorkspaceFolders` notification is sent from the client to the server when the workspace
 * folder configuration changes.
 */
var DidChangeWorkspaceFoldersNotification;
(function (DidChangeWorkspaceFoldersNotification) {
    DidChangeWorkspaceFoldersNotification.type = new vscode_jsonrpc_1.NotificationType('workspace/didChangeWorkspaceFolders');
})(DidChangeWorkspaceFoldersNotification = exports.DidChangeWorkspaceFoldersNotification || (exports.DidChangeWorkspaceFoldersNotification = {}));


/***/ }),

/***/ 381:
/***/ ((__unused_webpack_module, exports) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
function boolean(value) {
    return value === true || value === false;
}
exports.boolean = boolean;
function string(value) {
    return typeof value === 'string' || value instanceof String;
}
exports.string = string;
function number(value) {
    return typeof value === 'number' || value instanceof Number;
}
exports.number = number;
function error(value) {
    return value instanceof Error;
}
exports.error = error;
function func(value) {
    return typeof value === 'function';
}
exports.func = func;
function array(value) {
    return Array.isArray(value);
}
exports.array = array;
function stringArray(value) {
    return array(value) && value.every(elem => string(elem));
}
exports.stringArray = stringArray;
function typedArray(value, check) {
    return Array.isArray(value) && value.every(check);
}
exports.typedArray = typedArray;
function thenable(value) {
    return value && func(value.then);
}
exports.thenable = thenable;


/***/ }),

/***/ 674:
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "CodeAction": () => (/* binding */ CodeAction),
/* harmony export */   "CodeActionContext": () => (/* binding */ CodeActionContext),
/* harmony export */   "CodeActionKind": () => (/* binding */ CodeActionKind),
/* harmony export */   "CodeLens": () => (/* binding */ CodeLens),
/* harmony export */   "Color": () => (/* binding */ Color),
/* harmony export */   "ColorInformation": () => (/* binding */ ColorInformation),
/* harmony export */   "ColorPresentation": () => (/* binding */ ColorPresentation),
/* harmony export */   "Command": () => (/* binding */ Command),
/* harmony export */   "CompletionItem": () => (/* binding */ CompletionItem),
/* harmony export */   "CompletionItemKind": () => (/* binding */ CompletionItemKind),
/* harmony export */   "CompletionList": () => (/* binding */ CompletionList),
/* harmony export */   "CreateFile": () => (/* binding */ CreateFile),
/* harmony export */   "DeleteFile": () => (/* binding */ DeleteFile),
/* harmony export */   "Diagnostic": () => (/* binding */ Diagnostic),
/* harmony export */   "DiagnosticRelatedInformation": () => (/* binding */ DiagnosticRelatedInformation),
/* harmony export */   "DiagnosticSeverity": () => (/* binding */ DiagnosticSeverity),
/* harmony export */   "DocumentHighlight": () => (/* binding */ DocumentHighlight),
/* harmony export */   "DocumentHighlightKind": () => (/* binding */ DocumentHighlightKind),
/* harmony export */   "DocumentLink": () => (/* binding */ DocumentLink),
/* harmony export */   "DocumentSymbol": () => (/* binding */ DocumentSymbol),
/* harmony export */   "EOL": () => (/* binding */ EOL),
/* harmony export */   "FoldingRange": () => (/* binding */ FoldingRange),
/* harmony export */   "FoldingRangeKind": () => (/* binding */ FoldingRangeKind),
/* harmony export */   "FormattingOptions": () => (/* binding */ FormattingOptions),
/* harmony export */   "Hover": () => (/* binding */ Hover),
/* harmony export */   "InsertTextFormat": () => (/* binding */ InsertTextFormat),
/* harmony export */   "Location": () => (/* binding */ Location),
/* harmony export */   "LocationLink": () => (/* binding */ LocationLink),
/* harmony export */   "MarkedString": () => (/* binding */ MarkedString),
/* harmony export */   "MarkupContent": () => (/* binding */ MarkupContent),
/* harmony export */   "MarkupKind": () => (/* binding */ MarkupKind),
/* harmony export */   "ParameterInformation": () => (/* binding */ ParameterInformation),
/* harmony export */   "Position": () => (/* binding */ Position),
/* harmony export */   "Range": () => (/* binding */ Range),
/* harmony export */   "RenameFile": () => (/* binding */ RenameFile),
/* harmony export */   "SignatureInformation": () => (/* binding */ SignatureInformation),
/* harmony export */   "SymbolInformation": () => (/* binding */ SymbolInformation),
/* harmony export */   "SymbolKind": () => (/* binding */ SymbolKind),
/* harmony export */   "TextDocument": () => (/* binding */ TextDocument),
/* harmony export */   "TextDocumentEdit": () => (/* binding */ TextDocumentEdit),
/* harmony export */   "TextDocumentIdentifier": () => (/* binding */ TextDocumentIdentifier),
/* harmony export */   "TextDocumentItem": () => (/* binding */ TextDocumentItem),
/* harmony export */   "TextDocumentSaveReason": () => (/* binding */ TextDocumentSaveReason),
/* harmony export */   "TextEdit": () => (/* binding */ TextEdit),
/* harmony export */   "VersionedTextDocumentIdentifier": () => (/* binding */ VersionedTextDocumentIdentifier),
/* harmony export */   "WorkspaceChange": () => (/* binding */ WorkspaceChange),
/* harmony export */   "WorkspaceEdit": () => (/* binding */ WorkspaceEdit)
/* harmony export */ });
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

/**
 * The Position namespace provides helper functions to work with
 * [Position](#Position) literals.
 */
var Position;
(function (Position) {
    /**
     * Creates a new Position literal from the given line and character.
     * @param line The position's line.
     * @param character The position's character.
     */
    function create(line, character) {
        return { line: line, character: character };
    }
    Position.create = create;
    /**
     * Checks whether the given liternal conforms to the [Position](#Position) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.number(candidate.line) && Is.number(candidate.character);
    }
    Position.is = is;
})(Position || (Position = {}));
/**
 * The Range namespace provides helper functions to work with
 * [Range](#Range) literals.
 */
var Range;
(function (Range) {
    function create(one, two, three, four) {
        if (Is.number(one) && Is.number(two) && Is.number(three) && Is.number(four)) {
            return { start: Position.create(one, two), end: Position.create(three, four) };
        }
        else if (Position.is(one) && Position.is(two)) {
            return { start: one, end: two };
        }
        else {
            throw new Error("Range#create called with invalid arguments[" + one + ", " + two + ", " + three + ", " + four + "]");
        }
    }
    Range.create = create;
    /**
     * Checks whether the given literal conforms to the [Range](#Range) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Position.is(candidate.start) && Position.is(candidate.end);
    }
    Range.is = is;
})(Range || (Range = {}));
/**
 * The Location namespace provides helper functions to work with
 * [Location](#Location) literals.
 */
var Location;
(function (Location) {
    /**
     * Creates a Location literal.
     * @param uri The location's uri.
     * @param range The location's range.
     */
    function create(uri, range) {
        return { uri: uri, range: range };
    }
    Location.create = create;
    /**
     * Checks whether the given literal conforms to the [Location](#Location) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.range) && (Is.string(candidate.uri) || Is.undefined(candidate.uri));
    }
    Location.is = is;
})(Location || (Location = {}));
/**
 * The LocationLink namespace provides helper functions to work with
 * [LocationLink](#LocationLink) literals.
 */
var LocationLink;
(function (LocationLink) {
    /**
     * Creates a LocationLink literal.
     * @param targetUri The definition's uri.
     * @param targetRange The full range of the definition.
     * @param targetSelectionRange The span of the symbol definition at the target.
     * @param originSelectionRange The span of the symbol being defined in the originating source file.
     */
    function create(targetUri, targetRange, targetSelectionRange, originSelectionRange) {
        return { targetUri: targetUri, targetRange: targetRange, targetSelectionRange: targetSelectionRange, originSelectionRange: originSelectionRange };
    }
    LocationLink.create = create;
    /**
     * Checks whether the given literal conforms to the [LocationLink](#LocationLink) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.targetRange) && Is.string(candidate.targetUri)
            && (Range.is(candidate.targetSelectionRange) || Is.undefined(candidate.targetSelectionRange))
            && (Range.is(candidate.originSelectionRange) || Is.undefined(candidate.originSelectionRange));
    }
    LocationLink.is = is;
})(LocationLink || (LocationLink = {}));
/**
 * The Color namespace provides helper functions to work with
 * [Color](#Color) literals.
 */
var Color;
(function (Color) {
    /**
     * Creates a new Color literal.
     */
    function create(red, green, blue, alpha) {
        return {
            red: red,
            green: green,
            blue: blue,
            alpha: alpha,
        };
    }
    Color.create = create;
    /**
     * Checks whether the given literal conforms to the [Color](#Color) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.number(candidate.red)
            && Is.number(candidate.green)
            && Is.number(candidate.blue)
            && Is.number(candidate.alpha);
    }
    Color.is = is;
})(Color || (Color = {}));
/**
 * The ColorInformation namespace provides helper functions to work with
 * [ColorInformation](#ColorInformation) literals.
 */
var ColorInformation;
(function (ColorInformation) {
    /**
     * Creates a new ColorInformation literal.
     */
    function create(range, color) {
        return {
            range: range,
            color: color,
        };
    }
    ColorInformation.create = create;
    /**
     * Checks whether the given literal conforms to the [ColorInformation](#ColorInformation) interface.
     */
    function is(value) {
        var candidate = value;
        return Range.is(candidate.range) && Color.is(candidate.color);
    }
    ColorInformation.is = is;
})(ColorInformation || (ColorInformation = {}));
/**
 * The Color namespace provides helper functions to work with
 * [ColorPresentation](#ColorPresentation) literals.
 */
var ColorPresentation;
(function (ColorPresentation) {
    /**
     * Creates a new ColorInformation literal.
     */
    function create(label, textEdit, additionalTextEdits) {
        return {
            label: label,
            textEdit: textEdit,
            additionalTextEdits: additionalTextEdits,
        };
    }
    ColorPresentation.create = create;
    /**
     * Checks whether the given literal conforms to the [ColorInformation](#ColorInformation) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.string(candidate.label)
            && (Is.undefined(candidate.textEdit) || TextEdit.is(candidate))
            && (Is.undefined(candidate.additionalTextEdits) || Is.typedArray(candidate.additionalTextEdits, TextEdit.is));
    }
    ColorPresentation.is = is;
})(ColorPresentation || (ColorPresentation = {}));
/**
 * Enum of known range kinds
 */
var FoldingRangeKind;
(function (FoldingRangeKind) {
    /**
     * Folding range for a comment
     */
    FoldingRangeKind["Comment"] = "comment";
    /**
     * Folding range for a imports or includes
     */
    FoldingRangeKind["Imports"] = "imports";
    /**
     * Folding range for a region (e.g. `#region`)
     */
    FoldingRangeKind["Region"] = "region";
})(FoldingRangeKind || (FoldingRangeKind = {}));
/**
 * The folding range namespace provides helper functions to work with
 * [FoldingRange](#FoldingRange) literals.
 */
var FoldingRange;
(function (FoldingRange) {
    /**
     * Creates a new FoldingRange literal.
     */
    function create(startLine, endLine, startCharacter, endCharacter, kind) {
        var result = {
            startLine: startLine,
            endLine: endLine
        };
        if (Is.defined(startCharacter)) {
            result.startCharacter = startCharacter;
        }
        if (Is.defined(endCharacter)) {
            result.endCharacter = endCharacter;
        }
        if (Is.defined(kind)) {
            result.kind = kind;
        }
        return result;
    }
    FoldingRange.create = create;
    /**
     * Checks whether the given literal conforms to the [FoldingRange](#FoldingRange) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.number(candidate.startLine) && Is.number(candidate.startLine)
            && (Is.undefined(candidate.startCharacter) || Is.number(candidate.startCharacter))
            && (Is.undefined(candidate.endCharacter) || Is.number(candidate.endCharacter))
            && (Is.undefined(candidate.kind) || Is.string(candidate.kind));
    }
    FoldingRange.is = is;
})(FoldingRange || (FoldingRange = {}));
/**
 * The DiagnosticRelatedInformation namespace provides helper functions to work with
 * [DiagnosticRelatedInformation](#DiagnosticRelatedInformation) literals.
 */
var DiagnosticRelatedInformation;
(function (DiagnosticRelatedInformation) {
    /**
     * Creates a new DiagnosticRelatedInformation literal.
     */
    function create(location, message) {
        return {
            location: location,
            message: message
        };
    }
    DiagnosticRelatedInformation.create = create;
    /**
     * Checks whether the given literal conforms to the [DiagnosticRelatedInformation](#DiagnosticRelatedInformation) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Location.is(candidate.location) && Is.string(candidate.message);
    }
    DiagnosticRelatedInformation.is = is;
})(DiagnosticRelatedInformation || (DiagnosticRelatedInformation = {}));
/**
 * The diagnostic's severity.
 */
var DiagnosticSeverity;
(function (DiagnosticSeverity) {
    /**
     * Reports an error.
     */
    DiagnosticSeverity.Error = 1;
    /**
     * Reports a warning.
     */
    DiagnosticSeverity.Warning = 2;
    /**
     * Reports an information.
     */
    DiagnosticSeverity.Information = 3;
    /**
     * Reports a hint.
     */
    DiagnosticSeverity.Hint = 4;
})(DiagnosticSeverity || (DiagnosticSeverity = {}));
/**
 * The Diagnostic namespace provides helper functions to work with
 * [Diagnostic](#Diagnostic) literals.
 */
var Diagnostic;
(function (Diagnostic) {
    /**
     * Creates a new Diagnostic literal.
     */
    function create(range, message, severity, code, source, relatedInformation) {
        var result = { range: range, message: message };
        if (Is.defined(severity)) {
            result.severity = severity;
        }
        if (Is.defined(code)) {
            result.code = code;
        }
        if (Is.defined(source)) {
            result.source = source;
        }
        if (Is.defined(relatedInformation)) {
            result.relatedInformation = relatedInformation;
        }
        return result;
    }
    Diagnostic.create = create;
    /**
     * Checks whether the given literal conforms to the [Diagnostic](#Diagnostic) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate)
            && Range.is(candidate.range)
            && Is.string(candidate.message)
            && (Is.number(candidate.severity) || Is.undefined(candidate.severity))
            && (Is.number(candidate.code) || Is.string(candidate.code) || Is.undefined(candidate.code))
            && (Is.string(candidate.source) || Is.undefined(candidate.source))
            && (Is.undefined(candidate.relatedInformation) || Is.typedArray(candidate.relatedInformation, DiagnosticRelatedInformation.is));
    }
    Diagnostic.is = is;
})(Diagnostic || (Diagnostic = {}));
/**
 * The Command namespace provides helper functions to work with
 * [Command](#Command) literals.
 */
var Command;
(function (Command) {
    /**
     * Creates a new Command literal.
     */
    function create(title, command) {
        var args = [];
        for (var _i = 2; _i < arguments.length; _i++) {
            args[_i - 2] = arguments[_i];
        }
        var result = { title: title, command: command };
        if (Is.defined(args) && args.length > 0) {
            result.arguments = args;
        }
        return result;
    }
    Command.create = create;
    /**
     * Checks whether the given literal conforms to the [Command](#Command) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.title) && Is.string(candidate.command);
    }
    Command.is = is;
})(Command || (Command = {}));
/**
 * The TextEdit namespace provides helper function to create replace,
 * insert and delete edits more easily.
 */
var TextEdit;
(function (TextEdit) {
    /**
     * Creates a replace text edit.
     * @param range The range of text to be replaced.
     * @param newText The new text.
     */
    function replace(range, newText) {
        return { range: range, newText: newText };
    }
    TextEdit.replace = replace;
    /**
     * Creates a insert text edit.
     * @param position The position to insert the text at.
     * @param newText The text to be inserted.
     */
    function insert(position, newText) {
        return { range: { start: position, end: position }, newText: newText };
    }
    TextEdit.insert = insert;
    /**
     * Creates a delete text edit.
     * @param range The range of text to be deleted.
     */
    function del(range) {
        return { range: range, newText: '' };
    }
    TextEdit.del = del;
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate)
            && Is.string(candidate.newText)
            && Range.is(candidate.range);
    }
    TextEdit.is = is;
})(TextEdit || (TextEdit = {}));
/**
 * The TextDocumentEdit namespace provides helper function to create
 * an edit that manipulates a text document.
 */
var TextDocumentEdit;
(function (TextDocumentEdit) {
    /**
     * Creates a new `TextDocumentEdit`
     */
    function create(textDocument, edits) {
        return { textDocument: textDocument, edits: edits };
    }
    TextDocumentEdit.create = create;
    function is(value) {
        var candidate = value;
        return Is.defined(candidate)
            && VersionedTextDocumentIdentifier.is(candidate.textDocument)
            && Array.isArray(candidate.edits);
    }
    TextDocumentEdit.is = is;
})(TextDocumentEdit || (TextDocumentEdit = {}));
var CreateFile;
(function (CreateFile) {
    function create(uri, options) {
        var result = {
            kind: 'create',
            uri: uri
        };
        if (options !== void 0 && (options.overwrite !== void 0 || options.ignoreIfExists !== void 0)) {
            result.options = options;
        }
        return result;
    }
    CreateFile.create = create;
    function is(value) {
        var candidate = value;
        return candidate && candidate.kind === 'create' && Is.string(candidate.uri) &&
            (candidate.options === void 0 ||
                ((candidate.options.overwrite === void 0 || Is.boolean(candidate.options.overwrite)) && (candidate.options.ignoreIfExists === void 0 || Is.boolean(candidate.options.ignoreIfExists))));
    }
    CreateFile.is = is;
})(CreateFile || (CreateFile = {}));
var RenameFile;
(function (RenameFile) {
    function create(oldUri, newUri, options) {
        var result = {
            kind: 'rename',
            oldUri: oldUri,
            newUri: newUri
        };
        if (options !== void 0 && (options.overwrite !== void 0 || options.ignoreIfExists !== void 0)) {
            result.options = options;
        }
        return result;
    }
    RenameFile.create = create;
    function is(value) {
        var candidate = value;
        return candidate && candidate.kind === 'rename' && Is.string(candidate.oldUri) && Is.string(candidate.newUri) &&
            (candidate.options === void 0 ||
                ((candidate.options.overwrite === void 0 || Is.boolean(candidate.options.overwrite)) && (candidate.options.ignoreIfExists === void 0 || Is.boolean(candidate.options.ignoreIfExists))));
    }
    RenameFile.is = is;
})(RenameFile || (RenameFile = {}));
var DeleteFile;
(function (DeleteFile) {
    function create(uri, options) {
        var result = {
            kind: 'delete',
            uri: uri
        };
        if (options !== void 0 && (options.recursive !== void 0 || options.ignoreIfNotExists !== void 0)) {
            result.options = options;
        }
        return result;
    }
    DeleteFile.create = create;
    function is(value) {
        var candidate = value;
        return candidate && candidate.kind === 'delete' && Is.string(candidate.uri) &&
            (candidate.options === void 0 ||
                ((candidate.options.recursive === void 0 || Is.boolean(candidate.options.recursive)) && (candidate.options.ignoreIfNotExists === void 0 || Is.boolean(candidate.options.ignoreIfNotExists))));
    }
    DeleteFile.is = is;
})(DeleteFile || (DeleteFile = {}));
var WorkspaceEdit;
(function (WorkspaceEdit) {
    function is(value) {
        var candidate = value;
        return candidate &&
            (candidate.changes !== void 0 || candidate.documentChanges !== void 0) &&
            (candidate.documentChanges === void 0 || candidate.documentChanges.every(function (change) {
                if (Is.string(change.kind)) {
                    return CreateFile.is(change) || RenameFile.is(change) || DeleteFile.is(change);
                }
                else {
                    return TextDocumentEdit.is(change);
                }
            }));
    }
    WorkspaceEdit.is = is;
})(WorkspaceEdit || (WorkspaceEdit = {}));
var TextEditChangeImpl = /** @class */ (function () {
    function TextEditChangeImpl(edits) {
        this.edits = edits;
    }
    TextEditChangeImpl.prototype.insert = function (position, newText) {
        this.edits.push(TextEdit.insert(position, newText));
    };
    TextEditChangeImpl.prototype.replace = function (range, newText) {
        this.edits.push(TextEdit.replace(range, newText));
    };
    TextEditChangeImpl.prototype.delete = function (range) {
        this.edits.push(TextEdit.del(range));
    };
    TextEditChangeImpl.prototype.add = function (edit) {
        this.edits.push(edit);
    };
    TextEditChangeImpl.prototype.all = function () {
        return this.edits;
    };
    TextEditChangeImpl.prototype.clear = function () {
        this.edits.splice(0, this.edits.length);
    };
    return TextEditChangeImpl;
}());
/**
 * A workspace change helps constructing changes to a workspace.
 */
var WorkspaceChange = /** @class */ (function () {
    function WorkspaceChange(workspaceEdit) {
        var _this = this;
        this._textEditChanges = Object.create(null);
        if (workspaceEdit) {
            this._workspaceEdit = workspaceEdit;
            if (workspaceEdit.documentChanges) {
                workspaceEdit.documentChanges.forEach(function (change) {
                    if (TextDocumentEdit.is(change)) {
                        var textEditChange = new TextEditChangeImpl(change.edits);
                        _this._textEditChanges[change.textDocument.uri] = textEditChange;
                    }
                });
            }
            else if (workspaceEdit.changes) {
                Object.keys(workspaceEdit.changes).forEach(function (key) {
                    var textEditChange = new TextEditChangeImpl(workspaceEdit.changes[key]);
                    _this._textEditChanges[key] = textEditChange;
                });
            }
        }
    }
    Object.defineProperty(WorkspaceChange.prototype, "edit", {
        /**
         * Returns the underlying [WorkspaceEdit](#WorkspaceEdit) literal
         * use to be returned from a workspace edit operation like rename.
         */
        get: function () {
            return this._workspaceEdit;
        },
        enumerable: true,
        configurable: true
    });
    WorkspaceChange.prototype.getTextEditChange = function (key) {
        if (VersionedTextDocumentIdentifier.is(key)) {
            if (!this._workspaceEdit) {
                this._workspaceEdit = {
                    documentChanges: []
                };
            }
            if (!this._workspaceEdit.documentChanges) {
                throw new Error('Workspace edit is not configured for document changes.');
            }
            var textDocument = key;
            var result = this._textEditChanges[textDocument.uri];
            if (!result) {
                var edits = [];
                var textDocumentEdit = {
                    textDocument: textDocument,
                    edits: edits
                };
                this._workspaceEdit.documentChanges.push(textDocumentEdit);
                result = new TextEditChangeImpl(edits);
                this._textEditChanges[textDocument.uri] = result;
            }
            return result;
        }
        else {
            if (!this._workspaceEdit) {
                this._workspaceEdit = {
                    changes: Object.create(null)
                };
            }
            if (!this._workspaceEdit.changes) {
                throw new Error('Workspace edit is not configured for normal text edit changes.');
            }
            var result = this._textEditChanges[key];
            if (!result) {
                var edits = [];
                this._workspaceEdit.changes[key] = edits;
                result = new TextEditChangeImpl(edits);
                this._textEditChanges[key] = result;
            }
            return result;
        }
    };
    WorkspaceChange.prototype.createFile = function (uri, options) {
        this.checkDocumentChanges();
        this._workspaceEdit.documentChanges.push(CreateFile.create(uri, options));
    };
    WorkspaceChange.prototype.renameFile = function (oldUri, newUri, options) {
        this.checkDocumentChanges();
        this._workspaceEdit.documentChanges.push(RenameFile.create(oldUri, newUri, options));
    };
    WorkspaceChange.prototype.deleteFile = function (uri, options) {
        this.checkDocumentChanges();
        this._workspaceEdit.documentChanges.push(DeleteFile.create(uri, options));
    };
    WorkspaceChange.prototype.checkDocumentChanges = function () {
        if (!this._workspaceEdit || !this._workspaceEdit.documentChanges) {
            throw new Error('Workspace edit is not configured for document changes.');
        }
    };
    return WorkspaceChange;
}());

/**
 * The TextDocumentIdentifier namespace provides helper functions to work with
 * [TextDocumentIdentifier](#TextDocumentIdentifier) literals.
 */
var TextDocumentIdentifier;
(function (TextDocumentIdentifier) {
    /**
     * Creates a new TextDocumentIdentifier literal.
     * @param uri The document's uri.
     */
    function create(uri) {
        return { uri: uri };
    }
    TextDocumentIdentifier.create = create;
    /**
     * Checks whether the given literal conforms to the [TextDocumentIdentifier](#TextDocumentIdentifier) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri);
    }
    TextDocumentIdentifier.is = is;
})(TextDocumentIdentifier || (TextDocumentIdentifier = {}));
/**
 * The VersionedTextDocumentIdentifier namespace provides helper functions to work with
 * [VersionedTextDocumentIdentifier](#VersionedTextDocumentIdentifier) literals.
 */
var VersionedTextDocumentIdentifier;
(function (VersionedTextDocumentIdentifier) {
    /**
     * Creates a new VersionedTextDocumentIdentifier literal.
     * @param uri The document's uri.
     * @param uri The document's text.
     */
    function create(uri, version) {
        return { uri: uri, version: version };
    }
    VersionedTextDocumentIdentifier.create = create;
    /**
     * Checks whether the given literal conforms to the [VersionedTextDocumentIdentifier](#VersionedTextDocumentIdentifier) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && (candidate.version === null || Is.number(candidate.version));
    }
    VersionedTextDocumentIdentifier.is = is;
})(VersionedTextDocumentIdentifier || (VersionedTextDocumentIdentifier = {}));
/**
 * The TextDocumentItem namespace provides helper functions to work with
 * [TextDocumentItem](#TextDocumentItem) literals.
 */
var TextDocumentItem;
(function (TextDocumentItem) {
    /**
     * Creates a new TextDocumentItem literal.
     * @param uri The document's uri.
     * @param languageId The document's language identifier.
     * @param version The document's version number.
     * @param text The document's text.
     */
    function create(uri, languageId, version, text) {
        return { uri: uri, languageId: languageId, version: version, text: text };
    }
    TextDocumentItem.create = create;
    /**
     * Checks whether the given literal conforms to the [TextDocumentItem](#TextDocumentItem) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && Is.string(candidate.languageId) && Is.number(candidate.version) && Is.string(candidate.text);
    }
    TextDocumentItem.is = is;
})(TextDocumentItem || (TextDocumentItem = {}));
/**
 * Describes the content type that a client supports in various
 * result literals like `Hover`, `ParameterInfo` or `CompletionItem`.
 *
 * Please note that `MarkupKinds` must not start with a `$`. This kinds
 * are reserved for internal usage.
 */
var MarkupKind;
(function (MarkupKind) {
    /**
     * Plain text is supported as a content format
     */
    MarkupKind.PlainText = 'plaintext';
    /**
     * Markdown is supported as a content format
     */
    MarkupKind.Markdown = 'markdown';
})(MarkupKind || (MarkupKind = {}));
(function (MarkupKind) {
    /**
     * Checks whether the given value is a value of the [MarkupKind](#MarkupKind) type.
     */
    function is(value) {
        var candidate = value;
        return candidate === MarkupKind.PlainText || candidate === MarkupKind.Markdown;
    }
    MarkupKind.is = is;
})(MarkupKind || (MarkupKind = {}));
var MarkupContent;
(function (MarkupContent) {
    /**
     * Checks whether the given value conforms to the [MarkupContent](#MarkupContent) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(value) && MarkupKind.is(candidate.kind) && Is.string(candidate.value);
    }
    MarkupContent.is = is;
})(MarkupContent || (MarkupContent = {}));
/**
 * The kind of a completion entry.
 */
var CompletionItemKind;
(function (CompletionItemKind) {
    CompletionItemKind.Text = 1;
    CompletionItemKind.Method = 2;
    CompletionItemKind.Function = 3;
    CompletionItemKind.Constructor = 4;
    CompletionItemKind.Field = 5;
    CompletionItemKind.Variable = 6;
    CompletionItemKind.Class = 7;
    CompletionItemKind.Interface = 8;
    CompletionItemKind.Module = 9;
    CompletionItemKind.Property = 10;
    CompletionItemKind.Unit = 11;
    CompletionItemKind.Value = 12;
    CompletionItemKind.Enum = 13;
    CompletionItemKind.Keyword = 14;
    CompletionItemKind.Snippet = 15;
    CompletionItemKind.Color = 16;
    CompletionItemKind.File = 17;
    CompletionItemKind.Reference = 18;
    CompletionItemKind.Folder = 19;
    CompletionItemKind.EnumMember = 20;
    CompletionItemKind.Constant = 21;
    CompletionItemKind.Struct = 22;
    CompletionItemKind.Event = 23;
    CompletionItemKind.Operator = 24;
    CompletionItemKind.TypeParameter = 25;
})(CompletionItemKind || (CompletionItemKind = {}));
/**
 * Defines whether the insert text in a completion item should be interpreted as
 * plain text or a snippet.
 */
var InsertTextFormat;
(function (InsertTextFormat) {
    /**
     * The primary text to be inserted is treated as a plain string.
     */
    InsertTextFormat.PlainText = 1;
    /**
     * The primary text to be inserted is treated as a snippet.
     *
     * A snippet can define tab stops and placeholders with `$1`, `$2`
     * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
     * the end of the snippet. Placeholders with equal identifiers are linked,
     * that is typing in one will update others too.
     *
     * See also: https://github.com/Microsoft/vscode/blob/master/src/vs/editor/contrib/snippet/common/snippet.md
     */
    InsertTextFormat.Snippet = 2;
})(InsertTextFormat || (InsertTextFormat = {}));
/**
 * The CompletionItem namespace provides functions to deal with
 * completion items.
 */
var CompletionItem;
(function (CompletionItem) {
    /**
     * Create a completion item and seed it with a label.
     * @param label The completion item's label
     */
    function create(label) {
        return { label: label };
    }
    CompletionItem.create = create;
})(CompletionItem || (CompletionItem = {}));
/**
 * The CompletionList namespace provides functions to deal with
 * completion lists.
 */
var CompletionList;
(function (CompletionList) {
    /**
     * Creates a new completion list.
     *
     * @param items The completion items.
     * @param isIncomplete The list is not complete.
     */
    function create(items, isIncomplete) {
        return { items: items ? items : [], isIncomplete: !!isIncomplete };
    }
    CompletionList.create = create;
})(CompletionList || (CompletionList = {}));
var MarkedString;
(function (MarkedString) {
    /**
     * Creates a marked string from plain text.
     *
     * @param plainText The plain text.
     */
    function fromPlainText(plainText) {
        return plainText.replace(/[\\`*_{}[\]()#+\-.!]/g, "\\$&"); // escape markdown syntax tokens: http://daringfireball.net/projects/markdown/syntax#backslash
    }
    MarkedString.fromPlainText = fromPlainText;
    /**
     * Checks whether the given value conforms to the [MarkedString](#MarkedString) type.
     */
    function is(value) {
        var candidate = value;
        return Is.string(candidate) || (Is.objectLiteral(candidate) && Is.string(candidate.language) && Is.string(candidate.value));
    }
    MarkedString.is = is;
})(MarkedString || (MarkedString = {}));
var Hover;
(function (Hover) {
    /**
     * Checks whether the given value conforms to the [Hover](#Hover) interface.
     */
    function is(value) {
        var candidate = value;
        return !!candidate && Is.objectLiteral(candidate) && (MarkupContent.is(candidate.contents) ||
            MarkedString.is(candidate.contents) ||
            Is.typedArray(candidate.contents, MarkedString.is)) && (value.range === void 0 || Range.is(value.range));
    }
    Hover.is = is;
})(Hover || (Hover = {}));
/**
 * The ParameterInformation namespace provides helper functions to work with
 * [ParameterInformation](#ParameterInformation) literals.
 */
var ParameterInformation;
(function (ParameterInformation) {
    /**
     * Creates a new parameter information literal.
     *
     * @param label A label string.
     * @param documentation A doc string.
     */
    function create(label, documentation) {
        return documentation ? { label: label, documentation: documentation } : { label: label };
    }
    ParameterInformation.create = create;
    ;
})(ParameterInformation || (ParameterInformation = {}));
/**
 * The SignatureInformation namespace provides helper functions to work with
 * [SignatureInformation](#SignatureInformation) literals.
 */
var SignatureInformation;
(function (SignatureInformation) {
    function create(label, documentation) {
        var parameters = [];
        for (var _i = 2; _i < arguments.length; _i++) {
            parameters[_i - 2] = arguments[_i];
        }
        var result = { label: label };
        if (Is.defined(documentation)) {
            result.documentation = documentation;
        }
        if (Is.defined(parameters)) {
            result.parameters = parameters;
        }
        else {
            result.parameters = [];
        }
        return result;
    }
    SignatureInformation.create = create;
})(SignatureInformation || (SignatureInformation = {}));
/**
 * A document highlight kind.
 */
var DocumentHighlightKind;
(function (DocumentHighlightKind) {
    /**
     * A textual occurrence.
     */
    DocumentHighlightKind.Text = 1;
    /**
     * Read-access of a symbol, like reading a variable.
     */
    DocumentHighlightKind.Read = 2;
    /**
     * Write-access of a symbol, like writing to a variable.
     */
    DocumentHighlightKind.Write = 3;
})(DocumentHighlightKind || (DocumentHighlightKind = {}));
/**
 * DocumentHighlight namespace to provide helper functions to work with
 * [DocumentHighlight](#DocumentHighlight) literals.
 */
var DocumentHighlight;
(function (DocumentHighlight) {
    /**
     * Create a DocumentHighlight object.
     * @param range The range the highlight applies to.
     */
    function create(range, kind) {
        var result = { range: range };
        if (Is.number(kind)) {
            result.kind = kind;
        }
        return result;
    }
    DocumentHighlight.create = create;
})(DocumentHighlight || (DocumentHighlight = {}));
/**
 * A symbol kind.
 */
var SymbolKind;
(function (SymbolKind) {
    SymbolKind.File = 1;
    SymbolKind.Module = 2;
    SymbolKind.Namespace = 3;
    SymbolKind.Package = 4;
    SymbolKind.Class = 5;
    SymbolKind.Method = 6;
    SymbolKind.Property = 7;
    SymbolKind.Field = 8;
    SymbolKind.Constructor = 9;
    SymbolKind.Enum = 10;
    SymbolKind.Interface = 11;
    SymbolKind.Function = 12;
    SymbolKind.Variable = 13;
    SymbolKind.Constant = 14;
    SymbolKind.String = 15;
    SymbolKind.Number = 16;
    SymbolKind.Boolean = 17;
    SymbolKind.Array = 18;
    SymbolKind.Object = 19;
    SymbolKind.Key = 20;
    SymbolKind.Null = 21;
    SymbolKind.EnumMember = 22;
    SymbolKind.Struct = 23;
    SymbolKind.Event = 24;
    SymbolKind.Operator = 25;
    SymbolKind.TypeParameter = 26;
})(SymbolKind || (SymbolKind = {}));
var SymbolInformation;
(function (SymbolInformation) {
    /**
     * Creates a new symbol information literal.
     *
     * @param name The name of the symbol.
     * @param kind The kind of the symbol.
     * @param range The range of the location of the symbol.
     * @param uri The resource of the location of symbol, defaults to the current document.
     * @param containerName The name of the symbol containing the symbol.
     */
    function create(name, kind, range, uri, containerName) {
        var result = {
            name: name,
            kind: kind,
            location: { uri: uri, range: range }
        };
        if (containerName) {
            result.containerName = containerName;
        }
        return result;
    }
    SymbolInformation.create = create;
})(SymbolInformation || (SymbolInformation = {}));
/**
 * Represents programming constructs like variables, classes, interfaces etc.
 * that appear in a document. Document symbols can be hierarchical and they
 * have two ranges: one that encloses its definition and one that points to
 * its most interesting range, e.g. the range of an identifier.
 */
var DocumentSymbol = /** @class */ (function () {
    function DocumentSymbol() {
    }
    return DocumentSymbol;
}());

(function (DocumentSymbol) {
    /**
     * Creates a new symbol information literal.
     *
     * @param name The name of the symbol.
     * @param detail The detail of the symbol.
     * @param kind The kind of the symbol.
     * @param range The range of the symbol.
     * @param selectionRange The selectionRange of the symbol.
     * @param children Children of the symbol.
     */
    function create(name, detail, kind, range, selectionRange, children) {
        var result = {
            name: name,
            detail: detail,
            kind: kind,
            range: range,
            selectionRange: selectionRange
        };
        if (children !== void 0) {
            result.children = children;
        }
        return result;
    }
    DocumentSymbol.create = create;
    /**
     * Checks whether the given literal conforms to the [DocumentSymbol](#DocumentSymbol) interface.
     */
    function is(value) {
        var candidate = value;
        return candidate &&
            Is.string(candidate.name) && Is.number(candidate.kind) &&
            Range.is(candidate.range) && Range.is(candidate.selectionRange) &&
            (candidate.detail === void 0 || Is.string(candidate.detail)) &&
            (candidate.deprecated === void 0 || Is.boolean(candidate.deprecated)) &&
            (candidate.children === void 0 || Array.isArray(candidate.children));
    }
    DocumentSymbol.is = is;
})(DocumentSymbol || (DocumentSymbol = {}));
/**
 * A set of predefined code action kinds
 */
var CodeActionKind;
(function (CodeActionKind) {
    /**
     * Base kind for quickfix actions: 'quickfix'
     */
    CodeActionKind.QuickFix = 'quickfix';
    /**
     * Base kind for refactoring actions: 'refactor'
     */
    CodeActionKind.Refactor = 'refactor';
    /**
     * Base kind for refactoring extraction actions: 'refactor.extract'
     *
     * Example extract actions:
     *
     * - Extract method
     * - Extract function
     * - Extract variable
     * - Extract interface from class
     * - ...
     */
    CodeActionKind.RefactorExtract = 'refactor.extract';
    /**
     * Base kind for refactoring inline actions: 'refactor.inline'
     *
     * Example inline actions:
     *
     * - Inline function
     * - Inline variable
     * - Inline constant
     * - ...
     */
    CodeActionKind.RefactorInline = 'refactor.inline';
    /**
     * Base kind for refactoring rewrite actions: 'refactor.rewrite'
     *
     * Example rewrite actions:
     *
     * - Convert JavaScript function to class
     * - Add or remove parameter
     * - Encapsulate field
     * - Make method static
     * - Move method to base class
     * - ...
     */
    CodeActionKind.RefactorRewrite = 'refactor.rewrite';
    /**
     * Base kind for source actions: `source`
     *
     * Source code actions apply to the entire file.
     */
    CodeActionKind.Source = 'source';
    /**
     * Base kind for an organize imports source action: `source.organizeImports`
     */
    CodeActionKind.SourceOrganizeImports = 'source.organizeImports';
})(CodeActionKind || (CodeActionKind = {}));
/**
 * The CodeActionContext namespace provides helper functions to work with
 * [CodeActionContext](#CodeActionContext) literals.
 */
var CodeActionContext;
(function (CodeActionContext) {
    /**
     * Creates a new CodeActionContext literal.
     */
    function create(diagnostics, only) {
        var result = { diagnostics: diagnostics };
        if (only !== void 0 && only !== null) {
            result.only = only;
        }
        return result;
    }
    CodeActionContext.create = create;
    /**
     * Checks whether the given literal conforms to the [CodeActionContext](#CodeActionContext) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.typedArray(candidate.diagnostics, Diagnostic.is) && (candidate.only === void 0 || Is.typedArray(candidate.only, Is.string));
    }
    CodeActionContext.is = is;
})(CodeActionContext || (CodeActionContext = {}));
var CodeAction;
(function (CodeAction) {
    function create(title, commandOrEdit, kind) {
        var result = { title: title };
        if (Command.is(commandOrEdit)) {
            result.command = commandOrEdit;
        }
        else {
            result.edit = commandOrEdit;
        }
        if (kind !== void null) {
            result.kind = kind;
        }
        return result;
    }
    CodeAction.create = create;
    function is(value) {
        var candidate = value;
        return candidate && Is.string(candidate.title) &&
            (candidate.diagnostics === void 0 || Is.typedArray(candidate.diagnostics, Diagnostic.is)) &&
            (candidate.kind === void 0 || Is.string(candidate.kind)) &&
            (candidate.edit !== void 0 || candidate.command !== void 0) &&
            (candidate.command === void 0 || Command.is(candidate.command)) &&
            (candidate.edit === void 0 || WorkspaceEdit.is(candidate.edit));
    }
    CodeAction.is = is;
})(CodeAction || (CodeAction = {}));
/**
 * The CodeLens namespace provides helper functions to work with
 * [CodeLens](#CodeLens) literals.
 */
var CodeLens;
(function (CodeLens) {
    /**
     * Creates a new CodeLens literal.
     */
    function create(range, data) {
        var result = { range: range };
        if (Is.defined(data))
            result.data = data;
        return result;
    }
    CodeLens.create = create;
    /**
     * Checks whether the given literal conforms to the [CodeLens](#CodeLens) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.range) && (Is.undefined(candidate.command) || Command.is(candidate.command));
    }
    CodeLens.is = is;
})(CodeLens || (CodeLens = {}));
/**
 * The FormattingOptions namespace provides helper functions to work with
 * [FormattingOptions](#FormattingOptions) literals.
 */
var FormattingOptions;
(function (FormattingOptions) {
    /**
     * Creates a new FormattingOptions literal.
     */
    function create(tabSize, insertSpaces) {
        return { tabSize: tabSize, insertSpaces: insertSpaces };
    }
    FormattingOptions.create = create;
    /**
     * Checks whether the given literal conforms to the [FormattingOptions](#FormattingOptions) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.number(candidate.tabSize) && Is.boolean(candidate.insertSpaces);
    }
    FormattingOptions.is = is;
})(FormattingOptions || (FormattingOptions = {}));
/**
 * A document link is a range in a text document that links to an internal or external resource, like another
 * text document or a web site.
 */
var DocumentLink = /** @class */ (function () {
    function DocumentLink() {
    }
    return DocumentLink;
}());

/**
 * The DocumentLink namespace provides helper functions to work with
 * [DocumentLink](#DocumentLink) literals.
 */
(function (DocumentLink) {
    /**
     * Creates a new DocumentLink literal.
     */
    function create(range, target, data) {
        return { range: range, target: target, data: data };
    }
    DocumentLink.create = create;
    /**
     * Checks whether the given literal conforms to the [DocumentLink](#DocumentLink) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.range) && (Is.undefined(candidate.target) || Is.string(candidate.target));
    }
    DocumentLink.is = is;
})(DocumentLink || (DocumentLink = {}));
var EOL = ['\n', '\r\n', '\r'];
var TextDocument;
(function (TextDocument) {
    /**
     * Creates a new ITextDocument literal from the given uri and content.
     * @param uri The document's uri.
     * @param languageId  The document's language Id.
     * @param content The document's content.
     */
    function create(uri, languageId, version, content) {
        return new FullTextDocument(uri, languageId, version, content);
    }
    TextDocument.create = create;
    /**
     * Checks whether the given literal conforms to the [ITextDocument](#ITextDocument) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && (Is.undefined(candidate.languageId) || Is.string(candidate.languageId)) && Is.number(candidate.lineCount)
            && Is.func(candidate.getText) && Is.func(candidate.positionAt) && Is.func(candidate.offsetAt) ? true : false;
    }
    TextDocument.is = is;
    function applyEdits(document, edits) {
        var text = document.getText();
        var sortedEdits = mergeSort(edits, function (a, b) {
            var diff = a.range.start.line - b.range.start.line;
            if (diff === 0) {
                return a.range.start.character - b.range.start.character;
            }
            return diff;
        });
        var lastModifiedOffset = text.length;
        for (var i = sortedEdits.length - 1; i >= 0; i--) {
            var e = sortedEdits[i];
            var startOffset = document.offsetAt(e.range.start);
            var endOffset = document.offsetAt(e.range.end);
            if (endOffset <= lastModifiedOffset) {
                text = text.substring(0, startOffset) + e.newText + text.substring(endOffset, text.length);
            }
            else {
                throw new Error('Overlapping edit');
            }
            lastModifiedOffset = startOffset;
        }
        return text;
    }
    TextDocument.applyEdits = applyEdits;
    function mergeSort(data, compare) {
        if (data.length <= 1) {
            // sorted
            return data;
        }
        var p = (data.length / 2) | 0;
        var left = data.slice(0, p);
        var right = data.slice(p);
        mergeSort(left, compare);
        mergeSort(right, compare);
        var leftIdx = 0;
        var rightIdx = 0;
        var i = 0;
        while (leftIdx < left.length && rightIdx < right.length) {
            var ret = compare(left[leftIdx], right[rightIdx]);
            if (ret <= 0) {
                // smaller_equal -> take left to preserve order
                data[i++] = left[leftIdx++];
            }
            else {
                // greater -> take right
                data[i++] = right[rightIdx++];
            }
        }
        while (leftIdx < left.length) {
            data[i++] = left[leftIdx++];
        }
        while (rightIdx < right.length) {
            data[i++] = right[rightIdx++];
        }
        return data;
    }
})(TextDocument || (TextDocument = {}));
/**
 * Represents reasons why a text document is saved.
 */
var TextDocumentSaveReason;
(function (TextDocumentSaveReason) {
    /**
     * Manually triggered, e.g. by the user pressing save, by starting debugging,
     * or by an API call.
     */
    TextDocumentSaveReason.Manual = 1;
    /**
     * Automatic after a delay.
     */
    TextDocumentSaveReason.AfterDelay = 2;
    /**
     * When the editor lost focus.
     */
    TextDocumentSaveReason.FocusOut = 3;
})(TextDocumentSaveReason || (TextDocumentSaveReason = {}));
var FullTextDocument = /** @class */ (function () {
    function FullTextDocument(uri, languageId, version, content) {
        this._uri = uri;
        this._languageId = languageId;
        this._version = version;
        this._content = content;
        this._lineOffsets = null;
    }
    Object.defineProperty(FullTextDocument.prototype, "uri", {
        get: function () {
            return this._uri;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(FullTextDocument.prototype, "languageId", {
        get: function () {
            return this._languageId;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(FullTextDocument.prototype, "version", {
        get: function () {
            return this._version;
        },
        enumerable: true,
        configurable: true
    });
    FullTextDocument.prototype.getText = function (range) {
        if (range) {
            var start = this.offsetAt(range.start);
            var end = this.offsetAt(range.end);
            return this._content.substring(start, end);
        }
        return this._content;
    };
    FullTextDocument.prototype.update = function (event, version) {
        this._content = event.text;
        this._version = version;
        this._lineOffsets = null;
    };
    FullTextDocument.prototype.getLineOffsets = function () {
        if (this._lineOffsets === null) {
            var lineOffsets = [];
            var text = this._content;
            var isLineStart = true;
            for (var i = 0; i < text.length; i++) {
                if (isLineStart) {
                    lineOffsets.push(i);
                    isLineStart = false;
                }
                var ch = text.charAt(i);
                isLineStart = (ch === '\r' || ch === '\n');
                if (ch === '\r' && i + 1 < text.length && text.charAt(i + 1) === '\n') {
                    i++;
                }
            }
            if (isLineStart && text.length > 0) {
                lineOffsets.push(text.length);
            }
            this._lineOffsets = lineOffsets;
        }
        return this._lineOffsets;
    };
    FullTextDocument.prototype.positionAt = function (offset) {
        offset = Math.max(Math.min(offset, this._content.length), 0);
        var lineOffsets = this.getLineOffsets();
        var low = 0, high = lineOffsets.length;
        if (high === 0) {
            return Position.create(0, offset);
        }
        while (low < high) {
            var mid = Math.floor((low + high) / 2);
            if (lineOffsets[mid] > offset) {
                high = mid;
            }
            else {
                low = mid + 1;
            }
        }
        // low is the least x for which the line offset is larger than the current offset
        // or array.length if no line offset is larger than the current offset
        var line = low - 1;
        return Position.create(line, offset - lineOffsets[line]);
    };
    FullTextDocument.prototype.offsetAt = function (position) {
        var lineOffsets = this.getLineOffsets();
        if (position.line >= lineOffsets.length) {
            return this._content.length;
        }
        else if (position.line < 0) {
            return 0;
        }
        var lineOffset = lineOffsets[position.line];
        var nextLineOffset = (position.line + 1 < lineOffsets.length) ? lineOffsets[position.line + 1] : this._content.length;
        return Math.max(Math.min(lineOffset + position.character, nextLineOffset), lineOffset);
    };
    Object.defineProperty(FullTextDocument.prototype, "lineCount", {
        get: function () {
            return this.getLineOffsets().length;
        },
        enumerable: true,
        configurable: true
    });
    return FullTextDocument;
}());
var Is;
(function (Is) {
    var toString = Object.prototype.toString;
    function defined(value) {
        return typeof value !== 'undefined';
    }
    Is.defined = defined;
    function undefined(value) {
        return typeof value === 'undefined';
    }
    Is.undefined = undefined;
    function boolean(value) {
        return value === true || value === false;
    }
    Is.boolean = boolean;
    function string(value) {
        return toString.call(value) === '[object String]';
    }
    Is.string = string;
    function number(value) {
        return toString.call(value) === '[object Number]';
    }
    Is.number = number;
    function func(value) {
        return toString.call(value) === '[object Function]';
    }
    Is.func = func;
    function objectLiteral(value) {
        // Strictly speaking class instances pass this check as well. Since the LSP
        // doesn't use classes we ignore this for now. If we do we need to add something
        // like this: `Object.getPrototypeOf(Object.getPrototypeOf(x)) === null`
        return value !== null && typeof value === 'object';
    }
    Is.objectLiteral = objectLiteral;
    function typedArray(value, check) {
        return Array.isArray(value) && value.every(check);
    }
    Is.typedArray = typedArray;
})(Is || (Is = {}));


/***/ }),

/***/ 496:
/***/ ((module) => {

"use strict";
module.exports = require("vscode");

/***/ }),

/***/ 81:
/***/ ((module) => {

"use strict";
module.exports = require("child_process");

/***/ }),

/***/ 113:
/***/ ((module) => {

"use strict";
module.exports = require("crypto");

/***/ }),

/***/ 147:
/***/ ((module) => {

"use strict";
module.exports = require("fs");

/***/ }),

/***/ 808:
/***/ ((module) => {

"use strict";
module.exports = require("net");

/***/ }),

/***/ 37:
/***/ ((module) => {

"use strict";
module.exports = require("os");

/***/ }),

/***/ 17:
/***/ ((module) => {

"use strict";
module.exports = require("path");

/***/ })

/******/ 	});
/************************************************************************/
/******/ 	// The module cache
/******/ 	var __webpack_module_cache__ = {};
/******/ 	
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/ 		// Check if module is in cache
/******/ 		var cachedModule = __webpack_module_cache__[moduleId];
/******/ 		if (cachedModule !== undefined) {
/******/ 			return cachedModule.exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = __webpack_module_cache__[moduleId] = {
/******/ 			// no module.id needed
/******/ 			// no module.loaded needed
/******/ 			exports: {}
/******/ 		};
/******/ 	
/******/ 		// Execute the module function
/******/ 		__webpack_modules__[moduleId](module, module.exports, __webpack_require__);
/******/ 	
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/ 	
/************************************************************************/
/******/ 	/* webpack/runtime/compat get default export */
/******/ 	(() => {
/******/ 		// getDefaultExport function for compatibility with non-harmony modules
/******/ 		__webpack_require__.n = (module) => {
/******/ 			var getter = module && module.__esModule ?
/******/ 				() => (module['default']) :
/******/ 				() => (module);
/******/ 			__webpack_require__.d(getter, { a: getter });
/******/ 			return getter;
/******/ 		};
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/define property getters */
/******/ 	(() => {
/******/ 		// define getter functions for harmony exports
/******/ 		__webpack_require__.d = (exports, definition) => {
/******/ 			for(var key in definition) {
/******/ 				if(__webpack_require__.o(definition, key) && !__webpack_require__.o(exports, key)) {
/******/ 					Object.defineProperty(exports, key, { enumerable: true, get: definition[key] });
/******/ 				}
/******/ 			}
/******/ 		};
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/hasOwnProperty shorthand */
/******/ 	(() => {
/******/ 		__webpack_require__.o = (obj, prop) => (Object.prototype.hasOwnProperty.call(obj, prop))
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/make namespace object */
/******/ 	(() => {
/******/ 		// define __esModule on exports
/******/ 		__webpack_require__.r = (exports) => {
/******/ 			if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 				Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 			}
/******/ 			Object.defineProperty(exports, '__esModule', { value: true });
/******/ 		};
/******/ 	})();
/******/ 	
/************************************************************************/
var __webpack_exports__ = {};
// This entry need to be wrapped in an IIFE because it need to be in strict mode.
(() => {
"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "activate": () => (/* binding */ activate),
/* harmony export */   "deactivate": () => (/* binding */ deactivate)
/* harmony export */ });
/* harmony import */ var path__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(17);
/* harmony import */ var path__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(path__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var vscode__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(496);
/* harmony import */ var vscode__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(vscode__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var vscode_languageclient__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(56);
/* harmony import */ var vscode_languageclient__WEBPACK_IMPORTED_MODULE_2___default = /*#__PURE__*/__webpack_require__.n(vscode_languageclient__WEBPACK_IMPORTED_MODULE_2__);
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */



let client;
function activate(context) {
    vscode__WEBPACK_IMPORTED_MODULE_1__.commands.registerCommand('m3Server.startStreaming', () => {
    });
    // The server is implemented in m3
    const cm3exec = context.asAbsolutePath(path__WEBPACK_IMPORTED_MODULE_0__.join('bin', 'm3_lang_server'));
    const serverOptions = {
        command: cm3exec,
    };
    // Options to control the language client
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'm3' }],
    };
    // Create the language client and start the client.
    client = new vscode_languageclient__WEBPACK_IMPORTED_MODULE_2__.LanguageClient('m3Server', 'M3 Language Server', serverOptions, clientOptions);
    // Start the client. This will also launch the server
    client.start();
}
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

})();

module.exports = __webpack_exports__;
/******/ })()
;
//# sourceMappingURL=main.js.map