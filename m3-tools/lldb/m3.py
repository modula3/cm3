"""lldb support for Modula-3 programs.

Load it with

    (lldb) command script import <cm3>/m3-tools/lldb/m3.py

It asks the stopped program's own runtime (RTDebug.Format, in m3core) to
describe traced references, so it works for programs built with either the
C or the LLVM (MSIR) backend.  It adds:

    m3 <expression>    describe the traced reference the expression yields:
                       a TEXT's contents, an open array's type and shape,
                       or an object's dynamic type

and summaries for values of type TEXT and REFANY, so `frame variable` shows
a TEXT's contents and what a REFANY refers to.  The runtime calls need a live process; for a core file only the
DWARF view is available.
"""

import lldb


def _call(target, frame, fn, addr):
    """The C string the runtime's fn(addr) returns, or None."""
    opts = lldb.SBExpressionOptions()
    opts.SetLanguage(lldb.eLanguageTypeC)
    opts.SetUnwindOnError(True)
    opts.SetIgnoreBreakpoints(True)
    opts.SetTryAllThreads(False)
    opts.SetTimeoutInMicroSeconds(2000000)
    # Through an explicit function type: without a prototype from debug
    # info lldb would take the result as an int and truncate it.
    expr = "((char *(*)(char *))%s)((char *)0x%x)" % (fn, addr)
    v = frame.EvaluateExpression(expr, opts) if frame and frame.IsValid() \
        else target.EvaluateExpression(expr, opts)
    if not v.IsValid() or v.GetError().Fail():
        return None
    p = v.GetValueAsUnsigned(0)
    if p == 0:
        return None
    err = lldb.SBError()
    s = target.GetProcess().ReadCStringFromMemory(p, 4096, err)
    return s if err.Success() else None


def _dynamic_type(target, name):
    """The DWARF structure for a referent whose typecell is called name.

    MSIR's DWARF names types as their typecells do ("Main.T"), so the match
    is exact.  None if the program's DWARF has no such structure (a type no
    variable is declared with), or for a REF type, whose referent is
    reached through the static type instead."""
    types = target.FindTypes(name)
    for i in range(types.GetSize()):
        t = types.GetTypeAtIndex(i)
        if t.GetName() != name:
            continue
        t = t.GetCanonicalType()
        if t.GetTypeClass() in (lldb.eTypeClassStruct, lldb.eTypeClassClass):
            return t
    return None


def _format(target, frame, addr):
    """RTDebug.Format(addr) in the target, or None."""
    if addr == 0:
        return "NIL"
    return _call(target, frame, "RTDebug__Format", addr)


def m3_command(debugger, command, result, internal_dict):
    target = debugger.GetSelectedTarget()
    frame = target.GetProcess().GetSelectedThread().GetSelectedFrame()
    v = frame.EvaluateExpression(command) if frame.IsValid() \
        else target.EvaluateExpression(command)
    if not v.IsValid() or v.GetError().Fail():
        # A Modula-3 variable name lldb's C parser rejects: try the frame.
        v = frame.FindVariable(command.strip()) if frame.IsValid() else None
    if v is None or not v.IsValid():
        result.SetError("cannot evaluate: %s" % command)
        return
    addr = v.GetValueAsUnsigned(0)
    s = _format(target, frame, addr)
    if s is None:
        result.SetError("0x%x is not a traced reference" % addr)
        return
    result.AppendMessage(s)
    # An object's fields, under its dynamic type.
    name = _call(target, frame, "RTDebug__TypeName", addr) if addr else None
    t = _dynamic_type(target, name) if name else None
    if t is not None:
        obj = target.CreateValueFromAddress("*", lldb.SBAddress(addr, target), t)
        for i in range(obj.GetNumChildren()):
            c = obj.GetChildAtIndex(i)
            if c.GetName() != "__vtable":
                result.AppendMessage("  %s = %s" % (
                    c.GetName(), c.GetSummary() or c.GetValue() or ""))


def ref_summary(valobj, internal_dict):
    addr = valobj.GetValueAsUnsigned(0)
    target = valobj.GetTarget()
    frame = valobj.GetFrame()
    s = _format(target, frame, addr)
    return s if s is not None else "0x%x" % addr


def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand(
        "command script add -o -f %s.m3_command m3" % __name__)
    for t in ("TEXT", "REFANY"):
        debugger.HandleCommand(
            "type summary add -w modula3 -F %s.ref_summary %s" % (__name__, t))
    debugger.HandleCommand("type category enable modula3")
