/*jslint unparam: true, sub: true, vars: true, white: true, plusplus: true, maxerr: 50, indent: 4 */

// Procedures

// For historical reasons, this module is called 'functions' instead of 'procedures'.
// This may change soon.

/*global plt*/

(function (baselib, plt) {
    'use strict';
    var exports = {};
    baselib.functions = exports;
    
    // Procedure types: a procedure is either a Primitive or a Closure.

    // A Primitive is a function that's expected to return.  It is not
    // allowed to call into Closures.  Its caller is expected to pop off
    // its argument stack space.
    //



    var isPrimitiveProcedure = function (x) {
        return typeof (x) === 'function';
    };







    // A Closure is a function that takes on more responsibilities: it is
    // responsible for popping off stack space before it finishes, and it
    // is also explicitly responsible for continuing the computation by 
    // popping off the control stack and doing the jump.  Because of this,
    // closures can do pretty much anything to the machine.

    // A closure consists of its free variables as well as a label
    // into its text segment.
    var Closure = function (label, arity, closedVals, displayName) {
        this.label = label;              // (MACHINE -> void)
        this.racketArity = arity;              // number
        this.closedVals = closedVals;    // arrayof number
        this.displayName = displayName;  // string
    };


    // Finalize the return from a closure.  This is a helper function
    // for those who implement Closures by hand.
    //
    // If used in the body of a Closure, it must be in tail
    // position.  This finishes the closure call, and does the following:
    //
    //     * Clears out the existing arguments off the stack frame
    //     * Sets up the return value
    //     * Jumps either to the single-value return point, or the multiple-value
    //       return point.
    //
    // I'd personally love for this to be a macro and avoid the
    // extra function call here.
    var finalizeClosureCall = function (MACHINE) {
        MACHINE.cbt--;
        var returnArgs = [].slice.call(arguments, 1);

        // clear out stack space
        MACHINE.e.length -= MACHINE.a;

        if (returnArgs.length === 1) {
            MACHINE.v = returnArgs[0];
            return MACHINE.c.pop().label(MACHINE);
        } else if (returnArgs.length === 0) {
            MACHINE.a = 0;
            return (MACHINE.c.pop().label.mvr || plt.runtime.si_context_expected_1)(MACHINE);
        } else {
            MACHINE.a = returnArgs.length;
            MACHINE.v = returnArgs.shift();
            MACHINE.e.push.apply(MACHINE.e, returnArgs.reverse());
            return (MACHINE.c.pop().label.mvr || plt.runtime.si_context_expected_1)(MACHINE);
        }
    };


    var isClosure = function (x) {
        return x instanceof Closure;
    };


    var isProcedure = function (x) {
        return (typeof (x) === 'function' ||
                x instanceof Closure);
    };








  
    var coersePrimitiveToJavaScript = function (v, MACHINE) {
        return function (succ, fail) {
            try {
                succ = succ || function () {};
                fail = fail || function () {};

                var oldArgcount = MACHINE.a, i;
                MACHINE.a = arguments.length - 2;
                for (i = 0; i < arguments.length - 2; i++) {
                    MACHINE.e.push(arguments[arguments.length - 1 - i]);
                }

                if (!(baselib.arity.isArityMatching(v.racketArity, MACHINE.a))) {
                    var msg = baselib.format.format("arity mismatch: ~s expected ~s arguments, but received ~s",
                                                    [v.displayName, v.racketArity, MACHINE.a]);
                    return fail(new baselib.exceptions.RacketError(
                        msg,
                        baselib.exceptions.makeExnFailContractArity(msg,
                                                                    MACHINE.captureContinuationMarks())));
                }

                var result = v(MACHINE);
                MACHINE.a = oldArgcount;
                for (i = 0; i < arguments.length - 2; i++) { 
                    MACHINE.e.pop();
                }
                succ(result);
            } catch (e) {
                fail(e);
            }
        };
    };

    var coerseClosureToJavaScript = function (v, MACHINE) {
        var f = function (succ, fail) {
            succ = succ || function () {};
            fail = fail || function () {};

            if (!(baselib.arity.isArityMatching(v.racketArity, arguments.length - 2))) {
                var msg = baselib.format.format(
                    "arity mismatch: ~s expected ~s argument(s) but received ~s",
                    [v.displayName, v.racketArity, arguments.length - 2]);
                return fail(new baselib.exceptions.RacketError(
                    msg,
                    baselib.exceptions.makeExnFailContractArity(msg,
                                                                MACHINE.captureContinuationMarks())));
            }

            var oldVal = MACHINE.v;
            var oldArgcount = MACHINE.a;
            var oldProc = MACHINE.p;

            var oldErrorHandler = MACHINE.params['currentErrorHandler'];
            var afterGoodInvoke = function (MACHINE) { 
                plt.runtime.PAUSE(
                    function (restart) {
                        MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                        var returnValue = MACHINE.v;
                        MACHINE.v = oldVal;
                        MACHINE.a = oldArgcount;
                        MACHINE.p = oldProc;
                        succ(returnValue);
                    });
            };
            afterGoodInvoke.mvr = function (MACHINE) {
                plt.runtime.PAUSE(
                    function (restart) {
                        MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                        var returnValues = [MACHINE.v], i;
                        for (i = 0; i < MACHINE.a - 1; i++) {
                            returnValues.push(MACHINE.e.pop());
                        }
                        MACHINE.v = oldVal;
                        MACHINE.a = oldArgcount;
                        MACHINE.p = oldProc;
                        succ.apply(null, returnValues);
                    });
            };

            MACHINE.c.push(
                new baselib.frames.CallFrame(afterGoodInvoke, v));
            MACHINE.a = arguments.length - 2;
            var i;
            for (i = 0; i < arguments.length - 2; i++) {
                MACHINE.e.push(arguments[arguments.length - 1 - i]);
            }
            MACHINE.p = v;
            MACHINE.params['currentErrorHandler'] = function (MACHINE, e) {
                MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                MACHINE.v = oldVal;
                MACHINE.a = oldArgcount;
                MACHINE.p = oldProc;
                fail(e);
            };
            MACHINE.trampoline(v.label);
        };
        return f;
    };

    // coerseToJavaScript: racket function -> JavaScript function
    // Given a closure or primitive, produces an
    // asynchronous JavaScript function.
    // The function will run on the provided MACHINE.
    //
    // It assumes that it must begin its own trampoline.
    var asJavaScriptFunction = function (v, MACHINE) {
        MACHINE = MACHINE || plt.runtime.currentMachine;
        if (isPrimitiveProcedure(v)) {
            return coersePrimitiveToJavaScript(v, MACHINE);
        } else if (isClosure(v)) {
            return coerseClosureToJavaScript(v, MACHINE);
        } else {
            baselib.exceptions.raise(MACHINE,
                                     baselib.exceptions.makeExnFailContract(
                                         baselib.format.format(
                                             "Not a procedure: ~e",
                                             v),
                                         MACHINE.captureContinuationMarks()));
        }
    };


    // internallCallDuringPause: call a Racket procedure and get its results.
    // The use assumes the machine is in a running-but-paused state.
    var internalCallDuringPause = function (MACHINE, proc, success, fail) {
        var i;
        var oldArgcount, oldVal, oldProc, oldErrorHandler;
        if (! baselib.arity.isArityMatching(proc.racketArity, arguments.length - 4)) {
            var msg = baselib.format.format("arity mismatch: ~s expected ~s arguments, but received ~s",
                                            [proc.displayName, proc.racketArity, arguments.length - 4]);
            return fail(baselib.exceptions.makeExnFailContractArity(msg,
                                                                    MACHINE.captureContinuationMarks()));
        }

        if (isPrimitiveProcedure(proc)) {
            oldArgcount = MACHINE.a;
            MACHINE.a = arguments.length - 4;
            for (i = 0; i < arguments.length - 4; i++) {
                MACHINE.e.push(arguments[arguments.length - 1 - i]);
            }
            var result = proc(MACHINE);
            for (i = 0; i < arguments.length - 4; i++) {
                MACHINE.e.pop();
            }
            success(result);
        } else if (isClosure(proc)) {
            oldVal = MACHINE.v;
            oldArgcount = MACHINE.a;
            oldProc = MACHINE.p;

            oldErrorHandler = MACHINE.params['currentErrorHandler'];
            var afterGoodInvoke = function (MACHINE) { 
                plt.runtime.PAUSE(function (restart) {
                    MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                    var returnValue = MACHINE.v;
                    MACHINE.v = oldVal;
                    MACHINE.a = oldArgcount;
                    MACHINE.p = oldProc;
                    success(returnValue);
                });
            };
            afterGoodInvoke.mvr = function (MACHINE) {
                plt.runtime.PAUSE(function (restart) {
                    MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                    var returnValues = [MACHINE.v];
                    var i;
                    for (i = 0; i < MACHINE.a - 1; i++) {
                        returnValues.push(MACHINE.e.pop());
                    }
                    MACHINE.v = oldVal;
                    MACHINE.a = oldArgcount;
                    MACHINE.p = oldProc;
                    success.apply(null, returnValues);
                });
            };

            MACHINE.c.push(
                new baselib.frames.CallFrame(afterGoodInvoke, proc));
            MACHINE.a = arguments.length - 4;
            for (i = 0; i < arguments.length - 4; i++) {
                MACHINE.e.push(arguments[arguments.length - 1 - i]);
            }
            MACHINE.p = proc;
            MACHINE.params['currentErrorHandler'] = function (MACHINE, e) {
                MACHINE.params['currentErrorHandler'] = oldErrorHandler;
                MACHINE.v = oldVal;
                MACHINE.a = oldArgcount;
                MACHINE.p = oldProc;
                fail(e);
            };
            MACHINE.trampoline(proc.label);
        } else {
            fail(baselib.exceptions.makeExnFail(
                baselib.format.format(
                    "Not a procedure: ~e",
                    proc),
                MACHINE.captureContinuationMarks()));
        }
    };








    var makeClosure = function (name, arity, f, closureArgs) {
        if (! closureArgs) { closureArgs = []; }
        return new Closure(f,
                           arity,
                           closureArgs,
                           name);
    };


    var makePrimitiveProcedure = function (name, arity, f) {
        // f.racketArity = arity;
        // f.displayName = name;
        // return f;
        return makeClosure(name,
                           arity,
                           function(M) {
                               --M.cbt;
                               M.v = f(M);
                               M.e.length -= M.a;
                               return M.c.pop().label(M);
                           },
                           []);
    };








    var renameProcedure = function (f, name) {
        if (isPrimitiveProcedure(f)) {
            return makePrimitiveProcedure(
                name,
                f.racketArity,
                function (MACHINE) {
                    return f(MACHINE);
                });
        } else {
            return makeClosure(name, f.racketArity, f.label, f.closedVals);
        }
    };





    //////////////////////////////////////////////////////////////////////
    exports.Closure = Closure;
    exports.internalCallDuringPause = internalCallDuringPause;
    exports.finalizeClosureCall = finalizeClosureCall;

    exports.makePrimitiveProcedure = makePrimitiveProcedure;
    exports.makeClosure = makeClosure;

    exports.isPrimitiveProcedure = isPrimitiveProcedure;
    exports.isClosure = isClosure;

    exports.isProcedure = isProcedure;


    exports.renameProcedure = renameProcedure;


    exports.asJavaScriptFunction = asJavaScriptFunction;

}(this.plt.baselib, this.plt));