// Arity structure
/*jslint unparam: true, sub: true, vars: true, maxerr: 50, indent: 4 */
/*globals $*/
(function (baselib, $) {
    'use strict';
    var exports = {};
    baselib.ports = exports;


    // Output Ports

    var OutputPort = function () {};
    var isOutputPort = baselib.makeClassPredicate(OutputPort);


    var StandardOutputPort = function () {
        OutputPort.call(this);
    };
    StandardOutputPort.prototype = baselib.heir(OutputPort.prototype);
    StandardOutputPort.prototype.writeDomNode = function (MACHINE, domNode) {
        MACHINE.params['currentDisplayer'](MACHINE, domNode);
        $(domNode).trigger({type : 'afterAttach'});
        $('*', domNode).trigger({type : 'afterAttach'});
    };

    var StandardErrorPort = function () {
        OutputPort.call(this);
    };
    StandardErrorPort.prototype = baselib.heir(OutputPort.prototype);
    StandardErrorPort.prototype.writeDomNode = function (MACHINE, domNode) {
        MACHINE.params['currentErrorDisplayer'](MACHINE, domNode);
        $(domNode).trigger({type : 'afterAttach'});
        $('*', domNode).trigger({type : 'afterAttach'});
    };





    var OutputStringPort = function () {
        this.buf = [];
    };
    OutputStringPort.prototype = baselib.heir(OutputPort.prototype);
    OutputStringPort.prototype.writeDomNode = function (MACHINE, v) {
        this.buf.push($(v).text());
    };
    OutputStringPort.prototype.getOutputString = function () {
        return this.buf.join('');
    };
    var isOutputStringPort = baselib.makeClassPredicate(OutputStringPort);




    exports.OutputPort = OutputPort;
    exports.isOutputPort = isOutputPort;
    exports.StandardOutputPort = StandardOutputPort;
    exports.StandardErrorPort = StandardErrorPort;
    exports.OutputStringPort = OutputStringPort;
    exports.isOutputStringPort = isOutputStringPort;


}(this.plt.baselib, $));