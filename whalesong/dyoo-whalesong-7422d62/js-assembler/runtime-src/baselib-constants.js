/*jslint vars: true, maxerr: 50, indent: 4 */


// Other miscellaneous constants
(function (baselib) {
    'use strict';
    var exports = {};
    baselib.constants = exports;


    var VoidValue = function () {};
    VoidValue.prototype.toString = function () {
        return "#<void>";
    };

    var VOID_VALUE = new VoidValue();


    var EofValue = function () {};
    EofValue.prototype.toString = function () {
        return "#<eof>";
    };

    var EOF_VALUE = new EofValue();


    exports.VOID_VALUE = VOID_VALUE;
    exports.EOF_VALUE = EOF_VALUE;
}(this.plt.baselib));