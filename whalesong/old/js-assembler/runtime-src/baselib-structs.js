/*jslint browser: true, unparam: true, vars: true, white: true, nomen: true, plusplus: true, maxerr: 50, indent: 4 */
/*globals $*/
(function (baselib, $) {
    "use strict";
    var exports = {};
    baselib.structs = exports;



    //////////////////////////////////////////////////////////////////////

    var Struct = function (constructorName, fields) {
        this._constructorName = constructorName; 
        this._fields = [];
    };

    Struct.prototype.toWrittenString = function (cache) { 
        var buffer = [], i;
        cache.put(this, true);
        buffer.push("(");
        buffer.push(this._constructorName);
        for(i = 0; i < this._fields.length; i++) {
            buffer.push(" ");
            buffer.push(baselib.format.toWrittenString(this._fields[i], cache));
        }
        buffer.push(")");
        return buffer.join("");
    };

    Struct.prototype.toDisplayedString = function (cache) {
        return baselib.format.toWrittenString(this, cache); 
    };

    Struct.prototype.toDomNode = function (params) {
        var node = document.createElement("span"), i;
        params.put(this, true);
        $(node).append(document.createTextNode("("));
        $(node).append(document.createTextNode(this._constructorName));
        for(i = 0; i < this._fields.length; i++) {
            $(node).append(document.createTextNode(" "));
            $(node).append(baselib.format.toDomNode(this._fields[i], params));
        }
        $(node).append(document.createTextNode(")"));
        return node;
    };


    Struct.prototype.equals = function (other, aUnionFind) {
        var i;
        if (!(other instanceof this.type)) {
            return false;
        }
        for (i = 0; i < this._fields.length; i++) {
            if (! baselib.equality.equals(this._fields[i],
                         other._fields[i],
                         aUnionFind)) {
                return false;
            }
        }
        return true;
    };

    Struct.prototype.type = Struct;


    //////////////////////////////////////////////////////////////////////


    var StructType = function (name,             // string
                               type,             // StructType
                               numberOfArgs,     // number
                               numberOfFields,   // number
                               firstField,
                               applyGuard,
                               constructor,
                               predicate, 
                               accessor,
                               mutator) {
        this.name = name;
        this.type = type;
        this.numberOfArgs = numberOfArgs;
        this.numberOfFields = numberOfFields;
        this.firstField = firstField;

        this.applyGuard = applyGuard;
        this.constructor = constructor;
        this.predicate = predicate;
        this.accessor = accessor;
        this.mutator = mutator;
    };


    StructType.prototype.toString = function (cache) {
        return '#<struct-type:' + this.name + '>';
    };


    StructType.prototype.equals = function (other, aUnionFind) {
        return this === other;
    };





    // guard-function: array string (array -> value)





    // Default structure guard just calls the continuation argument.
    var DEFAULT_GUARD = function (args, name, k) { 
        return k(args); 
    };


    // The default parent type refers to the toplevel Struct.
    var DEFAULT_PARENT_TYPE = { type: Struct,
                                numberOfArgs: 0,
                                numberOfFields: 0,
                                firstField: 0,
                                applyGuard: DEFAULT_GUARD };



    // makeStructureType: string StructType number number boolean
    //                    guard-function -> StructType
    //
    // Creates a new structure type.

    var makeStructureType = function (theName,
                                      parentType, 
                                      initFieldCnt, 
                                      autoFieldCnt, 
                                      autoV, 
                                      guard) {


        // Defaults
        autoFieldCnt = autoFieldCnt || 0;
        parentType = parentType || DEFAULT_PARENT_TYPE;
        guard = guard || DEFAULT_GUARD;



        // RawConstructor creates a new struct type inheriting from
        // the parent, with no guard checks.
        var RawConstructor = function (name, args) {
            var i;
            parentType.type.call(this, name, args);
            for (i = 0; i < initFieldCnt; i++) {
                this._fields.push(args[i+parentType.numberOfArgs]);
            }
            for (i = 0; i < autoFieldCnt; i++) {
                this._fields.push(autoV);
            }
        };
        RawConstructor.prototype = baselib.heir(parentType.type.prototype);



        // Set type, necessary for equality checking
        RawConstructor.prototype.type = RawConstructor;

        // The structure type consists of the name, its constructor, a
        // record of how many argument it and its parent type contains,
        // the list of autofields, the guard, and functions corresponding
        // to the constructor, the predicate, the accessor, and mutators.
        var newType = new StructType(
            theName,
            RawConstructor,
            initFieldCnt + parentType.numberOfArgs,
            initFieldCnt + autoFieldCnt,
            parentType.firstField + parentType.numberOfFields,
            function (args, name, k) {
                return guard(args, name,
                             function (result) {
                                 var parentArgs = result.slice(0, parentType.numberOfArgs);
                                 var restArgs = result.slice(parentType.numberOfArgs);
                                 return parentType.applyGuard(
                                     parentArgs, name,
                                     function (parentRes) {
                                         return k( parentRes.concat(restArgs) ); });
                             });
            },
            // constructor
            function () {
                var args = [].slice.call(arguments);
                return newType.applyGuard(
                    args,
                    baselib.symbols.Symbol.makeInstance(theName),
                    function (res) { 
                        return new RawConstructor(theName, res); });
            },

            // predicate
            function (x) { 
                return x instanceof RawConstructor; 
            },

            // accessor
            function (x, i) { return x._fields[i + this.firstField]; },

            // mutator
            function (x, i, v) { x._fields[i + this.firstField] = v; });
        return newType;
    };








    var isStruct = function (x) { return x instanceof Struct; };
    var isStructType = function (x) { return x instanceof StructType; };


    //////////////////////////////////////////////////////////////////////


    exports.StructType = StructType;
    exports.Struct = Struct;
    exports.makeStructureType = makeStructureType;
    exports.isStruct = isStruct;
    exports.isStructType = isStructType;

}(this.plt.baselib, $));