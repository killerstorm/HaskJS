"use strict";
function init() {
}
    
function run_coin_kernel_on_graph(kernel_name, transactions) {
    var arr = [];
    for (var i = 0; i < transactions.length; i++) {
        var json = {};
        json.txPayload     = get_payload(transactions[i]);
        json.txInputs      = get_inputs(transactions[i]);
        json.txOutputCount = transactions[i].outs.length;
        json.txID          = transactions[i].getId();        
        arr[i] = JSON.stringify(json);
    }
    return Haste[kernel_name](arr);
}

function get_mux_shape(kernel_name, payload) {
        //????????????????????   
}

function maybe_get_op_return(script) {
  if (script.chunks.length == 2 && script.chunks[0] == 106) {
    return script.chunks[1];
  } else { return null; }
}
 
function get_payload(transaction) {
  for (var i = 0; i < transaction.outs.length; i++) {
      var op_return = maybe_get_op_return(transaction.outs[i].script);
      if (op_return) return op_return.toString('hex');
  }
  return "";
}

function get_inputs(transaction) {
    var inputs = {};
    for (var i = 0; i < transaction.ins.length; i++) {
        var temp = {};
        temp.hashHex = transaction.ins[i].hash.toString('hex');
        temp.index   = transaction.ins[i].index;
        inputs[i] = temp;
    }
    return inputs;
}


exports.run_coin_kernel_on_graph = run_coin_kernel_on_graph;
exports.get_mux_shape            = get_mux_shape;
exports.get_payload              = get_payload;
exports.get_inputs               = get_inputs;

// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            if(t.x === __updatable) {
                t.x = f();
            } else {
                return f();
            }
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw E(err);
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

window['jsGetMouseCoords'] = function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsElemsByClassName(cls) {
    var es = document.getElementsByClassName(cls);
    var els = [0];

    for (var i = es.length-1; i >= 0; --i) {
        els = [1, [0, es[i]], els];
    }
    return els;
}

function jsQuerySelectorAll(elem, query) {
    var els = [0], nl;

    if (!elem || typeof elem.querySelectorAll !== 'function') {
        return els;
    }

    nl = elem.querySelectorAll(query);

    for (var i = nl.length-1; i >= 0; --i) {
        els = [1, [0, nl[i]], els];
    }

    return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, jsRead(obj)];
    case 'string':
        return [1, obj];
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);}),true]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}
window['arr2lst'] = arr2lst;

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}
window['lst2arr'] = lst2arr;

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
    var n = s.length,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=s.length; i+=64) {
        md5cycle(state, md5blk(s.substring(i-64, i)));
    }
    s = s.substring(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<s.length; i++)
        tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s.charCodeAt(i)
            + (s.charCodeAt(i+1) << 8)
            + (s.charCodeAt(i+2) << 16)
            + (s.charCodeAt(i+3) << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s) {
    return hex(md51(s));
}

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return [0, 1, E(w).val];
}

function finalizeWeak(w) {
    return [0, B(A(E(w).fin, [0]))];
}

var _0=0,_1=function(_2){var _3=B(A(_2,[_])),_4=_3;return E(_4);},_5=function(_6){var _7=function(_){var _=0;return new F(function(){return eval(_6);});};return new F(function(){return _1(_7);});},_8=new T(function(){return [0,"(function(s,f){Haste[s] = f;})"];}),_9=new T(function(){return B(_5((_8=E(_8))[1]));}),_a=function(_b,_c){var _d=function(_e,_){var _f=B(A(new T(function(){var _g=(_c=E(_c))[1];return B(A(_9,[_g=E(_g)]));}),[B(A(_b,[_e])),_])),_h=_f;return _0;};return E(_d);},_i=new T(function(){return [0,"getMuxShape"];}),_j=new T(function(){return B(_5("(function(f) {  return (function() {      return (function(){        var args=Array.prototype.slice.call(arguments,0);        args.push(0);        return E(B(A(f, args)));    });  });})"));}),_k=function(_l,_){return new F(function(){return A(_j,[_l=E(_l),_]);});},_m=function(_n,_){return new F(function(){return _k(_n,_);});},_o=function(_p){var _q=function(_){var _=0,_r=function(_s){var _t=function(_){var _=0,_u=String(_s),_v=_u,_w=B(A(_p,[[0,_v],_])),_x=_w,_y=(_x=E(_x))[1];return _y=E(_y);};return new F(function(){return _1(_t);});};return new F(function(){return _m(_r,_);});};return new F(function(){return _1(_q);});},_z=new T(function(){return B(_a(_o,_i));}),_A=[1],_B=[0],_C=function(_D,_E){return ((_D=E(_D))[0]==0)?E(_E):[1,_D[1],new T(function(){return B(_C(_D[2],_E));})];},_F=function(_G){while(1){if(!(_G=E(_G))[0]){var _H=[1,I_fromInt(_G[1])];_G=_H;continue;}else{return new F(function(){return I_toString(_G[1]);});}}},_I=function(_J,_K){return new F(function(){return _C(fromJSStr(B(_F(_J))),_K);});},_L=function(_M,_N){if(!(_M=E(_M))[0]){var _O=_M[1];return ((_N=E(_N))[0]==0)?_O<_N[1]:I_compareInt(_N[1],_O)>0;}else{var _P=_M[1];return ((_N=E(_N))[0]==0)?I_compareInt(_P,_N[1])<0:I_compare(_P,_N[1])<0;}},_Q=[0,41],_R=[0,40],_S=[0,0],_T=function(_U,_V,_W){if(_U<=6){return new F(function(){return _I(_V,_W);});}else{if(!B(_L(_V,_S))){return new F(function(){return _I(_V,_W);});}else{return [1,_R,new T(function(){return B(_C(fromJSStr(B(_F(_V))),[1,_Q,_W]));})];}}},_X=function(_Y,_Z){var _10=jsShowI(_Y),_11=_10;return new F(function(){return _C(fromJSStr(_11),_Z);});},_12=function(_13,_14,_15){if(_14>=0){return new F(function(){return _X(_14,_15);});}else{if(_13<=6){return new F(function(){return _X(_14,_15);});}else{return [1,_R,new T(function(){var _16=jsShowI(_14),_17=_16;return B(_C(fromJSStr(_17),[1,_Q,_15]));})];}}},_18=new T(function(){return B(unCStr("null"));}),_19=[0,125],_1a=[1,_19,_B],_1b=new T(function(){return B(_C(_18,_1a));}),_1c=new T(function(){return B(unCStr("invalid"));}),_1d=new T(function(){return B(_C(_1c,_1a));}),_1e=new T(function(){return B(unCStr("missing"));}),_1f=new T(function(){return B(_C(_1e,_1a));}),_1g=[0,44],_1h=[0,34],_1i=[0,58],_1j=[0,123],_1k=function(_1l,_1m){while(1){var _1n=(function(_1o,_1p){if(!(_1p=E(_1p))[0]){var _1q=_1p[2],_1r=_1p[3],_1s=_1q[2];_1l=[1,new T(function(){return [0,toJSStr([1,_1j,new T(function(){return B(unAppCStr("\"hashHex\"",[1,_1i,[1,_1h,new T(function(){return B(_C((_1q=E(_1q))[1],[1,_1h,[1,_1g,new T(function(){return B(unAppCStr("\"index\"",[1,_1i,new T(function(){return B(_C(B(_12(0,(_1s=E(_1s))[1],_B)),[1,_1g,new T(function(){return B(unAppCStr("\"coinState\"",[1,_1i,new T(function(){switch((_1r=E(_1r))[0]){case 0:return B(_C(B(_T(0,_1r[1],_B)),_1a));break;case 1:return E(_1f);break;case 2:return E(_1d);break;default:return E(_1b);}})]));})]));})]));})]]));})]]));})])];}),new T(function(){return B(_1k(_1o,_1p[5]));})];var _1t=_1p[4];_1m=_1t;return null;}else{return E(_1o);}})(_1l,_1m);if(_1n!=null){return _1n;}}},_1u=function(_1v){return new F(function(){return err(B(unAppCStr("Haste.JSON.!: unable to look up key ",new T(function(){return fromJSStr((_1v=E(_1v))[1]);}))));});},_1w=function(_1x,_1y){var _1z=strEq((_1x=E(_1x))[1],(_1y=E(_1y))[1]),_1A=_1z;return ((_1A=E(_1A))==0)?true:false;},_1B=function(_1C,_1D){var _1E=strEq((_1C=E(_1C))[1],(_1D=E(_1D))[1]),_1F=_1E;return ((_1F=E(_1F))==0)?false:true;},_1G=[0,_1B,_1w],_1H=function(_1I){return E((_1I=E(_1I))[1]);},_1J=function(_1K,_1L,_1M){while(1){if(!(_1M=E(_1M))[0]){return [0];}else{var _1N=_1M[1];if(!B(A(_1H,[_1K,_1L,(_1N=E(_1N))[1]]))){var _1O=_1M[2];_1M=_1O;continue;}else{return [1,_1N[2]];}}}},_1P=function(_1Q,_1R){if((_1Q=E(_1Q))[0]==4){var _1S=B(_1J(_1G,_1R,_1Q[1]));if(!_1S[0]){return new F(function(){return _1u(_1R);});}else{return E(_1S[1]);}}else{return new F(function(){return _1u(_1R);});}},_1T=new T(function(){return [0,"hashHex"];}),_1U=new T(function(){return [0,"index"];}),_1V=[0,93],_1W=[1,_1V,_B],_1X=new T(function(){return [0,toJSStr(_1W)];}),_1Y=[0,125],_1Z=[1,_1Y,_B],_20=new T(function(){return [0,toJSStr(_1Z)];}),_21=[0,58],_22=[1,_21,_B],_23=new T(function(){return [0,toJSStr(_22)];}),_24=[0,44],_25=[1,_24,_B],_26=new T(function(){return [0,toJSStr(_25)];}),_27=new T(function(){return [0,"false"];}),_28=function(_29){var _2a=jsStringify((_29=E(_29))[1]),_2b=_2a;return [0,_2b];},_2c=new T(function(){return [0,"null"];}),_2d=[0,91],_2e=[1,_2d,_B],_2f=new T(function(){return [0,toJSStr(_2e)];}),_2g=[0,123],_2h=[1,_2g,_B],_2i=new T(function(){return [0,toJSStr(_2h)];}),_2j=[0,34],_2k=[1,_2j,_B],_2l=new T(function(){return [0,toJSStr(_2k)];}),_2m=new T(function(){return [0,"true"];}),_2n=function(_2o,_2p){switch((_2p=E(_2p))[0]){case 0:return [0,new T(function(){var _2q=jsShow(_2p[1]),_2r=_2q;return [0,_2r];}),_2o];case 1:return [0,new T(function(){var _2s=jsStringify(_2p[1]),_2t=_2s;return [0,_2t];}),_2o];case 2:var _2u=_2p[1];return (!(_2u=E(_2u)))?[0,_27,_2o]:[0,_2m,_2o];case 3:var _2v=_2p[1];return ((_2v=E(_2v))[0]==0)?[0,_2f,[1,_1X,_2o]]:[0,_2f,new T(function(){var _2w=B(_2n(new T(function(){var _2x=function(_2y){return ((_2y=E(_2y))[0]==0)?E([1,_1X,_2o]):[1,_26,new T(function(){var _2z=B(_2n(new T(function(){return B(_2x(_2y[2]));}),_2y[1]));return [1,_2z[1],_2z[2]];})];};return B(_2x(_2v[2]));}),_2v[1]));return [1,_2w[1],_2w[2]];})];case 4:var _2A=_2p[1];if(!(_2A=E(_2A))[0]){return [0,_2i,[1,_20,_2o]];}else{var _2B=_2A[1];return [0,_2i,[1,new T(function(){return B(_28((_2B=E(_2B))[1]));}),[1,_23,new T(function(){var _2C=B(_2n(new T(function(){var _2D=function(_2E){if(!(_2E=E(_2E))[0]){return E([1,_20,_2o]);}else{var _2F=_2E[1];return [1,_26,[1,_2l,[1,(_2F=E(_2F))[1],[1,_2l,[1,_23,new T(function(){var _2G=B(_2n(new T(function(){return B(_2D(_2E[2]));}),_2F[2]));return [1,_2G[1],_2G[2]];})]]]]];}};return B(_2D(_2A[2]));}),_2B[2]));return [1,_2C[1],_2C[2]];})]]];}break;default:return [0,_2c,_2o];}},_2H=new T(function(){return [0,toJSStr(_B)];}),_2I=function(_2J){var _2K=jsCat(new T(function(){var _2L=B(_2n(_B,_2J));return [1,_2L[1],_2L[2]];}),(_2H=E(_2H))[1]),_2M=_2K;return E(_2M);},_2N=function(_2O,_2P){return new F(function(){return fromJSStr(B(_2I(B(_1P(_2O,_2P)))));});},_2Q=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_2R=new T(function(){return B(err(_2Q));}),_2S=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_2T=new T(function(){return B(err(_2S));}),_2U=new T(function(){return B(unCStr("Control.Exception.Base"));}),_2V=new T(function(){return B(unCStr("base"));}),_2W=new T(function(){return B(unCStr("PatternMatchFail"));}),_2X=new T(function(){var _2Y=hs_wordToWord64(18445595),_2Z=_2Y,_30=hs_wordToWord64(52003073),_31=_30;return [0,_2Z,_31,[0,_2Z,_31,_2V,_2U,_2W],_B];}),_32=function(_33){return E(_2X);},_34=function(_35){return E((_35=E(_35))[1]);},_36=function(_37,_38,_39){var _3a=B(A(_37,[_])),_3b=B(A(_38,[_])),_3c=hs_eqWord64(_3a[1],_3b[1]),_3d=_3c;if(!(_3d=E(_3d))){return [0];}else{var _3e=hs_eqWord64(_3a[2],_3b[2]),_3f=_3e;return ((_3f=E(_3f))==0)?[0]:[1,_39];}},_3g=function(_3h){return new F(function(){return _36(B(_34((_3h=E(_3h))[1])),_32,_3h[2]);});},_3i=function(_3j){return E((_3j=E(_3j))[1]);},_3k=function(_3l,_3m){return new F(function(){return _C((_3l=E(_3l))[1],_3m);});},_3n=[0,44],_3o=[0,93],_3p=[0,91],_3q=function(_3r,_3s,_3t){if(!(_3s=E(_3s))[0]){return new F(function(){return unAppCStr("[]",_3t);});}else{return [1,_3p,new T(function(){return B(A(_3r,[_3s[1],new T(function(){var _3u=function(_3v){return ((_3v=E(_3v))[0]==0)?E([1,_3o,_3t]):[1,_3n,new T(function(){return B(A(_3r,[_3v[1],new T(function(){return B(_3u(_3v[2]));})]));})];};return B(_3u(_3s[2]));})]));})];}},_3w=function(_3x,_3y){return new F(function(){return _3q(_3k,_3x,_3y);});},_3z=function(_3A,_3B,_3C){return new F(function(){return _C((_3B=E(_3B))[1],_3C);});},_3D=[0,_3z,_3i,_3w],_3E=new T(function(){return [0,_32,_3D,_3F,_3g];}),_3F=function(_3G){return [0,_3E,_3G];},_3H=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_3I=function(_3J,_3K){return new F(function(){return die(new T(function(){return B(A(_3K,[_3J]));}));});},_3L=function(_3M,_3N){if(!(_3N=E(_3N))[0]){return [0,_B,_B];}else{var _3O=_3N[1];if(!B(A(_3M,[_3O]))){return [0,_B,_3N];}else{var _3P=new T(function(){var _3Q=B(_3L(_3M,_3N[2]));return [0,_3Q[1],_3Q[2]];});return [0,[1,_3O,new T(function(){return E((_3P=E(_3P))[1]);})],new T(function(){return E((_3P=E(_3P))[2]);})];}}},_3R=[0,32],_3S=[0,10],_3T=[1,_3S,_B],_3U=function(_3V){var _3W=(_3V=E(_3V))[1];return ((_3W=E(_3W))==124)?false:true;},_3X=function(_3Y,_3Z){var _40=B(_3L(_3U,B(unCStr(_3Y)))),_41=_40[1],_42=_40[2],_43=function(_44,_45){return new F(function(){return _C(_44,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_C(_3Z,new T(function(){return B(_C(_45,_3T));},1)));})));},1));});};if(!(_42=E(_42))[0]){return new F(function(){return _43(_41,_B);});}else{var _46=_42[1],_47=(_46=E(_46))[1];if((_47=E(_47))==124){return new F(function(){return _43(_41,[1,_3R,_42[2]]);});}else{return new F(function(){return _43(_41,_B);});}}},_48=function(_49){return new F(function(){return _3I([0,new T(function(){return B(_3X(_49,_3H));})],_3F);});},_4a=new T(function(){return B(_48("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_4b=function(_4c,_4d){while(1){var _4e=(function(_4f,_4g){switch((_4f=E(_4f))[0]){case 0:if(!(_4g=E(_4g))[0]){return [0];}else{var _4h=B(A(_4f[1],[_4g[1]])),_4i=_4g[2];_4c=_4h;_4d=_4i;return null;}break;case 1:var _4h=B(A(_4f[1],[_4g])),_4i=_4g;_4c=_4h;_4d=_4i;return null;case 2:return [0];case 3:return [1,[0,_4f[1],_4g],new T(function(){return B(_4b(_4f[2],_4g));})];default:return E(_4f[1]);}})(_4c,_4d);if(_4e!=null){return _4e;}}},_4j=function(_4k,_4l){var _4m=function(_4n){if((_4l=E(_4l))[0]==3){return [3,_4l[1],new T(function(){return B(_4j(_4k,_4l[2]));})];}else{if((_4k=E(_4k))[0]==2){return E(_4l);}else{if((_4l=E(_4l))[0]==2){return E(_4k);}else{var _4o=function(_4p){if((_4l=E(_4l))[0]==4){var _4q=function(_4r){return [4,new T(function(){return B(_C(B(_4b(_4k,_4r)),_4l[1]));})];};return [1,_4q];}else{if((_4k=E(_4k))[0]==1){var _4s=_4k[1];if(!(_4l=E(_4l))[0]){var _4t=function(_4u){return new F(function(){return _4j(B(A(_4s,[_4u])),_4l);});};return [1,_4t];}else{var _4v=function(_4w){return new F(function(){return _4j(B(A(_4s,[_4w])),new T(function(){return B(A(_4l[1],[_4w]));}));});};return [1,_4v];}}else{if(!(_4l=E(_4l))[0]){return E(_4a);}else{var _4x=function(_4y){return new F(function(){return _4j(_4k,new T(function(){return B(A(_4l[1],[_4y]));}));});};return [1,_4x];}}}};switch((_4k=E(_4k))[0]){case 1:if((_4l=E(_4l))[0]==4){var _4z=function(_4A){return [4,new T(function(){return B(_C(B(_4b(B(A(_4k[1],[_4A])),_4A)),_4l[1]));})];};return [1,_4z];}else{return new F(function(){return _4o(_);});}break;case 4:var _4B=_4k[1];switch((_4l=E(_4l))[0]){case 0:var _4C=function(_4D){return [4,new T(function(){return B(_C(_4B,new T(function(){return B(_4b(_4l,_4D));},1)));})];};return [1,_4C];case 1:var _4E=function(_4F){return [4,new T(function(){return B(_C(_4B,new T(function(){return B(_4b(B(A(_4l[1],[_4F])),_4F));},1)));})];};return [1,_4E];default:return [4,new T(function(){return B(_C(_4B,_4l[1]));})];}break;default:return new F(function(){return _4o(_);});}}}}};switch((_4k=E(_4k))[0]){case 0:if(!(_4l=E(_4l))[0]){var _4G=function(_4H){return new F(function(){return _4j(B(A(_4k[1],[_4H])),new T(function(){return B(A(_4l[1],[_4H]));}));});};return [0,_4G];}else{return new F(function(){return _4m(_);});}break;case 3:return [3,_4k[1],new T(function(){return B(_4j(_4k[2],_4l));})];default:return new F(function(){return _4m(_);});}},_4I=[0,41],_4J=[1,_4I,_B],_4K=[0,40],_4L=[1,_4K,_B],_4M=function(_4N,_4O){while(1){if(!(_4N=E(_4N))[0]){return ((_4O=E(_4O))[0]==0)?true:false;}else{var _4P=_4N[1];if(!(_4O=E(_4O))[0]){return false;}else{var _4Q=_4O[1];if((_4P=E(_4P))[1]!=(_4Q=E(_4Q))[1]){return false;}else{var _4R=_4N[2],_4S=_4O[2];_4N=_4R;_4O=_4S;continue;}}}}},_4T=function(_4U,_4V){return (_4U=E(_4U))[1]!=(_4V=E(_4V))[1];},_4W=function(_4X,_4Y){return (_4X=E(_4X))[1]==(_4Y=E(_4Y))[1];},_4Z=[0,_4W,_4T],_50=function(_51,_52){while(1){if(!(_51=E(_51))[0]){return ((_52=E(_52))[0]==0)?true:false;}else{var _53=_51[1];if(!(_52=E(_52))[0]){return false;}else{var _54=_52[1];if((_53=E(_53))[1]!=(_54=E(_54))[1]){return false;}else{var _55=_51[2],_56=_52[2];_51=_55;_52=_56;continue;}}}}},_57=function(_58,_59){return (!B(_50(_58,_59)))?true:false;},_5a=[0,_50,_57],_5b=function(_5c,_5d){switch((_5c=E(_5c))[0]){case 0:var _5e=function(_5f){return new F(function(){return _5b(B(A(_5c[1],[_5f])),_5d);});};return [0,_5e];case 1:var _5g=function(_5h){return new F(function(){return _5b(B(A(_5c[1],[_5h])),_5d);});};return [1,_5g];case 2:return [2];case 3:return new F(function(){return _4j(B(A(_5d,[_5c[1]])),new T(function(){return B(_5b(_5c[2],_5d));}));});break;default:var _5i=function(_5j){if(!(_5j=E(_5j))[0]){return [0];}else{var _5k=_5j[1];return new F(function(){return _C(B(_4b(B(A(_5d,[(_5k=E(_5k))[1]])),_5k[2])),new T(function(){return B(_5i(_5j[2]));},1));});}},_5l=B(_5i(_5c[1]));return (_5l[0]==0)?[2]:[4,_5l];}},_5m=[2],_5n=function(_5o){return [3,_5o,_5m];},_5p=function(_5q,_5r){if(!(_5q=E(_5q))){return new F(function(){return A(_5r,[_0]);});}else{var _5s=function(_5t){return E(new T(function(){return B(_5p(_5q-1|0,_5r));}));};return [0,_5s];}},_5u=function(_5v,_5w,_5x){var _5y=function(_5z,_5A,_5B){while(1){var _5C=(function(_5D,_5E,_5F){switch((_5D=E(_5D))[0]){case 0:if(!(_5E=E(_5E))[0]){return E(_5w);}else{var _5G=B(A(_5D[1],[_5E[1]])),_5H=_5E[2],_5I=_5F+1|0;_5z=_5G;_5A=_5H;_5B=_5I;return null;}break;case 1:var _5G=B(A(_5D[1],[_5E])),_5H=_5E,_5I=_5F;_5z=_5G;_5A=_5H;_5B=_5I;return null;case 2:return E(_5w);case 3:var _5J=function(_5K){var _5L=function(_5M){return E(new T(function(){return B(_5b(_5D,_5K));}));};return new F(function(){return _5p(_5F,_5L);});};return E(_5J);default:return function(_5N){return new F(function(){return _5b(_5D,_5N);});};}})(_5z,_5A,_5B);if(_5C!=null){return _5C;}}};return function(_5O){return new F(function(){return A(_5y,[new T(function(){return B(A(_5v,[_5n]));}),_5O,0,_5x]);});};},_5P=function(_5Q){return new F(function(){return A(_5Q,[_B]);});},_5R=function(_5S,_5T){var _5U=function(_5V){if(!(_5V=E(_5V))[0]){return E(_5P);}else{var _5W=_5V[1];if(!B(A(_5S,[_5W]))){return E(_5P);}else{var _5X=function(_5Y){var _5Z=function(_60){return E(new T(function(){var _61=function(_62){return new F(function(){return A(_5Y,[[1,_5W,_62]]);});};return B(A(new T(function(){return B(_5U(_5V[2]));}),[_61]));}));};return [0,_5Z];};return E(_5X);}}};return function(_63){return new F(function(){return A(_5U,[_63,_5T]);});};},_64=[6],_65=function(_66){return E(_66);},_67=new T(function(){return B(unCStr("valDig: Bad base"));}),_68=new T(function(){return B(err(_67));}),_69=function(_6a,_6b){var _6c=function(_6d,_6e){if(!(_6d=E(_6d))[0]){var _6f=function(_6g){return new F(function(){return A(_6g,[new T(function(){return B(A(_6e,[_B]));})]);});};return E(_6f);}else{var _6h=_6d[1],_6i=(_6a=E(_6a))[1],_6j=(_6h=E(_6h))[1],_6k=function(_6l){var _6m=function(_6n){var _6o=function(_6p){return E(new T(function(){return B(A(new T(function(){var _6q=function(_6r){return new F(function(){return A(_6e,[[1,_6l,_6r]]);});};return B(_6c(_6d[2],_6q));}),[_6n]));}));};return [0,_6o];};return E(_6m);};switch(_6i=E(_6i)){case 8:if(48>_6j){var _6s=function(_6t){return new F(function(){return A(_6t,[new T(function(){return B(A(_6e,[_B]));})]);});};return E(_6s);}else{if(_6j>55){var _6u=function(_6v){return new F(function(){return A(_6v,[new T(function(){return B(A(_6e,[_B]));})]);});};return E(_6u);}else{return new F(function(){return _6k([0,_6j-48|0]);});}}break;case 10:if(48>_6j){var _6w=function(_6x){return new F(function(){return A(_6x,[new T(function(){return B(A(_6e,[_B]));})]);});};return E(_6w);}else{if(_6j>57){var _6y=function(_6z){return new F(function(){return A(_6z,[new T(function(){return B(A(_6e,[_B]));})]);});};return E(_6y);}else{return new F(function(){return _6k([0,_6j-48|0]);});}}break;case 16:if(48>_6j){if(97>_6j){if(65>_6j){var _6A=function(_6B){return new F(function(){return A(_6B,[new T(function(){return B(A(_6e,[_B]));})]);});};return E(_6A);}else{if(_6j>70){var _6C=function(_6D){return new F(function(){return A(_6D,[new T(function(){return B(A(_6e,[_B]));})]);});};return E(_6C);}else{return new F(function(){return _6k([0,(_6j-65|0)+10|0]);});}}}else{if(_6j>102){if(65>_6j){var _6E=function(_6F){return new F(function(){return A(_6F,[new T(function(){return B(A(_6e,[_B]));})]);});};return E(_6E);}else{if(_6j>70){var _6G=function(_6H){return new F(function(){return A(_6H,[new T(function(){return B(A(_6e,[_B]));})]);});};return E(_6G);}else{return new F(function(){return _6k([0,(_6j-65|0)+10|0]);});}}}else{return new F(function(){return _6k([0,(_6j-97|0)+10|0]);});}}}else{if(_6j>57){if(97>_6j){if(65>_6j){var _6I=function(_6J){return new F(function(){return A(_6J,[new T(function(){return B(A(_6e,[_B]));})]);});};return E(_6I);}else{if(_6j>70){var _6K=function(_6L){return new F(function(){return A(_6L,[new T(function(){return B(A(_6e,[_B]));})]);});};return E(_6K);}else{return new F(function(){return _6k([0,(_6j-65|0)+10|0]);});}}}else{if(_6j>102){if(65>_6j){var _6M=function(_6N){return new F(function(){return A(_6N,[new T(function(){return B(A(_6e,[_B]));})]);});};return E(_6M);}else{if(_6j>70){var _6O=function(_6P){return new F(function(){return A(_6P,[new T(function(){return B(A(_6e,[_B]));})]);});};return E(_6O);}else{return new F(function(){return _6k([0,(_6j-65|0)+10|0]);});}}}else{return new F(function(){return _6k([0,(_6j-97|0)+10|0]);});}}}else{return new F(function(){return _6k([0,_6j-48|0]);});}}break;default:return E(_68);}}},_6Q=function(_6R){if(!(_6R=E(_6R))[0]){return [2];}else{return new F(function(){return A(_6b,[_6R]);});}};return function(_6S){return new F(function(){return A(_6c,[_6S,_65,_6Q]);});};},_6T=[0,10],_6U=[0,1],_6V=[0,2147483647],_6W=function(_6X,_6Y){while(1){if(!(_6X=E(_6X))[0]){var _6Z=_6X[1];if(!(_6Y=E(_6Y))[0]){var _70=_6Y[1],_71=addC(_6Z,_70),_72=_71[2];if(!(_72=E(_72))){return [0,_71[1]];}else{_6X=[1,I_fromInt(_6Z)];_6Y=[1,I_fromInt(_70)];continue;}}else{_6X=[1,I_fromInt(_6Z)];continue;}}else{if(!(_6Y=E(_6Y))[0]){var _73=[1,I_fromInt(_6Y[1])];_6Y=_73;continue;}else{return [1,I_add(_6X[1],_6Y[1])];}}}},_74=new T(function(){return B(_6W(_6V,_6U));}),_75=function(_76){if(!(_76=E(_76))[0]){var _77=_76[1];return ((_77=E(_77))==(-2147483648))?E(_74):[0, -_77];}else{return [1,I_negate(_76[1])];}},_78=[0,10],_79=[0,0],_7a=function(_7b){return [0,_7b];},_7c=function(_7d,_7e){while(1){if(!(_7d=E(_7d))[0]){var _7f=_7d[1];if(!(_7e=E(_7e))[0]){var _7g=_7e[1];if(!(imul(_7f,_7g)|0)){return [0,imul(_7f,_7g)|0];}else{_7d=[1,I_fromInt(_7f)];_7e=[1,I_fromInt(_7g)];continue;}}else{_7d=[1,I_fromInt(_7f)];continue;}}else{if(!(_7e=E(_7e))[0]){var _7h=[1,I_fromInt(_7e[1])];_7e=_7h;continue;}else{return [1,I_mul(_7d[1],_7e[1])];}}}},_7i=function(_7j,_7k,_7l){while(1){if(!(_7l=E(_7l))[0]){return E(_7k);}else{var _7m=_7l[1],_7n=B(_6W(B(_7c(_7k,_7j)),B(_7a((_7m=E(_7m))[1])))),_7o=_7l[2];_7k=_7n;_7l=_7o;continue;}}},_7p=function(_7q){var _7r=new T(function(){var _7s=function(_7t){var _7u=(_7t=E(_7t))[1];if((_7u=E(_7u))==43){var _7v=function(_7w){return new F(function(){return A(_7q,[[1,new T(function(){return B(_7i(_78,_79,_7w));})]]);});};return [1,B(_69(_6T,_7v))];}else{return [2];}},_7x=function(_7y){var _7z=(_7y=E(_7y))[1];if((_7z=E(_7z))==45){var _7A=function(_7B){return new F(function(){return A(_7q,[[1,new T(function(){return B(_75(B(_7i(_78,_79,_7B))));})]]);});};return [1,B(_69(_6T,_7A))];}else{return [2];}};return B(_4j(B(_4j([0,_7x],[0,_7s])),new T(function(){var _7C=function(_7D){return new F(function(){return A(_7q,[[1,new T(function(){return B(_7i(_78,_79,_7D));})]]);});};return [1,B(_69(_6T,_7C))];})));}),_7E=function(_7F){var _7G=(_7F=E(_7F))[1];return ((_7G=E(_7G))==69)?E(_7r):[2];},_7H=function(_7I){var _7J=(_7I=E(_7I))[1];return ((_7J=E(_7J))==101)?E(_7r):[2];};return new F(function(){return _4j([0,_7H],[0,_7E]);});},_7K=[0],_7L=function(_7M){return new F(function(){return A(_7M,[_7K]);});},_7N=function(_7O){return new F(function(){return A(_7O,[_7K]);});},_7P=function(_7Q){var _7R=function(_7S){return new F(function(){return A(_7Q,[[1,_7S]]);});};return function(_7T){var _7U=(_7T=E(_7T))[1];return ((_7U=E(_7U))==46)?[1,B(_69(_6T,_7R))]:[2];};},_7V=function(_7W){return [0,B(_7P(_7W))];},_7X=function(_7Y){var _7Z=function(_80){var _81=function(_82){var _83=function(_84){return new F(function(){return A(_7Y,[[5,[1,_80,_82,_84]]]);});};return [1,B(_5u(_7p,_7N,_83))];};return [1,B(_5u(_7V,_7L,_81))];};return new F(function(){return _69(_6T,_7Z);});},_85=function(_86){return [1,B(_7X(_86))];},_87=function(_88,_89,_8a){while(1){if(!(_8a=E(_8a))[0]){return false;}else{if(!B(A(_1H,[_88,_89,_8a[1]]))){var _8b=_8a[2];_8a=_8b;continue;}else{return true;}}}},_8c=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_8d=function(_8e){return new F(function(){return _87(_4Z,_8e,_8c);});},_8f=[0,8],_8g=[0,16],_8h=function(_8i){var _8j=function(_8k){return new F(function(){return A(_8i,[[5,[0,_8f,_8k]]]);});},_8l=function(_8m){return new F(function(){return A(_8i,[[5,[0,_8g,_8m]]]);});},_8n=function(_8o){var _8p=(_8o=E(_8o))[1];switch(_8p=E(_8p)){case 79:return [1,B(_69(_8f,_8j))];case 88:return [1,B(_69(_8g,_8l))];case 111:return [1,B(_69(_8f,_8j))];case 120:return [1,B(_69(_8g,_8l))];default:return [2];}};return function(_8q){var _8r=(_8q=E(_8q))[1];return ((_8r=E(_8r))==48)?E([0,_8n]):[2];};},_8s=function(_8t){return [0,B(_8h(_8t))];},_8u=false,_8v=true,_8w=function(_8x){var _8y=new T(function(){return B(A(_8x,[_8f]));}),_8z=new T(function(){return B(A(_8x,[_8g]));});return function(_8A){var _8B=(_8A=E(_8A))[1];switch(_8B=E(_8B)){case 79:return E(_8y);case 88:return E(_8z);case 111:return E(_8y);case 120:return E(_8z);default:return [2];}};},_8C=function(_8D){return [0,B(_8w(_8D))];},_8E=[0,92],_8F=function(_8G){return new F(function(){return A(_8G,[_6T]);});},_8H=function(_8I){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_12(9,_8I,_B));}))));});},_8J=function(_8K){if(!(_8K=E(_8K))[0]){return E(_8K[1]);}else{return new F(function(){return I_toInt(_8K[1]);});}},_8L=function(_8M,_8N){if(!(_8M=E(_8M))[0]){var _8O=_8M[1];return ((_8N=E(_8N))[0]==0)?_8O<=_8N[1]:I_compareInt(_8N[1],_8O)>=0;}else{var _8P=_8M[1];return ((_8N=E(_8N))[0]==0)?I_compareInt(_8P,_8N[1])<=0:I_compare(_8P,_8N[1])<=0;}},_8Q=function(_8R){return [2];},_8S=function(_8T){if(!(_8T=E(_8T))[0]){return E(_8Q);}else{var _8U=_8T[1],_8V=_8T[2];if(!(_8V=E(_8V))[0]){return E(_8U);}else{var _8W=function(_8X){return new F(function(){return _4j(B(A(_8U,[_8X])),new T(function(){return B(A(new T(function(){return B(_8S(_8V));}),[_8X]));}));});};return E(_8W);}}},_8Y=function(_8Z){return [2];},_90=function(_91,_92){var _93=function(_94){return new F(function(){return A(_94,[_91]);});},_95=function(_96,_97){if(!(_96=E(_96))[0]){return E(_93);}else{var _98=_96[1];if(!(_97=E(_97))[0]){return E(_8Y);}else{var _99=_97[1];if((_98=E(_98))[1]!=(_99=E(_99))[1]){return E(_8Y);}else{var _9a=function(_9b){var _9c=function(_9d){return E(new T(function(){return B(A(new T(function(){return B(_95(_96[2],_97[2]));}),[_9b]));}));};return [0,_9c];};return E(_9a);}}}};return function(_9e){return new F(function(){return A(_95,[_91,_9e,_92]);});};},_9f=new T(function(){return B(unCStr("SOH"));}),_9g=[0,1],_9h=function(_9i){var _9j=function(_9k){return E(new T(function(){return B(A(_9i,[_9g]));}));};return [1,B(_90(_9f,_9j))];},_9l=new T(function(){return B(unCStr("SO"));}),_9m=[0,14],_9n=function(_9o){var _9p=function(_9q){return E(new T(function(){return B(A(_9o,[_9m]));}));};return [1,B(_90(_9l,_9p))];},_9r=function(_9s){return [1,B(_5u(_9h,_9n,_9s))];},_9t=new T(function(){return B(unCStr("NUL"));}),_9u=[0,0],_9v=function(_9w){var _9x=function(_9y){return E(new T(function(){return B(A(_9w,[_9u]));}));};return [1,B(_90(_9t,_9x))];},_9z=new T(function(){return B(unCStr("STX"));}),_9A=[0,2],_9B=function(_9C){var _9D=function(_9E){return E(new T(function(){return B(A(_9C,[_9A]));}));};return [1,B(_90(_9z,_9D))];},_9F=new T(function(){return B(unCStr("ETX"));}),_9G=[0,3],_9H=function(_9I){var _9J=function(_9K){return E(new T(function(){return B(A(_9I,[_9G]));}));};return [1,B(_90(_9F,_9J))];},_9L=new T(function(){return B(unCStr("EOT"));}),_9M=[0,4],_9N=function(_9O){var _9P=function(_9Q){return E(new T(function(){return B(A(_9O,[_9M]));}));};return [1,B(_90(_9L,_9P))];},_9R=new T(function(){return B(unCStr("ENQ"));}),_9S=[0,5],_9T=function(_9U){var _9V=function(_9W){return E(new T(function(){return B(A(_9U,[_9S]));}));};return [1,B(_90(_9R,_9V))];},_9X=new T(function(){return B(unCStr("ACK"));}),_9Y=[0,6],_9Z=function(_a0){var _a1=function(_a2){return E(new T(function(){return B(A(_a0,[_9Y]));}));};return [1,B(_90(_9X,_a1))];},_a3=new T(function(){return B(unCStr("BEL"));}),_a4=[0,7],_a5=function(_a6){var _a7=function(_a8){return E(new T(function(){return B(A(_a6,[_a4]));}));};return [1,B(_90(_a3,_a7))];},_a9=new T(function(){return B(unCStr("BS"));}),_aa=[0,8],_ab=function(_ac){var _ad=function(_ae){return E(new T(function(){return B(A(_ac,[_aa]));}));};return [1,B(_90(_a9,_ad))];},_af=new T(function(){return B(unCStr("HT"));}),_ag=[0,9],_ah=function(_ai){var _aj=function(_ak){return E(new T(function(){return B(A(_ai,[_ag]));}));};return [1,B(_90(_af,_aj))];},_al=new T(function(){return B(unCStr("LF"));}),_am=[0,10],_an=function(_ao){var _ap=function(_aq){return E(new T(function(){return B(A(_ao,[_am]));}));};return [1,B(_90(_al,_ap))];},_ar=new T(function(){return B(unCStr("VT"));}),_as=[0,11],_at=function(_au){var _av=function(_aw){return E(new T(function(){return B(A(_au,[_as]));}));};return [1,B(_90(_ar,_av))];},_ax=new T(function(){return B(unCStr("FF"));}),_ay=[0,12],_az=function(_aA){var _aB=function(_aC){return E(new T(function(){return B(A(_aA,[_ay]));}));};return [1,B(_90(_ax,_aB))];},_aD=new T(function(){return B(unCStr("CR"));}),_aE=[0,13],_aF=function(_aG){var _aH=function(_aI){return E(new T(function(){return B(A(_aG,[_aE]));}));};return [1,B(_90(_aD,_aH))];},_aJ=new T(function(){return B(unCStr("SI"));}),_aK=[0,15],_aL=function(_aM){var _aN=function(_aO){return E(new T(function(){return B(A(_aM,[_aK]));}));};return [1,B(_90(_aJ,_aN))];},_aP=new T(function(){return B(unCStr("DLE"));}),_aQ=[0,16],_aR=function(_aS){var _aT=function(_aU){return E(new T(function(){return B(A(_aS,[_aQ]));}));};return [1,B(_90(_aP,_aT))];},_aV=new T(function(){return B(unCStr("DC1"));}),_aW=[0,17],_aX=function(_aY){var _aZ=function(_b0){return E(new T(function(){return B(A(_aY,[_aW]));}));};return [1,B(_90(_aV,_aZ))];},_b1=new T(function(){return B(unCStr("DC2"));}),_b2=[0,18],_b3=function(_b4){var _b5=function(_b6){return E(new T(function(){return B(A(_b4,[_b2]));}));};return [1,B(_90(_b1,_b5))];},_b7=new T(function(){return B(unCStr("DC3"));}),_b8=[0,19],_b9=function(_ba){var _bb=function(_bc){return E(new T(function(){return B(A(_ba,[_b8]));}));};return [1,B(_90(_b7,_bb))];},_bd=new T(function(){return B(unCStr("DC4"));}),_be=[0,20],_bf=function(_bg){var _bh=function(_bi){return E(new T(function(){return B(A(_bg,[_be]));}));};return [1,B(_90(_bd,_bh))];},_bj=new T(function(){return B(unCStr("NAK"));}),_bk=[0,21],_bl=function(_bm){var _bn=function(_bo){return E(new T(function(){return B(A(_bm,[_bk]));}));};return [1,B(_90(_bj,_bn))];},_bp=new T(function(){return B(unCStr("SYN"));}),_bq=[0,22],_br=function(_bs){var _bt=function(_bu){return E(new T(function(){return B(A(_bs,[_bq]));}));};return [1,B(_90(_bp,_bt))];},_bv=new T(function(){return B(unCStr("ETB"));}),_bw=[0,23],_bx=function(_by){var _bz=function(_bA){return E(new T(function(){return B(A(_by,[_bw]));}));};return [1,B(_90(_bv,_bz))];},_bB=new T(function(){return B(unCStr("CAN"));}),_bC=[0,24],_bD=function(_bE){var _bF=function(_bG){return E(new T(function(){return B(A(_bE,[_bC]));}));};return [1,B(_90(_bB,_bF))];},_bH=new T(function(){return B(unCStr("EM"));}),_bI=[0,25],_bJ=function(_bK){var _bL=function(_bM){return E(new T(function(){return B(A(_bK,[_bI]));}));};return [1,B(_90(_bH,_bL))];},_bN=new T(function(){return B(unCStr("SUB"));}),_bO=[0,26],_bP=function(_bQ){var _bR=function(_bS){return E(new T(function(){return B(A(_bQ,[_bO]));}));};return [1,B(_90(_bN,_bR))];},_bT=new T(function(){return B(unCStr("ESC"));}),_bU=[0,27],_bV=function(_bW){var _bX=function(_bY){return E(new T(function(){return B(A(_bW,[_bU]));}));};return [1,B(_90(_bT,_bX))];},_bZ=new T(function(){return B(unCStr("FS"));}),_c0=[0,28],_c1=function(_c2){var _c3=function(_c4){return E(new T(function(){return B(A(_c2,[_c0]));}));};return [1,B(_90(_bZ,_c3))];},_c5=new T(function(){return B(unCStr("GS"));}),_c6=[0,29],_c7=function(_c8){var _c9=function(_ca){return E(new T(function(){return B(A(_c8,[_c6]));}));};return [1,B(_90(_c5,_c9))];},_cb=new T(function(){return B(unCStr("RS"));}),_cc=[0,30],_cd=function(_ce){var _cf=function(_cg){return E(new T(function(){return B(A(_ce,[_cc]));}));};return [1,B(_90(_cb,_cf))];},_ch=new T(function(){return B(unCStr("US"));}),_ci=[0,31],_cj=function(_ck){var _cl=function(_cm){return E(new T(function(){return B(A(_ck,[_ci]));}));};return [1,B(_90(_ch,_cl))];},_cn=new T(function(){return B(unCStr("SP"));}),_co=[0,32],_cp=function(_cq){var _cr=function(_cs){return E(new T(function(){return B(A(_cq,[_co]));}));};return [1,B(_90(_cn,_cr))];},_ct=new T(function(){return B(unCStr("DEL"));}),_cu=[0,127],_cv=function(_cw){var _cx=function(_cy){return E(new T(function(){return B(A(_cw,[_cu]));}));};return [1,B(_90(_ct,_cx))];},_cz=[1,_cv,_B],_cA=[1,_cp,_cz],_cB=[1,_cj,_cA],_cC=[1,_cd,_cB],_cD=[1,_c7,_cC],_cE=[1,_c1,_cD],_cF=[1,_bV,_cE],_cG=[1,_bP,_cF],_cH=[1,_bJ,_cG],_cI=[1,_bD,_cH],_cJ=[1,_bx,_cI],_cK=[1,_br,_cJ],_cL=[1,_bl,_cK],_cM=[1,_bf,_cL],_cN=[1,_b9,_cM],_cO=[1,_b3,_cN],_cP=[1,_aX,_cO],_cQ=[1,_aR,_cP],_cR=[1,_aL,_cQ],_cS=[1,_aF,_cR],_cT=[1,_az,_cS],_cU=[1,_at,_cT],_cV=[1,_an,_cU],_cW=[1,_ah,_cV],_cX=[1,_ab,_cW],_cY=[1,_a5,_cX],_cZ=[1,_9Z,_cY],_d0=[1,_9T,_cZ],_d1=[1,_9N,_d0],_d2=[1,_9H,_d1],_d3=[1,_9B,_d2],_d4=[1,_9v,_d3],_d5=[1,_9r,_d4],_d6=new T(function(){return B(_8S(_d5));}),_d7=[0,1114111],_d8=[0,34],_d9=[0,39],_da=function(_db){var _dc=new T(function(){return B(A(_db,[_a4]));}),_dd=new T(function(){return B(A(_db,[_aa]));}),_de=new T(function(){return B(A(_db,[_ag]));}),_df=new T(function(){return B(A(_db,[_am]));}),_dg=new T(function(){return B(A(_db,[_as]));}),_dh=new T(function(){return B(A(_db,[_ay]));}),_di=new T(function(){return B(A(_db,[_aE]));}),_dj=function(_dk){var _dl=(_dk=E(_dk))[1];switch(_dl=E(_dl)){case 34:return E(new T(function(){return B(A(_db,[_d8]));}));case 39:return E(new T(function(){return B(A(_db,[_d9]));}));case 92:return E(new T(function(){return B(A(_db,[_8E]));}));case 97:return E(_dc);case 98:return E(_dd);case 102:return E(_dh);case 110:return E(_df);case 114:return E(_di);case 116:return E(_de);case 118:return E(_dg);default:return [2];}};return new F(function(){return _4j([0,_dj],new T(function(){var _dm=function(_dn){var _do=function(_dp){var _dq=B(_7i(new T(function(){return B(_7a((_dn=E(_dn))[1]));}),_79,_dp));if(!B(_8L(_dq,_d7))){return [2];}else{return new F(function(){return A(_db,[new T(function(){var _dr=B(_8J(_dq));if(_dr>>>0>1114111){return B(_8H(_dr));}else{return [0,_dr];}})]);});}};return [1,B(_69(_dn,_do))];};return B(_4j([1,B(_5u(_8C,_8F,_dm))],new T(function(){var _ds=function(_dt){var _du=(_dt=E(_dt))[1];switch(_du=E(_du)){case 64:return E(new T(function(){return B(A(_db,[_9u]));}));case 65:return E(new T(function(){return B(A(_db,[_9g]));}));case 66:return E(new T(function(){return B(A(_db,[_9A]));}));case 67:return E(new T(function(){return B(A(_db,[_9G]));}));case 68:return E(new T(function(){return B(A(_db,[_9M]));}));case 69:return E(new T(function(){return B(A(_db,[_9S]));}));case 70:return E(new T(function(){return B(A(_db,[_9Y]));}));case 71:return E(_dc);case 72:return E(_dd);case 73:return E(_de);case 74:return E(_df);case 75:return E(_dg);case 76:return E(_dh);case 77:return E(_di);case 78:return E(new T(function(){return B(A(_db,[_9m]));}));case 79:return E(new T(function(){return B(A(_db,[_aK]));}));case 80:return E(new T(function(){return B(A(_db,[_aQ]));}));case 81:return E(new T(function(){return B(A(_db,[_aW]));}));case 82:return E(new T(function(){return B(A(_db,[_b2]));}));case 83:return E(new T(function(){return B(A(_db,[_b8]));}));case 84:return E(new T(function(){return B(A(_db,[_be]));}));case 85:return E(new T(function(){return B(A(_db,[_bk]));}));case 86:return E(new T(function(){return B(A(_db,[_bq]));}));case 87:return E(new T(function(){return B(A(_db,[_bw]));}));case 88:return E(new T(function(){return B(A(_db,[_bC]));}));case 89:return E(new T(function(){return B(A(_db,[_bI]));}));case 90:return E(new T(function(){return B(A(_db,[_bO]));}));case 91:return E(new T(function(){return B(A(_db,[_bU]));}));case 92:return E(new T(function(){return B(A(_db,[_c0]));}));case 93:return E(new T(function(){return B(A(_db,[_c6]));}));case 94:return E(new T(function(){return B(A(_db,[_cc]));}));case 95:return E(new T(function(){return B(A(_db,[_ci]));}));default:return [2];}},_dv=function(_dw){var _dx=(_dw=E(_dw))[1];return ((_dx=E(_dx))==94)?E([0,_ds]):[2];};return B(_4j([0,_dv],new T(function(){return B(A(_d6,[_db]));})));})));}));});},_dy=function(_dz){return new F(function(){return A(_dz,[_0]);});},_dA=function(_dB){if(!(_dB=E(_dB))[0]){return E(_dy);}else{var _dC=_dB[1],_dD=_dB[2],_dE=(_dC=E(_dC))[1];switch(_dE=E(_dE)){case 9:var _dF=function(_dG){var _dH=function(_dI){return E(new T(function(){return B(A(new T(function(){return B(_dA(_dD));}),[_dG]));}));};return [0,_dH];};return E(_dF);case 10:var _dJ=function(_dK){var _dL=function(_dM){return E(new T(function(){return B(A(new T(function(){return B(_dA(_dD));}),[_dK]));}));};return [0,_dL];};return E(_dJ);case 11:var _dN=function(_dO){var _dP=function(_dQ){return E(new T(function(){return B(A(new T(function(){return B(_dA(_dD));}),[_dO]));}));};return [0,_dP];};return E(_dN);case 12:var _dR=function(_dS){var _dT=function(_dU){return E(new T(function(){return B(A(new T(function(){return B(_dA(_dD));}),[_dS]));}));};return [0,_dT];};return E(_dR);case 13:var _dV=function(_dW){var _dX=function(_dY){return E(new T(function(){return B(A(new T(function(){return B(_dA(_dD));}),[_dW]));}));};return [0,_dX];};return E(_dV);case 32:var _dZ=function(_e0){var _e1=function(_e2){return E(new T(function(){return B(A(new T(function(){return B(_dA(_dD));}),[_e0]));}));};return [0,_e1];};return E(_dZ);case 160:var _e3=function(_e4){var _e5=function(_e6){return E(new T(function(){return B(A(new T(function(){return B(_dA(_dD));}),[_e4]));}));};return [0,_e5];};return E(_e3);default:var _e7=u_iswspace(_dE),_e8=_e7;if(!(_e8=E(_e8))){return E(_dy);}else{var _e9=function(_ea){var _eb=function(_ec){return E(new T(function(){return B(A(new T(function(){return B(_dA(_dD));}),[_ea]));}));};return [0,_eb];};return E(_e9);}}}},_ed=function(_ee){var _ef=new T(function(){return B(_ed(_ee));}),_eg=function(_eh){var _ei=(_eh=E(_eh))[1];return ((_ei=E(_ei))==92)?E(_ef):[2];},_ej=function(_ek){return E([0,_eg]);},_el=function(_em){return new F(function(){return A(_dA,[_em,_ej]);});},_en=[1,_el],_eo=function(_ep){var _eq=(_ep=E(_ep))[1];switch(_eq=E(_eq)){case 9:return E(_en);case 10:return E(_en);case 11:return E(_en);case 12:return E(_en);case 13:return E(_en);case 32:return E(_en);case 38:return E(_ef);case 160:return E(_en);default:var _er=u_iswspace(_eq),_es=_er;return ((_es=E(_es))==0)?[2]:E(_en);}},_et=function(_eu){var _ev=(_eu=E(_eu))[1];if((_ev=E(_ev))==92){return E(new T(function(){var _ew=function(_ex){return new F(function(){return A(_ee,[[0,_ex,_8v]]);});};return B(_da(_ew));}));}else{return new F(function(){return A(_ee,[[0,_eu,_8u]]);});}},_ey=function(_ez){var _eA=(_ez=E(_ez))[1];return ((_eA=E(_eA))==92)?E([0,_eo]):[2];};return new F(function(){return _4j([0,_ey],[0,_et]);});},_eB=function(_eC,_eD){var _eE=function(_eF){var _eG=(_eF=E(_eF))[1],_eH=_eF[2],_eI=(_eG=E(_eG))[1];if((_eI=E(_eI))==34){if(!(_eH=E(_eH))){return E(new T(function(){return B(A(_eD,[[1,new T(function(){return B(A(_eC,[_B]));})]]));}));}else{var _eJ=function(_eK){return new F(function(){return A(_eC,[[1,_eG,_eK]]);});};return new F(function(){return _eB(_eJ,_eD);});}}else{var _eL=function(_eM){return new F(function(){return A(_eC,[[1,_eG,_eM]]);});};return new F(function(){return _eB(_eL,_eD);});}};return new F(function(){return _ed(_eE);});},_eN=new T(function(){return B(unCStr("_\'"));}),_eO=function(_eP){var _eQ=u_iswalnum(_eP),_eR=_eQ;if(!(_eR=E(_eR))){return new F(function(){return _87(_4Z,[0,_eP],_eN);});}else{return true;}},_eS=function(_eT){return new F(function(){return _eO((_eT=E(_eT))[1]);});},_eU=new T(function(){return B(unCStr(",;()[]{}`"));}),_eV=new T(function(){return B(unCStr(".."));}),_eW=new T(function(){return B(unCStr("::"));}),_eX=new T(function(){return B(unCStr("->"));}),_eY=[0,64],_eZ=[1,_eY,_B],_f0=[0,126],_f1=[1,_f0,_B],_f2=new T(function(){return B(unCStr("=>"));}),_f3=[1,_f2,_B],_f4=[1,_f1,_f3],_f5=[1,_eZ,_f4],_f6=[1,_eX,_f5],_f7=new T(function(){return B(unCStr("<-"));}),_f8=[1,_f7,_f6],_f9=[0,124],_fa=[1,_f9,_B],_fb=[1,_fa,_f8],_fc=[1,_8E,_B],_fd=[1,_fc,_fb],_fe=[0,61],_ff=[1,_fe,_B],_fg=[1,_ff,_fd],_fh=[1,_eW,_fg],_fi=[1,_eV,_fh],_fj=function(_fk){var _fl=function(_fm){return ((_fm=E(_fm))[0]==0)?E(new T(function(){return B(A(_fk,[_64]));})):[2];};return new F(function(){return _4j([1,_fl],new T(function(){var _fn=function(_fo){var _fp=(_fo=E(_fo))[1];switch(_fp=E(_fp)){case 39:return [2];case 92:return E(new T(function(){var _fq=function(_fr){var _fs=function(_ft){var _fu=(_ft=E(_ft))[1];return ((_fu=E(_fu))==39)?E(new T(function(){return B(A(_fk,[[0,_fr]]));})):[2];};return [0,_fs];};return B(_da(_fq));}));default:var _fv=function(_fw){var _fx=(_fw=E(_fw))[1];return ((_fx=E(_fx))==39)?E(new T(function(){return B(A(_fk,[[0,_fo]]));})):[2];};return [0,_fv];}},_fy=function(_fz){var _fA=(_fz=E(_fz))[1];return ((_fA=E(_fA))==39)?E([0,_fn]):[2];};return B(_4j([0,_fy],new T(function(){var _fB=function(_fC){var _fD=(_fC=E(_fC))[1];return ((_fD=E(_fD))==34)?E(new T(function(){return B(_eB(_65,_fk));})):[2];};return B(_4j([0,_fB],new T(function(){var _fE=function(_fF){if(!B(_87(_4Z,_fF,_eU))){return [2];}else{return new F(function(){return A(_fk,[[2,[1,_fF,_B]]]);});}};return B(_4j([0,_fE],new T(function(){var _fG=function(_fH){if(!B(_87(_4Z,_fH,_8c))){return [2];}else{var _fI=function(_fJ){var _fK=[1,_fH,_fJ];if(!B(_87(_5a,_fK,_fi))){return new F(function(){return A(_fk,[[4,_fK]]);});}else{return new F(function(){return A(_fk,[[2,_fK]]);});}};return [1,B(_5R(_8d,_fI))];}};return B(_4j([0,_fG],new T(function(){var _fL=function(_fM){var _fN=(_fM=E(_fM))[1],_fO=u_iswalpha(_fN),_fP=_fO;if(!(_fP=E(_fP))){if((_fN=E(_fN))==95){var _fQ=function(_fR){return new F(function(){return A(_fk,[[3,[1,_fM,_fR]]]);});};return [1,B(_5R(_eS,_fQ))];}else{return [2];}}else{var _fS=function(_fT){return new F(function(){return A(_fk,[[3,[1,_fM,_fT]]]);});};return [1,B(_5R(_eS,_fS))];}};return B(_4j([0,_fL],new T(function(){return [1,B(_5u(_8s,_85,_fk))];})));})));})));})));})));}));});},_fU=[0,0],_fV=function(_fW,_fX){var _fY=function(_fZ){return E(new T(function(){var _g0=function(_g1){return ((_g1=E(_g1))[0]==2)?(!B(_4M(_g1[1],_4L)))?[2]:E(new T(function(){var _g2=function(_g3){var _g4=function(_g5){return E(new T(function(){var _g6=function(_g7){return ((_g7=E(_g7))[0]==2)?(!B(_4M(_g7[1],_4J)))?[2]:E(new T(function(){return B(A(_fX,[_g3]));})):[2];};return B(_fj(_g6));}));},_g8=function(_g9){return new F(function(){return A(_dA,[_g9,_g4]);});};return [1,_g8];};return B(A(_fW,[_fU,_g2]));})):[2];};return B(_fj(_g0));}));};return function(_ga){return new F(function(){return A(_dA,[_ga,_fY]);});};},_gb=function(_gc,_gd,_ge){var _gf=function(_gg,_gh){var _gi=function(_gj){return new F(function(){return A(_gh,[new T(function(){return [0, -(_gj=E(_gj))[1]];})]);});},_gk=function(_gl){return new F(function(){return A(_gc,[_gl,_gg,_gi]);});},_gm=function(_gn){return E(new T(function(){return B(_fj(_gk));}));},_go=function(_gp){return new F(function(){return A(_dA,[_gp,_gm]);});},_gq=function(_gr){if((_gr=E(_gr))[0]==4){var _gs=_gr[1];if(!(_gs=E(_gs))[0]){return new F(function(){return A(_gc,[_gr,_gg,_gh]);});}else{var _gt=_gs[1],_gu=_gs[2],_gv=(_gt=E(_gt))[1];if((_gv=E(_gv))==45){if(!(_gu=E(_gu))[0]){return E([1,_go]);}else{return new F(function(){return A(_gc,[_gr,_gg,_gh]);});}}else{return new F(function(){return A(_gc,[_gr,_gg,_gh]);});}}}else{return new F(function(){return A(_gc,[_gr,_gg,_gh]);});}},_gw=function(_gx){return E(new T(function(){return B(_fj(_gq));}));},_gy=function(_gz){return new F(function(){return A(_dA,[_gz,_gw]);});};return new F(function(){return _4j([1,_gy],new T(function(){return [1,B(_fV(_gf,_gh))];}));});};return new F(function(){return _gf(_gd,_ge);});},_gA=function(_gB,_gC){return [2];},_gD=function(_gE){if(!(_gE=E(_gE))[0]){var _gF=_gE[1];return [1,new T(function(){return B(_7i(new T(function(){return B(_7a((_gF=E(_gF))[1]));}),_79,_gE[2]));})];}else{var _gG=_gE[2],_gH=_gE[3];return ((_gG=E(_gG))[0]==0)?((_gH=E(_gH))[0]==0)?[1,new T(function(){return B(_7i(_78,_79,_gE[1]));})]:[0]:[0];}},_gI=function(_gJ){if((_gJ=E(_gJ))[0]==5){var _gK=B(_gD(_gJ[1]));if(!_gK[0]){return E(_gA);}else{var _gL=function(_gM,_gN){return new F(function(){return A(_gN,[new T(function(){return [0,B(_8J(_gK[1]))];})]);});};return E(_gL);}}else{return E(_gA);}},_gO=function(_gP){var _gQ=function(_gR){return E([3,_gP,_5m]);},_gS=function(_gT){return new F(function(){return A(_dA,[_gT,_gQ]);});};return [1,_gS];},_gU=new T(function(){return B(_gb(_gI,_fU,_gO));}),_gV=function(_gW){while(1){var _gX=(function(_gY){if(!(_gY=E(_gY))[0]){return [0];}else{var _gZ=_gY[1],_h0=_gY[2],_h1=_gZ[2];if(!(_h1=E(_h1))[0]){return [1,(_gZ=E(_gZ))[1],new T(function(){return B(_gV(_h0));})];}else{_gW=_h0;return null;}}})(_gW);if(_gX!=null){return _gX;}}},_h2=function(_h3,_h4,_h5){while(1){var _h6=(function(_h7,_h8,_h9){var _ha=B(_1J(_1G,new T(function(){return [0,toJSStr(B(_12(0,_h8,_B)))];}),_h7));if(!_ha[0]){return E(_h9);}else{var _hb=_ha[1],_hc=_h7,_hd=_h8+1|0,_he=[1,[0,new T(function(){return B(_2N(_hb,_1T));}),new T(function(){var _hf=B(_gV(B(_4b(_gU,new T(function(){return B(_2N(_hb,_1U));})))));if(!_hf[0]){return E(_2T);}else{var _hg=_hf[2];if(!(_hg=E(_hg))[0]){return E(_hf[1]);}else{return E(_2R);}}})],_h9];_h3=_hc;_h4=_hd;_h5=_he;return null;}})(_h3,_h4,_h5);if(_h6!=null){return _h6;}}},_hh=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_hi=function(_hj){return new F(function(){return _3I([0,new T(function(){return B(_3X(_hj,_hh));})],_3F);});},_hk=new T(function(){return B(_hi("main.hs:48:9-33|Data.Either.Right json"));}),_hl=new T(function(){return [0,"txID"];}),_hm=new T(function(){return [0,"txInputs"];}),_hn=new T(function(){return [0,"txOutputCount"];}),_ho=new T(function(){return [0,"txPayload"];}),_hp=function(_hq,_hr){while(1){var _hs=(function(_ht,_hu){if(!(_hu=E(_hu))[0]){return E(_ht);}else{var _hv=_hu[1],_hw=new T(function(){var _hx=jsParseJSON((_hv=E(_hv))[1]),_hy=_hx,_hz=_hy;if(!(_hz=E(_hz))[0]){return E(_hk);}else{return E(_hz[1]);}}),_hA=[1,[0,new T(function(){return B(_2N(_hw,_ho));}),new T(function(){var _hB=B(_1P(_hw,_hm));if(_hB[0]==4){var _hC=_hB[1],_hD=B(_1J(_1G,new T(function(){return [0,toJSStr(B(_12(0,0,_B)))];}),_hC));if(!_hD[0]){return [0];}else{var _hE=_hD[1];return B(_h2(_hC,1,[1,[0,new T(function(){return B(_2N(_hE,_1T));}),new T(function(){var _hF=B(_gV(B(_4b(_gU,new T(function(){return B(_2N(_hE,_1U));})))));if(!_hF[0]){return E(_2T);}else{var _hG=_hF[2];if(!(_hG=E(_hG))[0]){return E(_hF[1]);}else{return E(_2R);}}})],_B]));}}else{return [0];}}),new T(function(){return B(_2N(_hw,_hl));}),new T(function(){var _hH=B(_gV(B(_4b(_gU,new T(function(){return B(_2N(_hw,_hn));})))));if(!_hH[0]){return E(_2T);}else{var _hI=_hH[2];if(!(_hI=E(_hI))[0]){return E(_hH[1]);}else{return E(_2R);}}})],_ht],_hJ=_hu[2];_hq=_hA;_hr=_hJ;return null;}})(_hq,_hr);if(_hs!=null){return _hs;}}},_hK=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_hL=function(_hM){return new F(function(){return err(_hK);});},_hN=new T(function(){return B(_hL(_));}),_hO=function(_hP,_hQ,_hR,_hS){if(!(_hR=E(_hR))[0]){var _hT=_hR[1];if(!(_hS=E(_hS))[0]){var _hU=_hS[1],_hV=_hS[2],_hW=_hS[3],_hX=_hS[4],_hY=_hS[5];if(_hU<=(imul(3,_hT)|0)){return [0,(1+_hT|0)+_hU|0,E(_hP=E(_hP)),_hQ,E(_hR),E(_hS)];}else{if(!(_hX=E(_hX))[0]){var _hZ=_hX[1],_i0=_hX[2],_i1=_hX[3],_i2=_hX[4],_i3=_hX[5];if(!(_hY=E(_hY))[0]){var _i4=_hY[1];if(_hZ>=(imul(2,_i4)|0)){var _i5=function(_i6){return ((_i3=E(_i3))[0]==0)?[0,(1+_hT|0)+_hU|0,E(_i0),_i1,E([0,(1+_hT|0)+_i6|0,E(_hP),_hQ,E(_hR),E(_i2)]),E([0,(1+_i4|0)+_i3[1]|0,E(_hV),_hW,E(_i3),E(_hY)])]:[0,(1+_hT|0)+_hU|0,E(_i0),_i1,E([0,(1+_hT|0)+_i6|0,E(_hP=E(_hP)),_hQ,E(_hR),E(_i2)]),E([0,1+_i4|0,E(_hV),_hW,E(_A),E(_hY)])];};if(!(_i2=E(_i2))[0]){return new F(function(){return _i5(_i2[1]);});}else{return new F(function(){return _i5(0);});}}else{return [0,(1+_hT|0)+_hU|0,E(_hV),_hW,E([0,(1+_hT|0)+_hZ|0,E(_hP=E(_hP)),_hQ,E(_hR),E(_hX)]),E(_hY)];}}else{return E(_hN);}}else{return E(_hN);}}}else{return [0,1+_hT|0,E(_hP=E(_hP)),_hQ,E(_hR),E(_A)];}}else{if(!(_hS=E(_hS))[0]){var _i7=_hS[1],_i8=_hS[2],_i9=_hS[3],_ia=_hS[4],_ib=_hS[5];if(!(_ia=E(_ia))[0]){var _ic=_ia[1],_id=_ia[2],_ie=_ia[3],_if=_ia[4],_ig=_ia[5];if(!(_ib=E(_ib))[0]){var _ih=_ib[1];if(_ic>=(imul(2,_ih)|0)){var _ii=function(_ij){return ((_ig=E(_ig))[0]==0)?[0,1+_i7|0,E(_id),_ie,E([0,1+_ij|0,E(_hP),_hQ,E(_A),E(_if)]),E([0,(1+_ih|0)+_ig[1]|0,E(_i8),_i9,E(_ig),E(_ib)])]:[0,1+_i7|0,E(_id),_ie,E([0,1+_ij|0,E(_hP=E(_hP)),_hQ,E(_A),E(_if)]),E([0,1+_ih|0,E(_i8),_i9,E(_A),E(_ib)])];};if(!(_if=E(_if))[0]){return new F(function(){return _ii(_if[1]);});}else{return new F(function(){return _ii(0);});}}else{return [0,1+_i7|0,E(_i8),_i9,E([0,1+_ic|0,E(_hP=E(_hP)),_hQ,E(_A),E(_ia)]),E(_ib)];}}else{return [0,3,E(_id),_ie,E([0,1,E(_hP=E(_hP)),_hQ,E(_A),E(_A)]),E([0,1,E(_i8),_i9,E(_A),E(_A)])];}}else{return ((_ib=E(_ib))[0]==0)?[0,3,E(_i8),_i9,E([0,1,E(_hP=E(_hP)),_hQ,E(_A),E(_A)]),E(_ib)]:[0,2,E(_hP=E(_hP)),_hQ,E(_A),E(_hS)];}}else{return [0,1,E(_hP=E(_hP)),_hQ,E(_A),E(_A)];}}},_ik=function(_il,_im){return [0,1,E(_il=E(_il)),_im,E(_A),E(_A)];},_in=function(_io,_ip,_iq){if(!(_iq=E(_iq))[0]){return new F(function(){return _hO(_iq[2],_iq[3],_iq[4],B(_in(_io,_ip,_iq[5])));});}else{return new F(function(){return _ik(_io,_ip);});}},_ir=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_is=function(_it){return new F(function(){return err(_ir);});},_iu=new T(function(){return B(_is(_));}),_iv=function(_iw,_ix,_iy,_iz){if(!(_iz=E(_iz))[0]){var _iA=_iz[1];if(!(_iy=E(_iy))[0]){var _iB=_iy[1],_iC=_iy[2],_iD=_iy[3],_iE=_iy[4],_iF=_iy[5];if(_iB<=(imul(3,_iA)|0)){return [0,(1+_iB|0)+_iA|0,E(_iw=E(_iw)),_ix,E(_iy),E(_iz)];}else{if(!(_iE=E(_iE))[0]){var _iG=_iE[1];if(!(_iF=E(_iF))[0]){var _iH=_iF[1],_iI=_iF[2],_iJ=_iF[3],_iK=_iF[4],_iL=_iF[5];if(_iH>=(imul(2,_iG)|0)){var _iM=function(_iN){return ((_iL=E(_iL))[0]==0)?[0,(1+_iB|0)+_iA|0,E(_iI),_iJ,E([0,(1+_iG|0)+_iN|0,E(_iC),_iD,E(_iE),E(_iK)]),E([0,(1+_iA|0)+_iL[1]|0,E(_iw=E(_iw)),_ix,E(_iL),E(_iz)])]:[0,(1+_iB|0)+_iA|0,E(_iI),_iJ,E([0,(1+_iG|0)+_iN|0,E(_iC),_iD,E(_iE),E(_iK)]),E([0,1+_iA|0,E(_iw=E(_iw)),_ix,E(_A),E(_iz)])];};if(!(_iK=E(_iK))[0]){return new F(function(){return _iM(_iK[1]);});}else{return new F(function(){return _iM(0);});}}else{return [0,(1+_iB|0)+_iA|0,E(_iC),_iD,E(_iE),E([0,(1+_iA|0)+_iH|0,E(_iw=E(_iw)),_ix,E(_iF),E(_iz)])];}}else{return E(_iu);}}else{return E(_iu);}}}else{return [0,1+_iA|0,E(_iw=E(_iw)),_ix,E(_A),E(_iz)];}}else{if(!(_iy=E(_iy))[0]){var _iO=_iy[1],_iP=_iy[2],_iQ=_iy[3],_iR=_iy[4],_iS=_iy[5];if(!(_iR=E(_iR))[0]){var _iT=_iR[1];if(!(_iS=E(_iS))[0]){var _iU=_iS[1],_iV=_iS[2],_iW=_iS[3],_iX=_iS[4],_iY=_iS[5];if(_iU>=(imul(2,_iT)|0)){var _iZ=function(_j0){return ((_iY=E(_iY))[0]==0)?[0,1+_iO|0,E(_iV),_iW,E([0,(1+_iT|0)+_j0|0,E(_iP),_iQ,E(_iR),E(_iX)]),E([0,1+_iY[1]|0,E(_iw=E(_iw)),_ix,E(_iY),E(_A)])]:[0,1+_iO|0,E(_iV),_iW,E([0,(1+_iT|0)+_j0|0,E(_iP),_iQ,E(_iR),E(_iX)]),E([0,1,E(_iw=E(_iw)),_ix,E(_A),E(_A)])];};if(!(_iX=E(_iX))[0]){return new F(function(){return _iZ(_iX[1]);});}else{return new F(function(){return _iZ(0);});}}else{return [0,1+_iO|0,E(_iP),_iQ,E(_iR),E([0,1+_iU|0,E(_iw=E(_iw)),_ix,E(_iS),E(_A)])];}}else{return [0,3,E(_iP),_iQ,E(_iR),E([0,1,E(_iw=E(_iw)),_ix,E(_A),E(_A)])];}}else{return ((_iS=E(_iS))[0]==0)?[0,3,E(_iS[2]),_iS[3],E([0,1,E(_iP),_iQ,E(_A),E(_A)]),E([0,1,E(_iw=E(_iw)),_ix,E(_A),E(_A)])]:[0,2,E(_iw=E(_iw)),_ix,E(_iy),E(_A)];}}else{return [0,1,E(_iw=E(_iw)),_ix,E(_A),E(_A)];}}},_j1=function(_j2,_j3,_j4){if(!(_j4=E(_j4))[0]){return new F(function(){return _iv(_j4[2],_j4[3],B(_j1(_j2,_j3,_j4[4])),_j4[5]);});}else{return new F(function(){return _ik(_j2,_j3);});}},_j5=function(_j6,_j7,_j8,_j9,_ja,_jb,_jc){return new F(function(){return _iv(_j9,_ja,B(_j1(_j6,_j7,_jb)),_jc);});},_jd=function(_je,_jf,_jg,_jh,_ji,_jj,_jk,_jl){if(!(_jg=E(_jg))[0]){var _jm=_jg[1],_jn=_jg[2],_jo=_jg[3],_jp=_jg[4],_jq=_jg[5];if((imul(3,_jm)|0)>=_jh){if((imul(3,_jh)|0)>=_jm){return [0,(_jm+_jh|0)+1|0,E(_je=E(_je)),_jf,E(_jg),E([0,_jh,E(_ji),_jj,E(_jk),E(_jl)])];}else{return new F(function(){return _hO(_jn,_jo,_jp,B(_jd(_je,_jf,_jq,_jh,_ji,_jj,_jk,_jl)));});}}else{return new F(function(){return _iv(_ji,_jj,B(_jr(_je,_jf,_jm,_jn,_jo,_jp,_jq,_jk)),_jl);});}}else{return new F(function(){return _j5(_je,_jf,_jh,_ji,_jj,_jk,_jl);});}},_jr=function(_js,_jt,_ju,_jv,_jw,_jx,_jy,_jz){if(!(_jz=E(_jz))[0]){var _jA=_jz[1],_jB=_jz[2],_jC=_jz[3],_jD=_jz[4],_jE=_jz[5];if((imul(3,_ju)|0)>=_jA){if((imul(3,_jA)|0)>=_ju){return [0,(_ju+_jA|0)+1|0,E(_js=E(_js)),_jt,E([0,_ju,E(_jv),_jw,E(_jx),E(_jy)]),E(_jz)];}else{return new F(function(){return _hO(_jv,_jw,_jx,B(_jd(_js,_jt,_jy,_jA,_jB,_jC,_jD,_jE)));});}}else{return new F(function(){return _iv(_jB,_jC,B(_jr(_js,_jt,_ju,_jv,_jw,_jx,_jy,_jD)),_jE);});}}else{return new F(function(){return _in(_js,_jt,[0,_ju,E(_jv),_jw,E(_jx),E(_jy)]);});}},_jF=function(_jG,_jH,_jI,_jJ){if(!(_jI=E(_jI))[0]){var _jK=_jI[1],_jL=_jI[2],_jM=_jI[3],_jN=_jI[4],_jO=_jI[5];if(!(_jJ=E(_jJ))[0]){var _jP=_jJ[1],_jQ=_jJ[2],_jR=_jJ[3],_jS=_jJ[4],_jT=_jJ[5];if((imul(3,_jK)|0)>=_jP){if((imul(3,_jP)|0)>=_jK){return [0,(_jK+_jP|0)+1|0,E(_jG=E(_jG)),_jH,E(_jI),E(_jJ)];}else{return new F(function(){return _hO(_jL,_jM,_jN,B(_jd(_jG,_jH,_jO,_jP,_jQ,_jR,_jS,_jT)));});}}else{return new F(function(){return _iv(_jQ,_jR,B(_jr(_jG,_jH,_jK,_jL,_jM,_jN,_jO,_jS)),_jT);});}}else{return new F(function(){return _in(_jG,_jH,_jI);});}}else{return new F(function(){return _j1(_jG,_jH,_jJ);});}},_jU=function(_jV,_jW,_jX,_jY){if((_jV=E(_jV))==1){if(!(_jY=E(_jY))[0]){return [0,[0,1,E([0,_jW]),_jX,E(_A),E(_A)],_B,_B];}else{var _jZ=_jY[1],_k0=(_jZ=E(_jZ))[1];return (_jW<(_k0=E(_k0))[1])?[0,[0,1,E([0,_jW]),_jX,E(_A),E(_A)],_jY,_B]:[0,[0,1,E([0,_jW]),_jX,E(_A),E(_A)],_B,_jY];}}else{var _k1=B(_jU(_jV>>1,_jW,_jX,_jY)),_k2=_k1[1],_k3=_k1[2],_k4=_k1[3];if(!(_k3=E(_k3))[0]){return [0,_k2,_B,_k4];}else{var _k5=_k3[1],_k6=_k3[2],_k7=(_k5=E(_k5))[1],_k8=_k5[2];if(!(_k6=E(_k6))[0]){return [0,new T(function(){return B(_in(_k7,_k8,_k2));}),_B,_k4];}else{var _k9=_k6[1],_ka=(_k9=E(_k9))[1],_kb=(_ka=E(_ka))[1];if((_k7=E(_k7))[1]<_kb){var _kc=B(_jU(_jV>>1,_kb,_k9[2],_k6[2]));return [0,new T(function(){return B(_jF(_k7,_k8,_k2,_kc[1]));}),_kc[2],_kc[3]];}else{return [0,_k2,_B,_k3];}}}}},_kd=function(_ke,_kf,_kg){if(!(_kg=E(_kg))[0]){var _kh=_kg[2],_ki=_kg[3],_kj=_kg[4],_kk=_kg[5],_kl=(_kh=E(_kh))[1];if(_ke>=_kl){if(_ke!=_kl){return new F(function(){return _hO(_kh,_ki,_kj,B(_kd(_ke,_kf,_kk)));});}else{return [0,_kg[1],E([0,_ke]),_kf,E(_kj),E(_kk)];}}else{return new F(function(){return _iv(_kh,_ki,B(_kd(_ke,_kf,_kj)),_kk);});}}else{return [0,1,E([0,_ke]),_kf,E(_A),E(_A)];}},_km=function(_kn,_ko){while(1){if(!(_ko=E(_ko))[0]){return E(_kn);}else{var _kp=_ko[1],_kq=(_kp=E(_kp))[1],_kr=B(_kd((_kq=E(_kq))[1],_kp[2],_kn)),_ks=_ko[2];_kn=_kr;_ko=_ks;continue;}}},_kt=function(_ku,_kv,_kw,_kx){return new F(function(){return _km(B(_kd(_kv,_kw,_ku)),_kx);});},_ky=function(_kz,_kA,_kB){var _kC=(_kA=E(_kA))[1];return new F(function(){return _km(B(_kd((_kC=E(_kC))[1],_kA[2],_kz)),_kB);});},_kD=function(_kE,_kF,_kG){while(1){if(!(_kG=E(_kG))[0]){return E(_kF);}else{var _kH=_kG[1],_kI=_kG[2],_kJ=(_kH=E(_kH))[1],_kK=_kH[2];if(!(_kI=E(_kI))[0]){return new F(function(){return _in(_kJ,_kK,_kF);});}else{var _kL=_kI[1],_kM=(_kL=E(_kL))[1],_kN=(_kJ=E(_kJ))[1],_kO=(_kM=E(_kM))[1];if(_kN<_kO){var _kP=B(_jU(_kE,_kO,_kL[2],_kI[2])),_kQ=_kP[1],_kR=_kP[3];if(!(_kR=E(_kR))[0]){var _kS=_kE<<1,_kT=B(_jF(_kJ,_kK,_kF,_kQ));_kG=_kP[2];_kE=_kS;_kF=_kT;continue;}else{return new F(function(){return _ky(B(_jF(_kJ,_kK,_kF,_kQ)),_kR[1],_kR[2]);});}}else{return new F(function(){return _kt(_kF,_kN,_kK,_kI);});}}}}},_kU=function(_kV,_kW,_kX,_kY,_kZ){if(!(_kZ=E(_kZ))[0]){return new F(function(){return _in([0,_kX],_kY,_kW);});}else{var _l0=_kZ[1],_l1=(_l0=E(_l0))[1],_l2=(_l1=E(_l1))[1];if(_kX<_l2){var _l3=B(_jU(_kV,_l2,_l0[2],_kZ[2])),_l4=_l3[1],_l5=_l3[3];if(!(_l5=E(_l5))[0]){return new F(function(){return _kD(_kV<<1,B(_jF([0,_kX],_kY,_kW,_l4)),_l3[2]);});}else{return new F(function(){return _ky(B(_jF([0,_kX],_kY,_kW,_l4)),_l5[1],_l5[2]);});}}else{return new F(function(){return _kt(_kW,_kX,_kY,_kZ);});}}},_l6=function(_l7){if(!(_l7=E(_l7))[0]){return [1];}else{var _l8=_l7[1],_l9=_l7[2],_la=(_l8=E(_l8))[1],_lb=_l8[2];if(!(_l9=E(_l9))[0]){return [0,1,E(_la=E(_la)),_lb,E(_A),E(_A)];}else{var _lc=_l9[1],_ld=_l9[2],_le=(_lc=E(_lc))[1],_lf=_lc[2],_lg=(_le=E(_le))[1];if((_la=E(_la))[1]<_lg){return new F(function(){return _kU(1,[0,1,E(_la),_lb,E(_A),E(_A)],_lg,_lf,_ld);});}else{return new F(function(){return _kt([0,1,E(_la),_lb,E(_A),E(_A)],_lg,_lf,_ld);});}}}},_lh=function(_li,_lj){while(1){if(!(_li=E(_li))[0]){return E(_lj);}else{var _lk=_li[2],_ll=_lj+1|0;_li=_lk;_lj=_ll;continue;}}},_lm=function(_ln,_lo){while(1){if(!(_lo=E(_lo))[0]){var _lp=_lo[2],_lq=(_lp=E(_lp))[1];if(_ln>=_lq){if(_ln!=_lq){var _lr=_lo[5];_lo=_lr;continue;}else{return [1,_lo[3]];}}else{var _lr=_lo[4];_lo=_lr;continue;}}else{return [0];}}},_ls=[2],_lt=[1,_ls,_B],_lu=function(_lv){return (_lv>1)?[1,_ls,new T(function(){return B(_lu(_lv-1|0));})]:E(_lt);},_lw=function(_lx,_ly){return ((_lx=E(_lx))[0]==0)?[0]:((_ly=E(_ly))[0]==0)?[0]:[1,[0,_lx[1],_ly[1]],new T(function(){return B(_lw(_lx[2],_ly[2]));})];},_lz=function(_lA,_lB,_lC){var _lD=_lB-1|0;if(0<=_lD){var _lE=function(_lF){return [1,new T(function(){var _lG=B(_lm(_lF,new T(function(){return B(_l6(B(_lw(_lA,new T(function(){var _lH=B(_lh(_lA,0));if(B(_lh(_lC,0))!=_lH){if(_lH>0){return B(_lu(_lH));}else{return [0];}}else{return E(_lC);}},1)))));})));if(!_lG[0]){return [3];}else{return E(_lG[1]);}}),new T(function(){if(_lF!=_lD){return B(_lE(_lF+1|0));}else{return [0];}})];};return new F(function(){return _lE(0);});}else{return [0];}},_lI=function(_lJ,_lK){while(1){if(!(_lK=E(_lK))[0]){return false;}else{if(!B(A(_lJ,[_lK[1]]))){var _lL=_lK[2];_lK=_lL;continue;}else{return true;}}}},_lM=function(_lN){return ((_lN=E(_lN))[0]==2)?true:false;},_lO=function(_lP){return ((_lP=E(_lP))[0]==1)?true:false;},_lQ=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_lR=new T(function(){return B(err(_lQ));}),_lS=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_lT=new T(function(){return B(err(_lS));}),_lU=function(_lV,_lW){while(1){if(!(_lV=E(_lV))[0]){return E(_lT);}else{if(!(_lW=E(_lW))){return E(_lV[1]);}else{var _lX=_lV[2],_lY=_lW-1|0;_lV=_lX;_lW=_lY;continue;}}}},_lZ=function(_m0,_m1){return ((_m1=E(_m1))[0]==0)?[0]:[1,new T(function(){return B(A(_m0,[_m1[1]]));}),new T(function(){return B(_lZ(_m0,_m1[2]));})];},_m2=function(_m3,_m4){var _m5=function(_m6){var _m7=(_m6=E(_m6))[1];if(_m7>=B(_lh(_m4,0))){return [2];}else{if(_m7>=0){return new F(function(){return _lU(_m4,_m7);});}else{return E(_lR);}}};return new F(function(){return _lZ(_m5,_m3);});},_m8=function(_m9){return (_m9>1)?[1,_ls,new T(function(){return B(_m8(_m9-1|0));})]:E(_lt);},_ma=[1],_mb=[1,_ma,_B],_mc=function(_md){return (_md>1)?[1,_ma,new T(function(){return B(_mc(_md-1|0));})]:E(_mb);},_me=function(_mf,_mg,_mh,_mi,_mj){return new F(function(){return _lz(_mh,_mi,new T(function(){var _mk=B(_m2(_mg,_mj));if(!B(_lI(_lM,_mk))){if(!B(_lI(_lO,_mk))){return B(A(_mf,[_mk]));}else{if(_mi>0){return B(_mc(_mi));}else{return [0];}}}else{if(_mi>0){return B(_m8(_mi));}else{return [0];}}}));});},_ml=[0,0],_mm=function(_mn){return [0,_mn];},_mo=function(_mp,_mq){while(1){if(!(_mq=E(_mq))[0]){return true;}else{if(!B(A(_mp,[_mq[1]]))){return false;}else{var _mr=_mq[2];_mq=_mr;continue;}}}},_ms=function(_mt){return ((_mt=E(_mt))[0]==0)?true:false;},_mu=function(_mv){if(!(_mv=E(_mv))[0]){return E(_mv[1]);}else{return new F(function(){return _48("CoinKernel.hs:54:61-78|lambda");});}},_mw=function(_mx,_my){if(!(_mx=E(_mx))[0]){var _mz=_mx[1];return ((_my=E(_my))[0]==0)?_mz==_my[1]:(I_compareInt(_my[1],_mz)==0)?true:false;}else{var _mA=_mx[1];return ((_my=E(_my))[0]==0)?(I_compareInt(_mA,_my[1])==0)?true:false:(I_compare(_mA,_my[1])==0)?true:false;}},_mB=[0,0],_mC=function(_mD,_mE){while(1){if(!(_mD=E(_mD))[0]){return E(_mE);}else{var _mF=_mD[2],_mG=B(_6W(_mE,_mD[1]));_mD=_mF;_mE=_mG;continue;}}},_mH=function(_mI){return new F(function(){return _mC(_mI,_mB);});},_mJ=new T(function(){return B(err(_2Q));}),_mK=new T(function(){return B(err(_2S));}),_mL=function(_mM,_mN,_mO){var _mP=function(_mQ,_mR){var _mS=function(_mT){return new F(function(){return A(_mR,[new T(function(){return B(_75(_mT));})]);});},_mU=function(_mV){return new F(function(){return A(_mM,[_mV,_mQ,_mS]);});},_mW=function(_mX){return E(new T(function(){return B(_fj(_mU));}));},_mY=function(_mZ){return new F(function(){return A(_dA,[_mZ,_mW]);});},_n0=function(_n1){if((_n1=E(_n1))[0]==4){var _n2=_n1[1];if(!(_n2=E(_n2))[0]){return new F(function(){return A(_mM,[_n1,_mQ,_mR]);});}else{var _n3=_n2[1],_n4=_n2[2],_n5=(_n3=E(_n3))[1];if((_n5=E(_n5))==45){if(!(_n4=E(_n4))[0]){return E([1,_mY]);}else{return new F(function(){return A(_mM,[_n1,_mQ,_mR]);});}}else{return new F(function(){return A(_mM,[_n1,_mQ,_mR]);});}}}else{return new F(function(){return A(_mM,[_n1,_mQ,_mR]);});}},_n6=function(_n7){return E(new T(function(){return B(_fj(_n0));}));},_n8=function(_n9){return new F(function(){return A(_dA,[_n9,_n6]);});};return new F(function(){return _4j([1,_n8],new T(function(){return [1,B(_fV(_mP,_mR))];}));});};return new F(function(){return _mP(_mN,_mO);});},_na=function(_nb,_nc){return [2];},_nd=function(_ne){if((_ne=E(_ne))[0]==5){var _nf=B(_gD(_ne[1]));if(!_nf[0]){return E(_na);}else{var _ng=function(_nh,_ni){return new F(function(){return A(_ni,[_nf[1]]);});};return E(_ng);}}else{return E(_na);}},_nj=function(_nk,_nl){return new F(function(){return _mL(_nd,_nk,_nl);});},_nm=[0,91],_nn=[1,_nm,_B],_no=function(_np,_nq){var _nr=function(_ns,_nt){var _nu=function(_nv){if((_nv=E(_nv))[0]==2){var _nw=_nv[1];if(!(_nw=E(_nw))[0]){return [2];}else{var _nx=_nw[1],_ny=_nw[2],_nz=(_nx=E(_nx))[1];switch(_nz=E(_nz)){case 44:return ((_ny=E(_ny))[0]==0)?(!(_ns=E(_ns)))?[2]:E(new T(function(){var _nA=function(_nB){var _nC=function(_nD){return new F(function(){return A(_nt,[[1,_nB,_nD]]);});};return new F(function(){return _nr(_8v,_nC);});};return B(A(_np,[_fU,_nA]));})):[2];case 93:return ((_ny=E(_ny))[0]==0)?E(new T(function(){return B(A(_nt,[_B]));})):[2];default:return [2];}}}else{return [2];}},_nE=function(_nF){return E(new T(function(){return B(_fj(_nu));}));},_nG=function(_nH){return new F(function(){return A(_dA,[_nH,_nE]);});};return [1,_nG];},_nI=function(_nJ,_nK){return new F(function(){return _nL(_nK);});},_nL=function(_nM){var _nN=function(_nO){return E(new T(function(){var _nP=function(_nQ){return ((_nQ=E(_nQ))[0]==2)?(!B(_4M(_nQ[1],_nn)))?[2]:E(new T(function(){return B(_4j(B(_nr(_8u,_nM)),new T(function(){var _nR=function(_nS){var _nT=function(_nU){return new F(function(){return A(_nM,[[1,_nS,_nU]]);});};return new F(function(){return _nr(_8v,_nT);});};return B(A(_np,[_fU,_nR]));})));})):[2];};return B(_fj(_nP));}));},_nV=function(_nW){return new F(function(){return A(_dA,[_nW,_nN]);});};return new F(function(){return _4j([1,_nV],new T(function(){return [1,B(_fV(_nI,_nM))];}));});};return new F(function(){return _nL(_nq);});},_nX=new T(function(){return B(_no(_nj,_gO));}),_nY=function(_nZ,_o0){var _o1=B(_gV(B(_4b(_nX,_nZ))));if(!_o1[0]){return E(_mK);}else{var _o2=_o1[1],_o3=_o1[2];return ((_o3=E(_o3))[0]==0)?(!B(_mw(B(_mH(_o0)),B(_mH(_o2)))))?[0]:E(_o2):E(_mJ);}},_o4=function(_o5,_o6){if(!B(_mo(_ms,_o6))){return [0];}else{return new F(function(){return _lZ(_mm,B(_nY(_o5,B(_lZ(_mu,_o6)))));});}},_o7=[0,_ml,_o4],_o8=[1,_o7,_B],_o9=new T(function(){return B(_l6(_o8));}),_oa=function(_nk,_nl){return new F(function(){return _gb(_gI,_nk,_nl);});},_ob=function(_oc,_od){return new F(function(){return _no(_oa,_od);});},_oe=function(_of){return function(_5N){return new F(function(){return _4b(new T(function(){return B(_gb(_gI,_of,_5n));}),_5N);});};},_og=new T(function(){return B(_no(_oa,_5n));}),_oh=function(_nl){return new F(function(){return _4b(_og,_nl);});},_oi=[0,_oe,_oh,_oa,_ob],_oj=[0,44],_ok=[1,_oj,_B],_ol=function(_om){return E((_om=E(_om))[3]);},_on=function(_oo,_op,_oq){var _or=function(_os){var _ot=function(_ou){var _ov=function(_ow){return E(new T(function(){var _ox=function(_oy){return ((_oy=E(_oy))[0]==2)?(!B(_4M(_oy[1],_ok)))?[2]:E(new T(function(){var _oz=function(_oA){return new F(function(){return A(_os,[[0,_ou,_oA]]);});};return B(A(new T(function(){return B(_ol(_op));}),[_oq,_oz]));})):[2];};return B(_fj(_ox));}));},_oB=function(_oC){return new F(function(){return A(_dA,[_oC,_ov]);});};return [1,_oB];};return new F(function(){return A(new T(function(){return B(A(_ol,[_oo,_oq]));}),[_ot]);});};return E(_or);},_oD=function(_oE,_oF){var _oG=function(_oH,_oI){return new F(function(){return _oJ(_oI);});},_oJ=function(_oK){return new F(function(){return _4j([1,B(_fV(_oE,_oK))],new T(function(){return [1,B(_fV(_oG,_oK))];}));});};return new F(function(){return _oJ(_oF);});},_oL=function(_oM,_oN,_oO,_oP){var _oQ=function(_oR,_oS){var _oT=function(_oU){var _oV=function(_oW){return E(new T(function(){var _oX=function(_oY){return ((_oY=E(_oY))[0]==2)?(!B(_4M(_oY[1],_ok)))?[2]:E(new T(function(){var _oZ=function(_p0){return new F(function(){return A(_oS,[[0,(_oU=E(_oU))[1],_oU[2],_p0]]);});};return B(A(new T(function(){return B(_ol(_oO));}),[_oR,_oZ]));})):[2];};return B(_fj(_oX));}));},_p1=function(_p2){return new F(function(){return A(_dA,[_p2,_oV]);});};return [1,_p1];};return new F(function(){return A(_on,[_oM,_oN,_oR,_oT]);});};return new F(function(){return _oD(_oQ,_oP);});},_p3=function(_p4){return E((_p4=E(_p4))[4]);},_p5=function(_p6,_p7,_p8){return new F(function(){return _no(new T(function(){return B(_p3(_p6));}),_p8);});},_p9=function(_pa){return function(_5N){return new F(function(){return _4b(new T(function(){return B(_no(new T(function(){return B(_p3(_pa));}),_5n));}),_5N);});};},_pb=function(_pc,_pd){return function(_5N){return new F(function(){return _4b(new T(function(){return B(A(_p3,[_pc,_pd,_5n]));}),_5N);});};},_pe=function(_pf){var _pg=function(_nk,_nl){return new F(function(){return _p5(_pf,_nk,_nl);});},_ph=function(_nl){return new F(function(){return _pb(_pf,_nl);});};return [0,_ph,new T(function(){return B(_p9(_pf));}),new T(function(){return B(_p3(_pf));}),_pg];},_pi=new T(function(){return B(_pe(_oi));}),_pj=new T(function(){return B(_oL(_pi,_pi,_oi,_5n));}),_pk=new T(function(){return B(_gb(_gI,_fU,_5n));}),_pl=function(_pm){return [0];},_pn=function(_po,_pp){var _pq=B(_4b(_pk,_pp));if(!_pq[0]){return E(_pl);}else{var _pr=_pq[1],_ps=_pq[2];if(!(_ps=E(_ps))[0]){var _pt=(_pr=E(_pr))[1],_pu=B(_lm((_pt=E(_pt))[1],_po));if(!_pu[0]){return E(_pl);}else{return new F(function(){return A(_pu[1],[_pr[2]]);});}}else{return E(_pl);}}},_pv=function(_pw){var _px=B(_4b(_pj,_pw));if(!_px[0]){return E(_pl);}else{var _py=_px[1],_pz=_px[2];if(!(_pz=E(_pz))[0]){var _pA=(_py=E(_py))[1],_pB=function(_pC){var _pD=_pA[3];return new F(function(){return _me(new T(function(){return B(_pn(_o9,_py[2]));}),(_pA=E(_pA))[1],_pA[2],(_pD=E(_pD))[1],_pC);});};return E(_pB);}else{return E(_pl);}}},_pE=function(_pF,_pG){while(1){if(!(_pF=E(_pF))[0]){return ((_pG=E(_pG))[0]==0)?1:0;}else{var _pH=_pF[1];if(!(_pG=E(_pG))[0]){return 2;}else{var _pI=_pG[1],_pJ=(_pH=E(_pH))[1],_pK=(_pI=E(_pI))[1];if(_pJ!=_pK){return (_pJ>_pK)?2:0;}else{var _pL=_pF[2],_pM=_pG[2];_pF=_pL;_pG=_pM;continue;}}}}},_pN=function(_pO,_pP,_pQ,_pR,_pS){if((_pO=E(_pO))==1){if(!(_pS=E(_pS))[0]){return [0,[0,1,E([0,_pP,[0,_pQ]]),_pR,E(_A),E(_A)],_B,_B];}else{var _pT=_pS[1],_pU=(_pT=E(_pT))[1],_pV=_pU[2];switch(B(_pE(_pP,(_pU=E(_pU))[1]))){case 0:return [0,[0,1,E([0,_pP,[0,_pQ]]),_pR,E(_A),E(_A)],_pS,_B];case 1:return (_pQ<(_pV=E(_pV))[1])?[0,[0,1,E([0,_pP,[0,_pQ]]),_pR,E(_A),E(_A)],_pS,_B]:[0,[0,1,E([0,_pP,[0,_pQ]]),_pR,E(_A),E(_A)],_B,_pS];default:return [0,[0,1,E([0,_pP,[0,_pQ]]),_pR,E(_A),E(_A)],_B,_pS];}}}else{var _pW=B(_pN(_pO>>1,_pP,_pQ,_pR,_pS)),_pX=_pW[1],_pY=_pW[2],_pZ=_pW[3];if(!(_pY=E(_pY))[0]){return [0,_pX,_B,_pZ];}else{var _q0=_pY[1],_q1=_pY[2],_q2=(_q0=E(_q0))[1],_q3=_q0[2];if(!(_q1=E(_q1))[0]){return [0,new T(function(){return B(_in(_q2,_q3,_pX));}),_B,_pZ];}else{var _q4=_q1[1],_q5=_q1[2],_q6=(_q4=E(_q4))[1],_q7=_q4[2],_q8=_q2[2],_q9=(_q6=E(_q6))[1],_qa=_q6[2];switch(B(_pE((_q2=E(_q2))[1],_q9))){case 0:var _qb=B(_qc(_pO>>1,_q9,_qa,_q7,_q5));return [0,new T(function(){return B(_jF(_q2,_q3,_pX,_qb[1]));}),_qb[2],_qb[3]];case 1:var _qd=(_qa=E(_qa))[1];if((_q8=E(_q8))[1]<_qd){var _qe=B(_pN(_pO>>1,_q9,_qd,_q7,_q5));return [0,new T(function(){return B(_jF(_q2,_q3,_pX,_qe[1]));}),_qe[2],_qe[3]];}else{return [0,_pX,_B,_pY];}break;default:return [0,_pX,_B,_pY];}}}}},_qc=function(_qf,_qg,_qh,_qi,_qj){if((_qf=E(_qf))==1){if(!(_qj=E(_qj))[0]){return [0,[0,1,E([0,_qg,_qh]),_qi,E(_A),E(_A)],_B,_B];}else{var _qk=_qj[1],_ql=(_qk=E(_qk))[1],_qm=_ql[2];switch(B(_pE(_qg,(_ql=E(_ql))[1]))){case 0:return [0,[0,1,E([0,_qg,_qh]),_qi,E(_A),E(_A)],_qj,_B];case 1:return ((_qh=E(_qh))[1]<(_qm=E(_qm))[1])?[0,[0,1,E([0,_qg,_qh]),_qi,E(_A),E(_A)],_qj,_B]:[0,[0,1,E([0,_qg,_qh]),_qi,E(_A),E(_A)],_B,_qj];default:return [0,[0,1,E([0,_qg,_qh]),_qi,E(_A),E(_A)],_B,_qj];}}}else{var _qn=B(_qc(_qf>>1,_qg,_qh,_qi,_qj)),_qo=_qn[1],_qp=_qn[2],_qq=_qn[3];if(!(_qp=E(_qp))[0]){return [0,_qo,_B,_qq];}else{var _qr=_qp[1],_qs=_qp[2],_qt=(_qr=E(_qr))[1],_qu=_qr[2];if(!(_qs=E(_qs))[0]){return [0,new T(function(){return B(_in(_qt,_qu,_qo));}),_B,_qq];}else{var _qv=_qs[1],_qw=_qs[2],_qx=(_qv=E(_qv))[1],_qy=_qv[2],_qz=_qt[2],_qA=(_qx=E(_qx))[1],_qB=_qx[2];switch(B(_pE((_qt=E(_qt))[1],_qA))){case 0:var _qC=B(_qc(_qf>>1,_qA,_qB,_qy,_qw));return [0,new T(function(){return B(_jF(_qt,_qu,_qo,_qC[1]));}),_qC[2],_qC[3]];case 1:var _qD=(_qB=E(_qB))[1];if((_qz=E(_qz))[1]<_qD){var _qE=B(_pN(_qf>>1,_qA,_qD,_qy,_qw));return [0,new T(function(){return B(_jF(_qt,_qu,_qo,_qE[1]));}),_qE[2],_qE[3]];}else{return [0,_qo,_B,_qp];}break;default:return [0,_qo,_B,_qp];}}}}},_qF=function(_qG,_qH,_qI,_qJ){if(!(_qJ=E(_qJ))[0]){var _qK=_qJ[2],_qL=_qJ[3],_qM=_qJ[4],_qN=_qJ[5],_qO=_qK[2];switch(B(_pE(_qG,(_qK=E(_qK))[1]))){case 0:return new F(function(){return _iv(_qK,_qL,B(_qF(_qG,_qH,_qI,_qM)),_qN);});break;case 1:var _qP=(_qO=E(_qO))[1];if(_qH>=_qP){if(_qH!=_qP){return new F(function(){return _hO(_qK,_qL,_qM,B(_qF(_qG,_qH,_qI,_qN)));});}else{return [0,_qJ[1],E([0,_qG,[0,_qH]]),_qI,E(_qM),E(_qN)];}}else{return new F(function(){return _iv(_qK,_qL,B(_qF(_qG,_qH,_qI,_qM)),_qN);});}break;default:return new F(function(){return _hO(_qK,_qL,_qM,B(_qF(_qG,_qH,_qI,_qN)));});}}else{return [0,1,E([0,_qG,[0,_qH]]),_qI,E(_A),E(_A)];}},_qQ=function(_qR,_qS,_qT,_qU){if(!(_qU=E(_qU))[0]){var _qV=_qU[2],_qW=_qU[3],_qX=_qU[4],_qY=_qU[5],_qZ=_qV[2];switch(B(_pE(_qR,(_qV=E(_qV))[1]))){case 0:return new F(function(){return _iv(_qV,_qW,B(_qQ(_qR,_qS,_qT,_qX)),_qY);});break;case 1:var _r0=(_qS=E(_qS))[1],_r1=(_qZ=E(_qZ))[1];if(_r0>=_r1){if(_r0!=_r1){return new F(function(){return _hO(_qV,_qW,_qX,B(_qF(_qR,_r0,_qT,_qY)));});}else{return [0,_qU[1],E([0,_qR,_qS]),_qT,E(_qX),E(_qY)];}}else{return new F(function(){return _iv(_qV,_qW,B(_qF(_qR,_r0,_qT,_qX)),_qY);});}break;default:return new F(function(){return _hO(_qV,_qW,_qX,B(_qQ(_qR,_qS,_qT,_qY)));});}}else{return [0,1,E([0,_qR,_qS]),_qT,E(_A),E(_A)];}},_r2=function(_r3,_r4){while(1){if(!(_r4=E(_r4))[0]){return E(_r3);}else{var _r5=_r4[1],_r6=(_r5=E(_r5))[1],_r7=B(_qQ((_r6=E(_r6))[1],_r6[2],_r5[2],_r3)),_r8=_r4[2];_r3=_r7;_r4=_r8;continue;}}},_r9=function(_ra,_rb,_rc,_rd,_re){return new F(function(){return _r2(B(_qQ(_rb,_rc,_rd,_ra)),_re);});},_rf=function(_rg,_rh,_ri){var _rj=(_rh=E(_rh))[1];return new F(function(){return _r2(B(_qQ((_rj=E(_rj))[1],_rj[2],_rh[2],_rg)),_ri);});},_rk=function(_rl,_rm,_rn){if(!(_rn=E(_rn))[0]){return E(_rm);}else{var _ro=_rn[1],_rp=_rn[2],_rq=(_ro=E(_ro))[1],_rr=_ro[2];if(!(_rp=E(_rp))[0]){return new F(function(){return _in(_rq,_rr,_rm);});}else{var _rs=_rp[1],_rt=(_rs=E(_rs))[1],_ru=(_rq=E(_rq))[1],_rv=_rq[2],_rw=(_rt=E(_rt))[1],_rx=_rt[2],_ry=function(_rz){var _rA=B(_qc(_rl,_rw,_rx,_rs[2],_rp[2])),_rB=_rA[1],_rC=_rA[3];if(!(_rC=E(_rC))[0]){return new F(function(){return _rk(_rl<<1,B(_jF(_rq,_rr,_rm,_rB)),_rA[2]);});}else{return new F(function(){return _rf(B(_jF(_rq,_rr,_rm,_rB)),_rC[1],_rC[2]);});}};switch(B(_pE(_ru,_rw))){case 0:return new F(function(){return _ry(_);});break;case 1:if((_rv=E(_rv))[1]<(_rx=E(_rx))[1]){return new F(function(){return _ry(_);});}else{return new F(function(){return _r9(_rm,_ru,_rv,_rr,_rp);});}break;default:return new F(function(){return _r9(_rm,_ru,_rv,_rr,_rp);});}}}},_rD=function(_rE,_rF,_rG,_rH,_rI,_rJ){if(!(_rJ=E(_rJ))[0]){return new F(function(){return _in([0,_rG,_rH],_rI,_rF);});}else{var _rK=_rJ[1],_rL=(_rK=E(_rK))[1],_rM=(_rL=E(_rL))[1],_rN=_rL[2],_rO=function(_rP){var _rQ=B(_qc(_rE,_rM,_rN,_rK[2],_rJ[2])),_rR=_rQ[1],_rS=_rQ[3];if(!(_rS=E(_rS))[0]){return new F(function(){return _rk(_rE<<1,B(_jF([0,_rG,_rH],_rI,_rF,_rR)),_rQ[2]);});}else{return new F(function(){return _rf(B(_jF([0,_rG,_rH],_rI,_rF,_rR)),_rS[1],_rS[2]);});}};switch(B(_pE(_rG,_rM))){case 0:return new F(function(){return _rO(_);});break;case 1:if((_rH=E(_rH))[1]<(_rN=E(_rN))[1]){return new F(function(){return _rO(_);});}else{return new F(function(){return _r9(_rF,_rG,_rH,_rI,_rJ);});}break;default:return new F(function(){return _r9(_rF,_rG,_rH,_rI,_rJ);});}}},_rT=function(_rU,_rV,_rW,_rX,_rY,_rZ){if(!(_rZ=E(_rZ))[0]){return new F(function(){return _in([0,_rW,[0,_rX]],_rY,_rV);});}else{var _s0=_rZ[1],_s1=(_s0=E(_s0))[1],_s2=(_s1=E(_s1))[1],_s3=_s1[2],_s4=function(_s5){var _s6=B(_qc(_rU,_s2,_s3,_s0[2],_rZ[2])),_s7=_s6[1],_s8=_s6[3];if(!(_s8=E(_s8))[0]){return new F(function(){return _rk(_rU<<1,B(_jF([0,_rW,[0,_rX]],_rY,_rV,_s7)),_s6[2]);});}else{return new F(function(){return _rf(B(_jF([0,_rW,[0,_rX]],_rY,_rV,_s7)),_s8[1],_s8[2]);});}};switch(B(_pE(_rW,_s2))){case 0:return new F(function(){return _s4(_);});break;case 1:if(_rX<(_s3=E(_s3))[1]){return new F(function(){return _s4(_);});}else{return new F(function(){return _r9(_rV,_rW,[0,_rX],_rY,_rZ);});}break;default:return new F(function(){return _r9(_rV,_rW,[0,_rX],_rY,_rZ);});}}},_s9=function(_sa){if(!(_sa=E(_sa))[0]){return [1];}else{var _sb=_sa[1],_sc=_sa[2],_sd=(_sb=E(_sb))[1],_se=_sb[2];if(!(_sc=E(_sc))[0]){return [0,1,E(_sd=E(_sd)),_se,E(_A),E(_A)];}else{var _sf=_sc[1],_sg=_sc[2],_sh=(_sf=E(_sf))[1],_si=_sf[2],_sj=_sd[2],_sk=(_sh=E(_sh))[1],_sl=_sh[2];switch(B(_pE((_sd=E(_sd))[1],_sk))){case 0:return new F(function(){return _rD(1,[0,1,E(_sd),_se,E(_A),E(_A)],_sk,_sl,_si,_sg);});break;case 1:var _sm=(_sl=E(_sl))[1];if((_sj=E(_sj))[1]<_sm){return new F(function(){return _rT(1,[0,1,E(_sd),_se,E(_A),E(_A)],_sk,_sm,_si,_sg);});}else{return new F(function(){return _r9([0,1,E(_sd),_se,E(_A),E(_A)],_sk,_sl,_si,_sg);});}break;default:return new F(function(){return _r9([0,1,E(_sd),_se,E(_A),E(_A)],_sk,_sl,_si,_sg);});}}}},_sn=function(_so){return E((_so=E(_so))[3]);},_sp=function(_sq,_sr,_ss,_st,_su,_sv){switch(B(A(_sq,[_ss,_su]))){case 0:return true;case 1:return new F(function(){return A(_sn,[_sr,_st,_sv]);});break;default:return false;}},_sw=function(_sx,_sy,_sz,_sA,_sB){return new F(function(){return _sp((_sy=E(_sy))[2],_sz,(_sA=E(_sA))[1],_sA[2],(_sB=E(_sB))[1],_sB[2]);});},_sC=function(_sD){return E((_sD=E(_sD))[6]);},_sE=function(_sF,_sG,_sH,_sI,_sJ,_sK){switch(B(A(_sF,[_sH,_sJ]))){case 0:return true;case 1:return new F(function(){return A(_sC,[_sG,_sI,_sK]);});break;default:return false;}},_sL=function(_sM,_sN,_sO,_sP,_sQ){return new F(function(){return _sE((_sN=E(_sN))[2],_sO,(_sP=E(_sP))[1],_sP[2],(_sQ=E(_sQ))[1],_sQ[2]);});},_sR=function(_sS){return E((_sS=E(_sS))[5]);},_sT=function(_sU,_sV,_sW,_sX,_sY,_sZ){switch(B(A(_sU,[_sW,_sY]))){case 0:return false;case 1:return new F(function(){return A(_sR,[_sV,_sX,_sZ]);});break;default:return true;}},_t0=function(_t1,_t2,_t3,_t4,_t5){return new F(function(){return _sT((_t2=E(_t2))[2],_t3,(_t4=E(_t4))[1],_t4[2],(_t5=E(_t5))[1],_t5[2]);});},_t6=function(_t7){return E((_t7=E(_t7))[4]);},_t8=function(_t9,_ta,_tb,_tc,_td,_te){switch(B(A(_t9,[_tb,_td]))){case 0:return false;case 1:return new F(function(){return A(_t6,[_ta,_tc,_te]);});break;default:return true;}},_tf=function(_tg,_th,_ti,_tj,_tk){return new F(function(){return _t8((_th=E(_th))[2],_ti,(_tj=E(_tj))[1],_tj[2],(_tk=E(_tk))[1],_tk[2]);});},_tl=function(_tm){return E((_tm=E(_tm))[2]);},_tn=function(_to,_tp,_tq,_tr,_ts,_tt){switch(B(A(_to,[_tq,_ts]))){case 0:return 0;case 1:return new F(function(){return A(_tl,[_tp,_tr,_tt]);});break;default:return 2;}},_tu=function(_tv,_tw,_tx,_ty,_tz){return new F(function(){return _tn((_tw=E(_tw))[2],_tx,(_ty=E(_ty))[1],_ty[2],(_tz=E(_tz))[1],_tz[2]);});},_tA=function(_tB,_tC,_tD,_tE,_tF){var _tG=(_tE=E(_tE))[1],_tH=_tE[2],_tI=(_tF=E(_tF))[1],_tJ=_tF[2];switch(B(A((_tC=E(_tC))[2],[_tG,_tI]))){case 0:return [0,_tI,_tJ];case 1:return (!B(A(_sC,[_tD,_tH,_tJ])))?[0,_tG,_tH]:[0,_tI,_tJ];default:return [0,_tG,_tH];}},_tK=function(_tL,_tM,_tN,_tO,_tP){var _tQ=(_tO=E(_tO))[1],_tR=_tO[2],_tS=(_tP=E(_tP))[1],_tT=_tP[2];switch(B(A((_tM=E(_tM))[2],[_tQ,_tS]))){case 0:return [0,_tQ,_tR];case 1:return (!B(A(_sC,[_tN,_tR,_tT])))?[0,_tS,_tT]:[0,_tQ,_tR];default:return [0,_tS,_tT];}},_tU=function(_tV,_tW,_tX){var _tY=function(_tZ,_u0){return new F(function(){return _tK(_tV,_tW,_tX,_tZ,_u0);});},_u1=function(_tZ,_u0){return new F(function(){return _tA(_tV,_tW,_tX,_tZ,_u0);});},_u2=function(_tZ,_u0){return new F(function(){return _sL(_tV,_tW,_tX,_tZ,_u0);});},_u3=function(_tZ,_u0){return new F(function(){return _t0(_tV,_tW,_tX,_tZ,_u0);});},_u4=function(_tZ,_u0){return new F(function(){return _tf(_tV,_tW,_tX,_tZ,_u0);});},_u5=function(_tZ,_u0){return new F(function(){return _sw(_tV,_tW,_tX,_tZ,_u0);});},_u6=function(_tZ,_u0){return new F(function(){return _tu(_tV,_tW,_tX,_tZ,_u0);});};return [0,_tV,_u6,_u5,_u4,_u3,_u2,_u1,_tY];},_u7=function(_u8,_u9){return (_u8=E(_u8))[1]==(_u9=E(_u9))[1];},_ua=function(_ub,_uc){return (_ub=E(_ub))[1]!=(_uc=E(_uc))[1];},_ud=[0,_u7,_ua],_ue=function(_uf,_ug){return ((_uf=E(_uf))[1]>(_ug=E(_ug))[1])?E(_uf):E(_ug);},_uh=function(_ui,_uj){return ((_ui=E(_ui))[1]>(_uj=E(_uj))[1])?E(_uj):E(_ui);},_uk=function(_ul,_um){return (_ul>=_um)?(_ul!=_um)?2:1:0;},_un=function(_uo,_up){return new F(function(){return _uk((_uo=E(_uo))[1],(_up=E(_up))[1]);});},_uq=function(_ur,_us){return (_ur=E(_ur))[1]>=(_us=E(_us))[1];},_ut=function(_uu,_uv){return (_uu=E(_uu))[1]>(_uv=E(_uv))[1];},_uw=function(_ux,_uy){return (_ux=E(_ux))[1]<=(_uy=E(_uy))[1];},_uz=function(_uA,_uB){return (_uA=E(_uA))[1]<(_uB=E(_uB))[1];},_uC=[0,_ud,_un,_uz,_uq,_ut,_uw,_ue,_uh],_uD=function(_uE,_uF){return (B(_pE(_uE,_uF))==0)?true:false;},_uG=function(_uH,_uI){return (B(_pE(_uH,_uI))==2)?false:true;},_uJ=function(_uK,_uL){return (B(_pE(_uK,_uL))==2)?true:false;},_uM=function(_uN,_uO){return (B(_pE(_uN,_uO))==0)?false:true;},_uP=function(_uQ,_uR){return (B(_pE(_uQ,_uR))==2)?E(_uQ):E(_uR);},_uS=function(_uT,_uU){return (B(_pE(_uT,_uU))==2)?E(_uU):E(_uT);},_uV=[0,_5a,_pE,_uD,_uM,_uJ,_uG,_uP,_uS],_uW=function(_uX,_uY,_uZ,_v0,_v1,_v2){return (!B(A(_uX,[_uZ,_v1])))?true:(!B(A(_1H,[_uY,_v0,_v2])))?true:false;},_v3=function(_v4,_v5,_v6,_v7){return new F(function(){return _uW((_v4=E(_v4))[1],_v5,(_v6=E(_v6))[1],_v6[2],(_v7=E(_v7))[1],_v7[2]);});},_v8=function(_v9,_va,_vb,_vc,_vd,_ve){if(!B(A(_v9,[_vb,_vd]))){return false;}else{return new F(function(){return A(_1H,[_va,_vc,_ve]);});}},_vf=function(_vg,_vh,_vi,_vj){return new F(function(){return _v8((_vg=E(_vg))[1],_vh,(_vi=E(_vi))[1],_vi[2],(_vj=E(_vj))[1],_vj[2]);});},_vk=function(_vl,_vm){var _vn=function(_tZ,_u0){return new F(function(){return _v3(_vl,_vm,_tZ,_u0);});},_vo=function(_tZ,_u0){return new F(function(){return _vf(_vl,_vm,_tZ,_u0);});};return [0,_vo,_vn];},_vp=new T(function(){return B(_vk(_5a,_ud));}),_vq=new T(function(){return B(_tU(_vp,_uV,_uC));}),_vr=[0],_vs=function(_vt,_vu,_vv){if(!(_vu=E(_vu))[0]){return E(_vv);}else{var _vw=function(_vx,_vy){while(1){if(!(_vy=E(_vy))[0]){var _vz=_vy[2],_vA=_vy[5];switch(B(A(new T(function(){return B(_tl(_vt));}),[_vx,_vz]))){case 0:return new F(function(){return _jF(_vz,_vy[3],B(_vw(_vx,_vy[4])),_vA);});break;case 1:return E(_vA);default:_vy=_vA;continue;}}else{return [1];}}};return new F(function(){return _vw(_vu[1],_vv);});}},_vB=function(_vC,_vD,_vE){if(!(_vD=E(_vD))[0]){return E(_vE);}else{var _vF=function(_vG,_vH){while(1){if(!(_vH=E(_vH))[0]){var _vI=_vH[2],_vJ=_vH[4];switch(B(A(new T(function(){return B(_tl(_vC));}),[_vI,_vG]))){case 0:return new F(function(){return _jF(_vI,_vH[3],_vJ,B(_vF(_vG,_vH[5])));});break;case 1:return E(_vJ);default:_vH=_vJ;continue;}}else{return [1];}}};return new F(function(){return _vF(_vD[1],_vE);});}},_vK=function(_vL,_vM,_vN,_vO){if(!(_vO=E(_vO))[0]){var _vP=_vO[2],_vQ=_vO[3],_vR=_vO[4],_vS=_vO[5];switch(B(A(_tl,[_vL,_vM,_vP]))){case 0:return new F(function(){return _iv(_vP,_vQ,B(_vK(_vL,_vM,_vN,_vR)),_vS);});break;case 1:return E(_vO);default:return new F(function(){return _hO(_vP,_vQ,_vR,B(_vK(_vL,_vM,_vN,_vS)));});}}else{return [0,1,E(_vM=E(_vM)),_vN,E(_A),E(_A)];}},_vT=function(_vU,_vV,_vW,_vX){return new F(function(){return _vK(_vU,_vV,_vW,_vX);});},_vY=function(_vZ,_w0,_w1,_w2){var _w3=new T(function(){return B(_t6(_vZ));}),_w4=new T(function(){return B(_sC(_vZ));});if(!(_w0=E(_w0))[0]){if(!(_w1=E(_w1))[0]){return E(_w2);}else{var _w5=function(_w6,_w7){while(1){if(!(_w7=E(_w7))[0]){if(!B(A(_w3,[_w7[2],_w6]))){return E(_w7);}else{var _w8=_w7[4];_w7=_w8;continue;}}else{return [1];}}};return new F(function(){return _w5(_w1[1],_w2);});}}else{var _w9=_w0[1];if(!(_w1=E(_w1))[0]){var _wa=function(_wb,_wc){while(1){if(!(_wc=E(_wc))[0]){if(!B(A(_w4,[_wc[2],_wb]))){return E(_wc);}else{var _wd=_wc[5];_wc=_wd;continue;}}else{return [1];}}};return new F(function(){return _wa(_w9,_w2);});}else{var _we=function(_wf,_wg,_wh){while(1){if(!(_wh=E(_wh))[0]){var _wi=_wh[2];if(!B(A(_w4,[_wi,_wf]))){if(!B(A(_w3,[_wi,_wg]))){return E(_wh);}else{var _wj=_wh[4];_wh=_wj;continue;}}else{var _wj=_wh[5];_wh=_wj;continue;}}else{return [1];}}};return new F(function(){return _we(_w9,_w1[1],_w2);});}}},_wk=function(_wl,_wm,_wn,_wo,_wp){if(!(_wp=E(_wp))[0]){var _wq=_wp[2],_wr=_wp[3],_ws=_wp[4],_wt=_wp[5];if(!(_wo=E(_wo))[0]){var _wu=_wo[2],_wv=function(_ww){var _wx=[1,E(_wu)];return new F(function(){return _jF(_wu,_wo[3],B(_wk(_wl,_wm,_wx,_wo[4],B(_vY(_wl,_wm,_wx,_wp)))),B(_wk(_wl,_wx,_wn,_wo[5],B(_vY(_wl,_wx,_wn,_wp)))));});};if(!(_ws=E(_ws))[0]){return new F(function(){return _wv(_);});}else{if(!(_wt=E(_wt))[0]){return new F(function(){return _wv(_);});}else{return new F(function(){return _vT(_wl,_wq,_wr,_wo);});}}}else{return new F(function(){return _jF(_wq,_wr,B(_vs(_wl,_wm,_ws)),B(_vB(_wl,_wn,_wt)));});}}else{return E(_wo);}},_wy=function(_wz,_wA,_wB,_wC,_wD,_wE,_wF,_wG,_wH,_wI,_wJ,_wK,_wL){var _wM=function(_wN){var _wO=[1,E(_wD)];return new F(function(){return _jF(_wD,_wE,B(_wk(_wz,_wA,_wO,_wF,B(_vY(_wz,_wA,_wO,[0,_wH,E(_wI),_wJ,E(_wK),E(_wL)])))),B(_wk(_wz,_wO,_wB,_wG,B(_vY(_wz,_wO,_wB,[0,_wH,E(_wI),_wJ,E(_wK),E(_wL)])))));});};if(!(_wK=E(_wK))[0]){return new F(function(){return _wM(_);});}else{if(!(_wL=E(_wL))[0]){return new F(function(){return _wM(_);});}else{return new F(function(){return _vT(_wz,_wI,_wJ,[0,_wC,E(_wD),_wE,E(_wF),E(_wG)]);});}}},_wP=function(_wQ,_wR){if(!(_wQ=E(_wQ))[0]){if(!(_wR=E(_wR))[0]){return new F(function(){return _wy(_vq,_vr,_vr,_wQ[1],_wQ[2],_wQ[3],_wQ[4],_wQ[5],_wR[1],_wR[2],_wR[3],_wR[4],_wR[5]);});}else{return E(_wQ);}}else{return E(_wR);}},_wS=function(_wT,_wU,_wV){while(1){if(!(_wV=E(_wV))[0]){var _wW=_wV[2],_wX=_wV[4],_wY=_wV[5],_wZ=_wW[2];switch(B(_pE(_wT,(_wW=E(_wW))[1]))){case 0:_wV=_wX;continue;case 1:var _x0=(_wZ=E(_wZ))[1];if(_wU>=_x0){if(_wU!=_x0){_wV=_wY;continue;}else{return [1,_wV[3]];}}else{_wV=_wX;continue;}break;default:_wV=_wY;continue;}}else{return [0];}}},_x1=function(_x2,_x3,_x4){while(1){if(!(_x4=E(_x4))[0]){var _x5=_x4[2],_x6=_x4[4],_x7=_x4[5],_x8=_x5[2];switch(B(_pE(_x2,(_x5=E(_x5))[1]))){case 0:_x4=_x6;continue;case 1:var _x9=(_x3=E(_x3))[1],_xa=(_x8=E(_x8))[1];if(_x9>=_xa){if(_x9!=_xa){return new F(function(){return _wS(_x2,_x9,_x7);});}else{return [1,_x4[3]];}}else{return new F(function(){return _wS(_x2,_x9,_x6);});}break;default:_x4=_x7;continue;}}else{return [0];}}},_xb=function(_xc,_xd){if(_xc<=_xd){var _xe=function(_xf){return [1,[0,_xf],new T(function(){if(_xf!=_xd){return B(_xe(_xf+1|0));}else{return [0];}})];};return new F(function(){return _xe(_xc);});}else{return [0];}},_xg=new T(function(){return B(_xb(0,2147483647));}),_xh=function(_xi,_xj,_xk){var _xl=function(_xm,_xn){while(1){var _xo=(function(_xp,_xq){if(!(_xp=E(_xp))[0]){return [0];}else{var _xr=_xp[2];if(!(_xq=E(_xq))[0]){return [0];}else{var _xs=_xq[1],_xt=_xq[2];if((_xs=E(_xs))[0]==1){_xm=_xr;_xn=_xt;return null;}else{return [1,[0,[0,new T(function(){return E((_xj=E(_xj))[3]);}),_xp[1]],_xs],new T(function(){return B(_xl(_xr,_xt));})];}}}})(_xm,_xn);if(_xo!=null){return _xo;}}};return new F(function(){return _wP(_xk,B(_s9(B(_xl(_xg,new T(function(){return B(A(_xi,[new T(function(){return E((_xj=E(_xj))[1]);}),new T(function(){var _xu=function(_xv){var _xw=B(_x1((_xv=E(_xv))[1],_xv[2],_xk));return (_xw[0]==0)?[1]:E(_xw[1]);};return B(_lZ(_xu,(_xj=E(_xj))[2]));})]));},1))))));});},_xx=function(_xy,_xz){while(1){if(!(_xz=E(_xz))[0]){return E(_xy);}else{var _xA=B(_xh(_pv,_xz[1],_xy)),_xB=_xz[2];_xy=_xA;_xz=_xB;continue;}}},_xC=function(_xD,_){return new T(function(){return B(_1k(_B,B(_xx(_A,B(_hp(_B,_xD))))));});},_xE=new T(function(){return [0,"runCoinKernelOnGraph"];}),_xF=function(_xG){var _xH=String(_xG),_xI=_xH;return [0,_xI];},_xJ=new T(function(){return B(_5("lst2arr"));}),_xK=new T(function(){return [0,"arr2lst"];}),_xL=function(_xM,_xN){var _xO=_xN,_xP=function(_){var _=0;return new F(function(){return A(_5,[(_xK=E(_xK))[1],_xM=E(_xM),_xO=E(_xO),_]);});};return new F(function(){return _1(_xP);});},_xQ=function(_xR){return E((_xR=E(_xR))[1]);},_xS=function(_xT){var _xU=function(_){var _=0,_xV=function(_xW){var _xX=function(_){var _=0,_xY=B(A(_xT,[B(_lZ(_xF,B(_xL(_xW,0)))),_])),_xZ=_xY,_y0=function(_){var _=0;return new F(function(){return A(_xJ,[B(_lZ(_xQ,_xZ)),_]);});};return new F(function(){return _1(_y0);});};return new F(function(){return _1(_xX);});};return new F(function(){return _m(_xV,_);});};return new F(function(){return _1(_xU);});},_y1=new T(function(){return B(_a(_xS,_xE));}),_y2=function(_y3,_){return _y3;},_y4=function(_){var _y5=B(A(_y1,[_xC,_])),_y6=_y5;return new F(function(){return A(_z,[_y2,_]);});},_y7=function(_){return new F(function(){return _y4(_);});};
var hasteMain = function() {B(A(_y7, [0]));};hasteMain(); init();