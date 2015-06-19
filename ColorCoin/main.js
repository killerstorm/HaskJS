function getHaste() {
    return Haste
}

module.exports = {
    getHaste                 : getHaste
}

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
    throw err;
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

function jsGetMouseCoords(e) {
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

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;
    case 'wheel':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            var mdx = [0,x.deltaX];
            var mdy = [0,x.deltaY];
            var mdz = [0,x.deltaZ];
            B(A(cb,[[0,mx,my],[0,mdx,mdy,mdz],0]));
        };
        break;
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
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

var jsJSONParse = JSON.parse;

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

var _0=[0,44],_1=function(_2,_3,_4){return new F(function(){return A(_2,[[1,_0,new T(function(){return B(A(_3,[_4]));})]]);});},_5=[0,93],_6=[0,91],_7=function(_8,_9,_a){var _b=E(_9);return _b[0]==0?B(unAppCStr("[]",_a)):[1,_6,new T(function(){return B(A(_8,[_b[1],new T(function(){var _c=function(_d){var _e=E(_d);return _e[0]==0?E([1,_5,_a]):[1,_0,new T(function(){return B(A(_8,[_e[1],new T(function(){return B(_c(_e[2]));})]));})];};return B(_c(_b[2]));})]));})];},_f=function(_g,_h){var _i=E(_g);return _i[0]==0?E(_h):[1,_i[1],new T(function(){return B(_f(_i[2],_h));})];},_j=function(_k,_l){var _m=jsShowI(_k),_n=_m;return new F(function(){return _f(fromJSStr(_n),_l);});},_o=[0,41],_p=[0,40],_q=function(_r,_s,_t){if(_s>=0){return new F(function(){return _j(_s,_t);});}else{return _r<=6?B(_j(_s,_t)):[1,_p,new T(function(){var _u=jsShowI(_s),_v=_u;return B(_f(fromJSStr(_v),[1,_o,_t]));})];}},_w=function(_x,_y){return new F(function(){return _q(0,E(_x)[1],_y);});},_z=function(_A,_B){return new F(function(){return _7(_w,_A,_B);});},_C=[0],_D=new T(function(){return B(unCStr(": empty list"));}),_E=new T(function(){return B(unCStr("Prelude."));}),_F=function(_G){return new F(function(){return err(B(_f(_E,new T(function(){return B(_f(_G,_D));},1))));});},_H=new T(function(){return B(unCStr("foldr1"));}),_I=new T(function(){return B(_F(_H));}),_J=function(_K,_L){var _M=E(_L);if(!_M[0]){return E(_I);}else{var _N=_M[1],_O=E(_M[2]);if(!_O[0]){return E(_N);}else{return new F(function(){return A(_K,[_N,new T(function(){return B(_J(_K,_O));})]);});}}},_P=[1,_o,_C],_Q=new T(function(){return B(unCStr("Nothing"));}),_R=[2],_S=function(_T){return [3,_T,_R];},_U=new T(function(){return B(unCStr("Control.Exception.Base"));}),_V=new T(function(){return B(unCStr("base"));}),_W=new T(function(){return B(unCStr("PatternMatchFail"));}),_X=new T(function(){var _Y=hs_wordToWord64(18445595),_Z=_Y,_10=hs_wordToWord64(52003073),_11=_10;return [0,_Z,_11,[0,_Z,_11,_V,_U,_W],_C];}),_12=function(_13){return E(_X);},_14=function(_15){return E(E(_15)[1]);},_16=function(_17,_18,_19){var _1a=B(A(_17,[_])),_1b=B(A(_18,[_])),_1c=hs_eqWord64(_1a[1],_1b[1]),_1d=_1c;if(!E(_1d)){return [0];}else{var _1e=hs_eqWord64(_1a[2],_1b[2]),_1f=_1e;return E(_1f)==0?[0]:[1,_19];}},_1g=function(_1h){var _1i=E(_1h);return new F(function(){return _16(B(_14(_1i[1])),_12,_1i[2]);});},_1j=function(_1k){return E(E(_1k)[1]);},_1l=function(_1m,_1n){return new F(function(){return _f(E(_1m)[1],_1n);});},_1o=function(_1p,_1q){return new F(function(){return _7(_1l,_1p,_1q);});},_1r=function(_1s,_1t,_1u){return new F(function(){return _f(E(_1t)[1],_1u);});},_1v=[0,_1r,_1j,_1o],_1w=new T(function(){return [0,_12,_1v,_1x,_1g];}),_1x=function(_1y){return [0,_1w,_1y];},_1z=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_1A=function(_1B,_1C){return new F(function(){return die(new T(function(){return B(A(_1C,[_1B]));}));});},_1D=function(_1E,_1F){var _1G=E(_1F);if(!_1G[0]){return [0,_C,_C];}else{var _1H=_1G[1];if(!B(A(_1E,[_1H]))){return [0,_C,_1G];}else{var _1I=new T(function(){var _1J=B(_1D(_1E,_1G[2]));return [0,_1J[1],_1J[2]];});return [0,[1,_1H,new T(function(){return E(E(_1I)[1]);})],new T(function(){return E(E(_1I)[2]);})];}}},_1K=[0,32],_1L=[0,10],_1M=[1,_1L,_C],_1N=function(_1O){return E(E(_1O)[1])==124?false:true;},_1P=function(_1Q,_1R){var _1S=B(_1D(_1N,B(unCStr(_1Q)))),_1T=_1S[1],_1U=function(_1V,_1W){return new F(function(){return _f(_1V,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_f(_1R,new T(function(){return B(_f(_1W,_1M));},1)));})));},1));});},_1X=E(_1S[2]);if(!_1X[0]){return new F(function(){return _1U(_1T,_C);});}else{return E(E(_1X[1])[1])==124?B(_1U(_1T,[1,_1K,_1X[2]])):B(_1U(_1T,_C));}},_1Y=function(_1Z){return new F(function(){return _1A([0,new T(function(){return B(_1P(_1Z,_1z));})],_1x);});},_20=new T(function(){return B(_1Y("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_21=function(_22,_23){while(1){var _24=(function(_25,_26){var _27=E(_25);switch(_27[0]){case 0:var _28=E(_26);if(!_28[0]){return [0];}else{_22=B(A(_27[1],[_28[1]]));_23=_28[2];return null;}break;case 1:var _29=B(A(_27[1],[_26])),_2a=_26;_22=_29;_23=_2a;return null;case 2:return [0];case 3:return [1,[0,_27[1],_26],new T(function(){return B(_21(_27[2],_26));})];default:return E(_27[1]);}})(_22,_23);if(_24!=null){return _24;}}},_2b=function(_2c,_2d){var _2e=function(_2f){var _2g=E(_2d);if(_2g[0]==3){return [3,_2g[1],new T(function(){return B(_2b(_2c,_2g[2]));})];}else{var _2h=E(_2c);if(_2h[0]==2){return E(_2g);}else{var _2i=E(_2g);if(_2i[0]==2){return E(_2h);}else{var _2j=function(_2k){var _2l=E(_2i);if(_2l[0]==4){return [1,function(_2m){return [4,new T(function(){return B(_f(B(_21(_2h,_2m)),_2l[1]));})];}];}else{var _2n=E(_2h);if(_2n[0]==1){var _2o=_2n[1],_2p=E(_2l);return _2p[0]==0?[1,function(_2q){return new F(function(){return _2b(B(A(_2o,[_2q])),_2p);});}]:[1,function(_2r){return new F(function(){return _2b(B(A(_2o,[_2r])),new T(function(){return B(A(_2p[1],[_2r]));}));});}];}else{var _2s=E(_2l);return _2s[0]==0?E(_20):[1,function(_2t){return new F(function(){return _2b(_2n,new T(function(){return B(A(_2s[1],[_2t]));}));});}];}}},_2u=E(_2h);switch(_2u[0]){case 1:var _2v=E(_2i);if(_2v[0]==4){return [1,function(_2w){return [4,new T(function(){return B(_f(B(_21(B(A(_2u[1],[_2w])),_2w)),_2v[1]));})];}];}else{return new F(function(){return _2j(_);});}break;case 4:var _2x=_2u[1],_2y=E(_2i);switch(_2y[0]){case 0:return [1,function(_2z){return [4,new T(function(){return B(_f(_2x,new T(function(){return B(_21(_2y,_2z));},1)));})];}];case 1:return [1,function(_2A){return [4,new T(function(){return B(_f(_2x,new T(function(){return B(_21(B(A(_2y[1],[_2A])),_2A));},1)));})];}];default:return [4,new T(function(){return B(_f(_2x,_2y[1]));})];}break;default:return new F(function(){return _2j(_);});}}}}},_2B=E(_2c);switch(_2B[0]){case 0:var _2C=E(_2d);if(!_2C[0]){return [0,function(_2D){return new F(function(){return _2b(B(A(_2B[1],[_2D])),new T(function(){return B(A(_2C[1],[_2D]));}));});}];}else{return new F(function(){return _2e(_);});}break;case 3:return [3,_2B[1],new T(function(){return B(_2b(_2B[2],_2d));})];default:return new F(function(){return _2e(_);});}},_2E=[0,41],_2F=[1,_2E,_C],_2G=[0,40],_2H=[1,_2G,_C],_2I=function(_2J,_2K){while(1){var _2L=E(_2J);if(!_2L[0]){return E(_2K)[0]==0?true:false;}else{var _2M=E(_2K);if(!_2M[0]){return false;}else{if(E(_2L[1])[1]!=E(_2M[1])[1]){return false;}else{_2J=_2L[2];_2K=_2M[2];continue;}}}}},_2N=function(_2O,_2P){return E(_2O)[1]!=E(_2P)[1];},_2Q=function(_2R,_2S){return E(_2R)[1]==E(_2S)[1];},_2T=[0,_2Q,_2N],_2U=function(_2V,_2W){while(1){var _2X=E(_2V);if(!_2X[0]){return E(_2W)[0]==0?true:false;}else{var _2Y=E(_2W);if(!_2Y[0]){return false;}else{if(E(_2X[1])[1]!=E(_2Y[1])[1]){return false;}else{_2V=_2X[2];_2W=_2Y[2];continue;}}}}},_2Z=function(_30,_31){return !B(_2U(_30,_31))?true:false;},_32=[0,_2U,_2Z],_33=function(_34,_35){var _36=E(_34);switch(_36[0]){case 0:return [0,function(_37){return new F(function(){return _33(B(A(_36[1],[_37])),_35);});}];case 1:return [1,function(_38){return new F(function(){return _33(B(A(_36[1],[_38])),_35);});}];case 2:return [2];case 3:return new F(function(){return _2b(B(A(_35,[_36[1]])),new T(function(){return B(_33(_36[2],_35));}));});break;default:var _39=function(_3a){var _3b=E(_3a);if(!_3b[0]){return [0];}else{var _3c=E(_3b[1]);return new F(function(){return _f(B(_21(B(A(_35,[_3c[1]])),_3c[2])),new T(function(){return B(_39(_3b[2]));},1));});}},_3d=B(_39(_36[1]));return _3d[0]==0?[2]:[4,_3d];}},_3e=0,_3f=function(_3g,_3h){var _3i=E(_3g);if(!_3i){return new F(function(){return A(_3h,[_3e]);});}else{return [0,function(_3j){return E(new T(function(){return B(_3f(_3i-1|0,_3h));}));}];}},_3k=function(_3l,_3m,_3n){return function(_3o){return new F(function(){return A(function(_3p,_3q,_3r){while(1){var _3s=(function(_3t,_3u,_3v){var _3w=E(_3t);switch(_3w[0]){case 0:var _3x=E(_3u);if(!_3x[0]){return E(_3m);}else{_3p=B(A(_3w[1],[_3x[1]]));_3q=_3x[2];var _3y=_3v+1|0;_3r=_3y;return null;}break;case 1:var _3z=B(A(_3w[1],[_3u])),_3A=_3u,_3y=_3v;_3p=_3z;_3q=_3A;_3r=_3y;return null;case 2:return E(_3m);case 3:return function(_3B){return new F(function(){return _3f(_3v,function(_3C){return E(new T(function(){return B(_33(_3w,_3B));}));});});};default:return function(_3D){return new F(function(){return _33(_3w,_3D);});};}})(_3p,_3q,_3r);if(_3s!=null){return _3s;}}},[new T(function(){return B(A(_3l,[_S]));}),_3o,0,_3n]);});};},_3E=function(_3F){return new F(function(){return A(_3F,[_C]);});},_3G=function(_3H,_3I){var _3J=function(_3K){var _3L=E(_3K);if(!_3L[0]){return E(_3E);}else{var _3M=_3L[1];return !B(A(_3H,[_3M]))?E(_3E):function(_3N){return [0,function(_3O){return E(new T(function(){return B(A(new T(function(){return B(_3J(_3L[2]));}),[function(_3P){return new F(function(){return A(_3N,[[1,_3M,_3P]]);});}]));}));}];};}};return function(_3Q){return new F(function(){return A(_3J,[_3Q,_3I]);});};},_3R=[6],_3S=function(_3T){return E(_3T);},_3U=new T(function(){return B(unCStr("valDig: Bad base"));}),_3V=new T(function(){return B(err(_3U));}),_3W=function(_3X,_3Y){var _3Z=function(_40,_41){var _42=E(_40);if(!_42[0]){return function(_43){return new F(function(){return A(_43,[new T(function(){return B(A(_41,[_C]));})]);});};}else{var _44=E(_42[1])[1],_45=function(_46){return function(_47){return [0,function(_48){return E(new T(function(){return B(A(new T(function(){return B(_3Z(_42[2],function(_49){return new F(function(){return A(_41,[[1,_46,_49]]);});}));}),[_47]));}));}];};};switch(E(E(_3X)[1])){case 8:if(48>_44){return function(_4a){return new F(function(){return A(_4a,[new T(function(){return B(A(_41,[_C]));})]);});};}else{if(_44>55){return function(_4b){return new F(function(){return A(_4b,[new T(function(){return B(A(_41,[_C]));})]);});};}else{return new F(function(){return _45([0,_44-48|0]);});}}break;case 10:if(48>_44){return function(_4c){return new F(function(){return A(_4c,[new T(function(){return B(A(_41,[_C]));})]);});};}else{if(_44>57){return function(_4d){return new F(function(){return A(_4d,[new T(function(){return B(A(_41,[_C]));})]);});};}else{return new F(function(){return _45([0,_44-48|0]);});}}break;case 16:if(48>_44){if(97>_44){if(65>_44){return function(_4e){return new F(function(){return A(_4e,[new T(function(){return B(A(_41,[_C]));})]);});};}else{if(_44>70){return function(_4f){return new F(function(){return A(_4f,[new T(function(){return B(A(_41,[_C]));})]);});};}else{return new F(function(){return _45([0,(_44-65|0)+10|0]);});}}}else{if(_44>102){if(65>_44){return function(_4g){return new F(function(){return A(_4g,[new T(function(){return B(A(_41,[_C]));})]);});};}else{if(_44>70){return function(_4h){return new F(function(){return A(_4h,[new T(function(){return B(A(_41,[_C]));})]);});};}else{return new F(function(){return _45([0,(_44-65|0)+10|0]);});}}}else{return new F(function(){return _45([0,(_44-97|0)+10|0]);});}}}else{if(_44>57){if(97>_44){if(65>_44){return function(_4i){return new F(function(){return A(_4i,[new T(function(){return B(A(_41,[_C]));})]);});};}else{if(_44>70){return function(_4j){return new F(function(){return A(_4j,[new T(function(){return B(A(_41,[_C]));})]);});};}else{return new F(function(){return _45([0,(_44-65|0)+10|0]);});}}}else{if(_44>102){if(65>_44){return function(_4k){return new F(function(){return A(_4k,[new T(function(){return B(A(_41,[_C]));})]);});};}else{if(_44>70){return function(_4l){return new F(function(){return A(_4l,[new T(function(){return B(A(_41,[_C]));})]);});};}else{return new F(function(){return _45([0,(_44-65|0)+10|0]);});}}}else{return new F(function(){return _45([0,(_44-97|0)+10|0]);});}}}else{return new F(function(){return _45([0,_44-48|0]);});}}break;default:return E(_3V);}}};return function(_4m){return new F(function(){return A(_3Z,[_4m,_3S,function(_4n){var _4o=E(_4n);return _4o[0]==0?[2]:B(A(_3Y,[_4o]));}]);});};},_4p=[0,10],_4q=[0,1],_4r=[0,2147483647],_4s=function(_4t,_4u){while(1){var _4v=E(_4t);if(!_4v[0]){var _4w=_4v[1],_4x=E(_4u);if(!_4x[0]){var _4y=_4x[1],_4z=addC(_4w,_4y);if(!E(_4z[2])){return [0,_4z[1]];}else{_4t=[1,I_fromInt(_4w)];_4u=[1,I_fromInt(_4y)];continue;}}else{_4t=[1,I_fromInt(_4w)];_4u=_4x;continue;}}else{var _4A=E(_4u);if(!_4A[0]){_4t=_4v;_4u=[1,I_fromInt(_4A[1])];continue;}else{return [1,I_add(_4v[1],_4A[1])];}}}},_4B=new T(function(){return B(_4s(_4r,_4q));}),_4C=function(_4D){var _4E=E(_4D);if(!_4E[0]){var _4F=E(_4E[1]);return _4F==(-2147483648)?E(_4B):[0, -_4F];}else{return [1,I_negate(_4E[1])];}},_4G=[0,10],_4H=[0,0],_4I=function(_4J){return [0,_4J];},_4K=function(_4L,_4M){while(1){var _4N=E(_4L);if(!_4N[0]){var _4O=_4N[1],_4P=E(_4M);if(!_4P[0]){var _4Q=_4P[1];if(!(imul(_4O,_4Q)|0)){return [0,imul(_4O,_4Q)|0];}else{_4L=[1,I_fromInt(_4O)];_4M=[1,I_fromInt(_4Q)];continue;}}else{_4L=[1,I_fromInt(_4O)];_4M=_4P;continue;}}else{var _4R=E(_4M);if(!_4R[0]){_4L=_4N;_4M=[1,I_fromInt(_4R[1])];continue;}else{return [1,I_mul(_4N[1],_4R[1])];}}}},_4S=function(_4T,_4U,_4V){while(1){var _4W=E(_4V);if(!_4W[0]){return E(_4U);}else{var _4X=B(_4s(B(_4K(_4U,_4T)),B(_4I(E(_4W[1])[1]))));_4V=_4W[2];_4U=_4X;continue;}}},_4Y=function(_4Z){var _50=new T(function(){return B(_2b(B(_2b([0,function(_51){return E(E(_51)[1])==45?[1,B(_3W(_4p,function(_52){return new F(function(){return A(_4Z,[[1,new T(function(){return B(_4C(B(_4S(_4G,_4H,_52))));})]]);});}))]:[2];}],[0,function(_53){return E(E(_53)[1])==43?[1,B(_3W(_4p,function(_54){return new F(function(){return A(_4Z,[[1,new T(function(){return B(_4S(_4G,_4H,_54));})]]);});}))]:[2];}])),new T(function(){return [1,B(_3W(_4p,function(_55){return new F(function(){return A(_4Z,[[1,new T(function(){return B(_4S(_4G,_4H,_55));})]]);});}))];})));});return new F(function(){return _2b([0,function(_56){return E(E(_56)[1])==101?E(_50):[2];}],[0,function(_57){return E(E(_57)[1])==69?E(_50):[2];}]);});},_58=[0],_59=function(_5a){return new F(function(){return A(_5a,[_58]);});},_5b=function(_5c){return new F(function(){return A(_5c,[_58]);});},_5d=function(_5e){return function(_5f){return E(E(_5f)[1])==46?[1,B(_3W(_4p,function(_5g){return new F(function(){return A(_5e,[[1,_5g]]);});}))]:[2];};},_5h=function(_5i){return [0,B(_5d(_5i))];},_5j=function(_5k){return new F(function(){return _3W(_4p,function(_5l){return [1,B(_3k(_5h,_59,function(_5m){return [1,B(_3k(_4Y,_5b,function(_5n){return new F(function(){return A(_5k,[[5,[1,_5l,_5m,_5n]]]);});}))];}))];});});},_5o=function(_5p){return [1,B(_5j(_5p))];},_5q=function(_5r){return E(E(_5r)[1]);},_5s=function(_5t,_5u,_5v){while(1){var _5w=E(_5v);if(!_5w[0]){return false;}else{if(!B(A(_5q,[_5t,_5u,_5w[1]]))){_5v=_5w[2];continue;}else{return true;}}}},_5x=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_5y=function(_5z){return new F(function(){return _5s(_2T,_5z,_5x);});},_5A=[0,8],_5B=[0,16],_5C=function(_5D){var _5E=function(_5F){return new F(function(){return A(_5D,[[5,[0,_5A,_5F]]]);});},_5G=function(_5H){return new F(function(){return A(_5D,[[5,[0,_5B,_5H]]]);});};return function(_5I){return E(E(_5I)[1])==48?E([0,function(_5J){switch(E(E(_5J)[1])){case 79:return [1,B(_3W(_5A,_5E))];case 88:return [1,B(_3W(_5B,_5G))];case 111:return [1,B(_3W(_5A,_5E))];case 120:return [1,B(_3W(_5B,_5G))];default:return [2];}}]):[2];};},_5K=function(_5L){return [0,B(_5C(_5L))];},_5M=false,_5N=true,_5O=function(_5P){var _5Q=new T(function(){return B(A(_5P,[_5A]));}),_5R=new T(function(){return B(A(_5P,[_5B]));});return function(_5S){switch(E(E(_5S)[1])){case 79:return E(_5Q);case 88:return E(_5R);case 111:return E(_5Q);case 120:return E(_5R);default:return [2];}};},_5T=function(_5U){return [0,B(_5O(_5U))];},_5V=[0,92],_5W=function(_5X){return new F(function(){return A(_5X,[_4p]);});},_5Y=function(_5Z){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_q(9,_5Z,_C));}))));});},_60=function(_61){var _62=E(_61);return _62[0]==0?E(_62[1]):I_toInt(_62[1]);},_63=function(_64,_65){var _66=E(_64);if(!_66[0]){var _67=_66[1],_68=E(_65);return _68[0]==0?_67<=_68[1]:I_compareInt(_68[1],_67)>=0;}else{var _69=_66[1],_6a=E(_65);return _6a[0]==0?I_compareInt(_69,_6a[1])<=0:I_compare(_69,_6a[1])<=0;}},_6b=function(_6c){return [2];},_6d=function(_6e){var _6f=E(_6e);if(!_6f[0]){return E(_6b);}else{var _6g=_6f[1],_6h=E(_6f[2]);return _6h[0]==0?E(_6g):function(_6i){return new F(function(){return _2b(B(A(_6g,[_6i])),new T(function(){return B(A(new T(function(){return B(_6d(_6h));}),[_6i]));}));});};}},_6j=function(_6k){return [2];},_6l=function(_6m,_6n){var _6o=function(_6p,_6q){var _6r=E(_6p);if(!_6r[0]){return function(_6s){return new F(function(){return A(_6s,[_6m]);});};}else{var _6t=E(_6q);return _6t[0]==0?E(_6j):E(_6r[1])[1]!=E(_6t[1])[1]?E(_6j):function(_6u){return [0,function(_6v){return E(new T(function(){return B(A(new T(function(){return B(_6o(_6r[2],_6t[2]));}),[_6u]));}));}];};}};return function(_6w){return new F(function(){return A(_6o,[_6m,_6w,_6n]);});};},_6x=new T(function(){return B(unCStr("SOH"));}),_6y=[0,1],_6z=function(_6A){return [1,B(_6l(_6x,function(_6B){return E(new T(function(){return B(A(_6A,[_6y]));}));}))];},_6C=new T(function(){return B(unCStr("SO"));}),_6D=[0,14],_6E=function(_6F){return [1,B(_6l(_6C,function(_6G){return E(new T(function(){return B(A(_6F,[_6D]));}));}))];},_6H=function(_6I){return [1,B(_3k(_6z,_6E,_6I))];},_6J=new T(function(){return B(unCStr("NUL"));}),_6K=[0,0],_6L=function(_6M){return [1,B(_6l(_6J,function(_6N){return E(new T(function(){return B(A(_6M,[_6K]));}));}))];},_6O=new T(function(){return B(unCStr("STX"));}),_6P=[0,2],_6Q=function(_6R){return [1,B(_6l(_6O,function(_6S){return E(new T(function(){return B(A(_6R,[_6P]));}));}))];},_6T=new T(function(){return B(unCStr("ETX"));}),_6U=[0,3],_6V=function(_6W){return [1,B(_6l(_6T,function(_6X){return E(new T(function(){return B(A(_6W,[_6U]));}));}))];},_6Y=new T(function(){return B(unCStr("EOT"));}),_6Z=[0,4],_70=function(_71){return [1,B(_6l(_6Y,function(_72){return E(new T(function(){return B(A(_71,[_6Z]));}));}))];},_73=new T(function(){return B(unCStr("ENQ"));}),_74=[0,5],_75=function(_76){return [1,B(_6l(_73,function(_77){return E(new T(function(){return B(A(_76,[_74]));}));}))];},_78=new T(function(){return B(unCStr("ACK"));}),_79=[0,6],_7a=function(_7b){return [1,B(_6l(_78,function(_7c){return E(new T(function(){return B(A(_7b,[_79]));}));}))];},_7d=new T(function(){return B(unCStr("BEL"));}),_7e=[0,7],_7f=function(_7g){return [1,B(_6l(_7d,function(_7h){return E(new T(function(){return B(A(_7g,[_7e]));}));}))];},_7i=new T(function(){return B(unCStr("BS"));}),_7j=[0,8],_7k=function(_7l){return [1,B(_6l(_7i,function(_7m){return E(new T(function(){return B(A(_7l,[_7j]));}));}))];},_7n=new T(function(){return B(unCStr("HT"));}),_7o=[0,9],_7p=function(_7q){return [1,B(_6l(_7n,function(_7r){return E(new T(function(){return B(A(_7q,[_7o]));}));}))];},_7s=new T(function(){return B(unCStr("LF"));}),_7t=[0,10],_7u=function(_7v){return [1,B(_6l(_7s,function(_7w){return E(new T(function(){return B(A(_7v,[_7t]));}));}))];},_7x=new T(function(){return B(unCStr("VT"));}),_7y=[0,11],_7z=function(_7A){return [1,B(_6l(_7x,function(_7B){return E(new T(function(){return B(A(_7A,[_7y]));}));}))];},_7C=new T(function(){return B(unCStr("FF"));}),_7D=[0,12],_7E=function(_7F){return [1,B(_6l(_7C,function(_7G){return E(new T(function(){return B(A(_7F,[_7D]));}));}))];},_7H=new T(function(){return B(unCStr("CR"));}),_7I=[0,13],_7J=function(_7K){return [1,B(_6l(_7H,function(_7L){return E(new T(function(){return B(A(_7K,[_7I]));}));}))];},_7M=new T(function(){return B(unCStr("SI"));}),_7N=[0,15],_7O=function(_7P){return [1,B(_6l(_7M,function(_7Q){return E(new T(function(){return B(A(_7P,[_7N]));}));}))];},_7R=new T(function(){return B(unCStr("DLE"));}),_7S=[0,16],_7T=function(_7U){return [1,B(_6l(_7R,function(_7V){return E(new T(function(){return B(A(_7U,[_7S]));}));}))];},_7W=new T(function(){return B(unCStr("DC1"));}),_7X=[0,17],_7Y=function(_7Z){return [1,B(_6l(_7W,function(_80){return E(new T(function(){return B(A(_7Z,[_7X]));}));}))];},_81=new T(function(){return B(unCStr("DC2"));}),_82=[0,18],_83=function(_84){return [1,B(_6l(_81,function(_85){return E(new T(function(){return B(A(_84,[_82]));}));}))];},_86=new T(function(){return B(unCStr("DC3"));}),_87=[0,19],_88=function(_89){return [1,B(_6l(_86,function(_8a){return E(new T(function(){return B(A(_89,[_87]));}));}))];},_8b=new T(function(){return B(unCStr("DC4"));}),_8c=[0,20],_8d=function(_8e){return [1,B(_6l(_8b,function(_8f){return E(new T(function(){return B(A(_8e,[_8c]));}));}))];},_8g=new T(function(){return B(unCStr("NAK"));}),_8h=[0,21],_8i=function(_8j){return [1,B(_6l(_8g,function(_8k){return E(new T(function(){return B(A(_8j,[_8h]));}));}))];},_8l=new T(function(){return B(unCStr("SYN"));}),_8m=[0,22],_8n=function(_8o){return [1,B(_6l(_8l,function(_8p){return E(new T(function(){return B(A(_8o,[_8m]));}));}))];},_8q=new T(function(){return B(unCStr("ETB"));}),_8r=[0,23],_8s=function(_8t){return [1,B(_6l(_8q,function(_8u){return E(new T(function(){return B(A(_8t,[_8r]));}));}))];},_8v=new T(function(){return B(unCStr("CAN"));}),_8w=[0,24],_8x=function(_8y){return [1,B(_6l(_8v,function(_8z){return E(new T(function(){return B(A(_8y,[_8w]));}));}))];},_8A=new T(function(){return B(unCStr("EM"));}),_8B=[0,25],_8C=function(_8D){return [1,B(_6l(_8A,function(_8E){return E(new T(function(){return B(A(_8D,[_8B]));}));}))];},_8F=new T(function(){return B(unCStr("SUB"));}),_8G=[0,26],_8H=function(_8I){return [1,B(_6l(_8F,function(_8J){return E(new T(function(){return B(A(_8I,[_8G]));}));}))];},_8K=new T(function(){return B(unCStr("ESC"));}),_8L=[0,27],_8M=function(_8N){return [1,B(_6l(_8K,function(_8O){return E(new T(function(){return B(A(_8N,[_8L]));}));}))];},_8P=new T(function(){return B(unCStr("FS"));}),_8Q=[0,28],_8R=function(_8S){return [1,B(_6l(_8P,function(_8T){return E(new T(function(){return B(A(_8S,[_8Q]));}));}))];},_8U=new T(function(){return B(unCStr("GS"));}),_8V=[0,29],_8W=function(_8X){return [1,B(_6l(_8U,function(_8Y){return E(new T(function(){return B(A(_8X,[_8V]));}));}))];},_8Z=new T(function(){return B(unCStr("RS"));}),_90=[0,30],_91=function(_92){return [1,B(_6l(_8Z,function(_93){return E(new T(function(){return B(A(_92,[_90]));}));}))];},_94=new T(function(){return B(unCStr("US"));}),_95=[0,31],_96=function(_97){return [1,B(_6l(_94,function(_98){return E(new T(function(){return B(A(_97,[_95]));}));}))];},_99=new T(function(){return B(unCStr("SP"));}),_9a=[0,32],_9b=function(_9c){return [1,B(_6l(_99,function(_9d){return E(new T(function(){return B(A(_9c,[_9a]));}));}))];},_9e=new T(function(){return B(unCStr("DEL"));}),_9f=[0,127],_9g=function(_9h){return [1,B(_6l(_9e,function(_9i){return E(new T(function(){return B(A(_9h,[_9f]));}));}))];},_9j=[1,_9g,_C],_9k=[1,_9b,_9j],_9l=[1,_96,_9k],_9m=[1,_91,_9l],_9n=[1,_8W,_9m],_9o=[1,_8R,_9n],_9p=[1,_8M,_9o],_9q=[1,_8H,_9p],_9r=[1,_8C,_9q],_9s=[1,_8x,_9r],_9t=[1,_8s,_9s],_9u=[1,_8n,_9t],_9v=[1,_8i,_9u],_9w=[1,_8d,_9v],_9x=[1,_88,_9w],_9y=[1,_83,_9x],_9z=[1,_7Y,_9y],_9A=[1,_7T,_9z],_9B=[1,_7O,_9A],_9C=[1,_7J,_9B],_9D=[1,_7E,_9C],_9E=[1,_7z,_9D],_9F=[1,_7u,_9E],_9G=[1,_7p,_9F],_9H=[1,_7k,_9G],_9I=[1,_7f,_9H],_9J=[1,_7a,_9I],_9K=[1,_75,_9J],_9L=[1,_70,_9K],_9M=[1,_6V,_9L],_9N=[1,_6Q,_9M],_9O=[1,_6L,_9N],_9P=[1,_6H,_9O],_9Q=new T(function(){return B(_6d(_9P));}),_9R=[0,1114111],_9S=[0,34],_9T=[0,39],_9U=function(_9V){var _9W=new T(function(){return B(A(_9V,[_7e]));}),_9X=new T(function(){return B(A(_9V,[_7j]));}),_9Y=new T(function(){return B(A(_9V,[_7o]));}),_9Z=new T(function(){return B(A(_9V,[_7t]));}),_a0=new T(function(){return B(A(_9V,[_7y]));}),_a1=new T(function(){return B(A(_9V,[_7D]));}),_a2=new T(function(){return B(A(_9V,[_7I]));});return new F(function(){return _2b([0,function(_a3){switch(E(E(_a3)[1])){case 34:return E(new T(function(){return B(A(_9V,[_9S]));}));case 39:return E(new T(function(){return B(A(_9V,[_9T]));}));case 92:return E(new T(function(){return B(A(_9V,[_5V]));}));case 97:return E(_9W);case 98:return E(_9X);case 102:return E(_a1);case 110:return E(_9Z);case 114:return E(_a2);case 116:return E(_9Y);case 118:return E(_a0);default:return [2];}}],new T(function(){return B(_2b([1,B(_3k(_5T,_5W,function(_a4){return [1,B(_3W(_a4,function(_a5){var _a6=B(_4S(new T(function(){return B(_4I(E(_a4)[1]));}),_4H,_a5));return !B(_63(_a6,_9R))?[2]:B(A(_9V,[new T(function(){var _a7=B(_60(_a6));if(_a7>>>0>1114111){var _a8=B(_5Y(_a7));}else{var _a8=[0,_a7];}var _a9=_a8,_aa=_a9,_ab=_aa;return _ab;})]));}))];}))],new T(function(){return B(_2b([0,function(_ac){return E(E(_ac)[1])==94?E([0,function(_ad){switch(E(E(_ad)[1])){case 64:return E(new T(function(){return B(A(_9V,[_6K]));}));case 65:return E(new T(function(){return B(A(_9V,[_6y]));}));case 66:return E(new T(function(){return B(A(_9V,[_6P]));}));case 67:return E(new T(function(){return B(A(_9V,[_6U]));}));case 68:return E(new T(function(){return B(A(_9V,[_6Z]));}));case 69:return E(new T(function(){return B(A(_9V,[_74]));}));case 70:return E(new T(function(){return B(A(_9V,[_79]));}));case 71:return E(_9W);case 72:return E(_9X);case 73:return E(_9Y);case 74:return E(_9Z);case 75:return E(_a0);case 76:return E(_a1);case 77:return E(_a2);case 78:return E(new T(function(){return B(A(_9V,[_6D]));}));case 79:return E(new T(function(){return B(A(_9V,[_7N]));}));case 80:return E(new T(function(){return B(A(_9V,[_7S]));}));case 81:return E(new T(function(){return B(A(_9V,[_7X]));}));case 82:return E(new T(function(){return B(A(_9V,[_82]));}));case 83:return E(new T(function(){return B(A(_9V,[_87]));}));case 84:return E(new T(function(){return B(A(_9V,[_8c]));}));case 85:return E(new T(function(){return B(A(_9V,[_8h]));}));case 86:return E(new T(function(){return B(A(_9V,[_8m]));}));case 87:return E(new T(function(){return B(A(_9V,[_8r]));}));case 88:return E(new T(function(){return B(A(_9V,[_8w]));}));case 89:return E(new T(function(){return B(A(_9V,[_8B]));}));case 90:return E(new T(function(){return B(A(_9V,[_8G]));}));case 91:return E(new T(function(){return B(A(_9V,[_8L]));}));case 92:return E(new T(function(){return B(A(_9V,[_8Q]));}));case 93:return E(new T(function(){return B(A(_9V,[_8V]));}));case 94:return E(new T(function(){return B(A(_9V,[_90]));}));case 95:return E(new T(function(){return B(A(_9V,[_95]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_9Q,[_9V]));})));})));}));});},_ae=function(_af){return new F(function(){return A(_af,[_3e]);});},_ag=function(_ah){var _ai=E(_ah);if(!_ai[0]){return E(_ae);}else{var _aj=_ai[2],_ak=E(E(_ai[1])[1]);switch(_ak){case 9:return function(_al){return [0,function(_am){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_al]));}));}];};case 10:return function(_an){return [0,function(_ao){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_an]));}));}];};case 11:return function(_ap){return [0,function(_aq){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_ap]));}));}];};case 12:return function(_ar){return [0,function(_as){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_ar]));}));}];};case 13:return function(_at){return [0,function(_au){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_at]));}));}];};case 32:return function(_av){return [0,function(_aw){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_av]));}));}];};case 160:return function(_ax){return [0,function(_ay){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_ax]));}));}];};default:var _az=u_iswspace(_ak),_aA=_az;return E(_aA)==0?E(_ae):function(_aB){return [0,function(_aC){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_aB]));}));}];};}}},_aD=function(_aE){var _aF=new T(function(){return B(_aD(_aE));}),_aG=[1,function(_aH){return new F(function(){return A(_ag,[_aH,function(_aI){return E([0,function(_aJ){return E(E(_aJ)[1])==92?E(_aF):[2];}]);}]);});}];return new F(function(){return _2b([0,function(_aK){return E(E(_aK)[1])==92?E([0,function(_aL){var _aM=E(E(_aL)[1]);switch(_aM){case 9:return E(_aG);case 10:return E(_aG);case 11:return E(_aG);case 12:return E(_aG);case 13:return E(_aG);case 32:return E(_aG);case 38:return E(_aF);case 160:return E(_aG);default:var _aN=u_iswspace(_aM),_aO=_aN;return E(_aO)==0?[2]:E(_aG);}}]):[2];}],[0,function(_aP){var _aQ=E(_aP);return E(_aQ[1])==92?E(new T(function(){return B(_9U(function(_aR){return new F(function(){return A(_aE,[[0,_aR,_5N]]);});}));})):B(A(_aE,[[0,_aQ,_5M]]));}]);});},_aS=function(_aT,_aU){return new F(function(){return _aD(function(_aV){var _aW=E(_aV),_aX=E(_aW[1]);if(E(_aX[1])==34){if(!E(_aW[2])){return E(new T(function(){return B(A(_aU,[[1,new T(function(){return B(A(_aT,[_C]));})]]));}));}else{return new F(function(){return _aS(function(_aY){return new F(function(){return A(_aT,[[1,_aX,_aY]]);});},_aU);});}}else{return new F(function(){return _aS(function(_aZ){return new F(function(){return A(_aT,[[1,_aX,_aZ]]);});},_aU);});}});});},_b0=new T(function(){return B(unCStr("_\'"));}),_b1=function(_b2){var _b3=u_iswalnum(_b2),_b4=_b3;return E(_b4)==0?B(_5s(_2T,[0,_b2],_b0)):true;},_b5=function(_b6){return new F(function(){return _b1(E(_b6)[1]);});},_b7=new T(function(){return B(unCStr(",;()[]{}`"));}),_b8=new T(function(){return B(unCStr(".."));}),_b9=new T(function(){return B(unCStr("::"));}),_ba=new T(function(){return B(unCStr("->"));}),_bb=[0,64],_bc=[1,_bb,_C],_bd=[0,126],_be=[1,_bd,_C],_bf=new T(function(){return B(unCStr("=>"));}),_bg=[1,_bf,_C],_bh=[1,_be,_bg],_bi=[1,_bc,_bh],_bj=[1,_ba,_bi],_bk=new T(function(){return B(unCStr("<-"));}),_bl=[1,_bk,_bj],_bm=[0,124],_bn=[1,_bm,_C],_bo=[1,_bn,_bl],_bp=[1,_5V,_C],_bq=[1,_bp,_bo],_br=[0,61],_bs=[1,_br,_C],_bt=[1,_bs,_bq],_bu=[1,_b9,_bt],_bv=[1,_b8,_bu],_bw=function(_bx){return new F(function(){return _2b([1,function(_by){return E(_by)[0]==0?E(new T(function(){return B(A(_bx,[_3R]));})):[2];}],new T(function(){return B(_2b([0,function(_bz){return E(E(_bz)[1])==39?E([0,function(_bA){var _bB=E(_bA);switch(E(_bB[1])){case 39:return [2];case 92:return E(new T(function(){return B(_9U(function(_bC){return [0,function(_bD){return E(E(_bD)[1])==39?E(new T(function(){return B(A(_bx,[[0,_bC]]));})):[2];}];}));}));default:return [0,function(_bE){return E(E(_bE)[1])==39?E(new T(function(){return B(A(_bx,[[0,_bB]]));})):[2];}];}}]):[2];}],new T(function(){return B(_2b([0,function(_bF){return E(E(_bF)[1])==34?E(new T(function(){return B(_aS(_3S,_bx));})):[2];}],new T(function(){return B(_2b([0,function(_bG){return !B(_5s(_2T,_bG,_b7))?[2]:B(A(_bx,[[2,[1,_bG,_C]]]));}],new T(function(){return B(_2b([0,function(_bH){return !B(_5s(_2T,_bH,_5x))?[2]:[1,B(_3G(_5y,function(_bI){var _bJ=[1,_bH,_bI];return !B(_5s(_32,_bJ,_bv))?B(A(_bx,[[4,_bJ]])):B(A(_bx,[[2,_bJ]]));}))];}],new T(function(){return B(_2b([0,function(_bK){var _bL=E(_bK),_bM=_bL[1],_bN=u_iswalpha(_bM),_bO=_bN;return E(_bO)==0?E(_bM)==95?[1,B(_3G(_b5,function(_bP){return new F(function(){return A(_bx,[[3,[1,_bL,_bP]]]);});}))]:[2]:[1,B(_3G(_b5,function(_bQ){return new F(function(){return A(_bx,[[3,[1,_bL,_bQ]]]);});}))];}],new T(function(){return [1,B(_3k(_5K,_5o,_bx))];})));})));})));})));})));}));});},_bR=[0,0],_bS=function(_bT,_bU){return function(_bV){return new F(function(){return A(_ag,[_bV,function(_bW){return E(new T(function(){return B(_bw(function(_bX){var _bY=E(_bX);return _bY[0]==2?!B(_2I(_bY[1],_2H))?[2]:E(new T(function(){return B(A(_bT,[_bR,function(_bZ){return [1,function(_c0){return new F(function(){return A(_ag,[_c0,function(_c1){return E(new T(function(){return B(_bw(function(_c2){var _c3=E(_c2);return _c3[0]==2?!B(_2I(_c3[1],_2F))?[2]:E(new T(function(){return B(A(_bU,[_bZ]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_c4=function(_c5,_c6,_c7){var _c8=function(_c9,_ca){return new F(function(){return _2b([1,function(_cb){return new F(function(){return A(_ag,[_cb,function(_cc){return E(new T(function(){return B(_bw(function(_cd){var _ce=E(_cd);if(_ce[0]==4){var _cf=E(_ce[1]);if(!_cf[0]){return new F(function(){return A(_c5,[_ce,_c9,_ca]);});}else{return E(E(_cf[1])[1])==45?E(_cf[2])[0]==0?E([1,function(_cg){return new F(function(){return A(_ag,[_cg,function(_ch){return E(new T(function(){return B(_bw(function(_ci){return new F(function(){return A(_c5,[_ci,_c9,function(_cj){return new F(function(){return A(_ca,[new T(function(){return [0, -E(_cj)[1]];})]);});}]);});}));}));}]);});}]):B(A(_c5,[_ce,_c9,_ca])):B(A(_c5,[_ce,_c9,_ca]));}}else{return new F(function(){return A(_c5,[_ce,_c9,_ca]);});}}));}));}]);});}],new T(function(){return [1,B(_bS(_c8,_ca))];}));});};return new F(function(){return _c8(_c6,_c7);});},_ck=function(_cl,_cm){return [2];},_cn=function(_co){var _cp=E(_co);return _cp[0]==0?[1,new T(function(){return B(_4S(new T(function(){return B(_4I(E(_cp[1])[1]));}),_4H,_cp[2]));})]:E(_cp[2])[0]==0?E(_cp[3])[0]==0?[1,new T(function(){return B(_4S(_4G,_4H,_cp[1]));})]:[0]:[0];},_cq=function(_cr){var _cs=E(_cr);if(_cs[0]==5){var _ct=B(_cn(_cs[1]));return _ct[0]==0?E(_ck):function(_cu,_cv){return new F(function(){return A(_cv,[new T(function(){return [0,B(_60(_ct[1]))];})]);});};}else{return E(_ck);}},_cw=function(_cx,_cy){return new F(function(){return _c4(_cq,_cx,_cy);});},_cz=[0,91],_cA=[1,_cz,_C],_cB=function(_cC,_cD){var _cE=function(_cF,_cG){return [1,function(_cH){return new F(function(){return A(_ag,[_cH,function(_cI){return E(new T(function(){return B(_bw(function(_cJ){var _cK=E(_cJ);if(_cK[0]==2){var _cL=E(_cK[1]);if(!_cL[0]){return [2];}else{var _cM=_cL[2];switch(E(E(_cL[1])[1])){case 44:return E(_cM)[0]==0?!E(_cF)?[2]:E(new T(function(){return B(A(_cC,[_bR,function(_cN){return new F(function(){return _cE(_5N,function(_cO){return new F(function(){return A(_cG,[[1,_cN,_cO]]);});});});}]));})):[2];case 93:return E(_cM)[0]==0?E(new T(function(){return B(A(_cG,[_C]));})):[2];default:return [2];}}}else{return [2];}}));}));}]);});}];},_cP=function(_cQ){return new F(function(){return _2b([1,function(_cR){return new F(function(){return A(_ag,[_cR,function(_cS){return E(new T(function(){return B(_bw(function(_cT){var _cU=E(_cT);return _cU[0]==2?!B(_2I(_cU[1],_cA))?[2]:E(new T(function(){return B(_2b(B(_cE(_5M,_cQ)),new T(function(){return B(A(_cC,[_bR,function(_cV){return new F(function(){return _cE(_5N,function(_cW){return new F(function(){return A(_cQ,[[1,_cV,_cW]]);});});});}]));})));})):[2];}));}));}]);});}],new T(function(){return [1,B(_bS(function(_cX,_cY){return new F(function(){return _cP(_cY);});},_cQ))];}));});};return new F(function(){return _cP(_cD);});},_cZ=function(_d0,_d1){return new F(function(){return _cB(_cw,_d1);});},_d2=function(_d3){return function(_3D){return new F(function(){return _21(new T(function(){return B(_c4(_cq,_d3,_S));}),_3D);});};},_d4=new T(function(){return B(_cB(_cw,_S));}),_d5=function(_cy){return new F(function(){return _21(_d4,_cy);});},_d6=[0,_d2,_d5,_cw,_cZ],_d7=[0,44],_d8=[1,_d7,_C],_d9=function(_da){return E(E(_da)[3]);},_db=function(_dc,_dd,_de){return function(_df){return new F(function(){return A(new T(function(){return B(A(_d9,[_dc,_de]));}),[function(_dg){return [1,function(_dh){return new F(function(){return A(_ag,[_dh,function(_di){return E(new T(function(){return B(_bw(function(_dj){var _dk=E(_dj);return _dk[0]==2?!B(_2I(_dk[1],_d8))?[2]:E(new T(function(){return B(A(new T(function(){return B(_d9(_dd));}),[_de,function(_dl){return new F(function(){return A(_df,[[0,_dg,_dl]]);});}]));})):[2];}));}));}]);});}];}]);});};},_dm=function(_dn,_do){var _dp=function(_dq){return new F(function(){return _2b([1,B(_bS(_dn,_dq))],new T(function(){return [1,B(_bS(function(_dr,_ds){return new F(function(){return _dp(_ds);});},_dq))];}));});};return new F(function(){return _dp(_do);});},_dt=function(_du,_dv,_dw,_dx){return new F(function(){return _dm(function(_dy,_dz){return new F(function(){return A(_db,[_du,_dv,_dy,function(_dA){var _dB=E(_dA);return [1,function(_dC){return new F(function(){return A(_ag,[_dC,function(_dD){return E(new T(function(){return B(_bw(function(_dE){var _dF=E(_dE);return _dF[0]==2?!B(_2I(_dF[1],_d8))?[2]:E(new T(function(){return B(A(new T(function(){return B(_d9(_dw));}),[_dy,function(_dG){return new F(function(){return A(_dz,[[0,_dB[1],_dB[2],_dG]]);});}]));})):[2];}));}));}]);});}];}]);});},_dx);});},_dH=function(_dI){return E(E(_dI)[4]);},_dJ=function(_dK,_dL,_dM){return new F(function(){return _cB(new T(function(){return B(_dH(_dK));}),_dM);});},_dN=function(_dO){return function(_3D){return new F(function(){return _21(new T(function(){return B(_cB(new T(function(){return B(_dH(_dO));}),_S));}),_3D);});};},_dP=function(_dQ,_dR){return function(_3D){return new F(function(){return _21(new T(function(){return B(A(_dH,[_dQ,_dR,_S]));}),_3D);});};},_dS=function(_dT){return [0,function(_cy){return new F(function(){return _dP(_dT,_cy);});},new T(function(){return B(_dN(_dT));}),new T(function(){return B(_dH(_dT));}),function(_cx,_cy){return new F(function(){return _dJ(_dT,_cx,_cy);});}];},_dU=new T(function(){return B(_dS(_d6));}),_dV=new T(function(){return B(_dt(_dU,_dU,_d6,_S));}),_dW=function(_dX,_){return new T(function(){var _dY=B(_21(_dV,_dX));if(!_dY[0]){var _dZ=E(_Q);}else{if(!E(_dY[2])[0]){var _e0=E(E(_dY[1])[1]),_e1=[1,_p,new T(function(){return B(A(_J,[_1,[1,function(_e2){return new F(function(){return _z(_e0[1],_e2);});},[1,function(_e2){return new F(function(){return _z(_e0[2],_e2);});},[1,function(_e3){return new F(function(){return _q(0,E(_e0[3])[1],_e3);});},_C]]],_P]));})];}else{var _e1=E(_Q);}var _dZ=_e1;}return _dZ;});},_e4=function(_e5){var _e6=B(A(_e5,[_])),_e7=_e6;return E(_e7);},_e8=function(_e9){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return eval(_e9);});});});},_ea=new T(function(){return [0,"(function(s,f){Haste[s] = f;})"];}),_eb=new T(function(){return B(_e8(E(_ea)[1]));}),_ec=function(_ed,_ee){return function(_ef,_){var _eg=B(A(new T(function(){return B(A(_eb,[E(E(_ee)[1])]));}),[B(A(_ed,[_ef])),_])),_eh=_eg;return _3e;};},_ei=new T(function(){return [0,"getMuxShape"];}),_ej=function(_ek){var _el=String(_ek),_em=_el;return new F(function(){return fromJSStr(_em);});},_en=new T(function(){return B(_e8("(function(f) {  return (function() {      return (function(){        var args=Array.prototype.slice.call(arguments,0);        args.push(0);        return E(B(A(f, args)));    });  });})"));}),_eo=function(_ep,_){return new F(function(){return A(_en,[E(_ep),_]);});},_eq=function(_er,_){return new F(function(){return _eo(_er,_);});},_es=function(_et){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return _eq(function(_eu){return new F(function(){return _e4(function(_){var _=0,_ev=B(A(_et,[B(_ej(_eu)),_])),_ew=_ev;return E(toJSStr(E(_ew)));});});},_);});});});},_ex=new T(function(){return B(_ec(_es,_ei));}),_ey=[1],_ez=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_eA=function(_eB){return new F(function(){return err(_ez);});},_eC=new T(function(){return B(_eA(_));}),_eD=function(_eE,_eF,_eG,_eH){var _eI=E(_eG);if(!_eI[0]){var _eJ=_eI[1],_eK=E(_eH);if(!_eK[0]){var _eL=_eK[1],_eM=_eK[2],_eN=_eK[3];if(_eL<=(imul(3,_eJ)|0)){return [0,(1+_eJ|0)+_eL|0,E(E(_eE)),_eF,E(_eI),E(_eK)];}else{var _eO=E(_eK[4]);if(!_eO[0]){var _eP=_eO[1],_eQ=_eO[2],_eR=_eO[3],_eS=_eO[4],_eT=E(_eK[5]);if(!_eT[0]){var _eU=_eT[1];if(_eP>=(imul(2,_eU)|0)){var _eV=function(_eW){var _eX=E(_eE),_eY=E(_eO[5]);return _eY[0]==0?[0,(1+_eJ|0)+_eL|0,E(_eQ),_eR,E([0,(1+_eJ|0)+_eW|0,E(_eX),_eF,E(_eI),E(_eS)]),E([0,(1+_eU|0)+_eY[1]|0,E(_eM),_eN,E(_eY),E(_eT)])]:[0,(1+_eJ|0)+_eL|0,E(_eQ),_eR,E([0,(1+_eJ|0)+_eW|0,E(_eX),_eF,E(_eI),E(_eS)]),E([0,1+_eU|0,E(_eM),_eN,E(_ey),E(_eT)])];},_eZ=E(_eS);return _eZ[0]==0?B(_eV(_eZ[1])):B(_eV(0));}else{return [0,(1+_eJ|0)+_eL|0,E(_eM),_eN,E([0,(1+_eJ|0)+_eP|0,E(E(_eE)),_eF,E(_eI),E(_eO)]),E(_eT)];}}else{return E(_eC);}}else{return E(_eC);}}}else{return [0,1+_eJ|0,E(E(_eE)),_eF,E(_eI),E(_ey)];}}else{var _f0=E(_eH);if(!_f0[0]){var _f1=_f0[1],_f2=_f0[2],_f3=_f0[3],_f4=_f0[5],_f5=E(_f0[4]);if(!_f5[0]){var _f6=_f5[1],_f7=_f5[2],_f8=_f5[3],_f9=_f5[4],_fa=E(_f4);if(!_fa[0]){var _fb=_fa[1];if(_f6>=(imul(2,_fb)|0)){var _fc=function(_fd){var _fe=E(_eE),_ff=E(_f5[5]);return _ff[0]==0?[0,1+_f1|0,E(_f7),_f8,E([0,1+_fd|0,E(_fe),_eF,E(_ey),E(_f9)]),E([0,(1+_fb|0)+_ff[1]|0,E(_f2),_f3,E(_ff),E(_fa)])]:[0,1+_f1|0,E(_f7),_f8,E([0,1+_fd|0,E(_fe),_eF,E(_ey),E(_f9)]),E([0,1+_fb|0,E(_f2),_f3,E(_ey),E(_fa)])];},_fg=E(_f9);return _fg[0]==0?B(_fc(_fg[1])):B(_fc(0));}else{return [0,1+_f1|0,E(_f2),_f3,E([0,1+_f6|0,E(E(_eE)),_eF,E(_ey),E(_f5)]),E(_fa)];}}else{return [0,3,E(_f7),_f8,E([0,1,E(E(_eE)),_eF,E(_ey),E(_ey)]),E([0,1,E(_f2),_f3,E(_ey),E(_ey)])];}}else{var _fh=E(_f4);return _fh[0]==0?[0,3,E(_f2),_f3,E([0,1,E(E(_eE)),_eF,E(_ey),E(_ey)]),E(_fh)]:[0,2,E(E(_eE)),_eF,E(_ey),E(_f0)];}}else{return [0,1,E(E(_eE)),_eF,E(_ey),E(_ey)];}}},_fi=function(_fj,_fk){return [0,1,E(E(_fj)),_fk,E(_ey),E(_ey)];},_fl=function(_fm,_fn,_fo){var _fp=E(_fo);if(!_fp[0]){return new F(function(){return _eD(_fp[2],_fp[3],_fp[4],B(_fl(_fm,_fn,_fp[5])));});}else{return new F(function(){return _fi(_fm,_fn);});}},_fq=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_fr=function(_fs){return new F(function(){return err(_fq);});},_ft=new T(function(){return B(_fr(_));}),_fu=function(_fv,_fw,_fx,_fy){var _fz=E(_fy);if(!_fz[0]){var _fA=_fz[1],_fB=E(_fx);if(!_fB[0]){var _fC=_fB[1],_fD=_fB[2],_fE=_fB[3];if(_fC<=(imul(3,_fA)|0)){return [0,(1+_fC|0)+_fA|0,E(E(_fv)),_fw,E(_fB),E(_fz)];}else{var _fF=E(_fB[4]);if(!_fF[0]){var _fG=_fF[1],_fH=E(_fB[5]);if(!_fH[0]){var _fI=_fH[1],_fJ=_fH[2],_fK=_fH[3],_fL=_fH[4];if(_fI>=(imul(2,_fG)|0)){var _fM=function(_fN){var _fO=E(_fH[5]);return _fO[0]==0?[0,(1+_fC|0)+_fA|0,E(_fJ),_fK,E([0,(1+_fG|0)+_fN|0,E(_fD),_fE,E(_fF),E(_fL)]),E([0,(1+_fA|0)+_fO[1]|0,E(E(_fv)),_fw,E(_fO),E(_fz)])]:[0,(1+_fC|0)+_fA|0,E(_fJ),_fK,E([0,(1+_fG|0)+_fN|0,E(_fD),_fE,E(_fF),E(_fL)]),E([0,1+_fA|0,E(E(_fv)),_fw,E(_ey),E(_fz)])];},_fP=E(_fL);return _fP[0]==0?B(_fM(_fP[1])):B(_fM(0));}else{return [0,(1+_fC|0)+_fA|0,E(_fD),_fE,E(_fF),E([0,(1+_fA|0)+_fI|0,E(E(_fv)),_fw,E(_fH),E(_fz)])];}}else{return E(_ft);}}else{return E(_ft);}}}else{return [0,1+_fA|0,E(E(_fv)),_fw,E(_ey),E(_fz)];}}else{var _fQ=E(_fx);if(!_fQ[0]){var _fR=_fQ[1],_fS=_fQ[2],_fT=_fQ[3],_fU=_fQ[5],_fV=E(_fQ[4]);if(!_fV[0]){var _fW=_fV[1],_fX=E(_fU);if(!_fX[0]){var _fY=_fX[1],_fZ=_fX[2],_g0=_fX[3],_g1=_fX[4];if(_fY>=(imul(2,_fW)|0)){var _g2=function(_g3){var _g4=E(_fX[5]);return _g4[0]==0?[0,1+_fR|0,E(_fZ),_g0,E([0,(1+_fW|0)+_g3|0,E(_fS),_fT,E(_fV),E(_g1)]),E([0,1+_g4[1]|0,E(E(_fv)),_fw,E(_g4),E(_ey)])]:[0,1+_fR|0,E(_fZ),_g0,E([0,(1+_fW|0)+_g3|0,E(_fS),_fT,E(_fV),E(_g1)]),E([0,1,E(E(_fv)),_fw,E(_ey),E(_ey)])];},_g5=E(_g1);return _g5[0]==0?B(_g2(_g5[1])):B(_g2(0));}else{return [0,1+_fR|0,E(_fS),_fT,E(_fV),E([0,1+_fY|0,E(E(_fv)),_fw,E(_fX),E(_ey)])];}}else{return [0,3,E(_fS),_fT,E(_fV),E([0,1,E(E(_fv)),_fw,E(_ey),E(_ey)])];}}else{var _g6=E(_fU);return _g6[0]==0?[0,3,E(_g6[2]),_g6[3],E([0,1,E(_fS),_fT,E(_ey),E(_ey)]),E([0,1,E(E(_fv)),_fw,E(_ey),E(_ey)])]:[0,2,E(E(_fv)),_fw,E(_fQ),E(_ey)];}}else{return [0,1,E(E(_fv)),_fw,E(_ey),E(_ey)];}}},_g7=function(_g8,_g9,_ga){var _gb=E(_ga);if(!_gb[0]){return new F(function(){return _fu(_gb[2],_gb[3],B(_g7(_g8,_g9,_gb[4])),_gb[5]);});}else{return new F(function(){return _fi(_g8,_g9);});}},_gc=function(_gd,_ge,_gf,_gg,_gh,_gi,_gj){return new F(function(){return _fu(_gg,_gh,B(_g7(_gd,_ge,_gi)),_gj);});},_gk=function(_gl,_gm,_gn,_go,_gp,_gq,_gr,_gs){var _gt=E(_gn);if(!_gt[0]){var _gu=_gt[1],_gv=_gt[2],_gw=_gt[3],_gx=_gt[4],_gy=_gt[5];if((imul(3,_gu)|0)>=_go){if((imul(3,_go)|0)>=_gu){return [0,(_gu+_go|0)+1|0,E(E(_gl)),_gm,E(_gt),E([0,_go,E(_gp),_gq,E(_gr),E(_gs)])];}else{return new F(function(){return _eD(_gv,_gw,_gx,B(_gk(_gl,_gm,_gy,_go,_gp,_gq,_gr,_gs)));});}}else{return new F(function(){return _fu(_gp,_gq,B(_gz(_gl,_gm,_gu,_gv,_gw,_gx,_gy,_gr)),_gs);});}}else{return new F(function(){return _gc(_gl,_gm,_go,_gp,_gq,_gr,_gs);});}},_gz=function(_gA,_gB,_gC,_gD,_gE,_gF,_gG,_gH){var _gI=E(_gH);if(!_gI[0]){var _gJ=_gI[1],_gK=_gI[2],_gL=_gI[3],_gM=_gI[4],_gN=_gI[5];if((imul(3,_gC)|0)>=_gJ){if((imul(3,_gJ)|0)>=_gC){return [0,(_gC+_gJ|0)+1|0,E(E(_gA)),_gB,E([0,_gC,E(_gD),_gE,E(_gF),E(_gG)]),E(_gI)];}else{return new F(function(){return _eD(_gD,_gE,_gF,B(_gk(_gA,_gB,_gG,_gJ,_gK,_gL,_gM,_gN)));});}}else{return new F(function(){return _fu(_gK,_gL,B(_gz(_gA,_gB,_gC,_gD,_gE,_gF,_gG,_gM)),_gN);});}}else{return new F(function(){return _fl(_gA,_gB,[0,_gC,E(_gD),_gE,E(_gF),E(_gG)]);});}},_gO=function(_gP,_gQ,_gR,_gS){var _gT=E(_gR);if(!_gT[0]){var _gU=_gT[1],_gV=_gT[2],_gW=_gT[3],_gX=_gT[4],_gY=_gT[5],_gZ=E(_gS);if(!_gZ[0]){var _h0=_gZ[1],_h1=_gZ[2],_h2=_gZ[3],_h3=_gZ[4],_h4=_gZ[5];if((imul(3,_gU)|0)>=_h0){if((imul(3,_h0)|0)>=_gU){return [0,(_gU+_h0|0)+1|0,E(E(_gP)),_gQ,E(_gT),E(_gZ)];}else{return new F(function(){return _eD(_gV,_gW,_gX,B(_gk(_gP,_gQ,_gY,_h0,_h1,_h2,_h3,_h4)));});}}else{return new F(function(){return _fu(_h1,_h2,B(_gz(_gP,_gQ,_gU,_gV,_gW,_gX,_gY,_h3)),_h4);});}}else{return new F(function(){return _fl(_gP,_gQ,_gT);});}}else{return new F(function(){return _g7(_gP,_gQ,_gS);});}},_h5=function(_h6,_h7,_h8,_h9){var _ha=E(_h6);if(_ha==1){var _hb=E(_h9);return _hb[0]==0?[0,[0,1,E([0,_h7]),_h8,E(_ey),E(_ey)],_C,_C]:_h7<E(E(_hb[1])[1])[1]?[0,[0,1,E([0,_h7]),_h8,E(_ey),E(_ey)],_hb,_C]:[0,[0,1,E([0,_h7]),_h8,E(_ey),E(_ey)],_C,_hb];}else{var _hc=B(_h5(_ha>>1,_h7,_h8,_h9)),_hd=_hc[1],_he=_hc[3],_hf=E(_hc[2]);if(!_hf[0]){return [0,_hd,_C,_he];}else{var _hg=E(_hf[1]),_hh=_hg[1],_hi=_hg[2],_hj=E(_hf[2]);if(!_hj[0]){return [0,new T(function(){return B(_fl(_hh,_hi,_hd));}),_C,_he];}else{var _hk=E(_hj[1]),_hl=E(_hh),_hm=E(_hk[1])[1];if(_hl[1]<_hm){var _hn=B(_h5(_ha>>1,_hm,_hk[2],_hj[2]));return [0,new T(function(){return B(_gO(_hl,_hi,_hd,_hn[1]));}),_hn[2],_hn[3]];}else{return [0,_hd,_C,_hf];}}}}},_ho=function(_hp,_hq,_hr){var _hs=E(_hr);if(!_hs[0]){var _ht=_hs[3],_hu=_hs[4],_hv=_hs[5],_hw=E(_hs[2]),_hx=_hw[1];if(_hp>=_hx){if(_hp!=_hx){return new F(function(){return _eD(_hw,_ht,_hu,B(_ho(_hp,_hq,_hv)));});}else{return [0,_hs[1],E([0,_hp]),_hq,E(_hu),E(_hv)];}}else{return new F(function(){return _fu(_hw,_ht,B(_ho(_hp,_hq,_hu)),_hv);});}}else{return [0,1,E([0,_hp]),_hq,E(_ey),E(_ey)];}},_hy=function(_hz,_hA){while(1){var _hB=E(_hA);if(!_hB[0]){return E(_hz);}else{var _hC=E(_hB[1]),_hD=B(_ho(E(_hC[1])[1],_hC[2],_hz));_hA=_hB[2];_hz=_hD;continue;}}},_hE=function(_hF,_hG,_hH,_hI){return new F(function(){return _hy(B(_ho(_hG,_hH,_hF)),_hI);});},_hJ=function(_hK,_hL,_hM){var _hN=E(_hL);return new F(function(){return _hy(B(_ho(E(_hN[1])[1],_hN[2],_hK)),_hM);});},_hO=function(_hP,_hQ,_hR){while(1){var _hS=E(_hR);if(!_hS[0]){return E(_hQ);}else{var _hT=E(_hS[1]),_hU=_hT[1],_hV=_hT[2],_hW=E(_hS[2]);if(!_hW[0]){return new F(function(){return _fl(_hU,_hV,_hQ);});}else{var _hX=E(_hW[1]),_hY=E(_hU),_hZ=_hY[1],_i0=E(_hX[1])[1];if(_hZ<_i0){var _i1=B(_h5(_hP,_i0,_hX[2],_hW[2])),_i2=_i1[1],_i3=E(_i1[3]);if(!_i3[0]){var _i4=_hP<<1,_i5=B(_gO(_hY,_hV,_hQ,_i2));_hR=_i1[2];_hP=_i4;_hQ=_i5;continue;}else{return new F(function(){return _hJ(B(_gO(_hY,_hV,_hQ,_i2)),_i3[1],_i3[2]);});}}else{return new F(function(){return _hE(_hQ,_hZ,_hV,_hW);});}}}}},_i6=function(_i7,_i8,_i9,_ia,_ib){var _ic=E(_ib);if(!_ic[0]){return new F(function(){return _fl([0,_i9],_ia,_i8);});}else{var _id=E(_ic[1]),_ie=E(_id[1])[1];if(_i9<_ie){var _if=B(_h5(_i7,_ie,_id[2],_ic[2])),_ig=_if[1],_ih=E(_if[3]);if(!_ih[0]){return new F(function(){return _hO(_i7<<1,B(_gO([0,_i9],_ia,_i8,_ig)),_if[2]);});}else{return new F(function(){return _hJ(B(_gO([0,_i9],_ia,_i8,_ig)),_ih[1],_ih[2]);});}}else{return new F(function(){return _hE(_i8,_i9,_ia,_ic);});}}},_ii=function(_ij){var _ik=E(_ij);if(!_ik[0]){return [1];}else{var _il=E(_ik[1]),_im=_il[1],_in=_il[2],_io=E(_ik[2]);if(!_io[0]){return [0,1,E(E(_im)),_in,E(_ey),E(_ey)];}else{var _ip=_io[2],_iq=E(_io[1]),_ir=_iq[2],_is=E(_im),_it=E(_iq[1])[1];return _is[1]<_it?B(_i6(1,[0,1,E(_is),_in,E(_ey),E(_ey)],_it,_ir,_ip)):B(_hE([0,1,E(_is),_in,E(_ey),E(_ey)],_it,_ir,_ip));}}},_iu=function(_iv,_iw){while(1){var _ix=E(_iv);if(!_ix[0]){return E(_iw);}else{_iv=_ix[2];var _iy=_iw+1|0;_iw=_iy;continue;}}},_iz=function(_iA,_iB){while(1){var _iC=E(_iB);if(!_iC[0]){var _iD=E(_iC[2])[1];if(_iA>=_iD){if(_iA!=_iD){_iB=_iC[5];continue;}else{return [1,_iC[3]];}}else{_iB=_iC[4];continue;}}else{return [0];}}},_iE=[2],_iF=[1,_iE,_C],_iG=function(_iH){return _iH>1?[1,_iE,new T(function(){return B(_iG(_iH-1|0));})]:E(_iF);},_iI=function(_iJ,_iK){var _iL=E(_iJ);if(!_iL[0]){return [0];}else{var _iM=E(_iK);return _iM[0]==0?[0]:[1,[0,_iL[1],_iM[1]],new T(function(){return B(_iI(_iL[2],_iM[2]));})];}},_iN=function(_iO,_iP,_iQ){var _iR=_iP-1|0;if(0<=_iR){var _iS=function(_iT){return [1,new T(function(){var _iU=B(_iz(_iT,new T(function(){return B(_ii(B(_iI(_iO,new T(function(){var _iV=B(_iu(_iO,0));if(B(_iu(_iQ,0))!=_iV){var _iW=_iV>0?B(_iG(_iV)):[0];}else{var _iW=E(_iQ);}var _iX=_iW,_iY=_iX,_iZ=_iY;return _iZ;},1)))));})));return _iU[0]==0?[3]:E(_iU[1]);}),new T(function(){if(_iT!=_iR){var _j0=B(_iS(_iT+1|0));}else{var _j0=[0];}var _j1=_j0;return _j1;})];};return new F(function(){return _iS(0);});}else{return [0];}},_j2=function(_j3,_j4){while(1){var _j5=E(_j4);if(!_j5[0]){return false;}else{if(!B(A(_j3,[_j5[1]]))){_j4=_j5[2];continue;}else{return true;}}}},_j6=function(_j7){return E(_j7)[0]==2?true:false;},_j8=function(_j9){return E(_j9)[0]==1?true:false;},_ja=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_jb=new T(function(){return B(err(_ja));}),_jc=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_jd=new T(function(){return B(err(_jc));}),_je=function(_jf,_jg){while(1){var _jh=E(_jf);if(!_jh[0]){return E(_jd);}else{var _ji=E(_jg);if(!_ji){return E(_jh[1]);}else{_jf=_jh[2];_jg=_ji-1|0;continue;}}}},_jj=function(_jk,_jl){var _jm=E(_jl);return _jm[0]==0?[0]:[1,new T(function(){return B(A(_jk,[_jm[1]]));}),new T(function(){return B(_jj(_jk,_jm[2]));})];},_jn=function(_jo,_jp){return new F(function(){return _jj(function(_jq){var _jr=E(_jq)[1];return _jr>=B(_iu(_jp,0))?[2]:_jr>=0?B(_je(_jp,_jr)):E(_jb);},_jo);});},_js=function(_jt){return _jt>1?[1,_iE,new T(function(){return B(_js(_jt-1|0));})]:E(_iF);},_ju=[1],_jv=[1,_ju,_C],_jw=function(_jx){return _jx>1?[1,_ju,new T(function(){return B(_jw(_jx-1|0));})]:E(_jv);},_jy=function(_jz,_jA,_jB,_jC,_jD){return new F(function(){return _iN(_jB,_jC,new T(function(){var _jE=B(_jn(_jA,_jD));if(!B(_j2(_j6,_jE))){if(!B(_j2(_j8,_jE))){var _jF=B(A(_jz,[_jE]));}else{var _jF=_jC>0?B(_jw(_jC)):[0];}var _jG=_jF;}else{var _jG=_jC>0?B(_js(_jC)):[0];}var _jH=_jG;return _jH;}));});},_jI=function(_jJ){return E(E(_jJ)[2]);},_jK=function(_jL){return [0,new T(function(){return B(_jI(_jL));})];},_jM=function(_jN){return [0,_jN];},_jO=function(_jP,_jQ){while(1){var _jR=E(_jQ);if(!_jR[0]){return true;}else{if(!B(A(_jP,[_jR[1]]))){return false;}else{_jQ=_jR[2];continue;}}}},_jS=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_jT=new T(function(){return B(err(_jS));}),_jU=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_jV=new T(function(){return B(err(_jU));}),_jW=function(_jX,_jY,_jZ){var _k0=function(_k1,_k2){return new F(function(){return _2b([1,function(_k3){return new F(function(){return A(_ag,[_k3,function(_k4){return E(new T(function(){return B(_bw(function(_k5){var _k6=E(_k5);if(_k6[0]==4){var _k7=E(_k6[1]);if(!_k7[0]){return new F(function(){return A(_jX,[_k6,_k1,_k2]);});}else{return E(E(_k7[1])[1])==45?E(_k7[2])[0]==0?E([1,function(_k8){return new F(function(){return A(_ag,[_k8,function(_k9){return E(new T(function(){return B(_bw(function(_ka){return new F(function(){return A(_jX,[_ka,_k1,function(_kb){return new F(function(){return A(_k2,[new T(function(){return B(_4C(_kb));})]);});}]);});}));}));}]);});}]):B(A(_jX,[_k6,_k1,_k2])):B(A(_jX,[_k6,_k1,_k2]));}}else{return new F(function(){return A(_jX,[_k6,_k1,_k2]);});}}));}));}]);});}],new T(function(){return [1,B(_bS(_k0,_k2))];}));});};return new F(function(){return _k0(_jY,_jZ);});},_kc=function(_kd,_ke){return [2];},_kf=function(_kg){var _kh=E(_kg);if(_kh[0]==5){var _ki=B(_cn(_kh[1]));return _ki[0]==0?E(_kc):function(_kj,_kk){return new F(function(){return A(_kk,[_ki[1]]);});};}else{return E(_kc);}},_kl=function(_cx,_cy){return new F(function(){return _jW(_kf,_cx,_cy);});},_km=function(_kn){return [1,function(_ko){return new F(function(){return A(_ag,[_ko,function(_kp){return E([3,_kn,_R]);}]);});}];},_kq=new T(function(){return B(_cB(_kl,_km));}),_kr=function(_ks){return E(_ks)[0]==0?true:false;},_kt=function(_ku){while(1){var _kv=(function(_kw){var _kx=E(_kw);if(!_kx[0]){return [0];}else{var _ky=_kx[2],_kz=E(_kx[1]);if(!E(_kz[2])[0]){return [1,_kz[1],new T(function(){return B(_kt(_ky));})];}else{_ku=_ky;return null;}}})(_ku);if(_kv!=null){return _kv;}}},_kA=function(_kB){var _kC=E(_kB);return _kC[0]==0?E(_kC[1]):B(_1Y("CoinKernel.hs:57:61-78|lambda"));},_kD=function(_kE,_kF){if(!B(_jO(_kr,_kF))){return [0];}else{if(!B(_jj(_kA,_kF))[0]){var _kG=B(_kt(B(_21(_kq,_kE))));return _kG[0]==0?E(_jV):E(_kG[2])[0]==0?B(_jj(_jM,_kG[1])):E(_jT);}else{return [0];}}},_kH=[0,1],_kI=[0,_kH,_kD],_kJ=[1,_kI,_C],_kK=function(_kL,_kM){var _kN=E(_kL);if(!_kN[0]){var _kO=_kN[1],_kP=E(_kM);return _kP[0]==0?_kO==_kP[1]:I_compareInt(_kP[1],_kO)==0?true:false;}else{var _kQ=_kN[1],_kR=E(_kM);return _kR[0]==0?I_compareInt(_kQ,_kR[1])==0?true:false:I_compare(_kQ,_kR[1])==0?true:false;}},_kS=[0,0],_kT=function(_kU,_kV){while(1){var _kW=E(_kU);if(!_kW[0]){return E(_kV);}else{_kU=_kW[2];var _kX=B(_4s(_kV,_kW[1]));_kV=_kX;continue;}}},_kY=function(_kZ){return new F(function(){return _kT(_kZ,_kS);});},_l0=function(_l1,_l2){var _l3=B(_kt(B(_21(_kq,_l1))));if(!_l3[0]){return E(_jV);}else{var _l4=_l3[1];return E(_l3[2])[0]==0?!B(_kK(B(_kY(_l2)),B(_kY(_l4))))?[0]:E(_l4):E(_jT);}},_l5=function(_l6,_l7){if(!B(_jO(_kr,_l7))){return [0];}else{return new F(function(){return _jj(_jM,B(_l0(_l6,B(_jj(_kA,_l7)))));});}},_l8=[0,0],_l9=[0,_l8,_l5],_la=[1,_l9,_kJ],_lb=new T(function(){return B(_ii(_la));}),_lc=function(_ld,_le){if(_ld<=_le){var _lf=function(_lg){return [1,[0,_lg],new T(function(){if(_lg!=_le){var _lh=B(_lf(_lg+1|0));}else{var _lh=[0];}var _li=_lh;return _li;})];};return new F(function(){return _lf(_ld);});}else{return [0];}},_lj=new T(function(){return B(_lc(0,2147483647));}),_lk=function(_ll){while(1){var _lm=E(_ll);if(!_lm[0]){_ll=[1,I_fromInt(_lm[1])];continue;}else{return new F(function(){return I_toString(_lm[1]);});}}},_ln=function(_lo,_lp){return new F(function(){return _f(fromJSStr(B(_lk(_lo))),_lp);});},_lq=function(_lr,_ls){var _lt=E(_lr);if(!_lt[0]){var _lu=_lt[1],_lv=E(_ls);return _lv[0]==0?_lu<_lv[1]:I_compareInt(_lv[1],_lu)>0;}else{var _lw=_lt[1],_lx=E(_ls);return _lx[0]==0?I_compareInt(_lw,_lx[1])<0:I_compare(_lw,_lx[1])<0;}},_ly=[0,0],_lz=function(_lA,_lB,_lC){return _lA<=6?B(_ln(_lB,_lC)):!B(_lq(_lB,_ly))?B(_ln(_lB,_lC)):[1,_p,new T(function(){return B(_f(fromJSStr(B(_lk(_lB))),[1,_o,_lC]));})];},_lD=[0,125],_lE=[1,_lD,_C],_lF=[0,78],_lG=[0,73],_lH=[0,77],_lI=[0,123],_lJ=function(_lK,_lL,_lM,_lN){return E(toJSStr([1,_lI,new T(function(){return B(unAppCStr("\"txid\" : \"",new T(function(){return B(_f(_lK,new T(function(){return B(unAppCStr("\", ",new T(function(){return B(unAppCStr("\"index\" : ",new T(function(){return B(_f(B(_q(0,E(_lL)[1],_C)),new T(function(){return B(unAppCStr(", ",new T(function(){return B(unAppCStr("\"coinstate\" : \"",new T(function(){var _lO=new T(function(){return B(unAppCStr("\", ",new T(function(){return B(unAppCStr("\"value\" : ",new T(function(){return B(_f(B(_lz(0,_lN,_C)),_lE));})));})));}),_lP=E(_lM);switch(_lP[0]){case 0:var _lQ=B(_f(B(_lz(0,_lP[1],_C)),_lO));break;case 1:var _lQ=[1,_lH,_lO];break;case 2:var _lQ=[1,_lG,_lO];break;default:var _lQ=[1,_lF,_lO];}return _lQ;})));})));},1)));})));})));},1)));})));})]));},_lR=function(_lS){var _lT=E(_lS),_lU=E(_lT[1]);return [0,B(_lJ(_lU[1],_lU[2],_lT[2],_lT[3]))];},_lV=new T(function(){return B(_c4(_cq,_bR,_S));}),_lW=function(_lX){return [0];},_lY=function(_lZ,_m0){var _m1=B(_21(_lV,_m0));if(!_m1[0]){return E(_lW);}else{if(!E(_m1[2])[0]){var _m2=E(_m1[1]),_m3=B(_iz(E(_m2[1])[1],_lZ));return _m3[0]==0?E(_lW):B(A(_m3[1],[_m2[2]]));}else{return E(_lW);}}},_m4=function(_m5,_m6,_m7){var _m8=E(_m5);if(!_m8[0]){return [0];}else{var _m9=E(_m6);if(!_m9[0]){return [0];}else{var _ma=E(_m7);return _ma[0]==0?[0]:[1,[0,_m8[1],_m9[1],_ma[1]],new T(function(){return B(_m4(_m8[2],_m9[2],_ma[2]));})];}}},_mb=function(_mc,_md,_me,_){var _mf=E(_mc);return new T(function(){var _mg=function(_mh){var _mi=E(_mh);return _mi[0]==0?[0]:[1,[0,_mf[2],_mi[1]],new T(function(){return B(_mg(_mi[2]));})];};return B(_jj(_lR,B(_m4(B(_mg(_lj)),new T(function(){var _mj=B(_21(_dV,_mf[1]));if(!_mj[0]){var _mk=[0];}else{if(!E(_mj[2])[0]){var _ml=E(_mj[1]),_mm=E(_ml[1]),_mn=B(_jy(new T(function(){return B(_lY(_lb,_ml[2]));},1),_mm[1],_mm[2],E(_mm[3])[1],new T(function(){return B(_jj(_jK,_md));})));}else{var _mn=[0];}var _mk=_mn;}return _mk;},1),_me))));});},_mo=function(_mp,_mq){while(1){var _mr=E(_mp);if(!_mr[0]){return E(_mq)[0]==0?1:0;}else{var _ms=E(_mq);if(!_ms[0]){return 2;}else{var _mt=E(_mr[1])[1],_mu=E(_ms[1])[1];if(_mt!=_mu){return _mt>_mu?2:0;}else{_mp=_mr[2];_mq=_ms[2];continue;}}}}},_mv=function(_mw,_mx){return B(_mo(_mw,_mx))==0?true:false;},_my=function(_mz,_mA){return B(_mo(_mz,_mA))==2?false:true;},_mB=function(_mC,_mD){return B(_mo(_mC,_mD))==2?true:false;},_mE=function(_mF,_mG){return B(_mo(_mF,_mG))==0?false:true;},_mH=function(_mI,_mJ){return B(_mo(_mI,_mJ))==2?E(_mI):E(_mJ);},_mK=function(_mL,_mM){return B(_mo(_mL,_mM))==2?E(_mM):E(_mL);},_mN=[0,_32,_mo,_mv,_mE,_mB,_my,_mH,_mK],_mO=function(_mP,_mQ,_mR,_mS){var _mT=E(_mP);if(_mT==1){var _mU=E(_mS);return _mU[0]==0?[0,new T(function(){return [0,1,E(E(_mQ)),_mR,E(_ey),E(_ey)];}),_C,_C]:B(_mo(_mQ,E(_mU[1])[1]))==0?[0,new T(function(){return [0,1,E(E(_mQ)),_mR,E(_ey),E(_ey)];}),_mU,_C]:[0,new T(function(){return [0,1,E(E(_mQ)),_mR,E(_ey),E(_ey)];}),_C,_mU];}else{var _mV=B(_mO(_mT>>1,_mQ,_mR,_mS)),_mW=_mV[1],_mX=_mV[3],_mY=E(_mV[2]);if(!_mY[0]){return [0,_mW,_C,_mX];}else{var _mZ=E(_mY[1]),_n0=_mZ[1],_n1=_mZ[2],_n2=E(_mY[2]);if(!_n2[0]){return [0,new T(function(){return B(_fl(_n0,_n1,_mW));}),_C,_mX];}else{var _n3=E(_n2[1]),_n4=_n3[1];if(!B(_mo(_n0,_n4))){var _n5=B(_mO(_mT>>1,_n4,_n3[2],_n2[2]));return [0,new T(function(){return B(_gO(_n0,_n1,_mW,_n5[1]));}),_n5[2],_n5[3]];}else{return [0,_mW,_C,_mY];}}}}},_n6=function(_n7,_n8,_n9){var _na=E(_n7),_nb=E(_n9);if(!_nb[0]){var _nc=_nb[2],_nd=_nb[3],_ne=_nb[4],_nf=_nb[5];switch(B(_mo(_na,_nc))){case 0:return new F(function(){return _fu(_nc,_nd,B(_n6(_na,_n8,_ne)),_nf);});break;case 1:return [0,_nb[1],E(_na),_n8,E(_ne),E(_nf)];default:return new F(function(){return _eD(_nc,_nd,_ne,B(_n6(_na,_n8,_nf)));});}}else{return [0,1,E(_na),_n8,E(_ey),E(_ey)];}},_ng=function(_nh,_ni){while(1){var _nj=E(_ni);if(!_nj[0]){return E(_nh);}else{var _nk=E(_nj[1]),_nl=B(_n6(_nk[1],_nk[2],_nh));_ni=_nj[2];_nh=_nl;continue;}}},_nm=function(_nn,_no,_np,_nq){return new F(function(){return _ng(B(_n6(_no,_np,_nn)),_nq);});},_nr=function(_ns,_nt,_nu){var _nv=E(_nt);return new F(function(){return _ng(B(_n6(_nv[1],_nv[2],_ns)),_nu);});},_nw=function(_nx,_ny,_nz){while(1){var _nA=E(_nz);if(!_nA[0]){return E(_ny);}else{var _nB=E(_nA[1]),_nC=_nB[1],_nD=_nB[2],_nE=E(_nA[2]);if(!_nE[0]){return new F(function(){return _fl(_nC,_nD,_ny);});}else{var _nF=E(_nE[1]),_nG=_nF[1];if(!B(_mo(_nC,_nG))){var _nH=B(_mO(_nx,_nG,_nF[2],_nE[2])),_nI=_nH[1],_nJ=E(_nH[3]);if(!_nJ[0]){var _nK=_nx<<1,_nL=B(_gO(_nC,_nD,_ny,_nI));_nz=_nH[2];_nx=_nK;_ny=_nL;continue;}else{return new F(function(){return _nr(B(_gO(_nC,_nD,_ny,_nI)),_nJ[1],_nJ[2]);});}}else{return new F(function(){return _nm(_ny,_nC,_nD,_nE);});}}}}},_nM=function(_nN,_nO,_nP,_nQ,_nR){var _nS=E(_nR);if(!_nS[0]){return new F(function(){return _fl(_nP,_nQ,_nO);});}else{var _nT=E(_nS[1]),_nU=_nT[1];if(!B(_mo(_nP,_nU))){var _nV=B(_mO(_nN,_nU,_nT[2],_nS[2])),_nW=_nV[1],_nX=E(_nV[3]);if(!_nX[0]){return new F(function(){return _nw(_nN<<1,B(_gO(_nP,_nQ,_nO,_nW)),_nV[2]);});}else{return new F(function(){return _nr(B(_gO(_nP,_nQ,_nO,_nW)),_nX[1],_nX[2]);});}}else{return new F(function(){return _nm(_nO,_nP,_nQ,_nS);});}}},_nY=function(_nZ){var _o0=E(_nZ);if(!_o0[0]){return [1];}else{var _o1=E(_o0[1]),_o2=_o1[1],_o3=_o1[2],_o4=E(_o0[2]);if(!_o4[0]){return [0,1,E(E(_o2)),_o3,E(_ey),E(_ey)];}else{var _o5=_o4[2],_o6=E(_o4[1]),_o7=_o6[1],_o8=_o6[2];if(!B(_mo(_o2,_o7))){return new F(function(){return _nM(1,[0,1,E(E(_o2)),_o3,E(_ey),E(_ey)],_o7,_o8,_o5);});}else{return new F(function(){return _nm([0,1,E(E(_o2)),_o3,E(_ey),E(_ey)],_o7,_o8,_o5);});}}}},_o9=function(_oa){return E(E(_oa)[2]);},_ob=new T(function(){return B(unCStr("Map.!: given key is not an element in the map"));}),_oc=new T(function(){return B(err(_ob));}),_od=function(_oe,_of,_og){return new F(function(){return (function(_oh,_oi){while(1){var _oj=E(_oh),_ok=E(_oi);if(!_ok[0]){switch(B(A(new T(function(){return B(_o9(_oe));}),[_oj,_ok[2]]))){case 0:_oh=_oj;_oi=_ok[4];continue;case 1:return E(_ok[3]);default:_oh=_oj;_oi=_ok[5];continue;}}else{return E(_oc);}}})(_of,_og);});},_ol=function(_om){return E(E(_om)[1]);},_on=function(_oo,_op,_oq,_or){var _os=E(_op),_ot=E(_or);if(!_ot[0]){var _ou=_ot[2],_ov=_ot[3],_ow=_ot[4],_ox=_ot[5];switch(B(A(_o9,[_oo,_os,_ou]))){case 0:return new F(function(){return _fu(_ou,_ov,B(_on(_oo,_os,_oq,_ow)),_ox);});break;case 1:return [0,_ot[1],E(_os),_oq,E(_ow),E(_ox)];default:return new F(function(){return _eD(_ou,_ov,_ow,B(_on(_oo,_os,_oq,_ox)));});}}else{return [0,1,E(_os),_oq,E(_ey),E(_ey)];}},_oy=function(_oz,_oA,_oB,_oC){return new F(function(){return _on(_oz,_oA,_oB,_oC);});},_oD=function(_oE,_oF,_oG){return new F(function(){return (function(_oH,_oI){while(1){var _oJ=E(_oH),_oK=E(_oI);if(!_oK[0]){switch(B(A(new T(function(){return B(_o9(_oE));}),[_oJ,_oK[2]]))){case 0:_oH=_oJ;_oI=_oK[4];continue;case 1:return true;default:_oH=_oJ;_oI=_oK[5];continue;}}else{return false;}}})(_oF,_oG);});},_oL=function(_oM,_oN,_oO){var _oP=function(_oQ,_oR,_oS){while(1){var _oT=(function(_oU,_oV,_oW){var _oX=E(_oU);if(!_oX[0]){return [0,_oV,_oW];}else{var _oY=_oX[1],_oZ=_oX[2];if(!B(_oD(_oM,_oY,_oV))){if(!B(_oD(_oM,_oY,_oN))){_oQ=_oZ;var _p0=_oV,_p1=_oW;_oR=_p0;_oS=_p1;return null;}else{var _p2=new T(function(){var _p3=B(_od(_oM,_oY,_oN));return [0,_p3,_p3[1]];}),_p4=new T(function(){return E(E(_p2)[1]);}),_p5=new T(function(){var _p6=B(_oP(B(_jj(_ol,E(_p2)[2])),_oV,_oW));return [0,_p6[1],_p6[2]];});_oQ=_oZ;_oR=new T(function(){return B(_oy(_oM,_oY,_p4,E(_p5)[1]));});_oS=[1,[0,_oY,_p4],new T(function(){return E(E(_p5)[2]);})];return null;}}else{_oQ=_oZ;var _p0=_oV,_p1=_oW;_oR=_p0;_oS=_p1;return null;}}})(_oQ,_oR,_oS);if(_oT!=null){return _oT;}}};return new F(function(){return _oP(_oO,_ey,_C);});},_p7=[0,0],_p8=function(_p9){return E(E(_p9)[1]);},_pa=function(_pb,_pc,_pd,_pe){return new F(function(){return _7(function(_pf,_pg){var _ph=E(_pf);return [1,_p,new T(function(){return B(A(_J,[_1,[1,new T(function(){return B(A(new T(function(){return B(_p8(_pb));}),[_p7,_ph[1]]));}),[1,new T(function(){return B(A(new T(function(){return B(_p8(_pc));}),[_p7,_ph[2]]));}),_C]],[1,_o,_pg]]));})];},_pd,_pe);});},_pi=[0,34],_pj=function(_pk){return new F(function(){return _q(0,E(_pk)[1],_C);});},_pl=function(_pm,_pn,_po){return new F(function(){return _q(E(_pm)[1],E(_pn)[1],_po);});},_pp=[0,_pl,_pj,_z],_pq=new T(function(){return B(unCStr("Tx {"));}),_pr=[0,125],_ps=new T(function(){return B(unCStr("outputCount = "));}),_pt=new T(function(){return B(unCStr("txId = "));}),_pu=new T(function(){return B(unCStr("ACK"));}),_pv=new T(function(){return B(unCStr("BEL"));}),_pw=new T(function(){return B(unCStr("BS"));}),_px=new T(function(){return B(unCStr("SP"));}),_py=[1,_px,_C],_pz=new T(function(){return B(unCStr("US"));}),_pA=[1,_pz,_py],_pB=new T(function(){return B(unCStr("RS"));}),_pC=[1,_pB,_pA],_pD=new T(function(){return B(unCStr("GS"));}),_pE=[1,_pD,_pC],_pF=new T(function(){return B(unCStr("FS"));}),_pG=[1,_pF,_pE],_pH=new T(function(){return B(unCStr("ESC"));}),_pI=[1,_pH,_pG],_pJ=new T(function(){return B(unCStr("SUB"));}),_pK=[1,_pJ,_pI],_pL=new T(function(){return B(unCStr("EM"));}),_pM=[1,_pL,_pK],_pN=new T(function(){return B(unCStr("CAN"));}),_pO=[1,_pN,_pM],_pP=new T(function(){return B(unCStr("ETB"));}),_pQ=[1,_pP,_pO],_pR=new T(function(){return B(unCStr("SYN"));}),_pS=[1,_pR,_pQ],_pT=new T(function(){return B(unCStr("NAK"));}),_pU=[1,_pT,_pS],_pV=new T(function(){return B(unCStr("DC4"));}),_pW=[1,_pV,_pU],_pX=new T(function(){return B(unCStr("DC3"));}),_pY=[1,_pX,_pW],_pZ=new T(function(){return B(unCStr("DC2"));}),_q0=[1,_pZ,_pY],_q1=new T(function(){return B(unCStr("DC1"));}),_q2=[1,_q1,_q0],_q3=new T(function(){return B(unCStr("DLE"));}),_q4=[1,_q3,_q2],_q5=new T(function(){return B(unCStr("SI"));}),_q6=[1,_q5,_q4],_q7=new T(function(){return B(unCStr("SO"));}),_q8=[1,_q7,_q6],_q9=new T(function(){return B(unCStr("CR"));}),_qa=[1,_q9,_q8],_qb=new T(function(){return B(unCStr("FF"));}),_qc=[1,_qb,_qa],_qd=new T(function(){return B(unCStr("VT"));}),_qe=[1,_qd,_qc],_qf=new T(function(){return B(unCStr("LF"));}),_qg=[1,_qf,_qe],_qh=new T(function(){return B(unCStr("HT"));}),_qi=[1,_qh,_qg],_qj=[1,_pw,_qi],_qk=[1,_pv,_qj],_ql=[1,_pu,_qk],_qm=new T(function(){return B(unCStr("ENQ"));}),_qn=[1,_qm,_ql],_qo=new T(function(){return B(unCStr("EOT"));}),_qp=[1,_qo,_qn],_qq=new T(function(){return B(unCStr("ETX"));}),_qr=[1,_qq,_qp],_qs=new T(function(){return B(unCStr("STX"));}),_qt=[1,_qs,_qr],_qu=new T(function(){return B(unCStr("SOH"));}),_qv=[1,_qu,_qt],_qw=new T(function(){return B(unCStr("NUL"));}),_qx=[1,_qw,_qv],_qy=[0,92],_qz=new T(function(){return B(unCStr("\\DEL"));}),_qA=new T(function(){return B(unCStr("\\a"));}),_qB=new T(function(){return B(unCStr("\\\\"));}),_qC=new T(function(){return B(unCStr("\\SO"));}),_qD=new T(function(){return B(unCStr("\\r"));}),_qE=new T(function(){return B(unCStr("\\f"));}),_qF=new T(function(){return B(unCStr("\\v"));}),_qG=new T(function(){return B(unCStr("\\n"));}),_qH=new T(function(){return B(unCStr("\\t"));}),_qI=new T(function(){return B(unCStr("\\b"));}),_qJ=function(_qK,_qL){if(_qK<=127){var _qM=E(_qK);switch(_qM){case 92:return new F(function(){return _f(_qB,_qL);});break;case 127:return new F(function(){return _f(_qz,_qL);});break;default:if(_qM<32){var _qN=E(_qM);switch(_qN){case 7:return new F(function(){return _f(_qA,_qL);});break;case 8:return new F(function(){return _f(_qI,_qL);});break;case 9:return new F(function(){return _f(_qH,_qL);});break;case 10:return new F(function(){return _f(_qG,_qL);});break;case 11:return new F(function(){return _f(_qF,_qL);});break;case 12:return new F(function(){return _f(_qE,_qL);});break;case 13:return new F(function(){return _f(_qD,_qL);});break;case 14:return new F(function(){return _f(_qC,new T(function(){var _qO=E(_qL);if(!_qO[0]){var _qP=[0];}else{var _qP=E(E(_qO[1])[1])==72?B(unAppCStr("\\&",_qO)):E(_qO);}return _qP;},1));});break;default:return new F(function(){return _f([1,_qy,new T(function(){var _qQ=_qN;return _qQ>=0?B(_je(_qx,_qQ)):E(_jb);})],_qL);});}}else{return [1,[0,_qM],_qL];}}}else{return [1,_qy,new T(function(){var _qR=jsShowI(_qK),_qS=_qR;return B(_f(fromJSStr(_qS),new T(function(){var _qT=E(_qL);if(!_qT[0]){var _qU=[0];}else{var _qV=E(_qT[1])[1];if(_qV<48){var _qW=E(_qT);}else{var _qW=_qV>57?E(_qT):B(unAppCStr("\\&",_qT));}var _qX=_qW,_qY=_qX,_qU=_qY;}return _qU;},1)));})];}},_qZ=[0,39],_r0=[1,_qZ,_C],_r1=new T(function(){return B(unCStr("\'\\\'\'"));}),_r2=function(_r3){var _r4=E(E(_r3)[1]);return _r4==39?E(_r1):[1,_qZ,new T(function(){return B(_qJ(_r4,_r0));})];},_r5=new T(function(){return B(unCStr("\\\""));}),_r6=function(_r7,_r8){var _r9=E(_r7);if(!_r9[0]){return E(_r8);}else{var _ra=_r9[2],_rb=E(E(_r9[1])[1]);if(_rb==34){return new F(function(){return _f(_r5,new T(function(){return B(_r6(_ra,_r8));},1));});}else{return new F(function(){return _qJ(_rb,new T(function(){return B(_r6(_ra,_r8));}));});}}},_rc=function(_rd,_re){return [1,_pi,new T(function(){return B(_r6(_rd,[1,_pi,_re]));})];},_rf=function(_rg){return new F(function(){return _f(_r1,_rg);});},_rh=function(_ri,_rj){var _rk=E(E(_rj)[1]);return _rk==39?E(_rf):function(_rl){return [1,_qZ,new T(function(){return B(_qJ(_rk,[1,_qZ,_rl]));})];};},_rm=[0,_rh,_r2,_rc],_rn=function(_ro){return E(E(_ro)[3]);},_rp=function(_rq,_rr){return new F(function(){return A(_rn,[_rq,_rr,_C]);});},_rs=function(_rt,_ru,_rv){return new F(function(){return _7(new T(function(){return B(_rn(_rt));}),_ru,_rv);});},_rw=function(_rx){return [0,function(_ry){return E(new T(function(){return B(_rn(_rx));}));},function(_rg){return new F(function(){return _rp(_rx,_rg);});},function(_rz,_rg){return new F(function(){return _rs(_rx,_rz,_rg);});}];},_rA=new T(function(){return B(_rw(_rm));}),_rB=new T(function(){return B(unCStr("inputs = "));}),_rC=new T(function(){return B(unCStr(", "));}),_rD=new T(function(){return B(unCStr("payload = "));}),_rE=[0,0],_rF=function(_rG,_rH,_rI,_rJ,_rK,_rL){var _rM=function(_rN){return new F(function(){return _f(_rD,new T(function(){return B(A(new T(function(){return B(A(_p8,[_rG,_rE,_rI]));}),[new T(function(){return B(_f(_rC,new T(function(){return B(_f(_rB,new T(function(){return B(_pa(_rA,_pp,_rJ,new T(function(){return B(_f(_rC,new T(function(){return B(_f(_pt,[1,_pi,new T(function(){return B(_r6(_rK,[1,_pi,new T(function(){return B(_f(_rC,new T(function(){return B(_f(_ps,new T(function(){return B(_q(0,E(_rL)[1],[1,_pr,_rN]));},1)));},1)));})]));})]));},1)));})));},1)));},1)));})]));},1));});};return _rH<11?function(_rO){return new F(function(){return _f(_pq,new T(function(){return B(_rM(_rO));},1));});}:function(_rP){return [1,_p,new T(function(){return B(_f(_pq,new T(function(){return B(_rM([1,_o,_rP]));},1)));})];};},_rQ=new T(function(){return B(_rw(_rm));}),_rR=function(_rS){var _rT=new T(function(){return E(E(_rS)[2]);});return new F(function(){return A(_rF,[_rQ,0,new T(function(){return E(E(_rT)[2]);}),new T(function(){return E(E(_rT)[1]);}),new T(function(){return B(_ol(_rS));}),new T(function(){return E(E(_rT)[3]);}),_C]);});},_rU=function(_rV,_rW){while(1){var _rX=E(_rV);if(!_rX[0]){return E(_rW);}else{_rV=_rX[2];var _rY=[1,_rX[1],_rW];_rW=_rY;continue;}}},_rZ=function(_s0,_){return new T(function(){return B(_jj(_rR,B(_rU(B(_oL(_mN,new T(function(){return B(_nY(_s0));}),B(_jj(_ol,_s0))))[2],_C))));});},_s1=new T(function(){return [0,"runKernel"];}),_s2=function(_s3){return new F(function(){return _1Y("src/Haste/Foreign.hs:106:12-52|case");});},_s4=new T(function(){return B(_s2(_));}),_s5=new T(function(){return B(_e8("lst2arr"));}),_s6=new T(function(){return [0,"arr2lst"];}),_s7=function(_s8,_s9){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_e8,[E(_s6)[1],E(_s8),E(_s9),_]);});});});},_sa=function(_sb){return [0,_sb];},_sc=function(_sd){var _se=jsTrunc(_sd),_sf=_se;return [0,_sf];},_sg=function(_sh){var _si=B(_s7(_sh,0));if(!_si[0]){return E(_s4);}else{var _sj=E(_si[2]);return _sj[0]==0?E(_s4):E(_sj[2])[0]==0?[0,new T(function(){return B(_ej(_si[1]));}),new T(function(){return B(_sc(_sj[1]));})]:E(_s4);}},_sk=function(_sl){var _sm=B(_sg(_sl));return [0,_sm[1],_sm[2]];},_sn=function(_so){var _sp=B(_s7(_so,0));if(!_sp[0]){return E(_s4);}else{var _sq=E(_sp[2]);return _sq[0]==0?E(_s4):E(_sq[2])[0]==0?[0,new T(function(){return B(_sk(_sp[1]));}),[0,_sq[1]]]:E(_s4);}},_sr=function(_ss){return E(E(_ss)[1]);},_st=function(_su){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return _eq(function(_sv){return function(_sw){return function(_sx){return new F(function(){return _e4(function(_){var _=0,_sy=B(A(new T(function(){return B(A(new T(function(){var _sz=B(_s7(_sv,0));if(!_sz[0]){var _sA=E(_s4);}else{var _sB=E(_sz[2]),_sA=_sB[0]==0?E(_s4):E(_sB[2])[0]==0?[0,new T(function(){return B(_ej(_sz[1]));}),new T(function(){return B(_ej(_sB[1]));})]:E(_s4);}var _sC=_sA,_sD=B(A(_su,[_sC]));return _sD;}),[B(_jj(_sn,B(_s7(_sw,0))))]));}),[B(_jj(_sa,B(_s7(_sx,0)))),_])),_sE=_sy;return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_s5,[B(_jj(_sr,_sE)),_]);});});});});});};};},_);});});});},_sF=new T(function(){return B(_ec(_st,_s1));}),_sG=function(_sH,_sI){return !B(_2I(E(_sH)[3],E(_sI)[3]))?true:false;},_sJ=function(_sK,_sL){return new F(function(){return _2I(E(_sK)[3],E(_sL)[3]);});},_sM=[0,_sJ,_sG],_sN=function(_sO,_sP){while(1){var _sQ=(function(_sR,_sS){var _sT=E(_sS);if(!_sT[0]){return [0];}else{var _sU=_sT[1],_sV=_sT[2];if(!B(A(_sR,[_sU]))){var _sW=_sR;_sP=_sV;_sO=_sW;return null;}else{return [1,_sU,new T(function(){return B(_sN(_sR,_sV));})];}}})(_sO,_sP);if(_sQ!=null){return _sQ;}}},_sX=function(_sY){return E(E(_sY)[3]);},_sZ=function(_t0,_t1){var _t2=function(_t3,_t4){while(1){var _t5=(function(_t6,_t7){var _t8=E(_t6);if(!_t8[0]){return E(_t7);}else{var _t9=_t8[1],_ta=_t8[2];if(!B(_5s(_sM,_t9,_t7))){_t3=_ta;_t4=[1,_t9,new T(function(){return B(_t2(B(_sN(function(_tb){return new F(function(){return _5s(_32,new T(function(){return B(_sX(_tb));}),B(_jj(_ol,E(_t9)[2])));});},_t0)),_t7));})];return null;}else{_t3=_ta;var _tc=_t7;_t4=_tc;return null;}}})(_t3,_t4);if(_t5!=null){return _t5;}}};return new F(function(){return _t2(_t1,_C);});},_td=function(_te){var _tf=E(_te);return [0,_tf[1],_tf[2],_tf[3],_tf[4]];},_tg=function(_th,_ti){while(1){var _tj=(function(_tk,_tl){var _tm=E(_tl);if(!_tm[0]){return E(_tk);}else{_th=new T(function(){var _tn=E(_tm[1]);return [1,[0,_tn[1],_tn[2],_tn[3],_tn[4]],_tk];});_ti=_tm[2];return null;}})(_th,_ti);if(_tj!=null){return _tj;}}},_to=function(_tp,_){return new T(function(){var _tq=B(_tg(_C,_tp));return B(_jj(_td,B(_rU(B(_sZ(_tq,_tq)),_C))));});},_tr=new T(function(){return [0,"topSort"];}),_ts=function(_tt){return E(toJSStr(E(_tt)));},_tu=function(_tv,_tw){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_s5,[[1,new T(function(){return B(_ts(_tv));}),[1,new T(function(){return E(E(_tw)[1]);}),_C]],_]);});});});},_tx=function(_ty){var _tz=E(_ty);return new F(function(){return _tu(_tz[1],_tz[2]);});},_tA=function(_tB,_tC,_tD,_tE){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_s5,[[1,new T(function(){return B(_ts(_tB));}),[1,new T(function(){return B(_e4(function(_){var _=0;return new F(function(){return A(_s5,[B(_jj(_tx,_tC)),_]);});}));}),[1,new T(function(){return B(_ts(_tD));}),[1,new T(function(){return E(E(_tE)[1]);}),_C]]]],_]);});});});},_tF=function(_tG){var _tH=E(_tG);return new F(function(){return _tA(_tH[1],_tH[2],_tH[3],_tH[4]);});},_tI=function(_tJ){return new F(function(){return _1Y("src/Haste/Foreign.hs:113:12-74|case");});},_tK=new T(function(){return B(_tI(_));}),_tL=function(_tM){return new F(function(){return _jj(_sk,B(_s7(_tM,0)));});},_tN=function(_tO){var _tP=B(_s7(_tO,0));if(!_tP[0]){return E(_tK);}else{var _tQ=E(_tP[2]);if(!_tQ[0]){return E(_tK);}else{var _tR=E(_tQ[2]);if(!_tR[0]){return E(_tK);}else{var _tS=E(_tR[2]);return _tS[0]==0?E(_tK):E(_tS[2])[0]==0?[0,new T(function(){return B(_ej(_tP[1]));}),new T(function(){return B(_tL(_tQ[1]));}),new T(function(){return B(_ej(_tR[1]));}),new T(function(){return B(_sc(_tS[1]));})]:E(_tK);}}}},_tT=function(_tU){var _tV=B(_tN(_tU));return [0,_tV[1],_tV[2],_tV[3],_tV[4]];},_tW=function(_tX){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return _eq(function(_tY){return new F(function(){return _e4(function(_){var _=0,_tZ=B(A(_tX,[B(_jj(_tT,B(_s7(_tY,0)))),_])),_u0=_tZ;return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_s5,[B(_jj(_tF,_u0)),_]);});});});});});},_);});});});},_u1=new T(function(){return B(_ec(_tW,_tr));}),_u2=function(_u3){var _u4=new T(function(){return E(E(_u3)[2]);});return [0,new T(function(){return E(E(_u4)[2]);}),new T(function(){return E(E(_u4)[1]);}),new T(function(){return B(_ol(_u3));}),new T(function(){return E(E(_u4)[3]);})];},_u5=function(_u6){return new F(function(){return _lz(0,_u6,_C);});},_u7=new T(function(){return B(err(_jS));}),_u8=new T(function(){return B(err(_jU));}),_u9=new T(function(){return B(_c4(_cq,_bR,_km));}),_ua=function(_ub,_uc){while(1){var _ud=(function(_ue,_uf){var _ug=E(_uf);if(!_ug[0]){var _uh=_ug[2];_ub=[1,new T(function(){var _ui=E(_ug[3]);return _ui[0]==0?[0,_uh,new T(function(){var _uj=B(_kt(B(_21(_u9,new T(function(){return B(_u5(_ui[1]));})))));return _uj[0]==0?E(_u8):E(_uj[2])[0]==0?E(_uj[1]):E(_u7);})]:[0,_uh,_l8];}),new T(function(){return B(_ua(_ue,_ug[5]));})];_uc=_ug[4];return null;}else{return E(_ue);}})(_ub,_uc);if(_ud!=null){return _ud;}}},_uk=function(_ul){var _um=B(_21(_dV,_ul));if(!_um[0]){return E(_lW);}else{if(!E(_um[2])[0]){var _un=E(_um[1]);return function(_uo){var _up=E(_un[1]);return new F(function(){return _jy(new T(function(){return B(_lY(_lb,_un[2]));}),_up[1],_up[2],E(_up[3])[1],_uo);});};}else{return E(_lW);}}},_uq=function(_ur){return E(E(_ur)[3]);},_us=function(_ut,_uu,_uv,_uw,_ux,_uy){switch(B(A(_ut,[_uv,_ux]))){case 0:return true;case 1:return new F(function(){return A(_uq,[_uu,_uw,_uy]);});break;default:return false;}},_uz=function(_uA,_uB,_uC,_uD,_uE){var _uF=E(_uD),_uG=E(_uE);return new F(function(){return _us(E(_uB)[2],_uC,_uF[1],_uF[2],_uG[1],_uG[2]);});},_uH=function(_uI){return E(E(_uI)[6]);},_uJ=function(_uK,_uL,_uM,_uN,_uO,_uP){switch(B(A(_uK,[_uM,_uO]))){case 0:return true;case 1:return new F(function(){return A(_uH,[_uL,_uN,_uP]);});break;default:return false;}},_uQ=function(_uR,_uS,_uT,_uU,_uV){var _uW=E(_uU),_uX=E(_uV);return new F(function(){return _uJ(E(_uS)[2],_uT,_uW[1],_uW[2],_uX[1],_uX[2]);});},_uY=function(_uZ){return E(E(_uZ)[5]);},_v0=function(_v1,_v2,_v3,_v4,_v5,_v6){switch(B(A(_v1,[_v3,_v5]))){case 0:return false;case 1:return new F(function(){return A(_uY,[_v2,_v4,_v6]);});break;default:return true;}},_v7=function(_v8,_v9,_va,_vb,_vc){var _vd=E(_vb),_ve=E(_vc);return new F(function(){return _v0(E(_v9)[2],_va,_vd[1],_vd[2],_ve[1],_ve[2]);});},_vf=function(_vg){return E(E(_vg)[4]);},_vh=function(_vi,_vj,_vk,_vl,_vm,_vn){switch(B(A(_vi,[_vk,_vm]))){case 0:return false;case 1:return new F(function(){return A(_vf,[_vj,_vl,_vn]);});break;default:return true;}},_vo=function(_vp,_vq,_vr,_vs,_vt){var _vu=E(_vs),_vv=E(_vt);return new F(function(){return _vh(E(_vq)[2],_vr,_vu[1],_vu[2],_vv[1],_vv[2]);});},_vw=function(_vx,_vy,_vz,_vA,_vB,_vC){switch(B(A(_vx,[_vz,_vB]))){case 0:return 0;case 1:return new F(function(){return A(_o9,[_vy,_vA,_vC]);});break;default:return 2;}},_vD=function(_vE,_vF,_vG,_vH,_vI){var _vJ=E(_vH),_vK=E(_vI);return new F(function(){return _vw(E(_vF)[2],_vG,_vJ[1],_vJ[2],_vK[1],_vK[2]);});},_vL=function(_vM,_vN,_vO,_vP,_vQ){var _vR=E(_vP),_vS=_vR[1],_vT=_vR[2],_vU=E(_vQ),_vV=_vU[1],_vW=_vU[2];switch(B(A(E(_vN)[2],[_vS,_vV]))){case 0:return [0,_vV,_vW];case 1:return !B(A(_uH,[_vO,_vT,_vW]))?[0,_vS,_vT]:[0,_vV,_vW];default:return [0,_vS,_vT];}},_vX=function(_vY,_vZ,_w0,_w1,_w2){var _w3=E(_w1),_w4=_w3[1],_w5=_w3[2],_w6=E(_w2),_w7=_w6[1],_w8=_w6[2];switch(B(A(E(_vZ)[2],[_w4,_w7]))){case 0:return [0,_w4,_w5];case 1:return !B(A(_uH,[_w0,_w5,_w8]))?[0,_w7,_w8]:[0,_w4,_w5];default:return [0,_w7,_w8];}},_w9=function(_wa,_wb,_wc){return [0,_wa,function(_wd,_we){return new F(function(){return _vD(_wa,_wb,_wc,_wd,_we);});},function(_wd,_we){return new F(function(){return _uz(_wa,_wb,_wc,_wd,_we);});},function(_wd,_we){return new F(function(){return _vo(_wa,_wb,_wc,_wd,_we);});},function(_wd,_we){return new F(function(){return _v7(_wa,_wb,_wc,_wd,_we);});},function(_wd,_we){return new F(function(){return _uQ(_wa,_wb,_wc,_wd,_we);});},function(_wd,_we){return new F(function(){return _vL(_wa,_wb,_wc,_wd,_we);});},function(_wd,_we){return new F(function(){return _vX(_wa,_wb,_wc,_wd,_we);});}];},_wf=function(_wg,_wh){return E(_wg)[1]==E(_wh)[1];},_wi=function(_wj,_wk){return E(_wj)[1]!=E(_wk)[1];},_wl=[0,_wf,_wi],_wm=function(_wn,_wo){var _wp=E(_wn),_wq=E(_wo);return _wp[1]>_wq[1]?E(_wp):E(_wq);},_wr=function(_ws,_wt){var _wu=E(_ws),_wv=E(_wt);return _wu[1]>_wv[1]?E(_wv):E(_wu);},_ww=function(_wx,_wy){return _wx>=_wy?_wx!=_wy?2:1:0;},_wz=function(_wA,_wB){return new F(function(){return _ww(E(_wA)[1],E(_wB)[1]);});},_wC=function(_wD,_wE){return E(_wD)[1]>=E(_wE)[1];},_wF=function(_wG,_wH){return E(_wG)[1]>E(_wH)[1];},_wI=function(_wJ,_wK){return E(_wJ)[1]<=E(_wK)[1];},_wL=function(_wM,_wN){return E(_wM)[1]<E(_wN)[1];},_wO=[0,_wl,_wz,_wL,_wC,_wF,_wI,_wm,_wr],_wP=function(_wQ,_wR,_wS,_wT,_wU,_wV){return !B(A(_wQ,[_wS,_wU]))?true:!B(A(_5q,[_wR,_wT,_wV]))?true:false;},_wW=function(_wX,_wY,_wZ,_x0){var _x1=E(_wZ),_x2=E(_x0);return new F(function(){return _wP(E(_wX)[1],_wY,_x1[1],_x1[2],_x2[1],_x2[2]);});},_x3=function(_x4,_x5,_x6,_x7,_x8,_x9){return !B(A(_x4,[_x6,_x8]))?false:B(A(_5q,[_x5,_x7,_x9]));},_xa=function(_xb,_xc,_xd,_xe){var _xf=E(_xd),_xg=E(_xe);return new F(function(){return _x3(E(_xb)[1],_xc,_xf[1],_xf[2],_xg[1],_xg[2]);});},_xh=function(_xi,_xj){return [0,function(_wd,_we){return new F(function(){return _xa(_xi,_xj,_wd,_we);});},function(_wd,_we){return new F(function(){return _wW(_xi,_xj,_wd,_we);});}];},_xk=new T(function(){return B(_xh(_32,_wl));}),_xl=new T(function(){return B(_w9(_xk,_mN,_wO));}),_xm=function(_xn,_xo,_xp,_xq,_xr){var _xs=E(_xn);if(_xs==1){var _xt=E(_xr);if(!_xt[0]){return [0,[0,1,E([0,_xo,[0,_xp]]),_xq,E(_ey),E(_ey)],_C,_C];}else{var _xu=E(E(_xt[1])[1]);switch(B(_mo(_xo,_xu[1]))){case 0:return [0,[0,1,E([0,_xo,[0,_xp]]),_xq,E(_ey),E(_ey)],_xt,_C];case 1:return _xp<E(_xu[2])[1]?[0,[0,1,E([0,_xo,[0,_xp]]),_xq,E(_ey),E(_ey)],_xt,_C]:[0,[0,1,E([0,_xo,[0,_xp]]),_xq,E(_ey),E(_ey)],_C,_xt];default:return [0,[0,1,E([0,_xo,[0,_xp]]),_xq,E(_ey),E(_ey)],_C,_xt];}}}else{var _xv=B(_xm(_xs>>1,_xo,_xp,_xq,_xr)),_xw=_xv[1],_xx=_xv[3],_xy=E(_xv[2]);if(!_xy[0]){return [0,_xw,_C,_xx];}else{var _xz=E(_xy[1]),_xA=_xz[1],_xB=_xz[2],_xC=E(_xy[2]);if(!_xC[0]){return [0,new T(function(){return B(_fl(_xA,_xB,_xw));}),_C,_xx];}else{var _xD=_xC[2],_xE=E(_xC[1]),_xF=_xE[2],_xG=E(_xA),_xH=E(_xE[1]),_xI=_xH[1],_xJ=_xH[2];switch(B(_mo(_xG[1],_xI))){case 0:var _xK=B(_xL(_xs>>1,_xI,_xJ,_xF,_xD));return [0,new T(function(){return B(_gO(_xG,_xB,_xw,_xK[1]));}),_xK[2],_xK[3]];case 1:var _xM=E(_xJ)[1];if(E(_xG[2])[1]<_xM){var _xN=B(_xm(_xs>>1,_xI,_xM,_xF,_xD));return [0,new T(function(){return B(_gO(_xG,_xB,_xw,_xN[1]));}),_xN[2],_xN[3]];}else{return [0,_xw,_C,_xy];}break;default:return [0,_xw,_C,_xy];}}}}},_xL=function(_xO,_xP,_xQ,_xR,_xS){var _xT=E(_xO);if(_xT==1){var _xU=E(_xS);if(!_xU[0]){return [0,[0,1,E([0,_xP,_xQ]),_xR,E(_ey),E(_ey)],_C,_C];}else{var _xV=E(E(_xU[1])[1]);switch(B(_mo(_xP,_xV[1]))){case 0:return [0,[0,1,E([0,_xP,_xQ]),_xR,E(_ey),E(_ey)],_xU,_C];case 1:var _xW=E(_xQ);return _xW[1]<E(_xV[2])[1]?[0,[0,1,E([0,_xP,_xW]),_xR,E(_ey),E(_ey)],_xU,_C]:[0,[0,1,E([0,_xP,_xW]),_xR,E(_ey),E(_ey)],_C,_xU];default:return [0,[0,1,E([0,_xP,_xQ]),_xR,E(_ey),E(_ey)],_C,_xU];}}}else{var _xX=B(_xL(_xT>>1,_xP,_xQ,_xR,_xS)),_xY=_xX[1],_xZ=_xX[3],_y0=E(_xX[2]);if(!_y0[0]){return [0,_xY,_C,_xZ];}else{var _y1=E(_y0[1]),_y2=_y1[1],_y3=_y1[2],_y4=E(_y0[2]);if(!_y4[0]){return [0,new T(function(){return B(_fl(_y2,_y3,_xY));}),_C,_xZ];}else{var _y5=_y4[2],_y6=E(_y4[1]),_y7=_y6[2],_y8=E(_y2),_y9=E(_y6[1]),_ya=_y9[1],_yb=_y9[2];switch(B(_mo(_y8[1],_ya))){case 0:var _yc=B(_xL(_xT>>1,_ya,_yb,_y7,_y5));return [0,new T(function(){return B(_gO(_y8,_y3,_xY,_yc[1]));}),_yc[2],_yc[3]];case 1:var _yd=E(_yb)[1];if(E(_y8[2])[1]<_yd){var _ye=B(_xm(_xT>>1,_ya,_yd,_y7,_y5));return [0,new T(function(){return B(_gO(_y8,_y3,_xY,_ye[1]));}),_ye[2],_ye[3]];}else{return [0,_xY,_C,_y0];}break;default:return [0,_xY,_C,_y0];}}}}},_yf=function(_yg,_yh,_yi){return new F(function(){return (function(_yj,_yk){while(1){var _yl=E(_yk);if(!_yl[0]){return E(_yj);}else{var _ym=E(_yl[1]),_yn=B(_oy(_yg,_ym[1],_ym[2],_yj));_yk=_yl[2];_yj=_yn;continue;}}})(_yh,_yi);});},_yo=function(_yp,_yq,_yr){var _ys=E(_yr);if(!_ys[0]){return E(_yq);}else{var _yt=E(_ys[1]),_yu=_yt[1],_yv=_yt[2],_yw=E(_ys[2]);if(!_yw[0]){return new F(function(){return _fl(_yu,_yv,_yq);});}else{var _yx=E(_yw[1]),_yy=E(_yu),_yz=E(_yx[1]),_yA=_yz[1],_yB=_yz[2],_yC=function(_yD){var _yE=B(_xL(_yp,_yA,_yB,_yx[2],_yw[2])),_yF=_yE[1],_yG=E(_yE[3]);if(!_yG[0]){return new F(function(){return _yo(_yp<<1,B(_gO(_yy,_yv,_yq,_yF)),_yE[2]);});}else{return new F(function(){return _yf(_xl,B(_gO(_yy,_yv,_yq,_yF)),_yG);});}};switch(B(_mo(_yy[1],_yA))){case 0:return new F(function(){return _yC(_);});break;case 1:return E(_yy[2])[1]<E(_yB)[1]?B(_yC(_)):B(_yf(_xl,_yq,_ys));default:return new F(function(){return _yf(_xl,_yq,_ys);});}}}},_yH=function(_yI,_yJ,_yK,_yL,_yM,_yN){var _yO=E(_yN);if(!_yO[0]){return new F(function(){return _fl([0,_yK,_yL],_yM,_yJ);});}else{var _yP=E(_yO[1]),_yQ=E(_yP[1]),_yR=_yQ[1],_yS=_yQ[2],_yT=function(_yU){var _yV=B(_xL(_yI,_yR,_yS,_yP[2],_yO[2])),_yW=_yV[1],_yX=E(_yV[3]);if(!_yX[0]){return new F(function(){return _yo(_yI<<1,B(_gO([0,_yK,_yL],_yM,_yJ,_yW)),_yV[2]);});}else{return new F(function(){return _yf(_xl,B(_gO([0,_yK,_yL],_yM,_yJ,_yW)),_yX);});}};switch(B(_mo(_yK,_yR))){case 0:return new F(function(){return _yT(_);});break;case 1:var _yY=E(_yL);return _yY[1]<E(_yS)[1]?B(_yT(_)):B(_yf(_xl,_yJ,[1,[0,[0,_yK,_yY],_yM],_yO]));default:return new F(function(){return _yf(_xl,_yJ,[1,[0,[0,_yK,_yL],_yM],_yO]);});}}},_yZ=function(_z0,_z1,_z2,_z3,_z4,_z5){var _z6=E(_z5);if(!_z6[0]){return new F(function(){return _fl([0,_z2,[0,_z3]],_z4,_z1);});}else{var _z7=E(_z6[1]),_z8=E(_z7[1]),_z9=_z8[1],_za=_z8[2],_zb=function(_zc){var _zd=B(_xL(_z0,_z9,_za,_z7[2],_z6[2])),_ze=_zd[1],_zf=E(_zd[3]);if(!_zf[0]){return new F(function(){return _yo(_z0<<1,B(_gO([0,_z2,[0,_z3]],_z4,_z1,_ze)),_zd[2]);});}else{return new F(function(){return _yf(_xl,B(_gO([0,_z2,[0,_z3]],_z4,_z1,_ze)),_zf);});}};switch(B(_mo(_z2,_z9))){case 0:return new F(function(){return _zb(_);});break;case 1:return _z3<E(_za)[1]?B(_zb(_)):B(_yf(_xl,_z1,[1,[0,[0,_z2,[0,_z3]],_z4],_z6]));default:return new F(function(){return _yf(_xl,_z1,[1,[0,[0,_z2,[0,_z3]],_z4],_z6]);});}}},_zg=function(_zh){var _zi=E(_zh);if(!_zi[0]){return [1];}else{var _zj=E(_zi[1]),_zk=_zj[1],_zl=_zj[2],_zm=E(_zi[2]);if(!_zm[0]){return [0,1,E(E(_zk)),_zl,E(_ey),E(_ey)];}else{var _zn=_zm[2],_zo=E(_zm[1]),_zp=_zo[2],_zq=E(_zk),_zr=E(_zo[1]),_zs=_zr[1],_zt=_zr[2];switch(B(_mo(_zq[1],_zs))){case 0:return new F(function(){return _yH(1,[0,1,E(_zq),_zl,E(_ey),E(_ey)],_zs,_zt,_zp,_zn);});break;case 1:var _zu=E(_zt)[1];return E(_zq[2])[1]<_zu?B(_yZ(1,[0,1,E(_zq),_zl,E(_ey),E(_ey)],_zs,_zu,_zp,_zn)):B(_yf(_xl,[0,1,E(_zq),_zl,E(_ey),E(_ey)],_zm));default:return new F(function(){return _yf(_xl,[0,1,E(_zq),_zl,E(_ey),E(_ey)],_zm);});}}}},_zv=[0],_zw=function(_zx,_zy,_zz){var _zA=E(_zy);if(!_zA[0]){return E(_zz);}else{var _zB=function(_zC,_zD){while(1){var _zE=E(_zD);if(!_zE[0]){var _zF=_zE[2],_zG=_zE[5];switch(B(A(new T(function(){return B(_o9(_zx));}),[_zC,_zF]))){case 0:return new F(function(){return _gO(_zF,_zE[3],B(_zB(_zC,_zE[4])),_zG);});break;case 1:return E(_zG);default:_zD=_zG;continue;}}else{return [1];}}};return new F(function(){return _zB(_zA[1],_zz);});}},_zH=function(_zI,_zJ,_zK){var _zL=E(_zJ);if(!_zL[0]){return E(_zK);}else{var _zM=function(_zN,_zO){while(1){var _zP=E(_zO);if(!_zP[0]){var _zQ=_zP[2],_zR=_zP[4];switch(B(A(new T(function(){return B(_o9(_zI));}),[_zQ,_zN]))){case 0:return new F(function(){return _gO(_zQ,_zP[3],_zR,B(_zM(_zN,_zP[5])));});break;case 1:return E(_zR);default:_zO=_zR;continue;}}else{return [1];}}};return new F(function(){return _zM(_zL[1],_zK);});}},_zS=function(_zT,_zU,_zV,_zW){var _zX=E(_zU),_zY=E(_zW);if(!_zY[0]){var _zZ=_zY[2],_A0=_zY[3],_A1=_zY[4],_A2=_zY[5];switch(B(A(_o9,[_zT,_zX,_zZ]))){case 0:return new F(function(){return _fu(_zZ,_A0,B(_zS(_zT,_zX,_zV,_A1)),_A2);});break;case 1:return E(_zY);default:return new F(function(){return _eD(_zZ,_A0,_A1,B(_zS(_zT,_zX,_zV,_A2)));});}}else{return [0,1,E(_zX),_zV,E(_ey),E(_ey)];}},_A3=function(_A4,_A5,_A6,_A7){return new F(function(){return _zS(_A4,_A5,_A6,_A7);});},_A8=function(_A9,_Aa,_Ab,_Ac){var _Ad=new T(function(){return B(_vf(_A9));}),_Ae=new T(function(){return B(_uH(_A9));}),_Af=E(_Aa);if(!_Af[0]){var _Ag=E(_Ab);if(!_Ag[0]){return E(_Ac);}else{return new F(function(){return (function(_Ah,_Ai){while(1){var _Aj=E(_Ai);if(!_Aj[0]){if(!B(A(_Ad,[_Aj[2],_Ah]))){return E(_Aj);}else{_Ai=_Aj[4];continue;}}else{return [1];}}})(_Ag[1],_Ac);});}}else{var _Ak=_Af[1],_Al=E(_Ab);if(!_Al[0]){return new F(function(){return (function(_Am,_An){while(1){var _Ao=E(_An);if(!_Ao[0]){if(!B(A(_Ae,[_Ao[2],_Am]))){return E(_Ao);}else{_An=_Ao[5];continue;}}else{return [1];}}})(_Ak,_Ac);});}else{return new F(function(){return (function(_Ap,_Aq,_Ar){while(1){var _As=E(_Ar);if(!_As[0]){var _At=_As[2];if(!B(A(_Ae,[_At,_Ap]))){if(!B(A(_Ad,[_At,_Aq]))){return E(_As);}else{_Ar=_As[4];continue;}}else{_Ar=_As[5];continue;}}else{return [1];}}})(_Ak,_Al[1],_Ac);});}}},_Au=function(_Av,_Aw,_Ax,_Ay,_Az){var _AA=E(_Az);if(!_AA[0]){var _AB=_AA[2],_AC=_AA[3],_AD=_AA[4],_AE=_AA[5],_AF=E(_Ay);if(!_AF[0]){var _AG=_AF[2],_AH=function(_AI){var _AJ=[1,E(_AG)];return new F(function(){return _gO(_AG,_AF[3],B(_Au(_Av,_Aw,_AJ,_AF[4],B(_A8(_Av,_Aw,_AJ,_AA)))),B(_Au(_Av,_AJ,_Ax,_AF[5],B(_A8(_Av,_AJ,_Ax,_AA)))));});};return E(_AD)[0]==0?B(_AH(_)):E(_AE)[0]==0?B(_AH(_)):B(_A3(_Av,_AB,_AC,_AF));}else{return new F(function(){return _gO(_AB,_AC,B(_zw(_Av,_Aw,_AD)),B(_zH(_Av,_Ax,_AE)));});}}else{return E(_Ay);}},_AK=function(_AL,_AM,_AN,_AO,_AP,_AQ,_AR,_AS,_AT,_AU,_AV,_AW,_AX){var _AY=function(_AZ){var _B0=[1,E(_AP)];return new F(function(){return _gO(_AP,_AQ,B(_Au(_AL,_AM,_B0,_AR,B(_A8(_AL,_AM,_B0,[0,_AT,E(_AU),_AV,E(_AW),E(_AX)])))),B(_Au(_AL,_B0,_AN,_AS,B(_A8(_AL,_B0,_AN,[0,_AT,E(_AU),_AV,E(_AW),E(_AX)])))));});};return E(_AW)[0]==0?B(_AY(_)):E(_AX)[0]==0?B(_AY(_)):B(_A3(_AL,_AU,_AV,[0,_AO,E(_AP),_AQ,E(_AR),E(_AS)]));},_B1=function(_B2,_B3){var _B4=E(_B2);if(!_B4[0]){var _B5=E(_B3);return _B5[0]==0?B(_AK(_xl,_zv,_zv,_B4[1],_B4[2],_B4[3],_B4[4],_B4[5],_B5[1],_B5[2],_B5[3],_B5[4],_B5[5])):E(_B4);}else{return E(_B3);}},_B6=function(_B7,_B8,_B9){while(1){var _Ba=E(_B9);if(!_Ba[0]){var _Bb=_Ba[4],_Bc=_Ba[5],_Bd=E(_Ba[2]);switch(B(_mo(_B7,_Bd[1]))){case 0:_B9=_Bb;continue;case 1:var _Be=E(_Bd[2])[1];if(_B8>=_Be){if(_B8!=_Be){_B9=_Bc;continue;}else{return [1,_Ba[3]];}}else{_B9=_Bb;continue;}break;default:_B9=_Bc;continue;}}else{return [0];}}},_Bf=function(_Bg,_Bh,_Bi){while(1){var _Bj=E(_Bi);if(!_Bj[0]){var _Bk=_Bj[4],_Bl=_Bj[5],_Bm=E(_Bj[2]);switch(B(_mo(_Bg,_Bm[1]))){case 0:_Bi=_Bk;continue;case 1:var _Bn=E(_Bh)[1],_Bo=E(_Bm[2])[1];if(_Bn>=_Bo){return _Bn!=_Bo?B(_B6(_Bg,_Bn,_Bl)):[1,_Bj[3]];}else{return new F(function(){return _B6(_Bg,_Bn,_Bk);});}break;default:_Bi=_Bl;continue;}}else{return [0];}}},_Bp=new T(function(){return B(_lc(0,2147483647));}),_Bq=function(_Br,_Bs,_Bt){var _Bu=function(_Bv,_Bw){while(1){var _Bx=(function(_By,_Bz){var _BA=E(_By);if(!_BA[0]){return [0];}else{var _BB=_BA[2],_BC=E(_Bz);if(!_BC[0]){return [0];}else{var _BD=_BC[2],_BE=E(_BC[1]);if(_BE[0]==1){_Bv=_BB;_Bw=_BD;return null;}else{return [1,[0,[0,new T(function(){return E(E(_Bs)[3]);}),_BA[1]],_BE],new T(function(){return B(_Bu(_BB,_BD));})];}}}})(_Bv,_Bw);if(_Bx!=null){return _Bx;}}};return new F(function(){return _B1(_Bt,B(_zg(B(_Bu(_Bp,new T(function(){return B(A(_Br,[new T(function(){return E(E(_Bs)[1]);}),new T(function(){return B(_jj(function(_BF){var _BG=E(_BF),_BH=B(_Bf(_BG[1],_BG[2],_Bt));return _BH[0]==0?[1]:E(_BH[1]);},E(_Bs)[2]));})]));},1))))));});},_BI=function(_BJ,_BK){while(1){var _BL=E(_BK);if(!_BL[0]){return E(_BJ);}else{var _BM=B(_Bq(_uk,_BL[1],_BJ));_BK=_BL[2];_BJ=_BM;continue;}}},_BN=new T(function(){return B(unCStr("tail"));}),_BO=new T(function(){return B(_F(_BN));}),_BP=function(_BQ,_){return new T(function(){var _BR=B(_rU(B(_oL(_mN,new T(function(){return B(_nY(_BQ));}),B(_jj(_ol,_BQ))))[2],_C));if(!_BR[0]){var _BS=E(_BO);}else{var _BS=B(_ua(_C,B(_BI(_ey,B(_jj(_u2,_BR[2]))))));}var _BT=_BS,_BU=_BT;return _BU;});},_BV=new T(function(){return [0,"runCoinKernelOn"];}),_BW=function(_BX){var _BY=E(_BX);return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_s5,[[1,new T(function(){return B(_tx(_BY[1]));}),[1,new T(function(){return E(E(_BY[2])[1]);}),_C]],_]);});});});},_BZ=function(_C0){return new F(function(){return _1Y("src/Haste/Foreign.hs:109:12-63|case");});},_C1=new T(function(){return B(_BZ(_));}),_C2=function(_C3){var _C4=B(_s7(_C3,0));if(!_C4[0]){return E(_s4);}else{var _C5=E(_C4[2]);return _C5[0]==0?E(_s4):E(_C5[2])[0]==0?[0,new T(function(){return B(_ej(_C4[1]));}),new T(function(){var _C6=B(_s7(_C5[1],0));if(!_C6[0]){var _C7=E(_C1);}else{var _C8=E(_C6[2]);if(!_C8[0]){var _C9=E(_C1);}else{var _Ca=E(_C8[2]),_C9=_Ca[0]==0?E(_C1):E(_Ca[2])[0]==0?[0,new T(function(){return B(_tL(_C6[1]));}),new T(function(){return B(_ej(_C8[1]));}),new T(function(){return B(_sc(_Ca[1]));})]:E(_C1);}var _C7=_C9;}return _C7;})]:E(_s4);}},_Cb=function(_Cc){var _Cd=B(_C2(_Cc));return [0,_Cd[1],_Cd[2]];},_Ce=function(_Cf){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return _eq(function(_Cg){return new F(function(){return _e4(function(_){var _=0,_Ch=B(A(_Cf,[B(_jj(_Cb,B(_s7(_Cg,0)))),_])),_Ci=_Ch;return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_s5,[B(_jj(_BW,_Ci)),_]);});});});});});},_);});});});},_Cj=new T(function(){return B(_ec(_Ce,_BV));}),_Ck=new T(function(){return B(unCStr("\"}"));}),_Cl=[1,_lH,_Ck],_Cm=[1,_lG,_Ck],_Cn=[1,_lF,_Ck],_Co=function(_Cp,_Cq,_Cr){return E(toJSStr([1,_lI,new T(function(){return B(unAppCStr("\"txid\" : \"",new T(function(){return B(_f(_Cp,new T(function(){return B(unAppCStr("\", \"index\" : ",new T(function(){return B(_f(B(_q(0,E(_Cq)[1],_C)),new T(function(){return B(unAppCStr(", \"coinstate\" : \"",new T(function(){var _Cs=E(_Cr);switch(_Cs[0]){case 0:var _Ct=B(_f(B(_lz(0,_Cs[1],_C)),_Ck));break;case 1:var _Ct=E(_Cl);break;case 2:var _Ct=E(_Cm);break;default:var _Ct=E(_Cn);}return _Ct;})));},1)));})));},1)));})));})]));},_Cu=function(_Cv,_Cw){while(1){var _Cx=(function(_Cy,_Cz){var _CA=E(_Cz);if(!_CA[0]){var _CB=E(_CA[2]);_Cv=[1,new T(function(){return [0,B(_Co(_CB[1],_CB[2],_CA[3]))];}),new T(function(){return B(_Cu(_Cy,_CA[5]));})];_Cw=_CA[4];return null;}else{return E(_Cy);}})(_Cv,_Cw);if(_Cx!=null){return _Cx;}}},_CC=function(_CD,_CE){while(1){var _CF=(function(_CG,_CH){var _CI=E(_CH);if(!_CI[0]){return E(_CG);}else{_CD=new T(function(){var _CJ=E(_CI[1]);return [1,[0,_CJ[1],_CJ[2],_CJ[3],_CJ[4]],_CG];});_CE=_CI[2];return null;}})(_CD,_CE);if(_CF!=null){return _CF;}}},_CK=function(_CL,_CM){while(1){var _CN=E(_CM);if(!_CN[0]){return E(_CL);}else{var _CO=B(_Bq(_uk,_CN[1],_CL));_CM=_CN[2];_CL=_CO;continue;}}},_CP=function(_CQ,_CR,_){return new T(function(){return B(_Cu(_C,B(_CK(_ey,B(_rU(B(_sZ(new T(function(){return B(_CC(_C,_CR));}),[1,new T(function(){var _CS=E(_CQ);return [0,_CS[1],_CS[2],_CS[3],_CS[4]];}),_C])),_C))))));});},_CT=new T(function(){return [0,"_runCoinKernelOnGraph"];}),_CU=function(_CV,_CW){return new F(function(){return _e4(function(_){var _=0,_CX=B(A(_CV,[B(_jj(_tT,B(_s7(_CW,0)))),_])),_CY=_CX;return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_s5,[B(_jj(_sr,_CY)),_]);});});});});});},_CZ=function(_D0){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return _eq(function(_D1){return function(_3D){return new F(function(){return _CU(new T(function(){var _D2=B(_s7(_D1,0));if(!_D2[0]){var _D3=E(_tK);}else{var _D4=E(_D2[2]);if(!_D4[0]){var _D5=E(_tK);}else{var _D6=E(_D4[2]);if(!_D6[0]){var _D7=E(_tK);}else{var _D8=E(_D6[2]),_D7=_D8[0]==0?E(_tK):E(_D8[2])[0]==0?[0,new T(function(){return B(_ej(_D2[1]));}),new T(function(){return B(_tL(_D4[1]));}),new T(function(){return B(_ej(_D6[1]));}),new T(function(){return B(_sc(_D8[1]));})]:E(_tK);}var _D5=_D7;}var _D3=_D5;}var _D9=_D3,_Da=B(A(_D0,[_D9]));return _Da;}),_3D);});};},_);});});});},_Db=new T(function(){return B(_ec(_CZ,_CT));}),_Dc=new T(function(){return [0,"toposort"];}),_Dd=function(_De){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return _eq(function(_Df){return new F(function(){return _e4(function(_){var _=0,_Dg=B(A(_De,[B(_jj(_Cb,B(_s7(_Df,0)))),_])),_Dh=_Dg;return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_s5,[B(_jj(_ts,_Dh)),_]);});});});});});},_);});});});},_Di=new T(function(){return B(_ec(_Dd,_Dc));}),_Dj=function(_Dk,_Dl){while(1){var _Dm=(function(_Dn,_Do){var _Dp=E(_Do);if(!_Dp[0]){var _Dq=E(_Dp[2]);_Dk=[1,new T(function(){return [0,B(_Co(_Dq[1],_Dq[2],_Dp[3]))];}),new T(function(){return B(_Dj(_Dn,_Dp[5]));})];_Dl=_Dp[4];return null;}else{return E(_Dn);}})(_Dk,_Dl);if(_Dm!=null){return _Dm;}}},_Dr=function(_Ds,_Dt){while(1){var _Du=E(_Dt);if(!_Du[0]){return E(_Ds);}else{var _Dv=B(_Bq(_uk,_Du[1],_Ds));_Dt=_Du[2];_Ds=_Dv;continue;}}},_Dw=function(_Dx,_Dy){while(1){var _Dz=(function(_DA,_DB){var _DC=E(_DB);if(!_DC[0]){return E(_DA);}else{_Dx=new T(function(){var _DD=E(_DC[1]);return [1,[0,_DD[1],_DD[2],_DD[3],_DD[4]],_DA];});_Dy=_DC[2];return null;}})(_Dx,_Dy);if(_Dz!=null){return _Dz;}}},_DE=function(_DF,_){return new T(function(){var _DG=B(_Dw(_C,_DF));return B(_Dj(_C,B(_Dr(_ey,B(_rU(B(_sZ(_DG,_DG)),_C))))));});},_DH=new T(function(){return [0,"runCoinKernelOnGraph"];}),_DI=function(_DJ){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return _eq(new T(function(){return function(_3D){return new F(function(){return _CU(_DJ,_3D);});};}),_);});});});},_DK=new T(function(){return B(_ec(_DI,_DH));}),_DL=function(_){var _DM=B(A(_DK,[_DE,_])),_DN=_DM,_DO=B(A(_Db,[_CP,_])),_DP=_DO,_DQ=B(A(_Cj,[_BP,_])),_DR=_DQ,_DS=B(A(_u1,[_to,_])),_DT=_DS,_DU=B(A(_sF,[_mb,_])),_DV=_DU,_DW=B(A(_ex,[_dW,_])),_DX=_DW;return new F(function(){return A(_Di,[_rZ,_]);});},_DY=function(_){return new F(function(){return _DL(_);});};
var hasteMain = function() {B(A(_DY, [0]));};hasteMain();