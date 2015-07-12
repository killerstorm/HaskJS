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

var _0=[0,44],_1=function(_2,_3,_4){return new F(function(){return A(_2,[[1,_0,new T(function(){return B(A(_3,[_4]));})]]);});},_5=[0,93],_6=[0,91],_7=function(_8,_9,_a){var _b=E(_9);return _b[0]==0?B(unAppCStr("[]",_a)):[1,_6,new T(function(){return B(A(_8,[_b[1],new T(function(){var _c=function(_d){var _e=E(_d);return _e[0]==0?E([1,_5,_a]):[1,_0,new T(function(){return B(A(_8,[_e[1],new T(function(){return B(_c(_e[2]));})]));})];};return B(_c(_b[2]));})]));})];},_f=function(_g,_h){var _i=E(_g);return _i[0]==0?E(_h):[1,_i[1],new T(function(){return B(_f(_i[2],_h));})];},_j=function(_k,_l){var _m=jsShowI(_k),_n=_m;return new F(function(){return _f(fromJSStr(_n),_l);});},_o=[0,41],_p=[0,40],_q=function(_r,_s,_t){if(_s>=0){return new F(function(){return _j(_s,_t);});}else{return _r<=6?B(_j(_s,_t)):[1,_p,new T(function(){var _u=jsShowI(_s),_v=_u;return B(_f(fromJSStr(_v),[1,_o,_t]));})];}},_w=function(_x,_y){return new F(function(){return _q(0,E(_x)[1],_y);});},_z=function(_A,_B){return new F(function(){return _7(_w,_A,_B);});},_C=[0],_D=new T(function(){return B(unCStr(": empty list"));}),_E=new T(function(){return B(unCStr("Prelude."));}),_F=function(_G){return new F(function(){return err(B(_f(_E,new T(function(){return B(_f(_G,_D));},1))));});},_H=new T(function(){return B(unCStr("foldr1"));}),_I=new T(function(){return B(_F(_H));}),_J=function(_K,_L){var _M=E(_L);if(!_M[0]){return E(_I);}else{var _N=_M[1],_O=E(_M[2]);if(!_O[0]){return E(_N);}else{return new F(function(){return A(_K,[_N,new T(function(){return B(_J(_K,_O));})]);});}}},_P=[1,_o,_C],_Q=new T(function(){return B(unCStr("Nothing"));}),_R=[2],_S=function(_T){return [3,_T,_R];},_U=new T(function(){return B(unCStr("Control.Exception.Base"));}),_V=new T(function(){return B(unCStr("base"));}),_W=new T(function(){return B(unCStr("PatternMatchFail"));}),_X=new T(function(){var _Y=hs_wordToWord64(18445595),_Z=_Y,_10=hs_wordToWord64(52003073),_11=_10;return [0,_Z,_11,[0,_Z,_11,_V,_U,_W],_C];}),_12=function(_13){return E(_X);},_14=function(_15){return E(E(_15)[1]);},_16=function(_17,_18,_19){var _1a=B(A(_17,[_])),_1b=B(A(_18,[_])),_1c=hs_eqWord64(_1a[1],_1b[1]),_1d=_1c;if(!E(_1d)){return [0];}else{var _1e=hs_eqWord64(_1a[2],_1b[2]),_1f=_1e;return E(_1f)==0?[0]:[1,_19];}},_1g=function(_1h){var _1i=E(_1h);return new F(function(){return _16(B(_14(_1i[1])),_12,_1i[2]);});},_1j=function(_1k){return E(E(_1k)[1]);},_1l=function(_1m,_1n){return new F(function(){return _f(E(_1m)[1],_1n);});},_1o=function(_1p,_1q){return new F(function(){return _7(_1l,_1p,_1q);});},_1r=function(_1s,_1t,_1u){return new F(function(){return _f(E(_1t)[1],_1u);});},_1v=[0,_1r,_1j,_1o],_1w=new T(function(){return [0,_12,_1v,_1x,_1g];}),_1x=function(_1y){return [0,_1w,_1y];},_1z=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_1A=function(_1B,_1C){return new F(function(){return die(new T(function(){return B(A(_1C,[_1B]));}));});},_1D=function(_1E,_1F){var _1G=E(_1F);if(!_1G[0]){return [0,_C,_C];}else{var _1H=_1G[1];if(!B(A(_1E,[_1H]))){return [0,_C,_1G];}else{var _1I=new T(function(){var _1J=B(_1D(_1E,_1G[2]));return [0,_1J[1],_1J[2]];});return [0,[1,_1H,new T(function(){return E(E(_1I)[1]);})],new T(function(){return E(E(_1I)[2]);})];}}},_1K=[0,32],_1L=[0,10],_1M=[1,_1L,_C],_1N=function(_1O){return E(E(_1O)[1])==124?false:true;},_1P=function(_1Q,_1R){var _1S=B(_1D(_1N,B(unCStr(_1Q)))),_1T=_1S[1],_1U=function(_1V,_1W){return new F(function(){return _f(_1V,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_f(_1R,new T(function(){return B(_f(_1W,_1M));},1)));})));},1));});},_1X=E(_1S[2]);if(!_1X[0]){return new F(function(){return _1U(_1T,_C);});}else{return E(E(_1X[1])[1])==124?B(_1U(_1T,[1,_1K,_1X[2]])):B(_1U(_1T,_C));}},_1Y=function(_1Z){return new F(function(){return _1A([0,new T(function(){return B(_1P(_1Z,_1z));})],_1x);});},_20=new T(function(){return B(_1Y("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_21=function(_22,_23){while(1){var _24=(function(_25,_26){var _27=E(_25);switch(_27[0]){case 0:var _28=E(_26);if(!_28[0]){return [0];}else{_22=B(A(_27[1],[_28[1]]));_23=_28[2];return null;}break;case 1:var _29=B(A(_27[1],[_26])),_2a=_26;_22=_29;_23=_2a;return null;case 2:return [0];case 3:return [1,[0,_27[1],_26],new T(function(){return B(_21(_27[2],_26));})];default:return E(_27[1]);}})(_22,_23);if(_24!=null){return _24;}}},_2b=function(_2c,_2d){var _2e=function(_2f){var _2g=E(_2d);if(_2g[0]==3){return [3,_2g[1],new T(function(){return B(_2b(_2c,_2g[2]));})];}else{var _2h=E(_2c);if(_2h[0]==2){return E(_2g);}else{var _2i=E(_2g);if(_2i[0]==2){return E(_2h);}else{var _2j=function(_2k){var _2l=E(_2i);if(_2l[0]==4){return [1,function(_2m){return [4,new T(function(){return B(_f(B(_21(_2h,_2m)),_2l[1]));})];}];}else{var _2n=E(_2h);if(_2n[0]==1){var _2o=_2n[1],_2p=E(_2l);return _2p[0]==0?[1,function(_2q){return new F(function(){return _2b(B(A(_2o,[_2q])),_2p);});}]:[1,function(_2r){return new F(function(){return _2b(B(A(_2o,[_2r])),new T(function(){return B(A(_2p[1],[_2r]));}));});}];}else{var _2s=E(_2l);return _2s[0]==0?E(_20):[1,function(_2t){return new F(function(){return _2b(_2n,new T(function(){return B(A(_2s[1],[_2t]));}));});}];}}},_2u=E(_2h);switch(_2u[0]){case 1:var _2v=E(_2i);if(_2v[0]==4){return [1,function(_2w){return [4,new T(function(){return B(_f(B(_21(B(A(_2u[1],[_2w])),_2w)),_2v[1]));})];}];}else{return new F(function(){return _2j(_);});}break;case 4:var _2x=_2u[1],_2y=E(_2i);switch(_2y[0]){case 0:return [1,function(_2z){return [4,new T(function(){return B(_f(_2x,new T(function(){return B(_21(_2y,_2z));},1)));})];}];case 1:return [1,function(_2A){return [4,new T(function(){return B(_f(_2x,new T(function(){return B(_21(B(A(_2y[1],[_2A])),_2A));},1)));})];}];default:return [4,new T(function(){return B(_f(_2x,_2y[1]));})];}break;default:return new F(function(){return _2j(_);});}}}}},_2B=E(_2c);switch(_2B[0]){case 0:var _2C=E(_2d);if(!_2C[0]){return [0,function(_2D){return new F(function(){return _2b(B(A(_2B[1],[_2D])),new T(function(){return B(A(_2C[1],[_2D]));}));});}];}else{return new F(function(){return _2e(_);});}break;case 3:return [3,_2B[1],new T(function(){return B(_2b(_2B[2],_2d));})];default:return new F(function(){return _2e(_);});}},_2E=[0,41],_2F=[1,_2E,_C],_2G=[0,40],_2H=[1,_2G,_C],_2I=function(_2J,_2K){while(1){var _2L=E(_2J);if(!_2L[0]){return E(_2K)[0]==0?true:false;}else{var _2M=E(_2K);if(!_2M[0]){return false;}else{if(E(_2L[1])[1]!=E(_2M[1])[1]){return false;}else{_2J=_2L[2];_2K=_2M[2];continue;}}}}},_2N=function(_2O,_2P){return E(_2O)[1]!=E(_2P)[1];},_2Q=function(_2R,_2S){return E(_2R)[1]==E(_2S)[1];},_2T=[0,_2Q,_2N],_2U=function(_2V,_2W){while(1){var _2X=E(_2V);if(!_2X[0]){return E(_2W)[0]==0?true:false;}else{var _2Y=E(_2W);if(!_2Y[0]){return false;}else{if(E(_2X[1])[1]!=E(_2Y[1])[1]){return false;}else{_2V=_2X[2];_2W=_2Y[2];continue;}}}}},_2Z=function(_30,_31){return !B(_2U(_30,_31))?true:false;},_32=[0,_2U,_2Z],_33=function(_34,_35){var _36=E(_34);switch(_36[0]){case 0:return [0,function(_37){return new F(function(){return _33(B(A(_36[1],[_37])),_35);});}];case 1:return [1,function(_38){return new F(function(){return _33(B(A(_36[1],[_38])),_35);});}];case 2:return [2];case 3:return new F(function(){return _2b(B(A(_35,[_36[1]])),new T(function(){return B(_33(_36[2],_35));}));});break;default:var _39=function(_3a){var _3b=E(_3a);if(!_3b[0]){return [0];}else{var _3c=E(_3b[1]);return new F(function(){return _f(B(_21(B(A(_35,[_3c[1]])),_3c[2])),new T(function(){return B(_39(_3b[2]));},1));});}},_3d=B(_39(_36[1]));return _3d[0]==0?[2]:[4,_3d];}},_3e=0,_3f=function(_3g,_3h){var _3i=E(_3g);if(!_3i){return new F(function(){return A(_3h,[_3e]);});}else{return [0,function(_3j){return E(new T(function(){return B(_3f(_3i-1|0,_3h));}));}];}},_3k=function(_3l,_3m,_3n){return function(_3o){return new F(function(){return A(function(_3p,_3q,_3r){while(1){var _3s=(function(_3t,_3u,_3v){var _3w=E(_3t);switch(_3w[0]){case 0:var _3x=E(_3u);if(!_3x[0]){return E(_3m);}else{_3p=B(A(_3w[1],[_3x[1]]));_3q=_3x[2];var _3y=_3v+1|0;_3r=_3y;return null;}break;case 1:var _3z=B(A(_3w[1],[_3u])),_3A=_3u,_3y=_3v;_3p=_3z;_3q=_3A;_3r=_3y;return null;case 2:return E(_3m);case 3:return function(_3B){return new F(function(){return _3f(_3v,function(_3C){return E(new T(function(){return B(_33(_3w,_3B));}));});});};default:return function(_3D){return new F(function(){return _33(_3w,_3D);});};}})(_3p,_3q,_3r);if(_3s!=null){return _3s;}}},[new T(function(){return B(A(_3l,[_S]));}),_3o,0,_3n]);});};},_3E=function(_3F){return new F(function(){return A(_3F,[_C]);});},_3G=function(_3H,_3I){var _3J=function(_3K){var _3L=E(_3K);if(!_3L[0]){return E(_3E);}else{var _3M=_3L[1];return !B(A(_3H,[_3M]))?E(_3E):function(_3N){return [0,function(_3O){return E(new T(function(){return B(A(new T(function(){return B(_3J(_3L[2]));}),[function(_3P){return new F(function(){return A(_3N,[[1,_3M,_3P]]);});}]));}));}];};}};return function(_3Q){return new F(function(){return A(_3J,[_3Q,_3I]);});};},_3R=[6],_3S=function(_3T){return E(_3T);},_3U=new T(function(){return B(unCStr("valDig: Bad base"));}),_3V=new T(function(){return B(err(_3U));}),_3W=function(_3X,_3Y){var _3Z=function(_40,_41){var _42=E(_40);if(!_42[0]){return function(_43){return new F(function(){return A(_43,[new T(function(){return B(A(_41,[_C]));})]);});};}else{var _44=E(_42[1])[1],_45=function(_46){return function(_47){return [0,function(_48){return E(new T(function(){return B(A(new T(function(){return B(_3Z(_42[2],function(_49){return new F(function(){return A(_41,[[1,_46,_49]]);});}));}),[_47]));}));}];};};switch(E(E(_3X)[1])){case 8:if(48>_44){return function(_4a){return new F(function(){return A(_4a,[new T(function(){return B(A(_41,[_C]));})]);});};}else{if(_44>55){return function(_4b){return new F(function(){return A(_4b,[new T(function(){return B(A(_41,[_C]));})]);});};}else{return new F(function(){return _45([0,_44-48|0]);});}}break;case 10:if(48>_44){return function(_4c){return new F(function(){return A(_4c,[new T(function(){return B(A(_41,[_C]));})]);});};}else{if(_44>57){return function(_4d){return new F(function(){return A(_4d,[new T(function(){return B(A(_41,[_C]));})]);});};}else{return new F(function(){return _45([0,_44-48|0]);});}}break;case 16:if(48>_44){if(97>_44){if(65>_44){return function(_4e){return new F(function(){return A(_4e,[new T(function(){return B(A(_41,[_C]));})]);});};}else{if(_44>70){return function(_4f){return new F(function(){return A(_4f,[new T(function(){return B(A(_41,[_C]));})]);});};}else{return new F(function(){return _45([0,(_44-65|0)+10|0]);});}}}else{if(_44>102){if(65>_44){return function(_4g){return new F(function(){return A(_4g,[new T(function(){return B(A(_41,[_C]));})]);});};}else{if(_44>70){return function(_4h){return new F(function(){return A(_4h,[new T(function(){return B(A(_41,[_C]));})]);});};}else{return new F(function(){return _45([0,(_44-65|0)+10|0]);});}}}else{return new F(function(){return _45([0,(_44-97|0)+10|0]);});}}}else{if(_44>57){if(97>_44){if(65>_44){return function(_4i){return new F(function(){return A(_4i,[new T(function(){return B(A(_41,[_C]));})]);});};}else{if(_44>70){return function(_4j){return new F(function(){return A(_4j,[new T(function(){return B(A(_41,[_C]));})]);});};}else{return new F(function(){return _45([0,(_44-65|0)+10|0]);});}}}else{if(_44>102){if(65>_44){return function(_4k){return new F(function(){return A(_4k,[new T(function(){return B(A(_41,[_C]));})]);});};}else{if(_44>70){return function(_4l){return new F(function(){return A(_4l,[new T(function(){return B(A(_41,[_C]));})]);});};}else{return new F(function(){return _45([0,(_44-65|0)+10|0]);});}}}else{return new F(function(){return _45([0,(_44-97|0)+10|0]);});}}}else{return new F(function(){return _45([0,_44-48|0]);});}}break;default:return E(_3V);}}};return function(_4m){return new F(function(){return A(_3Z,[_4m,_3S,function(_4n){var _4o=E(_4n);return _4o[0]==0?[2]:B(A(_3Y,[_4o]));}]);});};},_4p=[0,10],_4q=[0,1],_4r=[0,2147483647],_4s=function(_4t,_4u){while(1){var _4v=E(_4t);if(!_4v[0]){var _4w=_4v[1],_4x=E(_4u);if(!_4x[0]){var _4y=_4x[1],_4z=addC(_4w,_4y);if(!E(_4z[2])){return [0,_4z[1]];}else{_4t=[1,I_fromInt(_4w)];_4u=[1,I_fromInt(_4y)];continue;}}else{_4t=[1,I_fromInt(_4w)];_4u=_4x;continue;}}else{var _4A=E(_4u);if(!_4A[0]){_4t=_4v;_4u=[1,I_fromInt(_4A[1])];continue;}else{return [1,I_add(_4v[1],_4A[1])];}}}},_4B=new T(function(){return B(_4s(_4r,_4q));}),_4C=function(_4D){var _4E=E(_4D);if(!_4E[0]){var _4F=E(_4E[1]);return _4F==(-2147483648)?E(_4B):[0, -_4F];}else{return [1,I_negate(_4E[1])];}},_4G=[0,10],_4H=[0,0],_4I=function(_4J){return [0,_4J];},_4K=function(_4L,_4M){while(1){var _4N=E(_4L);if(!_4N[0]){var _4O=_4N[1],_4P=E(_4M);if(!_4P[0]){var _4Q=_4P[1];if(!(imul(_4O,_4Q)|0)){return [0,imul(_4O,_4Q)|0];}else{_4L=[1,I_fromInt(_4O)];_4M=[1,I_fromInt(_4Q)];continue;}}else{_4L=[1,I_fromInt(_4O)];_4M=_4P;continue;}}else{var _4R=E(_4M);if(!_4R[0]){_4L=_4N;_4M=[1,I_fromInt(_4R[1])];continue;}else{return [1,I_mul(_4N[1],_4R[1])];}}}},_4S=function(_4T,_4U,_4V){while(1){var _4W=E(_4V);if(!_4W[0]){return E(_4U);}else{var _4X=B(_4s(B(_4K(_4U,_4T)),B(_4I(E(_4W[1])[1]))));_4V=_4W[2];_4U=_4X;continue;}}},_4Y=function(_4Z){var _50=new T(function(){return B(_2b(B(_2b([0,function(_51){return E(E(_51)[1])==45?[1,B(_3W(_4p,function(_52){return new F(function(){return A(_4Z,[[1,new T(function(){return B(_4C(B(_4S(_4G,_4H,_52))));})]]);});}))]:[2];}],[0,function(_53){return E(E(_53)[1])==43?[1,B(_3W(_4p,function(_54){return new F(function(){return A(_4Z,[[1,new T(function(){return B(_4S(_4G,_4H,_54));})]]);});}))]:[2];}])),new T(function(){return [1,B(_3W(_4p,function(_55){return new F(function(){return A(_4Z,[[1,new T(function(){return B(_4S(_4G,_4H,_55));})]]);});}))];})));});return new F(function(){return _2b([0,function(_56){return E(E(_56)[1])==101?E(_50):[2];}],[0,function(_57){return E(E(_57)[1])==69?E(_50):[2];}]);});},_58=[0],_59=function(_5a){return new F(function(){return A(_5a,[_58]);});},_5b=function(_5c){return new F(function(){return A(_5c,[_58]);});},_5d=function(_5e){return function(_5f){return E(E(_5f)[1])==46?[1,B(_3W(_4p,function(_5g){return new F(function(){return A(_5e,[[1,_5g]]);});}))]:[2];};},_5h=function(_5i){return [0,B(_5d(_5i))];},_5j=function(_5k){return new F(function(){return _3W(_4p,function(_5l){return [1,B(_3k(_5h,_59,function(_5m){return [1,B(_3k(_4Y,_5b,function(_5n){return new F(function(){return A(_5k,[[5,[1,_5l,_5m,_5n]]]);});}))];}))];});});},_5o=function(_5p){return [1,B(_5j(_5p))];},_5q=function(_5r){return E(E(_5r)[1]);},_5s=function(_5t,_5u,_5v){while(1){var _5w=E(_5v);if(!_5w[0]){return false;}else{if(!B(A(_5q,[_5t,_5u,_5w[1]]))){_5v=_5w[2];continue;}else{return true;}}}},_5x=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_5y=function(_5z){return new F(function(){return _5s(_2T,_5z,_5x);});},_5A=[0,8],_5B=[0,16],_5C=function(_5D){var _5E=function(_5F){return new F(function(){return A(_5D,[[5,[0,_5A,_5F]]]);});},_5G=function(_5H){return new F(function(){return A(_5D,[[5,[0,_5B,_5H]]]);});};return function(_5I){return E(E(_5I)[1])==48?E([0,function(_5J){switch(E(E(_5J)[1])){case 79:return [1,B(_3W(_5A,_5E))];case 88:return [1,B(_3W(_5B,_5G))];case 111:return [1,B(_3W(_5A,_5E))];case 120:return [1,B(_3W(_5B,_5G))];default:return [2];}}]):[2];};},_5K=function(_5L){return [0,B(_5C(_5L))];},_5M=false,_5N=true,_5O=function(_5P){var _5Q=new T(function(){return B(A(_5P,[_5A]));}),_5R=new T(function(){return B(A(_5P,[_5B]));});return function(_5S){switch(E(E(_5S)[1])){case 79:return E(_5Q);case 88:return E(_5R);case 111:return E(_5Q);case 120:return E(_5R);default:return [2];}};},_5T=function(_5U){return [0,B(_5O(_5U))];},_5V=[0,92],_5W=function(_5X){return new F(function(){return A(_5X,[_4p]);});},_5Y=function(_5Z){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_q(9,_5Z,_C));}))));});},_60=function(_61){var _62=E(_61);return _62[0]==0?E(_62[1]):I_toInt(_62[1]);},_63=function(_64,_65){var _66=E(_64);if(!_66[0]){var _67=_66[1],_68=E(_65);return _68[0]==0?_67<=_68[1]:I_compareInt(_68[1],_67)>=0;}else{var _69=_66[1],_6a=E(_65);return _6a[0]==0?I_compareInt(_69,_6a[1])<=0:I_compare(_69,_6a[1])<=0;}},_6b=function(_6c){return [2];},_6d=function(_6e){var _6f=E(_6e);if(!_6f[0]){return E(_6b);}else{var _6g=_6f[1],_6h=E(_6f[2]);return _6h[0]==0?E(_6g):function(_6i){return new F(function(){return _2b(B(A(_6g,[_6i])),new T(function(){return B(A(new T(function(){return B(_6d(_6h));}),[_6i]));}));});};}},_6j=function(_6k){return [2];},_6l=function(_6m,_6n){var _6o=function(_6p,_6q){var _6r=E(_6p);if(!_6r[0]){return function(_6s){return new F(function(){return A(_6s,[_6m]);});};}else{var _6t=E(_6q);return _6t[0]==0?E(_6j):E(_6r[1])[1]!=E(_6t[1])[1]?E(_6j):function(_6u){return [0,function(_6v){return E(new T(function(){return B(A(new T(function(){return B(_6o(_6r[2],_6t[2]));}),[_6u]));}));}];};}};return function(_6w){return new F(function(){return A(_6o,[_6m,_6w,_6n]);});};},_6x=new T(function(){return B(unCStr("SOH"));}),_6y=[0,1],_6z=function(_6A){return [1,B(_6l(_6x,function(_6B){return E(new T(function(){return B(A(_6A,[_6y]));}));}))];},_6C=new T(function(){return B(unCStr("SO"));}),_6D=[0,14],_6E=function(_6F){return [1,B(_6l(_6C,function(_6G){return E(new T(function(){return B(A(_6F,[_6D]));}));}))];},_6H=function(_6I){return [1,B(_3k(_6z,_6E,_6I))];},_6J=new T(function(){return B(unCStr("NUL"));}),_6K=[0,0],_6L=function(_6M){return [1,B(_6l(_6J,function(_6N){return E(new T(function(){return B(A(_6M,[_6K]));}));}))];},_6O=new T(function(){return B(unCStr("STX"));}),_6P=[0,2],_6Q=function(_6R){return [1,B(_6l(_6O,function(_6S){return E(new T(function(){return B(A(_6R,[_6P]));}));}))];},_6T=new T(function(){return B(unCStr("ETX"));}),_6U=[0,3],_6V=function(_6W){return [1,B(_6l(_6T,function(_6X){return E(new T(function(){return B(A(_6W,[_6U]));}));}))];},_6Y=new T(function(){return B(unCStr("EOT"));}),_6Z=[0,4],_70=function(_71){return [1,B(_6l(_6Y,function(_72){return E(new T(function(){return B(A(_71,[_6Z]));}));}))];},_73=new T(function(){return B(unCStr("ENQ"));}),_74=[0,5],_75=function(_76){return [1,B(_6l(_73,function(_77){return E(new T(function(){return B(A(_76,[_74]));}));}))];},_78=new T(function(){return B(unCStr("ACK"));}),_79=[0,6],_7a=function(_7b){return [1,B(_6l(_78,function(_7c){return E(new T(function(){return B(A(_7b,[_79]));}));}))];},_7d=new T(function(){return B(unCStr("BEL"));}),_7e=[0,7],_7f=function(_7g){return [1,B(_6l(_7d,function(_7h){return E(new T(function(){return B(A(_7g,[_7e]));}));}))];},_7i=new T(function(){return B(unCStr("BS"));}),_7j=[0,8],_7k=function(_7l){return [1,B(_6l(_7i,function(_7m){return E(new T(function(){return B(A(_7l,[_7j]));}));}))];},_7n=new T(function(){return B(unCStr("HT"));}),_7o=[0,9],_7p=function(_7q){return [1,B(_6l(_7n,function(_7r){return E(new T(function(){return B(A(_7q,[_7o]));}));}))];},_7s=new T(function(){return B(unCStr("LF"));}),_7t=[0,10],_7u=function(_7v){return [1,B(_6l(_7s,function(_7w){return E(new T(function(){return B(A(_7v,[_7t]));}));}))];},_7x=new T(function(){return B(unCStr("VT"));}),_7y=[0,11],_7z=function(_7A){return [1,B(_6l(_7x,function(_7B){return E(new T(function(){return B(A(_7A,[_7y]));}));}))];},_7C=new T(function(){return B(unCStr("FF"));}),_7D=[0,12],_7E=function(_7F){return [1,B(_6l(_7C,function(_7G){return E(new T(function(){return B(A(_7F,[_7D]));}));}))];},_7H=new T(function(){return B(unCStr("CR"));}),_7I=[0,13],_7J=function(_7K){return [1,B(_6l(_7H,function(_7L){return E(new T(function(){return B(A(_7K,[_7I]));}));}))];},_7M=new T(function(){return B(unCStr("SI"));}),_7N=[0,15],_7O=function(_7P){return [1,B(_6l(_7M,function(_7Q){return E(new T(function(){return B(A(_7P,[_7N]));}));}))];},_7R=new T(function(){return B(unCStr("DLE"));}),_7S=[0,16],_7T=function(_7U){return [1,B(_6l(_7R,function(_7V){return E(new T(function(){return B(A(_7U,[_7S]));}));}))];},_7W=new T(function(){return B(unCStr("DC1"));}),_7X=[0,17],_7Y=function(_7Z){return [1,B(_6l(_7W,function(_80){return E(new T(function(){return B(A(_7Z,[_7X]));}));}))];},_81=new T(function(){return B(unCStr("DC2"));}),_82=[0,18],_83=function(_84){return [1,B(_6l(_81,function(_85){return E(new T(function(){return B(A(_84,[_82]));}));}))];},_86=new T(function(){return B(unCStr("DC3"));}),_87=[0,19],_88=function(_89){return [1,B(_6l(_86,function(_8a){return E(new T(function(){return B(A(_89,[_87]));}));}))];},_8b=new T(function(){return B(unCStr("DC4"));}),_8c=[0,20],_8d=function(_8e){return [1,B(_6l(_8b,function(_8f){return E(new T(function(){return B(A(_8e,[_8c]));}));}))];},_8g=new T(function(){return B(unCStr("NAK"));}),_8h=[0,21],_8i=function(_8j){return [1,B(_6l(_8g,function(_8k){return E(new T(function(){return B(A(_8j,[_8h]));}));}))];},_8l=new T(function(){return B(unCStr("SYN"));}),_8m=[0,22],_8n=function(_8o){return [1,B(_6l(_8l,function(_8p){return E(new T(function(){return B(A(_8o,[_8m]));}));}))];},_8q=new T(function(){return B(unCStr("ETB"));}),_8r=[0,23],_8s=function(_8t){return [1,B(_6l(_8q,function(_8u){return E(new T(function(){return B(A(_8t,[_8r]));}));}))];},_8v=new T(function(){return B(unCStr("CAN"));}),_8w=[0,24],_8x=function(_8y){return [1,B(_6l(_8v,function(_8z){return E(new T(function(){return B(A(_8y,[_8w]));}));}))];},_8A=new T(function(){return B(unCStr("EM"));}),_8B=[0,25],_8C=function(_8D){return [1,B(_6l(_8A,function(_8E){return E(new T(function(){return B(A(_8D,[_8B]));}));}))];},_8F=new T(function(){return B(unCStr("SUB"));}),_8G=[0,26],_8H=function(_8I){return [1,B(_6l(_8F,function(_8J){return E(new T(function(){return B(A(_8I,[_8G]));}));}))];},_8K=new T(function(){return B(unCStr("ESC"));}),_8L=[0,27],_8M=function(_8N){return [1,B(_6l(_8K,function(_8O){return E(new T(function(){return B(A(_8N,[_8L]));}));}))];},_8P=new T(function(){return B(unCStr("FS"));}),_8Q=[0,28],_8R=function(_8S){return [1,B(_6l(_8P,function(_8T){return E(new T(function(){return B(A(_8S,[_8Q]));}));}))];},_8U=new T(function(){return B(unCStr("GS"));}),_8V=[0,29],_8W=function(_8X){return [1,B(_6l(_8U,function(_8Y){return E(new T(function(){return B(A(_8X,[_8V]));}));}))];},_8Z=new T(function(){return B(unCStr("RS"));}),_90=[0,30],_91=function(_92){return [1,B(_6l(_8Z,function(_93){return E(new T(function(){return B(A(_92,[_90]));}));}))];},_94=new T(function(){return B(unCStr("US"));}),_95=[0,31],_96=function(_97){return [1,B(_6l(_94,function(_98){return E(new T(function(){return B(A(_97,[_95]));}));}))];},_99=new T(function(){return B(unCStr("SP"));}),_9a=[0,32],_9b=function(_9c){return [1,B(_6l(_99,function(_9d){return E(new T(function(){return B(A(_9c,[_9a]));}));}))];},_9e=new T(function(){return B(unCStr("DEL"));}),_9f=[0,127],_9g=function(_9h){return [1,B(_6l(_9e,function(_9i){return E(new T(function(){return B(A(_9h,[_9f]));}));}))];},_9j=[1,_9g,_C],_9k=[1,_9b,_9j],_9l=[1,_96,_9k],_9m=[1,_91,_9l],_9n=[1,_8W,_9m],_9o=[1,_8R,_9n],_9p=[1,_8M,_9o],_9q=[1,_8H,_9p],_9r=[1,_8C,_9q],_9s=[1,_8x,_9r],_9t=[1,_8s,_9s],_9u=[1,_8n,_9t],_9v=[1,_8i,_9u],_9w=[1,_8d,_9v],_9x=[1,_88,_9w],_9y=[1,_83,_9x],_9z=[1,_7Y,_9y],_9A=[1,_7T,_9z],_9B=[1,_7O,_9A],_9C=[1,_7J,_9B],_9D=[1,_7E,_9C],_9E=[1,_7z,_9D],_9F=[1,_7u,_9E],_9G=[1,_7p,_9F],_9H=[1,_7k,_9G],_9I=[1,_7f,_9H],_9J=[1,_7a,_9I],_9K=[1,_75,_9J],_9L=[1,_70,_9K],_9M=[1,_6V,_9L],_9N=[1,_6Q,_9M],_9O=[1,_6L,_9N],_9P=[1,_6H,_9O],_9Q=new T(function(){return B(_6d(_9P));}),_9R=[0,1114111],_9S=[0,34],_9T=[0,39],_9U=function(_9V){var _9W=new T(function(){return B(A(_9V,[_7e]));}),_9X=new T(function(){return B(A(_9V,[_7j]));}),_9Y=new T(function(){return B(A(_9V,[_7o]));}),_9Z=new T(function(){return B(A(_9V,[_7t]));}),_a0=new T(function(){return B(A(_9V,[_7y]));}),_a1=new T(function(){return B(A(_9V,[_7D]));}),_a2=new T(function(){return B(A(_9V,[_7I]));});return new F(function(){return _2b([0,function(_a3){switch(E(E(_a3)[1])){case 34:return E(new T(function(){return B(A(_9V,[_9S]));}));case 39:return E(new T(function(){return B(A(_9V,[_9T]));}));case 92:return E(new T(function(){return B(A(_9V,[_5V]));}));case 97:return E(_9W);case 98:return E(_9X);case 102:return E(_a1);case 110:return E(_9Z);case 114:return E(_a2);case 116:return E(_9Y);case 118:return E(_a0);default:return [2];}}],new T(function(){return B(_2b([1,B(_3k(_5T,_5W,function(_a4){return [1,B(_3W(_a4,function(_a5){var _a6=B(_4S(new T(function(){return B(_4I(E(_a4)[1]));}),_4H,_a5));return !B(_63(_a6,_9R))?[2]:B(A(_9V,[new T(function(){var _a7=B(_60(_a6));if(_a7>>>0>1114111){var _a8=B(_5Y(_a7));}else{var _a8=[0,_a7];}var _a9=_a8,_aa=_a9,_ab=_aa;return _ab;})]));}))];}))],new T(function(){return B(_2b([0,function(_ac){return E(E(_ac)[1])==94?E([0,function(_ad){switch(E(E(_ad)[1])){case 64:return E(new T(function(){return B(A(_9V,[_6K]));}));case 65:return E(new T(function(){return B(A(_9V,[_6y]));}));case 66:return E(new T(function(){return B(A(_9V,[_6P]));}));case 67:return E(new T(function(){return B(A(_9V,[_6U]));}));case 68:return E(new T(function(){return B(A(_9V,[_6Z]));}));case 69:return E(new T(function(){return B(A(_9V,[_74]));}));case 70:return E(new T(function(){return B(A(_9V,[_79]));}));case 71:return E(_9W);case 72:return E(_9X);case 73:return E(_9Y);case 74:return E(_9Z);case 75:return E(_a0);case 76:return E(_a1);case 77:return E(_a2);case 78:return E(new T(function(){return B(A(_9V,[_6D]));}));case 79:return E(new T(function(){return B(A(_9V,[_7N]));}));case 80:return E(new T(function(){return B(A(_9V,[_7S]));}));case 81:return E(new T(function(){return B(A(_9V,[_7X]));}));case 82:return E(new T(function(){return B(A(_9V,[_82]));}));case 83:return E(new T(function(){return B(A(_9V,[_87]));}));case 84:return E(new T(function(){return B(A(_9V,[_8c]));}));case 85:return E(new T(function(){return B(A(_9V,[_8h]));}));case 86:return E(new T(function(){return B(A(_9V,[_8m]));}));case 87:return E(new T(function(){return B(A(_9V,[_8r]));}));case 88:return E(new T(function(){return B(A(_9V,[_8w]));}));case 89:return E(new T(function(){return B(A(_9V,[_8B]));}));case 90:return E(new T(function(){return B(A(_9V,[_8G]));}));case 91:return E(new T(function(){return B(A(_9V,[_8L]));}));case 92:return E(new T(function(){return B(A(_9V,[_8Q]));}));case 93:return E(new T(function(){return B(A(_9V,[_8V]));}));case 94:return E(new T(function(){return B(A(_9V,[_90]));}));case 95:return E(new T(function(){return B(A(_9V,[_95]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_9Q,[_9V]));})));})));}));});},_ae=function(_af){return new F(function(){return A(_af,[_3e]);});},_ag=function(_ah){var _ai=E(_ah);if(!_ai[0]){return E(_ae);}else{var _aj=_ai[2],_ak=E(E(_ai[1])[1]);switch(_ak){case 9:return function(_al){return [0,function(_am){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_al]));}));}];};case 10:return function(_an){return [0,function(_ao){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_an]));}));}];};case 11:return function(_ap){return [0,function(_aq){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_ap]));}));}];};case 12:return function(_ar){return [0,function(_as){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_ar]));}));}];};case 13:return function(_at){return [0,function(_au){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_at]));}));}];};case 32:return function(_av){return [0,function(_aw){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_av]));}));}];};case 160:return function(_ax){return [0,function(_ay){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_ax]));}));}];};default:var _az=u_iswspace(_ak),_aA=_az;return E(_aA)==0?E(_ae):function(_aB){return [0,function(_aC){return E(new T(function(){return B(A(new T(function(){return B(_ag(_aj));}),[_aB]));}));}];};}}},_aD=function(_aE){var _aF=new T(function(){return B(_aD(_aE));}),_aG=[1,function(_aH){return new F(function(){return A(_ag,[_aH,function(_aI){return E([0,function(_aJ){return E(E(_aJ)[1])==92?E(_aF):[2];}]);}]);});}];return new F(function(){return _2b([0,function(_aK){return E(E(_aK)[1])==92?E([0,function(_aL){var _aM=E(E(_aL)[1]);switch(_aM){case 9:return E(_aG);case 10:return E(_aG);case 11:return E(_aG);case 12:return E(_aG);case 13:return E(_aG);case 32:return E(_aG);case 38:return E(_aF);case 160:return E(_aG);default:var _aN=u_iswspace(_aM),_aO=_aN;return E(_aO)==0?[2]:E(_aG);}}]):[2];}],[0,function(_aP){var _aQ=E(_aP);return E(_aQ[1])==92?E(new T(function(){return B(_9U(function(_aR){return new F(function(){return A(_aE,[[0,_aR,_5N]]);});}));})):B(A(_aE,[[0,_aQ,_5M]]));}]);});},_aS=function(_aT,_aU){return new F(function(){return _aD(function(_aV){var _aW=E(_aV),_aX=E(_aW[1]);if(E(_aX[1])==34){if(!E(_aW[2])){return E(new T(function(){return B(A(_aU,[[1,new T(function(){return B(A(_aT,[_C]));})]]));}));}else{return new F(function(){return _aS(function(_aY){return new F(function(){return A(_aT,[[1,_aX,_aY]]);});},_aU);});}}else{return new F(function(){return _aS(function(_aZ){return new F(function(){return A(_aT,[[1,_aX,_aZ]]);});},_aU);});}});});},_b0=new T(function(){return B(unCStr("_\'"));}),_b1=function(_b2){var _b3=u_iswalnum(_b2),_b4=_b3;return E(_b4)==0?B(_5s(_2T,[0,_b2],_b0)):true;},_b5=function(_b6){return new F(function(){return _b1(E(_b6)[1]);});},_b7=new T(function(){return B(unCStr(",;()[]{}`"));}),_b8=new T(function(){return B(unCStr(".."));}),_b9=new T(function(){return B(unCStr("::"));}),_ba=new T(function(){return B(unCStr("->"));}),_bb=[0,64],_bc=[1,_bb,_C],_bd=[0,126],_be=[1,_bd,_C],_bf=new T(function(){return B(unCStr("=>"));}),_bg=[1,_bf,_C],_bh=[1,_be,_bg],_bi=[1,_bc,_bh],_bj=[1,_ba,_bi],_bk=new T(function(){return B(unCStr("<-"));}),_bl=[1,_bk,_bj],_bm=[0,124],_bn=[1,_bm,_C],_bo=[1,_bn,_bl],_bp=[1,_5V,_C],_bq=[1,_bp,_bo],_br=[0,61],_bs=[1,_br,_C],_bt=[1,_bs,_bq],_bu=[1,_b9,_bt],_bv=[1,_b8,_bu],_bw=function(_bx){return new F(function(){return _2b([1,function(_by){return E(_by)[0]==0?E(new T(function(){return B(A(_bx,[_3R]));})):[2];}],new T(function(){return B(_2b([0,function(_bz){return E(E(_bz)[1])==39?E([0,function(_bA){var _bB=E(_bA);switch(E(_bB[1])){case 39:return [2];case 92:return E(new T(function(){return B(_9U(function(_bC){return [0,function(_bD){return E(E(_bD)[1])==39?E(new T(function(){return B(A(_bx,[[0,_bC]]));})):[2];}];}));}));default:return [0,function(_bE){return E(E(_bE)[1])==39?E(new T(function(){return B(A(_bx,[[0,_bB]]));})):[2];}];}}]):[2];}],new T(function(){return B(_2b([0,function(_bF){return E(E(_bF)[1])==34?E(new T(function(){return B(_aS(_3S,_bx));})):[2];}],new T(function(){return B(_2b([0,function(_bG){return !B(_5s(_2T,_bG,_b7))?[2]:B(A(_bx,[[2,[1,_bG,_C]]]));}],new T(function(){return B(_2b([0,function(_bH){return !B(_5s(_2T,_bH,_5x))?[2]:[1,B(_3G(_5y,function(_bI){var _bJ=[1,_bH,_bI];return !B(_5s(_32,_bJ,_bv))?B(A(_bx,[[4,_bJ]])):B(A(_bx,[[2,_bJ]]));}))];}],new T(function(){return B(_2b([0,function(_bK){var _bL=E(_bK),_bM=_bL[1],_bN=u_iswalpha(_bM),_bO=_bN;return E(_bO)==0?E(_bM)==95?[1,B(_3G(_b5,function(_bP){return new F(function(){return A(_bx,[[3,[1,_bL,_bP]]]);});}))]:[2]:[1,B(_3G(_b5,function(_bQ){return new F(function(){return A(_bx,[[3,[1,_bL,_bQ]]]);});}))];}],new T(function(){return [1,B(_3k(_5K,_5o,_bx))];})));})));})));})));})));}));});},_bR=[0,0],_bS=function(_bT,_bU){return function(_bV){return new F(function(){return A(_ag,[_bV,function(_bW){return E(new T(function(){return B(_bw(function(_bX){var _bY=E(_bX);return _bY[0]==2?!B(_2I(_bY[1],_2H))?[2]:E(new T(function(){return B(A(_bT,[_bR,function(_bZ){return [1,function(_c0){return new F(function(){return A(_ag,[_c0,function(_c1){return E(new T(function(){return B(_bw(function(_c2){var _c3=E(_c2);return _c3[0]==2?!B(_2I(_c3[1],_2F))?[2]:E(new T(function(){return B(A(_bU,[_bZ]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_c4=function(_c5,_c6,_c7){var _c8=function(_c9,_ca){return new F(function(){return _2b([1,function(_cb){return new F(function(){return A(_ag,[_cb,function(_cc){return E(new T(function(){return B(_bw(function(_cd){var _ce=E(_cd);if(_ce[0]==4){var _cf=E(_ce[1]);if(!_cf[0]){return new F(function(){return A(_c5,[_ce,_c9,_ca]);});}else{return E(E(_cf[1])[1])==45?E(_cf[2])[0]==0?E([1,function(_cg){return new F(function(){return A(_ag,[_cg,function(_ch){return E(new T(function(){return B(_bw(function(_ci){return new F(function(){return A(_c5,[_ci,_c9,function(_cj){return new F(function(){return A(_ca,[new T(function(){return [0, -E(_cj)[1]];})]);});}]);});}));}));}]);});}]):B(A(_c5,[_ce,_c9,_ca])):B(A(_c5,[_ce,_c9,_ca]));}}else{return new F(function(){return A(_c5,[_ce,_c9,_ca]);});}}));}));}]);});}],new T(function(){return [1,B(_bS(_c8,_ca))];}));});};return new F(function(){return _c8(_c6,_c7);});},_ck=function(_cl,_cm){return [2];},_cn=function(_co){var _cp=E(_co);return _cp[0]==0?[1,new T(function(){return B(_4S(new T(function(){return B(_4I(E(_cp[1])[1]));}),_4H,_cp[2]));})]:E(_cp[2])[0]==0?E(_cp[3])[0]==0?[1,new T(function(){return B(_4S(_4G,_4H,_cp[1]));})]:[0]:[0];},_cq=function(_cr){var _cs=E(_cr);if(_cs[0]==5){var _ct=B(_cn(_cs[1]));return _ct[0]==0?E(_ck):function(_cu,_cv){return new F(function(){return A(_cv,[new T(function(){return [0,B(_60(_ct[1]))];})]);});};}else{return E(_ck);}},_cw=function(_cx,_cy){return new F(function(){return _c4(_cq,_cx,_cy);});},_cz=[0,91],_cA=[1,_cz,_C],_cB=function(_cC,_cD){var _cE=function(_cF,_cG){return [1,function(_cH){return new F(function(){return A(_ag,[_cH,function(_cI){return E(new T(function(){return B(_bw(function(_cJ){var _cK=E(_cJ);if(_cK[0]==2){var _cL=E(_cK[1]);if(!_cL[0]){return [2];}else{var _cM=_cL[2];switch(E(E(_cL[1])[1])){case 44:return E(_cM)[0]==0?!E(_cF)?[2]:E(new T(function(){return B(A(_cC,[_bR,function(_cN){return new F(function(){return _cE(_5N,function(_cO){return new F(function(){return A(_cG,[[1,_cN,_cO]]);});});});}]));})):[2];case 93:return E(_cM)[0]==0?E(new T(function(){return B(A(_cG,[_C]));})):[2];default:return [2];}}}else{return [2];}}));}));}]);});}];},_cP=function(_cQ){return new F(function(){return _2b([1,function(_cR){return new F(function(){return A(_ag,[_cR,function(_cS){return E(new T(function(){return B(_bw(function(_cT){var _cU=E(_cT);return _cU[0]==2?!B(_2I(_cU[1],_cA))?[2]:E(new T(function(){return B(_2b(B(_cE(_5M,_cQ)),new T(function(){return B(A(_cC,[_bR,function(_cV){return new F(function(){return _cE(_5N,function(_cW){return new F(function(){return A(_cQ,[[1,_cV,_cW]]);});});});}]));})));})):[2];}));}));}]);});}],new T(function(){return [1,B(_bS(function(_cX,_cY){return new F(function(){return _cP(_cY);});},_cQ))];}));});};return new F(function(){return _cP(_cD);});},_cZ=function(_d0,_d1){return new F(function(){return _cB(_cw,_d1);});},_d2=function(_d3){return function(_3D){return new F(function(){return _21(new T(function(){return B(_c4(_cq,_d3,_S));}),_3D);});};},_d4=new T(function(){return B(_cB(_cw,_S));}),_d5=function(_cy){return new F(function(){return _21(_d4,_cy);});},_d6=[0,_d2,_d5,_cw,_cZ],_d7=[0,44],_d8=[1,_d7,_C],_d9=function(_da){return E(E(_da)[3]);},_db=function(_dc,_dd,_de){return function(_df){return new F(function(){return A(new T(function(){return B(A(_d9,[_dc,_de]));}),[function(_dg){return [1,function(_dh){return new F(function(){return A(_ag,[_dh,function(_di){return E(new T(function(){return B(_bw(function(_dj){var _dk=E(_dj);return _dk[0]==2?!B(_2I(_dk[1],_d8))?[2]:E(new T(function(){return B(A(new T(function(){return B(_d9(_dd));}),[_de,function(_dl){return new F(function(){return A(_df,[[0,_dg,_dl]]);});}]));})):[2];}));}));}]);});}];}]);});};},_dm=function(_dn,_do){var _dp=function(_dq){return new F(function(){return _2b([1,B(_bS(_dn,_dq))],new T(function(){return [1,B(_bS(function(_dr,_ds){return new F(function(){return _dp(_ds);});},_dq))];}));});};return new F(function(){return _dp(_do);});},_dt=function(_du,_dv,_dw,_dx){return new F(function(){return _dm(function(_dy,_dz){return new F(function(){return A(_db,[_du,_dv,_dy,function(_dA){var _dB=E(_dA);return [1,function(_dC){return new F(function(){return A(_ag,[_dC,function(_dD){return E(new T(function(){return B(_bw(function(_dE){var _dF=E(_dE);return _dF[0]==2?!B(_2I(_dF[1],_d8))?[2]:E(new T(function(){return B(A(new T(function(){return B(_d9(_dw));}),[_dy,function(_dG){return new F(function(){return A(_dz,[[0,_dB[1],_dB[2],_dG]]);});}]));})):[2];}));}));}]);});}];}]);});},_dx);});},_dH=function(_dI){return E(E(_dI)[4]);},_dJ=function(_dK,_dL,_dM){return new F(function(){return _cB(new T(function(){return B(_dH(_dK));}),_dM);});},_dN=function(_dO){return function(_3D){return new F(function(){return _21(new T(function(){return B(_cB(new T(function(){return B(_dH(_dO));}),_S));}),_3D);});};},_dP=function(_dQ,_dR){return function(_3D){return new F(function(){return _21(new T(function(){return B(A(_dH,[_dQ,_dR,_S]));}),_3D);});};},_dS=function(_dT){return [0,function(_cy){return new F(function(){return _dP(_dT,_cy);});},new T(function(){return B(_dN(_dT));}),new T(function(){return B(_dH(_dT));}),function(_cx,_cy){return new F(function(){return _dJ(_dT,_cx,_cy);});}];},_dU=new T(function(){return B(_dS(_d6));}),_dV=new T(function(){return B(_dt(_dU,_dU,_d6,_S));}),_dW=function(_dX,_){return new T(function(){var _dY=B(_21(_dV,_dX));if(!_dY[0]){var _dZ=E(_Q);}else{if(!E(_dY[2])[0]){var _e0=E(E(_dY[1])[1]),_e1=[1,_p,new T(function(){return B(A(_J,[_1,[1,function(_e2){return new F(function(){return _z(_e0[1],_e2);});},[1,function(_e2){return new F(function(){return _z(_e0[2],_e2);});},[1,function(_e3){return new F(function(){return _q(0,E(_e0[3])[1],_e3);});},_C]]],_P]));})];}else{var _e1=E(_Q);}var _dZ=_e1;}return _dZ;});},_e4=function(_e5){var _e6=B(A(_e5,[_])),_e7=_e6;return E(_e7);},_e8=function(_e9){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return eval(_e9);});});});},_ea=new T(function(){return [0,"(function(s,f){Haste[s] = f;})"];}),_eb=new T(function(){return B(_e8(E(_ea)[1]));}),_ec=function(_ed,_ee){return function(_ef,_){var _eg=B(A(new T(function(){return B(A(_eb,[E(E(_ee)[1])]));}),[B(A(_ed,[_ef])),_])),_eh=_eg;return _3e;};},_ei=new T(function(){return [0,"runKernel"];}),_ej=new T(function(){return B(_e8("lst2arr"));}),_ek=new T(function(){return B(_e8("(function(f) {  return (function() {      return (function(){        var args=Array.prototype.slice.call(arguments,0);        args.push(0);        return E(B(A(f, args)));    });  });})"));}),_el=function(_em,_){return new F(function(){return A(_ek,[E(_em),_]);});},_en=function(_eo,_){return new F(function(){return _el(_eo,_);});},_ep=function(_eq,_er){var _es=E(_er);return _es[0]==0?[0]:[1,new T(function(){return B(A(_eq,[_es[1]]));}),new T(function(){return B(_ep(_eq,_es[2]));})];},_et=function(_eu){return E(E(_eu)[1]);},_ev=function(_ew){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return _en(function(_ex){return new F(function(){return _e4(function(_){var _=0,_ey=String(_ex),_ez=_ey,_eA=B(A(_ew,[[0,_ez],_])),_eB=_eA;return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_ej,[B(_ep(_et,_eB)),_]);});});});});});},_);});});});},_eC=new T(function(){return B(_ec(_ev,_ei));}),_eD=new T(function(){return [0,"getMuxShape"];}),_eE=function(_eF){var _eG=String(_eF),_eH=_eG;return new F(function(){return fromJSStr(_eH);});},_eI=function(_eJ){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return _en(function(_eK){return new F(function(){return _e4(function(_){var _=0,_eL=B(A(_eJ,[B(_eE(_eK)),_])),_eM=_eL;return E(toJSStr(E(_eM)));});});},_);});});});},_eN=new T(function(){return B(_ec(_eI,_eD));}),_eO=function(_eP,_eQ){return !B(_2I(E(_eP)[3],E(_eQ)[3]))?true:false;},_eR=function(_eS,_eT){return new F(function(){return _2I(E(_eS)[3],E(_eT)[3]);});},_eU=[0,_eR,_eO],_eV=function(_eW,_eX){while(1){var _eY=(function(_eZ,_f0){var _f1=E(_f0);if(!_f1[0]){return [0];}else{var _f2=_f1[1],_f3=_f1[2];if(!B(A(_eZ,[_f2]))){var _f4=_eZ;_eX=_f3;_eW=_f4;return null;}else{return [1,_f2,new T(function(){return B(_eV(_eZ,_f3));})];}}})(_eW,_eX);if(_eY!=null){return _eY;}}},_f5=function(_f6){return E(E(_f6)[1]);},_f7=function(_f8){return E(E(_f8)[3]);},_f9=function(_fa,_fb){var _fc=function(_fd,_fe){while(1){var _ff=(function(_fg,_fh){var _fi=E(_fg);if(!_fi[0]){return E(_fh);}else{var _fj=_fi[1],_fk=_fi[2];if(!B(_5s(_eU,_fj,_fh))){_fd=_fk;_fe=[1,_fj,new T(function(){return B(_fc(B(_eV(function(_fl){return new F(function(){return _5s(_32,new T(function(){return B(_f7(_fl));}),B(_ep(_f5,E(_fj)[2])));});},_fa)),_fh));})];return null;}else{_fd=_fk;var _fm=_fh;_fe=_fm;return null;}}})(_fd,_fe);if(_ff!=null){return _ff;}}};return new F(function(){return _fc(_fb,_C);});},_fn=function(_fo){var _fp=E(_fo);return [0,_fp[1],_fp[2],_fp[3],_fp[4]];},_fq=function(_fr,_fs){while(1){var _ft=(function(_fu,_fv){var _fw=E(_fv);if(!_fw[0]){return E(_fu);}else{_fr=new T(function(){var _fx=E(_fw[1]);return [1,[0,_fx[1],_fx[2],_fx[3],_fx[4]],_fu];});_fs=_fw[2];return null;}})(_fr,_fs);if(_ft!=null){return _ft;}}},_fy=function(_fz,_fA){while(1){var _fB=E(_fz);if(!_fB[0]){return E(_fA);}else{_fz=_fB[2];var _fC=[1,_fB[1],_fA];_fA=_fC;continue;}}},_fD=function(_fE,_){return new T(function(){var _fF=B(_fq(_C,_fE));return B(_ep(_fn,B(_fy(B(_f9(_fF,_fF)),_C))));});},_fG=new T(function(){return [0,"topSort"];}),_fH=new T(function(){return [0,"arr2lst"];}),_fI=function(_fJ,_fK){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_e8,[E(_fH)[1],E(_fJ),E(_fK),_]);});});});},_fL=function(_fM){return E(toJSStr(E(_fM)));},_fN=function(_fO){var _fP=E(_fO);return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_ej,[[1,new T(function(){return B(_fL(_fP[1]));}),[1,new T(function(){return E(E(_fP[2])[1]);}),_C]],_]);});});});},_fQ=function(_fR,_fS,_fT,_fU){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_ej,[[1,new T(function(){return B(_fL(_fR));}),[1,new T(function(){return B(_e4(function(_){var _=0;return new F(function(){return A(_ej,[B(_ep(_fN,_fS)),_]);});}));}),[1,new T(function(){return B(_fL(_fT));}),[1,new T(function(){return E(E(_fU)[1]);}),_C]]]],_]);});});});},_fV=function(_fW){var _fX=E(_fW);return new F(function(){return _fQ(_fX[1],_fX[2],_fX[3],_fX[4]);});},_fY=function(_fZ){return new F(function(){return _1Y("src/Haste/Foreign.hs:113:12-74|case");});},_g0=new T(function(){return B(_fY(_));}),_g1=function(_g2){var _g3=jsTrunc(_g2),_g4=_g3;return [0,_g4];},_g5=function(_g6){return new F(function(){return _1Y("src/Haste/Foreign.hs:106:12-52|case");});},_g7=new T(function(){return B(_g5(_));}),_g8=function(_g9){var _ga=B(_fI(_g9,0));if(!_ga[0]){return E(_g7);}else{var _gb=E(_ga[2]);return _gb[0]==0?E(_g7):E(_gb[2])[0]==0?[0,new T(function(){return B(_eE(_ga[1]));}),new T(function(){return B(_g1(_gb[1]));})]:E(_g7);}},_gc=function(_gd){var _ge=B(_g8(_gd));return [0,_ge[1],_ge[2]];},_gf=function(_gg){var _gh=B(_fI(_gg,0));if(!_gh[0]){return E(_g0);}else{var _gi=E(_gh[2]);if(!_gi[0]){return E(_g0);}else{var _gj=E(_gi[2]);if(!_gj[0]){return E(_g0);}else{var _gk=E(_gj[2]);return _gk[0]==0?E(_g0):E(_gk[2])[0]==0?[0,new T(function(){return B(_eE(_gh[1]));}),new T(function(){return B(_ep(_gc,B(_fI(_gi[1],0))));}),new T(function(){return B(_eE(_gj[1]));}),new T(function(){return B(_g1(_gk[1]));})]:E(_g0);}}}},_gl=function(_gm){var _gn=B(_gf(_gm));return [0,_gn[1],_gn[2],_gn[3],_gn[4]];},_go=function(_gp){return new F(function(){return _e4(function(_){var _=0;return new F(function(){return _en(function(_gq){return new F(function(){return _e4(function(_){var _=0,_gr=B(A(_gp,[B(_ep(_gl,B(_fI(_gq,0)))),_])),_gs=_gr;return new F(function(){return _e4(function(_){var _=0;return new F(function(){return A(_ej,[B(_ep(_fV,_gs)),_]);});});});});});},_);});});});},_gt=new T(function(){return B(_ec(_go,_fG));}),_gu=function(_gv,_gw){var _gx=strEq(E(_gv)[1],E(_gw)[1]),_gy=_gx;return E(_gy)==0?true:false;},_gz=function(_gA,_gB){var _gC=strEq(E(_gA)[1],E(_gB)[1]),_gD=_gC;return E(_gD)==0?false:true;},_gE=[0,_gz,_gu],_gF=[1,_C],_gG=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_gH=[0,_gG],_gI=new T(function(){return B(unCStr("Tried to deserialize a non-array into a pair!"));}),_gJ=[0,_gI],_gK=function(_gL){return E(E(_gL)[3]);},_gM=function(_gN,_gO,_gP){var _gQ=E(_gP);if(_gQ[0]==3){var _gR=E(_gQ[1]);if(!_gR[0]){return E(_gJ);}else{var _gS=E(_gR[2]);if(!_gS[0]){return E(_gJ);}else{if(!E(_gS[2])[0]){var _gT=B(A(_gK,[_gN,_gR[1]]));if(!_gT[0]){return [0,_gT[1]];}else{var _gU=B(A(_gK,[_gO,_gS[1]]));return _gU[0]==0?[0,_gU[1]]:[1,[0,_gT[1],_gU[1]]];}}else{return E(_gJ);}}}}else{return E(_gJ);}},_gV=function(_gW,_gX,_gY){var _gZ=E(_gY);if(_gZ[0]==3){var _h0=function(_h1){var _h2=E(_h1);if(!_h2[0]){return E(_gF);}else{var _h3=B(_gM(_gW,_gX,_h2[1]));if(!_h3[0]){return [0,_h3[1]];}else{var _h4=B(_h0(_h2[2]));return _h4[0]==0?[0,_h4[1]]:[1,[1,_h3[1],_h4[1]]];}}};return new F(function(){return _h0(_gZ[1]);});}else{return E(_gH);}},_h5=function(_h6){return [0,E(_h6)[1]];},_h7=function(_h8){return [3,E(B(_ep(_h5,_h8)))];},_h9=new T(function(){return B(unCStr("The given Number can\'t be represented as an Int"));}),_ha=[0,_h9],_hb=new T(function(){return B(unCStr("Tried to deserialize a non-Number to an Int"));}),_hc=[0,_hb],_hd=function(_he){var _hf=E(_he);if(!_hf[0]){var _hg=_hf[1],_hh=_hg&4294967295;return _hh!=_hg?E(_ha):[1,[0,_hh]];}else{return E(_hc);}},_hi=[0,_gG],_hj=[1,_C],_hk=[0,_h9],_hl=[0,_hb],_hm=function(_hn){var _ho=E(_hn);if(!_ho[0]){return E(_hj);}else{var _hp=E(_ho[1]);if(!_hp[0]){var _hq=_hp[1],_hr=_hq&4294967295;if(_hr!=_hq){return E(_hk);}else{var _hs=B(_hm(_ho[2]));return _hs[0]==0?[0,_hs[1]]:[1,[1,[0,_hr],_hs[1]]];}}else{return E(_hl);}}},_ht=function(_hu){var _hv=E(_hu);return _hv[0]==3?B(_hm(_hv[1])):E(_hi);},_hw=[0,_h5,_h7,_hd,_ht],_hx=[0,_h9],_hy=new T(function(){return [0,"payload"];}),_hz=[0,_hb],_hA=new T(function(){return [0,"outputCount"];}),_hB=new T(function(){return B(unCStr("Tried to deserialize a non-JSString to a JSString"));}),_hC=[0,_hB],_hD=new T(function(){return [0,"txid"];}),_hE=function(_hF){return [1,toJSStr(E(_hF))];},_hG=new T(function(){return B(unCStr("Tried to deserialize long string to a Char"));}),_hH=[0,_hG],_hI=new T(function(){return B(unCStr("Tried to deserialize a non-string to a Char"));}),_hJ=[0,_hI],_hK=function(_hL){var _hM=E(_hL);if(_hM[0]==1){var _hN=fromJSStr(_hM[1]);return _hN[0]==0?E(_hH):E(_hN[2])[0]==0?[1,_hN[1]]:E(_hH);}else{return E(_hJ);}},_hO=[0,_hB],_hP=function(_hQ){var _hR=E(_hQ);return _hR[0]==1?[1,new T(function(){return fromJSStr(_hR[1]);})]:E(_hO);},_hS=function(_hT){return [1,toJSStr([1,_hT,_C])];},_hU=[0,_hS,_hE,_hK,_hP],_hV=function(_hW){return E(E(_hW)[2]);},_hX=function(_hY,_hZ){return [3,E(B(_ep(new T(function(){return B(_hV(_hY));}),_hZ)))];},_i0=[1,_C],_i1=[0,_gG],_i2=function(_i3){return E(E(_i3)[4]);},_i4=function(_i5,_i6){var _i7=E(_i6);if(_i7[0]==3){var _i8=function(_i9){var _ia=E(_i9);if(!_ia[0]){return E(_i0);}else{var _ib=B(A(new T(function(){return B(_i2(_i5));}),[_ia[1]]));if(!_ib[0]){return [0,_ib[1]];}else{var _ic=B(_i8(_ia[2]));return _ic[0]==0?[0,_ic[1]]:[1,[1,_ib[1],_ic[1]]];}}};return new F(function(){return _i8(_i7[1]);});}else{return E(_i1);}},_id=function(_ie){return [0,new T(function(){return B(_hV(_ie));}),function(_if){return new F(function(){return _hX(_ie,_if);});},new T(function(){return B(_i2(_ie));}),function(_if){return new F(function(){return _i4(_ie,_if);});}];},_ig=new T(function(){return B(_id(_hU));}),_ih=new T(function(){return B(unCStr("Key not found"));}),_ii=[0,_ih],_ij=new T(function(){return [0,"inputs"];}),_ik=new T(function(){return B(unCStr("Tried to do lookup on non-object!"));}),_il=[0,_ik],_im=[0,_ih],_in=[0,_ik],_io=function(_ip,_iq,_ir){while(1){var _is=E(_ir);if(!_is[0]){return [0];}else{var _it=E(_is[1]);if(!B(A(_5q,[_ip,_iq,_it[1]]))){_ir=_is[2];continue;}else{return [1,_it[2]];}}}},_iu=function(_iv,_iw,_ix){var _iy=E(_iw);if(_iy[0]==4){var _iz=B(_io(_gE,_ix,_iy[1]));return _iz[0]==0?E(_im):B(A(_gK,[_iv,_iz[1]]));}else{return E(_in);}},_iA=function(_iB,_iC){var _iD=B(_iu(_iB,_iC,_hy));if(!_iD[0]){return [0,_iD[1]];}else{var _iE=E(_iC);if(_iE[0]==4){var _iF=_iE[1],_iG=B(_io(_gE,_ij,_iF));if(!_iG[0]){return E(_ii);}else{var _iH=B(_gV(_ig,_hw,_iG[1]));if(!_iH[0]){return [0,_iH[1]];}else{var _iI=B(_io(_gE,_hD,_iF));if(!_iI[0]){return E(_ii);}else{var _iJ=E(_iI[1]);if(_iJ[0]==1){var _iK=B(_io(_gE,_hA,_iF));if(!_iK[0]){return E(_ii);}else{var _iL=E(_iK[1]);if(!_iL[0]){var _iM=_iL[1],_iN=_iM&4294967295;return _iN!=_iM?E(_hx):[1,[0,_iD[1],_iH[1],new T(function(){return fromJSStr(_iJ[1]);}),[0,_iN]]];}else{return E(_hz);}}}else{return E(_hC);}}}}}else{return E(_il);}}},_iO=new T(function(){return B(_id(_hU));}),_iP=new T(function(){return B(unCStr("Invalid JSON!"));}),_iQ=new T(function(){return B(err(_iP));}),_iR=function(_iS){var _iT=jsParseJSON(_iS),_iU=_iT,_iV=E(_iU);if(!_iV[0]){return E(_iQ);}else{var _iW=B(_iA(_iO,_iV[1]));return _iW[0]==0?B(err(_iW[1])):E(_iW[1]);}},_iX=[1],_iY=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_iZ=function(_j0){return new F(function(){return err(_iY);});},_j1=new T(function(){return B(_iZ(_));}),_j2=function(_j3,_j4,_j5,_j6){var _j7=E(_j5);if(!_j7[0]){var _j8=_j7[1],_j9=E(_j6);if(!_j9[0]){var _ja=_j9[1],_jb=_j9[2],_jc=_j9[3];if(_ja<=(imul(3,_j8)|0)){return [0,(1+_j8|0)+_ja|0,E(E(_j3)),_j4,E(_j7),E(_j9)];}else{var _jd=E(_j9[4]);if(!_jd[0]){var _je=_jd[1],_jf=_jd[2],_jg=_jd[3],_jh=_jd[4],_ji=E(_j9[5]);if(!_ji[0]){var _jj=_ji[1];if(_je>=(imul(2,_jj)|0)){var _jk=function(_jl){var _jm=E(_j3),_jn=E(_jd[5]);return _jn[0]==0?[0,(1+_j8|0)+_ja|0,E(_jf),_jg,E([0,(1+_j8|0)+_jl|0,E(_jm),_j4,E(_j7),E(_jh)]),E([0,(1+_jj|0)+_jn[1]|0,E(_jb),_jc,E(_jn),E(_ji)])]:[0,(1+_j8|0)+_ja|0,E(_jf),_jg,E([0,(1+_j8|0)+_jl|0,E(_jm),_j4,E(_j7),E(_jh)]),E([0,1+_jj|0,E(_jb),_jc,E(_iX),E(_ji)])];},_jo=E(_jh);return _jo[0]==0?B(_jk(_jo[1])):B(_jk(0));}else{return [0,(1+_j8|0)+_ja|0,E(_jb),_jc,E([0,(1+_j8|0)+_je|0,E(E(_j3)),_j4,E(_j7),E(_jd)]),E(_ji)];}}else{return E(_j1);}}else{return E(_j1);}}}else{return [0,1+_j8|0,E(E(_j3)),_j4,E(_j7),E(_iX)];}}else{var _jp=E(_j6);if(!_jp[0]){var _jq=_jp[1],_jr=_jp[2],_js=_jp[3],_jt=_jp[5],_ju=E(_jp[4]);if(!_ju[0]){var _jv=_ju[1],_jw=_ju[2],_jx=_ju[3],_jy=_ju[4],_jz=E(_jt);if(!_jz[0]){var _jA=_jz[1];if(_jv>=(imul(2,_jA)|0)){var _jB=function(_jC){var _jD=E(_j3),_jE=E(_ju[5]);return _jE[0]==0?[0,1+_jq|0,E(_jw),_jx,E([0,1+_jC|0,E(_jD),_j4,E(_iX),E(_jy)]),E([0,(1+_jA|0)+_jE[1]|0,E(_jr),_js,E(_jE),E(_jz)])]:[0,1+_jq|0,E(_jw),_jx,E([0,1+_jC|0,E(_jD),_j4,E(_iX),E(_jy)]),E([0,1+_jA|0,E(_jr),_js,E(_iX),E(_jz)])];},_jF=E(_jy);return _jF[0]==0?B(_jB(_jF[1])):B(_jB(0));}else{return [0,1+_jq|0,E(_jr),_js,E([0,1+_jv|0,E(E(_j3)),_j4,E(_iX),E(_ju)]),E(_jz)];}}else{return [0,3,E(_jw),_jx,E([0,1,E(E(_j3)),_j4,E(_iX),E(_iX)]),E([0,1,E(_jr),_js,E(_iX),E(_iX)])];}}else{var _jG=E(_jt);return _jG[0]==0?[0,3,E(_jr),_js,E([0,1,E(E(_j3)),_j4,E(_iX),E(_iX)]),E(_jG)]:[0,2,E(E(_j3)),_j4,E(_iX),E(_jp)];}}else{return [0,1,E(E(_j3)),_j4,E(_iX),E(_iX)];}}},_jH=function(_jI,_jJ){return [0,1,E(E(_jI)),_jJ,E(_iX),E(_iX)];},_jK=function(_jL,_jM,_jN){var _jO=E(_jN);if(!_jO[0]){return new F(function(){return _j2(_jO[2],_jO[3],_jO[4],B(_jK(_jL,_jM,_jO[5])));});}else{return new F(function(){return _jH(_jL,_jM);});}},_jP=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_jQ=function(_jR){return new F(function(){return err(_jP);});},_jS=new T(function(){return B(_jQ(_));}),_jT=function(_jU,_jV,_jW,_jX){var _jY=E(_jX);if(!_jY[0]){var _jZ=_jY[1],_k0=E(_jW);if(!_k0[0]){var _k1=_k0[1],_k2=_k0[2],_k3=_k0[3];if(_k1<=(imul(3,_jZ)|0)){return [0,(1+_k1|0)+_jZ|0,E(E(_jU)),_jV,E(_k0),E(_jY)];}else{var _k4=E(_k0[4]);if(!_k4[0]){var _k5=_k4[1],_k6=E(_k0[5]);if(!_k6[0]){var _k7=_k6[1],_k8=_k6[2],_k9=_k6[3],_ka=_k6[4];if(_k7>=(imul(2,_k5)|0)){var _kb=function(_kc){var _kd=E(_k6[5]);return _kd[0]==0?[0,(1+_k1|0)+_jZ|0,E(_k8),_k9,E([0,(1+_k5|0)+_kc|0,E(_k2),_k3,E(_k4),E(_ka)]),E([0,(1+_jZ|0)+_kd[1]|0,E(E(_jU)),_jV,E(_kd),E(_jY)])]:[0,(1+_k1|0)+_jZ|0,E(_k8),_k9,E([0,(1+_k5|0)+_kc|0,E(_k2),_k3,E(_k4),E(_ka)]),E([0,1+_jZ|0,E(E(_jU)),_jV,E(_iX),E(_jY)])];},_ke=E(_ka);return _ke[0]==0?B(_kb(_ke[1])):B(_kb(0));}else{return [0,(1+_k1|0)+_jZ|0,E(_k2),_k3,E(_k4),E([0,(1+_jZ|0)+_k7|0,E(E(_jU)),_jV,E(_k6),E(_jY)])];}}else{return E(_jS);}}else{return E(_jS);}}}else{return [0,1+_jZ|0,E(E(_jU)),_jV,E(_iX),E(_jY)];}}else{var _kf=E(_jW);if(!_kf[0]){var _kg=_kf[1],_kh=_kf[2],_ki=_kf[3],_kj=_kf[5],_kk=E(_kf[4]);if(!_kk[0]){var _kl=_kk[1],_km=E(_kj);if(!_km[0]){var _kn=_km[1],_ko=_km[2],_kp=_km[3],_kq=_km[4];if(_kn>=(imul(2,_kl)|0)){var _kr=function(_ks){var _kt=E(_km[5]);return _kt[0]==0?[0,1+_kg|0,E(_ko),_kp,E([0,(1+_kl|0)+_ks|0,E(_kh),_ki,E(_kk),E(_kq)]),E([0,1+_kt[1]|0,E(E(_jU)),_jV,E(_kt),E(_iX)])]:[0,1+_kg|0,E(_ko),_kp,E([0,(1+_kl|0)+_ks|0,E(_kh),_ki,E(_kk),E(_kq)]),E([0,1,E(E(_jU)),_jV,E(_iX),E(_iX)])];},_ku=E(_kq);return _ku[0]==0?B(_kr(_ku[1])):B(_kr(0));}else{return [0,1+_kg|0,E(_kh),_ki,E(_kk),E([0,1+_kn|0,E(E(_jU)),_jV,E(_km),E(_iX)])];}}else{return [0,3,E(_kh),_ki,E(_kk),E([0,1,E(E(_jU)),_jV,E(_iX),E(_iX)])];}}else{var _kv=E(_kj);return _kv[0]==0?[0,3,E(_kv[2]),_kv[3],E([0,1,E(_kh),_ki,E(_iX),E(_iX)]),E([0,1,E(E(_jU)),_jV,E(_iX),E(_iX)])]:[0,2,E(E(_jU)),_jV,E(_kf),E(_iX)];}}else{return [0,1,E(E(_jU)),_jV,E(_iX),E(_iX)];}}},_kw=function(_kx,_ky,_kz){var _kA=E(_kz);if(!_kA[0]){return new F(function(){return _jT(_kA[2],_kA[3],B(_kw(_kx,_ky,_kA[4])),_kA[5]);});}else{return new F(function(){return _jH(_kx,_ky);});}},_kB=function(_kC,_kD,_kE,_kF,_kG,_kH,_kI){return new F(function(){return _jT(_kF,_kG,B(_kw(_kC,_kD,_kH)),_kI);});},_kJ=function(_kK,_kL,_kM,_kN,_kO,_kP,_kQ,_kR){var _kS=E(_kM);if(!_kS[0]){var _kT=_kS[1],_kU=_kS[2],_kV=_kS[3],_kW=_kS[4],_kX=_kS[5];if((imul(3,_kT)|0)>=_kN){if((imul(3,_kN)|0)>=_kT){return [0,(_kT+_kN|0)+1|0,E(E(_kK)),_kL,E(_kS),E([0,_kN,E(_kO),_kP,E(_kQ),E(_kR)])];}else{return new F(function(){return _j2(_kU,_kV,_kW,B(_kJ(_kK,_kL,_kX,_kN,_kO,_kP,_kQ,_kR)));});}}else{return new F(function(){return _jT(_kO,_kP,B(_kY(_kK,_kL,_kT,_kU,_kV,_kW,_kX,_kQ)),_kR);});}}else{return new F(function(){return _kB(_kK,_kL,_kN,_kO,_kP,_kQ,_kR);});}},_kY=function(_kZ,_l0,_l1,_l2,_l3,_l4,_l5,_l6){var _l7=E(_l6);if(!_l7[0]){var _l8=_l7[1],_l9=_l7[2],_la=_l7[3],_lb=_l7[4],_lc=_l7[5];if((imul(3,_l1)|0)>=_l8){if((imul(3,_l8)|0)>=_l1){return [0,(_l1+_l8|0)+1|0,E(E(_kZ)),_l0,E([0,_l1,E(_l2),_l3,E(_l4),E(_l5)]),E(_l7)];}else{return new F(function(){return _j2(_l2,_l3,_l4,B(_kJ(_kZ,_l0,_l5,_l8,_l9,_la,_lb,_lc)));});}}else{return new F(function(){return _jT(_l9,_la,B(_kY(_kZ,_l0,_l1,_l2,_l3,_l4,_l5,_lb)),_lc);});}}else{return new F(function(){return _jK(_kZ,_l0,[0,_l1,E(_l2),_l3,E(_l4),E(_l5)]);});}},_ld=function(_le,_lf,_lg,_lh){var _li=E(_lg);if(!_li[0]){var _lj=_li[1],_lk=_li[2],_ll=_li[3],_lm=_li[4],_ln=_li[5],_lo=E(_lh);if(!_lo[0]){var _lp=_lo[1],_lq=_lo[2],_lr=_lo[3],_ls=_lo[4],_lt=_lo[5];if((imul(3,_lj)|0)>=_lp){if((imul(3,_lp)|0)>=_lj){return [0,(_lj+_lp|0)+1|0,E(E(_le)),_lf,E(_li),E(_lo)];}else{return new F(function(){return _j2(_lk,_ll,_lm,B(_kJ(_le,_lf,_ln,_lp,_lq,_lr,_ls,_lt)));});}}else{return new F(function(){return _jT(_lq,_lr,B(_kY(_le,_lf,_lj,_lk,_ll,_lm,_ln,_ls)),_lt);});}}else{return new F(function(){return _jK(_le,_lf,_li);});}}else{return new F(function(){return _kw(_le,_lf,_lh);});}},_lu=function(_lv,_lw,_lx,_ly){var _lz=E(_lv);if(_lz==1){var _lA=E(_ly);return _lA[0]==0?[0,[0,1,E([0,_lw]),_lx,E(_iX),E(_iX)],_C,_C]:_lw<E(E(_lA[1])[1])[1]?[0,[0,1,E([0,_lw]),_lx,E(_iX),E(_iX)],_lA,_C]:[0,[0,1,E([0,_lw]),_lx,E(_iX),E(_iX)],_C,_lA];}else{var _lB=B(_lu(_lz>>1,_lw,_lx,_ly)),_lC=_lB[1],_lD=_lB[3],_lE=E(_lB[2]);if(!_lE[0]){return [0,_lC,_C,_lD];}else{var _lF=E(_lE[1]),_lG=_lF[1],_lH=_lF[2],_lI=E(_lE[2]);if(!_lI[0]){return [0,new T(function(){return B(_jK(_lG,_lH,_lC));}),_C,_lD];}else{var _lJ=E(_lI[1]),_lK=E(_lG),_lL=E(_lJ[1])[1];if(_lK[1]<_lL){var _lM=B(_lu(_lz>>1,_lL,_lJ[2],_lI[2]));return [0,new T(function(){return B(_ld(_lK,_lH,_lC,_lM[1]));}),_lM[2],_lM[3]];}else{return [0,_lC,_C,_lE];}}}}},_lN=function(_lO,_lP,_lQ){var _lR=E(_lQ);if(!_lR[0]){var _lS=_lR[3],_lT=_lR[4],_lU=_lR[5],_lV=E(_lR[2]),_lW=_lV[1];if(_lO>=_lW){if(_lO!=_lW){return new F(function(){return _j2(_lV,_lS,_lT,B(_lN(_lO,_lP,_lU)));});}else{return [0,_lR[1],E([0,_lO]),_lP,E(_lT),E(_lU)];}}else{return new F(function(){return _jT(_lV,_lS,B(_lN(_lO,_lP,_lT)),_lU);});}}else{return [0,1,E([0,_lO]),_lP,E(_iX),E(_iX)];}},_lX=function(_lY,_lZ){while(1){var _m0=E(_lZ);if(!_m0[0]){return E(_lY);}else{var _m1=E(_m0[1]),_m2=B(_lN(E(_m1[1])[1],_m1[2],_lY));_lZ=_m0[2];_lY=_m2;continue;}}},_m3=function(_m4,_m5,_m6,_m7){return new F(function(){return _lX(B(_lN(_m5,_m6,_m4)),_m7);});},_m8=function(_m9,_ma,_mb){var _mc=E(_ma);return new F(function(){return _lX(B(_lN(E(_mc[1])[1],_mc[2],_m9)),_mb);});},_md=function(_me,_mf,_mg){while(1){var _mh=E(_mg);if(!_mh[0]){return E(_mf);}else{var _mi=E(_mh[1]),_mj=_mi[1],_mk=_mi[2],_ml=E(_mh[2]);if(!_ml[0]){return new F(function(){return _jK(_mj,_mk,_mf);});}else{var _mm=E(_ml[1]),_mn=E(_mj),_mo=_mn[1],_mp=E(_mm[1])[1];if(_mo<_mp){var _mq=B(_lu(_me,_mp,_mm[2],_ml[2])),_mr=_mq[1],_ms=E(_mq[3]);if(!_ms[0]){var _mt=_me<<1,_mu=B(_ld(_mn,_mk,_mf,_mr));_mg=_mq[2];_me=_mt;_mf=_mu;continue;}else{return new F(function(){return _m8(B(_ld(_mn,_mk,_mf,_mr)),_ms[1],_ms[2]);});}}else{return new F(function(){return _m3(_mf,_mo,_mk,_ml);});}}}}},_mv=function(_mw,_mx,_my,_mz,_mA){var _mB=E(_mA);if(!_mB[0]){return new F(function(){return _jK([0,_my],_mz,_mx);});}else{var _mC=E(_mB[1]),_mD=E(_mC[1])[1];if(_my<_mD){var _mE=B(_lu(_mw,_mD,_mC[2],_mB[2])),_mF=_mE[1],_mG=E(_mE[3]);if(!_mG[0]){return new F(function(){return _md(_mw<<1,B(_ld([0,_my],_mz,_mx,_mF)),_mE[2]);});}else{return new F(function(){return _m8(B(_ld([0,_my],_mz,_mx,_mF)),_mG[1],_mG[2]);});}}else{return new F(function(){return _m3(_mx,_my,_mz,_mB);});}}},_mH=function(_mI){var _mJ=E(_mI);if(!_mJ[0]){return [1];}else{var _mK=E(_mJ[1]),_mL=_mK[1],_mM=_mK[2],_mN=E(_mJ[2]);if(!_mN[0]){return [0,1,E(E(_mL)),_mM,E(_iX),E(_iX)];}else{var _mO=_mN[2],_mP=E(_mN[1]),_mQ=_mP[2],_mR=E(_mL),_mS=E(_mP[1])[1];return _mR[1]<_mS?B(_mv(1,[0,1,E(_mR),_mM,E(_iX),E(_iX)],_mS,_mQ,_mO)):B(_m3([0,1,E(_mR),_mM,E(_iX),E(_iX)],_mS,_mQ,_mO));}}},_mT=function(_mU,_mV){while(1){var _mW=E(_mU);if(!_mW[0]){return E(_mV);}else{_mU=_mW[2];var _mX=_mV+1|0;_mV=_mX;continue;}}},_mY=function(_mZ,_n0){while(1){var _n1=E(_n0);if(!_n1[0]){var _n2=E(_n1[2])[1];if(_mZ>=_n2){if(_mZ!=_n2){_n0=_n1[5];continue;}else{return [1,_n1[3]];}}else{_n0=_n1[4];continue;}}else{return [0];}}},_n3=[2],_n4=[1,_n3,_C],_n5=function(_n6){return _n6>1?[1,_n3,new T(function(){return B(_n5(_n6-1|0));})]:E(_n4);},_n7=function(_n8,_n9){var _na=E(_n8);if(!_na[0]){return [0];}else{var _nb=E(_n9);return _nb[0]==0?[0]:[1,[0,_na[1],_nb[1]],new T(function(){return B(_n7(_na[2],_nb[2]));})];}},_nc=function(_nd,_ne,_nf){var _ng=_ne-1|0;if(0<=_ng){var _nh=function(_ni){return [1,new T(function(){var _nj=B(_mY(_ni,new T(function(){return B(_mH(B(_n7(_nd,new T(function(){var _nk=B(_mT(_nd,0));if(B(_mT(_nf,0))!=_nk){var _nl=_nk>0?B(_n5(_nk)):[0];}else{var _nl=E(_nf);}var _nm=_nl,_nn=_nm,_no=_nn;return _no;},1)))));})));return _nj[0]==0?[3]:E(_nj[1]);}),new T(function(){if(_ni!=_ng){var _np=B(_nh(_ni+1|0));}else{var _np=[0];}var _nq=_np;return _nq;})];};return new F(function(){return _nh(0);});}else{return [0];}},_nr=function(_ns,_nt){while(1){var _nu=E(_nt);if(!_nu[0]){return false;}else{if(!B(A(_ns,[_nu[1]]))){_nt=_nu[2];continue;}else{return true;}}}},_nv=function(_nw){return E(_nw)[0]==2?true:false;},_nx=function(_ny){return E(_ny)[0]==1?true:false;},_nz=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_nA=new T(function(){return B(err(_nz));}),_nB=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_nC=new T(function(){return B(err(_nB));}),_nD=function(_nE,_nF){while(1){var _nG=E(_nE);if(!_nG[0]){return E(_nC);}else{var _nH=E(_nF);if(!_nH){return E(_nG[1]);}else{_nE=_nG[2];_nF=_nH-1|0;continue;}}}},_nI=function(_nJ,_nK){return new F(function(){return _ep(function(_nL){var _nM=E(_nL)[1];return _nM>=B(_mT(_nK,0))?[2]:_nM>=0?B(_nD(_nK,_nM)):E(_nA);},_nJ);});},_nN=function(_nO){return _nO>1?[1,_n3,new T(function(){return B(_nN(_nO-1|0));})]:E(_n4);},_nP=[1],_nQ=[1,_nP,_C],_nR=function(_nS){return _nS>1?[1,_nP,new T(function(){return B(_nR(_nS-1|0));})]:E(_nQ);},_nT=function(_nU,_nV,_nW,_nX,_nY){return new F(function(){return _nc(_nW,_nX,new T(function(){var _nZ=B(_nI(_nV,_nY));if(!B(_nr(_nv,_nZ))){if(!B(_nr(_nx,_nZ))){var _o0=B(A(_nU,[_nZ]));}else{var _o0=_nX>0?B(_nR(_nX)):[0];}var _o1=_o0;}else{var _o1=_nX>0?B(_nN(_nX)):[0];}var _o2=_o1;return _o2;}));});},_o3=function(_o4){return [0,_o4];},_o5=function(_o6,_o7){while(1){var _o8=E(_o7);if(!_o8[0]){return true;}else{if(!B(A(_o6,[_o8[1]]))){return false;}else{_o7=_o8[2];continue;}}}},_o9=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_oa=new T(function(){return B(err(_o9));}),_ob=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_oc=new T(function(){return B(err(_ob));}),_od=function(_oe,_of,_og){var _oh=function(_oi,_oj){return new F(function(){return _2b([1,function(_ok){return new F(function(){return A(_ag,[_ok,function(_ol){return E(new T(function(){return B(_bw(function(_om){var _on=E(_om);if(_on[0]==4){var _oo=E(_on[1]);if(!_oo[0]){return new F(function(){return A(_oe,[_on,_oi,_oj]);});}else{return E(E(_oo[1])[1])==45?E(_oo[2])[0]==0?E([1,function(_op){return new F(function(){return A(_ag,[_op,function(_oq){return E(new T(function(){return B(_bw(function(_or){return new F(function(){return A(_oe,[_or,_oi,function(_os){return new F(function(){return A(_oj,[new T(function(){return B(_4C(_os));})]);});}]);});}));}));}]);});}]):B(A(_oe,[_on,_oi,_oj])):B(A(_oe,[_on,_oi,_oj]));}}else{return new F(function(){return A(_oe,[_on,_oi,_oj]);});}}));}));}]);});}],new T(function(){return [1,B(_bS(_oh,_oj))];}));});};return new F(function(){return _oh(_of,_og);});},_ot=function(_ou,_ov){return [2];},_ow=function(_ox){var _oy=E(_ox);if(_oy[0]==5){var _oz=B(_cn(_oy[1]));return _oz[0]==0?E(_ot):function(_oA,_oB){return new F(function(){return A(_oB,[_oz[1]]);});};}else{return E(_ot);}},_oC=function(_cx,_cy){return new F(function(){return _od(_ow,_cx,_cy);});},_oD=function(_oE){return [1,function(_oF){return new F(function(){return A(_ag,[_oF,function(_oG){return E([3,_oE,_R]);}]);});}];},_oH=new T(function(){return B(_cB(_oC,_oD));}),_oI=function(_oJ){return E(_oJ)[0]==0?true:false;},_oK=function(_oL){while(1){var _oM=(function(_oN){var _oO=E(_oN);if(!_oO[0]){return [0];}else{var _oP=_oO[2],_oQ=E(_oO[1]);if(!E(_oQ[2])[0]){return [1,_oQ[1],new T(function(){return B(_oK(_oP));})];}else{_oL=_oP;return null;}}})(_oL);if(_oM!=null){return _oM;}}},_oR=function(_oS){var _oT=E(_oS);return _oT[0]==0?E(_oT[1]):B(_1Y("Src/CoinKernel.hs:80:61-78|lambda"));},_oU=function(_oV,_oW){if(!B(_o5(_oI,_oW))){return [0];}else{if(!B(_ep(_oR,_oW))[0]){var _oX=B(_oK(B(_21(_oH,_oV))));return _oX[0]==0?E(_oc):E(_oX[2])[0]==0?B(_ep(_o3,_oX[1])):E(_oa);}else{return [0];}}},_oY=[0,1],_oZ=[0,_oY,_oU],_p0=[1,_oZ,_C],_p1=[0,0],_p2=function(_p3,_p4){var _p5=E(_p3);if(!_p5[0]){var _p6=_p5[1],_p7=E(_p4);return _p7[0]==0?_p6==_p7[1]:I_compareInt(_p7[1],_p6)==0?true:false;}else{var _p8=_p5[1],_p9=E(_p4);return _p9[0]==0?I_compareInt(_p8,_p9[1])==0?true:false:I_compare(_p8,_p9[1])==0?true:false;}},_pa=[0,0],_pb=function(_pc,_pd){while(1){var _pe=E(_pc);if(!_pe[0]){return E(_pd);}else{_pc=_pe[2];var _pf=B(_4s(_pd,_pe[1]));_pd=_pf;continue;}}},_pg=function(_ph){return new F(function(){return _pb(_ph,_pa);});},_pi=function(_pj,_pk){var _pl=B(_oK(B(_21(_oH,_pj))));if(!_pl[0]){return E(_oc);}else{var _pm=_pl[1];return E(_pl[2])[0]==0?!B(_p2(B(_pg(_pk)),B(_pg(_pm))))?[0]:E(_pm):E(_oa);}},_pn=function(_po,_pp){if(!B(_o5(_oI,_pp))){return [0];}else{return new F(function(){return _ep(_o3,B(_pi(_po,B(_ep(_oR,_pp)))));});}},_pq=[0,_p1,_pn],_pr=[1,_pq,_p0],_ps=new T(function(){return B(_mH(_pr));}),_pt=new T(function(){return B(unCStr("head"));}),_pu=new T(function(){return B(_F(_pt));}),_pv=new T(function(){return B(_c4(_cq,_bR,_S));}),_pw=new T(function(){return B(_cB(_oC,_oD));}),_px=function(_py,_pz){if(_py<=_pz){var _pA=function(_pB){return [1,[0,_pB],new T(function(){if(_pB!=_pz){var _pC=B(_pA(_pB+1|0));}else{var _pC=[0];}var _pD=_pC;return _pD;})];};return new F(function(){return _pA(_py);});}else{return [0];}},_pE=new T(function(){return B(_px(0,2147483647));}),_pF=new T(function(){return B(err(_o9));}),_pG=new T(function(){return B(err(_ob));}),_pH=new T(function(){return B(_dS(_d6));}),_pI=new T(function(){return B(_dt(_pH,_pH,_d6,_S));}),_pJ=function(_pK){while(1){var _pL=E(_pK);if(!_pL[0]){_pK=[1,I_fromInt(_pL[1])];continue;}else{return new F(function(){return I_toString(_pL[1]);});}}},_pM=function(_pN,_pO){return new F(function(){return _f(fromJSStr(B(_pJ(_pN))),_pO);});},_pP=function(_pQ,_pR){var _pS=E(_pQ);if(!_pS[0]){var _pT=_pS[1],_pU=E(_pR);return _pU[0]==0?_pT<_pU[1]:I_compareInt(_pU[1],_pT)>0;}else{var _pV=_pS[1],_pW=E(_pR);return _pW[0]==0?I_compareInt(_pV,_pW[1])<0:I_compare(_pV,_pW[1])<0;}},_pX=[0,0],_pY=function(_pZ,_q0,_q1){return _pZ<=6?B(_pM(_q0,_q1)):!B(_pP(_q0,_pX))?B(_pM(_q0,_q1)):[1,_p,new T(function(){return B(_f(fromJSStr(B(_pJ(_q0))),[1,_o,_q1]));})];},_q2=function(_q3){return new F(function(){return _pY(0,_q3,_C);});},_q4=new T(function(){return [0,"index"];}),_q5=new T(function(){return [0,"txid"];}),_q6=new T(function(){return [0,"coinstate"];}),_q7=new T(function(){return [0,"value"];}),_q8=function(_q9){var _qa=E(_q9);return _qa[0]==0?_qa[1]:I_toNumber(_qa[1]);},_qb=function(_qc){return [0,B(_q8(_qc))];},_qd=function(_qe,_qf,_qg,_qh){return [1,[0,_q5,new T(function(){return B(_hE(_qe));})],[1,[0,_q4,new T(function(){return B(_h5(_qf));})],[1,[0,_q7,new T(function(){return B(_qb(_qg));})],[1,[0,_q6,new T(function(){return B(_hE(_qh));})],_C]]]];},_qi=new T(function(){return [0,toJSStr(_C)];}),_qj=[0,93],_qk=[1,_qj,_C],_ql=new T(function(){return [0,toJSStr(_qk)];}),_qm=[0,125],_qn=[1,_qm,_C],_qo=new T(function(){return [0,toJSStr(_qn)];}),_qp=[0,58],_qq=[1,_qp,_C],_qr=new T(function(){return [0,toJSStr(_qq)];}),_qs=[0,44],_qt=[1,_qs,_C],_qu=new T(function(){return [0,toJSStr(_qt)];}),_qv=new T(function(){return [0,"false"];}),_qw=function(_qx){var _qy=jsStringify(E(_qx)[1]),_qz=_qy;return [0,_qz];},_qA=new T(function(){return [0,"null"];}),_qB=[0,91],_qC=[1,_qB,_C],_qD=new T(function(){return [0,toJSStr(_qC)];}),_qE=[0,123],_qF=[1,_qE,_C],_qG=new T(function(){return [0,toJSStr(_qF)];}),_qH=[0,34],_qI=[1,_qH,_C],_qJ=new T(function(){return [0,toJSStr(_qI)];}),_qK=new T(function(){return [0,"true"];}),_qL=function(_qM,_qN){var _qO=E(_qN);switch(_qO[0]){case 0:return [0,new T(function(){var _qP=jsShow(_qO[1]),_qQ=_qP;return [0,_qQ];}),_qM];case 1:return [0,new T(function(){var _qR=jsStringify(_qO[1]),_qS=_qR;return [0,_qS];}),_qM];case 2:return !E(_qO[1])?[0,_qv,_qM]:[0,_qK,_qM];case 3:var _qT=E(_qO[1]);return _qT[0]==0?[0,_qD,[1,_ql,_qM]]:[0,_qD,new T(function(){var _qU=B(_qL(new T(function(){var _qV=function(_qW){var _qX=E(_qW);return _qX[0]==0?E([1,_ql,_qM]):[1,_qu,new T(function(){var _qY=B(_qL(new T(function(){return B(_qV(_qX[2]));}),_qX[1]));return [1,_qY[1],_qY[2]];})];};return B(_qV(_qT[2]));}),_qT[1]));return [1,_qU[1],_qU[2]];})];case 4:var _qZ=E(_qO[1]);if(!_qZ[0]){return [0,_qG,[1,_qo,_qM]];}else{var _r0=E(_qZ[1]);return [0,_qG,[1,new T(function(){return B(_qw(_r0[1]));}),[1,_qr,new T(function(){var _r1=B(_qL(new T(function(){var _r2=function(_r3){var _r4=E(_r3);if(!_r4[0]){return E([1,_qo,_qM]);}else{var _r5=E(_r4[1]);return [1,_qu,[1,_qJ,[1,_r5[1],[1,_qJ,[1,_qr,new T(function(){var _r6=B(_qL(new T(function(){return B(_r2(_r4[2]));}),_r5[2]));return [1,_r6[1],_r6[2]];})]]]]];}};return B(_r2(_qZ[2]));}),_r0[2]));return [1,_r1[1],_r1[2]];})]]];}break;default:return [0,_qA,_qM];}},_r7=function(_r8){var _r9=jsCat(new T(function(){var _ra=B(_qL(_C,_r8));return [1,_ra[1],_ra[2]];}),E(_qi)[1]),_rb=_r9;return E(_rb);},_rc=[0,78],_rd=[1,_rc,_C],_re=[0,73],_rf=[1,_re,_C],_rg=[0,77],_rh=[1,_rg,_C],_ri=function(_rj,_rk,_rl,_rm){return new F(function(){return _r7([4,E(B(_qd(_rj,_rk,_rm,new T(function(){var _rn=E(_rl);switch(_rn[0]){case 0:var _ro=B(_q2(_rn[1]));break;case 1:var _ro=E(_rh);break;case 2:var _ro=E(_rf);break;default:var _ro=E(_rd);}return _ro;},1))))]);});},_rp=function(_rq){var _rr=E(_rq),_rs=E(_rr[1]);return [0,B(_ri(_rs[1],_rs[2],_rr[2],_rr[3]))];},_rt=function(_ru){return E(E(_ru)[1]);},_rv=new T(function(){return B(_c4(_cq,_bR,_S));}),_rw=function(_rx){return [0];},_ry=function(_rz,_rA){var _rB=B(_21(_rv,_rA));if(!_rB[0]){return E(_rw);}else{if(!E(_rB[2])[0]){var _rC=E(_rB[1]),_rD=B(_mY(E(_rC[1])[1],_rz));return _rD[0]==0?E(_rw):B(A(_rD[1],[_rC[2]]));}else{return E(_rw);}}},_rE=function(_rF,_rG,_rH){var _rI=E(_rF);if(!_rI[0]){return [0];}else{var _rJ=E(_rG);if(!_rJ[0]){return [0];}else{var _rK=E(_rH);return _rK[0]==0?[0]:[1,[0,_rI[1],_rJ[1],_rK[1]],new T(function(){return B(_rE(_rI[2],_rJ[2],_rK[2]));})];}}},_rL=function(_rM,_){return new T(function(){var _rN=new T(function(){return B(_iR(E(_rM)[1]));}),_rO=function(_rP){var _rQ=E(_rP);return _rQ[0]==0?[0]:[1,[0,new T(function(){return E(E(_rN)[3]);}),_rQ[1]],new T(function(){return B(_rO(_rQ[2]));})];};return B(_ep(_rp,B(_rE(B(_rO(_pE)),new T(function(){var _rR=B(_21(_dV,new T(function(){return B(_rt(_rN));})));if(!_rR[0]){var _rS=[0];}else{if(!E(_rR[2])[0]){var _rT=E(_rR[1]),_rU=E(_rT[1]),_rV=B(_nT(new T(function(){return B(_ry(_ps,_rT[2]));},1),_rU[1],_rU[2],E(_rU[3])[1],_C));}else{var _rV=[0];}var _rS=_rV;}return _rS;},1),new T(function(){var _rW=B(_oK(B(_21(_pw,new T(function(){var _rX=B(_21(_pv,new T(function(){var _rY=B(_21(_pI,new T(function(){return E(E(_rN)[1]);})));if(!_rY[0]){var _rZ=E(_pu);}else{var _rZ=E(E(_rY[1])[2]);}return _rZ;})));if(!_rX[0]){var _s0=E(_pu);}else{var _s0=E(E(_rX[1])[2]);}return _s0;})))));return _rW[0]==0?E(_pG):E(_rW[2])[0]==0?E(_rW[1]):E(_pF);},1)))));});},_s1=function(_){var _s2=B(A(_gt,[_fD,_])),_s3=_s2,_s4=B(A(_eC,[_rL,_])),_s5=_s4;return new F(function(){return A(_eN,[_dW,_]);});},_s6=function(_){return new F(function(){return _s1(_);});};
var hasteMain = function() {B(A(_s6, [0]));};hasteMain();