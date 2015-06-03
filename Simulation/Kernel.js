var maketx = require('../Maketx/src/maketx.js')
var coinkernel = require('../ColorCoin/src/ckernel.js')
var h = require('../ColorCoin/main.js').getHaste()

function Kernel() {
    
}

function Color() {
    throw new Error ("Color not implemented");
    //TODO
}


function ColorValue() {
    throw new Error ("ColorValue not implemented");
    //TODO

}

module.exports = {
    Kernel     : Kernel,
    Color      : Color,
    ColorValue : ColorValue
}
