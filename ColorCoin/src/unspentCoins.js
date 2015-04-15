var _ = require('lodash');

function Unspent(all, colored, uncolored) {
    this.all       = all || [];
    this.colored   = colored || [];
    this.uncolored = uncolored || [];
}


Unspent.prototype.selectColored = function(n) {
    this.selectCoins(this.colored, n);
}

Unspent.prototype.selectUncolored = function(n) {
    this.selectCoins(this.uncolored, n);
}


Unspent.prototype.selectCoins = function (unspentCoins, n) {
    var total = 0;
    var unspent = _.takeWhile(unspentCoins, function(n) {
        var value = parseInt(n.cs) == NaN ? 0 : parseInt(n.cs);
        return total >= neededSum ? false : (total += value) && true;
    });

    if (total < neededSum)
        throw new Error ("Not enough coins!");

    unspentCoins = _.difference(unspentCoins, unspent);
    unspent = _.filter(unspent, function(n) {return parseInt(n.cs) != NaN;});
    return unspent;        
} 


module.exports = Unspent
