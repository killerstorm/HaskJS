const _           = require('lodash');
const bitcoin     = require('bitcoinjs-lib');
const buffertools = require('buffertools');
const Transaction = bitcoin.Transaction;

/**
 * dustThreshold
 * @const {number}
 */
const dustThreshold = 546

/**
 * @return {string} payload
 */
function createPayload (ins, outs, opid, outValues) {
  return '(' +
    JSON.stringify(_.range(ins)) + ', ' +
    JSON.stringify(_.range(outs)) + ', ' +
    outs + ') ' +
    opid.toString() + ' ' + JSON.stringify(outValues)
}

/**
 * Remove spent coins
 */
function removeSpentCoins (unspentCoins, spentCoins) {
  _.each(spentCoins, function(coin) {
    unspentCoins.splice(_.findIndex(unspentCoins, coin), 1)
  })
}

/**
 * Select coins
 * @return {[Object]}
 */
function selectCoins (unspentCoins, coinValueFn, neededSum) {
  var total = 0
  
  var selected = _.takeWhile(unspentCoins, function (n) {
    return total < neededSum ? (total += coinValueFn (n)) && true : false
  })

  if (total < neededSum)
    throw new Error ("Not enough coins!")

  return selected
}

/**
 * Compose colored send transaction skeleton
 * @return {Object}
 */
function composeColoredSendTx (unspentCoins, targets, changeAddress) {
  function coinValueFn (coin) {
    return coin.cv.getValue()
  }

  function getColorId (cv) {
    return cv.getColorID()
  }

  function getTargetValue (target) {
    return target.value.getValue()
  }

  var outputColorValues = _.map(targets, 'value')
  var colorIds          = _.uniq(_.map(outputColorValues, getColorId))
  
  if(colorIds.length > 1)
    throw new Error ('Only one color supported!')
    
  var colorID = _.head(colorIds)
  var unspentColoredCoins = _.filter(unspentCoins, function (coin) {
    if (coin.cv)
      return coin.cv.getColorID() === colorID
    return false
  })
  
  targets = _.clone(targets)
  var neededSum = _.sum(targets, getTargetValue) 
  var coins     = selectCoins(unspentColoredCoins, coinValueFn, neededSum)
  var inputSum  = _.sum(coins, coinValueFn)
  var change    = inputSum - neededSum   

  removeSpentCoins (unspentCoins, coins)
  
  _.each(targets, function (target) {
    target.value = target.value.getValue()
  })
    
  if (change > 0) 
    targets.push({address: changeAddress, value : change})

  return {inputs: coins, targets: targets}
}

/**
 * Compose colored issue transactions skeleton
 * @return {Object}
 */
function composeColoredIssueTx (targets) {
  return {inputs: [], targets: targets}
}

/**
 * Compose bitcoin transaction
 * @return {Transaction}
 */
function composeBitcoinTx (coloredTx, unspentCoins, changeAddress) {
  var tx = new Transaction()

  var index = 0
  var unspentUncoloredCoins = _.reject(unspentCoins, 'cv')
 
  var coloredTargets = coloredTx.targets
  var coloredInputs  = coloredTx.inputs
  var fee = 10000
  var uncoloredNeeded   = coloredTargets.length * dustThreshold + fee

  _.each(coloredTargets, function(target) {
    tx.addOutput(target.address, target.value)
    index++
  })
 
  _.each(coloredInputs, function(coin) {
      tx.addInput(coin.txid, coin.index)
      uncoloredNeeded -= coin.value
  })

  var uncoloredSum = 0
  var uncoloredInputs = []
  var change = 0
    
  if (uncoloredNeeded > 0) {
    uncoloredInputs = selectCoins(
      unspentUncoloredCoins,
      function (coin) { return coin.value },
      uncoloredNeeded
    )
    
    uncoloredSum        = _.sum(uncoloredInputs, 'value')
    _.each(uncoloredInputs, function(coin) {
      tx.addInput(coin.txid, coin.index)
    })
    change = uncoloredSum - uncoloredNeeded - _.sum(coloredTargets, 'value')
  
    if (change > 0) {
      tx.addOutput(changeAddress, change)
    }
  }

  var outValues = _.pluck(coloredTargets, 'value').concat(
    change == 0 ? [] : [change])
  
  var ins =
    (coloredInputs === [])
    ? coloredInputs.length + uncoloredInputs.length
    : 0

  var outs =
    change
    ? coloredTargets.length + 1
    : coloredTargets.length

  var opid =
    coloredInputs.length
    ? 0
    : 1
  
  var payload = createPayload (ins, outs, opid, outValues)
    
  tx.addOutput(bitcoin.scripts.nullDataOutput(new Buffer(payload)), 0)
    
  removeSpentCoins(unspentCoins, uncoloredInputs)

  return tx
}

module.exports = {
    composeColoredSendTx  : composeColoredSendTx
  , composeBitcoinTx      : composeBitcoinTx
  , composeColoredIssueTx : composeColoredIssueTx
  , selectCoins           : selectCoins
  , removeSpentCoins      : removeSpentCoins
}
