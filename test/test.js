var Simulation = require('../Simulation/simulation.js')
var kernel     = require('../Simulation/kernel.js')
var bitcoin    = require('bitcoinjs-lib')
var _          = require('lodash')
var expect     = require('chai').expect
var Kernel     = kernel.Kernel
var Color      = kernel.Color
var ColorValue = kernel.ColorValue


describe("Simulation", function() {
  var sim
  var alice
  var bob
  var bitcoin
  var kernel
  var color
    
  it("'new Simulation()' should return new simulation object", function() {
    sim = new Simulation()
    expect(sim).to.be.an.instanceof(Simulation)
    expect(sim).to.have.a.property('name', 'test')
    expect(sim).to.have.a.property('transactions')
    expect(sim).to.have.a.property('coins')
    expect(sim.wallets).to.have.a.property('bitcoin')
  })
    
  describe("Wallets initialization", function () {
    it("Default wallet must be already created", function () {
      bitcoin = sim.wallets['bitcoin']
      expect(Object.keys(sim.wallets).length).to.equal(1)
    })
    
    it("Alice's wallet initialisation", function() {
      alice = sim.wallet('alice')
      expect(Object.keys(sim.wallets).length).to.equal(2)
    })

    it("Bob's wallet initialisation", function() {
      bob = sim.wallet('bob')
      expect(Object.keys(sim.wallets).length).to.equal(3)
    })
  })

  describe("Create kernel", function() {
    it("Just one kernel now available, 'toy', "
      + "so sim.kernel('toy') returns new Kernel object", function() {
        kernel = sim.kernel('toy')
        expect(kernel).to.be.an.instanceof(Kernel)
      })
  })
      
  describe("Wallet operations", function() {
    it("Default wallet must contain some bitcoins already", function() {
      var balance = bitcoin.getBalance()
      expect(balance).to.be.above(0)
    })

    describe("Get coins", function () {
      it("Alice gets 10 bitcoins", function() {
        alice.getCoins(1000000000)
      })

      it("Bob gets 20 bitcoins", function() {
        bob.getCoins(2000000000)
      })
    })

    describe("IssueCoin must retun new Color object", function () {
      it("Alice issues a Coin", function () {
        color = alice.issueCoin (kernel, 5000000, 'test_color')
        expect(color).to.be.an.instanceof(Color)
      })
    })
    
    describe("Get balance", function() {
      it("Alice getBalance()", function() {
        var balance = alice.getBalance()
      })

      it("Bob getBalance()", function() {
        var balance = bob.getBalance()
      })
    })

    describe("Send", function() {
      it("Alice sends coloredCoin to Bob", function() {
        alice.send(new ColorValue (color, 1250000), bob)
      })

      it("Bob sends coloredCoin to Alice", function() {
        bob.send(new ColorValue (color, 250000), alice)
      })

    })        
  })
})
