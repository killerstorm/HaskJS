var Simulation = require('../Simulation/simulation.js')
var kernel     = require('../Simulation/kernel.js')
var bitcoin    = require('bitcoinjs-lib')
var _          = require('lodash')
var expect     = require('chai').expect



describe("Simulation", function() {
  var sim
  var alice
  var bob
  var bitcoin
    
  it("'new Simulation()' should return new simulation object", function() {
    sim = new Simulation()
    expect(sim).to.be.an.instanceof(Simulation)
    expect(sim).to.have.a.property('name', 'test')
    expect(sim).to.have.a.property('transactions')
    expect(sim.wallets).to.have.a.property('bitcoin')
  });
    
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
        

    describe("Get balance", function() {
      it("Alice's balance, expected to equal 1000000000 satoshi", function() {
        var balance = alice.getBalance()
        expect(balance).to.equal(1000000000)
      })

      it("Bob's balance, expected to equal 2000000000 satoshi", function() {
        var balance = bob.getBalance()
        expect(balance).to.equal(2000000000)
      })
    })

    describe("Send", function() {
      it("Alice sends 1000000 satoshi to Bob", function() {
        alice.send(1000000, bob)
      })

      it("Bob sends 1000000000 satoshi to Alice", function() {
        bob.send(1000000000, alice)
      })

    })        
  })
})
