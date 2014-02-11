var request   = require('request')
  , mongoose  = require('mongoose')
  , chai      = require('chai')
  , expect    = chai.expect
  , should    = chai.should()
  , assert    = chai.assert
  , User      = require(process.env.PRISM_HOME + 'models/auth.js')
  , server	  = require(process.env.PRISM_HOME + 'server');